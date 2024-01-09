{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Moore Machines and related machinery.
module Machines.Moore
  ( MooreM (..),
    Moore,
    MooreM' (..),
    Moore',
    fixMooreM,
    fixMoore,
    scanMooreM,
    scanMoore,
    processMooreM,
    processMoore,
    processMooreM',
    processMoore',
    (/\),
    (\/),
    (/+\),
  )
where

--------------------------------------------------------------------------------

import Control.Applicative (liftA2)
import Control.Arrow
import Control.Category.Cartesian (split)
import Control.Monad.Identity (Identity (..))
import Data.Biapplicative
import Data.Bifunctor.Monoidal qualified as Bifunctor
import Data.Bifunctor.Monoidal.Specialized (mux)
import Data.Functor.Monoidal qualified as Functor
import Data.Profunctor (Closed (..), Costrong (..), Profunctor (..))
import Data.Profunctor.Rep (Corep, Corepresentable (..))
import Data.Profunctor.Sieve (Cosieve (..))
import Data.These (These (..), these)
import Data.Trifunctor.Monoidal qualified as Trifunctor
import Data.Void (Void, absurd)

--------------------------------------------------------------------------------

-- | Monadic Moore Machine consists of:
--
--   * A finite set of states @S@
--   * An initial state @s : S@
--   * A monad @M@
--   * A finite set called the input @I@
--   * A finite set called the output @O@
--   * A function @transistion : S × I → S@
--   * A function @observe : S → O@
--
-- In this particular encoding we receive the initial state and
-- produce a tuple of the observation at the initial state and the
-- next state transition function.
newtype MooreM m s i o = MooreM {runMooreM :: s -> m (o, i -> s)}
  deriving (Functor)

instance (Applicative m) => Trifunctor.Semigroupal (->) (,) (,) (,) (,) (MooreM m) where
  combine :: (MooreM m s i o, MooreM m t i' o') -> MooreM m (s, t) (i, i') (o, o')
  combine (MooreM m1, MooreM m2) =
    MooreM $ \(s, t) ->
      liftA2 (curry $ fmap biapply . Bifunctor.combine @(->) @(,) @(,)) (m1 s) (m2 t)

instance (Applicative m) => Trifunctor.Semigroupal (->) (,) Either (,) (,) (MooreM m) where
  combine :: (MooreM m s i o, MooreM m t i' o') -> MooreM m (s, t) (Either i i') (o, o')
  combine (MooreM m1, MooreM m2) =
    MooreM $ \(s, t) -> liftA2 (curry $ fmap (\(f, g) -> either ((,t) . f) ((s,) . g)) . Bifunctor.combine @(->) @(,) @(,)) (m1 s) (m2 t)

instance (Applicative m) => Trifunctor.Semigroupal (->) (,) These (,) (,) (MooreM m) where
  combine :: (MooreM m s i o, MooreM m t i' o') -> MooreM m (s, t) (These i i') (o, o')
  combine (MooreM m1, MooreM m2) =
    MooreM $ \(s, t) -> liftA2 (curry $ fmap (\(f, g) -> these ((,t) . f) ((s,) . g) (curry (f *** g))) . Bifunctor.combine @(->) @(,) @(,)) (m1 s) (m2 t)

instance (Applicative m) => Trifunctor.Unital (->) () () () () (MooreM m) where
  introduce :: () -> MooreM m () () ()
  introduce () = MooreM $ \() -> pure ((), const ())

instance (Applicative m) => Trifunctor.Unital (->) () Void () () (MooreM m) where
  introduce :: () -> MooreM m () Void ()
  introduce () = MooreM $ \() -> pure ((), const ())

instance (Applicative m) => Trifunctor.Monoidal (->) (,) () (,) () (,) () (,) () (MooreM m)

instance (Applicative m) => Trifunctor.Monoidal (->) (,) () Either Void (,) () (,) () (MooreM m)

instance (Applicative m) => Trifunctor.Monoidal (->) (,) () These Void (,) () (,) () (MooreM m)

instance (Functor m) => Profunctor (MooreM m s) where
  dimap :: (i' -> i) -> (o -> o') -> MooreM m s i o -> MooreM m s i' o'
  dimap f g (MooreM moore) = MooreM $ fmap (fmap (bimap g (lmap f))) moore

-- | Moore Machine
--
--   * A finite set of states @S@
--   * An initial state @s : S@
--   * A finite set called the input @I@
--   * A finite set called the output @O@
--   * A function @transistion : S × I → S@
--   * A function @observe : S → O@
--
-- In this particular encoding we receive the initial state and
-- produce a tuple of the observation at the initial state and the
-- next state transition function.
type Moore = MooreM Identity

instance Cosieve Moore' [] where
  cosieve :: Moore' i o -> [i] -> o
  cosieve (runIdentity . runMooreM' -> (o, _)) [] = o
  cosieve (runIdentity . runMooreM' -> (_, k)) (x : xs) = cosieve (k x) xs

instance Corepresentable Moore' where
  type Corep Moore' = []

  cotabulate :: (Corep Moore' i -> o) -> Moore' i o
  cotabulate f = MooreM' $ Identity (f [], \i -> cotabulate (f . (i :)))

instance Closed Moore' where
  closed :: Moore' i o -> Moore' (x -> i) (x -> o)
  closed m = cotabulate $ \fs x -> cosieve m (fmap ($ x) fs)

--------------------------------------------------------------------------------

-- | The fixed point of a 'MooreM' Machine. By taking the fixpoint we
-- are able to hide the state parameter @s@.
newtype MooreM' m i o = MooreM' {runMooreM' :: m (o, i -> MooreM' m i o)}
  deriving (Functor)

instance (Applicative m) => Applicative (MooreM' m i) where
  pure :: o -> MooreM' m i o
  pure o = let r = MooreM' $ pure (o, const r) in r

  liftA2 :: (o -> o' -> o'') -> MooreM' m i o -> MooreM' m i o' -> MooreM' m i o''
  liftA2 f m1 m2 = dimap split (uncurry f) $ Bifunctor.combine @(->) @(,) @(,) @(,) (m1, m2)

instance (Applicative m) => Bifunctor.Semigroupal (->) (,) (,) (,) (MooreM' m) where
  combine :: (MooreM' m i o, MooreM' m i' o') -> MooreM' m (i, i') (o, o')
  combine (MooreM' m1, MooreM' m2) =
    MooreM' $ liftA2 (\(o, m1') (o', m2') -> ((o, o'), \(i, i') -> Bifunctor.combine (m1' i, m2' i'))) m1 m2

instance (Applicative m) => Bifunctor.Unital (->) () () () (MooreM' m) where
  introduce :: () -> MooreM' m () ()
  introduce () = MooreM' $ pure ((), Bifunctor.introduce)

instance (Applicative m) => Bifunctor.Unital (->) Void () () (MooreM' m) where
  introduce :: () -> MooreM' m Void ()
  introduce () = MooreM' $ pure ((), absurd)

instance (Applicative m) => Bifunctor.Monoidal (->) (,) () (,) () (,) () (MooreM' m)

instance (Applicative m, Semigroup o) => Semigroup (MooreM' m i o) where
  (<>) :: MooreM' m i o -> MooreM' m i o -> MooreM' m i o
  MooreM' m1 <> MooreM' m2 = MooreM' $ liftA2 (<>) m1 m2

instance (Applicative m, Monoid o) => Monoid (MooreM' m i o) where
  mempty :: MooreM' m i o
  mempty = MooreM' $ pure mempty

instance (Functor m) => Profunctor (MooreM' m) where
  dimap :: (i' -> i) -> (o -> o') -> MooreM' m i o -> MooreM' m i' o'
  dimap f g (MooreM' moore) =
    MooreM' $ fmap (bimap g (dimap f (dimap f g))) moore

instance (Functor m) => Costrong (MooreM' m) where
  unfirst :: MooreM' m (i, d) (o, d) -> MooreM' m i o
  unfirst (MooreM' moore) = MooreM' $ fmap (\((o, d), m) -> (o, \i -> unfirst (m (i, d)))) moore

-- | The fixed point of a 'Moore' Machine. By taking the fixpoint we
-- are able to hide the state parameter @s@.
type Moore' = MooreM' Identity

-- | Take the fixpoint of @MooreM s i o@ by recursively constructing an
-- @s -> MooreM' i o@ action and tupling it with the output observation
-- @o@ from its parent action.
fixMooreM :: forall m s o i. (Functor m) => MooreM m s o i -> s -> MooreM' m o i
fixMooreM (MooreM moore) = go
  where
    go :: s -> MooreM' m o i
    go s = MooreM' $ fmap (fmap go) <$> moore s

-- | Take the fixpoint of @Moore s i o@ by recursively constructing an
-- @s -> Moore' i o@ action and tupling it with the output observation
-- @o@ from its parent action.
fixMoore :: forall s o i. Moore s o i -> s -> Moore' o i
fixMoore = fixMooreM

--------------------------------------------------------------------------------

-- | Feed inputs into a 'Moore' Machine and extract the observation at
-- each state/input in a 'scan' style.
scanMooreM :: (Monad m) => s -> [i] -> MooreM m s i o -> m [(o, s)]
scanMooreM state' inputs machine = do
  (o, transition) <- runMooreM machine state'
  case inputs of
    [] -> pure [(o, state')]
    i : xs -> do
      ys <- scanMooreM (transition i) xs machine
      pure $ (o, state') : ys

-- | Feed inputs into a 'Moore' Machine and extract the observation at
-- each state/input in a 'scan' style.
scanMoore :: s -> [i] -> Moore s i o -> [(o, s)]
scanMoore state' inputs machine =
  let (o, transition) = runIdentity $ runMooreM machine state'
   in case inputs of
        [] -> [(o, state')]
        i : xs -> (o, state') : scanMoore (transition i) xs machine

-- | Feed inputs into a 'Moore' Machine and then observe the final
-- result.
processMooreM :: (Monad m) => s -> [i] -> MooreM m s i o -> m o
processMooreM initialState inputs machine = do
  (o, transition) <- runMooreM machine initialState
  case inputs of
    [] -> pure o
    i : xs -> processMooreM (transition i) xs machine

-- | Feed inputs into a 'Moore' Machine and then observe the final
-- result.
processMoore :: s -> [i] -> Moore s i o -> o
processMoore initialState inputs machine =
  let (o, transition) = runIdentity $ runMooreM machine initialState
   in case inputs of
        [] -> o
        i : xs -> processMoore (transition i) xs machine

-- | Feed inputs into a 'MooreM'' Machine and then observe the final
-- result.
processMooreM' :: (Monad m) => [i] -> MooreM' m i o -> m o
processMooreM' [] (MooreM' moore) = fmap fst moore
processMooreM' (i : xs) (MooreM' moore) = do
  (_, nextMoore) <- moore
  processMooreM' xs (nextMoore i)

-- | Feed inputs into a 'Moore'' Machine and then observe the final
-- result.
processMoore' :: [i] -> Moore' i o -> o
processMoore' = flip cosieve

--------------------------------------------------------------------------------
-- Tensors

infixr 9 /\

(/\) :: (Applicative m) => MooreM m s i o -> MooreM m t i o' -> MooreM m (s, t) i (o, o')
(/\) m1 m2 = lmap split $ (Trifunctor.combine @_ @(,) @(,) @(,)) (m1, m2)

infixr 9 /+\

(/+\) :: (Applicative m) => MooreM m s i o -> MooreM m t i' o' -> MooreM m (s, t) (These i i') (o, o')
(/+\) m1 m2 = Trifunctor.combine @_ @(,) @These @(,) (m1, m2)

infixr 9 \/

(\/) :: (Applicative m) => MooreM m s i o -> MooreM m t i' o' -> MooreM m (s, t) (Either i i') (o, o')
(\/) m1 m2 = Trifunctor.combine @_ @(,) @Either @(,) (m1, m2)

--------------------------------------------------------------------------------
-- TODO: Move to monoidal functors:

liftA2' :: (Functor m, Functor.Semigroupal (->) (,) (,) m) => (a -> b -> c) -> m a -> m b -> m c
liftA2' f m1 m2 = uncurry f <$> Functor.combine (m1, m2)

biliftA2' :: (Bifunctor m, Bifunctor.Semigroupal (->) (,) (,) (,) m) => (a -> b -> c) -> (d -> e -> f) -> m a d -> m b e -> m c f
biliftA2' f g m1 m2 = bimap (uncurry f) (uncurry g) $ Bifunctor.combine (m1, m2)

biapply :: (Bifunctor p, Bifunctor.Semigroupal (->) (,) (,) (,) p) => p (a -> b) (c -> d) -> p a c -> p b d
biapply = fmap (bimap (uncurry ($)) (uncurry ($))) . mux
