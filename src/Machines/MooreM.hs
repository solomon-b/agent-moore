{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Moore Machines and related machinery.
module Machines.MooreM
  ( MooreM (..),
    MooreM' (..),
    fixMooreM,
    scanMooreM,
    processMooreM,
    -- processMooreM',
    (/\),
    (\/),
    (/+\),
  )
where

--------------------------------------------------------------------------------

import Control.Applicative (liftA2)
import Control.Category.Cartesian (split)
import Data.Bifunctor (bimap)
import Data.Bifunctor.Monoidal qualified as Bifunctor
import Data.Functor.Monoidal qualified as Functor
import Data.Profunctor (Costrong (..), Profunctor (..))
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

instance (Monad m) => Trifunctor.Semigroupal (->) (,) (,) (,) (,) (MooreM m) where
  combine :: (MooreM m s i o, MooreM m t i' o') -> MooreM m (s, t) (i, i') (o, o')
  combine (MooreM m1, MooreM m2) =
    MooreM $ \(s, t) -> do
      (o, transition) <- m1 s
      (o', transition') <- m2 t
      pure ((o, o'), bimap transition transition')

instance (Monad m) => Trifunctor.Semigroupal (->) (,) Either (,) (,) (MooreM m) where
  combine :: (MooreM m s i o, MooreM m t i' o') -> MooreM m (s, t) (Either i i') (o, o')
  combine (MooreM m1, MooreM m2) =
    MooreM $ \(s, t) -> do
      (o, transition) <- m1 s
      (o', transition') <- m2 t
      pure ((o, o'), either ((,t) . transition) ((s,) . transition'))

instance (Monad m) => Trifunctor.Semigroupal (->) (,) These (,) (,) (MooreM m) where
  combine :: (MooreM m s i o, MooreM m t i' o') -> MooreM m (s, t) (These i i') (o, o')
  combine (MooreM m1, MooreM m2) =
    MooreM $ \(s, t) -> do
      (o, transition) <- m1 s
      (o', transition') <- m2 t
      pure ((o, o'), these ((,t) . transition) ((s,) . transition') (\i i' -> (transition i, transition' i')))

instance (Applicative m) => Trifunctor.Unital (->) () () () () (MooreM m) where
  introduce :: () -> MooreM m () () ()
  introduce () = MooreM $ \() -> pure ((), const ())

instance (Applicative m) => Trifunctor.Unital (->) () Void () () (MooreM m) where
  introduce :: () -> MooreM m () Void ()
  introduce () = MooreM $ \() -> pure ((), const ())

instance (Monad m) => Trifunctor.Monoidal (->) (,) () (,) () (,) () (,) () (MooreM m)

instance (Monad m) => Trifunctor.Monoidal (->) (,) () Either Void (,) () (,) () (MooreM m)

instance (Monad m) => Trifunctor.Monoidal (->) (,) () These Void (,) () (,) () (MooreM m)

instance (Functor m) => Profunctor (MooreM m s) where
  dimap :: (i' -> i) -> (o -> o') -> MooreM m s i o -> MooreM m s i' o'
  dimap f g (MooreM moore) = MooreM $ fmap (fmap (bimap g (lmap f))) moore

--------------------------------------------------------------------------------

-- | The fixed point of a 'MooreM' Machine. By taking the fixpoint we
-- are able to hide the state parameter @s@.
newtype MooreM' m i o = MooreM' {runMooreM' :: m (o, i -> MooreM' m i o)}
  deriving (Functor)

instance (Applicative m, Functor.Semigroupal (->) (,) (,) m) => Applicative (MooreM' m i) where
  pure :: o -> MooreM' m i o
  pure o = let r = MooreM' $ pure (o, const r) in r

  liftA2 :: (o -> o' -> o'') -> MooreM' m i o -> MooreM' m i o' -> MooreM' m i o''
  liftA2 f m1 m2 = dimap split (uncurry f) $ Bifunctor.combine @(->) @(,) @(,) @(,) (m1, m2)

-- TODO: Move to monoidal functors:
liftA2' :: (Functor m, Functor.Semigroupal (->) (,) (,) m) => (a -> b -> c) -> m a -> m b -> m c
liftA2' f m1 m2 = uncurry f <$> Functor.combine (m1, m2)

-- TODO: Move to monoidal functors:
-- biliftA2' :: (Bifunctor m, Bifunctor.Semigroupal (->) (,) (,) (,) m) => (a -> b -> c) -> (d -> e -> f) -> m a d -> m b e -> m c f
-- biliftA2' f g m1 m2 = bimap (uncurry f) (uncurry g) $ Bifunctor.combine (m1, m2)

instance (Functor m, Functor.Semigroupal (->) (,) (,) m) => Bifunctor.Semigroupal (->) (,) (,) (,) (MooreM' m) where
  combine :: (MooreM' m i o, MooreM' m i' o') -> MooreM' m (i, i') (o, o')
  combine (MooreM' m1, MooreM' m2) =
    MooreM' $ liftA2' (\(o, m1') (o', m2') -> ((o, o'), \(i, i') -> Bifunctor.combine (m1' i, m2' i'))) m1 m2

instance (Applicative m) => Bifunctor.Unital (->) () () () (MooreM' m) where
  introduce :: () -> MooreM' m () ()
  introduce () = MooreM' $ pure ((), Bifunctor.introduce)

instance (Applicative m) => Bifunctor.Unital (->) Void () () (MooreM' m) where
  introduce :: () -> MooreM' m Void ()
  introduce () = MooreM' $ pure ((), absurd)

instance (Applicative m, Functor.Semigroupal (->) (,) (,) m) => Bifunctor.Monoidal (->) (,) () (,) () (,) () (MooreM' m)

instance (Functor m) => Profunctor (MooreM' m) where
  dimap :: (i' -> i) -> (o -> o') -> MooreM' m i o -> MooreM' m i' o'
  dimap f g (MooreM' moore) =
    MooreM' $ fmap (bimap g (dimap f (dimap f g))) moore

instance (Monad m) => Costrong (MooreM' m) where
  unfirst :: MooreM' m (i, d) (o, d) -> MooreM' m i o
  unfirst (MooreM' moore) = MooreM' $ do
    ((o, d), moore') <- moore
    pure (o, \i -> unfirst (moore' (i, d)))

{-
instance Cosieve (MooreM' m) [] where
  cosieve :: MooreM' m i o -> [i] -> o
  cosieve (MooreM' (o, _)) [] = o
  cosieve (MooreM' (_, k)) (x : xs) = cosieve (k x) xs

instance Corepresentable (MooreM' m) where
  type Corep (MooreM' m) = []

  cotabulate :: (Corep (MooreM' m) i -> o) -> MooreM' m i o
  cotabulate f = MooreM' (f [], \i -> cotabulate (f . (i :)))

instance Closed (MooreM' m) where
  closed :: MooreM' m i o -> MooreM' m (x -> i) (x -> o)
  closed m = cotabulate $ \fs x -> cosieve m (fmap ($ x) fs)
-}

-- | Take the fixpoint of @MooreM s i o@ by recursively constructing an
-- @s -> MooreM' i o@ action and tupling it with the output observation
-- @o@ from its parent action.
fixMooreM :: forall m s o i. (Functor m) => MooreM m s o i -> s -> MooreM' m o i
fixMooreM (MooreM moore) = go
  where
    go :: s -> MooreM' m o i
    go s = MooreM' $ fmap (fmap go) <$> moore s

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

-- | Feed inputs into a 'Moore' Machine and then observe the final
-- result.
processMooreM :: (Monad m) => s -> [i] -> MooreM m s i o -> m o
processMooreM initialState inputs machine = do
  (o, transition) <- runMooreM machine initialState
  case inputs of
    [] -> pure o
    i : xs -> processMooreM (transition i) xs machine

-- | Feed inputs into a 'Moore'' Machine and then observe the final
-- result.
-- processMooreM' :: [i] -> MooreM' m i o -> o
-- processMooreM' = flip cosieve

--------------------------------------------------------------------------------
-- Tensors

infixr 9 /\

(/\) :: (Monad m) => MooreM m s i o -> MooreM m t i o' -> MooreM m (s, t) i (o, o')
(/\) m1 m2 = lmap split $ (Trifunctor.combine @_ @(,) @(,) @(,)) (m1, m2)

infixr 9 /+\

(/+\) :: (Monad m) => MooreM m s i o -> MooreM m t i' o' -> MooreM m (s, t) (These i i') (o, o')
(/+\) m1 m2 = Trifunctor.combine @_ @(,) @These @(,) (m1, m2)

infixr 9 \/

(\/) :: (Monad m) => MooreM m s i o -> MooreM m t i' o' -> MooreM m (s, t) (Either i i') (o, o')
(\/) m1 m2 = Trifunctor.combine @_ @(,) @Either @(,) (m1, m2)
