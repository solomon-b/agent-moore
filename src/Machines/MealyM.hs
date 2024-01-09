{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Mealy Machines and related machinery.
module Machines.MealyM
  ( MealyM (..),
    MealyM' (..),
    fixMealyM,
    hoistBehavior,
    liftBehavior,
    scanMealyM,
    processMealyM,
    (/\),
    (\/),
    (/+\),
  )
where

--------------------------------------------------------------------------------

import Control.Category.Cartesian (split)
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (bimap, first)
import Data.Bifunctor.Monoidal qualified as Bifunctor
import Data.Profunctor (Choice (..), Profunctor, Strong (..))
import Data.Profunctor.Unsafe (Profunctor (..))
import Data.These
import Data.Trifunctor.Monoidal qualified as Trifunctor
import Data.Void (Void, absurd)

--------------------------------------------------------------------------------

-- | A Monadic Mealy Machine consists of:
--
--   * A finite set of states @S@
--   * An initial state @s : S@
--   * A monad @M@
--   * A finite set called the input @I@
--   * A finite set called the output @O@
--   * A function @transistion : S × I → M S@
--   * A function @observe : S × I → M O@
--
-- In this particular encoding combine the @transition@ and @Observe@
-- functions into @S × I → M (O × S)@. This definition is isomorphic.
newtype MealyM m s i o = MealyM {runMealy :: s -> i -> m (o, s)}
  deriving
    (Functor, Applicative, Monad, MonadState s, MonadReader i)
    via StateT s (ReaderT i m)

instance (Monad m) => Trifunctor.Semigroupal (->) (,) (,) (,) (,) (MealyM m) where
  combine :: (MealyM m s i o, MealyM m t i' o') -> MealyM m (s, t) (i, i') (o, o')
  combine (MealyM m1, MealyM m2) = MealyM $ \(s, t) (i, i') -> do
    (o, s') <- m1 s i
    (o', t') <- m2 t i'
    pure ((o, o'), (s', t'))

instance (Monad m) => Trifunctor.Semigroupal (->) (,) Either Either (,) (MealyM m) where
  combine :: (MealyM m s i o, MealyM m t i' o') -> MealyM m (s, t) (Either i i') (Either o o')
  combine (MealyM m1, MealyM m2) = MealyM $ \(s, t) -> \case
    Left i -> (bimap Left (,t) <$> m1 s i)
    Right i' -> bimap Right (s,) <$> m2 t i'

instance (Monad m) => Trifunctor.Semigroupal (->) (,) These These (,) (MealyM m) where
  combine :: (MealyM m s i o, MealyM m t i' o') -> MealyM m (s, t) (These i i') (These o o')
  combine (MealyM m1, MealyM m2) = MealyM $ \(s, t) -> \case
    This i -> bimap This (,t) <$> m1 s i
    That i' -> bimap That (s,) <$> m2 t i'
    These i i' -> do
      (o, s') <- m1 s i
      (o', t') <- m2 t i'
      pure (These o o', (s', t'))

instance (Applicative m) => Trifunctor.Unital (->) () () () () (MealyM m) where
  introduce :: () -> MealyM m () () ()
  introduce () = MealyM $ \() () -> pure ((), ())

instance Trifunctor.Unital (->) () Void Void () (MealyM m) where
  introduce :: () -> MealyM m () Void Void
  introduce () = MealyM $ \() -> absurd

instance (Monad m) => Trifunctor.Monoidal (->) (,) () (,) () (,) () (,) () (MealyM m)

instance (Monad m) => Trifunctor.Monoidal (->) (,) () Either Void Either Void (,) () (MealyM m)

instance (Monad m) => Trifunctor.Monoidal (->) (,) () These Void These Void (,) () (MealyM m)

instance (Functor m) => Profunctor (MealyM m s) where
  dimap :: (i' -> i) -> (o -> o') -> MealyM m s i o -> MealyM m s i' o'
  dimap f g (MealyM mealy) = MealyM $ fmap (dimap f (fmap (first g))) mealy

instance (Functor m) => Strong (MealyM m s) where
  first' :: MealyM m s i o -> MealyM m s (i, c) (o, c)
  first' (MealyM mealy) = MealyM $ \s (i, c) -> first (,c) <$> mealy s i

--------------------------------------------------------------------------------

-- | The fixed point of a 'Mealy' Machine. By taking the fixpoint we
-- are able to hide the state parameter @s@.
newtype MealyM' m i o = MealyM' {runMealyM' :: i -> m (o, MealyM' m i o)}
  deriving stock (Functor)

instance (Monad m) => Bifunctor.Semigroupal (->) (,) (,) (,) (MealyM' m) where
  combine :: (MealyM' m i o, MealyM' m i' o') -> MealyM' m (i, i') (o, o')
  combine (MealyM' m1, MealyM' m2) = MealyM' $ \(i, i') -> do
    (o, m1') <- m1 i
    (o', m2') <- m2 i'
    pure ((o, o'), Bifunctor.combine (m1', m2'))

instance (Applicative m) => Bifunctor.Unital (->) () () () (MealyM' m) where
  introduce :: () -> MealyM' m () ()
  introduce () = MealyM' $ \() -> pure ((), Bifunctor.introduce ())

instance (Monad m) => Bifunctor.Monoidal (->) (,) () (,) () (,) () (MealyM' m)

instance (Functor m) => Profunctor (MealyM' m) where
  dimap :: (i' -> i) -> (o -> o') -> MealyM' m i o -> MealyM' m i' o'
  dimap f g (MealyM' mealy) = MealyM' $ dimap f (fmap (bimap g (dimap f g))) mealy

instance (Functor m) => Strong (MealyM' m) where
  first' :: MealyM' m i o -> MealyM' m (i, x) (o, x)
  first' (MealyM' mealy) = MealyM' $ \(i, x) -> bimap (,x) first' <$> mealy i

instance (Applicative m) => Choice (MealyM' m) where
  left' :: MealyM' m i o -> MealyM' m (Either i x) (Either o x)
  left' (MealyM' mealy) = MealyM' $ either (fmap (bimap Left left') . mealy) (pure . (,left' (MealyM' mealy)) . Right)

-- | Take the fixpoint of @Mealy s i o@ by recursively constructing an
-- @s -> Mealy' i o@ action adn tupling it with the output observation
-- @o@ from its parent action.
fixMealyM :: forall m s i o. (Functor m) => MealyM m s i o -> s -> MealyM' m i o
fixMealyM (MealyM mealy) = go
  where
    go :: s -> MealyM' m i o
    go s = MealyM' (fmap (second' go) . mealy s)

-- | Lift a monad morphism from @m@ to @n@ into a monad morphism from
-- @MealyM' m s i o@ to @MealyM' n s i o@
hoistBehavior :: (Functor n, Functor m) => (forall x. m x -> n x) -> MealyM' m i o -> MealyM' n i o
hoistBehavior f (MealyM' mealy) = MealyM' $ \i -> f (fmap (hoistBehavior f) <$> mealy i)

-- | Lift a computation on the monad @m@ to the constructed monad @t
-- m@ in the context of a 'MealyM''.
liftBehavior :: (Functor (t m), Monad m, MonadTrans t) => MealyM' m i o -> MealyM' (t m) i o
liftBehavior = hoistBehavior lift

--------------------------------------------------------------------------------

-- | Feed inputs into a 'Mealy' Machine and extract the observation at
-- each state/input in a 'scan' style.
scanMealyM :: (Monad m) => s -> [i] -> MealyM m s i o -> m [(o, s)]
scanMealyM initialState inputs machine =
  case inputs of
    [] -> pure []
    input : xs -> do
      (o, s) <- runMealy machine initialState input
      ys <- scanMealyM s xs machine
      pure $ (o, s) : ys

-- | Feed inputs into a 'Mealy' Machine and then observe the final
-- result.
processMealyM :: (Monad m) => s -> [i] -> MealyM m s i o -> m o
processMealyM state' inputs machine =
  case inputs of
    [] -> undefined
    [input] -> fmap fst (runMealy machine state' input)
    input : xs -> do
      (_o, s) <- runMealy machine state' input
      processMealyM s xs machine

--------------------------------------------------------------------------------
-- Monoidal

infixr 9 /\

(/\) :: (Monad m) => MealyM m s i o -> MealyM m t i o' -> MealyM m (s, t) i (o, o')
(/\) m1 m2 = lmap split $ Trifunctor.combine @_ @(,) @(,) @(,) (m1, m2)

infixr 9 /+\

(/+\) :: (Monad m) => MealyM m s i o -> MealyM m t i' o' -> MealyM m (s, t) (These i i') (These o o')
(/+\) m1 m2 = Trifunctor.combine @_ @(,) @These @These (m1, m2)

infixr 9 \/

(\/) :: (Monad m) => MealyM m s i o -> MealyM m t i' o' -> MealyM m (s, t) (Either i i') (Either o o')
(\/) m1 m2 = Trifunctor.combine @_ @(,) @Either @Either (m1, m2)
