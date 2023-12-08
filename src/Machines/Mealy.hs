{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Mealy Machines and related machinery.
module Machines.Mealy
  ( Mealy (..),
    Mealy' (..),
    fixMealy,
    scanMealy,
    processMealy,
    (/\),
    (\/),
    (/+\),
  )
where

--------------------------------------------------------------------------------

import Control.Category.Cartesian (split)
import Control.Monad.Identity
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

-- | A Mealy Machine consists of:
--
--   * A finite set of states @S@
--   * An initial state @s : S@
--   * A finite set called the input @I@
--   * A finite set called the output @O@
--   * A function @transistion : S × I → S@
--   * A function @observe : S × I → O@
--
-- In this particular encoding combine the @transition@ and @Observe@
-- functions into @S × I → O × S@. This definition is isomorphic.
newtype Mealy s i o = Mealy {runMealy :: s -> i -> (o, s)}
  deriving
    (Functor, Applicative, Monad, MonadState s, MonadReader i)
    via StateT s (ReaderT i Identity)

instance Trifunctor.Semigroupal (->) (,) (,) (,) (,) Mealy where
  combine :: (Mealy s i o, Mealy t i' o') -> Mealy (s, t) (i, i') (o, o')
  combine (Mealy m1, Mealy m2) = Mealy $ \(s, t) (i, i') ->
    let (o, s') = m1 s i
        (o', t') = m2 t i'
     in ((o, o'), (s', t'))

instance Trifunctor.Semigroupal (->) (,) Either Either (,) Mealy where
  combine :: (Mealy s i o, Mealy t i' o') -> Mealy (s, t) (Either i i') (Either o o')
  combine (Mealy m1, Mealy m2) = Mealy $ \(s, t) -> \case
    Left i -> bimap Left (,t) $ m1 s i
    Right i' -> bimap Right (s,) $ m2 t i'

instance Trifunctor.Semigroupal (->) (,) These These (,) Mealy where
  combine :: (Mealy s i o, Mealy t i' o') -> Mealy (s, t) (These i i') (These o o')
  combine (Mealy m1, Mealy m2) = Mealy $ \(s, t) -> \case
    This i -> bimap This (,t) $ m1 s i
    That i' -> bimap That (s,) $ m2 t i'
    These i i' ->
      let (o, s') = m1 s i
          (o', t') = m2 t i'
       in (These o o', (s', t'))

instance Trifunctor.Unital (->) () () () () Mealy where
  introduce :: () -> Mealy () () ()
  introduce () = Mealy $ \() () -> ((), ())

instance Trifunctor.Unital (->) () Void Void () Mealy where
  introduce :: () -> Mealy () Void Void
  introduce () = Mealy $ \() -> absurd

instance Trifunctor.Monoidal (->) (,) () (,) () (,) () (,) () Mealy

instance Trifunctor.Monoidal (->) (,) () Either Void Either Void (,) () Mealy

instance Trifunctor.Monoidal (->) (,) () These Void These Void (,) () Mealy

instance Profunctor (Mealy s) where
  dimap :: (i' -> i) -> (o -> o') -> Mealy s i o -> Mealy s i' o'
  dimap f g (Mealy mealy) = Mealy $ fmap (dimap f (first g)) mealy

instance Strong (Mealy s) where
  first' :: Mealy s i o -> Mealy s (i, c) (o, c)
  first' (Mealy mealy) = Mealy $ \s (i, c) -> first (,c) $ mealy s i

--------------------------------------------------------------------------------

-- | The fixed point of a 'Mealy' Machine. By taking the fixpoint we
-- are able to hide the state parameter @s@.
newtype Mealy' i o = Mealy' {runMealy' :: i -> (o, Mealy' i o)}
  deriving stock (Functor)

instance Bifunctor.Semigroupal (->) (,) (,) (,) Mealy' where
  combine :: (Mealy' i o, Mealy' i' o') -> Mealy' (i, i') (o, o')
  combine (Mealy' m1, Mealy' m2) = Mealy' $ \(i, i') ->
    let (o, m1') = m1 i
        (o', m2') = m2 i'
     in ((o, o'), Bifunctor.combine (m1', m2'))

instance Bifunctor.Unital (->) () () () Mealy' where
  introduce :: () -> Mealy' () ()
  introduce () = Mealy' $ \() -> ((), Bifunctor.introduce ())

instance Bifunctor.Monoidal (->) (,) () (,) () (,) () Mealy'

instance Profunctor Mealy' where
  dimap :: (i' -> i) -> (o -> o') -> Mealy' i o -> Mealy' i' o'
  dimap f g (Mealy' mealy) = Mealy' $ dimap f (bimap g (dimap f g)) mealy

instance Strong Mealy' where
  first' :: Mealy' i o -> Mealy' (i, x) (o, x)
  first' (Mealy' mealy) = Mealy' $ \(i, x) -> bimap (,x) first' $ mealy i

instance Choice Mealy' where
  left' :: Mealy' i o -> Mealy' (Either i x) (Either o x)
  left' (Mealy' mealy) = Mealy' $ either (bimap Left left' . mealy) ((,left' (Mealy' mealy)) . Right)

-- | Take the fixpoint of @Mealy s i o@ by recursively constructing an
-- @s -> Mealy' i o@ action adn tupling it with the output observation
-- @o@ from its parent action.
fixMealy :: forall s i o. Mealy s i o -> s -> Mealy' i o
fixMealy (Mealy mealy) = go
  where
    go :: s -> Mealy' i o
    go s = Mealy' (fmap go . mealy s)

--------------------------------------------------------------------------------

-- | Feed inputs into a 'Mealy' Machine and extract the observation at
-- each state/input in a 'scan' style.
scanMealy :: s -> [i] -> Mealy s i o -> [(o, s)]
scanMealy initialState inputs machine =
  case inputs of
    [] -> []
    input : xs ->
      let (o, s) = runMealy machine initialState input
       in (o, s) : scanMealy s xs machine

-- | Feed inputs into a 'Mealy' Machine and then observe the final
-- result.
processMealy :: s -> [i] -> Mealy s i o -> o
processMealy state' inputs machine =
  case inputs of
    [] -> undefined
    [input] -> fst (runMealy machine state' input)
    input : xs ->
      let (_o, s) = runMealy machine state' input
       in processMealy s xs machine

--------------------------------------------------------------------------------
-- Monoidal

infixr 9 /\

(/\) :: Mealy s i o -> Mealy t i o' -> Mealy (s, t) i (o, o')
(/\) m1 m2 = lmap split $ Trifunctor.combine @_ @(,) @(,) @(,) (m1, m2)

infixr 9 /+\

(/+\) :: Mealy s i o -> Mealy t i' o' -> Mealy (s, t) (These i i') (These o o')
(/+\) m1 m2 = Trifunctor.combine @_ @(,) @These @These (m1, m2)

infixr 9 \/

(\/) :: Mealy s i o -> Mealy t i' o' -> Mealy (s, t) (Either i i') (Either o o')
(\/) m1 m2 = Trifunctor.combine @_ @(,) @Either @Either (m1, m2)
