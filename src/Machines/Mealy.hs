{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Mealy Machines and related machinery.
module Machines.Mealy
  ( Mealy (..),
    Mealy' (..),
    fixMealy,
    scanMealy,
    processMealy,
    (/\),
    (/+\),
  )
where

--------------------------------------------------------------------------------

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (bimap, first)
import Data.Profunctor (Choice (..), Profunctor, Strong (..))
import Data.Profunctor.Unsafe (Profunctor (..))
import Data.These

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

instance Profunctor (Mealy s) where
  dimap :: (i' -> i) -> (o -> o') -> Mealy s i o -> Mealy s i' o'
  dimap f g (Mealy mealy) = Mealy $ fmap (dimap f (first g)) mealy

instance Strong (Mealy s) where
  first' :: Mealy s i o -> Mealy s (i, c) (o, c)
  first' (Mealy mealy) = Mealy $ \s (i, c) -> first (,c) $ mealy s i

-- | The fixed point of a 'Mealy' Machine. By taking the fixpoint we
-- are able to hide the state parameter @s@.
newtype Mealy' i o = Mealy' {runMealy' :: i -> (o, Mealy' i o)}
  deriving stock (Functor)

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
(/\) (Mealy b1) (Mealy b2) = Mealy $ \(s, t) i ->
  let (o, s') = b1 s i
      (o', t') = b2 t i
   in (,) (o, o') (s', t')

infixr 9 /+\

(/+\) :: Mealy s i o -> Mealy t i' o' -> Mealy (s, t) (These i i') (These o o')
(/+\) (Mealy m1) (Mealy m2) = Mealy $ \(s, t) -> \case
  This i -> bimap This (,t) $ m1 s i
  That i' -> bimap That (s,) $ m2 t i'
  These i i' ->
    let (o, s') = m1 s i
        (o', t') = m2 t i'
     in (These o o', (s', t'))
