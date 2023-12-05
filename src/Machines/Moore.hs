{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Moore Machines and related machinery.
module Machines.Moore
  ( Moore (..),
    Moore' (..),
    fixMoore,
    scanMoore,
    processMoore,
    (/\),
  )
where

--------------------------------------------------------------------------------

import Control.Applicative (liftA2)
import Data.Bifunctor (bimap)
import Data.Profunctor (Profunctor (..))

--------------------------------------------------------------------------------

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
newtype Moore s i o = Moore {runMoore :: s -> (o, i -> s)}
  deriving (Functor)

instance Profunctor (Moore s) where
  dimap :: (i' -> i) -> (o -> o') -> Moore s i o -> Moore s i' o'
  dimap f g (Moore moore) = Moore $ fmap (bimap g (lmap f)) moore

-- | The fixed point of a 'Moore' Machine. By taking the fixpoint we
-- are able to hide the state parameter @s@.
newtype Moore' i o = Moore' {runMoore' :: (o, i -> Moore' i o)}
  deriving (Functor)

instance Applicative (Moore' i) where
  pure :: o -> Moore' i o
  pure o = let r = Moore' (o, const r) in r

  liftA2 :: (o -> o' -> o'') -> Moore' i o -> Moore' i o' -> Moore' i o''
  liftA2 f (Moore' (a, m1)) (Moore' (b, m2)) = Moore' (f a b, \i -> liftA2 f (m1 i) (m2 i))

instance Monad (Moore' i) where
  return :: o -> Moore' i o
  return = pure

  (>>=) :: Moore' i o -> (o -> Moore' i o') -> Moore' i o'
  m >>= f =
    let join' (Moore' (a, g)) = Moore' (fst $ runMoore' a, \i -> join' (($ i) . snd . runMoore' <$> g i))
     in join' (fmap f m)

instance Profunctor Moore' where
  dimap :: (i' -> i) -> (o -> o') -> Moore' i o -> Moore' i' o'
  dimap f g (Moore' serve) =
    Moore' $ bimap g (dimap f (dimap f g)) serve

-- | Take the fixpoint of @Moore s i o@ by recursively constructing an
-- @s -> Moore' i o@ action and tupling it with the output observation
-- @o@ from its parent action.
fixMoore :: forall s o i. Moore s o i -> s -> Moore' o i
fixMoore (Moore moore) = go
  where
    go :: s -> Moore' o i
    go s = Moore' $ fmap go <$> moore s

-- | Feed inputs into a 'Moore' Machine and extract the observation at
-- each state/input in a 'scan' style.
scanMoore :: s -> [i] -> Moore s i o -> [(o, s)]
scanMoore state' inputs machine =
  let (o, transition) = runMoore machine state'
   in case inputs of
        [] -> [(o, state')]
        i : xs -> (o, state') : scanMoore (transition i) xs machine

-- | Feed inputs into a 'Moore' Machine and then observe the final
-- result.
processMoore :: s -> [i] -> Moore s i o -> o
processMoore initialState inputs machine =
  let (o, transition) = runMoore machine initialState
   in case inputs of
        [] -> o
        i : xs -> processMoore (transition i) xs machine

--------------------------------------------------------------------------------
-- Monoidal

infixr 9 /\

(/\) :: Moore s i o -> Moore t i o' -> Moore (s, t) i (o, o')
(/\) (Moore m1) (Moore m2) =
  Moore $ \(s, t) ->
    let (o, transition) = m1 s
        (o', transition') = m2 t
     in ((o, o'), \i -> (transition i, transition' i))
