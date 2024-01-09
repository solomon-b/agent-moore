{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Moore Machines and related machinery.
module Machines.Moore
  ( Moore,
    Moore',
    fixMoore,
    scanMoore,
    processMoore,
    processMoore',
    module Machines.MooreM,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.Identity (Identity (..))
import Data.Profunctor (Closed (..))
import Data.Profunctor.Rep (Corepresentable (..))
import Data.Profunctor.Sieve (Cosieve (..))
import Machines.MooreM

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
type Moore = MooreM Identity

--------------------------------------------------------------------------------

-- | The fixed point of a 'Moore' Machine. By taking the fixpoint we
-- are able to hide the state parameter @s@.
type Moore' = MooreM' Identity

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

-- | Take the fixpoint of @Moore s i o@ by recursively constructing an
-- @s -> Moore' i o@ action and tupling it with the output observation
-- @o@ from its parent action.
fixMoore :: forall s o i. Moore s o i -> s -> Moore' o i
fixMoore = fixMooreM

--------------------------------------------------------------------------------

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
processMoore :: s -> [i] -> Moore s i o -> o
processMoore initialState inputs machine =
  let (o, transition) = runIdentity $ runMooreM machine initialState
   in case inputs of
        [] -> o
        i : xs -> processMoore (transition i) xs machine

-- | Feed inputs into a 'Moore'' Machine and then observe the final
-- result.
processMoore' :: [i] -> Moore' i o -> o
processMoore' = flip cosieve
