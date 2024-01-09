{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Mealy Machines and related machinery.
module Machines.Mealy
  ( Mealy,
    Mealy',
    fixMealy,
    scanMealy,
    processMealy,
    module Machines.MealyM,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.Identity
import Machines.MealyM

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
type Mealy = MealyM Identity

--------------------------------------------------------------------------------

-- | The fixed point of a 'Mealy' Machine. By taking the fixpoint we
-- are able to hide the state parameter @s@.
type Mealy' = MealyM' Identity

-- | Take the fixpoint of @Mealy s i o@ by recursively constructing an
-- @s -> Mealy' i o@ action adn tupling it with the output observation
-- @o@ from its parent action.
fixMealy :: forall s i o. Mealy s i o -> s -> Mealy' i o
fixMealy = fixMealyM

--------------------------------------------------------------------------------

-- | Feed inputs into a 'Mealy' Machine and extract the observation at
-- each state/input in a 'scan' style.
scanMealy :: s -> [i] -> Mealy s i o -> [(o, s)]
scanMealy initialState inputs machine = runIdentity $ scanMealyM initialState inputs machine

-- | Feed inputs into a 'Mealy' Machine and then observe the final
-- result.
processMealy :: s -> [i] -> Mealy s i o -> o
processMealy state' inputs machine = runIdentity $ processMealyM state' inputs machine
