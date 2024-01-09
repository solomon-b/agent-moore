module Machines
  ( module M,
    Fix (..),
    -- annihilate,
    loop,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.Identity
import Machines.Mealy as M hiding ((/+\), (/\), (\/))
import Machines.Moore as M hiding ((/+\), (/\), (\/))

--------------------------------------------------------------------------------

newtype Fix f = Fix {unFix :: f (Fix f)}

-- | "An Ay^B Mealy Machine is the 'universal thing' that interacts
-- with a By^A Moore Machine. Its the universal thing that can be put
-- together with a By^A Moore Machine. They're not just two different
-- definitions, they are dual in certain sense." -- David Spivak
-- annihilate :: Moore' i o -> Mealy' o i -> Fix Identity
-- annihilate (Moore' moore) (MealyM' mealy) =
--   Fix $ do
--     let (o, transition) = moore
--         (i, mealy') = mealy o
--         moore' = transition i
--     pure $ annihilate moore' mealy'

-- | Recursively unfold fixed point @Fix m@.
loop :: (Monad f) => Fix f -> f x
loop (Fix x) = x >>= loop
