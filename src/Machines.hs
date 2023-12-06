module Machines
  ( module M,
    Fix (..),
    annihilate,
    loop,
  )
where

--------------------------------------------------------------------------------

import Machines.Mealy as M hiding ((/+\), (/\))
import Machines.Moore as M hiding ((/+\), (/\))

--------------------------------------------------------------------------------

newtype Fix f = Fix {unFix :: f (Fix f)}

-- | "An Ay^B Mealy Machine is the 'universal thing' that interacts
-- with a By^A Moore Machine. Its the universal thing that can be put
-- together with a By^A Moore Machine. They're not just two different
-- definitions, they are dual in certain sense." -- David Spivak
annihilate :: (Monad f) => (o -> f ()) -> Moore' i o -> Mealy' o i -> Fix f
annihilate act (Moore' moore) (Mealy' mealy) =
  Fix $ do
    let (o, transition) = moore
        (i, mealy') = mealy o
        moore' = transition i
    act o
    pure $ annihilate act moore' mealy'

-- | Recursively unfold fixed point @Fix m@.
loop :: (Monad f) => Fix f -> f x
loop (Fix x) = x >>= loop
