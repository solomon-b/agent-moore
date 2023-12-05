module Main where

--------------------------------------------------------------------------------

import AgentMoore
import Machines

--------------------------------------------------------------------------------

main :: IO ()
main = print $ scanMoore NA [E1, E3, E4] equivalentMoore
