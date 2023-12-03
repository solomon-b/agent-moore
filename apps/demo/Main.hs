module Main where

--------------------------------------------------------------------------------

import AgentMoore

--------------------------------------------------------------------------------

main :: IO ()
main = print $ processMoore NA [E1, E3, E4] equivalentMoore
