{-# language
    OverloadedStrings
#-}

module Main where

--------------------------------------------------------------------------------

import AgentMoore
import Data.Function ((&))
import Graph (Graph, edge, addEdge, setNodeLabel, setEdgeLabel, setEdgeWeight)
import Graph qualified as G
import Machines
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Dot.Text qualified as Dot

--------------------------------------------------------------------------------

main :: IO ()
main = do
  Text.putStrLn "extraSimpleTestGraph:"
  p extraSimpleTestGraph
  --print $ scanMealy 0 [Solved, Didn'tSolve] $ graphToMealy simpleObserve extraSimpleTestGraph
  pure ()

-- s -> [i] -> Mealy s i o -> [(o, s)]
type ProblemId = Word

data Input = Solved | Didn'tSolve
  deriving stock (Eq, Show)

data Observation = Understood | Didn'tUnderstand
  deriving stock (Eq, Show)

simpleObserve :: ProblemId -> Input -> Observation
simpleObserve _ = \case 
  Solved -> Understood
  Didn'tSolve -> Didn'tUnderstand

p :: Graph Word Text Observation ProblemId -> IO ()
p g = Text.putStrLn $ Dot.encode $ G.toDot (Text.pack . show) id (Text.pack . show) (Text.pack . show) g

extraSimpleTestGraph :: Graph Word Text Observation ProblemId
extraSimpleTestGraph = G.empty
  & addEdge (edge 0 1 & setNodeLabel "0" & setEdgeLabel Understood       & setEdgeWeight 10)
  & addEdge (edge 0 2 & setNodeLabel "0" & setEdgeLabel Didn'tUnderstand & setEdgeWeight 5)

testGraph :: Graph Word Text Observation ProblemId
testGraph = extraSimpleTestGraph
  & addEdge (edge 0 3 & setNodeLabel "0" & setEdgeLabel Understood       & setEdgeWeight 8)
  & addEdge (edge 0 4 & setNodeLabel "0" & setEdgeLabel Didn'tUnderstand & setEdgeWeight 3)
  & addEdge (edge 1 2 & setNodeLabel "1" & setEdgeLabel Understood       & setEdgeWeight 10)
  & addEdge (edge 1 3 & setNodeLabel "1" & setEdgeLabel Understood       & setEdgeWeight 3)