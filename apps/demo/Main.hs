{-# language
    OverloadedRecordDot
  , OverloadedStrings
#-}

module Main where

--------------------------------------------------------------------------------

import AgentMoore
import Control.Exception (assert)
import Data.Function ((&))
import Graph (Graph, edge, addEdge, setNodeLabel, setEdgeLabel, setEdgeWeight)
import Graph qualified as G
import Machines
import Data.Map (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Dot.Text qualified as Dot

--------------------------------------------------------------------------------

main :: IO ()
main = do
  let testSelfLoopsRemoved =
        let lhs = scanMealy 0 [Solved, Didn'tSolve] (graphToMealy simpleObserve extraSimpleTestGraph)
            rhs = scanMealy 0 [Solved, Didn'tSolve] (graphToMealy simpleObserve (extraSimpleTestGraph & addEdge (edge 0 0) & addEdge (edge 1 1)))
        in assert (lhs == rhs) (pure ())
  testSelfLoopsRemoved

  Text.putStrLn "\nextraSimpleTestGraph:" >> p extraSimpleTestGraph
  print $ scanMealy 0 [Solved, Didn'tSolve] $ graphToMealy simpleObserve extraSimpleTestGraph

  Text.putStrLn "\nsimpleTestGraph:" >> p simpleTestGraph
  print $ scanMealy 0 [Didn'tSolve, Solved, Solved, Solved, Didn'tSolve] $ graphToMealy simpleObserve simpleTestGraph

type ProblemId = Word
type Weight = Word

-------------------------------------------------------------------------
-- "Simple" Graphs: Binary decision trees based on Solved/Didn't solve --
-------------------------------------------------------------------------

data SimpleInput = Solved | Didn'tSolve
  deriving stock (Eq, Show)

data SimpleObservation = Understood | Didn'tUnderstand
  deriving stock (Eq, Show)

simpleObserve :: ProblemId -> SimpleInput -> SimpleObservation
simpleObserve _ = \case 
  Solved -> Understood
  Didn'tSolve -> Didn'tUnderstand

type SimpleGraph = Graph Weight Text SimpleObservation ProblemId

extraSimpleTestGraph :: SimpleGraph
extraSimpleTestGraph = G.empty
  & addEdge (edge 0 1 & setEdgeLabel Understood       & setEdgeWeight 10)
  & addEdge (edge 0 2 & setEdgeLabel Didn'tUnderstand & setEdgeWeight 5)

simpleTestGraph :: SimpleGraph
simpleTestGraph = extraSimpleTestGraph
  & addEdge (edge 0 3 & setEdgeLabel Understood       & setEdgeWeight 8)
  & addEdge (edge 0 4 & setEdgeLabel Didn'tUnderstand & setEdgeWeight 3)
  & addEdge (edge 1 2 & setEdgeLabel Understood       & setEdgeWeight 10)
  & addEdge (edge 1 3 & setEdgeLabel Understood       & setEdgeWeight 3)
  & addEdge (edge 2 0 & setEdgeLabel Understood       & setEdgeWeight 9)
  & addEdge (edge 2 1 & setEdgeLabel Didn'tUnderstand & setEdgeWeight 7)
  & addEdge (edge 3 2 & setEdgeLabel Understood       & setEdgeWeight 6)
  & addEdge (edge 3 4 & setEdgeLabel Didn'tUnderstand & setEdgeWeight 5)
  & addEdge (edge 4 2 & setEdgeLabel Didn'tUnderstand & setEdgeWeight 6)
  & addEdge (edge 4 0 & setEdgeLabel Understood       & setEdgeWeight 2)

-------------------------------------------------
--                                             --
-------------------------------------------------

data ProblemStats = ProblemStats
  { numAttempts :: Word
  , numQuestionsAsked :: Word
  , progressionScore :: Double -- in [0, 1]. vague notion of how much understanding the student gained by asking questions/repeated attempts of the problem
  , timeSpent :: Word -- elapsed seconds
  , solved :: Bool
  }
  deriving stock (Eq, Show)

data Input = Input
  { stats :: ProblemStats
  , history :: Map ProblemType ProblemStats
  }

data Observation
  = NeedsMoreContext
  | NeedsDifferentProblemType
  | GoodGraspOnSubjectNeedsMorePractice
  | GreatGraspOnSubjectCanMoveOn
  deriving stock (Eq, Show)

data ProblemType
  = VisualProblem
  | WordProblem
  deriving stock (Eq, Show)

data Node = Node
  { problemId :: ProblemId
  , problemType :: ProblemType
  -- , goodVibes :: Set Node -- nodes that are suspected to work well with this node to progress the student
  -- , badVibes :: Set Node -- nodes that are suspected to work poorly with this node to progress the student
  }
  deriving stock (Eq, Show)

observe :: Input -> Node -> Observation
observe i n
  | i.stats.solved = observeSolved i n
  | otherwise = observeUnsolved i n
  where
    observeSolved :: Input -> Node -> Observation
    observeSolved i n = undefined

    observeUnsolved :: Input -> Node -> Observation
    observeUnsolved i n = undefined

{-
data Input = Input
  { numAttempts :: Word
  , numQuestionsAsked :: Word
  , progressionScore :: Double -- in [0, 1]. vague notion of how much understanding the student gained by asking questions/repeated attempts of the problem
  , timeSpent :: Word -- elapsed seconds
  , solved :: Word
  }
  deriving stock (Eq, Show)

data Observation
  = NeedsMoreContext
  | NeedsDifferentProblemType
  | GoodGraspOnSubjectNeedsMorePractice
  | GreatGraspOnSubjectCanMoveOn
-}


p :: (Show w, Show el, Show n, Ord n) => Graph w Text el n -> IO ()
p g = Text.putStrLn $ Dot.encode $ G.toDot (Text.pack . show) id (Text.pack . show) (Text.pack . show) g