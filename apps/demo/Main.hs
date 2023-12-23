{-# language
    BangPatterns
  , MultiWayIf
  , OverloadedRecordDot
  , OverloadedStrings
#-}

module Main where

--------------------------------------------------------------------------------

import AgentMoore
import Control.Exception (assert)
import Data.Function ((&))
import Data.IORef
import Graph (Graph, edge, addEdge, setEdgeLabel, setEdgeWeight)
import Graph qualified as G
import Machines
import Data.Map (Map)
import Data.Map.Strict qualified as Map
--import Data.Maybe (fromMaybe)
--import Data.Set (Set)
--import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Dot.Text qualified as Dot
import System.IO.Unsafe (unsafePerformIO)

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

  Text.putStrLn "\ndemoGraph:" >> p demoGraph
  print $ scanMealy node0 demoInputs $ graphToMealy (flip observe) demoGraph

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

type SimpleGraph = Graph Weight SimpleObservation ProblemId

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

---------------------------------------------------------
-- More complex logic, not just a binary decision tree --
---------------------------------------------------------

type History = Map ProblemType [ProblemStats]

history :: IORef History
history = unsafePerformIO $ newIORef Map.empty
{-# noinline history #-}

data ProblemStats = ProblemStats
  { numAttempts :: Word
    -- ^ Number of attempts on the problem
  , numQuestionsAsked :: Word
    -- ^ Number of questions asked to the assistant about the problem
  -- , progressionScore :: Double
    -- ^ A number in [0, 1]. Vague notion of how much understanding the
    --   student gained by asking questions/repeated attempts of the problem.
  , timeSpent :: Word
    -- ^ Elapsed seconds spent on the problem
  , solved :: Bool
    -- ^ Whether or not the problem was solved
  }
  deriving stock (Eq, Show)

data Input = Input
  { stats :: ProblemStats
  -- , history :: Map ProblemType ProblemStats
  }

accessHistory :: (History -> x) -> x
accessHistory f = unsafePerformIO $ do
  h <- readIORef history
  pure $ f h
{-# noinline accessHistory #-}

modifyHistory :: ProblemType -> ProblemStats -> a -> a
modifyHistory ptype pstats x = unsafePerformIO $ do
  modifyIORef' history $ \m -> Map.insertWith (++) ptype [pstats] m
  pure $! x
{-# noinline modifyHistory #-}

historyNumAttempts :: ProblemType -> Word
historyNumAttempts ptype = accessHistory $ \h -> case Map.lookup ptype h of
  Nothing -> 0
  Just ps -> sum (map numAttempts ps)

historyAvgNumAttempts :: ProblemType -> Word
historyAvgNumAttempts ptype = accessHistory $ \h -> case Map.lookup ptype h of
  Nothing -> 0
  Just ps -> sum (map numAttempts ps) `div` (fromIntegral $ length ps)

data Observation
  = NeedsMoreContext
    -- ^ Learner needs more context on the problem. This could mean additional problems or reading.
  | NeedsDifferentProblemType
    -- ^ Learner doesn't do well with this problem type, so we should give them a different one.
  | GoodGraspOnSubjectNeedsMorePractice
    -- ^ Learner has a decent grasp on the subject, could tackle more problems to solidify understanding.
  | GreatGraspOnSubjectCanMoveOn
    -- ^ Learner has a solid understanding of the subject. They can likely move on, or just do additional practice.
  deriving stock (Eq, Show)

data ProblemType
  = VisualProblem
  | WordProblem
  deriving stock (Eq, Ord, Show)

subscriptProblemType :: ProblemType -> String
subscriptProblemType = \case 
  VisualProblem -> "v"
  WordProblem -> "w"

data Node = Node
  { problemId :: ProblemId
  , problemType :: ProblemType
  -- , goodVibes :: Set Node -- nodes that are suspected to work well with this node to progress the student
  -- , badVibes :: Set Node -- nodes that are suspected to work poorly with this node to progress the student
  }
  deriving stock (Eq, Ord)

instance Show Node where
  show n = show n.problemId ++ subscriptProblemType n.problemType

observe :: Input -> Node -> Observation
observe i0 n0
  | i0.stats.solved = modifyHistory n0.problemType i0.stats $ observeSolved i0 n0
  | otherwise = modifyHistory n0.problemType i0.stats $ observeUnsolved i0 n0
  where
    observeSolved :: Input -> Node -> Observation
    observeSolved i n
      | i.stats.numAttempts >= 3 =
          if | historyAvgNumAttempts n.problemType >= 3 -> NeedsDifferentProblemType
             | historyAvgNumAttempts n.problemType >= 2 -> NeedsMoreContext
             | otherwise                                -> GoodGraspOnSubjectNeedsMorePractice
      | i.stats.numQuestionsAsked >= 10 = NeedsMoreContext
      | i.stats.timeSpent >= 60 * 10 = NeedsMoreContext
      | i.stats.timeSpent >= 60 * 5  = GoodGraspOnSubjectNeedsMorePractice
      | otherwise = GreatGraspOnSubjectCanMoveOn

    observeUnsolved :: Input -> Node -> Observation
    observeUnsolved i n
      | i.stats.numAttempts >= 2 =
          if | historyAvgNumAttempts n.problemType >= 2 -> NeedsDifferentProblemType
             | otherwise                                -> NeedsMoreContext
      | otherwise = NeedsMoreContext

type DemoGraph = Graph Weight Observation Node

demoInputs :: [Input]
demoInputs =
  [ Input $ ProblemStats
      { numAttempts = 3
      , numQuestionsAsked = 6
      , timeSpent = 360 -- 6m
      , solved = False
      }
  , Input $ ProblemStats
      { numAttempts = 1
      , numQuestionsAsked = 2
      , timeSpent = 100 -- 1m40s
      , solved = True
      }
  , Input $ ProblemStats
      { numAttempts = 2
      , numQuestionsAsked = 1
      , timeSpent = 30 -- 30s
      , solved = True
      }
  , Input $ ProblemStats
      { numAttempts = 1
      , numQuestionsAsked = 10
      , timeSpent = 300 -- 6m
      , solved = False
      }
  ]

demoGraph :: DemoGraph
demoGraph = G.empty
  & addEdge (edge node0 node1 & setEdgeLabel NeedsMoreContext                    & setEdgeWeight 10)
  & addEdge (edge node0 node2 & setEdgeLabel GoodGraspOnSubjectNeedsMorePractice & setEdgeWeight 10)
  & addEdge (edge node0 node4 & setEdgeLabel NeedsDifferentProblemType           & setEdgeWeight 10)
  & addEdge (edge node0 node3 & setEdgeLabel GreatGraspOnSubjectCanMoveOn        & setEdgeWeight 5)
  & addEdge (edge node1 node0 & setEdgeLabel NeedsDifferentProblemType           & setEdgeWeight 6)
  & addEdge (edge node1 node2 & setEdgeLabel NeedsMoreContext                    & setEdgeWeight 7)
  & addEdge (edge node1 node3 & setEdgeLabel GoodGraspOnSubjectNeedsMorePractice & setEdgeWeight 6)
  & addEdge (edge node1 node4 & setEdgeLabel GreatGraspOnSubjectCanMoveOn        & setEdgeWeight 3)
  & addEdge (edge node2 node1 & setEdgeLabel GoodGraspOnSubjectNeedsMorePractice & setEdgeWeight 6)
  & addEdge (edge node3 node2 & setEdgeLabel NeedsMoreContext                    & setEdgeWeight 13)
  & addEdge (edge node3 node4 & setEdgeLabel GreatGraspOnSubjectCanMoveOn        & setEdgeWeight 9)
  & addEdge (edge node4 node0 & setEdgeLabel GreatGraspOnSubjectCanMoveOn        & setEdgeWeight 3)

node0 :: Node
node0 = Node 0 VisualProblem

node1 :: Node
node1 = Node 1 WordProblem

node2 :: Node
node2 = Node 2 VisualProblem

node3 :: Node
node3 = Node 3 VisualProblem

node4 :: Node
node4 = Node 4 WordProblem

p :: (Show w, Show el, Show n, Ord n) => Graph w el n -> IO ()
p g = Text.putStrLn $ Dot.encode $ G.toDot (Text.pack . show) (Text.pack . show) (Text.pack . show) g