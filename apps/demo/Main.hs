{-# language
    BangPatterns
  , FlexibleContexts
  , MultiWayIf
  , OverloadedRecordDot
  , OverloadedStrings
  , ScopedTypeVariables
#-}

module Main where

--------------------------------------------------------------------------------

import AgentMoore (graphToMealy)
import Control.Exception (assert)
import Data.Function ((&))
import Graph (Graph, addEdge)
import Graph qualified as G
import Machines.Mealy (scanMealy, scanMealyM)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Dot.Text qualified as Dot
import Control.Monad.State (MonadState, modify, get, evalState)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  let testSelfLoopsRemoved =
        let lhs = scanMealy 0 [Solved, Didn'tSolve] (graphToMealy simpleObserve extraSimpleTestGraph)
            rhs = scanMealy 0 [Solved, Didn'tSolve] (graphToMealy simpleObserve (extraSimpleTestGraph & addEdge 0 0 4 Understood & addEdge 1 1 4 Didn'tUnderstand))
        in assert (lhs == rhs) (pure ())
  testSelfLoopsRemoved

  Text.putStrLn "\nextraSimpleTestGraph:" >> p extraSimpleTestGraph
  print $ scanMealy 0 [Solved, Didn'tSolve] $ graphToMealy simpleObserve extraSimpleTestGraph

  Text.putStrLn "\nsimpleTestGraph:" >> p simpleTestGraph
  print $ scanMealy 0 [Didn'tSolve, Solved, Solved, Solved, Didn'tSolve] $ graphToMealy simpleObserve simpleTestGraph

  Text.putStrLn "\ndemoGraph:" >> p demoGraph
  let path = flip evalState Map.empty $ scanMealyM node0 demoInputs $ graphToMealy observe demoGraph
  print path

type ProblemId = Word
type Weight = Word

-------------------------------------------------------------------------
-- "Simple" Graphs: Binary decision trees based on Solved/Didn't solve --
-------------------------------------------------------------------------

data SimpleInput = Solved | Didn'tSolve
  deriving stock (Eq, Show)

data SimpleObservation = Understood | Didn'tUnderstand
  deriving stock (Eq, Show)

simpleObserve :: Monad m => ProblemId -> SimpleInput -> m SimpleObservation
simpleObserve _ = \case 
  Solved -> pure Understood
  Didn'tSolve -> pure Didn'tUnderstand

type SimpleGraph = Graph Weight SimpleObservation ProblemId

extraSimpleTestGraph :: SimpleGraph
extraSimpleTestGraph = G.empty
  & addEdge 0 1 10 Understood
  & addEdge 0 2 5 Didn'tUnderstand

simpleTestGraph :: SimpleGraph
simpleTestGraph = extraSimpleTestGraph
  & addEdge 0 3 8 Understood
  & addEdge 0 4 3 Didn'tUnderstand
  & addEdge 1 2 10 Understood
  & addEdge 1 3 3 Understood
  & addEdge 2 0 9 Understood
  & addEdge 2 1 7 Didn'tUnderstand
  & addEdge 3 2 6 Understood
  & addEdge 3 4 5 Didn'tUnderstand
  & addEdge 4 2 6 Didn'tUnderstand
  & addEdge 4 0 2 Understood

---------------------------------------------------------
-- More complex logic, not just a binary decision tree --
---------------------------------------------------------

type History = Map ProblemType [ProblemStats]

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

modifyHistory :: MonadState History m => ProblemType -> ProblemStats -> m ()
modifyHistory ptype pstats = do
  modify $ \m -> Map.insertWith (++) ptype [pstats] m

historyNumAttempts :: MonadState History m => ProblemType -> m Word
historyNumAttempts ptype = do
  history <- get
  pure $ sum $ map numAttempts $ Map.findWithDefault [] ptype history

historyAvgNumAttempts :: MonadState History m => ProblemType -> m Word
historyAvgNumAttempts ptype = do
  history <- get
  let ps = Map.findWithDefault [] ptype history
  pure $ sum (map numAttempts ps) `div` (fromIntegral (length ps))

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

observe :: forall m. MonadState History m => Node -> Input -> m Observation
observe n0 i0 = do
  modifyHistory n0.problemType i0.stats
  if i0.stats.solved
  then observeSolved n0 i0
  else observeUnsolved n0 i0
  where
    observeSolved :: Node -> Input -> m Observation
    observeSolved n i
      | i.stats.numAttempts >= 3 = do
          avgNumAttempts <- historyAvgNumAttempts n.problemType
          pure $
            if | avgNumAttempts >= 3 -> NeedsDifferentProblemType
               | avgNumAttempts >= 2 -> NeedsMoreContext
               | otherwise           -> GoodGraspOnSubjectNeedsMorePractice
      | i.stats.numQuestionsAsked >= 10 = pure NeedsMoreContext
      | i.stats.timeSpent >= 60 * 10 = pure NeedsMoreContext
      | i.stats.timeSpent >= 60 * 5  = pure GoodGraspOnSubjectNeedsMorePractice
      | otherwise = pure GreatGraspOnSubjectCanMoveOn

    observeUnsolved :: Node -> Input -> m Observation
    observeUnsolved n i
      | i.stats.numAttempts >= 2 = do
          avgNumAttempts <- historyAvgNumAttempts n.problemType
          pure $
            if | avgNumAttempts >= 2 -> NeedsDifferentProblemType
               | otherwise           -> NeedsMoreContext
      | otherwise = pure NeedsMoreContext

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
  & addEdge node0 node1 10 NeedsMoreContext
  & addEdge node0 node2 10 GoodGraspOnSubjectNeedsMorePractice
  & addEdge node0 node4 10 NeedsDifferentProblemType
  & addEdge node0 node3 5 GreatGraspOnSubjectCanMoveOn
  & addEdge node1 node0 6 NeedsDifferentProblemType
  & addEdge node1 node2 7 NeedsMoreContext
  & addEdge node1 node3 6 GoodGraspOnSubjectNeedsMorePractice
  & addEdge node1 node4 3 GreatGraspOnSubjectCanMoveOn
  & addEdge node2 node1 6 GoodGraspOnSubjectNeedsMorePractice
  & addEdge node3 node2 13 NeedsMoreContext
  & addEdge node3 node4 9 GreatGraspOnSubjectCanMoveOn
  & addEdge node4 node0 3 GreatGraspOnSubjectCanMoveOn

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
