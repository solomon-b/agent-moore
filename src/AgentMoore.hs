{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module AgentMoore where

--------------------------------------------------------------------------------

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Profunctor

--------------------------------------------------------------------------------
-- Machines

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
newtype Mealy s i o = Mealy {runMealy :: s -> i -> (o, s)}
  deriving
    (Functor, Applicative, Monad, MonadState s, MonadReader i)
    via StateT s (ReaderT i Identity)

instance Profunctor (Mealy s) where
  dimap :: (i' -> i) -> (o -> o') -> Mealy s i o -> Mealy s i' o'
  dimap f g (Mealy mealy) = Mealy $ fmap (dimap f (first g)) mealy

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
newtype Moore s i o = Moore {runMoore :: s -> (o, i -> s)}
  deriving (Functor)

instance Profunctor (Moore s) where
  dimap :: (i' -> i) -> (o -> o') -> Moore s i o -> Moore s i' o'
  dimap f g (Moore moore) = Moore $ fmap (bimap g (lmap f)) moore

-- | Feed inputs into a 'Moore' Machine and then observe the result.
processMoore :: s -> [i] -> Moore s i o -> o
processMoore initialState inputs machine =
  let (o, transition) = runMoore machine initialState
  in case inputs of
    [] -> o
    i : xs -> processMoore (transition i) xs machine

--------------------------------------------------------------------------------

data S = A | B | C

data I = I0 | I1

data O = O0 | O1

-- | Sample machine for this diagram:
-- http://image.slideserve.com/657197/mealy-machine-example-l.jpg
exampleMealy :: Mealy S I O
exampleMealy = Mealy $ curry \case
  (A, I0) -> (O1, B)
  (A, I1) -> (O0, C)
  (B, I0) -> (O0, B)
  (B, I1) -> (O1, A)
  (C, I0) -> (O0, A)
  (C, I1) -> (O0, C)

--------------------------------------------------------------------------------
-- DAG

-- | Lets use the same Fin3 to represent a DAG with 3 nodes:
data Node = NA | NB | NC | ND | NE
  deriving (Show, Eq, Ord)

-- | Edge Labels
data Edge = E1 | E2 | E3 | E4

type DAG n o = Map n [(n, o)]

type Graph = DAG Node String

-- Convert a DAG to a DOT format string
dagToDot :: Graph -> String
dagToDot graph = "digraph G {\n" ++ concatMap edgesToDot (Map.toList graph) ++ "}"

-- Convert a node and its adjacent nodes to DOT format strings
edgesToDot :: (Node, [(Node, o)]) -> String
edgesToDot (node, adjNodes) = concatMap (\adjNode -> "    " ++ show node ++ " -> " ++ adjNode ++ ";\n") (fmap (show . fst) adjNodes)

-- | https://en.wikipedia.org/wiki/Directed_acyclic_graph#/media/File:Tred-G.svg
exampleDAG :: DAG Node String
exampleDAG =
  Map.fromList
    [ (NA, fmap (,"foo") [NB, NC, ND, NE]),
      (NB, fmap (,"foo") [ND]),
      (NC, fmap (,"foo") [ND, NE]),
      (ND, fmap (,"foo") [NE]),
      (NE, [])
    ]

--------------------------------------------------------------------------------
-- DAG Machine
--
-- We want to define some machine which when given an initial state is
-- able to traverse the DAG and then present an observation of the
-- result.
--
-- We started with a Mealy Machine, but I'm now thinking that we
-- should be using Moore. Hence this code is a big fragmented and
-- doesn't all add up properly.
--
-- I think we want Moore instead of Mealy because we want to simply
-- iterate the state repeatedly and then take a single final
-- observation after completing our DAG traversal. The two machines
-- can be defined in terms of one another, but I think Moore fits our
-- problem more ergonomically.

-- | A 'Mealy' Machine which can walk 'exampleDAG'.
equivalentMealy :: Mealy Node Edge (Maybe String)
equivalentMealy = Mealy $ curry \case
  (NA, E1) -> (Just "A -> B", NB)
  (NA, E2) -> (Just "A -> C", NC)
  (NA, E3) -> (Just "A -> D", ND)
  (NA, E4) -> (Just "A -> E", NE)
  (NB, E3) -> (Just "B -> D", ND)
  (NC, E3) -> (Just "C -> D", ND)
  (NC, E4) -> (Just "C -> E", NE)
  (ND, E4) -> (Just "D -> E", NE)
  (s, _) -> (Nothing, s)

-- | A 'Moore' Machine which can walk 'exampleDAG'.
equivalentMoore :: Moore Node Edge (Maybe String)
equivalentMoore = Moore $ \case
  NA -> (Nothing, \case E1 -> NB; E2 -> NC; E3 -> ND; E4 -> NE)
  NB -> (Nothing, \case E3 -> ND; _ -> NB)
  NC -> (Nothing, \case E3 -> ND; E4 -> NE; _ -> NC)
  ND -> (Nothing, \case E4 -> NE; _ -> ND)
  NE -> (Just "Finished!", const NE)

-- TODO:
dag2mealy :: DAG n o -> Mealy n Word (Maybe o)
dag2mealy = undefined

dag2moore :: DAG n o -> Moore n Word (Maybe o)
dag2moore = undefined

interpretMealy :: Mealy n Word (Maybe o) -> IO ()
interpretMealy = undefined

interpretMoore :: Moore n Word (Maybe o) -> IO ()
interpretMoore = undefined
