{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module AgentMoore where

--------------------------------------------------------------------------------

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Machines

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
edgesToDot (node, adjNodes) = concatMap ((\adjNode -> "    " ++ show node ++ " -> " ++ adjNode ++ ";\n") . show . fst) adjNodes

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
  NA -> (Just "NA", \case E1 -> NB; E2 -> NC; E3 -> ND; E4 -> NE)
  NB -> (Just "NB", \case E3 -> ND; _ -> NB)
  NC -> (Just "NC", \case E3 -> ND; E4 -> NE; _ -> NC)
  ND -> (Just "ND", \case E4 -> NE; _ -> ND)
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
