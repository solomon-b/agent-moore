{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module AgentMoore
  ( graphToMealy
  )
  where

--------------------------------------------------------------------------------

import Control.Monad (join)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Graph (Graph)
import Graph qualified as G
import Data.Ord (Down(..))
import Machines (Mealy(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE

-- | Convert a Graph to a Mealy machine.
--
--   This takes an observation function which operates on the current node and some input.
--   The edge labels between nodes are observations.
--   (Weighted) Observations determine transitions.
graphToMealy :: forall i o weight node. (Ord node, Ord weight, Eq o)
  => (node -> i -> o)
  -> Graph weight o node -- edge labels are observations
  -> Mealy node i (Maybe o)
graphToMealy observe (G.removeSelfLoops -> g) =
  let
    -- all edges
    vs :: Map node [(node, weight, o)]
    vs = Map.fromList
      $ List.map (\(n, es) -> (n, List.map (\(eNode, el, w) -> (eNode, fromJust "weight" w, fromJust "edge label" el)) es))
      $ G.toList g
  in
  Mealy $ \currentNode input -> 
    let
      -- make the observation
      observation :: o
      observation = observe currentNode input

      -- all the edges out from the current node
      edges :: Maybe [(node, weight, o)]
      edges = Map.lookup currentNode vs

      -- Filter for edges that match the observation, then sort by weight descending
      choiceEdges :: Maybe (NonEmpty (node, weight, o))
      choiceEdges = join $ fmap NE.nonEmpty $ fmap (List.sortOn (\(_, w, _) -> Down w) . List.filter (\(_, _, x) -> x == observation)) $ edges
    in
    case choiceEdges of
      -- terminal node. maybe this should be an error case?
      Nothing -> (Nothing, currentNode)
      -- we make an observation here.
      Just ((target, _, _) :| _) -> (Just observation, target)

fromJust :: String -> Maybe a -> a
fromJust err = \case 
  Nothing -> error err
  Just a -> a