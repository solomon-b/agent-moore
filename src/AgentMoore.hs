{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Machines (MealyM (..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE

-- | Convert a Graph to a Mealy machine.
--
--   This takes an observation function which operates on the current node and some input.
--   The edge labels between nodes are observations.
--   (Weighted) Observations determine transitions.
graphToMealy :: forall m i o weight node. (Monad m, Ord node, Ord weight, Eq o)
  => (node -> i -> m o)
  -> Graph weight o node -- edge labels are observations
  -> MealyM m node i (Maybe o)
graphToMealy observe (G.removeSelfLoops -> g) =
  let
    -- all edges
    vs :: Map node [(node, weight, o)]
    vs = Map.fromList $ G.toList g
  in
  MealyM $ \currentNode input -> do
    observation <- observe currentNode input
    let
      -- all the edges out from the current node
      edges :: Maybe [(node, weight, o)]
      edges = Map.lookup currentNode vs

    let
      -- Filter for edges that match the observation, then sort by weight descending
      choiceEdges :: Maybe (NonEmpty (node, weight, o))
      choiceEdges = join $ fmap NE.nonEmpty $ fmap (List.sortOn (\(_, w, _) -> Down w) . List.filter (\(_, _, x) -> x == observation)) $ edges
    
    case choiceEdges of
      Nothing -> do
        pure (Nothing, currentNode)
      Just ((target, _, _) :| _) -> do
        pure (Just observation, target)
