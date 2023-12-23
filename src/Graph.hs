{-# language
    DeriveFunctor
  , DerivingStrategies
  , DuplicateRecordFields
  , InstanceSigs
  , MonoLocalBinds
  , OverloadedRecordDot
  , OverloadedStrings
  , PackageImports
  , ScopedTypeVariables
  , TupleSections
  , TypeApplications
#-}

module Graph
  ( Graph

  , empty
  , addVertex
  , addEdge
  , removeVertex

  , numVertices
  , numEdges
  , hasVertex
  , hasEdge

  , neighbors
  , vertices
  , edges
  , toList

  , selfLoops
  , removeSelfLoops
  , sccGraph
  , sccListGraph

  , mapVertices

  , toDot
  )
  where

import Data.Foldable qualified as F
import Data.Graph qualified as Containers
import Data.Functor.Const (Const(..))
import Data.List qualified as List
import Data.Maybe (catMaybes)
import Data.Foldable (foldMap')
import Data.Map (Map)
import Data.Monoid (Sum(..), Endo (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Dot.Types qualified as Dot
import GHC.Stack (HasCallStack)

newtype Graph weight edgeLabel node = Graph
  { getGraph :: Map node (Map node (weight, edgeLabel))
  }

empty :: Graph weight edgeLabel node
empty = Graph Map.empty

addVertex :: (Ord node)
  => node
  -> Graph weight edgeLabel node
  -> Graph weight edgeLabel node
addVertex n (Graph g) =
  Graph (Map.insertWith Map.union n Map.empty g)

addEdge :: (Ord node)
  => node
  -> node
  -> weight
  -> edgeLabel
  -> Graph weight edgeLabel node
  -> Graph weight edgeLabel node
addEdge n1 n2 w el (Graph g) =
  Graph (Map.insertWith Map.union n1 (Map.singleton n2 (w, el)) g)

numVertices :: ()
  => Graph weight edgeLabel node
  -> Word
numVertices = i2w . Map.size . getGraph

numEdges :: ()
  => Graph weight edgeLabel node
  -> Word
numEdges = i2w . getSum . foldMap' (Sum . Map.size) . getGraph

hasVertex :: (Ord node)
  => Graph weight edgeLabel node
  -> node
  -> Bool
hasVertex (Graph g) n = Map.member n g

hasEdge :: (Ord node)
  => Graph weight edgeLabel node
  -> node
  -> node
  -> Bool
hasEdge (Graph g) n1 n2 =
  case Map.lookup n1 g of
    Nothing -> False
    Just es -> Map.member n2 es

neighbors :: (Ord node)
  => Graph weight edgeLabel node
  -> node
  -> [(node, weight, edgeLabel)]
neighbors (Graph g) n =
  List.map (\(e, (weight, label)) -> (e, weight, label))
  $ Map.toList
  $ Map.findWithDefault Map.empty n g

vertices :: ()
  => Graph weight edgeLabel node
  -> [node]
vertices = Map.keys . getGraph

edges :: ()
  => Graph weight edgeLabel node
  -> [(node, node, weight, edgeLabel)]
edges =
  List.map (\(src, (tgt, (w, el))) -> (src, tgt, w, el))
  . List.concatMap (\(src, tgts) -> (src,) <$> tgts)
  . Map.toList
  . Map.map Map.toList
  . getGraph

toList :: ()
  => Graph weight edgeLabel node
  -> [(node, [(node, weight, edgeLabel)])]
toList =
  id
  . List.map (\(n, es) -> (n, List.map (\(e, (w, el)) -> (e, w, el)) es))
  . Map.toList
  . Map.map Map.toList
  . getGraph 

mapVertices
  :: (Ord node, Ord node')
  => (node -> node') -- ^ Must be a bijection!
  -> Graph weight edgeLabel node
  -> Graph weight edgeLabel node'
mapVertices f = Graph . Map.map (Map.mapKeys f) . Map.mapKeys f . getGraph

removeVertex :: (Ord node)
  => node
  -> Graph weight edgeLabel node
  -> Graph weight edgeLabel node
removeVertex n = Graph . fmap (Map.delete n) . Map.delete n . getGraph

sccGraph :: forall weight edgeLabel node. (Ord node)
  => Graph weight edgeLabel node
  -> Map node Word
sccGraph graph = Map.fromList (concatMap (\(i, ns) -> List.map (, i) ns) (List.zip [0..] scc))
  where
    scc :: [[node]]
    scc = sccListGraph graph
  
sccListGraph :: forall weight edgeLabel node. (Ord node, HasCallStack)
  => Graph weight edgeLabel node
  -> [[node]]
sccListGraph (Graph graph) =
  map (map (intToVertices Map.!) . F.toList)
  $ Containers.scc
  $ Containers.buildG bounds
  $ concatMap (\(i, js) -> List.map (\(j, _) -> (i, j)) (Map.toList js))
  $ Map.toList
  $ getGraph
  $ mapVertices (verticesToInt Map.!) (Graph graph)
  where
    bounds :: Containers.Bounds
    bounds = (0, Map.size graph - 1)

    verticesToInt :: Map node Int
    verticesToInt =
      Map.fromList $ zipWith (\(n, _) i -> (n, i)) (Map.toAscList graph) [0..]

    intToVertices :: Map Int node
    intToVertices =
      Map.fromList $ zipWith (\i (n, _) -> (i, n)) [0..] (Map.toAscList graph)

selfLoops :: (Ord node)
  => Graph weight edgeLabel node
  -> [node]
selfLoops =
  ($ [])
  . appEndo
  . getConst
  . Map.traverseWithKey (\n es -> if n `Map.member` es then Const (Endo (n :)) else mempty)
  . getGraph

removeSelfLoops :: forall weight edgeLabel node. (Ord node, Ord weight)
  => Graph weight edgeLabel node
  -> Graph weight edgeLabel node
removeSelfLoops (Graph g) = Graph $ Map.mapWithKey (\n es -> Map.delete n es) g

toDot :: forall weight edgeLabel node. (Ord node)
  => (weight -> Text)
  -> (edgeLabel -> Text)
  -> (node -> Text)
  -> Graph weight edgeLabel node
  -> Dot.DotGraph
toDot w2t el2t n2t g =
  Dot.DotGraph Dot.Strict Dot.Directed (Just "weighted") (List.map mkEdge (edges g))
  where
    mkEdge :: (node, node, weight, edgeLabel) -> Dot.Statement
    mkEdge (n1, n2, mWeight, mEdgeLabel) =
      let
        src = txtToNodeId $ n2t n1
        dst = txtToNodeId $ n2t n2
        edgeLabelAttr = Dot.Attribute "label" . Dot.Id . el2t $ mEdgeLabel
        weightAttr = Dot.Attribute "weight" . Dot.Id . w2t $ mWeight
      in
      Dot.StatementEdge
          $ Dot.EdgeStatement
              (Dot.ListTwo (Dot.EdgeNode src) (Dot.EdgeNode dst) [])
              [edgeLabelAttr, weightAttr]

    txtToNodeId :: Text -> Dot.NodeId
    txtToNodeId t = Dot.NodeId (Dot.Id t) Nothing

-----
----- Helpers
-----

i2w :: Int -> Word
i2w = fromIntegral
