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

  , EdgeBuilder
  , vertex
  , edge
  , setNodeLabel
  , setEdgeLabel
  , setEdgeWeight

  , empty
  , addEdge
  , addEdges
  , removeVertex

  , numVertices
  , numEdges
  , hasVertex
  , hasEdge

  , neighbors
  , vertices
  , edges

  , selfLoops
  , removeSelfLoops
  , sccGraph
  , sccListGraph

  , mapVertices

  , toDot

  , test
  )
  where

import Data.Foldable qualified as F
import Data.Graph qualified as Containers
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.List qualified as List
import Data.Maybe (catMaybes, mapMaybe)
import Data.Foldable (foldMap')
import Data.Map (Map)
import Data.Monoid (Sum(..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Dot.Text qualified as Dot
import Dot.Types qualified as Dot
import GHC.Stack (HasCallStack)

data Labeled label a = Labeled
  { label :: Maybe label
  , value :: a
  }
  deriving stock (Functor)

instance (Eq a) => Eq (Labeled label a) where
  (==) :: Eq a => Labeled label a -> Labeled label a -> Bool
  l1 == l2 = l1.value == l2.value

instance (Ord a) => Ord (Labeled label a) where
  (<=) :: Ord a => Labeled label a -> Labeled label a -> Bool
  l1 <= l2 = l1.value <= l2.value

newtype Graph weight nodeLabel edgeLabel node = Graph
  { getGraph :: Map (Labeled nodeLabel node) (Map (Labeled edgeLabel node) (Maybe weight))
  }

data EdgeBuilder weight nodeLabel edgeLabel node = EdgeBuilder
  { node :: node
  , nodeLabel :: Maybe nodeLabel
  , edgeNode :: Maybe node
  , edgeLabel :: Maybe edgeLabel
  , edgeWeight :: Maybe weight
  }

vertex :: ()
  => node
  -> EdgeBuilder weight nodeLabel edgeLabel node
vertex n = EdgeBuilder
  { node = n
  , nodeLabel = Nothing
  , edgeNode = Nothing
  , edgeLabel = Nothing
  , edgeWeight = Nothing
  }

edge :: ()
  => node
  -> node
  -> EdgeBuilder weight nodeLabel edgeLabel node
edge n1 n2 = (vertex n1) { edgeNode = Just n2 }

setNodeLabel :: ()
  => nodeLabel
  -> EdgeBuilder weight nodeLabel edgeLabel node
  -> EdgeBuilder weight nodeLabel edgeLabel node
setNodeLabel nl e = e { nodeLabel = Just nl }

setEdgeLabel :: ()
  => edgeLabel
  -> EdgeBuilder weight nodeLabel edgeLabel node
  -> EdgeBuilder weight nodeLabel edgeLabel node
setEdgeLabel el e = e { edgeLabel = Just el }

setEdgeWeight :: ()
  => weight
  -> EdgeBuilder weight nodeLabel edgeLabel node
  -> EdgeBuilder weight nodeLabel edgeLabel node
setEdgeWeight w e = e { edgeWeight = Just w }

empty :: Graph weight nodeLabel edgeLabel node
empty = Graph Map.empty

addEdge :: (Ord node)
  => EdgeBuilder weight nodeLabel edgeLabel node
  -> Graph       weight nodeLabel edgeLabel node
  -> Graph       weight nodeLabel edgeLabel node
addEdge b (Graph g) =
  Graph (Map.insertWith Map.union (Labeled b.nodeLabel b.node) es g)
  where
    es = case b.edgeNode of
      Nothing -> Map.empty
      Just e -> Map.singleton (Labeled b.edgeLabel e) b.edgeWeight

addEdges :: (Ord node, Ord weight)
  => [EdgeBuilder weight nodeLabel edgeLabel node]
  -> Graph        weight nodeLabel edgeLabel node
  -> Graph        weight nodeLabel edgeLabel node
addEdges es g = foldr addEdge g es

numVertices :: ()
  => Graph weight nodeLabel edgeLabel node
  -> Word
numVertices = i2w . Map.size . getGraph

numEdges :: ()
  => Graph weight nodeLabel edgeLabel node
  -> Word
numEdges = i2w . getSum . foldMap' (Sum . Map.size) . getGraph

hasVertex :: (Ord node)
  => Graph weight nodeLabel edgeLabel node
  -> node
  -> Bool
hasVertex (Graph g) n = Map.member (peeking n) g

hasEdge :: (Ord node)
  => Graph weight nodeLabel edgeLabel node
  -> node
  -> node
  -> Bool
hasEdge (Graph g) n1 n2 =
  case Map.lookup (peeking n1) g of
    Nothing -> False
    Just es -> Map.member (peeking n2) es

neighbors :: (Ord node)
  => Graph weight nodeLabel edgeLabel node
  -> node
  -> [(node, Maybe weight, Maybe edgeLabel)]
neighbors (Graph g) n =
  List.map (\(e, weight) -> (e.value, weight, e.label))
  $ Map.toList
  $ Map.findWithDefault Map.empty (peeking n) g

vertices :: ()
  => Graph weight nodeLabel edgeLabel node
  -> [(node, Maybe nodeLabel)]
vertices = List.map (\l -> (l.value, l.label)) . Map.keys . getGraph

edges :: ()
  => Graph weight nodeLabel edgeLabel node
  -> [(node, node, Maybe nodeLabel, Maybe edgeLabel, Maybe weight)]
edges =
  List.map (\(src, (tgt, w)) -> (src.value, tgt.value, src.label, tgt.label, w))
  . List.concatMap (\(src, tgts) -> (src,) <$> tgts)
  . Map.toList
  . Map.map Map.toList
  . getGraph

mapVertices
  :: (Ord node, Ord node')
  => (node -> node') -- ^ Must be a bijection!
  -> Graph weight nodeLabel edgeLabel node
  -> Graph weight nodeLabel edgeLabel node'
mapVertices f = Graph . Map.map (Map.mapKeys (fmap f)) . Map.mapKeys (fmap f) . getGraph

removeVertex :: (Ord node)
  => node
  -> Graph weight nodeLabel edgeLabel node
  -> Graph weight nodeLabel edgeLabel node
removeVertex n = Graph . fmap (Map.delete (peeking n)) . Map.delete (peeking n) . getGraph

-- Map (Labeled nodeLabel node) (Map (Labeled edgeLabel node) (Maybe weight))

sccGraph :: forall weight nodeLabel edgeLabel node. (Ord node)
  => Graph weight nodeLabel edgeLabel node
  -> Map node Word
sccGraph graph = Map.fromList (concatMap (\(i, ns) -> List.map (, i) ns) (List.zip [0..] scc))
  where
    scc :: [[node]]
    scc = sccListGraph graph
  
sccListGraph :: forall weight nodeLabel edgeLabel node. (Ord node, HasCallStack)
  => Graph weight nodeLabel edgeLabel node
  -> [[node]]
sccListGraph (Graph graph) =
  map (map (intToVertices Map.!) . F.toList)
  $ Containers.scc
  $ Containers.buildG bounds
  $ concatMap (\(i, js) -> List.map (\(j, _) -> (i.value, j.value)) (Map.toList js))
  $ Map.toList
  $ getGraph
  $ mapVertices (verticesToInt Map.!) (Graph graph)
  where
    bounds :: Containers.Bounds
    bounds = (0, Map.size graph - 1)

    verticesToInt :: Map node Int
    verticesToInt =
      Map.fromList $ zipWith (\(n, _) i -> (n.value, i)) (Map.toAscList graph) [0..]

    intToVertices :: Map Int node
    intToVertices =
      Map.fromList $ zipWith (\i (n, _) -> (i, n.value)) [0..] (Map.toAscList graph)

selfLoops :: (Ord node)
  => Graph weight nodeLabel edgeLabel node
  -> [node]
selfLoops =
  mapMaybe (\(n1, n2, _, _, _) -> if n1 == n2 then Just n1 else Nothing)
  . edges

removeSelfLoops :: (Ord node, Ord weight)
  => Graph weight nodeLabel edgeLabel node
  -> Graph weight nodeLabel edgeLabel node
removeSelfLoops g =
  addEdges
    (List.map
      (\(n1, n2, nl, el, w) -> (edge n1 n2)
          { nodeLabel = nl
          , edgeLabel = el
          , edgeWeight = w
          }
      )
      (List.filter (\(s,t,_,_,_) -> s /= t) (edges g))
    )
    (addEdges (List.map (\(n, nl) -> (vertex n) { nodeLabel = nl }) (vertices g)) empty)

testGraph :: Graph () Text Text Int
testGraph = empty
  & addEdge (edge 0 1 & setNodeLabel "A" & setEdgeLabel "01")
  & addEdge (edge 1 1 & setNodeLabel "B" & setEdgeLabel "11")
  & addEdge (edge 1 2 & setNodeLabel "C" & setEdgeLabel "12")
  & addEdge (edge 2 3 & setNodeLabel "D" & setEdgeLabel "23")
  & addEdge (edge 3 5 & setNodeLabel "E" & setEdgeLabel "35")
  & addEdge (edge 5 2 & setNodeLabel "F" & setEdgeLabel "52")
  & addEdge (edge 5 5 & setNodeLabel "G" & setEdgeLabel "55")

test :: IO ()
test = do
  let tshow :: (Show a) => a -> Text
      tshow = Text.pack . show

  let p :: (Ord node, Show weight, Show node)
        => Graph weight Text Text node
        -> IO ()
      p g = Text.putStrLn $ Dot.encode $ toDot tshow id id tshow g

  p testGraph
  p $ removeSelfLoops testGraph

toDot :: forall weight nodeLabel edgeLabel node. (Ord node)
  => (weight -> Text)
  -> (nodeLabel -> Text)
  -> (edgeLabel -> Text)
  -> (node -> Text)
  -> Graph weight nodeLabel edgeLabel node
  -> Dot.DotGraph
toDot w2t nl2t el2t n2t g =
  Dot.DotGraph Dot.Strict Dot.Directed (Just "weighted")
    (nodeLabels ++ es)
  where
    -- This is a little convoluted, but to make all the node labels come first,
    -- we accumulate them separately, then prepend them to the actual structure
    -- of the graph.
    (nodeLabels, es) = first List.concat $ List.unzip $ List.map mkEdge (edges g)

    mkEdge :: (node, node, Maybe nodeLabel, Maybe edgeLabel, Maybe weight) -> ([Dot.Statement], Dot.Statement)
    mkEdge (n1, n2, mNodeLabel, mEdgeLabel, mWeight) =
      let
        src = txtToNodeId $ n2t n1
        dst = txtToNodeId $ n2t n2
        srcLabelAttr = Dot.Attribute "label" . Dot.Id . nl2t <$> mNodeLabel
        edgeLabelAttr = Dot.Attribute "label" . Dot.Id . el2t <$> mEdgeLabel
        weightAttr = Dot.Attribute "weight" . Dot.Id . w2t <$> mWeight
      in
      (
        catMaybes
          [ Dot.StatementNode . Dot.NodeStatement src . List.singleton <$> srcLabelAttr
          ]
      ,
        Dot.StatementEdge
          $ Dot.EdgeStatement
              (Dot.ListTwo (Dot.EdgeNode src) (Dot.EdgeNode dst) [])
              (catMaybes [edgeLabelAttr, weightAttr])
      )

    txtToNodeId :: Text -> Dot.NodeId
    txtToNodeId t = Dot.NodeId (Dot.Id t) Nothing

-----
----- Helpers
-----

i2w :: Int -> Word
i2w = fromIntegral

-- Pass this to a lookup of a labeled value in a Map/Set.
-- the Eq and Ord instances for Labeled don't compare the label.
peeking :: node -> Labeled label node
peeking = Labeled Nothing

