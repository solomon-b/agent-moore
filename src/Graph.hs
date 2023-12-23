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
  , toList

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

newtype Graph weight edgeLabel node = Graph
  { getGraph :: Map node (Map (Labeled edgeLabel node) (Maybe weight))
  }

data EdgeBuilder weight edgeLabel node = EdgeBuilder
  { node :: node
  , edgeNode :: Maybe node
  , edgeLabel :: Maybe edgeLabel
  , edgeWeight :: Maybe weight
  }

vertex :: ()
  => node
  -> EdgeBuilder weight edgeLabel node
vertex n = EdgeBuilder
  { node = n
  , edgeNode = Nothing
  , edgeLabel = Nothing
  , edgeWeight = Nothing
  }

edge :: ()
  => node
  -> node
  -> EdgeBuilder weight edgeLabel node
edge n1 n2 = (vertex n1) { edgeNode = Just n2 }

setEdgeLabel :: ()
  => edgeLabel
  -> EdgeBuilder weight edgeLabel node
  -> EdgeBuilder weight edgeLabel node
setEdgeLabel el e = e { edgeLabel = Just el }

setEdgeWeight :: ()
  => weight
  -> EdgeBuilder weight edgeLabel node
  -> EdgeBuilder weight edgeLabel node
setEdgeWeight w e = e { edgeWeight = Just w }

empty :: Graph weight edgeLabel node
empty = Graph Map.empty

addEdge :: (Ord node)
  => EdgeBuilder weight edgeLabel node
  -> Graph       weight edgeLabel node
  -> Graph       weight edgeLabel node
addEdge b (Graph g) =
  Graph (Map.insertWith Map.union b.node es g)
  where
    es = case b.edgeNode of
      Nothing -> Map.empty
      Just e -> Map.singleton (Labeled b.edgeLabel e) b.edgeWeight

addEdges :: (Ord node, Ord weight)
  => [EdgeBuilder weight edgeLabel node]
  -> Graph        weight edgeLabel node
  -> Graph        weight edgeLabel node
addEdges es g = foldr addEdge g es

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
    Just es -> Map.member (peeking n2) es

neighbors :: (Ord node)
  => Graph weight edgeLabel node
  -> node
  -> [(node, Maybe weight, Maybe edgeLabel)]
neighbors (Graph g) n =
  List.map (\(e, weight) -> (e.value, weight, e.label))
  $ Map.toList
  $ Map.findWithDefault Map.empty n g

vertices :: ()
  => Graph weight edgeLabel node
  -> [node]
vertices = Map.keys . getGraph

edges :: ()
  => Graph weight edgeLabel node
  -> [(node, node, Maybe edgeLabel, Maybe weight)]
edges =
  List.map (\(src, (tgt, w)) -> (src, tgt.value, tgt.label, w))
  . List.concatMap (\(src, tgts) -> (src,) <$> tgts)
  . Map.toList
  . Map.map Map.toList
  . getGraph

toList :: ()
  => Graph weight edgeLabel node
  -> [(node, [(node, Maybe edgeLabel, Maybe weight)])]
toList =
  id
  . List.map (\(n, es) -> (n, List.map (\(e, w) -> (e.value, e.label, w)) es))
  . Map.toList
  . Map.map Map.toList
  . getGraph 

mapVertices
  :: (Ord node, Ord node')
  => (node -> node') -- ^ Must be a bijection!
  -> Graph weight edgeLabel node
  -> Graph weight edgeLabel node'
mapVertices f = Graph . Map.map (Map.mapKeys (fmap f)) . Map.mapKeys f . getGraph

removeVertex :: (Ord node)
  => node
  -> Graph weight edgeLabel node
  -> Graph weight edgeLabel node
removeVertex n = Graph . fmap (Map.delete (peeking n)) . Map.delete n . getGraph

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
  $ concatMap (\(i, js) -> List.map (\(j, _) -> (i, j.value)) (Map.toList js))
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
  mapMaybe (\(n1, n2, _, _) -> if n1 == n2 then Just n1 else Nothing)
  . edges

removeSelfLoops :: (Ord node, Ord weight)
  => Graph weight edgeLabel node
  -> Graph weight edgeLabel node
removeSelfLoops g =
  addEdges
    (List.map
      (\(n1, n2, el, w) -> (edge n1 n2)
          { edgeLabel = el
          , edgeWeight = w
          }
      )
      (List.filter (\(s,t,_,_) -> s /= t) (edges g))
    )
    (addEdges (List.map (\n -> vertex n) (vertices g)) empty)

testGraph :: Graph () Text Int
testGraph = empty
  & addEdge (edge 0 1 & setEdgeLabel "01")
  & addEdge (edge 1 1 & setEdgeLabel "11")
  & addEdge (edge 1 2 & setEdgeLabel "12")
  & addEdge (edge 2 3 & setEdgeLabel "23")
  & addEdge (edge 3 5 & setEdgeLabel "35")
  & addEdge (edge 5 2 & setEdgeLabel "52")
  & addEdge (edge 5 5 & setEdgeLabel "55")

test :: IO ()
test = do
  let tshow :: (Show a) => a -> Text
      tshow = Text.pack . show

  let p :: (Ord node, Show weight, Show node)
        => Graph weight Text node
        -> IO ()
      p g = Text.putStrLn $ Dot.encode $ toDot tshow id tshow g

  p testGraph
  p $ removeSelfLoops testGraph

toDot :: forall weight edgeLabel node. (Ord node)
  => (weight -> Text)
  -> (edgeLabel -> Text)
  -> (node -> Text)
  -> Graph weight edgeLabel node
  -> Dot.DotGraph
toDot w2t el2t n2t g =
  Dot.DotGraph Dot.Strict Dot.Directed (Just "weighted") (List.map mkEdge (edges g))
  where
    mkEdge :: (node, node, Maybe edgeLabel, Maybe weight) -> Dot.Statement
    mkEdge (n1, n2, mEdgeLabel, mWeight) =
      let
        src = txtToNodeId $ n2t n1
        dst = txtToNodeId $ n2t n2
        edgeLabelAttr = Dot.Attribute "label" . Dot.Id . el2t <$> mEdgeLabel
        weightAttr = Dot.Attribute "weight" . Dot.Id . w2t <$> mWeight
      in
      Dot.StatementEdge
          $ Dot.EdgeStatement
              (Dot.ListTwo (Dot.EdgeNode src) (Dot.EdgeNode dst) [])
              (catMaybes [edgeLabelAttr, weightAttr])

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

