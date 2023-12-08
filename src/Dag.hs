module Dag
  ( Dag
  )
  where

import Graph (Graph)
import Graph qualified

newtype Dag weight nodeLabel edgeLabel node = Dag
  { getDag :: Graph weight nodeLabel edgeLabel node
  }

fromGraph :: ()
  => Graph weight nodeLabel edgeLabel node
  -> Dag weight nodeLabel edgeLabel node
fromGraph g = Dag (Graph.removeCycles g)
