module Utility.Graph
  ( Graph  
  , connect
  , disconnect
  , isConnected
  , neighborOf
  , neighborSetOf
  ) where

import qualified Data.Set as Set

import Utility.DirectedGraph

class DirectedGraph g => Graph g where
  connect :: VertexId g -> VertexId g -> g -> g
  disconnect :: VertexId g -> VertexId g -> g -> g
  isConnected :: VertexId g -> VertexId g -> g -> Bool
  neighborOf :: VertexId g -> g -> Maybe [VertexId g]
  neighborSetOf :: VertexId g -> g -> Maybe (Set.Set (VertexId g))
 
