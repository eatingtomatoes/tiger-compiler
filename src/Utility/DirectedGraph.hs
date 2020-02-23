{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}

module Utility.DirectedGraph
  ( DirectedGraph(..)
  ) where

import qualified Data.Set as Set

import Utility.Conversion

class DirectedGraph g where
  type VertexId g
  type VertexData g
  empty :: g
  connectTo :: VertexId g -> VertexId g -> g -> g
  disconnectTo :: VertexId g -> VertexId g -> g -> g
  doesConnectTo :: VertexId g -> VertexId g -> g -> Bool
  existVertex :: VertexId g -> g -> Bool 
  addVertex :: VertexId g -> VertexData g -> g -> g
  removeVertex :: VertexId g -> g -> g
  dataOf :: VertexId g -> g -> Maybe (VertexData g)
  succOf :: Conversion (Set.Set (VertexId g)) (f (VertexId g)) => VertexId g -> g -> Maybe (f (VertexId g))
  -- succOf :: VertexId g -> g -> Maybe ([(VertexId g)])  
  -- succSetOf :: VertexId g -> g -> Maybe (Set.Set (VertexId g))
  predOf :: VertexId g -> g -> Maybe [VertexId g]
  predSetOf :: VertexId g -> g -> Maybe (Set.Set (VertexId g))
  vertexOf :: g -> [VertexId g]
  vertexSetOf :: g -> Set.Set (VertexId g)
  flattenData :: g -> [(VertexId g, VertexData g)]
  flattenSucc :: g -> [(VertexId g, [VertexId g])]    
  flattenSuccSet :: g -> [(VertexId g, Set.Set (VertexId g))]
  flattenPred :: g -> [(VertexId g, [VertexId g])]
  flattenPredSet :: g -> [(VertexId g, Set.Set (VertexId g))]


