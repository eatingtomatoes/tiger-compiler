{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}

module Utility.DirectedGraph
  ( DirectedGraph(..)
  ) where

import Data.Set (Set)
import Data.Map.Strict (Map)

import Utility.Conversion

class DirectedGraph g where
  type VertexId g
  type VertexData g

  empty :: g

  connectTo, disconnectTo
    :: VertexId g -> VertexId g -> g -> g

  doesConnectTo :: VertexId g -> VertexId g -> g -> Bool

  existVertex :: VertexId g -> g -> Bool 

  addVertex :: VertexId g -> VertexData g -> g -> g

  removeVertex :: VertexId g -> g -> g

  dataOf :: VertexId g -> g -> Maybe (VertexData g)

  succOf, predOf
    :: Conversion (Set (VertexId g)) (f (VertexId g))
    => VertexId g -> g -> Maybe (f (VertexId g))

  vertexOf
    :: Conversion (Set (VertexId g)) (f (VertexId g))
    => g -> f (VertexId g)

  flattenData
    -- :: Conversion (Map (VertexId g) (VertexData g)) (f (VertexId g) (VertexData g))
    -- =>  g -> f (VertexId g) (VertexData g)
    :: Conversion (Map (VertexId g) (VertexData g)) f 
    =>  g -> f

  flattenSucc, flattenPred
    -- :: Conversion (Map (VertexId g) (Set (VertexId g))) (f (VertexId g) (Set (VertexId g)))
    -- =>  g -> f (VertexId g) (Set (VertexId g))
    :: Conversion (Map (VertexId g) (Set (VertexId g))) f
    =>  g -> f 


