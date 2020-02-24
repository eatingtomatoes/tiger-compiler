{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Utility.Graph.MapBasedGraphForScalar
  ( MapBasedGraphForScalar
  , 
  ) where

import qualified Data.Set as Set

import Utility.Graph
import Utility.DirectedGraph
import qualified Utility.DirectedGraph.MapBasedDirectedGraphForScalar as MDG

newtype MapBasedGraphForScalar v
  = MapBasedGraphForScalar
  { _mbgMap :: MDG.MapBasedDirectedGraphForScalar v 
  } deriving (DirectedGraph)

instance Ord v => Graph (MapBasedGraphForScalar v) where
  connect x y = connectTo x y . connectTo y x
  disconnect x y = disconnectTo x y . disconnectTo y x
  isConnected x y g = doesConnectTo x y g || doesConnectTo y x g
  neighborOf x = fmap Set.toList . neighborSetOf x 
  neighborSetOf x g = Set.union <$> predOf x g <*> succOf x g
  
