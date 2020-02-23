{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Utility.Graph.MapBasedGraph
  ( MapBasedGraph
  ) where

import qualified Data.Set as Set

import Utility.DirectedGraph
import qualified Utility.DirectedGraph.MapBasedDirectedGraph as MDG
import Utility.Graph

newtype MapBasedGraph v d
  = MapBasedGraph
  { _mbgMap :: MDG.MapBasedDirectedGraph v d
  } deriving (DirectedGraph)

instance Ord v => Graph (MapBasedGraph v d) where
  connect x y = connectTo x y . connectTo y x
  disconnect x y = disconnectTo x y . disconnectTo y x
  isConnected x y g = doesConnectTo x y g || doesConnectTo y x g
  neighborOf x = fmap Set.toList . neighborSetOf x 
  neighborSetOf x g = Set.union <$> predSetOf x g <*> succOf x g
