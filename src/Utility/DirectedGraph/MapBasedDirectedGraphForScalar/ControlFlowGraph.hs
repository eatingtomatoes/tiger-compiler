{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Utility.DirectedGraph.MapBasedDirectedGraphForScalar.ControlFlowGraph
  ( ControlFlowGraph
  , buildControlFlowGraph
  , module Utility.DirectedGraph.MapBasedDirectedGraphForScalar
  ) where

import Utility.Visualize
import Utility.DirectedGraph.MapBasedDirectedGraphForScalar

newtype ControlFlowGraph = ControlFlowGraph (MapBasedDirectedGraphForScalar Int)
  deriving (DirectedGraph, DrawableDirectedGraph)

buildControlFlowGraph :: (a -> Int -> [Int]) -> [a] -> ControlFlowGraph
buildControlFlowGraph nextsOf xs = ControlFlowGraph $ foldr g empty $ zip [0..] $ zipWith nextsOf xs [0..]
  where
    g (i, js) graph = foldr (connectTo i) graph js

