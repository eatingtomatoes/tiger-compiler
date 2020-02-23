module MiddleEnd.Quadruple.ControlFlowGraph
  ( buildControlFlowGraphForQuadruple
  , module Utility.DirectedGraph.MapBasedDirectedGraphForScalar.ControlFlowGraph
  ) where

import Data.Maybe (catMaybes, mapMaybe)
import MiddleEnd.Quadruple
import Translation.LabelMap

import Utility.DirectedGraph.MapBasedDirectedGraphForScalar.ControlFlowGraph

buildControlFlowGraphForQuadruple :: [Quadruple] -> ControlFlowGraph
buildControlFlowGraphForQuadruple quads
  = flip buildControlFlowGraph quads
  $ \q i -> filter (< qnum) $ maybe [i+1] (mapMaybe (flip posOf labelMap)) $ branchDstOf q
  where
    labelMap = buildLabelMap labelOf quads
    qnum = length quads
