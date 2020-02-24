module MiddleEnd.SSA.Quadruple.BlockControlFlowGraph
  ( buildBlockControlFlowGraphForQuadruple
  , module Utility.DirectedGraph.MapBasedDirectedGraph.BlockControlFlowGraph
  ) where

import MiddleEnd.SSA.Quadruple

import Utility.DirectedGraph.MapBasedDirectedGraph.BlockControlFlowGraph

buildBlockControlFlowGraphForQuadruple :: [Quadruple] -> BlockControlFlowGraph Quadruple
buildBlockControlFlowGraphForQuadruple = buildBlockControlFlowGraph labelOf branchDstOf  
 
