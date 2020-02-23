module MiddleEnd.Quadruple.BlockControlFlowGraph
  ( BlockControlFlowGraph
  , buildBlockControlFlowGraphForQuadruple
  ) where

import MiddleEnd.Quadruple

import Utility.DirectedGraph.MapBasedDirectedGraph.BlockControlFlowGraph

buildBlockControlFlowGraphForQuadruple :: [Quadruple] -> BlockControlFlowGraph Quadruple
buildBlockControlFlowGraphForQuadruple = buildBlockControlFlowGraph labelOf branchDstOf  
