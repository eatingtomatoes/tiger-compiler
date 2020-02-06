module InterferenceGraph
  ( InterferenceGraph
  , buildInterferenceGraph
  ) where

import qualified Data.Set as Set
import Control.Arrow
import qualified Data.Map.Strict as Map

import Graph
import Temp
import Instruction
import Liveness

type InterferenceGraph = Graph Temp

buildInterferenceGraph :: [Instruction] -> InterferenceGraph
buildInterferenceGraph insts = uncurry (buildInterferenceGraph_ insts) $ calcInsOuts insts

buildInterferenceGraph_ :: [Instruction] -> [Set.Set Temp] -> [Set.Set Temp] -> InterferenceGraph
buildInterferenceGraph_ insts ins outs = addExtra $ foldr connectGroup mempty ins
  where
    addExtra g = foldr f g (zip insts outs) 
      where
        f (inst, out) g' = foldr (uncurry connect) g' $ Set.cartesianProduct dsts neighbors 
          where
            neighbors = (if isMove inst then flip Set.difference srcs else id) out
            (dsts, srcs) = (defs &&& uses) inst


