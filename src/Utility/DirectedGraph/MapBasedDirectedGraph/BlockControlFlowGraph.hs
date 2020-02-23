{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Utility.DirectedGraph.MapBasedDirectedGraph.BlockControlFlowGraph
  ( BlockControlFlowGraph
  , buildBlockControlFlowGraph
  ) where

import Data.List.Split (split, keepDelimsL, keepDelimsR, whenElt)
import Data.Maybe (isJust, catMaybes)
import Control.Monad ((>=>))
import Safe (headMay, lastMay)

import Utility.DirectedGraph.MapBasedDirectedGraph 
import Translation.Label
import Translation.LabelMap

newtype BlockControlFlowGraph a = BlockControlFlowGraph (MapBasedDirectedGraph Int [a])
  deriving (DirectedGraph) 

-- 基本块的排列顺序与Id的顺序一致
buildBlockControlFlowGraph :: (a -> Maybe Label) -> (a -> Maybe [Label]) -> [a] -> BlockControlFlowGraph a
buildBlockControlFlowGraph labelOf branchDstOf xs = foldr f unconnected $ zip [0..] blocks
  where
    f (i, ys) g
      = foldr (connectTo i) g
      $ filter (< length blocks) 
      $ (\js -> if null js then [i + 1] else js)
      $ maybe mempty (catMaybes . fmap (flip posOf labelMap))
      $ lastMay ys >>= branchDstOf

    unconnected = foldr ($) empty $ zipWith addVertex [0..] blocks
      
    labelMap = buildLabelMap (headMay >=> labelOf) blocks
        
    blocks = filter (not . null)
      $ concat
      $ fmap (split (keepDelimsR $ whenElt $ isJust . branchDstOf))
      $ split (keepDelimsL $ whenElt $ isJust . labelOf) xs

                        
