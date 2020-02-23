{-# LANGUAGE LambdaCase #-}
 
module Backend.Liveness
  ( calcInsOuts
  , calcDefsUses
  ) where

import qualified Data.Set as Set
import Safe (atMay)
import Control.Arrow
import Data.Maybe (catMaybes)

import Translation.Temp
import Backend.Instruction
import Backend.ControlFlowGraph
import Data.Functor ((<&>))

import Utility.Fixpoint

calcInsOuts :: [Instruction Temp] -> ([Set.Set Temp], [Set.Set Temp])
calcInsOuts insts = uncurry (calcInsOuts_ $ buildControlFlowGraph insts) $ calcDefsUses insts

calcDefsUses :: [Instruction Temp] -> ([Set.Set Temp], [Set.Set Temp])
calcDefsUses = unzip . fmap defsAndUses 

calcInsOuts_ :: ControlFlowGraph -> [Set.Set Temp] -> [Set.Set Temp] -> ([Set.Set Temp], [Set.Set Temp])
calcInsOuts_ graph idefs iuses = fixpoint (==) calc $ id &&& id $ mempty <$ idefs
  where
    calc (ins, outs) = (ins', outs')
      where
        ins' = zipWith3 f iuses outs idefs
        f use out def = use `Set.union` (out `Set.difference` def)

        outs' = fmap (\i -> fmap Set.toList $ successorsOf i graph) (zipWith const [0..] outs)
          <&> maybe mempty (Set.unions . catMaybes . fmap (ins `atMay`)) 
