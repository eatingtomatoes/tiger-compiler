{-# LANGUAGE RecordWildCards #-}

module Backend.Optimization
  ( removeRedundantMove
  , removeLonelyLabel
  , removeRedundantJump
  ) where
 
import qualified Data.Set as Set
import Data.Maybe (catMaybes)

import Backend.Instruction

removeRedundantMove :: Ord r => [Instruction r] -> [Instruction r]
removeRedundantMove insts = filter (not . p) insts
  where
    p Instruction{..} = case (_piOperator, _piAddressPattern) of
      (Mov, RR x y) | x == y -> True
      _ -> False

removeLonelyLabel :: [Instruction r] -> [Instruction r]
removeLonelyLabel insts = filter p insts 
  where
    p = maybe True (flip Set.member bucket) . labelOf
    bucket = Set.fromList $ catMaybes $ fmap branchDstOf insts

removeRedundantJump :: [Instruction r] -> [Instruction r]
removeRedundantJump (j : l : rest) = f $ removeRedundantJump (l : rest)
  where
    matched = maybe False id $ (==) <$> branchDstOf j <*> labelOf l
    f = if matched then id else (j :)
removeRedundantJump insts = insts
