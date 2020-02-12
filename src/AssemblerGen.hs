{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module AssemblerGen where

import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.ByteString.Lazy.Builder (Builder, toLazyByteString)
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Except
import Debug.Pretty.Simple
import Data.Maybe (catMaybes)
import Data.List.Split
import Data.List (intersperse)
import qualified Data.Set as Set
import Control.Lens (zoom, _1)

import Label
-- import Instruction
import PseudoInst
import Temp
import Fragment
import ToBuilder
import Register
-- import InstructionSelection
import PseudoInstSelection
import RegisterAllocation
import Frame

genAssemblerCode :: Builder -> Builder -> Char8.ByteString
genAssemblerCode dataSec textSec = toLazyByteString $ joinLines [dataSec, textSec]

genDataSection :: [Builder] -> Builder
genDataSection strs = joinLines $ dataHeader : strs

genTextSection :: [Char8.ByteString] -> [Char8.ByteString] -> [Builder] -> Builder
genTextSection global external funs = joinLines $ textHeader : global' : external' : funs
  where
    prefix x y = toBuilder x <> toBuilder y
    global' = joinLines $ ("global " `prefix`) <$> global    
    external' = joinLines $ ("extern " `prefix`) <$> external
 
textHeader :: Builder
textHeader = toBuilder "[section .text]"

dataHeader :: Builder
dataHeader = toBuilder "[section .data]"

genFuntion :: Label -> [PseudoInst Register] -> Builder
genFuntion name body
  = joinLines
  $ (toBuilder name <> toBuilder ":")
  : fmap (toBuilder . show) body

encodeType :: (Label, [Maybe Label]) -> Builder
encodeType (label, fields) = joinLines $ header : fields'
  where
    header = joinLines
      [ toBuilder label <> toBuilder ":"
      , toBuilder "dq 0x1234321"
      , toBuilder $ "dq " <> show (length fields)
      ]
    fields' = fmap f fields
      where
        f (Just (Label str)) = toBuilder "dq " <> toBuilder str
        f _ = toBuilder "dq 0"

encodeStrFrag :: StrFrag -> Builder
encodeStrFrag StrFrag{..}
  = label <> toBuilder ": db " <> content <> toBuilder ", 0x0"
  where
    label = toBuilder _sfLabel
    content = mconcat $ intersperse comma $ intersperse linebreak $ fmap (toBuilder . show) $ splitOn "\\n" $ Char8.unpack  _sfContent
      where
        linebreak = toBuilder "0xa"
        comma = toBuilder ", "

encodeInstruction :: RegisterDist -> PseudoInst Temp -> PseudoInst Register
encodeInstruction dist pit = fmap f pit
  where
    f x = case Map.lookup x dist of
      Just r -> r
      _ -> error $ "no assigned register for temp `" <> show x <> "'"

removeRedundantMove :: [PseudoInst Register] -> [PseudoInst Register]
removeRedundantMove insts = filter (not . p) insts
  where
    p PseudoInst{..} = case (_piOperator, _piAddrPattern) of
      (Mov, RR x y) | x == y -> True
      _ -> False

removeLonelyLabel :: [PseudoInst r] -> [PseudoInst r]
removeLonelyLabel insts = filter p insts 
  where
    p = maybe True (flip Set.member bucket) . labelOf
    bucket = Set.fromList $ catMaybes $ fmap branchDstOf insts

removeRedundantJump :: [PseudoInst r] -> [PseudoInst r]
removeRedundantJump (j : l : rest) = f $ removeRedundantJump (l : rest)
  where
    matched = maybe False id $ (==) <$> branchDstOf j <*> labelOf l
    f = if matched then id else (j :)
removeRedundantJump insts = insts
