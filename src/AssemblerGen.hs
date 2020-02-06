{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module AssemblerGen where

import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.ByteString.Lazy.Builder (Builder, toLazyByteString)
import qualified Data.Map.Strict as Map
import Data.Bifunctor
import Control.Monad.State
import Control.Monad.Except
import Debug.Pretty.Simple
import Data.Maybe (catMaybes)
import qualified Data.Set as Set

import Label
import Instruction
import Register
import Temp
import Fragment
import ToBuilder
import InstructionSelection
import RegisterAllocation
import Util
import Frame

data ASMError
  = NoMapForTemp Instruction Temp
  | IndexError Instruction IndexError
  deriving (Show)

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

genFuntion :: Label -> Builder -> Builder
genFuntion name body = joinLines $ [ toBuilder name <> toBuilder ":", body ]

encodeStrFrag :: StrFrag -> Builder
encodeStrFrag StrFrag{..}
  = label <> toBuilder ": db '" <> content <> toBuilder "', 0x0"
  where
    label = toBuilder _sfLabel
    content = toBuilder _sfContent
 
encodeProcFrag :: ProcFrag -> ExceptT String (State (TempPool, LabelPool)) Builder
encodeProcFrag frag@ProcFrag{..} = do
  -- entire <- removeRedundantJump <$> selectInstructions frag
  entire <- selectInstructions frag
  case allocRegisters entire machineRegisters of
    Left temp -> encodeProcFrag $ spill temp frag
    Right dist -> do
      insts <- unify $ do
        mapM (encodeInstruction $ buildTempMapper dist)
          $ removeLonelyLabel
          $ removeRedundantJump entire
      return $ genFuntion (_frName _pfFrame) $ joinLines $ catMaybes insts

removeLonelyLabel :: [Instruction] -> [Instruction]
removeLonelyLabel insts = filter p insts 
  where
    p = maybe True (flip Set.member bucket) . labelOf
    bucket = Set.fromList $ catMaybes $ fmap f insts
    f (Jmp _ label) = Just label
    f _ = Nothing

type TempMapper = Temp -> Either (Instruction -> ASMError) Register

buildTempMapper :: RegisterDist -> TempMapper
buildTempMapper dist t = maybe (Left $ flip NoMapForTemp t) Right $ Map.lookup t dist

encodeInstruction :: TempMapper -> Instruction -> Either ASMError (Maybe Builder)
encodeInstruction mapper inst = first ($ inst) $ do
  let mapAll = sequence . fmap mapper 
  case inst of
    Oper s fmt dsts srcs keys -> do
      dsts' <- mapAll dsts
      srcs' <- mapAll srcs
      if s && dsts' == srcs'
        then return Nothing
        else bimap (flip IndexError) (Just . toBuilder) $ format fmt dsts' srcs' keys
    Lab label -> do
      return $ Just $ toBuilder label <> toBuilder ":"
    Jmp op dst -> do
      return $ Just $ toBuilder op <> toBuilder " " <> toBuilder dst