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

genFuntion :: Label -> Builder -> Builder
genFuntion name body = joinLines $ [ toBuilder name <> toBuilder ":", body ]

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
  = label <> toBuilder ": db '" <> content <> toBuilder "', 0x0"
  where
    label = toBuilder _sfLabel
    content = toBuilder _sfContent
 
-- encodeProcFrag :: ProcFrag -> ExceptT String (State (TempPool, LabelPool)) Builder
-- encodeProcFrag = calc retryLimit
--   where
--     retryLimit = 3 :: Int
--     calc 0 _=  error "Failed to allocate registers"
--     calc retry frag@ProcFrag{..} = do
--       entire <- selectInstructions frag
--       case allocRegisters entire of
--         -- Left temp -> calc (pred retry) $ pTrace ("spill " <> show temp <> " out") $ spill temp frag
--         Left temp -> error $ show temp <> " needs spilling"
--         Right dist -> do
--           insts <- unify $ do
--             mapM (encodeInstruction $ buildTempMapper dist)
--               $ removeLonelyLabel
--               $ removeRedundantJump entire
--           return $ genFuntion (_frName _pfFrame) $ joinLines $ catMaybes insts

encodeProcFrag :: ProcFrag -> ExceptT String (State (TempPool, LabelPool)) Builder
encodeProcFrag frag@ProcFrag{..} = do
  insts <- selectInstructions frag
  calc limit insts
  where
    limit = 10 ::Int
    calc 0 _ =  error "Failed to allocate registers" 
    calc retry insts = do
      case allocRegisters insts of
        Left temp -> do
          insts' <- zoom _1 $ pTrace (show temp <> " needs spilling out") spill temp insts
          calc (pred retry) insts'
        Right dist -> return
            $ genFuntion (_frName _pfFrame)
            $ joinLines
            $ fmap (toBuilder . show)
            $ removeRedundantMove
            $ fmap (encodeInstruction dist)
            $ removeLonelyLabel
            $ removeRedundantJump insts

removeLonelyLabel :: [PseudoInst Temp] -> [PseudoInst Temp]
removeLonelyLabel insts = filter p insts 
  where
    p = maybe True (flip Set.member bucket) . labelOf
    bucket = Set.fromList $ catMaybes $ fmap branchDstOf insts

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
-- type TempMapper = Temp -> Either (PseudoInst Temp -> ASMError) Register

-- buildTempMapper :: RegisterDist -> TempMapper
-- buildTempMapper dist t = maybe (Left $ flip NoMapForTemp t) Right $ Map.lookup t dist

-- encodeInstruction :: TempMapper -> PseudoInst Temp -> Either ASMError (Maybe Builder)
-- encodeInstruction mapper inst = first ($ inst) $ do
--   let mapAll = sequence . fmap mapper 
--   case inst of
--     Oper _ fmt@("mov %r, %r") dsts srcs keys -> do
--       dsts' <- mapAll dsts
--       srcs' <- mapAll srcs
--       if dsts' == srcs'
--         then return Nothing
--         else bimap (flip IndexError) (Just . toBuilder) $ format fmt dsts' srcs' keys
--     Oper _ fmt dsts srcs keys -> do
--       dsts' <- mapAll dsts
--       srcs' <- mapAll srcs
--       bimap (flip IndexError) (Just . toBuilder) $ format fmt dsts' srcs' keys        
--     Lab label -> do
--       return $ Just $ toBuilder label <> toBuilder ":"
--     Jmp op dst -> do
--       return $ Just $ toBuilder op <> toBuilder " " <> toBuilder dst
--   where

