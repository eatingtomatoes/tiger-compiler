{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Backend.AssemblerGen where

import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.ByteString.Lazy.Builder (Builder, toLazyByteString)
import Data.List.Split
import Data.List (intersperse)

import Translation.Label
import Backend.Instruction
import Translation.Fragment
import Backend.ToBuilder
import Backend.Register

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

genFuntion :: Label -> [Instruction Register] -> Builder
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
