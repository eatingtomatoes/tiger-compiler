{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ToBuilder where

import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.List as List 

import Label


class ToBuilder a where
  toBuilder :: a -> Builder.Builder

instance ToBuilder Char8.ByteString where
  toBuilder = Builder.lazyByteString

instance ToBuilder String where
  toBuilder = Builder.stringUtf8

instance ToBuilder Label where
  toBuilder = toBuilder . _lbString 

joinLines :: [Builder.Builder] -> Builder.Builder
joinLines = mconcat . List.intersperse linebreak 
  where
    linebreak = toBuilder "\n"
