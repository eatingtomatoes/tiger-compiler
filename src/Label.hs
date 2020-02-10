{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
  
module Label where

import qualified Data.ByteString.Lazy.Char8 as Char8
import Control.Monad.State
import Control.Lens
import Data.Default

data Label
  = Label
  { _lbString :: Char8.ByteString
  } deriving (Show, Eq, Ord)

data LabelPool
  = LabelPool
  { _lplCount :: Int
  } deriving (Show)

makeLenses ''Label
makeLenses ''LabelPool

instance Default LabelPool where
  def = LabelPool
    { _lplCount = 0
    }

allocLabel :: MonadState LabelPool m => m Label
allocLabel = do
  modifying lplCount (+1)
  str <- ("label_" ++ ) . show <$> use lplCount
  return $ Label $ Char8.pack str

allocLabelWithTag :: MonadState LabelPool m => Char8.ByteString -> m Label
allocLabelWithTag tag = do
  modifying lplCount (+1)
  str <- ("label_" ++ ) .show <$> use lplCount
  return $ Label $ Char8.pack str <> "_" <> tag

labelFrom :: Char8.ByteString -> Label
labelFrom str = Label str

toString :: Label -> String
toString = Char8.unpack . _lbString
