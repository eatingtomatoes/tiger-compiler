{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Temp where

import Control.Monad.State
import Control.Lens
import Data.Default
import Text.Printf

data Temp
  = Temp
  { _tpIndex :: Int
  }
  | Reg String
  deriving (Eq)

instance Ord Temp where
  Reg _ <= Temp _ = True
  Temp _ <= Reg _ = False
  Temp x <= Temp y = x <= y
  Reg x <= Reg y = x <= y

instance Show Temp where
  show (Temp i) = "t" ++ show i
  show (Reg name) = name

data TempPool
  = TempPool
  { _tplCount :: Int
  } deriving (Show)

makeLenses ''Temp
makeLenses ''TempPool

isTemp :: Temp -> Bool
isTemp (Temp _) = True
isTemp _ = False

isReg :: Temp -> Bool
isReg (Reg _) = True
isReg _ = False

allocTemp :: MonadState TempPool m => m Temp
allocTemp = do
  modifying tplCount (+1)
  Temp <$> use tplCount

instance Default TempPool where
  def = TempPool
    { _tplCount = 1
    }
  
instance PrintfArg Temp where
  formatArg x fmt
    | fmtChar (vFmt 'r' fmt) == 'r' = formatString (show x) fmt
      { fmtChar = 's'
      , fmtPrecision = Nothing
      }
    | otherwise = errorBadFormat $ fmtChar fmt
