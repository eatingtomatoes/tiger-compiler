module Register where

import Text.Printf
import Data.Char

data Register
  = RAX
  | RBX
  | RCX
  | RDX
  | RDI  
  | RSI
  | RBP
  | RSP
  | R8
  | R9
  | R10
  | R11
  deriving (Show, Eq, Ord)

stringify :: Register -> String
stringify = fmap toLower . show

instance PrintfArg Register where
  formatArg x fmt
    | fmtChar (vFmt 'r' fmt) == 'r' = formatString (stringify x) fmt
      { fmtChar = 's'
      , fmtPrecision = Nothing
      }
    | otherwise = errorBadFormat $ fmtChar fmt

machineRegisters :: [Register]
machineRegisters =
  [ RAX
  , RBX
  , RCX
  , RDX
  , RDI  
  , RSI
  , RBP
  , RSP
  , R8
  , R9
  , R10
  , R11
  ]
