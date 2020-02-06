module Instruction
  ( Key (..)
  , KeySet (..)
  , Instruction (..)
  , IndexError (..)
  , defs
  , uses
  , isMove
  , labelOf
  , format
  ) where

import Temp
import Label
import Text.Printf
import Safe (atMay)
import qualified Data.Set as Set

data Key
  = Dst Int
  | Src Int
  deriving (Show, Eq)

data KeySet
  = Zero  
  | One Key 
  | Two Key Key
  | Three Key Key Key
  deriving (Show, Eq)

data Instruction
  = Oper Bool{-is move-} String [Temp] [Temp] KeySet 
  | Lab Label
  | Jmp String Label
  deriving (Eq)

instance Show Instruction where
  show (Oper _ fmt dsts srcs keySet) =
    case format fmt dsts srcs keySet of
      Left err -> show err
      Right s -> show s
  show (Lab label) = show label
  show (Jmp op dst) = show $ op <> " " <> show dst

defs :: Instruction -> Set.Set Temp
defs (Oper _ _ xs _ _) = Set.fromList xs
defs _ = mempty

uses :: Instruction -> Set.Set Temp         
uses (Oper _ _ _ xs _) = Set.fromList xs
uses _ = mempty

isMove :: Instruction -> Bool
isMove (Oper s _ _ _ _) = s
isMove _ = False

labelOf :: Instruction -> Maybe Label
labelOf (Lab label) = Just label
labelOf _ = Nothing
  
data IndexError
  = DstNotFound Int
  | SrcNotFound Int
  deriving (Show)

format :: PrintfArg a => String -> [a] -> [a] -> KeySet -> Either IndexError String
format fmt dsts srcs keySet = do
  case keySet of
    Zero -> return fmt
    One k -> do
      x <- indexBy dsts srcs k
      return $ printf fmt x
    Two k1 k2 -> do
      x <- indexBy dsts srcs k1
      y <- indexBy dsts srcs k2
      return $ printf fmt x y
    Three k1 k2 k3 -> do
      x <- indexBy dsts srcs k1
      y <- indexBy dsts srcs k2
      z <- indexBy dsts srcs k3      
      return $ printf fmt x y z

indexBy :: [a] -> [a] -> Key -> Either IndexError a
indexBy dsts srcs k = do
  case k of
    Dst i -> maybe (Left $ DstNotFound i) Right $ dsts `atMay` i
    Src i -> maybe (Left $ SrcNotFound i) Right $ srcs `atMay` i
