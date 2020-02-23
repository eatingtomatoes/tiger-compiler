{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Translation.Frame where

import qualified Data.Map.Strict as Map
import Control.Lens
import Control.Monad.State
-- import Debug.Pretty.Simple

import FrontEnd.Ast (VarId)
import Translation.Temp
import Translation.TempHelper
import Translation.Label
import MiddleEnd.Tree
import MiddleEnd.TreeHelper

type Offset = Int

data Access
  = InFrame Offset
  | InReg Temp
  deriving (Show)

data Frame
  = Frame
  { _frName :: Label
  , _frFormalsList :: [Temp]
  , _frFormals :: Map.Map VarId Access
  , _frPointer :: Int
  , _frWordSize :: Int
  , _frDepth :: Int
  } deriving (Show)

makeLenses ''Frame

makeFrame :: Label -> [(VarId, Temp)] -> Int -> Frame
makeFrame name formals dep = Frame
  { _frName = name
  -- , _frFormals = fst $ foldl f (Map.empty, 2 * ws) formals
  , _frFormalsList = fmap snd formals
  , _frFormals = Map.fromList $ fmap (over _2 InReg) formals
  , _frPointer = - ws -- left 8 bytes for staticlinking
  , _frWordSize = ws
  , _frDepth = dep
  }
  where
    -- f (t, offset) (vid, size) = (Map.insert vid (InFrame offset) t, offset + size * ws)
    ws = 8    

allocLocal :: MonadState Frame m => m Access
allocLocal = do
  ws <- use frWordSize
  modifying frPointer $ \x -> x - ws
  InFrame <$> use frPointer

toExpr :: Access -> Expr -> Expr
toExpr access fp = case access of
  InFrame offset -> MemExpr $ plus (ConstExpr offset) fp
  InReg temp_ -> TempExpr temp_

framePointer :: MonadState Frame m => m Expr
framePointer = return $ TempExpr bp

staticLinking :: MonadState Frame m => Int -> m Expr
staticLinking depth = do
  ws <- use frWordSize
  let calc dep bp
        | dep < 0 = error $ "invalid depth: " ++ show dep
        | dep == 0 = bp
        | otherwise = calc (dep - 1) $ MemExpr $ minus (constant ws) bp
  framePointer >>= return . calc depth 
  

  
  
  
