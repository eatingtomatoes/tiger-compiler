{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module SymbolTable where
 
import qualified Data.Map.Strict as Map
import Control.Lens hiding (lens)
import Data.Default (Default, def)
import Control.Applicative ((<|>))
import Control.Monad.State

import Ast
import Frame
import Label

data SymbolTable
  = SymbolTable
  { _stTypes :: Map.Map TypeId TypeSym
  , _stVars :: Map.Map VarId VarSym
  , _stFuns :: Map.Map VarId FunSym
  , _stParent :: Maybe SymbolTable
  } deriving (Show)

data TypeSym
  = TypeSym
  { _tsBody :: TypeBody
  } deriving (Show)

data VarSym
  = VarSym
  { _vsType :: TypeId
  , _vsDepth :: Int  
  , _vsAccess :: Maybe Access
  } deriving (Show)

data FunSym
  = FunSym
  { _fsFormals :: [TypeBinding]
  , _fsResult :: TypeId
  , _fsName :: Label
  } deriving (Show)

makeLenses ''TypeSym
makeLenses ''VarSym
makeLenses ''FunSym
makeLenses ''SymbolTable

instance Default SymbolTable where
  def = SymbolTable
    { _stTypes = Map.empty
    , _stVars = Map.empty
    , _stFuns = Map.empty
    , _stParent = Nothing
    }

register
  :: (MonadState SymbolTable m, Ord k)
  => ASetter' SymbolTable (Map.Map k v)
  -> k
  -> v
  -> m ()
register lens key value = modifying lens (Map.insert key value)

registerType :: MonadState SymbolTable m => TypeId -> TypeSym -> m ()
registerType = register stTypes 

registerVar :: MonadState SymbolTable m => VarId -> VarSym -> m ()
registerVar vid sym = do
  register stVars vid sym
  
completeVar :: MonadState SymbolTable m => VarId -> Access -> m ()
completeVar vid access
  = modifying stVars
  $ Map.adjust (set vsAccess $ Just access) vid

registerFun :: MonadState SymbolTable m => VarId -> FunSym -> m ()
registerFun = register stFuns

findSym
  :: (MonadState SymbolTable m, Ord k)
  => Lens' SymbolTable (Map.Map k v)
  -> k
  -> m (Maybe v)
findSym lens key = do
  get >>=  return
    . foldr (flip (<|>)) Nothing
    . fmap (Map.lookup key)
    . collectFields lens

collectFields :: Lens' SymbolTable a -> SymbolTable -> [a]
collectFields lens symTab = fmap (view lens) $ collect_ [] symTab
  where
    collect_ tabs st = maybe tabs' (collect_ tabs') $  _stParent st
      where tabs' = st : tabs

findType :: MonadState SymbolTable m => TypeId -> m (Maybe TypeSym)
findType = findSym stTypes 

findVar :: MonadState SymbolTable m => VarId -> m (Maybe VarSym)
findVar = findSym stVars

findFun :: MonadState SymbolTable m => VarId -> m (Maybe FunSym)
findFun = findSym stFuns

makeTypeSym :: TypeBody -> TypeSym
makeTypeSym body = TypeSym
  { _tsBody = body
  }

makeVarSymHead :: TypeId -> Int -> VarSym
makeVarSymHead tid depth = VarSym
  { _vsType = tid
  , _vsDepth = depth
  , _vsAccess = Nothing
  }

makeVarSym :: TypeId -> Access -> Int -> VarSym
makeVarSym tid access depth = VarSym
  { _vsType = tid
  , _vsDepth = depth
  , _vsAccess = Just access
  }

makeFunSym :: [TypeBinding] -> TypeId -> Label -> FunSym
makeFunSym formals result name = FunSym
  { _fsFormals = formals
  , _fsResult = result
  , _fsName = name
  }

findLocalVar :: VarId -> SymbolTable -> Maybe VarSym
findLocalVar vid st = Map.lookup vid $ _stVars st

nextSymbolTable :: SymbolTable -> SymbolTable
nextSymbolTable parent@SymbolTable{..} = def
  { _stParent = Just parent
  }
