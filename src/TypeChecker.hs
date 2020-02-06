{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
 
module TypeChecker
  ( checkType
  ) where

import Control.Monad.State
import Control.Monad.Except
import Data.Default (def)
import qualified Data.List as List

import Ast
import AstHelper
import SymTab
import SemanticError

type Context = ExceptT SemanticError (State SymTab)

checkType :: SymTab -> Program -> Either SemanticError ()
checkType symTab (Program decs) = flip evalState symTab $ runExceptT $ do
   when (not $ all isFunDec decs) $ do
     throwError $ OnlyFunDecAllowedAtTopLevel
   mapM_ glanceDec decs
   mapM_ checkDec decs

isFunDec :: Dec -> Bool
isFunDec (FunDec _ _ _ _) = True
isFunDec _ = False

glanceDec :: Dec -> Context ()
glanceDec dec = case dec of
  TypeDec tid body -> do
    modify $ registerType tid (makeTypeSym body)
  VarDec (vid, tid) _ -> do
    modify $ registerVar vid (makeVarSym tid)
  FunDec fid formals result _ -> do
    modify $ registerFun fid (makeFunSym formals result False)
  _ -> throwError $ UnknownDec dec

checkDec :: Dec -> Context ()
checkDec dec = case dec of
  TypeDec _ body -> do
    checkTypeBody body
  VarDec binding@(_, tid) initial -> do
    checkTypeBinding binding
    checkExpr initial >>= matchType tid 
  FunDec _ formals result body -> do
    newScope $ do
      mapM_ checkTypeBinding formals
      assertTypeExistence result
      actual <- checkExpr body
      matchType result actual
  PrimitiveFunDec _ formals result -> do
    newScope $ do
      mapM_ checkTypeBinding formals
      assertTypeExistence result

-- matchType :: TypeId -> TypeId -> Context ()
-- matchType x y
--   | x == nilType = return ()
--   | y == nilType = return ()
--   | otherwise = when (x /= y) $ throwError $ MismatchedType x y

matchType :: TypeId -> TypeId -> Context ()
matchType x y
  | y == anyType = error "`Any` is not allowed at the right side of matchType"
  | x == anyType = return ()
  | otherwise = do
      when (x /= y) $ do
        throwError $ MismatchedType x y

checkExpr :: Expr -> Context TypeId
checkExpr expr = case expr of
  VarExpr var -> checkVar var
  IntExpr _ -> return intType
  StrExpr _ -> return strType
  BinExpr op x y -> case op of
    _ | elem op intOps -> do
          checkExpr x >>= matchType intType
          checkExpr y >>= matchType intType
          return intType
    _ | elem op doubleOps -> do
          checkExpr x >>= matchType doubleType
          checkExpr y >>= matchType doubleType
          return doubleType
    _ | elem op relOps -> do
          xt <- checkExpr x
          yt <- checkExpr y
          matchType xt yt
          return boolType
    _ | elem op boolOps -> do
          checkExpr x >>= matchType boolType
          checkExpr y >>= matchType boolType
          return boolType
    _ -> throwError $ UnknownBinOp op 
  UnExpr op x -> do
    let tid = case op of
          Not -> boolType
          Negate -> intType
    checkExpr x >>= matchType tid
    return tid
  SeqExpr es -> do
    mapM checkExpr es >>= \case
      [] -> return voidType
      ts -> return $ last ts
  ArrayExpr tid _ _ -> return tid
  RecordExpr tid _ -> return tid
  CallExpr fid args -> do
    ats <- mapM checkExpr args
    matchFunArgs fid ats
  AssignExpr var value -> do
    x <- checkVar var
    y <- checkExpr value
    matchType x y
    -- return nilType
    return voidType
  IfExpr cond x y -> do
    checkExpr cond >>= matchType boolType 
    xt <- checkExpr x
    yt <- checkExpr y
    matchType xt yt
    return xt
  WhileExpr cond body -> do
    checkExpr cond >>= matchType boolType
    mapM_ checkExpr body
    return voidType
  ForExpr binding@(_, tid) low high body -> do
    checkExpr low >>= matchType tid 
    checkExpr high >>= matchType tid
    newScope $ do
      checkTypeBinding binding
      mapM_ checkExpr body
    return voidType
  BreakExpr -> do
    return voidType
  LetExpr decs body -> do
    newScope $ do
      mapM_ glanceDec decs
      mapM_ checkDec decs
      ts <- mapM checkExpr body
      if | null ts -> throwError EmptyLetBody
         | otherwise -> return $ last ts
  where
    intOps = [ Plus
             , Minus
             , Times ]
    doubleOps = [ Divide ]
    relOps = [ Equal
             , NotEqual
             , Less
             , LessOrEqual
             , Greater
             , GreaterOrEqual
             ]
    boolOps = [ And
              , Or
              ]

checkVar :: Var -> Context TypeId
checkVar var = case var of
  SimpleVar vid -> do
    varSym <- gets (findVar vid) `awareOf` UnknownVar vid
    return $ _vsType varSym
  FieldVar owner vid -> do
    ot <- checkVar owner
    typeSym <- gets (findType ot) `awareOf` UnknownType ot
    case _tsBody typeSym of
      RecordBody fields _ -> do
        case List.find (\(fid, _) -> fid == vid) fields of
          Nothing -> throwError $ NoSuchFiled owner vid
          Just (_, ft) -> return ft
      _ -> throwError $ NotRecord owner
  SubscriptVar array index -> do
    checkExpr index >>= matchType intType 
    at <- checkVar array
    typeSym <- gets (findType at) `awareOf` UnknownType at
    case _tsBody typeSym of
      ArrayBody et -> return et
      _ -> throwError $ NotArray array
    
matchFunArgs :: VarId -> [TypeId] -> Context TypeId
matchFunArgs fid ats = do
  funSym <- gets (findFun fid) `awareOf` UnknownFun fid
  let fts = fmap (\(_, t) -> t) $ _fsFormals funSym
  sequence_ $ zipWith matchType fts ats
  return $ _fsResult funSym

newScope :: Context a -> Context a
newScope action = do
  parent <- get
  put def { _stParent = Just parent }
  x <- action
  put parent
  return x

checkTypeBody :: TypeBody -> Context ()
checkTypeBody body = case body of
  Alias tid -> assertTypeExistence tid
  RecordBody fields _ -> mapM_ checkTypeBinding fields
  ArrayBody tid -> assertTypeExistence tid
  _ -> return ()

checkTypeBinding :: TypeBinding -> Context ()
checkTypeBinding (vid, tid) = do
  assertTypeExistence tid
  modify $ registerVar vid (makeVarSym tid)

assertTypeExistence :: TypeId -> Context ()
assertTypeExistence tid = do
  void $ gets (findType tid) `awareOf` UnknownType tid

awareOf :: Context (Maybe a) -> SemanticError -> Context a
awareOf action err = action >>= maybe (throwError err) return
