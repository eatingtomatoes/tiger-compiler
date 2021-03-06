{-# LANGUAGE OverloadedStrings #-}            
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
 
module Translation.AstToTree
  ( transAstToTree
  ) where
 
import Control.Monad.State
import Control.Monad.Except
import Control.Lens hiding (index, lens, op)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Bifunctor
import Control.Monad ((>=>))

import Debug.Pretty.Simple

import FrontEnd.Ast
import FrontEnd.AstHelper
import Translation.AstToTree.TransError
import Translation.AstToTree.TransState
import Translation.Fragment
import qualified MiddleEnd.Tree as Tree
import qualified MiddleEnd.TreeHelper as TH
import Translation.Label 
import Translation.Temp
import Translation.Frame
import Translation.AstToTree.SymbolTable

type Trans = ExceptT TransError (State TransState)

transAstToTree :: TransState -> Program -> Either String TransState
transAstToTree initial program = first show $ genInterRep' initial program

genInterRep' :: TransState -> Program -> Either TransError TransState
genInterRep' initial program = flip evalState initial $ runExceptT $ transProgram program >> get
 
transProgram :: Program -> Trans ()
transProgram (Program decs) = do
  when (any isVarDec decs) $ do 
     throwError $ NoVarDecAtTopLevelAllowed
  mapM_ glanceDec decs 
  mapM_ transDec decs

glanceDec :: Dec -> Trans ()
glanceDec dec = do
  case dec of
    TypeDec tid@(TypeId name) body -> do
      info <- zoom trsLabelPool $ allocLabelWithTag name
      zoom trsSymbolTable $ registerType tid $ makeTypeSym body info
    VarDec (vid, tid) _ -> do
      access <- zoom trsFrame allocLocal
      depth <- use $ trsFrame . frDepth
      zoom trsSymbolTable $ registerVar vid $ makeVarSym tid access depth
    FunDec fid@(VarId fname) formals result body isGlobal -> do
      name <- do
        if | isGlobal -> do 
               addGlobalName fname
               return $ labelFrom fname
           | isNothing body -> do
               addExternalName fname
               return $ labelFrom fname
           | otherwise -> do
               zoom trsLabelPool $ allocLabelWithTag fname
      let sym = makeFunSym formals result name
      zoom trsSymbolTable $ registerFun fid sym

transDec :: Dec -> Trans Tree.Stmt
transDec dec = do
  case dec of
    TypeDec _ body -> do
      checkTypeBody body
      return TH.dummyStmt
    VarDec (vid, tid) initial -> do
      assertTypeExistence tid
      (itid, iunion) <- transExpr initial
      matchType tid itid
      access <- do
        sym <- zoom trsSymbolTable (findVar vid) `awareOf` UnknownVar vid
        return (_vsAccess sym) `awareOf` UncompletedVar vid
      fp <- zoom trsFrame framePointer
      iexpr <- unEx iunion
      return $ TH.move (toExpr access fp) iexpr
    FunDec fid formals result body _ -> do
      assertTypeExistence result      
      name <- fmap _fsName $ zoom trsSymbolTable (findFun fid) `awareOf` UnknownFun fid
      case body of
        Just expr -> do
          params <- forM formals $ \(vid, tid) -> do
            assertTypeExistence tid
            zoom trsTempPool $ (vid, ) <$> allocTemp
            
          withFrame (makeFrame name params) $ do
            depth <- use $ trsFrame . frDepth
            newSymbolTable $ do
              forM_ formals $ \(pvid, ptid) -> do
                zoom trsSymbolTable $ registerVar pvid $ makeVarSymHead ptid depth
      
              fs <- use $ trsFrame . frFormals
              
              forM_ (Map.toList fs) $ \(pvid, access) -> do
                zoom trsSymbolTable $ completeVar pvid access
    
              (btid, bexpr) <- transExpr expr
              matchType result btid

              -- To return Void is to return nothing.
              if btid == voidType
                then unNx bexpr >>= emitFun 
                else unEx bexpr >>= emitFun . TH.ret
        _ -> return ()
      return TH.dummyStmt

toSeqStmt :: [Tree.Stmt] -> Tree.Stmt
toSeqStmt [] = TH.dummyStmt
toSeqStmt stmts = foldr1 Tree.SeqStmt stmts
    
data UnionExpr
  = Ex Tree.Expr
  | Nx Tree.Stmt
  | Cx (Label{-True-} -> Label{-False-} -> Tree.Stmt)

unEx :: UnionExpr -> Trans Tree.Expr
unEx (Ex expr) = return expr
unEx (Cx f) = do
  labels<- zoom trsLabelPool $ replicateM 3 allocLabel
  let [true, false, done] = labels
  temp <- zoom trsTempPool allocTemp
  let stmts = [ f true false
              , TH.label true
              , TH.move (TH.temp temp) TH.one
              , TH.jump done
              , TH.label false
              , TH.move (TH.temp temp) TH.zero
              , TH.label done
              ]
  return $ TH.eseq (toSeqStmt stmts) (TH.temp temp)
unEx (Nx stmt) = do
  throwError $ InvalidConversion $ "from Nx to Ex: " <> show stmt

unNx :: UnionExpr -> Trans Tree.Stmt
unNx (Ex expr) = return $ Tree.ExprStmt expr
unNx (Cx f) = do
  let true = labelFrom "true_label_for_debug"
      false = labelFrom "false_label_for_debug"
  throwError $ InvalidConversion $ "from Cx to Nx: " <> show (f true false)
unNx (Nx stmt) = return stmt

unCx :: UnionExpr -> Trans (Label -> Label -> Tree.Stmt)
unCx (Ex expr) = return $ TH.isTrue expr
unCx (Cx f) = return f 
unCx (Nx stmt) = do
  throwError $ InvalidConversion $ "from Nx to Cx: " <> show stmt

transExpr :: Expr -> Trans (TypeId, UnionExpr)
transExpr expr = do
  case expr of 
    VarExpr var -> fmap (second Ex) $ transVar var
    IntExpr x -> return (intType, Ex $ TH.constant x)
    StrExpr x -> do
      label <- zoom trsLabelPool allocLabel
      zoom trsStrFragSet $ addFrag StrFrag
        { _sfLabel = label
        , _sfContent = x
        }
      return (strType, Ex $ TH.name label)
    BinExpr op x y -> do
      if | elem op [ Plus, Minus, Times, Divide ] -> do
             (xtid, xunion) <- transExpr x
             (ytid, yunion) <- transExpr y
             matchType intType xtid
             matchType intType ytid
             xexpr <- unEx xunion
             yexpr <- unEx yunion
             let binOp f = return (intType, Ex $ f xexpr yexpr)
             case op of
               Plus -> binOp TH.plus
               Minus -> binOp TH.minus
               Times -> binOp TH.times
               Divide -> binOp TH.divide
               _ -> throwError $ UnhandledBinOp op
         | elem op [ Equal, NotEqual, Less, LessOrEqual, Greater, GreaterOrEqual ] -> do
             (xtid, xunion) <- transExpr x
             (ytid, yunion) <- transExpr y
             matchType intType xtid
             matchType intType ytid
             xexpr <- unEx xunion
             yexpr <- unEx yunion
             let cjump oper = return (condType, Cx $ TH.cjump oper xexpr yexpr)
             case op of
               Equal -> cjump Tree.Equal
               NotEqual -> cjump Tree.NotEqual
               Less -> cjump Tree.Less
               LessOrEqual -> cjump Tree.LessOrEqual
               Greater -> cjump Tree.Greater
               GreaterOrEqual -> cjump Tree.GreaterOrEqual             
               _ -> throwError $ UnhandledBinOp op
         | elem op [ Or, And ] -> do
             (xtid, xunion) <- transExpr x
             (ytid, yunion) <- transExpr y
             matchType condType xtid
             matchType condType ytid
             xf <- unCx xunion
             yf <- unCx yunion
             next <- zoom trsLabelPool allocLabel
             let f g = return (condType, Cx g) 
             case op of
                   Or -> f $ \true false -> toSeqStmt
                     [ xf true next
                     , Tree.LabelStmt next
                     , yf true false
                     ]
                   And -> f $ \true false -> toSeqStmt
                     [ xf next false
                     , Tree.LabelStmt next
                     , yf true false
                     ]
                   _ -> throwError $ UnhandledBinOp op
         | otherwise -> throwError $ UnhandledBinOp op
    UnExpr op x -> do
      (xtid, xunion) <- transExpr x
      case op of
        Negate -> do
          matchType intType xtid
          xexpr <- unEx xunion
          return (intType, Ex xexpr)
        Not -> do
          if | xtid == boolType -> do
                 xexpr <- unEx xunion
                 return (boolType, Ex $ TH.not xexpr)
             | xtid == condType -> do
                 xf <- unCx xunion
                 return (condType, Cx $ flip xf)
             | otherwise -> do
                 throwError $ Unnotable x 
    SeqExpr exprs -> do
      case exprs of
        [] -> return (voidType, Nx TH.dummyStmt)
        _ -> do
          prevs <- fmap toSeqStmt $ mapM (transExpr >=> unNx . snd) $ init exprs
          (rtid, runion) <- transExpr $ last exprs
          if | rtid == voidType -> do
                 rstmt <- unNx runion
                 return (rtid, Nx $ TH.seq prevs rstmt)
             | otherwise -> do
                 rexpr <- unEx runion
                 return (rtid, Ex $ TH.eseq prevs rexpr)
    ArrayExpr tid cnt _ -> do
      fp <- zoom trsFrame framePointer        
      size <- findElemType tid >>= sizeOf
      return (tid, Ex $ TH.callPrimitive "alloc" [fp, TH.constant (size * cnt)])
    RecordExpr tid bindings -> do
      fp <- zoom trsFrame framePointer
      sym <- zoom trsSymbolTable (findType tid) `awareOf ` UnknownType tid
      let fieldNum = case _tsBody sym of
            RecordBody fields _ -> length fields
            _ -> error $ "error in transExpr(RecordExpr): unexpected type body: " <> show (_tsBody sym)
      when (fieldNum /= length bindings) $ do
        throwError $ FieldInitializationNumError tid bindings
      temp <- zoom trsTempPool allocTemp
      
      let allocExpr = TH.move (TH.temp temp) $ TH.callPrimitive "allocObject" [fp, TH.name (_tsInfo sym)]
      initExprs <- forM bindings $ \(ValueBinding vid value) -> do
        (ftid, fexpr) <- fieldOf (TH.temp temp) tid vid
        (itid, iunion) <- transExpr value
        matchType ftid itid
        iexpr <- unEx iunion
        return $ TH.move fexpr iexpr
      return (tid, Ex $ TH.eseq (toSeqStmt $ allocExpr : initExprs) $ TH.temp temp)
    CallExpr fid args -> do
      funSym <- zoom trsSymbolTable (findFun fid) `awareOf` UnknownFun fid
      fp <- zoom trsFrame framePointer
      (atids, aexprs) <- fmap unzip $ forM args $ \arg -> do
        (atid, aunion) <- transExpr arg
        when (atid == voidType) $ do
          throwError $ FunArgMustBeExpression arg 
        aexpr <- unEx aunion
        return (atid, aexpr)
      rtid <- matchFunArgs fid atids
      return (rtid, Ex $ TH.call (_fsName funSym) $ fp : aexprs)
    AssignExpr var value -> do
      (ltid, lexpr) <- transVar var
      (rtid, runion) <- transExpr value
      matchType ltid rtid
      rexpr <- unEx runion
      return (voidType, Nx $ TH.move lexpr rexpr)
    IfExpr cond x y -> do
      (ctid, cunion) <- transExpr cond
      matchType condType ctid
      cf <- unCx cunion
      
      (xtid, xunion) <- transExpr x
      (ytid, yunion) <- transExpr y
      matchType xtid ytid

      labels <- zoom trsLabelPool $ replicateM 3 allocLabel
      let [ true, false, done ] = labels

      if | xtid == voidType -> do
             xstmt <- unNx xunion
             ystmt <- unNx yunion
             let body = [ cf true false
                         , TH.label true
                         , xstmt
                         , TH.jump done
                         , TH.label false
                         , ystmt
                         , TH.label done
                         ]
             return (xtid, Nx $ toSeqStmt body)
         | otherwise -> do
             xexpr <- unEx xunion
             yexpr <- unEx yunion
             temp <- fmap TH.temp $ zoom trsTempPool allocTemp
             let body = [ cf true false
                         , TH.label true
                         , TH.move temp xexpr
                         , TH.jump done
                         , TH.label false
                         , TH.move temp yexpr
                         , TH.label done
                         ]
             return (xtid, Ex $ TH.eseq (toSeqStmt body) temp)
    WhileExpr cond body -> do
      (ctid, cunion) <- transExpr cond
      matchType condType ctid
      cf <- unCx cunion
      surround $ \entry exit -> do
        bstmt <- transExpr body >>= unNx . snd
        bodyEntry <- zoom trsLabelPool allocLabel
        let stmts = toSeqStmt
              [ TH.label entry
              , cf bodyEntry exit
              , TH.label bodyEntry
              , bstmt
              , TH.jump entry
              , TH.label exit
              ]
        return (voidType, Nx stmts)
    ForExpr (vid, tid) low high body -> do
      assertTypeExistence tid
      newSymbolTable $ do
        -- access <- zoom trsFrame allocLocal
        depth <- use $ trsFrame . frDepth
        -- zoom trsSymbolTable $ registerVar vid $ SymbolTable.makeVarSym tid access depth

        index <- zoom trsTempPool allocTemp
        let access = InReg index
        zoom trsSymbolTable $ registerVar vid $ makeVarSym tid access depth
        
        (ltid, lunion) <- transExpr low
        (rtid, runion) <- transExpr high

        matchType tid ltid
        matchType tid rtid

        lexpr <- unEx lunion
        rexpr <- unEx runion
        
        -- fp <- zoom trsFrame framePointer
        -- let iexpr = toExpr access fp
        let iexpr = TH.temp index
        
        surround $ \entry exit -> do
          bstmt <- transExpr body >>= unNx . snd
          bodyEntry <- zoom trsLabelPool allocLabel
          let stmts = [ TH.move iexpr lexpr
                      , TH.label entry
                      , TH.less iexpr rexpr bodyEntry exit 
                      , TH.label bodyEntry
                      , bstmt
                      , TH.move iexpr $ TH.plus iexpr (TH.constant 1)
                      , TH.jump entry
                      , TH.label exit
                      ]
          return (voidType, Nx $ toSeqStmt stmts)

    BreakExpr -> do
      exit <- use trsExit
      return (voidType, Nx $ TH.jump exit)
      
    LetExpr decs body -> do
      newSymbolTable $ do
        mapM_ glanceDec decs
        stmts <- toSeqStmt <$> mapM transDec decs
        (btid, bunion) <- transExpr body
        when (btid == voidType) $ do
          throwError $ LetExprMustReturnValue expr 
        bexpr <- unEx bunion
        return (btid, Ex $ TH.eseq stmts bexpr)
        
surround :: (Label -> Label -> Trans a) -> Trans a
surround action = do
  labels <- zoom trsLabelPool $ replicateM 2 allocLabel
  let [ entry, exit ] = labels
  replaceWith trsEntry entry $ do
    replaceWith trsExit exit $ do
      action entry exit

matchFunArgs :: VarId -> [TypeId] -> Trans TypeId
matchFunArgs fid ats = do
  funSym <- zoom trsSymbolTable (findFun fid) `awareOf` UnknownFun fid
  when (length (_fsFormals funSym) /= length (ats)) $ do
    throwError $ FunArgsNumError fid (_fsFormals funSym) ats  
  sequence_ $ zipWith matchType (fmap snd $ _fsFormals funSym) ats
  return $ _fsResult funSym
    
transVar :: Var -> Trans (TypeId, Tree.Expr)
transVar var = do
  case var of
    SimpleVar vid -> do
      curDepth <- use $ trsFrame . frDepth
      VarSym{..} <- zoom trsSymbolTable (findVar vid) `awareOf` UnknownVar vid
      access <- return _vsAccess `awareOf` UncompletedVar vid
      fp <- zoom trsFrame $ staticLinking $ (curDepth - _vsDepth)
      return (_vsType, toExpr access fp)
    FieldVar owner vid -> do
      (otid, oexpr) <- transVar owner
      -- offset <- calcFieldOffset otid vid
      -- tid <- findFieldType otid vid
      -- ws <- use $ trsFrame . frWordSize
      -- return (tid, TH.mem $ TH.plus (TH.constant offset) (TH.mem $ TH.plus oexpr (TH.constant $ ws * 2)))
      -- return (tid, fieldOf oexpr otid vid)
      fieldOf oexpr otid vid
    SubscriptVar array index -> do
      (aid, aexpr) <- transVar array
      etid <- findElemType aid
      unitSize <- sizeOf etid
      (itid, iunion) <- transExpr index
      matchType intType itid
      iexpr <- unEx iunion
      return (etid, TH.mem $ TH.plus aexpr $ TH.times (TH.constant unitSize) iexpr)

sizeOf :: TypeId -> Trans Int
sizeOf tid = do
  sym <- zoom trsSymbolTable (findType tid) `awareOf` UnknownType tid
  case _tsBody sym of
    Alias another -> sizeOf another
    RecordBody fields _ -> do
      fmap sum $ mapM sizeOf $ snd $ unzip fields
    _ -> return 1

fieldOf :: Tree.Expr -> TypeId -> VarId -> Trans (TypeId, Tree.Expr)
fieldOf owner tid vid = do
  ws <- use $ trsFrame . frWordSize
  offset <- calcFieldOffset tid vid
  ty <- findFieldType tid vid
  return (ty, TH.mem $ TH.plus (TH.constant offset) (TH.mem $ TH.plus owner (TH.constant $ ws * 2)))

findFieldType :: TypeId -> VarId -> Trans TypeId
findFieldType tid attr = do
  sym <- zoom trsSymbolTable (findType tid) `awareOf` UnknownType tid
  case _tsBody sym of
    Alias another -> findFieldType another attr
    RecordBody fields _ -> do
      maybe (noSuchField tid attr) return $ lookup attr fields
    _ -> notRecordType tid

calcFieldOffset :: TypeId -> VarId -> Trans Int
calcFieldOffset tid attr = do
  sym <- zoom trsSymbolTable (findType tid) `awareOf` UnknownType tid
  case _tsBody sym of
    Alias another -> calcFieldOffset another attr
    RecordBody fields _ -> do
      case lookup attr fields of
        Nothing -> noSuchField tid attr
        _ -> do
          let ts = snd $ unzip $ takeWhile (\(vid, _) -> vid /= attr) fields
          ws <- use $ trsFrame . frWordSize 
          return $ length ts * ws
    _ -> notRecordType tid
  
findElemType :: TypeId -> Trans TypeId
findElemType tid = do
  sym <- zoom trsSymbolTable (findType tid) `awareOf` UnknownType tid
  case _tsBody sym of
    Alias another -> findElemType another
    ArrayBody eid -> return eid
    _ -> notArrayType tid

newSymbolTable :: Trans a -> Trans a
newSymbolTable action = do
  new <- nextSymbolTable <$> use trsSymbolTable
  replaceWith trsSymbolTable new action

withFrame :: (Int -> Frame)-> Trans a -> Trans a
withFrame frame action = do
  depth <- use $ trsFrame . frDepth
  replaceWith trsFrame (frame $ depth + 1) action

emitFun :: Tree.Stmt -> Trans ()
emitFun body = do
  frame <- use trsFrame
  zoom trsProcFragSet $ addFrag ProcFrag
    { _pfFrame = frame
    , _pfBody = body
    }

checkTypeBody :: TypeBody -> Trans ()
checkTypeBody body = case body of
  Alias tid -> assertTypeExistence tid
  RecordBody fields _ -> mapM_ assertTypeExistence $ fmap snd fields
  ArrayBody tid -> assertTypeExistence tid
  Hidden -> return ()

assertTypeExistence :: TypeId -> Trans ()
assertTypeExistence tid = do
  void $ zoom trsSymbolTable (findType tid) `awareOf` UnknownType tid

matchType :: TypeId -> TypeId -> Trans ()
matchType x y
  | y == anyType = error "`Any' is not allowed as the second argument of `matchType'"
  | x == anyType = return ()
  | x == condType = assure $ y == condType || y == boolType
  | otherwise = assure $ y == x

  where
    assure c = when (not c) $ throwError $ MismatchedType x y

awareOf :: Trans (Maybe a) -> TransError -> Trans a
awareOf action err = do
  action >>= \case
    Just x -> return x
    _ -> throwError err

replaceWith :: MonadState s m => Lens' s a -> a -> m b -> m b
replaceWith lens new action = do
  original <- use lens
  assign lens new
  x <- action
  assign lens original
  return x
