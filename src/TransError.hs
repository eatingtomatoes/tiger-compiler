{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module TransError where

import Control.Monad.Except

import Ast

data TransError
  = SizeUndecidable TypeId
  | NoVarDecAtTopLevelAllowed
  | SeqExprNotEndedWithExpr [Expr] 
  | NonExprAsArguments Expr
  | Unnotable Expr
  | LetExprMustReturnValue Expr
  | SeqExprMustReturnValue Expr
  | FunArgMustBeExpression Expr
  | MismatchedType TypeId{-expected-} TypeId{-actual-}
  | UnknownFun VarId
  | UnknownDec Dec
  | UnknownVar VarId
  | UnknownType TypeId
  | InvalidConversion String{-msg-}
  | UnhandledBinOp BinOp
  | UncompletedVar VarId
  | EmptyBody
  | NotArray VarId
  | NotArrayType TypeId
  | NotRecord VarId
  | NotRecordType TypeId
  | NoSuchField TypeId VarId
  | OffsetUndecidable TypeId VarId
  deriving (Show)

noSuchField :: MonadError TransError m => TypeId -> VarId -> m a
noSuchField tid vid = throwError $ NoSuchField tid vid

notRecordType :: MonadError TransError m => TypeId -> m a
notRecordType = throwError . NotRecordType 

notArrayType :: MonadError TransError m => TypeId -> m a
notArrayType = throwError . NotArrayType
