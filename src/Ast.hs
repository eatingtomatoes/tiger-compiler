module Ast
  ( Program (..)
  , Dec (..)
  , TypeBody (..)
  , TypeBinding
  , Var (..)
  , Expr (..)
  , ValueBinding (..)
  , TypeId (..)
  , VarId (..)
  , BinOp (..)
  , UnOp (..)
  , isAlias
  , isRecord
  , isArray
  , isPrimitive
  , isFunDec
  , isVarDec
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)

data Program
  = Program [Dec]
  deriving (Show, Eq)

data Dec
  = TypeDec TypeId TypeBody
  | VarDec TypeBinding Expr  
  | FunDec VarId [TypeBinding] TypeId (Maybe Expr) Bool{-is global-} 
  deriving (Show, Eq)

data TypeBody
  = Alias TypeId
  | RecordBody [TypeBinding] TypeId{- hidden tag-} 
  | ArrayBody TypeId
  | Hidden
  deriving (Show, Eq)


type TypeBinding = (VarId, TypeId)

data Var
  = SimpleVar VarId
  | FieldVar Var VarId
  | SubscriptVar Var Expr
  deriving (Show, Eq)

data Expr
  = VarExpr Var
  | IntExpr Int
  | StrExpr ByteString
  | BinExpr BinOp Expr Expr
  | UnExpr UnOp Expr
  | SeqExpr [Expr]  
  | ArrayExpr TypeId Int{-size-} Expr{-init-}  
  | RecordExpr TypeId [ValueBinding]
  | CallExpr VarId [Expr]  
  | AssignExpr Var Expr
  | IfExpr Expr Expr Expr
  | WhileExpr Expr Expr
  | ForExpr TypeBinding Expr{-low-} Expr{-high-} Expr
  | BreakExpr 
  | LetExpr [Dec] Expr
  deriving (Show, Eq)

data ValueBinding
  = ValueBinding VarId Expr
  deriving (Show, Eq)

data TypeId
  = TypeId ByteString
  -- | Nil
  deriving (Show, Eq, Ord)

data VarId
  = VarId ByteString
  deriving (Show, Eq, Ord)

data BinOp
  = Plus
  | Minus
  | Times
  | Divide
  | Equal
  | NotEqual
  | Less
  | LessOrEqual
  | Greater
  | GreaterOrEqual
  | Or
  | And
  deriving (Show, Eq)

data UnOp
  = Negate
  | Not
  deriving (Show, Eq)

isAlias :: TypeBody -> Bool
isAlias (Alias _) = True
isAlias _ = False

isRecord :: TypeBody -> Bool
isRecord (RecordBody _ _) = True
isRecord _ = False

isArray :: TypeBody -> Bool
isArray (ArrayBody _) = True
isArray _ = False

isPrimitive :: TypeBody -> Bool
isPrimitive Hidden = True
isPrimitive _ = False

isFunDec :: Dec -> Bool
isFunDec (FunDec _ _ _ _ _) = True
isFunDec _ = False
  
isVarDec :: Dec -> Bool
isVarDec (VarDec _ _) = True
isVarDec _ = False
