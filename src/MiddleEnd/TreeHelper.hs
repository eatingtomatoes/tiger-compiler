module MiddleEnd.TreeHelper where

import qualified Data.ByteString.Lazy.Char8 as Char8

import MiddleEnd.Tree
import Translation.Label
import Translation.Temp
import Translation.TempHelper

dummyStmt :: Stmt
dummyStmt = ExprStmt $ ConstExpr 0  

type SimplifiedBinOpExpr = Expr -> Expr -> Expr

type SimplifiedUnOpExpr = Expr -> Expr

plus :: SimplifiedBinOpExpr
plus = BinOpExpr Plus

minus :: SimplifiedBinOpExpr
minus = BinOpExpr Minus

times :: SimplifiedBinOpExpr
times = BinOpExpr Times

divide :: SimplifiedBinOpExpr
divide = BinOpExpr Divide

negate :: SimplifiedUnOpExpr
negate = UnOpExpr Negate

not :: SimplifiedUnOpExpr
not = UnOpExpr Not

ret :: Expr -> Stmt 
ret value = MoveStmt (TempExpr rv) value

retNone :: Stmt -> Stmt
retNone stmt = SeqStmt stmt (MoveStmt (TempExpr rv) (ConstExpr 0))

seq :: Stmt -> Stmt -> Stmt
seq = SeqStmt

expr :: Expr -> Stmt
expr = ExprStmt

constant :: Int -> Expr
constant x = ConstExpr x

const :: Int -> Expr
const x = ConstExpr x

zero :: Expr
zero = constant 0

one :: Expr
one = constant 1

name :: Label -> Expr
name = NameExpr

eseq :: Stmt -> Expr -> Expr
eseq = ESeqExpr

equal :: Expr -> Expr -> Label -> Label -> Stmt
equal = CJumpStmt Equal

less ::  Expr -> Expr -> Label -> Label -> Stmt
less = CJumpStmt Less

isTrue :: Expr -> Label -> Label -> Stmt
isTrue = equal one 

label :: Label -> Stmt
label = LabelStmt

jump :: Label -> Stmt
jump dst = JumpStmt (NameExpr dst) [dst]

cjump :: RelOp -> Expr -> Expr -> Label -> Label -> Stmt
cjump = CJumpStmt

move :: Expr -> Expr -> Stmt
move = MoveStmt
 
mem :: Expr -> Expr
mem = MemExpr

temp :: Temp -> Expr
temp = TempExpr

call :: Label -> [Expr] -> Expr
call name' = CallExpr (NameExpr name') 

callPrimitive :: Char8.ByteString -> [Expr] -> Expr
callPrimitive name' = call (labelFrom name')

dummyExpr :: Expr
dummyExpr = name $ labelFrom $ Char8.pack "dummyExpr"
