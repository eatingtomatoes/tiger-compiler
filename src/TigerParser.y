{
module TigerParser
  ( parseTiger
  ) where

import Control.Monad.Except
    
import qualified Token
import qualified Data.ByteString.Lazy.Char8 as Char8
import Ast
import qualified TigerLexer
import qualified Data.DList as DList
import Data.Either.Combinators    
}

%name parser
%tokentype { Token.Token }
%monad { TigerLexer.TigerLexer } 
%lexer { TigerLexer.lexTiger } { Token.Eof }
%error { TigerLexer.reportError }

%token

for {  Token.For }
to {  Token.To }
break {  Token.Break }
let {  Token.Let }
in {  Token.In }
end {  Token.End}
fun {  Token.Function }
while { Token.While }
var {  Token.Var }
type {  Token.Type }
array {  Token.Array }
if {  Token.If }
then {  Token.Then }
else {  Token.Else }
do {  Token.Do }
of {  Token.Of }
primitive { Token.Primitive }
is { Token.Is }
global { Token.Global }
'...' { Token.Ellipsis}
',' {  Token.Comma }
':' {  Token.Colon }
':=' { Token.Bind }
';' {  Token.Semicolon }
'(' {  Token.LeftParenthesis }
')' {  Token.RightParenthesis }
'[' {  Token.LeftBracket }
']' {  Token.RightBracket }
'{' {  Token.LeftBrace }
'}' {  Token.RightBrace }
'.' {  Token.Dot }
'+' {  Token.Plus }
'-' {  Token.Minus }
'*' {  Token.Time }
'/' {  Token.Div }
'=' {  Token.Equal }
'<>' {  Token.NotEqual }
'<' {  Token.Less }
'<=' {  Token.LessOrEqual }
'>' {  Token.Greater }
'>=' {  Token.GreaterOrEqual }
'!' { Token.Exclamation }
'&' {  Token.And }
'|' {  Token.Or }
upperId { Token.UpperId $$ }
lowerId { Token.LowerId $$ }
int { Token.IntegerLit $$ }
str { Token.StringLit $$ }

%left '|'
%left '&'
%nonassoc '<' '<=' '<>' '=' '>' '>='
%left '+' '-'
%left '*' '/'
%left NOT

%%

Program :: { Program } 
  : DecList { Program (DList.toList $1) }
  ;

DecList :: { DList.DList Dec } 
  : {- empty -} { DList.empty }
  | Dec { DList.singleton $1 }
  | DecList Dec { DList.snoc $1 $2 }
  ;
  
Dec :: { Dec }
  : type TypeId '=' TypeBody { TypeDec $2 ($4 $2) }
  | type TypeId is primitive { TypeDec $2 Hidden }
  | var TypeBinding ':=' Expr { VarDec $2 $4 }
  | IsGlobal fun VarId '(' TypeBindingList ')' ':' TypeId FunBody { FunDec $3 (DList.toList $5) $8 $9 $1 }
  ;

IsGlobal :: { Bool }
  : {- empty -} { False }
  | global { True }
  ;
  
FunBody :: { Maybe Expr }
  : {- empty -} { Nothing }
  | ':=' Expr { Just $2 }
  ; 

TypeBody :: { TypeId -> TypeBody }
  : TypeId { const (Alias $1) }
  | '{' TypeBindingList '}' { \tag -> RecordBody (DList.toList $2) tag }
  | array of TypeId { const (ArrayBody $3) }
  ;

TypeBindingList :: { DList.DList TypeBinding }
  : {- empty -} { DList.empty }
  | TypeBinding { DList.singleton $1 }
  | TypeBindingList ',' TypeBinding { DList.snoc $1 $3 }
  ;

TypeBinding :: { TypeBinding }
  : VarId ':' TypeId { ($1, $3) }
  ;

Var :: { Var }
  : VarId { SimpleVar $1 }
  | Var '.' VarId { FieldVar $1 $3 }
  | Var '[' Expr ']' { SubscriptVar $1 $3}
  ;

ExprList :: { DList.DList Expr }
  : {- empty -} { DList.empty }
  | Expr { DList.singleton $1 }
  | ExprList ';' Expr { DList.snoc $1 $3 }
  ;

ParamList :: { DList.DList Expr }
  : {- empty -} { DList.empty }
  | Expr { DList.singleton $1 }
  | ParamList ',' Expr { DList.snoc $1 $3 }
  ;

Expr :: { Expr }
  {- variables, fields, elements of an array -}
  : Var { VarExpr $1 }
  {- literals -}
  | int { IntExpr $1 }
  | str { StrExpr $1 }
  {- binary operations -}
  | Expr '|' Expr { BinExpr Or $1 $3 }  
  | Expr '&' Expr { BinExpr And $1 $3 }
  | Expr '<' Expr { BinExpr Less $1 $3 }
  | Expr '<=' Expr { BinExpr LessOrEqual $1 $3 }  
  | Expr '>' Expr { BinExpr Greater $1 $3 }    
  | Expr '>=' Expr { BinExpr GreaterOrEqual $1 $3 } 
  | Expr '=' Expr { BinExpr Equal $1 $3 }
  | Expr '<>' Expr { BinExpr NotEqual $1 $3 }   
  | Expr '+' Expr { BinExpr Plus $1 $3 }
  | Expr '-' Expr { BinExpr Minus $1 $3 }
  | Expr '*' Expr { BinExpr Times $1 $3 }
  | Expr '/' Expr { BinExpr Divide $1 $3 }
  {- unary operations -}
  | '!' Expr %prec NOT { UnExpr Not $2 }
  | '-' Expr %prec NOT { UnExpr Negate $2 }
  | '(' ExprList ')'  { SeqExpr (DList.toList $2) }
  {- array creation -}
  | TypeId '[' int ']' of Expr { ArrayExpr $1 $3 $6 }
  {- record creation -}
  | TypeId '{' ValueBindingList '}' { RecordExpr $1 (DList.toList $3) }
  {- function call -}
  | VarId '(' ParamList ')' { CallExpr $1 (DList.toList $3) }
  {- assignment -}
  | Var ':=' Expr { AssignExpr $1 $3 }
  {- control structures-}
  | if Expr then Expr else Expr { IfExpr $2 $4 $6 }
  | while Expr do '(' ExprList ')' { WhileExpr $2 $ SeqExpr (DList.toList $5) }
  | for TypeBinding ':=' Expr to Expr do '(' ExprList ')' { ForExpr $2 $4 $6 $ SeqExpr (DList.toList $9) }
  | break { BreakExpr }
  | let DecList in ExprList end { LetExpr (DList.toList $2) $ SeqExpr (DList.toList $4) }
  ;

ValueBindingList :: { DList.DList ValueBinding }
  : {- empty -} { DList.empty }
  | ValueBinding { DList.singleton $1 }
  | ValueBindingList ',' ValueBinding { DList.snoc $1 $3 }
  ;

ValueBinding :: { ValueBinding }
  : VarId '=' Expr { ValueBinding $1 $3 }
  ;

TypeId :: { TypeId }
  : upperId { TypeId $1 }
  ;

VarId :: { VarId }
  : lowerId { VarId $1 }
  ; 

{
--data ParserError = ParserError String
--  deriving (Show)

parseTiger :: Char8.ByteString -> Either String Program
parseTiger input = TigerLexer.runTigerLexer input parser
}
