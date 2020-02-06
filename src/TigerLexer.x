{
module TigerLexer
  ( TigerLexer
  , runTigerLexer  
  , lexTiger
  , reportError
  ) where

import Prelude hiding (lex)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Text.Printf

import Token
}

%wrapper "monad-bytestring"

$digit = 0-9
$alpha = [a-zA-Z]
$lower = [a-z]
$upper = [A-Z]
$underscore = _

tokens :-

$white+ ;
while { toAlexAction $ const Let }
for { toAlexAction $ const For }
to { toAlexAction $ const To }
break { toAlexAction $ const Break }
let { toAlexAction $ const Let }
in { toAlexAction $ const In }
end { toAlexAction $ const End}
fn { toAlexAction $ const Function }
var { toAlexAction $ const Var }
type { toAlexAction $ const Type }
array { toAlexAction $ const Array }
if { toAlexAction $ const If }
then { toAlexAction $ const Then }
else { toAlexAction $ const Else }
do { toAlexAction $ const Do }
of { toAlexAction $ const Of }
primitive { toAlexAction $ const Primitive }
is { toAlexAction $ const Is }
global { toAlexAction $ const Global }
"..." { toAlexAction $ const Ellipsis }
"," { toAlexAction $ const Comma }
":" { toAlexAction $ const Colon }
":=" { toAlexAction $ const Bind }
";" { toAlexAction $ const Semicolon }
"(" { toAlexAction $ const LeftParenthesis }
")" { toAlexAction $ const RightParenthesis }
"[" { toAlexAction $ const LeftBracket }
"]" { toAlexAction $ const RightBracket }
"{" { toAlexAction $ const LeftBrace }
"}" { toAlexAction $ const RightBrace }
"." { toAlexAction $ const Dot }
"+" { toAlexAction $ const Plus }
"-" { toAlexAction $ const Minus }
"*" { toAlexAction $ const Time }
"/" { toAlexAction $ const Div }
"=" { toAlexAction $ const Equal }
"<>" { toAlexAction $ const NotEqual }
"<" { toAlexAction $ const Less }
"<=" { toAlexAction $ const LessOrEqual }
">" { toAlexAction $ const Greater }
">=" { toAlexAction $ const GreaterOrEqual }
"!" { toAlexAction $ const Exclamation }
"&" { toAlexAction $ const And }
"|" { toAlexAction $ const Or }
$upper ($underscore | $alpha | $digit)* { toAlexAction UpperId }
$lower ($underscore | $alpha | $digit)* { toAlexAction LowerId }
$digit+ { toAlexAction $ IntegerLit . maybe 0 fst . Char8.readInt }
\" ~\"* \" { toAlexAction $ StringLit . Char8.init . Char8.tail }

{

-- type AlexInput = (AlexPosn, Char, Char8.ByteString, _)
-- type AlexAction result = AlexInput -> Int -> Alex result

type TigerLexer = Alex
runTigerLexer = runAlex

toAlexAction :: (Char8.ByteString -> Token) -> AlexAction Token
toAlexAction cont (pos, _, str, _) len  = return $ cont $ Char8.take len str

lexTiger :: (Token -> Alex a) -> Alex a
lexTiger cont = alexMonadScan >>= cont

alexEOF :: Alex Token
alexEOF = return Eof

reportError :: Token -> Alex a
reportError token = do
  ((AlexPn _ line column), _, _, _) <- alexGetInput
  let fmt = "unexpected `%v` at line %d, column %d"
  alexError $ printf fmt (show token) line column 
}
