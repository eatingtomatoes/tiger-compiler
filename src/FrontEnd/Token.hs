module FrontEnd.Token
  ( Token(..)
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)

data Token
  = While
  | For
  | To
  | Break
  | Let
  | In
  | End
  | Function
  | Var
  | Type
  | Array
  | If
  | Then
  | Else
  | Do
  | Of
  | Primitive
  | Is
  | Global
  | Ellipsis
  | Comma
  | Colon
  | Semicolon
  | LeftParenthesis
  | RightParenthesis
  | LeftBracket
  | RightBracket
  | LeftBrace
  | RightBrace
  | Dot
  | Plus
  | Minus
  | Time
  | Div
  | Equal
  | NotEqual
  | Less
  | LessOrEqual
  | Greater
  | GreaterOrEqual
  | Exclamation
  | And
  | Or
  | Bind
  | UpperId ByteString
  | LowerId ByteString
  | StringLit ByteString  
  | IntegerLit Int
  | Eof
  deriving (Show, Eq)
