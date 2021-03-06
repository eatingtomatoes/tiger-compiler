{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.AstHelper where

import FrontEnd.Ast

intType :: TypeId
intType = TypeId "Int"

strType :: TypeId
strType = TypeId "Str"

doubleType :: TypeId
doubleType = TypeId "Double"

boolType :: TypeId
boolType = TypeId "Bool"

voidType :: TypeId
voidType = TypeId "Void"

anyType :: TypeId
anyType = TypeId "Any"

condType :: TypeId
condType = TypeId "Cond"
