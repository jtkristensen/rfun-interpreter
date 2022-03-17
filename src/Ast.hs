{-|

Module      : Ast
Description : The abstract syntax for RCPL goes here.
Copyright   : Joachim Tilsted Kristensen
              Michael Kirkedal
              Eric Jul
Licence     : TBA
Maintainer  : tilsted@di.ku.dk
Stability   : experimental
Portability : POSIX

The current syntax of the reversible programming language suggested in
the article, `Towards a Reversible Functional Language`, presented at
the third international workshop on reversible computation (RC 2011, in
Gent, Beligium, July 2022).

-}

module Ast where

type Name       = String
type FName      = Name
type VName      = Name
type Program    = [Definition]
type Body       = Expression
type InPattern  = Pattern
type OutPattern = Pattern

newtype Definition
  = Function { f :: FName, p :: Pattern, e :: Body }
  deriving (Show, Eq)

data Pattern
  = Variable    VName
  | Constructor Name [Pattern]
  | Duplicate   Pattern
  deriving (Show, Eq)

data Expression
  = Pattern Pattern
  | Let  OutPattern FName InPattern  Body
  | RLet InPattern  FName OutPattern Body
  | Case Pattern [(Pattern, Body)]
  deriving (Show, Eq)

data Value
  = Value Name [Value] -- Algebraic data type.
  deriving (Show, Eq)

-- Todo:
-- [ ] Pretty printer?
-- [ ] What instances.
