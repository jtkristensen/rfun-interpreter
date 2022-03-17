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

-- * Abbreviations.
type Name       = String
type FName      = Name
type VName      = Name
type Body       = Expression
type InPattern  = Pattern
type OutPattern = Pattern

-- * Language definition.
type Program
  = [Definition]

data Definition
  = Function FName Pattern Body
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
  = Value Name [Value]
  deriving (Show, Eq)

-- Todo:
-- [ ] What instances ?
-- [ ] Documentation for syntactic parts (once they have been decided)?
-- [ ] What about equality ?
-- [ ] Pretty printer ?
