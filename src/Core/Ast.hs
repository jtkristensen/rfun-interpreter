{-# LANGUAGE DeriveFunctor #-}
{-|

Module      : Core.Ast
Description : The abstract syntax for RCPL goes here.
Author      : Joachim Tilsted Kristensen
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : tilsted@di.ku.dk
Stability   : experimental
Portability : POSIX

The current syntax of the reversible programming language suggested in
the article, `Towards a Reversible Functional Language`, presented at
the third international workshop on reversible computation (RC 2011, in
Gent, Beligium, July 2022).

-}

module Core.Ast where

-- * Abbreviations.

type Name            = String
type FName           = Name
type VName           = Name
type FunctionName    = Name
type VariableName    = Name
type Body       meta = Expression meta
type InPattern  meta = Pattern    meta
type OutPattern meta = Pattern    meta

-- * Language definition.
--
--   Note that everything is parameterized with a `meta` variable, that will
--   be populated by various information about each construct
--   (source code position, inferred type, etc..).
--
--   This information will generally be used for throwing proper error messages,
--   and for conveniently storing the output from analysis that relate to
--   program points.

newtype Program meta
  = Program [Definition meta]
  deriving (Show, Eq, Functor)

data Definition                               meta
  = Function FName (Pattern meta) (Body meta) meta
  deriving (Show, Eq, Functor)

data Pattern                         meta
  = Variable    VName                meta
  | Constructor Name  [Pattern meta] meta
  deriving (Eq, Functor)

data Expression                                                meta
  = Pattern (Pattern meta)
  | Let  (OutPattern meta) FName (InPattern  meta) (Body meta) meta
  | RLet (InPattern  meta) FName (OutPattern meta) (Body meta) meta
  | Case (Pattern meta) [(Pattern meta, Body meta)]            meta
  deriving (Show, Eq, Functor)

instance Show (Pattern meta) where
  show = ("pattern : "++) . show'
    where
      show' (Variable    x    _) = x
      show' (Constructor c ps _) =
        ("("++) . (++")") $
        c ++ foldr (\x xs -> " " ++ show' x ++ xs) "" ps

-- * A type class for extracting meta data.

class Meta m where
  meta :: m a -> a

instance Meta Definition where
  meta (Function _ _ _ a) = a

instance Meta Pattern where
  meta (Variable      _ a) = a
  meta (Constructor _ _ a) = a

instance Meta Expression where
  meta (Pattern      p) = meta p
  meta (Let  _ _ _ _ a) = a
  meta (RLet _ _ _ _ a) = a
  meta (Case     _ _ a) = a

-- * Everything valuates to an algebraic datatype:

data Value
  = Value Name [Value]
  deriving (Show, Eq)

fromValue :: meta -> Value -> Pattern meta
fromValue m (Value c vs) = Constructor c (fromValue m <$> vs) m

-- Todo:
-- [x] Collect meta data, such as source position and type.
-- [x] What instances (Meta, Functor, ..)
-- [ ] Documentation for syntactic parts (once they have been decided)?
-- [ ] What about equality ?
-- [ ] Pretty printer ?

-- Extensions will happen via a different Ast.
-- [ ] Extend with datatype definitions.
-- [ ] Extend to express actors.
-- [ ] Extend with send/unsend.
-- [ ] Extend with recieve/unrecieve.
-- [ ] Extend to include lambdas.
-- [ ] Extend with type declarations.
