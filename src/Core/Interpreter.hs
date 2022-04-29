{-|

Module      : Core.Interpreter
Description : An interpreter for RFun Core.
Author      : Joachim Tilsted Kristensen
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : tilsted@di.ku.dk
Stability   : experimental
Portability : POSIX

The following RFun Core implementation, is based on the article "Towards a
functional reversible language" by Yokoyama, Bock and Glück.

-}

module Core.Interpreter where

import Core.Syntax
import Core.Analysis

import Control.Arrow
import Control.Monad.RWS
import Control.Monad.Except

-- * Terminology:

-- An environment is a pair of functions, that may be used to lookup bound
-- values `and` function definitions.
newtype Environment  meta =
  Environment { unEnvironment :: ( Substitution meta , MutualRecursion meta ) }

type Substitution    meta = Transformation Pattern meta
type MutualRecursion meta = (Name, meta) -> Runtime meta (Definition meta)
type Error           meta = (String, meta)
type Runtime         meta = RWST (Environment meta) () () (Except (Error meta))

-- * Utility:

-- Picks a function definition from the environment (q in the paper).
definitionOf :: (Name, meta) -> Runtime meta (Definition meta)
definitionOf f = snd . unEnvironment <$> ask >>= \m -> m f

-- The r-match rule is just a variation of mgu.
match :: (Value, meta) -> Pattern meta -> Runtime meta (Substitution meta)
match (v, m) p =
  case patternMatch p (fromValue m v) of
    NoMatch     -> throwError ("Argument is malformed", m)
    (MatchBy s) -> return s

-- Valuation by substitution.
valuate :: Pattern meta -> Runtime meta Value
valuate p =
  do unifier <- fst . unEnvironment <$> ask
     case unifier p of
       (Variable    x m   ) -> throwError ("Unbound variable " ++ x, m)
       (Constructor c ps _) -> Value c <$> mapM valuate ps

-- Shadows a substitution.
withBindings :: Substitution meta -> (Transformation Environment meta)
withBindings g = Environment . first (\f -> f . g) . unEnvironment

-- Evaluates an expression with respect to a program.
runProgram :: Show meta => Program meta -> Expression meta -> Runtime meta Value
runProgram p e = local (const $ Environment (id, mutual p)) (interpret e)
  where
    mutual :: Program meta -> (Name, meta) -> Runtime meta (Definition meta)
    mutual (Program (f@(Function n _ _ _) : _)) m | n == fst m = return f
    mutual (Program (_ : fs)) m = mutual (Program fs) m
    mutual (Program [ ]) m = throwError $ first ("Missing definition of " ++) m

-- Computes the result of applying a function.
call :: (Name, meta) -> Pattern meta -> Runtime meta Value
call f p =
  do (Function _ q e _) <- definitionOf f
     v                  <- valuate p
     s                  <- match (v, meta p) q
     local (withBindings s) $ interpret e

-- Computes the (uniquely defined) environment.
uncall :: Value -> Pattern meta -> (Name, meta) -> Runtime meta (Environment meta)
uncall v p f =
  do (Function _ q e _) <- definitionOf f
     g                  <- uninterpret v e
     v'                 <- local (const g) (valuate q)
     uninterpret v' (Pattern p)

interpret :: Expression meta -> Runtime meta Value
interpret (Pattern p)     = valuate p
interpret (Let p f x e m) =
  do y <- call (f, m) x
     s <- match (y, meta x) p
     local (withBindings s) (interpret e)
interpret (RLet y f p e m) =
  do v <- valuate y
     s <- fst . unEnvironment <$> uncall v p (f, m)
     local (withBindings s) (interpret e)
interpret (Case p ps m) =
  do v         <- valuate p
     (q, e, i) <- firstMatch m v (ps `zip` [0..])
     s         <- match (v, meta p) q
     w         <- local (withBindings s) (interpret e)
     (_, _, j) <- firstMatch m w (leaves ps `zip` [0..])
     if i == j
       then return w
       else throwError ("Syntactic ortogonality violation in leaves", m)

-- Returns the leaf notes of a case statement.
leaves :: [(Pattern meta, Body meta)] -> [(Pattern meta, Body meta)]
leaves = undefined
-- leaves [] = []
-- leaves (p : rest) = leaf p : leaves rest
--   where
--     leaf (Pattern q) = (q, snd p)
--     leaf (Let _ _ _ e _) = leaf e
--     leaf (RLet _ _ _ e _) = leaf e

-- Returns the first matching pattern, with its body and its index into the case statement.
firstMatch
  :: meta
  -> Value
  -> [((Pattern meta, Body meta), Int)]
  -> Runtime meta (Pattern meta, Body meta, Int)
firstMatch m v (((p, e), i) : rest) =
  case patternMatch (fromValue m v) p of
    NoMatch -> firstMatch m v rest
    _       -> return (p, e, i)
firstMatch m _ [ ] =
  throwError ("Pattern matching not exhaustive", m)

uninterpret :: Value -> Expression meta -> Runtime meta (Environment meta)
uninterpret = undefined
