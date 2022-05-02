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
definitionOf f = ask >>= (\m -> m f) . snd . unEnvironment

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
     check (unifier p)
  where
     check :: Pattern meta -> Runtime meta Value
     check (Variable    x m   ) = throwError ("Unbound variable " ++ x, m)
     check (Constructor c ps _) = Value c <$> mapM check ps

-- Shadows a substitution.
withBindings :: Substitution meta -> Transformation Environment meta
withBindings g = Environment . first (. g) . unEnvironment

-- If the first reverse match of a particular value `v`, is in the i'th
-- branch of a top-level case statement, we shall call `i` the unmatch index
-- of that case statement with respect to `v`.
unmatchIndex :: Int -> (Value, meta) -> [Expression meta] -> Maybe Int
unmatchIndex _ _      [                ] =
  Nothing
unmatchIndex i (v, m) (Pattern p : rest) =
  if   isMatch (patternMatch (fromValue m v) p)
  then return i
  else unmatchIndex i (v, m) rest
unmatchIndex i p (Let  _ _ _ e _ : rest) = unmatchIndex (i + 1) p (e : rest)
unmatchIndex i p (RLet _ _ _ e _ : rest) = unmatchIndex (i + 1) p (e : rest)
unmatchIndex i p (Case _ ps _    : rest) =
  case unmatchIndex i p (map snd ps) of
    Nothing -> unmatchIndex (i + 1) p rest
    _       -> return i

-- Returns an environment where the substitution has been replaced by another.
environment :: Substitution meta -> Runtime meta (Environment meta)
environment s =
  Environment . (\p -> (s, snd p)) . unEnvironment <$> ask

-- * Implementation

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

-- Evaluates an expression at runtime.
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
     case unmatchIndex 0 (w, m) $ map snd ps of
       (Just j) | i == j -> return w
       Nothing           -> throwError ("/!\\ - Internal Error : _|_", m)
       _                 -> throwError ("Syntactic ortogonality violation in leaves"  , m)

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

-- Recalls the (unique) substitution that must have been used to valuate an
-- expression with respect to a value.
uninterpret :: Value -> Expression meta -> Runtime meta (Environment meta)
-- uninterpret v (Pattern p    ) =
--   do s <- match (v, meta p) p
--      environment s
-- uninterpret v (Let p f x e m) =
--   do s <- fst . unEnvironment <$> uninterpret v e
--      y <- local (withBindings s) (valuate p)
--      t <- fst . unEnvironment <$> uncall y x (f, m)
--      environment (s . t)
-- uninterpret v (RLet y f p e m) =
--   do s <- fst . unEnvironment <$> uninterpret v e
--      w <- local (withBindings s) (call (f, m) p)
--      t <- match (w, m) y
--      environment (s . t)
-- uninterpret v e@(Case p ps m) =
--   do j <- case unmatchIndex 0 (v, m) e of
--             Nothing  -> ("Reverse pattern matching not exhaustive", m)
--             (Just k) -> return k
--      let (p_j, e_j) = ps !! j
--      g <- uninterpret v e_j
--      w <- local (const g) (interpret p_j)
uninterpret = undefined
