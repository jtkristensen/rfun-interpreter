{-|

Module      : Core.Analysis.Unification
Description : Implements various unification algorithms used for Most General Match.
Author      : Joachim Tilsted Kristensen
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : tilsted@di.ku.dk
Stability   : experimental
Portability : POSIX

-}

module Core.Analysis.Unification where

import Core.Ast
import Core.Analysis.Bindings ( namesInPattern )
import Control.Monad.Except

type Transformation f a = (f a -> f a)

-- Iff two patterns match, then a transformation exists, that make unify
-- (make equal) expressions that consists of those patterns.
data PatternMatch meta
  = NoMatch
  | MatchBy (Transformation Expression meta)

-- Decides if two patterns match, and provides the above mentioned
-- transformation as evidence.
patternMatch :: Pattern meta -> Pattern meta -> PatternMatch meta
patternMatch p q =
  case runExcept $ unifier $ modify patternToExpression $ mgu p q of
    (Left ()) -> NoMatch
    (Right f) -> MatchBy f

-- A unifier is a computation that either fails, or provides the
-- transformation.
type Unifier a = Except () (a -> a)

doesNotUnify :: Unifier failiure
doesNotUnify = throwError ()

-- A is a special unifying transformation.
newtype Substitution f a
  = Substitution { unifier :: Unifier (f a) }

instance Semigroup (Substitution f a) where
  s1 <> s2 =
    Substitution $
    -- Reapplying the inner substitution ensures idempotens.
    (.) <$> unifier s2 <*> ((.) <$> unifier s1 <*> unifier s2)

instance Monoid (Substitution f a) where
  mempty  = Substitution $ return id
  mappend = (<>)

-- Computes the most general unifier (mgu) for patterns.
mgu :: Pattern meta -> Pattern meta -> Substitution Pattern meta
mgu (Variable x _)       (Variable y _) | x == y               = mempty
mgu (Variable x _)       p              | not (p `contains` x) = p `substitutes` x
mgu  p                   (Variable x _) | not (p `contains` x) = p `substitutes` x
mgu (Constructor c ps _) (Constructor t qs _)
  | c == t && length ps == length qs = foldl (<>) mempty $ zipWith mgu ps qs
mgu _ _
  = Substitution doesNotUnify

-- Holds if the variable argument appears in the pattern.
contains :: Pattern meta -> VName -> Bool
contains p x = x `elem` namesInPattern p

-- The substitution where `x` is replaced by `q`.
substitutes :: Pattern meta -> VName -> Substitution Pattern meta
substitutes p x = Substitution $ return subst
  where
    subst (Variable y _) | x == y = p
    subst (Constructor c ps m)    = Constructor c (subst <$> ps) m
    subst q                       = q

-- Modifies the transformation resulting from substitution.
modify
  :: (Transformation f a -> Transformation g b)
  -> Substitution f a
  -> Substitution g b
modify t s =
  Substitution $ t <$> unifier s

-- Lifts a transformation on `Pattern` to work on `Expression`
patternToExpression
  :: Transformation Pattern    meta
  -> Transformation Expression meta
patternToExpression f e =
  case e of
    (Pattern p           ) -> Pattern $ f p
    (Let out fn in_ e'  m) -> Let  out fn (f in_) (out `shadows` e') m
    (RLet in_ fn out e' m) -> RLet in_ fn (f out) (in_ `shadows` e') m
    (Case p cases       m) ->
      Case (f p) (map (\(p', e') -> (p', p' `shadows` e')) cases) m
  where
    shadows p
      = patternToExpression (foldl captureFree f $ namesInPattern p)
    captureFree _ x v@(Variable y _) | x == y
      = v
    captureFree t x (Constructor c ps m)
      = Constructor c (t `captureFree` x <$> ps) m
    captureFree t _ p
      = t p
