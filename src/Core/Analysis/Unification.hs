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
import Control.Monad.Except


type    DoesNotUnify   = ()
type    Unification  a = Except DoesNotUnify a
newtype Substitution a = Substitution { unify :: Unification (Pattern a -> Pattern a) }

-- Exports
mostGeneralUnifier :: Pattern meta -> Pattern meta -> Substitution meta
mostGeneralUnifier = mgu

-- Utililty for computing the most general unifier.
subst :: VName -> Pattern meta -> Substitution meta
subst x p = Substitution $
  do f <- unify $ subst x p
     return $ \q ->
       case p of
         (Variable y _) | x == y -> p
         (Constructor c ps m)    -> Constructor c (f <$> ps) m
         _                       -> q

doesNotUnify :: Substitution meta
doesNotUnify = Substitution $ throwError ()

instance Semigroup (Substitution meta) where
  s1 <> s2 = Substitution $ (.) <$> unify s1 <*> unify s2

instance Monoid (Substitution meta) where
  mempty  = Substitution $ return id
  mappend = (<>)

mgu :: Pattern meta -> Pattern meta -> Substitution meta
mgu (Variable x _)       (Variable y _)       | x == y               = mempty
mgu (Variable x _)       p                    | not (p `contains` x) = subst x p
mgu  p                   (Variable x _)       | not (p `contains` x) = subst x p
mgu (Constructor c ps _) (Constructor t qs _) | c == t && length ps == length qs
  = foldl (<>) mempty $ zipWith mgu ps qs
mgu _ _             = doesNotUnify

contains :: Pattern meta -> VName -> Bool
contains (Variable       y _) x = x == y
contains (Constructor _ ps _) x = any (`contains` x) ps
