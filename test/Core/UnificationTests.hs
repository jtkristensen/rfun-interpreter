
module Core.UnificationTests where

import Test.Tasty
-- import Test.Tasty.SmallCheck as SC -- TODO.
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Core.Ast
import Core.Analysis.Bindings
import Core.Analysis.Unification
import Core.TestConfig

import Control.Monad
import Control.Monad.Except

newtype AnyPattern
  = AP { unAP :: Pattern () }
  deriving (Show)

instance Arbitrary AnyPattern where
  arbitrary = resize sizeOfGeneratedPatterns $ AP <$> sized linearPattern
    where
      linearPattern = fPattern (\n -> n - 1)
      fPattern f 0 =
        do vname <- arbitrary
           return (Variable vname ())
      fPattern f n =
        oneof
          [ do vname <- arbitrary
               return (Variable vname ())
          , do cname <- arbitrary
               ps    <- resize n $ listOf (fPattern f (f n))
               return (Constructor cname ps ())
          ]
  shrink ap =
    case unAP ap of
      (Variable    x    _) ->
        AP <$> (map (flip Variable ()) $ shrink x)
      (Constructor c ps _) ->
        AP <$>
        map (\c' -> Constructor c' ps ()) (shrink c) ++
        map (\ps' -> Constructor c ps' ()) (iter ps)
     where
       iter [ ] = []
       iter [p] = map (return . unAP) $ shrink (AP p)
       iter (p : s) =
         do p' <- shrink (AP p)
            s' <- iter s
            return $ (unAP p') : s'

newtype AnyPairOfPatterns
  = APOP { unAPOP :: (Pattern (), Pattern ()) }
  deriving (Show)

instance Arbitrary AnyPairOfPatterns where
  arbitrary   = curry APOP <$> (unAP <$> arbitrary) <*> (unAP <$> arbitrary)
  shrink (APOP (p, q)) =
    do p' <- shrink (AP p)
       q' <- shrink (AP q)
       return $ APOP (unAP p', unAP q')

newtype APairOfStructurallyEquvialentPatterns
  = APOSEP { unAPOSEP :: (Pattern (), Pattern ()) }
  deriving (Show)

instance Arbitrary APairOfStructurallyEquvialentPatterns where
  arbitrary = APOSEP . forceEquivalent . unAPOP <$> arbitrary
  shrink (APOSEP (p, q)) = map (APOSEP . unAPOP) $ shrink (APOP (p, q))

newtype APairOfStructurallyDifferentPatterns
  = APOSDP { unAPSDP :: (Pattern (), Pattern ()) }
  deriving (Show)

instance Arbitrary APairOfStructurallyDifferentPatterns where
  arbitrary = APOSDP <$> (arbitrary >>= forceDifferent . unAPOP)

type Unifies      = APairOfStructurallyEquvialentPatterns -> Bool
type DoesNotUnify = APairOfStructurallyDifferentPatterns  -> Bool

equivalentPatternsUnify :: Unifies
equivalentPatternsUnify (APOSEP (p, q)) =
  case patternMatch p q of
    NoMatch -> False
    _       -> True

substitutionIsIdempotent :: Unifies
substitutionIsIdempotent (APOSEP (p, q)) =
  case patternMatch p q of
    (NoMatch  ) -> False
    (MatchBy f) -> f (Pattern p) == f (f (Pattern p))
                && f (Pattern q) == f (f (Pattern q))

substitutionUnifies :: Unifies
substitutionUnifies (APOSEP (p, q)) =
  case patternMatch p q of
    (NoMatch  ) -> False
    (MatchBy f) -> f (Pattern p) == f (Pattern q)

differentPatternsDontUnify :: DoesNotUnify
differentPatternsDontUnify (APOSDP (p, q)) =
  case patternMatch p q of
    NoMatch -> True
    _       -> False

testsOnAPOSEP :: [(String, Unifies)]
testsOnAPOSEP =
  [ ("Equivalent patterns always unify", equivalentPatternsUnify)
  , ("Substitution is idempotent", substitutionIsIdempotent)
  , ("The pattern matchin substitution is a unifier", substitutionUnifies)
  ]

testsOnAPOSDP :: [(String, DoesNotUnify)]
testsOnAPOSDP =
  [ ("Structurally different patterns never unify", differentPatternsDontUnify)
  ]

qcProperties :: TestTree
qcProperties =
  testGroup "Tested by Quick Check" $
    map (uncurry QC.testProperty) testsOnAPOSEP ++
    map (uncurry QC.testProperty) testsOnAPOSDP

-- Exports.
coreUnificationTests =
  testGroup "Tests about unification."
    [ qcProperties
    ]


-- Produces a pair of unifiable patterns from a pair of (possibly)--
-- ununifiable ones.
forceEquivalent p@(Variable _ _, Variable _ _          ) = p
forceEquivalent (Variable x _ , Constructor c ps _     ) =
  (Variable x (), Constructor c ((x `isNotANameIn`) <$> ps) ())
forceEquivalent (Constructor c ps _ , Variable x _     ) =
  (Constructor c ((x `isNotANameIn`) <$> ps) (), Variable x ())
forceEquivalent (Constructor c ps _, Constructor _ qs _) =
    (Constructor c ps' (), Constructor c qs' ())
  where (ps', qs') = unzip $ zipWith (curry forceEquivalent) ps qs


isNotANameIn x (Variable y m)       | x == y = Variable (y ++ "'") m
isNotANameIn x (Constructor c ps m)          =
  Constructor c ((x `isNotANameIn`) <$> ps) m
isNotANameIn x p                             = p


-- Produces a pair of ununifiable patterns from a pair of (possibly)
-- unifiable ones.
forceDifferent (Variable x _, Variable y _)       =
  oneof $ map return $
    [ (Constructor y [Variable x ()] (), Variable x ())
    , (Variable x (), Constructor y [Variable x ()] ())
    ]
forceDifferent (Variable x _, Constructor y ps _) =
  do ps' <- somethingEquals x ps
     return (Variable x (), Constructor y ps' ())
forceDifferent (Constructor y ps _, Variable x _) =
  do ps' <- somethingEquals x ps
     return (Constructor y ps' (), Variable x ())
forceDifferent (Constructor x ps _, Constructor y qs _) =
  oneof
    [ return (Constructor x ps (), Constructor (x++"'") qs ())
    , do (ps', qs') <- makeSomethingDifferent ps qs
         return (Constructor x ps' (), Constructor y qs' ())
    ]
-- Makes sure that the variable "x" occurs at least once.
somethingEquals x []
  = return [ Variable x () ]
somethingEquals x ((Variable y _) : rest)
  = oneof
      [ return (Variable x () : rest)
      , do rest' <- somethingEquals x rest
           return (Variable y () : rest')
      ]
somethingEquals x (Constructor c ps _ : rest)
  = oneof
      [ do ps' <- somethingEquals x ps
           return (Constructor c ps' () : rest)
      , do rest' <- somethingEquals x rest
           return (Constructor c ps () : rest')
      ]
-- Generates pairs of lists of patterns where at least one pair
-- is different.
makeSomethingDifferent [] [] =
  do ps <- listOf1 (unAP <$> arbitrary)
     return (ps, [])
makeSomethingDifferent (p:s) [] =
  return (p:s, [])
makeSomethingDifferent [] (q:s) =
  return ([], q:s)
makeSomethingDifferent (p:s) (q:t) =
  oneof
    [ do (p', q') <- makeSomethingDifferent [p] [q]
         return (p' ++ s, q' ++ t)
    , do (s', t') <- makeSomethingDifferent s t
         return (p : s', q : t')
    ]
