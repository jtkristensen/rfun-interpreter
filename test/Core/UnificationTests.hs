
module Core.UnificationTests where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Core.Ast
import Core.Analysis.Unification
import Core.Analysis.Bindings    ( namesInPattern )

import Core.TestConfig     ( sizeOfGeneratedPatterns )
import Control.Monad.State ( State    , runState , get, put )

-- *| Generators:

newtype AnyPattern
  = AP { unAP :: Pattern () }

instance Arbitrary AnyPattern where
  arbitrary = resize sizeOfGeneratedPatterns $ AP <$> sized linearlySized
    where
      linearlySized = sizedPattern (\n -> n - 1)
      sizedPattern f 0 =
        do vname <- variableName
           return (Variable vname ())
      sizedPattern f n =
        oneof
          [ do vname <- variableName
               return (Variable vname ())
          , do cname <- constructorName
               ps    <- resize n $ listOf (sizedPattern f (f n))
               return (Constructor cname ps ())
          ]
      constructorName = oneof $ return . return <$> ['A'..'Z']
      variableName    = oneof $ return . return <$> ['a'..'z']
  shrink (AP (Variable  _    _)) =
    do x <- return <$> ['a'..'z']
       return $ AP $ Variable x ()
  shrink (AP (Constructor c ps _)) =
    do ps' <- shrink (AP <$> ps)
       return $ AP $ Constructor c (unAP <$> ps') ()

newtype AnyPairOfPatterns
  = APOP { unAPOP :: (Pattern (), Pattern ()) }

instance Arbitrary AnyPairOfPatterns where
  arbitrary = curry APOP <$> (unAP <$> arbitrary) <*> (unAP <$> arbitrary)
  shrink  p =
    do p' <- shrink $ AP $ fst $ unAPOP p
       q' <- shrink $ AP $ snd $ unAPOP p
       return $ APOP (unAP p', unAP q')

newtype APairOfStructurallyEquvialentPatterns
  = APOSEP { unAPOSEP :: (Pattern (), Pattern ()) }
  deriving (Show)

instance Arbitrary APairOfStructurallyEquvialentPatterns where
  arbitrary = APOSEP . equivalentify <$> arbitrary
  shrink p  = APOSEP . equivalentify <$> shrink (APOP $ unAPOSEP p)

newtype APairOfStructurallyDifferentPatterns
  = APOSDP { unAPOSDP :: (Pattern (), Pattern ()) }
  deriving (Show)

instance Arbitrary APairOfStructurallyDifferentPatterns where
  arbitrary = APOSDP <$> (arbitrary >>= forceDifferent . unAPOP)
  shrink p  = APOSDP . unAPOP <$> shrink (APOP $ unAPOSDP p)


-- *| Properties:

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
  [ ("Equivalent patterns always unify"              , equivalentPatternsUnify)
  , ("Substitution is idempotent"                    , substitutionIsIdempotent)
  , ("The pattern matching substitution is a unifier", substitutionUnifies)
  ]

testsOnAPOSDP :: [(String, DoesNotUnify)]
testsOnAPOSDP =
  [ ("Structurally different patterns never unify"  , differentPatternsDontUnify)
  ]

qcProperties :: TestTree
qcProperties =
  testGroup "Tested by Quick Check" $
    map (uncurry QC.testProperty) testsOnAPOSEP ++
    map (uncurry QC.testProperty) testsOnAPOSDP

-- *| Exports:

coreUnificationTests =
  testGroup "Tests about unification."
    [ qcProperties
    ]

-- *| Nasty details:

-- Produces a regular pattern from a (possibly) irregular one.
forceRegular :: Pattern a -> Pattern a
forceRegular = fst . regularify []
  where
    regularify xs (Variable    x    a) = fresh xs x a
    regularify xs (Constructor c ps a) =
        (Constructor c ps' a, xs')
      where
        (ps', xs') = regularify' xs ps
    regularify' xs [     ] = ([], xs)
    regularify' xs (p : s) = (p' : s', xs')
      where
        (p', xs'') = regularify  xs   p
        (s', xs' ) = regularify' xs'' s
    fresh xs x a | x `elem` xs = fresh xs (x ++ "'") a
    fresh xs x a               = (Variable x a, x : xs)

-- Produces a pair of unifiable patterns from a pair of (possibly)
-- ununifiable ones.
-- TODO: find a weaker way than making forcing regularity ?
equivalentify :: AnyPairOfPatterns -> (Pattern (), Pattern ())
equivalentify =
    regularify . fst . flip runState [] . forceEquivalent . unAPOP
  where
    regularify (p, q) =
      let (Constructor _ [p', q'] _) = forceRegular (Constructor "pair" [p, q] ())
      in  (p', q')
    forceEquivalent
      :: (Pattern (), Pattern ())
      -> State [(Name, Pattern ())] (Pattern (), Pattern ())
    forceEquivalent (p, q) =
      case (p, q) of
        (Variable x _, _) ->
           do xqs <- get
              q'  <- case filter ((==x) . fst) xqs of
                        ((_, q'') : _) -> return q''
                        [            ] ->
                          do let q'' = x `isFreeIn` q
                             put $ (x, q'') : xqs
                             return q''
              return (p, q')
        (Constructor c ps a, Constructor _ qs _) ->
          do (ps', qs') <- iter ps qs
             return (Constructor c ps' a, Constructor c qs' a)
          where
            iter (p:s) (q:t) =
              do (p',  q' ) <- forceEquivalent (p, q)
                 (ps', qs') <- iter s t
                 return (p' : ps', q' : qs')
            iter _     _     = return ([], [])
        _ ->
          do (q', p') <- forceEquivalent (q, p)
             return (p', q')

-- Adds "plings" to make names differ from `x`.
isFreeIn :: Name -> Pattern a -> Pattern a
isFreeIn x (Variable y m)       | x == y = Variable (y ++ "'") m
isFreeIn x (Constructor c ps m)          = Constructor c ((x`isFreeIn`) <$> ps) m
isFreeIn x p                             = p

-- Produces a pair of ununifiable patterns from a pair of (possibly)
-- unifiable ones.
forceDifferent :: (Pattern (), Pattern ()) -> Gen (Pattern (), Pattern())
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
somethingEquals :: Name -> [Pattern ()] -> Gen [Pattern ()]
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
makeSomethingDifferent
  :: [Pattern ()]
  -> [Pattern ()]
  -> Gen ([Pattern ()], [Pattern ()])
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
