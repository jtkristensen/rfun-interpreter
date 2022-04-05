
module Core.UnificationTests where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Core.Ast
import Core.Analysis.Bindings

import Control.Monad
import Control.Monad.Except

newtype AnyPattern
  = AP { unAP :: Pattern () }

instance Arbitrary AnyPattern where
  arbitrary =
    oneof $ map (fmap AP)
      [ do vname <- arbitrary
           return (Variable vname ())
      , do cname <- arbitrary
           ps    <- map unAP <$> listOf arbitrary
           return (Constructor cname ps ())
      ]

newtype AnyPairOfPatterns
  = APOP { unAPOP :: (Pattern (), Pattern ()) }

instance Arbitrary AnyPairOfPatterns where
  arbitrary = curry APOP <$> (unAP <$> arbitrary) <*> (unAP <$> arbitrary)

newtype APairOfStructurallyEquvialentPatterns
  = APOSEP { unAPOSEP :: (Pattern (), Pattern ()) }

instance Arbitrary APairOfStructurallyEquvialentPatterns where
  arbitrary = APOSEP . forceEquivalent . unAPOP <$> arbitrary
    where
      forceEquivalent p@(Variable _ _, Variable _ _          ) = p
      forceEquivalent (Variable x _ , Constructor c ps _     ) =
                      (Variable x (), Constructor c (map (\y -> x =/= y) ps) ())
      forceEquivalent (Constructor c ps _ , Variable x _     ) =
                      (Constructor c (map (\y -> x =/= y) ps) (), Variable x ())
      forceEquivalent (Constructor c ps _, Constructor _ qs _) =
          (Constructor c ps' (), Constructor c qs' ())
        where (ps', qs') = unzip $ zipWith (curry forceEquivalent) ps qs
      x =/= (Variable y m)       | x == y = Variable (y ++ "'") m
      x =/= (Constructor c ps m)          = Constructor c ((=/=) x <$> ps) m
      _ =/= p                             = p

newtype APariOfStructurallyDifferentPatterns
  = APOSDP { unAPSDP :: (Pattern (), Pattern ()) }





-- Exports.
coreUnificationTests =
  testGroup "Tests about unification."
    [
    ]

-- properties :: TestTree
-- properties =
--   testGroup "Properties"
--     [ scProps
--     , qcProps
--     ]

-- scProps = testGroup "(checked by SmallCheck)"
--   [ SC.testProperty "sort == sort . reverse" $
--       \list -> sort (list :: [Int]) == sort (reverse list)
--   , SC.testProperty "Fermat's little theorem" $
--       \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--   -- the following property does not hold
--   -- , SC.testProperty "Fermat's last theorem" $
--   --     \x y z n ->
--   --       (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
--   ]

-- qcProps = testGroup "(checked by QuickCheck)"
--   [ QC.testProperty "sort == sort . reverse" $
--       \list -> sort (list :: [Int]) == sort (reverse list)
--   , QC.testProperty "Fermat's little theorem" $
--       \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--   -- the following property does not hold
--   -- , QC.testProperty "Fermat's last theorem" $
--   --     \x y z n ->
--   --       (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
--   ]
