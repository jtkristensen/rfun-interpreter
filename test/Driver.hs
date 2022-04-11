import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Core.ParserTests
import Core.BindingsAnalysisTests
import Core.UnificationTests

main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Tests for RFun Core"
    [ coreParserTests
    , coreBindingsAnalysisTests
    , coreUnificationTests
    ]
