import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import ParserTests
import BindingsAnalysisTests
import UnificationTests

main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Tests for RFun Core"
    [ coreParserTests
    , coreBindingsAnalysisTests
    , coreUnificationTests
    ]
