
module Core.AnalysisTests where

import Test.Tasty
import Test.Tasty.HUnit

import Core.Ast
import Core.Parser
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Error
import Data.Either

-- Exports.
coreAnalysisTests =
  testGroup "Unit tests about analyzing the core language."
    [
    ]

