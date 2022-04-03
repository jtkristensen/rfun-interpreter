
module Core.AnalysisTests where

import Test.Tasty
import Test.Tasty.HUnit

import Core.Ast
import Core.Analysis
import Core.Parser       ( Source, SourceFileReference, program )
import Text.Parsec       ( runParser                            )
import Text.Parsec.Error ( ParseError                           )

import Control.Monad
import Control.Monad.Except

-- Exports.
coreAnalysisTests =
  testGroup "Unit tests about analyzing the core language."
    [ positiveBindingsAnalysisTests
    , negativeBindingsAnalysisTests
    ]

run :: Source -> Either ParseError (Program SourceFileReference)
run = runParser program () "stdin"

expectProgram ::
  (Eq e, Show e, Eq a, Show a)
    => (Program SourceFileReference -> Except e a)
    -> (Either e a -> Either e a)
    -> Source
    -> Either e a
    -> Assertion
expectProgram analysis normalize src expected =
  case run src of
    (Left  err) -> assertBool ("<error>: " ++ show err) False
    (Right ast) -> normalize (runExcept $ analysis ast) @?= normalize expected

passesBindings :: Source -> TestTree
passesBindings src =
  testCase src $ expectProgram bindingsAnalysis id src (return ())

failsBindings :: Source -> TestTree
failsBindings src =
    testCase src $ expectProgram bindingsAnalysis norm src (Left (return []))
  where
    norm (Left (Right _)) = Left $ return []
    norm other            = other

positiveBindingsAnalysisTests =
  testGroup "Unit tests for programs that should pass bindings analysis" $
    map passesBindings
      [ "fun id  x = x"
      , "fun inc n = S n"
      , "fun inc n = let n = s n in n"
      ]

negativeBindingsAnalysisTests =
  testGroup "Unit tests for programs that should fail bindings analysis" $
    map failsBindings
      [ "fun inc n = LinearityViolation n n"
      , "fun f (G x y) = let y = f x in ConflictingDefinitionsOf y"
      , "fun f (Pair x x) = IrregularPattern"
      , "fun f x = DefinedButNotUsed"
      , "fun f UsedButNotDefined = x"
      ]
