
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
    => (Program SourceFileReference -> Except e a) -- The analysis.
    -> (Either e a -> Either e a)                  -- Normalization.
    -> Source                                      -- A source program.
    -> Either e a                                  -- The expected analysis result.
    -> Assertion
expectProgram analysis normalize src expected =
  case run src of
    (Left  err) -> assertBool ("<error>: " ++ show err) False
    (Right ast) -> normalize (runExcept $ analysis ast) @?= normalize expected

titleOf :: Source -> String
titleOf src =
    if   length src' > 60
    then take 60 src' ++ " .."
    else src'
  where
    src' = minimalSpacing src
    minimalSpacing (' ' : ' ' : s) = minimalSpacing $ ' ' : s
    minimalSpacing (  s : rc     ) = s : minimalSpacing rc
    minimalSpacing _               = ""

passesBindings :: Source -> TestTree
passesBindings src =
  testCase (titleOf src) $ expectProgram runBindingsAnalysis id src (return ())

failsBindings :: Source -> TestTree
failsBindings src =
    testCase (titleOf src) $ expectProgram runBindingsAnalysis norm src (Left (return []))
  where
    norm (Left (Right _)) = Left $ return []
    norm other            = other

positiveBindingsAnalysisTests =
  testGroup "Unit tests for programs that should pass bindings analysis" $
    map passesBindings
      [ "fun id  x = x"
      , "fun inc n = S n"
      , "fun inc n = let n = s n in n"
      , "fun add pair ="                                        ++
        "  case pair of"                                        ++
        "    (Pair    Z  n) -> Pair Z n;"                       ++
        "    (Pair (S m) n) -> let (Pair m n) = add (Pair m n)" ++
        "                      in  (Pair (S m) (S n));"
      , "fun inc n = let  m = s n in m"
      , "fun inc n = rlet n = s n in n"
      , "fun inc n = let  m = s n in m"
      , "fun inc n = rlet n = s n in n"
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
