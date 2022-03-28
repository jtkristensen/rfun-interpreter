
module CoreParserTests where

import Test.Tasty
import Test.Tasty.HUnit

import Ast
import CoreParser
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Error
import Data.Either

-- Exports.
coreParserTests =
  testGroup "Unit tests about parsing the core language."
    [ individualComponents
    , exampleFiles
    ]

-- Abbreviations for elaborate test cases.
fun :: String -> Pattern () -> Expression () -> Definition ()
fun s p e = Function s p e ()

var :: String -> Pattern ()
var = flip Variable ()

pat :: Pattern () -> Expression ()
pat = Pattern

con :: String -> [Pattern ()] -> Pattern ()
con c ps = Constructor c ps ()

-- Utility functions.
run :: Parser a -> String -> Either ParseError a
run p = runParser p () "stdin"

strip :: Functor f => f a -> f ()
strip = fmap $ const ()

positive :: (Functor f, Eq (f ()), Show (f ())) => Parser (f a) -> String -> (f ()) -> Assertion
positive p s a = (fmap strip) (run p s) @?= (return (strip a))

negative :: Parser a -> String -> String -> Assertion
negative p msg s = assertBool msg $ isLeft $ run p s

fileExample :: FilePath -> Program () -> TestTree
fileExample fp p =
  testCase ("Parsing the file " ++ fp) $
    do result <- parseFromFile fp
       assertEqual "" (return p) (fmap strip result)

-- Tests.
individualComponents :: TestTree
individualComponents =
  testGroup "Testing the individual components."
    [ testCase "Comments are not part of the program" $
        positive program
          " --hello \n --world" $
          Program []
    , testCase "The empty string is an empty program" $
        positive program
          "" $
          Program []
    , testCase "Keywords are lexemes" $
        positive (many (keyword "fun") <* eof)
          "fun -- hello \n fun\nfun fun --hello fun\n fun " $
          take 5 $ repeat ()
    , testCase "Juxtaposition for constructors (no left-recursion)" $
        positive pattern_
          "S (S n)"
          (con "S" [con "S" [var "n"]])
    ]


exampleFiles :: TestTree
exampleFiles =
  testGroup "Parsing the files located in the `examples` folder."
    [ fileExample "./examples/single_definition.rfun.core" $
      Program
        [ fun "id" (var "x") (pat $ var "x")
        ]
    -- , fileExample "./examples/nat.rfun.core" $
    --   Program
    --     [ fun "inc" (var "n") (pat $ con "S" [var "n"])
    --     ]
    ]
