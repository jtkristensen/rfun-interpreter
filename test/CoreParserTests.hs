
module CoreParserTests where

import Test.Tasty
import Test.Tasty.HUnit

import Ast
import CoreParser
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Error
import Data.Either

err :: ParseError
err = newErrorMessage (Message "err") (newPos "" 0 0)

coreParserTests =
  testGroup "Unit tests about parsing the core language."
    [ individualComponents
    , exampleFiles
    ]

run :: Parser a -> String -> Either ParseError a
run p = runParser p () "stdin"

strip :: Functor f => f a -> f ()
strip = fmap $ const ()

positive :: (Functor f, Eq (f ()), Show (f ())) => Parser (f a) -> String -> (f ()) -> Assertion
positive p s a = (fmap strip) (run p s) @?= (return (strip a))

negative :: Parser a -> String -> String -> Assertion
negative p msg s = assertBool msg $ isLeft $ run p s

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
    ]

fileExample :: FilePath -> Program () -> TestTree
fileExample fp p =
  testCase ("Parsing the file " ++ fp) $
    do result <- parseFromFile fp
       assertEqual "" (return p) (fmap strip result)

exampleFiles :: TestTree
exampleFiles =
  testGroup "Parsing the files located in the `examples` folder."
    [ fileExample "./examples/single_definition.rfun.core" $
      Program
        [ Function "id" (Variable "x" ()) (Pattern $ Variable "x" ()) ()
        ]
    ]
