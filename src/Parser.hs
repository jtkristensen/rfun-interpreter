{-|

Module      : Parser
Description : This is a parser for the RCPL language.
Copyright   : Joachim Tilsted Kristensen
              Michael Kirkedal
              Eric Jul
Licence     : TBA
Maintainer  : tilsted@di.ku.dk
Stability   : experimental
Portability : POSIX

The current syntax is a bit clunky, but is mainly purposed with writing tests.

-}

module Parser where

import Ast
import Text.Parsec
import Control.Monad (void)

type Parser = Parsec String ()

lexeme :: Parser a -> Parser a
lexeme p =
  p >>= \a -> many space >> return a

reserved :: [ Name ]
reserved =
  [ "fun", "let", "in", "rlet", "case", "dup" ]

isReserved :: Name -> Bool
isReserved name =
  name `elem` reserved

underscore :: Parser Char
underscore =
  char '_'

startsWith :: Parser Char -> Parser Name
startsWith p =
  (:) <$> p <*> many (choice [ letter , digit , underscore ] )

fname :: Parser Name
fname =
  do f <- lexeme $ startsWith lower
     if     isReserved f
       then fail $ "unexpected keyword : " ++ f
       else return f

cname :: Parser ([Pattern] -> Pattern)
cname =
  do c <- lexeme $ startsWith upper
     return $ Constructor c

keyword :: String -> Parser ()
keyword = void . lexeme . string

-- Todo:
-- [ ] Syntactic abbreviations for lists and pairs.
-- [ ] Documentation, once the syntax is locked.
-- [ ] Indentation sensitive syntax.
