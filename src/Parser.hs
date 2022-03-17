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
import Text.Parsec.Char ( lower, upper )

type Indentation = Int
type Parser      = Parsec String Indentation

lexeme :: Parser a -> Parser a
lexeme p = p >>= \a -> many space >> return a

reserved :: [ Name ]
reserved = [ "let", "in", "rlet", "case", "dup" ]

isReserved :: Name -> Bool
isReserved name = name `elem` reserved

underscore :: Parser Char
underscore = char '_'

name :: Parser Name
name =
  do head <- lower
     tail <- many (letter <|> digit <|> underscore)
     return (head : tail)

ident :: Parser Name
ident = try $
  do f <- name
     if     isReserved f
       then fail $ "unexpected keyword : " ++ f
       else return f


-- Todo:
-- [ ] Syntactic abbreviations for lists and pairs.
