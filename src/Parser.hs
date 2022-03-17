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

type Parser = Parsec String ()

lexeme :: Parser a -> Parser a
lexeme p = p >>= \a -> many space >> return a

reserved :: [ Name ]
reserved = [ "fun", "let", "in", "rlet", "case", "dup" ]

isReserved :: Name -> Bool
isReserved name = name `elem` reserved

underscore :: Parser Char
underscore = char '_'

starts_with :: Parser Char -> Parser Name
starts_with p = (:) <$> p <*> many (choice [ letter , digit , underscore ] )

fname :: Parser Name
fname =
  do f <- lexeme $ starts_with lower
     if     isReserved f
       then fail $ "unexpected keyword : " ++ f
       else return f

cname :: Parser ([Pattern] -> Pattern)
cname =
  do c <- lexeme $ starts_with upper
     return $ Constructor c

-- Todo:
-- [ ] Syntactic abbreviations for lists and pairs.
-- [ ] Documentation, once the syntax is locked.
-- [ ] Indentation sensitive syntax.
