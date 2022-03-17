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
  do a <- p
     _ <- many space
     return a

reserved :: [ Name ]
reserved =
  [ "fun" , "let"
  , "in"  , "rlet"
  , "dup" , "case"
  ]

isReserved :: Name -> Bool
isReserved name =
  name `elem` reserved

symbol :: String -> Parser ()
symbol s = void $ lexeme $ string s

keyword :: String -> Parser ()
keyword k =
  if   k `elem` reserved
  then symbol k
  else fail $ "Internal error (unexpected keyword) : " ++ k

underscore :: Parser Char
underscore =
  char '_'

inParentheses :: Parser a -> Parser a
inParentheses =
  between (symbol "(") (symbol ")")

startsWith :: Parser Char -> Parser Name
startsWith p =
  lexeme $
  do n   <- p
     ame <- many (choice [ letter , digit , underscore ] )
     return (n:ame)

fname :: Parser Name
fname =
  do f <- startsWith lower
     if     isReserved f
       then fail $ "Unexpected keyword : " ++ f
       else return f

vname :: Parser Name
vname = fname

cname :: Parser ([Pattern] -> Pattern)
cname =
  do c <- startsWith upper
     return $ Constructor c

pattern_ :: Parser Pattern
pattern_ =
  lexeme $
    choice
      [ Variable <$> vname
      , cname <*> many pattern_
      , keyword "dup" >> Duplicate <$> pattern_
      , inParentheses pattern_
      ]

let_ :: Parser Expression
let_ =
  do keyword "let"
     out  <- pattern_
     symbol "="
     f    <- fname
     in_  <- pattern_
     keyword "in"
     body <- expression
     return $ Let out f in_ body

rlet :: Parser Expression
rlet =
  do keyword "rlet"
     in_  <- pattern_
     symbol "="
     f    <- fname
     out  <- pattern_
     keyword "in"
     body <- expression
     return $ Let in_ f out body

case_ :: Parser Expression
case_ =
  do keyword "case"
     p  <- pattern_
     keyword "of"
     cs <- cases
     return $ Case p cs
  where
    cases =
      many1 $
        do l <- pattern_
           symbol "->"
           e <- expression
           symbol ";"
           return (l, e)

expression :: Parser Expression
expression =
  lexeme $
    choice
      [ Pattern <$> pattern_
      , let_
      , rlet
      , case_
      ]

definition :: Parser Definition
definition =
  do keyword "fun"
     f <- fname
     p <- pattern_
     symbol "="
     e <- expression
     return $ Function f p e

program :: Parser Program
program =
  do ds <- many definition
     eof
     return ds

-- Todo:
-- [ ] Write tests.
-- [ ] Syntactic abbreviations for lists and pairs.
-- [ ] Documentation, once the syntax is locked.
-- [ ] Indentation sensitive syntax.

