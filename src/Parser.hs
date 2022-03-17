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

program    ::= definition*
definiiton ::= `fun` fname pattern `=` expression
pattern    ::= name | cname pattern* | `dup` pattern | `(` pattern `)`
expression ::= pattern
             | `let`  pattern `=` fname pattern `in` expression
             | `rlet` pattern `=` fname pattern `in` expression
             | `case` pattern `of` (pattern `->` expression `;`)+
fname      ::= <legal function names>
vname      ::= <legal variable names>
cname      ::= <legal constructor names>

-}

module Parser where

import Ast
import Text.Parsec
import Control.Monad (void)

type Parser = Parsec String ()

-- * Implementation

program :: Parser Program
program =
  do ds <- many definition
     eof
     return ds

definition :: Parser Definition
definition =
  do keyword "fun"
     f <- fname
     p <- pattern_
     symbol "="
     e <- expression
     return $ Function f p e

expression :: Parser Expression
expression =
  lexeme $
    choice
      [ Pattern <$> pattern_
      , let_
      , rlet
      , case_
      ]

pattern_ :: Parser Pattern
pattern_ =
  lexeme $
    choice
      [ cname <*> many pattern_
      , Variable <$> vname
      , keyword "dup" >> Duplicate <$> pattern_
      , inParentheses pattern_
      ]

-- * Details

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

-- Todo:
-- [ ] Write tests.
-- [ ] Syntactic abbreviations for lists and pairs.
-- [ ] Documentation, once the syntax is locked.
-- [ ] Indentation sensitive syntax.

