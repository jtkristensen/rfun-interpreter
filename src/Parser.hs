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
     Function f p <$> expression

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
isReserved = flip elem reserved

symbol :: String -> Parser ()
symbol s = void $ lexeme $ string s

keyword :: String -> Parser ()
keyword k =
  void $ lexeme $ try $
  do _ <- string k
     notFollowedBy name

underscore :: Parser Char
underscore =
  char '_'

name :: Parser Name
name =
  lexeme $ many (choice [ letter , digit , underscore ] )

inParentheses :: Parser a -> Parser a
inParentheses =
  between (symbol "(") (symbol ")")

startsWith :: Parser Char -> Parser Name
startsWith p =
  (:) <$> p <*> name

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
     Let out f in_ <$> expression

rlet :: Parser Expression
rlet =
  do keyword "rlet"
     in_  <- pattern_
     symbol "="
     f    <- fname
     out  <- pattern_
     keyword "in"
     Let in_ f out <$> expression

case_ :: Parser Expression
case_ =
     Case <$> (keyword "case" >> pattern_) <*> (keyword "of" >> cases)
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

