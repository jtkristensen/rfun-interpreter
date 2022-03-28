{-|

Module      : CoreParser
Description : This is a parser for the RFun Core language.
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

Additionally, we support two built-in datatype constructors.
That are just syntactic sugar for regular ones.

-}

module CoreParser where

import Ast
import Text.Parsec
import Control.Monad (void)

-- Shorthand.
type Parser = Parsec String ()

-- For now, the meta data is the cursor start and end position from where
-- the entity was parsed (used for error messages).
type Info = (SourcePos, SourcePos)

-- * Implementation

parseFromFile :: FilePath -> IO (Either ParseError (Program Info))
parseFromFile fileName =
  do input <- readFile fileName
     return $ runParser program () fileName input

program :: Parser (Program Info)
program =
  do lexeme $ return ()
     ds <- many definition
     eof
     return $ Program ds

definition :: Parser (Definition Info)
definition =
  info $
  do keyword "fun"
     f <- fname
     p <- pattern_
     symbol "="
     Function f p <$> expression

expression :: Parser (Expression Info)
expression =
  lexeme $
    choice
      [ let_
      , rlet
      , case_
      , inParentheses expression
      , Pattern <$> pattern_
      ]

pattern_ :: Parser (Pattern Info)
pattern_ =
  lexeme $
    choice
      [ info $ keyword "dup" >> Duplicate <$> pattern_
      , info $ Variable <$> fname
      , info   builtInTuple
      , info $ constructor <*>
          chainl (fmap return pattern_) (return mappend) []
      , inParentheses pattern_
      ]

-- -- * Details

comment :: Parser ()
comment =
  do _ <- try  $ string "--"
     _ <- many $ noneOf "\n"
     return ()

lexeme :: Parser a -> Parser a
lexeme p =
  do a <- p
     _ <- many (choice [ comment, void space ])
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
symbol s = void $ lexeme $ try $ string s

keyword :: String -> Parser ()
keyword k =
  void $ lexeme $
  do _ <- try $ string k
     notFollowedBy charAllowedInName

underscore :: Parser Char
underscore =
  char '_'

charAllowedInName :: Parser Char
charAllowedInName =
  try $ choice [ letter , digit , underscore ]

name :: Parser Name
name = lexeme $ many charAllowedInName

inParentheses :: Parser a -> Parser a
inParentheses =
  between (symbol "(") (symbol ")")

startsWith :: Parser Char -> Parser Name
startsWith p =
  (:) <$> p <*> name

vname :: Parser Name
vname = fname

fname :: Parser Name
fname = try $
  do f <- startsWith lower
     if     isReserved f
       then fail $ "Unexpected keyword : " ++ f
       else return f

constructor :: Parser ([Pattern meta] -> meta -> Pattern meta)
constructor =
  do c <- startsWith upper
     return $ Constructor c

builtInTuple :: Parser (Info -> Pattern Info)
builtInTuple =
  do symbol "{"
     ps <- chainr1 (fmap return pattern_) (symbol "," >> return (++))
     symbol "}"
     return $ Constructor ("builtin_Tuple" ++ show (length ps)) ps

builtInList :: Parser (Info -> Pattern Info)
builtInList =
  Constructor "builtin_List" <$>
    between (symbol "[") (symbol "]")
      (choice
        [ chainr1 (fmap return pattern_) (symbol "," >> return (++))
        , return []
        ]
      )

info :: Parser (Info -> a) -> Parser a
info p =
  do i <- getPosition
     m <- p
     j <- getPosition
     return (m (i, j))

let_ :: Parser (Expression Info)
let_ =
  info $
  do keyword "let"
     out  <- pattern_
     symbol "="
     f    <- fname
     in_  <- pattern_
     keyword "in"
     Let out f in_ <$> expression

rlet :: Parser (Expression Info)
rlet =
  info $
  do keyword "rlet"
     in_  <- pattern_
     symbol "="
     f    <- fname
     out  <- pattern_
     keyword "in"
     RLet in_ f out <$> expression

case_ :: Parser (Expression Info)
case_ =
  info $
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
-- [ ] Syntactic abbreviations for lists and pairs?
-- [ ] Documentation, once the syntax is locked.
-- [ ] Indentation sensitive syntax?

