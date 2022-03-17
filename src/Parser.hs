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

-- Todo:
-- [ ] Syntactic abbreviations for lists and pairs.
