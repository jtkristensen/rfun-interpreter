{-|

Module      : Core.Interpreter
Description : An interpreter for RFun Core.
Author      : Joachim Tilsted Kristensen
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : tilsted@di.ku.dk
Stability   : experimental
Portability : POSIX

The following RFun Core implementation, is based on the article "Towards a
functional reversible language" by Glück et al.

-}

module Core.Interpreter where

import Core.Syntax
import Core.Analysis

import Control.Monad.RWS
import Control.Monad.Except

import Control.Arrow

-- An environment is a pair of functions, that may be used to lookup bound
-- values `and` function definitions.
newtype Environment meta =
  Environment
  { unEnvironment ::
      ( Name -> Runtime meta Value
      , Name -> Runtime meta (Definition meta))
  }

type Error   meta = (String, meta)
type Runtime meta = RWST (Environment meta) () () (Except (Error meta))

-- Looks up the definition of a function in the program.
definition :: Program meta -> (Name, meta) -> Runtime meta (Definition meta)
definition (Program (f@(Function n _ _ _) : _)) m | n == fst m = return f
definition (Program (_ : fs)) m = definition (Program fs) m
definition (Program [ ]) m = throwError $ first ("Missing definition of " ++) m

-- Calls the function main on the empty
runProgram :: Show meta => Program meta -> Runtime meta Value
runProgram p =
  do (_          , def) <- unEnvironment <$> ask
     (Function _ _ _ m) <- def "main"
     interpret (Let (x m) "main" (u m) (Pattern $ x m) m)
  where
    x m = Variable    "data"    m
    u m = Constructor "Unit" [] m

-- call :: (Name, meta) -> Pattern meta -> Runtime meta Value
-- call f p =
--   do (Function _

valuate :: Pattern meta -> Runtime meta Value
valuate (Variable    x    _) = ask >>= (\f -> f x) . fst . unEnvironment
valuate (Constructor c ps _) = Value c <$> mapM valuate ps

interpret :: Expression meta -> Runtime meta Value
interpret = undefined
