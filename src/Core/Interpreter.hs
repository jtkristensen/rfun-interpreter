{-|

Module      : Core.Interpreter
Description : An interpreter for RFun Core.
Author      : Joachim Tilsted Kristensen
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : tilsted@di.ku.dk
Stability   : experimental
Portability : POSIX

The following RFun Core implementation, is based on the article "Towards a
functional reversible language" by Yokoyama, Bock and Glück.

-}

module Core.Interpreter where

import Core.Syntax
import Core.Analysis

import Control.Arrow
import Control.Monad.RWS
import Control.Monad.Except

-- An environment is a pair of functions, that may be used to lookup bound
-- values `and` function definitions.
newtype Environment meta =
  Environment
  { unEnvironment ::
      ( (Name, meta) -> Runtime meta Value
      , (Name, meta) -> Runtime meta (Definition meta))
  }

type Error   meta = (String, meta)
type Runtime meta = RWST (Environment meta) () () (Except (Error meta))

-- Looks up the definition of a function in the program.
definition :: Program meta -> (Name, meta) -> Runtime meta (Definition meta)
definition (Program (f@(Function n _ _ _) : _)) m | n == fst m = return f
definition (Program (_ : fs)) m = definition (Program fs) m
definition (Program [ ]) m = throwError $ first ("Missing definition of " ++) m

-- Evaluates an expression with respect to a program.
runProgram :: Show meta => Program meta -> Expression meta -> Runtime meta Value
runProgram p e = local (const $ Environment (f, g)) (interpret e)
  where
    f = throwError . first ("Unknown variable " ++)
    g = definition p

-- bind :: (Eq x, Eq a, Monad (m a)) => (x, a) -> b -> ((x, a) -> m a b) -> ((x, a) -> m a b)
-- bind x v f = \y -> if x == y then return v else f y

call :: (Name, meta) -> Pattern meta -> Runtime meta Value
call f p =
  do (_, def) <- unEnvironment <$> ask
     v        <- valuate p
     (Function _ q e _) <- def f
     case patternMatch (fromValue (meta p) v) q of
       NoMatch     -> throwError ("Malformed argument", meta p)
       (MatchBy g) -> interpret (g e) -- ?

valuate :: Pattern meta -> Runtime meta Value
valuate (Variable    x    m) = ask >>= (\f -> f (x, m)) . fst . unEnvironment
valuate (Constructor c ps _) = Value c <$> mapM valuate ps

interpret :: Expression meta -> Runtime meta Value
-- interpret (Pattern p)   = valuate p
-- interpret (Let p f x e m) =
--   do v <- call (f, m) x
interpret = undefined
