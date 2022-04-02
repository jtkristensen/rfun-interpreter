{-|

Module      : Core.Analysis
Description : Contains several analysis for the Core.RFun language.
Copyright   : Joachim Tilsted Kristensen
              Michael Kirkedal
              Eric Jul
Licence     : TBA
Maintainer  : tilsted@di.ku.dk
Stability   : experimental
Portability : POSIX

-}

-- Todo:
-- [ ] Repeated variables in patterns.
-- [ ] Each variable bound before use.
-- [ ] Variables used linearily in each branch.
-- [ ] Check syntactic orthogonality in case-expressions.

-- Future:
-- [ ] Typeinference (constraint solver).
-- [ ] Pattern matching not exhaustive (after types).
-- [ ] CFA1 (defunctionalization for lambda).

module Core.Analysis where

import Core.Ast
import Control.Monad.RWS
import Control.Monad.Except

-- All analysis are just stateful computations, that either `write`
-- constraints or `raise` errors. Some analysis need `read` or `state`
-- data. So, I just (conviniently) use RWS for everything.
type Analysis error read write state
  = (RWST read write state) (Except error)

-- In this analysis, we check that all variables are used exactly once in
-- each branch of the program.

data LinearityViolation meta
  = SeveralUsages Name [meta]

type Linearity meta
  = Analysis () () [LinearityViolation meta] [Name]

programLinearity :: Program meta -> Linearity meta ()
programLinearity (Program fs) = mapM_ definitionalLinearity fs

definitionalLinearity :: Definition meta -> Linearity meta ()
definitionalLinearity (Function _ p e _) =
  expressionalLinearity (collectNames p) e

collectNames :: Pattern meta -> [Name]
collectNames = undefined

expressionalLinearity :: [Name] -> Expression meta -> Linearity meta ()
expressionalLinearity _ _ = undefined
