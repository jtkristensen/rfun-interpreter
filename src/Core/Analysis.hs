{-|

Module      : Core.Analysis
Description : Exposes the implemented analysis for the Core language.
Author      : Joachim Tilsted Kristensen
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : tilsted@di.ku.dk
Stability   : experimental
Portability : POSIX

-}

-- Todo:
-- [x] Repeated variables in patterns.
-- [x] Each variable bound before use.
-- [x] Variables used linearily in each branch.
-- [ ] Check syntactic orthogonality in case-expressions.

-- Future:
-- [ ] Typeinference (constraint solver).
-- [ ] Pattern matching not exhaustive (after types).
-- [ ] CFA1 (defunctionalization for lambda).

module Core.Analysis
  ( bindingsAnalysis
  , PatternMatch(..) , patternMatch
  )
where

import Core.Analysis.Bindings    ( bindingsAnalysis )
import Core.Analysis.Unification ( PatternMatch(..) , patternMatch )
