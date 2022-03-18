{-|

Module      : Analysis
Description : Contains several analysis for the RCPL language.
Copyright   : Joachim Tilsted Kristensen
              Michael Kirkedal
              Eric Jul
Licence     : TBA
Maintainer  : tilsted@di.ku.dk
Stability   : experimental
Portability : POSIX

We want to perform several analysis during the development of this language.
For now, they all go here. However, things should be keept separate enough
that we can modularize if stuff gets complicated (advanced is good,
complicated is bad).

-}

module Analysis where

-- import Ast

-- Todo:
-- [ ] Repeated variables in patterns.
-- [ ] Each variable bound before use.
-- [ ] Variables used linearily in each branch.

-- Future:
-- [ ] Typeinference (constraint solver).
-- [ ] Pattern matching not exhaustive (after types).
-- [ ] CFA1 (defunctionalization for lambda).