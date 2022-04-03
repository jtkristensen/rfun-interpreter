{-|

Module      : Core.Analysis
Description : Contains several analysis for the Core.RFun language.
Author      : Joachim Tilsted Kristensen
Licence     : GNU GENERAL PUBLIC LICENSE
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

type InternalError = String

type BindingsAnalysis meta
  = Analysis
      InternalError            -- Something impossible happened.
      FunctionName             -- We are currently analysing this function.
      [BindingsViolation meta] -- These violations were found thus far.
      (CurrentBindings meta)   -- The current environment looks like this.

-- In this analysis, we check that all variables are used exactly once in
-- each branch of the program.
data BindingsViolation meta
  = LinearityViolation     FunctionName VariableName [meta] -- f x   = .. x .. x ..
  | DefinedButNotUsed      FunctionName VariableName  meta  -- f x   = ..
  | UsedButNotDefined      FunctionName VariableName  meta  -- f ..  = .. x ..
  | ConflictingDefinitions FunctionName VariableName [meta] -- f x x = ...

data CurrentBindings meta =
  CurrentBindings
  { bound :: [(Name, meta)]
  , used  :: [(Name, meta)]
  }

currentBindings :: BindingsAnalysis meta [(Name, meta)]
currentBindings = bound <$> get

bind :: (Name, meta) -> BindingsAnalysis meta ()
bind (x, m) =
  do environment <- get
     put $ environment { bound = (x, m) : bound environment}

unbind1 :: Name -> BindingsAnalysis meta ()
unbind1 x =
  do environment <- get
     put $ environment { bound = remove1 x $ bound environment }
  where
     remove1 x [              ] = [ ]
     remove1 x ((x', m) : xs) = if x == x' then xs else (x', m) : remove1 x xs

unbindAll :: Name -> BindingsAnalysis meta ()
unbindAll x =
  do xs <- currentBindings
     forM_ xs (const $ unbind1 x)

forceUniqueNames :: BindingsAnalysis meta ()
forceUniqueNames =
  do xs <- currentBindings
     let xs' = choose1 xs
     _  <- mapM_ (\(x, _) -> unbindAll x) xs'
     mapM_ bind xs'
  where
     choose1 [               ] = [ ]
     choose1 (x@(n, _) : rest) = x : filter ((/=n) . fst) (choose1 rest)

bindingsInProgram :: Program meta -> BindingsAnalysis meta ()
bindingsInProgram (Program fs) = mapM_ bindingsInDefinition fs

bindingsInDefinition :: Definition meta -> BindingsAnalysis meta ()
bindingsInDefinition (Function f p e _) =
  do _ <- checkBindings p
     put (CurrentBindings [] [])
     local (const f) $ bindingsInExpression e

namesInPattern :: Pattern meta -> BindingsAnalysis meta [(Name, meta)]
namesInPattern (Constructor _ ps _) = concat <$> mapM namesInPattern ps
namesInPattern (Duplicate   p    _) = namesInPattern p
namesInPattern (Variable    x    m) = return [(x, m)]

checkBindings :: Pattern meta -> BindingsAnalysis meta ()
checkBindings p =
  do f  <- ask
     ns <- namesInPattern p
     ms <- currentBindings
     -- Check that new bindings don't shadow old ones.
     forM_ ms $
       \(x, m) ->
         case filter ((==x) . fst) ns of
           [ ] -> return ()
           ns' -> tell $ map (\(_, m') -> Shadowing f x (m, m')) ns'
     -- Check that the pattern i regular.
     forM_ ns $
       \(x, _) ->
         case filter ((==x) . fst) ns of
           [ ] -> throwError "Bottom!"
           [_] -> return ()
           ns' -> tell [ ConflictingDefinitions f x $ map snd ns']
     forceUniqueNames

bindingsInExpression :: Expression meta -> BindingsAnalysis meta ()
bindingsInExpression (Pattern p)               = bindingsInPattern p
bindingsInExpression (Let output f input  e _) =
  do _ <- bindingsInPattern input
     _ <- checkBindings output
     bindingsInExpression e
bindingsInExpression (RLet input f output e _) =
  do _ <- bindingsInPattern output
     _ <- checkBindings     input
     bindingsInExpression e
bindingsInExpression (Case p cases          _) =
  do environment <- get
     forM_ cases $
       \(p, e) ->
         do put environment
            _ <- checkBindings p
            bindingsInExpression e

bindingsInPattern :: Pattern meta -> BindingsAnalysis meta ()
bindingsInPattern (Constructor _ _ _) = undefined
bindingsInPattern (Duplicate   _ _  ) = undefined
bindingsInPattern (Variable x m)      =
  do f  <- ask
     -- Check that x is bound.
     ms <- currentBindings
     case filter ((==x) . fst) ms of
       [ ] -> tell [ UsedButNotDefined f x m ]
       [_] -> return ()
       ms' -> tell [ ConflictingDefinitions f x $ map snd ms' ]
     -- Check that x is not already used.
     xs <- currentBindings
     case filter ((==x) . fst) xs of
       [ ] -> return ()
       xs' -> tell [ LinearityViolation f x $ m : map snd xs' ]
     unbind1 x
