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

type InternalError = String

-- In this analysis, we check that all variables are used exactly once in
-- each branch of the program.
data BindingsViolation meta
  = LinearityViolation     FName Name [meta]       -- f x   = .. x .. x ..
  | DefinedButNotUsed      FName Name meta         -- f x   = ..
  | UsedButNotDefined      FName Name meta         -- f ..  = .. x ..
  | Shadowing              FName Name (meta, meta) -- f x   = let x ..
  | ConflictingDefinitions FName Name [meta]       -- f x x = ...

type BindingsAnalysis meta
  = Analysis InternalError CurrentBindings [BindingsViolation meta] [(Name, meta)]

data CurrentBindings =
  CurrentBindings { currentFunctionName    :: FName
                  , currentlyUsedVariables :: VName
                  }

bindingsInProgram :: Program meta -> BindingsAnalysis meta ()
bindingsInProgram (Program fs) = mapM_ bindingsInDefinition fs

bindingsInDefinition :: Definition meta -> BindingsAnalysis meta ()
bindingsInDefinition (Function f p e _) =
  do _ <- checkBindings p
     local (\r -> r { currentFunctionName = f}) $
       bindingsInExpression e

namesInPattern :: Pattern meta -> BindingsAnalysis meta [(Name, meta)]
namesInPattern (Constructor _ ps _) = concat <$> mapM namesInPattern ps
namesInPattern (Duplicate   p    _) = namesInPattern p
namesInPattern (Variable    x    m) = return [(x, m)]

checkBindings :: Pattern meta -> BindingsAnalysis meta ()
checkBindings p =
  do f  <- currentFunctionName <$> ask
     ns <- namesInPattern p
     ms <- get
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
     put $ choose1 $ ns ++ ms
  where
    choose1 [      ] = [ ]
    choose1 (x : xs) = x : filter ((/=(fst x)) . fst) (choose1 xs)

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
  do ms <- get
     forM_ cases $
       \(p, e) ->
         do put ms
            _ <- checkBindings p
            bindingsInExpression e

bindingsInPattern :: Pattern meta -> BindingsAnalysis meta ()
bindingsInPattern (Constructor _ _ _) = undefined
bindingsInPattern (Duplicate   _ _  ) = undefined
bindingsInPattern (Variable x m)      =
  do f  <- currentFunctionName <$> ask
     ms <- get
     case filter ((==x) . fst) ms of
       [ ] -> tell [ UsedButNotDefined f x m ]
       [_] -> return ()
       ms' -> tell [ ConflictingDefinitions f x $ map snd ms' ]
     put $ removeFirst x ms
  where
    removeFirst x [              ] = [ ]
    removeFirst x ((x', m) : rest) = if x == x' then rest else (x', m) : removeFirst x rest
