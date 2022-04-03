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
-- [x] Repeated variables in patterns.
-- [x] Each variable bound before use.
-- [x] Variables used linearily in each branch.
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

-- The bindings analysis checks that patterns are regular, makes sure that
-- variables are only used once, and that no definitions are conflicting or
-- ambiguous.
type BindingsAnalysis meta
  = Analysis
      InternalError            -- Something impossible happened.
      FunctionName             -- We are currently analysing this function.
      [BindingsViolation meta] -- These violations were found thus far.
      (CurrentBindings meta)   -- The environment looks like this.

type InternalError
  = String

data BindingsViolation meta
  = IrregularPattern       FunctionName VariableName [meta] -- f x .. x = ?
  | DefinedButNotUsed      FunctionName VariableName  meta  -- f x      = ..
  | UsedButNotDefined      FunctionName VariableName  meta  -- f ..     = .. x ..
  | LinearityViolation     FunctionName VariableName [meta] -- f ?      = .. x .. x ..
  | ConflictingDefinitions FunctionName VariableName [meta] -- f x      = .. let x = ..

data CurrentBindings meta
  = CurrentBindings
    { bound :: [(Name, meta)]
    , used  :: [(Name, meta)]
    }

-- Does Bindings analysis.
bindingsAnalysis ::
  Program meta ->
  Except (Either InternalError [BindingsViolation meta]) ()
bindingsAnalysis p =
  case runExcept (runRWST (bindingsOfProgram p) "" (CurrentBindings [] [])) of
    (Left internalError) -> throwError (Left internalError)
    (Right (_, _, []))   -> return ()
    (Right (_, _, w ))   -> throwError (Right w)

-- Performs bindings analysis on a program.
bindingsOfProgram :: Program meta -> BindingsAnalysis meta ()
bindingsOfProgram (Program fs) = mapM_ bindingsOfDefinition fs

-- Performs bindings analysis on a function definition.
bindingsOfDefinition :: Definition meta -> BindingsAnalysis meta ()
bindingsOfDefinition (Function f p e _) =
  do _  <- checkBindings p
     put (CurrentBindings [] [])
     local (const f) $ bindingsOfExpression e
     environment <- get
     forM_ (bound environment) $
       \(x, m) -> do tell [ DefinedButNotUsed f x m ]
                     unbind1 x
     forM_ (used  environment) $ unuse1 . fst

-- Performs bindings analysis on an expression.
bindingsOfExpression :: Expression meta -> BindingsAnalysis meta ()
bindingsOfExpression (Pattern p)               = bindingsOfPattern p
bindingsOfExpression (Let output _ input  e _) =
  do _ <- bindingsOfPattern  input
     _ <- checkBindings     output
     bindingsOfExpression e
bindingsOfExpression (RLet input _ output e _) =
  do _ <- bindingsOfPattern output
     _ <- checkBindings      input
     bindingsOfExpression e
bindingsOfExpression (Case _ cases          _) =
  do environment <- get
     forM_ cases $
       \(p, e) ->
         do put environment
            _ <- checkBindings p
            bindingsOfExpression e

-- Performs bindings analysis on a pattern.
bindingsOfPattern :: Pattern meta -> BindingsAnalysis meta ()
bindingsOfPattern (Constructor _ ps _) = mapM_ bindingsInPattern ps
bindingsOfPattern (Duplicate   p _   ) = bindingsInPattern p
bindingsOfPattern (Variable x m)       =
  do f  <- ask
     -- Check that x is bound.
     ms <- bound <$> get
     case filter ((==x) . fst) ms of
       [ ] -> tell [ UsedButNotDefined f x m ]
       [_] -> return ()
       ms' -> tell [ ConflictingDefinitions f x $ map snd ms' ]
     -- Check that x is not already used.
     xs <- used <$> get
     case filter ((==x) . fst) xs of
       [ ] -> return ()
       xs' -> tell [ LinearityViolation f x $ m : map snd xs' ]
     unbind1 x

-- Returns the names that occur in a pattern.
namesInPattern :: Pattern meta -> BindingsAnalysis meta [(Name, meta)]
namesInPattern (Constructor _ ps _) = concat <$> mapM namesInPattern ps
namesInPattern (Duplicate   p    _) = namesInPattern p
namesInPattern (Variable    x    m) = return [(x, m)]

-- Checks the bindings of a pattern to be bound.
checkBindings :: Pattern meta -> BindingsAnalysis meta ()
checkBindings p =
  do f  <- ask
     ns <- namesInPattern p
     -- Check that the pattern itself is regular.
     forM_ ns $
       \(x, _) ->
         case filter ((==x) . fst) ns of
           [ ] -> throwError "Argh, we have quantum variables!"
           [_] -> return ()
           ns' -> tell [ IrregularPattern f x $ map snd ns']
     ms <- bound <$> get
     -- Check that out-bound variables don't redefine unused ones.
     forM_ ms $
       \(x, m) ->
         tell $ map (\(_, m') -> ConflictingDefinitions f x [m, m']) $ filter ((==x) . fst) ns
     us <- used <$> get
     -- Reset `used` on variable rebindings.
     forM_ us $
       \(x, _) ->
         mapM (const $ unuse1 x) $ filter ((==x) . fst) ns

-- When a variable is bound in a pattern, we put it into the environment
-- using this function.
bind :: (Name, meta) -> BindingsAnalysis meta ()
bind (x, m) =
  do environment <- get
     put $ environment { bound = (x, m) : bound environment}

-- Moves a bound variable into the used variables.
unbind1 :: Name -> BindingsAnalysis meta ()
unbind1 x =
  do environment <- get
     let (bound', meta') = remove1 x $ bound environment
     put $ environment
       { bound = bound'
       , used = meta' ++ used environment
       }

-- Unbinds all variables.
unbindAll :: Name -> BindingsAnalysis meta ()
unbindAll x =
  do xs <- bound <$> get
     forM_ xs (const $ unbind1 x)


-- Forgets that we used a variable.
unuse1 :: Name -> BindingsAnalysis meta ()
unuse1 x =
  do environment <- get
     let (used', _) = remove1 x $ used environment
     put $ environment { used = used' }

-- remove1 x [(y, my), (x, m0), (x, m1), (x, m2)] = ([(y, my), (x, m1), (x, m2)], [(x, m0)])
remove1 :: Name -> [(Name, meta)] -> ([(Name, meta)], [(Name, meta)])
remove1 _ [       ] = ([], [])
remove1 x (x' : xs) =
  if   x == fst x'
  then (xs, [x'])
  else
    let (     xs' , out) = remove1 x xs
    in  (x' : xs' , out)
