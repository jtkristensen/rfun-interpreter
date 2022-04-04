{-|

Module      : Core.Analysis.Bindings
Description : Includes pattern regularity and expressional linearity.
Author      : Joachim Tilsted Kristensen
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : tilsted@di.ku.dk
Stability   : experimental
Portability : POSIX

-}

module Core.Analysis.Bindings where

import Core.Ast
import Control.Monad.RWS
import Control.Monad.Except

type InternalError
  = String

-- The bindings analysis checks that patterns are regular, makes sure that
-- variables are only used once, and that no definitions are conflicting or
-- ambiguous.
type BindingsAnalysis meta
  = RWST
      FunctionName             -- We are currently analysing this function.
      [BindingsViolation meta] -- These violations were found thus far.
      (CurrentBindings meta)   -- The environment looks like this.
      (Except InternalError)   -- Something impossible happened.

data BindingsViolation meta
  = IrregularPattern       FunctionName VariableName [meta] -- f x .. x = ?
  | DefinedButNotUsed      FunctionName VariableName  meta  -- f x      = ..
  | UsedButNotDefined      FunctionName VariableName  meta  -- f ..     = .. x ..
  | LinearityViolation     FunctionName VariableName [meta] -- f ?      = .. x .. x ..
  | ConflictingDefinitions FunctionName VariableName [meta] -- f x      = .. let x = ..
  deriving (Eq, Show)

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
  do put (CurrentBindings [] [])
     _  <- bindPattern p
     local (const f) $ bindingsOfExpression e
     environment <- get
     forM_ (bound environment) $
       \(x, m) -> do tell [ DefinedButNotUsed f x m ]
                     unbind1 x

-- Performs bindings analysis on an expression.
bindingsOfExpression :: Expression meta -> BindingsAnalysis meta ()
bindingsOfExpression (Pattern p)               = bindingsOfPattern p
bindingsOfExpression (Let output _ input  e _) =
  do _ <- bindingsOfPattern  input
     _ <- bindPattern     output
     bindingsOfExpression e
bindingsOfExpression (RLet input _ output e _) =
  do _ <- bindingsOfPattern output
     _ <- bindPattern      input
     bindingsOfExpression e
bindingsOfExpression (Case p0 cases          _) =
  do _           <- bindingsOfPattern p0
     environment <- get
     forM_ cases $
       \(p1, e) ->
         do put environment
            _ <- bindPattern p1
            bindingsOfExpression e

-- Performs bindings analysis on a pattern.
bindingsOfPattern :: Pattern meta -> BindingsAnalysis meta ()
bindingsOfPattern (Constructor _ ps _) = mapM_ bindingsOfPattern ps
bindingsOfPattern (Variable x m)       =
  do f  <- ask
     -- Check that x is not already used.
     xs <- used <$> get
     case filter ((==x) . fst) xs of
       [ ] ->
         -- Check that x is bound.
         do ms <- bound <$> get
            case filter ((==x) . fst) ms of
              [ ] -> tell [ UsedButNotDefined f x m ]
              [_] -> return ()
              ms' -> tell [ ConflictingDefinitions f x $ map snd ms' ]
       xs' -> tell [ LinearityViolation f x $ m : map snd xs' ]
     unbind1 x

-- Returns the names that occur in a pattern.
namesInPattern :: Pattern meta -> BindingsAnalysis meta [(Name, meta)]
namesInPattern (Constructor _ ps _) = concat <$> mapM namesInPattern ps
namesInPattern (Variable    x    m) = return [(x, m)]

-- Checks the bindings of a pattern to be bound, and performs the bindings.
bindPattern :: Pattern meta -> BindingsAnalysis meta ()
bindPattern p =
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
     forM_ ns bind

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
