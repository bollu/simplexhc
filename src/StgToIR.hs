{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ParallelListComp #-}
module StgToIR where
import StgLanguage
import ColorUtils

import IR
import IRBuilder
import Control.Monad.Except
import Data.Traversable
import Data.Foldable
import Control.Monad.State.Strict
import qualified OrderedMap as M


-- | ID of a binding
type BindingIntID = Int
getBindsInProgram :: Program -> [Binding]
getBindsInProgram prog = prog >>= collectBindingsInBinding

buildMatchBBForBind :: M.OrderedMap VarName FunctionLabel -> (Binding, BindingIntID) -> State FunctionBuilder (Value, BBLabel)
buildMatchBBForBind fns (Binding{..}, bindingid) = do
  bbid <- createBB (Label ("switch." ++ (_getVarName _bindingName)))
  focusBB bbid
  setRetInst (RetInstReturn (ValueFnPointer (fns M.! _bindingName)))
  return ((ValueConstInt bindingid), bbid)


buildFnForBind :: (Binding, BindingIntID) -> State ModuleBuilder FunctionLabel
buildFnForBind (Binding{..}, bindingid) = let
    paramsty = []
    retty = IRTypeVoid
    fnname = ("bindingfn." ++ (_getVarName _bindingName))
  in
    runFunctionBuilder paramsty retty fnname $ do
      return ()

buildMatcherFn :: [Binding] -> M.OrderedMap VarName FunctionLabel -> State FunctionBuilder ()
buildMatcherFn binds fns = do
  entrybb <- getEntryBBLabel
  switchBBs <- for (zip binds  [1..]) (buildMatchBBForBind fns)
  param <- getParamValue 0
  errBB <- createBB (Label "switch.fail")
  focusBB entrybb
  setRetInst (RetInstSwitch param errBB  switchBBs)


programToModule :: Program -> Module
programToModule p = runModuleBuilder $ do
    let binds = getBindsInProgram p
    bindingfns <- for (zip binds [1..]) buildFnForBind
    let bindingfnmap = M.fromList [(_bindingName $ b, fn) | b <- binds | fn <- bindingfns]
    runFunctionBuilder [IRTypeInt 32] IRTypeVoid "main" (buildMatcherFn binds bindingfnmap)
    return ()
