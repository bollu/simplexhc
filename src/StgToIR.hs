{-# LANGUAGE RecordWildCards #-}
module StgToIR where
import StgLanguage
import ColorUtils

import IR
import IRBuilder
import Control.Monad.Except
import Data.Traversable
import Data.Foldable
import Control.Monad.State.Strict


-- | ID of a binding
type BindingIntID = Int
getBindsInProgram :: Program -> [Binding]
getBindsInProgram prog = prog >>= collectBindingsInBinding

buildMatchBBForBind :: (Binding, BindingIntID) ->  State FunctionBuilder (Value, BBLabel)
buildMatchBBForBind (Binding{..}, bbintid)  = do
  bbid <- createBB (Label ("switch." ++ (_getVarName _bindingName)))
  return (ValueConstInt bbintid, bbid)

buildMatcherFn :: [Binding] -> State FunctionBuilder ()
buildMatcherFn binds = do
  entrybb <- getEntryBBLabel
  switchBBs <- for (zip binds  [1..]) buildMatchBBForBind
  param <- getParamValue 0
  errBB <- createBB (Label "switch.fail")
  setRetInst (RetInstSwitch param errBB  switchBBs)


programToModule :: Program -> Module
programToModule p = runModuleBuilder $ do
    let binds = getBindsInProgram p
    runFunctionBuilder [IRTypeInt 32] IRTypeVoid "main" (buildMatcherFn binds)
    return ()
