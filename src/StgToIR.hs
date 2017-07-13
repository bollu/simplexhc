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


-- | G = global, P = pointer. General Context that we need throughout.
data Context = Context {
    -- | Stack pointer to raw values.
    rawstackGP:: Value,
    -- | Stack pointer to function values.
    fnstackGP:: Value,
    -- | Number of raw values on the stack.
    rawnG  :: Value,
    -- | Number of function values on the stack.
    fkstacknG :: Value,
    -- | Binding name to BindingId
    bindingIdToBinding :: M.OrderedMap BindingIntID Binding,
    -- | Bindings
    bindings :: [Binding]
    
    
}

-- | ID of a binding
type BindingIntID = Int
getBindsInProgram :: Program -> [Binding]
getBindsInProgram prog = prog >>= collectBindingsInBinding

buildMatchBBForBind :: M.OrderedMap VarName FunctionLabel -> (Binding, BindingIntID) -> State FunctionBuilder (Value, BBLabel)
buildMatchBBForBind fns (Binding{..}, bid) = do
  bbid <- createBB  ("switch." ++ (_getVarName _bindingName))
  focusBB bbid
  setRetInst (RetInstReturn (ValueFnPointer (fns M.! _bindingName)))
  return ((ValueConstInt bid), bbid)


-- | Build the function that corresponds to the binding
buildFnForBind :: Context -> (BindingIntID, Binding) -> State ModuleBuilder FunctionLabel
buildFnForBind ctx (bid, Binding{..}) = let
    paramsty = []
    retty = IRTypeVoid
    fnname = ("bindingfn." ++ (_getVarName _bindingName))
  in
    do
    fnlabel <- createFunction paramsty retty fnname
    runFunctionBuilder fnlabel $ do
      return ()


createContext :: [Binding] -> State ModuleBuilder Context
createContext bs = do
  rawstack <- createGlobalVariable "stackraw" (IRTypePointer (IRTypeInt 32))
  boxstack <- createGlobalVariable "stackfn" (IRTypePointer (IRTypeFunction [] IRTypeVoid))
  rawn <- createGlobalVariable "rawn" (IRTypeInt 32)
  boxn <- createGlobalVariable "fnn" (IRTypeInt 32)
  return $ Context {
    rawstackGP=rawstack,
    fnstackGP=boxstack,
    rawnG=rawn,
    fkstacknG=boxn,
    bindings=bs,
    bindingIdToBinding=M.fromList $ [(i, b) | b <- bs | i <- [1..]]
  }


pushPrimIntToStack :: Context -> Value -> State FunctionBuilder ()
pushPrimIntToStack ctx val = do
  -- load the rawn value
  rawn <- "rawn.val" =:= InstLoad (rawnG ctx)
  -- Load the pointer
  rawstackP <- "raw.val" =:= InstLoad (rawstackGP ctx)
  -- compute store addr
  storeaddr <- "storeaddr" =:= InstGEP rawstackP [rawn]
  appendInst $ InstStore storeaddr val
  return ()

pushFnToStack :: Context -> Value -> State FunctionBuilder ()
pushFnToStack ctx val = do
  -- load the rawn value
  rawn <- "rawn.val" =:= InstLoad (rawnG ctx)
  -- Load the pointer
  rawstackP <- "raw.val" =:= InstLoad (rawstackGP ctx)
  -- compute store addr
  storeaddr <- "storeaddr" =:= InstGEP rawstackP [rawn]
  appendInst $ InstStore storeaddr val
  return ()


buildMatcherFn :: [Binding] -> M.OrderedMap VarName FunctionLabel -> State FunctionBuilder ()
buildMatcherFn binds fns = do
  entrybb <- getEntryBBLabel
  switchBBs <- for (zip binds  [1..]) (buildMatchBBForBind fns)
  param <- getParamValue 0
  errBB <- createBB "switch.fail"
  focusBB entrybb
  setRetInst (RetInstSwitch param errBB  switchBBs)


programToModule :: Program -> Module
programToModule p = runModuleBuilder $ do
    let bs = getBindsInProgram p
    ctx <- createContext bs
    bfns <- for  (M.toList . bindingIdToBinding $ ctx) (buildFnForBind ctx)
    let bindingNameToFn = M.fromList [(_bindingName $ b, fn) | b <- bs | fn <- bfns]
    let matcherRetTy = IRTypePointer  (IRTypeFunction [] IRTypeVoid)
    matcher <- createFunction [IRTypeInt 32] matcherRetTy "matcher"
    runFunctionBuilder matcher (buildMatcherFn bs bindingNameToFn)
    return ()
