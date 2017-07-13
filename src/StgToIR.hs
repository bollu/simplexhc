{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ParallelListComp #-}
module StgToIR where
import StgLanguage
import ColorUtils


import Debug.Trace
import IR
import IRBuilder
import Control.Monad.Except
import Data.Traversable
import Data.Foldable
import Control.Monad.State.Strict
import Data.Text.Prettyprint.Doc as PP
import qualified OrderedMap as M

-- | Int value corresponding to binding
type BindingIntVal = Int
-- | Data associated to a binding
data BindingData = BindingData {
  binding :: Binding,
  bindingIntVal :: BindingIntVal,
  bindingFn :: FunctionLabel
}

instance Pretty BindingData where
  pretty BindingData{..}=
    vcat $ [pretty binding,
            pretty "id := " <+> pretty bindingIntVal,
            pretty bindingFn]

-- | G = global, P = pointer. General Context that we need throughout.
data Context = Context {
    -- | Stack pointer to raw values.
    rawstackGP:: Value,
    -- | Stack pointer to function values.
    fnstackGP:: Value,
    -- | Number of raw values on the stack.
    rawnG  :: Value,
    -- | Number of function values on the stack.
    fnstacknG :: Value,
    -- | Binding name to binding data
    bindingNameToData :: M.OrderedMap VarName BindingData
}

-- | ID of a binding
getBindsInProgram :: Program -> [Binding]
getBindsInProgram prog = prog >>= collectBindingsInBinding


-- | Build the function stubs that corresponds to the binding.
-- | We first build all the stubs to populate the Context. Then, we can build
-- | the indivisual bindings.
buildFnStubForBind :: Binding -> State ModuleBuilder FunctionLabel
buildFnStubForBind Binding{..} = let
    paramsty = []
    retty = IRTypeVoid
    fnname = ("bind." ++ (_unVarName _bindingName))
  in
    createFunction paramsty retty fnname


-- | Create the `Context` object that is contains data needed to build all of the
-- | LLVM Module for our program.
createContext :: [Binding] -> State ModuleBuilder Context
createContext bs = do
  rawstack <- createGlobalVariable "stackraw" (IRTypePointer (IRTypeInt 32))
  boxstack <- createGlobalVariable "stackfn" (IRTypePointer (IRTypeFunction [] IRTypeVoid))
  rawn <- createGlobalVariable "rawn" (IRTypeInt 32)
  boxn <- createGlobalVariable "fnn" (IRTypeInt 32)
  bfns <- for bs buildFnStubForBind 

  let bdatas = [BindingData {
                  bindingIntVal=bival,
                  bindingFn=fn,
                  binding=b} | bival <- [1..] | fn <- bfns | b <- bs]
  let bnames = map (_bindingName . binding) bdatas

  return $ Context {
    rawstackGP=rawstack,
    fnstackGP=boxstack,
    rawnG=rawn,
    fnstacknG=boxn,
    bindingNameToData=M.fromList (zip bnames bdatas)
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





createMatcher :: Context -> State ModuleBuilder FunctionLabel
createMatcher ctx = do
    -- (() -> Void)^
    let retty = IRTypePointer  (IRTypeFunction [] IRTypeVoid)
    matcher <- createFunction [IRTypeInt 32] retty "matcher"
    runFunctionBuilder matcher (buildMatcherFn_ (bindingNameToData ctx))
    return matcher
    where
    -- | Build a BB of the matcher that mathes with the ID and returns the 
    -- | actual function.
    -- | Return the IR::Value of the switch case needed, and the label of the BB
    -- | to jump to.
    buildMatchBBForBind_ :: M.OrderedMap VarName BindingData -> VarName -> State FunctionBuilder (Value, BBLabel)
    buildMatchBBForBind_ bdata bname = do
      bbid <- createBB  ("switch." ++ (_unVarName bname))
      focusBB bbid
      let bfn = bindingFn (bdata M.! bname) :: FunctionLabel
      let bintval = bindingIntVal (bdata M.! bname) :: BindingIntVal

      -- setRetInst (RetInstReturn (ValueFnPointer bfn))
      return ((ValueConstInt 3), bbid)
      -- return ((ValueConstInt bintval), bbid)
    -- | Build the matcher function, that takes a function ID and returns the
    -- | function corresponding to the ID.
    buildMatcherFn_ :: M.OrderedMap VarName BindingData ->
                       State FunctionBuilder ()
    buildMatcherFn_ bdata = do
      entrybb <- getEntryBBLabel
      let bnames = M.keys bdata
      trace ("bnames: " ++ show bnames) (return ())
      switchValAndBBs <- for bnames (buildMatchBBForBind_ bdata)
      param <- getParamValue 0
      errBB <- createBB "switch.fail"
      focusBB entrybb
      setRetInst (RetInstSwitch param errBB  switchValAndBBs)


programToModule :: Program -> Module
programToModule p = runModuleBuilder $ do
    let bs = getBindsInProgram p
    ctx <- createContext bs
    matcherfn <- createMatcher ctx
    return ()
