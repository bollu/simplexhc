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
import qualified Data.List as L

-- | Int value corresponding to binding
type BindingIntVal = Int
-- | Data associated to a binding
data BindingData = BindingData {
  binding :: Binding,
  bindingIntVal :: BindingIntVal,
  bindingFn :: Value
}

instance Pretty BindingData where
  pretty BindingData{..}=
    vcat [pretty "BindingData {",
      indent 4 (vcat $ [pretty "binding :=" <+> pretty binding,
                      pretty "id := " <+> pretty bindingIntVal,
                      pretty "bindingFn :=" <+> pretty bindingFn]), pretty "}"]

-- | G = global, P = pointer. General Context that we need throughout.
data Context = Context {
    -- | Stack pointer to raw values.
    rawstackGP:: Value,
    -- | Stack pointer to function values.
    boxstackGP:: Value,
    -- | Number of raw values on the stack.
    rawnG  :: Value,
    -- | Number of function values on the stack.
    boxstacknG :: Value,
    -- | Binding name to binding data
    bindingNameToData :: M.OrderedMap VarName BindingData,
    -- | Matcher function
    fnmatcher :: Value,
    -- | function to push boxed value to stack
    fnpushboxed :: Value,
    -- | function to pop boxed value on stack
    fnpopboxed :: Value,
    -- | function to push a raw int to stack
    fnpushint :: Value,
    -- | function to pop an int from the stack
    fnpopint :: Value
}

-- | ID of a binding
getBindsInProgram :: Program -> [Binding]
getBindsInProgram prog = prog >>= collectBindingsInBinding


-- | Build the function stubs that corresponds to the binding.
-- | We first build all the stubs to populate the Context. Then, we can build
-- | the indivisual bindings.
buildFnStubForBind :: Binding -> State ModuleBuilder Value
buildFnStubForBind Binding{..} = let
    paramsty = []
    retty = IRTypeVoid
    fnname = ("bind." ++ (_unVarName _bindingName))
  in
    createFunction paramsty retty fnname



 -- | The type of a boxed value
typeBoxed :: IRType
typeBoxed = IRTypePointer (IRTypeFunction [] IRTypeVoid)

-- | Create a function that pushes values on the stack
_createStackPushFn :: String ->  -- ^function name
                      IRType -> -- ^type of stack elements
                      Value -> -- ^count global
                      Value -> -- ^stack pointer global
                      State ModuleBuilder Value
_createStackPushFn fnname elemty nG stackGP = do
  lbl <- createFunction [elemty] IRTypeVoid fnname
  runFunctionBuilder lbl $ do
      -- load the rawn value
      n <- "n" =:= InstLoad nG
      -- Load the pointer
      stackP <- "stackp" =:= InstLoad stackGP
      -- compute store addr
      storeaddr <- "storeaddr" =:= InstGEP stackP [n]
      val <- getParamValue 0
      appendInst $ InstStore storeaddr val
      -- TODO: this should be (n + 1)
      ninc <- "ninc" =:= InstAdd n (ValueConstInt 1)
      appendInst $ InstStore nG ninc
      return ()
  return lbl


-- | Create a function that pops values off the stack
_createStackPopFn :: String -> -- ^Function name
                     IRType -> -- ^type of stack elements
                     Value -> -- ^count global
                     Value -> -- ^stack pointer global
                     State ModuleBuilder Value
_createStackPopFn fnname elemty nG stackGP = do
  lbl <- createFunction [] elemty  fnname
  runFunctionBuilder lbl $ do
      -- load the rawn value
      n <- "n" =:= InstLoad nG
      -- Load the pointer
      stackP <- "stackp" =:= InstLoad stackGP
      -- compute store addr
      loadaddr <- "loadaddr" =:= InstGEP stackP [n]
      loadval <-  "loadval" =:= InstLoad loadaddr

      n' <- "ndec" =:= InstAdd n (ValueConstInt (-1))
      appendInst $ InstStore nG n'
      setRetInst $  RetInstReturn loadval
      return ()
  return lbl


-- | Create the `Context` object that is contains data needed to build all of the
-- | LLVM Module for our program.
createContext :: [Binding] -> State ModuleBuilder Context
createContext bs = do
  rawstack <- createGlobalVariable "stackraw" (IRTypePointer (IRTypeInt 32))
  boxstack <- createGlobalVariable "stackfn" (IRTypePointer (IRTypeFunction [] IRTypeVoid))
  rawn <- createGlobalVariable "rawn" (IRTypeInt 32)
  boxn <- createGlobalVariable "fnn" (IRTypeInt 32)
  bfns <- for bs buildFnStubForBind

  let matcherretty = IRTypePointer  (IRTypeFunction [] IRTypeVoid)
  fnmatcher <- createFunction [] matcherretty "matcher"

  let bdatas = [BindingData {
                  bindingIntVal=bival,
                  bindingFn=fn,
                  binding=b} | bival <- [1..] | fn <- bfns | b <- bs]
  let bnames = map (_bindingName . binding) bdatas

  pushboxed <- _createStackPushFn "pushbox" typeBoxed boxn boxstack
  popboxed <- _createStackPopFn "popbox" typeBoxed boxn boxstack

  pushint <- _createStackPushFn "pushint" typeint32 boxn boxstack
  popint <- _createStackPopFn "popint" typeint32 boxn boxstack

  return $ Context {
    rawstackGP=rawstack,
    boxstackGP=boxstack,
    rawnG=rawn,
    boxstacknG=boxn,
    bindingNameToData=M.fromList (zip bnames bdatas),
    fnmatcher=fnmatcher,
    fnpushboxed=pushboxed,
    fnpopboxed=popboxed,
    fnpushint=pushint,
    fnpopint=popint
 }

-- | Push a function into the stack
pushBoxed :: Context -> Value -> State FunctionBuilder ()
pushBoxed ctx val = do
  let f = fnpushboxed ctx
  appendInst $ InstCall f [val]
  return ()

-- | Create the instruction to pop a function from the stack.
-- | Note that return value needs to be named with (=:=)
popBoxed :: Context -> Inst
popBoxed ctx =
  let f = fnpopboxed ctx in InstCall f []

-- | Push an int to the stack
pushInt :: Context -> Value -> State FunctionBuilder ()
pushInt ctx val = do
  let f = fnpushint ctx
  appendInst $ InstCall f [val]

-- | Create the instruction to pop a function from the stack.
-- | Note that return value needs to be named with (=:=)
popInt :: Context -> Inst
popInt ctx =
  let f = fnpopint ctx in InstCall f []

createMatcher :: Context -> State ModuleBuilder ()
createMatcher ctx = do
    runFunctionBuilder (fnmatcher ctx) (buildMatcherFn_ (bindingNameToData ctx))
    where
    -- | Build a BB of the matcher that mathes with the ID and returns the
    -- | actual function.
    -- | Return the IR::Value of the switch case needed, and the label of the BB
    -- | to jump to.
    buildMatchBBForBind_ :: M.OrderedMap VarName BindingData -> VarName -> State FunctionBuilder (Value, BBLabel)
    buildMatchBBForBind_ bdata bname = do
      bbid <- createBB  ("switch." ++ (_unVarName bname))
      focusBB bbid
      let bfn = bindingFn (bdata M.! bname) :: Value
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
      switchValAndBBs <- for bnames (buildMatchBBForBind_ bdata)
      param <- getParamValue 0
      errBB <- createBB "switch.fail"
      focusBB entrybb
      setRetInst (RetInstSwitch param errBB  switchValAndBBs)

-- | Create a call to the matcher to return the function with name VarName
createMatcherCallWithName :: Context -> VarName -> Inst
createMatcherCallWithName ctx bname = let
  bintval = bindingIntVal $ (bindingNameToData ctx) M.! bname
  in InstCall (fnmatcher ctx) [(ValueConstInt bintval)]


-- | push an STG atom to the correct stack
pushAtomToStack :: Context -> M.OrderedMap VarName Value -> Atom -> State FunctionBuilder ()
pushAtomToStack ctx _ (AtomInt (StgInt i)) =  pushInt ctx (ValueConstInt i)
pushAtomToStack ctx nametoval (AtomVarName v) = pushBoxed ctx (nametoval M.! v)


-- | Generate code for an expression node in the IR
codegenExprNode :: Context ->
                  [VarName] -> -- ^free variables
                  [VarName] -> -- ^bound variables
                  ExprNode -> -- ^expression node
                  State FunctionBuilder ()
-- | Function appplication codegen
codegenExprNode ctx free bound (ExprNodeFnApplication fnname atoms) = do
  -- if bound = A B C, stack will have
  -- C
  -- B
  -- A
  -- So we need to reverse the stack
  boundvals <-  for (reverse bound) (\b -> (_unVarName b) =:= (popBoxed ctx))
  let boundNameToVal = M.fromList $ zip bound boundvals :: M.OrderedMap VarName Value
  let toplevelNameToVal = fmap bindingFn (bindingNameToData ctx) :: M.OrderedMap VarName Value

  fn <- case fnname `L.elemIndex` bound of
          Just idx -> return $ boundvals L.!! idx
          Nothing -> -- actually we should check globals, but fuck it for now
                     "fn" =:= createMatcherCallWithName ctx fnname
  for atoms (pushAtomToStack ctx (boundNameToVal `M.union` toplevelNameToVal))
  appendInst $ InstCall fn []

  return ()

-- | Constructor codegen
codegenExprNode ctx free bound (ExprNodeConstructor (Constructor name atoms)) = do
    -- if bound = A B C, stack will have
    -- C
    -- B
    -- A
    -- So we need to reverse the stack
    boundvals <-  for (reverse bound) (\b -> (_unVarName b) =:= (popBoxed ctx))
    let boundNameToVal = M.fromList $ zip bound boundvals :: M.OrderedMap VarName Value
    let toplevelNameToVal = fmap bindingFn (bindingNameToData ctx) :: M.OrderedMap VarName Value
    error "working on this"
    return ()






codegenExprNode _ _ _ e = error . docToString $
  vcat [pretty " Unimplemented codegen for exprnode: ", indent 4 (pretty e)]

-- | Setup a binding with name VarName
setupBinding_ :: Context -> VarName -> State FunctionBuilder ()
setupBinding_ ctx name = do
  let b = binding $ (bindingNameToData ctx) M.! name :: Binding
  let Lambda{_lambdaFreeVarIdentifiers=free,
             _lambdaBoundVarIdentifiers=bound,
             _lambdaExprNode=e} = _bindingLambda b
  codegenExprNode ctx free bound e


programToModule :: Program -> Module
programToModule p = runModuleBuilder $ do
    let bs = getBindsInProgram p
    ctx <- createContext bs
    createMatcher ctx
    for_ (M.toList . bindingNameToData $ ctx)
          (\(bname, bdata) -> runFunctionBuilder
                                  (bindingFn bdata)
                                  (setupBinding_ ctx bname))
    return ()
