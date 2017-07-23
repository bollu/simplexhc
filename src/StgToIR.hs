{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ParallelListComp #-}
module StgToIR where
import StgLanguage hiding (constructorName)
import qualified StgLanguage as StgL
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
import qualified Data.Set as S

(&) :: a -> (a -> b) -> b
(&) = flip ($)

-- | The type of an entry function.
-- | () -> void
irTypeEntryFn :: IRType
irTypeEntryFn = IRTypeFunction [] IRTypeVoid


-- | The type of a continuation given by alts
-- | () -> void
irTypeContinuation :: IRType
irTypeContinuation = irTypeEntryFn

-- | The type of the ID of a heap object
irTypeHeapObjId :: IRType
irTypeHeapObjId = irTypeInt32


-- | The type of an entry function we need to tail call into.
-- remember, "boxed value" is a lie, they're just functions.
irTypeEntryFnPtr :: IRType
irTypeEntryFnPtr = IRTypePointer irTypeEntryFn

-- | Type of the info struct
-- struct {}
irTypeInfoStruct :: IRType
irTypeInfoStruct = IRTypeStruct [ irTypeEntryFnPtr, -- pointer to function to call,
                                  irTypeHeapObjId -- ID of this object
                                ]

-- | Type of the constructor tag
irTypeConstructorTag :: IRType
irTypeConstructorTag = irTypeInt32

-- | Type of a heap object
-- TODO: keep a pointer to the info table. Right now, just store a copy of the
-- info table, since it is way easier to do this.
-- struct { info, void *mem }
irTypeHeapObject :: IRType
irTypeHeapObject = IRTypeStruct [irTypeInfoStruct, -- info table,
                                 irTypeMemoryPtr -- data payload
                                 ]

-- | A pointer to a heap object
irTypeHeapObjectPtr :: IRType
irTypeHeapObjectPtr = IRTypePointer irTypeHeapObject

-- | Int value corresponding to binding
type BindingId = Int
-- | Data associated to a binding
data BindingData = BindingData {
  binding :: Binding,
  bindingId :: BindingId,
  bindingFn :: Value
}

instance Pretty BindingData where
  pretty BindingData{..}=
    vcat [pretty "BindingData {",
      indent 4 (vcat $ [pretty "binding :=" <+> pretty binding,
                      pretty "id := " <+> pretty bindingId,
                      pretty "bindingFn :=" <+> pretty bindingFn]), pretty "}"]


-- | Int val corresponding to to constructor
type ConstructorId = Int

data ConstructorData = ConstructorData {
  constructorName :: ConstructorName,
  constructorId :: ConstructorId
}

-- | TODO: create an IRStack object to represent a stack in LLVM IR
-- | G = global, P = pointer. General Context that we need throughout.
data Context = Context {
    -- | Stack pointer to continuation values.
    contstackGP:: Value,
    -- | Number of continuation values on the stack.
    contstacknG :: Value,
    -- | Register for the tag of a constructor
    rtagG :: Value,
    -- | Binding name to binding data
    bindingNameToData :: M.OrderedMap VarName BindingData,
    -- | constructor name to constructor data
    constructorNameToData :: M.OrderedMap ConstructorName ConstructorData,
    -- | Matcher function
    fnmatcher :: Value,
    -- | function to push continuation value to stack
    fnpushcont :: Value,
    -- | function to pop continuation value on stack
    fnpopcont :: Value
}


-- | Get all bindings in a program
getBindsInProgram :: Program -> [Binding]
getBindsInProgram prog = prog >>= collectBindingsInBinding


-- | Get all constructors in a program
getConstructorNamesInProgram :: Program -> [ConstructorName]
getConstructorNamesInProgram prog = prog >>= collectConstructorNamesInBinding


-- | Build the function stubs that corresponds to the binding.
-- We first build all the stubs to populate the Context. Then, we can build
-- the indivisual bindings.
buildFnStubForBind :: Binding -> State ModuleBuilder Value
buildFnStubForBind Binding{..} = let
    paramsty = []
    retty = IRTypeVoid
    fnname = ("bind." ++ (_unVarName _bindingName))
  in
    createFunction paramsty retty fnname


-- | Create a function that allocates a constructor heap object.
_createAllocConstructorFn ::  State ModuleBuilder Value
_createAllocConstructorFn = do
  lbl <- createFunction [irTypeHeapObjId] irTypeHeapObjectPtr "alloc_constructor"
  runFunctionBuilder lbl $ do
    mem <- "mem" =:= InstMalloc irTypeHeapObject
    heapObjIdLoc <- "heapObjIdLoc" =:= InstGEP mem [ValueConstInt 0, ValueConstInt 0, ValueConstInt 1]
    idval <- getParamValue 0
    appendInst $ InstStore heapObjIdLoc idval
    return ()
  return lbl

-- | Create a function that pushes values on the stack
_createStackPushFn :: String  -- ^function name
                      -> IRType -- ^type of stack elements
                      -> Value-- ^count global
                      -> Value -- ^stack pointer global
                      -> State ModuleBuilder Value
_createStackPushFn fnname elemty nG stackGP = do
  lbl <- createFunction [elemty] IRTypeVoid fnname
  runFunctionBuilder lbl $ do
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
_createStackPopFn :: String -- ^Function name
                     -> IRType -- ^type of stack elements
                     -> Value -- ^count global
                     -> Value -- ^stack pointer global
                     -> State ModuleBuilder Value
_createStackPopFn fnname elemty nG stackGP = do
  lbl <- createFunction [] elemty  fnname
  runFunctionBuilder lbl $ do
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
-- LLVM Module for our program.
createContext :: [Binding] -> [ConstructorName] -> State ModuleBuilder Context
createContext bs cnames = do
  contstack <- createGlobalVariable "stackcont" (IRTypePointer irTypeContinuation)
  contn <- createGlobalVariable "contn" (IRTypeInt 32)
  bfns <- for bs buildFnStubForBind

  rtag <- createGlobalVariable "rtag" irTypeConstructorTag

  let matcherretty = IRTypePointer  (IRTypeFunction [] IRTypeVoid)
  fnmatcher <- createFunction [] matcherretty "matcher"

  let bdatas = [BindingData {
                  bindingId=bid,
                  bindingFn=fn,
                  binding=b} | bid <- [1..] | fn <- bfns | b <- bs]
  let bnames = map (_bindingName . binding) bdatas


  let cdatas = [ConstructorData {
    constructorId=cid,
    constructorName=cname
  } | cid <- [1..] | cname <- cnames]



  pushcont <- _createStackPushFn "pushcont" irTypeContinuation contn contstack
  popcont <- _createStackPopFn "popcont" irTypeContinuation contn contstack

  -- allocContructor <- _createAllocConstructorFn

  return $ Context {
    contstackGP=contstack,
    contstacknG=contn,
    rtagG=rtag,
    bindingNameToData=M.fromList (zip bnames bdatas),
    constructorNameToData=M.fromList (zip cnames cdatas),
    fnmatcher=fnmatcher,
    fnpushcont=pushcont,
    fnpopcont=popcont
 }

-- | Push a continuation into the stack. Used by alts
pushCont :: Context -> Value -> State FunctionBuilder ()
pushCont ctx val = do
  let f = fnpushcont ctx
  appendInst $ InstCall f [val]
  return ()

-- | Create the instruction to pop a continuation from the stack.
-- Used by alts.
-- Note that return value needs to be named with (=:=)
popCont :: Context -> Inst
popCont ctx =
  let f = fnpushcont ctx in InstCall f []

createMatcher :: Context -> State ModuleBuilder ()
createMatcher ctx = do
    runFunctionBuilder (fnmatcher ctx) (buildMatcherFn_ (bindingNameToData ctx))
    where
    -- | Build a BB of the matcher that mathes with the ID and returns the
    -- actual function.
    -- Return the IR::Value of the switch case needed, and the label of the BB
    -- to jump to.
    buildMatchBBForBind_ :: M.OrderedMap VarName BindingData -> VarName -> State FunctionBuilder (Value, BBLabel)
    buildMatchBBForBind_ bdata bname = do
      bbid <- createBB  ("switch." ++ (_unVarName bname))
      focusBB bbid
      let bfn = (bdata M.! bname) & bindingFn :: Value
      let bid = (bdata M.! bname) & bindingId :: BindingId

      -- setRetInst (RetInstReturn (ValueFnPointer bfn))
      return ((ValueConstInt 3), bbid)
      -- return ((ValueConstInt bid), bbid)
    -- | Build the matcher function, that takes a function ID and returns the
    -- function corresponding to the ID.
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
  bid = bindingId $ (bindingNameToData ctx) M.! bname
  in InstCall (fnmatcher ctx) [(ValueConstInt bid)]


-- | push an STG atom to the correct stack
pushAtomToStack :: Context -> M.OrderedMap VarName Value -> Atom -> State FunctionBuilder ()
pushAtomToStack ctx _ (AtomInt (StgInt i)) =
  pushInt ctx (ValueConstInt i) where
    pushInt _ _ = error "Unimplemented pushInt"
pushAtomToStack ctx nametoval (AtomVarName v) = pushCont ctx (nametoval M.! v)


-- | Generate code for an expression node in the IR
codegenExprNode :: Context
                  -> M.OrderedMap VarName Value -- ^mapping between variable name and which value to use to access this
                  -> ExprNode -- ^expression node
                  -> State FunctionBuilder ()
-- | Function appplication codegen
codegenExprNode ctx nametoval (ExprNodeFnApplication fnname atoms) = do
  fn <- case fnname `M.lookup` nametoval of
          Just fn_ -> return $ fn_
          Nothing -> -- actually we should check globals, but fuck it for now
                     -- "fn" =:= createMatcherCallWithName ctx fnname
                     error "unimplemented, what should I do in this context?"
  for atoms (pushAtomToStack ctx nametoval)
  appendInst $ InstCall fn []

  return ()

-- | Constructor codegen
codegenExprNode ctx nametoval (ExprNodeConstructor (Constructor name atoms)) = do
  jumpfn <- "jumpfn" =:= popCont ctx
  for atoms (pushAtomToStack ctx nametoval)
  appendInst $ InstCall jumpfn []
  return ()


codegenExprNode _ nametoval  e = error . docToString $
  vcat [pretty " Unimplemented codegen for exprnode: ", indent 4 (pretty e)]

-- | Setup a binding with name VarName
setupTopLevelBinding :: Context -> VarName -> State FunctionBuilder ()
setupTopLevelBinding ctx name = do
  let b = binding $ (bindingNameToData ctx) M.! name :: Binding
  let Lambda{_lambdaFreeVarIdentifiers=free,
             _lambdaBoundVarIdentifiers=bound,
             _lambdaExprNode=e} = _bindingLambda b

    -- if bound = A B C, stack will have
    -- C
    -- B
    -- A
    -- So we need to reverse the stack
  boundvals <-  for (reverse bound) (\b -> (_unVarName b) =:= (popCont ctx))
  let boundNameToVal = M.fromList $ zip bound boundvals :: M.OrderedMap VarName Value
  let toplevelNameToVal = fmap bindingFn (bindingNameToData ctx) :: M.OrderedMap VarName Value

  let nameToVal = boundNameToVal `M.union` toplevelNameToVal

  codegenExprNode ctx nameToVal e


programToModule :: Program -> Module
programToModule p = runModuleBuilder $ do
    let bs = getBindsInProgram p
    let cs = getConstructorNamesInProgram p
    ctx <- createContext bs cs
    createMatcher ctx
    for_ (M.toList . bindingNameToData $ ctx)
          (\(bname, bdata) -> runFunctionBuilder
                                  (bindingFn bdata)
                                  (setupTopLevelBinding ctx bname))
    return ()
