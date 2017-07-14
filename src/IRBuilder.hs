{-# LANGUAGE RecordWildCards #-}

module IRBuilder(
  FunctionBuilder,
  ModuleBuilder,
  getCurrentBBLabel,
  getEntryBBLabel,
  focusBB,
  createBB,
  getTempInstName_,
  -- getTempRetInstName_,
  appendInst,
  (=:=),
  setRetInst, getParamValue,
  runFunctionBuilder,
  createFunction, -- NOT STABILISED
  runModuleBuilder,
  createGlobalVariable
  ) where
import IR
-- import qualified Data.Map.Strict as M
import qualified OrderedMap as M
import Data.Traversable
import Data.Foldable
import Control.Monad.State.Strict
import Control.Exception (assert)
import Data.Foldable (length)
import ColorUtils

type Literal = String

-- | Make a key unique for a map by using the function f and a function
-- | that takes the key and an ID that is a bump counter.
makeKeyUnique_ :: Ord k => (k -> Int -> k) -> M.OrderedMap k v -> k -> k
makeKeyUnique_ f m k =  unique_ f m 0 k where
  unique_ :: Ord k => (k -> Int -> k) -> M.OrderedMap k v -> Int -> k -> k
  unique_ f m i k = let uniquekey = f k i in
                        case M.lookup uniquekey m of
                          Nothing -> uniquekey
                          (Just _) -> unique_ f m (i + 1) k


appendLabelWithId_ :: Label a -> Int -> Label a
appendLabelWithId_ (Label l) i = if i == 0
                                 then Label l
                                 else Label (l ++ "." ++ show i)

makeLabelUniqueKey_ :: String -> M.OrderedMap (Label a) v -> Label a
makeLabelUniqueKey_ l m =
  makeKeyUnique_ appendLabelWithId_ m (Label l)

data FunctionBuilder = FunctionBuilder {
  -- | The first BB that is present in the module
  entryBBLabel :: BBLabel,
 -- | The BB the builder is currently focused on
  currentBBLabel :: BBLabel,
  -- | Mapping from BBLabel to BasicBlock
  bbLabelToBB :: M.OrderedMap BBLabel BasicBlock,
  -- | counter to generate new instruction name
  tmpInstNamesCounter :: Int,
  -- | Mapping from BBLabel to the order in which it was created
  -- bbLabelToOrder :: M.OrderedMap BBLabel BBOrder,
  -- | The type of the function
  type' :: ([IRType], IRType),
  -- | Map between parameters and their corresponding value
  paramLabelToParam :: M.OrderedMap (Label Param) Value,
  -- | Unique label of the function
  fbFunctionLabel :: FunctionLabel
}


_getParamName :: Int -> Label Param
_getParamName i = Label ("param." ++ show i)

-- | Create a new function builder with an empty basic block
_createFunctionBuilder :: [IRType] -> -- ^Parameter types
                      IRType -> -- ^Return type
                      FunctionLabel -> -- ^Function Name
                      FunctionBuilder
_createFunctionBuilder paramsty retty label =
  execState mkDefaultBB initbuilder
    where
      mkDefaultBB = do
        bbid <- createBB "entry"
        -- Set the "entry" basic block so we can later give it to IRProgram
        modify (\b -> b { entryBBLabel = bbid })
        focusBB bbid
      initbuilder = (FunctionBuilder {
        entryBBLabel=Label "INVALID BB",
        currentBBLabel=Label "INVALID BB",
        bbLabelToBB=mempty,
        tmpInstNamesCounter=0,
        paramLabelToParam=M.fromList $ map (\pname -> (pname, ValueParamRef pname)) pnames,
        type'=(paramsty, retty),
        fbFunctionLabel=label

    }) where
      -- | initial list of parameter names
      pnames :: [Label Param]
      pnames = map _getParamName [0..(length paramsty)]

getEntryBBLabel :: State FunctionBuilder BBLabel
getEntryBBLabel = gets entryBBLabel

-- | Get the current Basic block ID
getCurrentBBLabel :: State FunctionBuilder BBLabel
getCurrentBBLabel = gets currentBBLabel

-- | Focus the basic block given by the ID
focusBB :: BBLabel -> State FunctionBuilder ()
focusBB id = modify (\b-> b { currentBBLabel=id })

-- | Append a new basic block. DOES NOT switch the currentBBLabel to the new basic block. For that, see focusBB
createBB :: String -> State FunctionBuilder BBLabel
createBB name = do
  idtobbs <- gets bbLabelToBB
  let nbbs = M.size idtobbs
  let bborder = nbbs -- We can use the number of BBs as a unique stamp on this BB that will
                     -- provide a total order in terms of time of creation.
  let newbbid = makeLabelUniqueKey_ name idtobbs :: BBLabel
  let newbb = defaultBB { bbLabel=newbbid }
  modify (\b -> b { bbLabelToBB = M.insert newbbid newbb idtobbs })
  return newbbid


-- | Create a temporary instruction name.
getTempInstName_ :: State FunctionBuilder (Label Inst)
getTempInstName_ = do
  n <- gets tmpInstNamesCounter
  modify (\b -> b { tmpInstNamesCounter=n+1 })
  return . Label $ "_." ++ show n


{-
-- | Create a temporary name for a return instruction
-- | Note that we cheat in the implementation, by just "relabelling"
-- | an instruction label to a ret instruction label.
getTempRetInstName_ :: State FunctionBuilder (Label RetInst)
getTempRetInstName_ = Label . unLabel <$> getTempInstName_
-}


-- | lift an edit of a basic block to the current basic block focused
-- | in the FunctionBuilder.
liftBBEdit :: (BasicBlock -> BasicBlock) -> FunctionBuilder -> FunctionBuilder
liftBBEdit f fbuilder = fbuilder {
      bbLabelToBB = M.adjust f k m
    } where
        -- The key of the BB to adjust.
        k = currentBBLabel fbuilder
        -- The map where the BBs live.
        m = bbLabelToBB fbuilder


-- Append inst I with name to the functionBuilder
(=:=) :: String -> Inst -> State FunctionBuilder Value
name =:= inst = appendNamedInst_ $ Named (Label name) inst


appendInst :: Inst -> State FunctionBuilder Value
appendInst inst = do
  name <- getTempInstName_
  appendNamedInst_  (Named name inst)


-- | Append instruction "I" to the FunctionBuilder
appendNamedInst_ :: Named Inst -> State FunctionBuilder Value
appendNamedInst_ i = do
  modify . liftBBEdit $ (appendInstToBB i)
  return $ ValueInstRef (namedName i)
  where
    appendInstToBB :: Named Inst -> BasicBlock -> BasicBlock
    appendInstToBB i bb = bb { bbInsts=bbInsts bb ++ [i] }

setRetInst :: RetInst -> State FunctionBuilder ()
setRetInst i = do
  modify . liftBBEdit $ (setBBRetInst i)
  where
    setBBRetInst :: RetInst -> BasicBlock -> BasicBlock
    setBBRetInst i bb = bb { bbRetInst=i }


-- == Module builder ==

data ModuleBuilder = ModuleBuilder {
  mbFunctions :: M.OrderedMap FunctionLabel Function,
  mbGlobals :: M.OrderedMap GlobalLabel GlobalValue
}

_mbAppendFunction :: Label Function -> Function -> ModuleBuilder -> ModuleBuilder
_mbAppendFunction label fn (mb@ModuleBuilder{..}) =
    mb {
      mbFunctions = M.insert label fn mbFunctions
    }

-- | To create a function definition, first call `createFunction`.
-- | Given a function label and a builder, create it in the `ModuleBuilder`.
runFunctionBuilder :: Value -> State FunctionBuilder () -> State ModuleBuilder ()
runFunctionBuilder (ValueFnPointer label) fs = do
  -- Get the stub function that was created from createFunction
  origfn <- gets $ (M.! label) . mbFunctions
  let (ptys, retty) = functionType origfn
  let finalbuilder = execState fs (_createFunctionBuilder ptys retty label)

  let fn = _createFunctionFromBuilder finalbuilder
  modify (_mbAppendFunction label fn)

runFunctionBuilder v _ =
  error $ "called runFunctionBuilder, provided non-function value: " ++
            prettyToString v

-- | Create a new function . This is more fine grained that runFunctionBuilder.
createFunction :: [IRType] -> IRType -> String -> State ModuleBuilder Value
createFunction ptys retty name = do
  label <- gets (makeLabelUniqueKey_ name . mbFunctions)
  let defaultfn = _createFunctionFromBuilder (_createFunctionBuilder ptys retty label)
  modify (_mbAppendFunction label defaultfn)
  return $ ValueFnPointer label



-- | Run a module builder to create a module
runModuleBuilder :: State ModuleBuilder () -> Module
runModuleBuilder s = let final = execState s _createModuleBuilder in
                      _createModuleFromBuilder final


-- | Create an IR.Module from a ModuleBuilder
_createModuleFromBuilder :: ModuleBuilder -> Module
_createModuleFromBuilder ModuleBuilder{..} =
  Module (M.elems mbFunctions) mbGlobals

-- | Default module builder
_createModuleBuilder ::  ModuleBuilder
_createModuleBuilder =
  ModuleBuilder {
    mbFunctions=mempty,
    mbGlobals=mempty
  }


-- | Create an IR.function from a FunctionBuilder
_createFunctionFromBuilder :: FunctionBuilder -> IR.Function
_createFunctionFromBuilder  FunctionBuilder{..} =
  Function {
    functionBBMap=bbLabelToBB,
    functionEntryBBLabel=entryBBLabel,
    functionType=type',
    functionLabel=fbFunctionLabel,
    functionParamLabels=map _getParamName [0..(length paramLabelToParam)]
  }

-- | Get the i'th parameter in the function. 0-indexed
getParamValue :: Int -> State FunctionBuilder Value
getParamValue i = do
    params' <- gets paramLabelToParam
    return (assert (i < length params' && i >= 0) (params' M.! (_getParamName i)))

-- | Create an IR.GlobalVariable with the given name
createGlobalVariable :: String -> IRType -> State ModuleBuilder Value
createGlobalVariable name ty = do
  mglobals <- gets mbGlobals
  let label = makeLabelUniqueKey_ name mglobals
  let global = GlobalValue { gvType=ty, gvValue = Nothing }
  modify (\mb -> mb { mbGlobals=M.insert label global mglobals})
  return $ ValueGlobalRef label
