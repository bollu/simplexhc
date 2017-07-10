{-# LANGUAGE RecordWildCards #-}

module IRBuilder(
  FunctionBuilder,
  ModuleBuilder,
  getCurrentBBLabel,
  getEntryBBLabel,
  focusBB,
  createBB,
  getTempInstName,
  getTempRetInstName,
  appendInst,
  setRetInst,
  getParamValue,
  runFunctionBuilder,
  runModuleBuilder
  ) where
import IR
import qualified Data.Map.Strict as M
import Data.Traversable
import Data.Foldable
import Control.Monad.State.Strict
import Control.Exception (assert)
import Data.Foldable (length)

type Literal = String

{-
-- At some point, I need this. This is more convenient than overloading the key to store the insertion time.
-- | A dictionary that orders elements by insertion time
data OrderedDict k v = OrderedDict { orderedDictMap :: M.Map k v, orderedDictOrder :: [k] } deriving(Monoid, Show, Functor, Foldable, Traversable)
-}

-- | Make a key unique for a map by using the function f and a function
-- | that takes the key and an ID that is a bump counter.
makeKeyUnique_ :: Ord k => (k -> Int -> k) -> M.Map k v -> k -> k
makeKeyUnique_ f m k =  unique_ f m 0 k where
  unique_ :: Ord k => (k -> Int -> k) -> M.Map k v -> Int -> k -> k
  unique_ f m i k = let uniquekey = f k i in
                        case M.lookup uniquekey m of
                          Nothing -> uniquekey
                          (Just _) -> unique_ f m (i + 1) k


appendLabelWithId_ :: Label a -> Int -> Label a
appendLabelWithId_ (Label l) i = if i == 0
                                 then Label l 
                                 else Label (l ++ "." ++ show i)

makeLabelUniqueKey_ :: Label a -> M.Map (Label a) v -> Label a
makeLabelUniqueKey_ l m = makeKeyUnique_ appendLabelWithId_ m l 

{-
-- | Insert into the map by first making the key unique and then inserting
-- | the value into the map
insertWithUniqueKey_ :: Ord k => (k -> Int -> k) -> -- ^ Make the key unique
                        k -> v -> M.Map k v -> (k,  M.Map k v)
insertWithUniqueKey_  f k v m = let uniquekey = makeKeyUnique f m k in
                                   (uniquekey, M.insert uniquekey v m)
-- | Insert into a map by making the label unique
insertWithUniqueLabel :: (Label a) -> v -> M.Map (Label a) v -> 
                         (Label a, M.Map (Label a) v)
insertWithUniqueLabel =  insertWithUniqueKey_ appendLabelWithId_
-}

data FunctionBuilder = FunctionBuilder {
  -- | The first BB that is present in the module
  entryBBLabel :: BBLabel,
 -- | The BB the builder is currently focused on
  currentBBLabel :: BBLabel,
  -- | Mapping from BBLabel to BasicBlock
  bbLabelToBB :: M.Map BBLabel BasicBlock,
  -- | counter to generate new instruction name
  tmpInstNamesCounter :: Int,
  -- | Mapping from BBLabel to the order in which it was created
  bbLabelToOrder :: M.Map BBLabel BBOrder,
  -- | The type of the function
  type' :: ([IRType], IRType),
  -- | Map between parameters and their corresponding value
  paramLabelToParam :: M.Map (Label Param) Value,
  -- | Unique label of the function
  functionBuilderFunctionLabel :: FunctionLabel
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
        bbid <- createBB (Label "entry")
        -- Set the "entry" basic block so we can later give it to IRProgram
        modify (\b -> b { entryBBLabel = bbid })
        focusBB bbid
      initbuilder = (FunctionBuilder {
        entryBBLabel=Label "INVALID BB",
        currentBBLabel=Label "INVALID BB",
        bbLabelToBB=mempty,
        bbLabelToOrder=mempty,
        tmpInstNamesCounter=0,
        paramLabelToParam=M.fromList $ map (\pname -> (pname, ValueParamRef pname)) pnames,
        type'=(paramsty, retty),
        functionBuilderFunctionLabel=label

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
createBB :: Label BasicBlock -> State FunctionBuilder BBLabel
createBB name = do
  idtobbs <- gets bbLabelToBB
  idtoorder <- gets bbLabelToOrder

  let nbbs = M.size idtobbs
  let bborder = nbbs -- We can use the number of BBs as a unique stamp on this BB that will
                     -- provide a total order in terms of time of creation.
  let newbbid = makeLabelUniqueKey_ name idtobbs :: BBLabel
  let newbb = defaultBB { bbLabel=newbbid }
  modify (\b -> b { bbLabelToBB = M.insert newbbid newbb idtobbs,
                    bbLabelToOrder = M.insert newbbid bborder idtoorder} )
  return newbbid


-- | Create a temporary instruction name.
getTempInstName :: State FunctionBuilder (Label Inst)
getTempInstName = do
  n <- gets tmpInstNamesCounter
  modify (\b -> b { tmpInstNamesCounter=n+1 })
  return . Label $ "tmp." ++ show n


-- | Create a temporary name for a return instruction
-- | Note that we cheat in the implementation, by just "relabelling"
-- | an instruction label to a ret instruction label.
getTempRetInstName :: State FunctionBuilder (Label RetInst)
getTempRetInstName = Label . unLabel <$> getTempInstName


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


-- | Append instruction "I" to the FunctionBuilder
appendInst :: Named Inst -> State FunctionBuilder Value
appendInst i = do
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
  moduleBuilderFunctions :: M.Map FunctionLabel Function
}

_appendFunctionToModuleBuilder :: Label Function -> Function -> ModuleBuilder -> ModuleBuilder
_appendFunctionToModuleBuilder label fn (mb@ModuleBuilder{..}) =
    mb {
      moduleBuilderFunctions = M.insert label fn moduleBuilderFunctions
    }

-- | Given a function spec and a function name, create it in the modulebuilder.
runFunctionBuilder :: [IRType] -> IRType -> String -> State FunctionBuilder () -> State ModuleBuilder FunctionLabel
runFunctionBuilder ptys retty name fs = do
  -- generate a unique label
  label <- gets (makeLabelUniqueKey_ (Label name) . moduleBuilderFunctions)
  -- fncount <- gets (length . moduleBuilderFunctions)
  let fnbuilder = execState fs (_createFunctionBuilder ptys retty label)
  let fn = _createFunctionFromBuilder fnbuilder
  modify (_appendFunctionToModuleBuilder label fn)
  return label


-- | Run a module builder to create a module
runModuleBuilder :: State ModuleBuilder () -> Module
runModuleBuilder s = let final = execState s _createModuleBuilder in
                      _createModuleFromBuilder final


-- | Create an IR.Module from a ModuleBuilder
_createModuleFromBuilder :: ModuleBuilder -> Module
_createModuleFromBuilder ModuleBuilder{..} = Module (M.elems moduleBuilderFunctions)

-- | Default module builder
_createModuleBuilder ::  ModuleBuilder
_createModuleBuilder = ModuleBuilder {moduleBuilderFunctions=mempty}

-- | Create an IR.function from a FunctionBuilder
_createFunctionFromBuilder :: FunctionBuilder -> IR.Function
_createFunctionFromBuilder  FunctionBuilder{..} =
  Function {
    functionBBMap=bbLabelToBB,
    functionEntryBBLabel=entryBBLabel,
    functionBBOrderingMap=bbLabelToOrder,
    functionType=type',
    functionLabel=functionBuilderFunctionLabel,
    functionParamLabels=map _getParamName [0..(length paramLabelToParam)]
  }

-- | Get the i'th parameter in the function. 0-indexed
getParamValue :: Int -> State FunctionBuilder Value
getParamValue i = do
    params' <- gets paramLabelToParam
    return (assert (i < length params' && i >= 0) (params' M.! (_getParamName i)))

