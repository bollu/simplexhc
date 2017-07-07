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
  functionLabel :: FunctionLabel
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
        functionLabel=label

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
  let newbbid = Label ((unLabel name) ++ "." ++ show nbbs)
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
  fncount <- gets (length . moduleBuilderFunctions)
  -- generate a unique label
  let label = Label (name ++ "." ++ show fncount)
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
    functionType=type'
           }

-- | Get the i'th parameter in the function. 0-indexed
getParamValue :: Int -> State FunctionBuilder Value
getParamValue i = do
    params' <- gets paramLabelToParam
    return (assert (i < length params' && i >= 0) (params' M.! (_getParamName i)))

