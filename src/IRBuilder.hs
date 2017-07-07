module IRBuilder(
  FunctionBuilder,
  newFunctionBuilder,
  getCurrentBBId,
  focusBB,
  createNewBB,
  getTempInstName,
  getTempRetInstName,
  setBB,
  appendInst,
  setRetInst
  ) where
import IR
import qualified Data.Map.Strict as M
import Data.Traversable
import Data.Foldable
import Control.Monad.State.Strict

type Literal = String

data FunctionBuilder = FunctionBuilder {
  -- | The first BB that is present in the module
  entryBBId :: BBId,
 -- | The BB the builder is currently focused on
  currentBBId :: BBId,
  -- | Mapping from BBId to BasicBlock
  bbIdToBB :: M.Map BBId BasicBlock,
  -- | counter to generate new instruction name
  tmpInstNamesCounter :: Int,
  -- | Map from literal name to Value
  literalToValue :: M.Map Literal Value,
  -- | Mapping from BBId to the order in which it was created
  bbIdToOrder :: M.Map BBId BBOrder,
  -- | The type of the function
  type' :: ([IRType], IRType)
}



-- | Create a new function builder with an empty basic block
newFunctionBuilder :: [IRType] -> -- ^Parameter types
                      IRType -> -- ^Retrurn type
                      FunctionBuilder
newFunctionBuilder paramsty retty =
  execState mkDefaultBB initbuilder
    where
      mkDefaultBB = do
        bbid <- createNewBB (Label "default")
        -- Set the "entry" basic block so we can later give it to IRProgram
        modify (\b -> b { entryBBId = bbid })

      initbuilder = (FunctionBuilder {
        entryBBId=Label "INVALID BB",
        currentBBId=Label "INVALID BB",
        bbIdToBB=mempty,
        bbIdToOrder=mempty,
        tmpInstNamesCounter=0,
        literalToValue=mempty,
        type'=(paramsty, retty)
    })

-- | Get the current Basic block ID
getCurrentBBId :: State FunctionBuilder BBId
getCurrentBBId = gets currentBBId

-- | Focus the basic block given by the ID
focusBB :: BBId -> State FunctionBuilder ()
focusBB id = modify (\b-> b { currentBBId=id })

-- | Append a new basic block. DOES NOT switch the currentBBId to the new basic block. For that, see focusBB
createNewBB :: Label FunctionBuilder -> State FunctionBuilder BBId
createNewBB name = do
  idtobbs <- gets bbIdToBB
  idtoorder <- gets bbIdToOrder

  let nbbs = M.size idtobbs
  let bborder = nbbs -- We can use the number of BBs as a unique stamp on this BB that will
                     -- provide a total order in terms of time of creation.
  let newbbid = Label ((unLabel name) ++ "." ++ show nbbs)
  let newbb = defaultBB { bbLabel=newbbid }
  modify (\b -> b { bbIdToBB = M.insert newbbid newbb idtobbs,
                    bbIdToOrder = M.insert newbbid bborder idtoorder} )
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

-- | Add a mapping between literal and value.
mapLiteralToValue :: Literal -> Value -> State FunctionBuilder ()
mapLiteralToValue l v = do
  ltov <- gets literalToValue
  -- TODO: check that we do not repeat literals.
  modify (\b -> b { literalToValue=M.insert l v ltov })
  return ()

-- | Get the value that the Literal maps to.
getLiteralValueMapping :: Literal -> State FunctionBuilder Value
getLiteralValueMapping lit = do
  ltov <- gets literalToValue
  return $ ltov M.! lit

-- | lift an edit of a basic block to the current basic block focused
-- | in the FunctionBuilder.
liftBBEdit :: (BasicBlock -> BasicBlock) -> FunctionBuilder -> FunctionBuilder
liftBBEdit f fbuilder = fbuilder {
      bbIdToBB = M.adjust f k m
    } where
        -- The key of the BB to adjust.
        k = currentBBId fbuilder
        -- The map where the BBs live.
        m = bbIdToBB fbuilder


-- | Set the FunctionBuilder's current basic block to the i'th basic block
setBB :: FunctionBuilder -> BBId -> FunctionBuilder
setBB fbuilder i = fbuilder {
  currentBBId = i
}

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
  moduleBuilderFunctions :: M.Map FunctionId Function
}