module IRBuilder where
import IR 
import qualified Data.Map.Strict as M
import Data.Traversable
import Data.Foldable
import Control.Monad.State.Strict

type Literal = String

data Builder = Builder {
  -- | The first BB that is present in the module
  entryBBId :: BBId,
 -- | The BB the builder is currently focused on
  currentBBId :: BBId,
  -- | Mapping from BBId to BasicBlock
  bbIdToBB :: M.Map BBId BasicBlock,
  -- | counter to generate new instruction name
  tmpInstNamesCounter :: Int,
  -- | Map from literal name to Value
  literalToValue :: M.Map Literal Value
}

-- | Create a new builder with an empty basic block
newBuilder :: Builder
newBuilder = 
  execState mkDefaultBB initbuilder
    where
      mkDefaultBB = do
        bbid <- createNewBB (Label "default")
        -- Set the "entry" basic block so we can later give it to IRProgram
        modify (\b -> b { entryBBId = bbid })

      initbuilder = (Builder {
        entryBBId = Label "",
        currentBBId = Label "",
        bbIdToBB = M.empty,
        tmpInstNamesCounter=0,
        literalToValue=mempty
    }) 

-- | Get the current Basic block ID
getCurrentBBId :: State Builder BBId
getCurrentBBId = gets currentBBId

-- | Focus the basic block given by the ID
focusBB :: BBId -> State Builder ()
focusBB id = modify (\b-> b { currentBBId=id })

-- | Append a new basic block. DOES NOT switch the currentBBId to the new basic block. For that, see focusBB
createNewBB :: Label Builder -> State Builder BBId
createNewBB name = do
  idtobbs <- gets bbIdToBB
  let nbbs = M.size idtobbs
  let nameunique = Label ((unLabel name) ++ "." ++ show nbbs)
  let newbb = defaultBB { bbLabel=nameunique }
  modify (\b -> b { bbIdToBB = M.insert nameunique newbb idtobbs  } )
  return nameunique


-- | Create a temporary instruction name.
getTempInstName :: State Builder (Label Inst)
getTempInstName = do
  n <- gets tmpInstNamesCounter
  modify (\b -> b { tmpInstNamesCounter=n+1 })
  return . Label $ "tmp." ++ show n


-- | Create a temporary name for a return instruction
-- | Note that we cheat in the implementation, by just "relabelling"
-- | an instruction label to a ret instruction label.
getTempRetInstName :: State Builder (Label RetInst)
getTempRetInstName = Label . unLabel <$> getTempInstName

-- | Add a mapping between literal and value.
mapLiteralToValue :: Literal -> Value -> State Builder ()
mapLiteralToValue l v = do
  ltov <- gets literalToValue
  -- TODO: check that we do not repeat literals.
  modify (\b -> b { literalToValue=M.insert l v ltov })
  return ()

-- | Get the value that the Literal maps to.
getLiteralValueMapping :: Literal -> State Builder Value
getLiteralValueMapping lit = do
  ltov <- gets literalToValue
  return $ ltov M.! lit

-- | lift an edit of a basic block to the current basic block focused
-- | in the Builder.
liftBBEdit :: (BasicBlock -> BasicBlock) -> Builder -> Builder
liftBBEdit f builder = builder {
    bbIdToBB = M.adjust f (currentBBId builder) (bbIdToBB builder) 
}

-- | Set the builder's current basic block to the i'th basic block
setBB :: Builder -> BBId -> Builder
setBB builder i = builder {
  currentBBId = i
}


-- | Append instruction "I" to the builder
appendInst :: Named Inst -> State Builder Value
appendInst i = do
  modify . liftBBEdit $ (appendInstToBB i) 
  return $ ValueInstRef (namedName i)
  where
    appendInstToBB :: Named Inst -> BasicBlock -> BasicBlock
    appendInstToBB i bb = bb { bbInsts=bbInsts bb ++ [i] }

setRetInst :: RetInst -> State Builder ()
setRetInst i = do
  modify . liftBBEdit $ (setBBRetInst i) 
  where
    setBBRetInst :: RetInst -> BasicBlock -> BasicBlock
    setBBRetInst i bb = bb { bbRetInst=i }
