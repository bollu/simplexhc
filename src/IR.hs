{-# LANGUAGE GADTs, ViewPatterns #-}
module IR where
import Data.Text.Prettyprint.Doc as PP
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.List (sortOn)


data IRType = IRTyInt Int | IRTyVoid -- ^ number of bits

instance Pretty IRType where
  pretty (IRTyInt i) = pretty "int" <+> pretty i
  pretty IRTyVoid = pretty "void"

-- | A label that uses the phantom @a as a type based discriminator
data Label a = Label { unLabel ::  String } deriving(Eq, Ord)
instance Pretty (Label a) where
  pretty (Label s) = pretty s

-- a Value, which can either be a constant, or a reference to an instruction.
data Value = ValueConstInt Int | ValueInstRef (Label Inst)

instance Pretty Value where
  pretty (ValueConstInt i) = pretty i <> pretty "#"
  pretty (ValueInstRef name) = pretty "%" <> pretty name

-- | Instructions that we allow within a basic block.
data Inst  where
  InstAlloc :: Inst
  InstAdd :: Value -> Value -> Inst
  InstMul :: Value -> Value -> Inst
  InstL :: Value -> Value -> Inst
  InstAnd :: Value -> Value -> Inst
  InstLoad :: Value -> Inst 
  InstStore :: Value -> Value -> Inst 
  InstPhi :: NE.NonEmpty (BBId, Value) -> Inst

instance Pretty Inst where
  pretty (InstAlloc) = pretty "alloc"
  pretty (InstAdd l r) = pretty "add" <+> pretty l <+> pretty r
  pretty (InstMul l r) = pretty "mul" <+> pretty l <+> pretty r
  pretty (InstL l r) = pretty "lessthan" <+> pretty l <+> pretty r
  pretty (InstAnd l r) = pretty "and" <+> pretty l <+> pretty r
  pretty (InstLoad op) = pretty "load" <+> pretty op
  pretty (InstStore slot val) = pretty "store" <+> pretty val <+>
                                pretty "in" <+> pretty slot
  pretty (InstPhi philist) = 
    hcat (punctuate comma (NE.toList (fmap (\(bbid, val) ->
                                brackets (pretty bbid <+> pretty val)) philist)))

-- | Represents @a that is optionally named by a @Label a
data Named a = Named { namedName :: Label a, namedData :: a }


-- | Infix operator for @Named constructor
(=:=) :: Label a  -> a -> Named a
name =:= a = Named name a


instance Pretty a => Pretty (Named a) where
  pretty (Named name data') = pretty name <+> pretty ":=" <+> pretty data'


-- | Used to identify basic blocks
type BBId = Label BasicBlock
-- | A basic block. Single-entry, multiple-exit.
data BasicBlock = BasicBlock { bbInsts :: [Named Inst], bbRetInst :: RetInst , bbLabel :: Label BasicBlock }

-- | Default basic block.
defaultBB :: BasicBlock
defaultBB = BasicBlock [] (RetInstTerminal) (Label "undefined")

-- TODO: replace nest with indent
instance Pretty BasicBlock where
  pretty (BasicBlock insts ret label) = 
    nest 4 (vsep ([pretty label <> pretty ":"] ++ body)) where
      body = map pretty insts ++ [pretty ret]


-- | Return instructions are the only ones that can cause control flow
-- | between one basic block to another.
data RetInst =
  RetInstConditionalBranch Value BBId BBId |
  RetInstBranch BBId |
  RetInstSwitch {
    switchDefaultBB :: BBId,
    switchBBs :: [(Value, BBId)]
  } | 
  RetInstTerminal

instance Pretty RetInst where
  pretty (RetInstTerminal) = pretty "TERMINAL"
  pretty (RetInstBranch next) = pretty "branch" <+> pretty next
  pretty (RetInstConditionalBranch cond then' else') = pretty "branch if" <+> pretty cond <+> pretty "then" <+> pretty then' <+> pretty "else" <+> pretty else'
  pretty (RetInstSwitch default' switches ) =
    pretty "switch" <+> brackets (pretty "default: " <+> pretty default') <+>
      hcat (map pretty switches)



-- | Used to order basic blocks
type BBOrder = Int


-- | A function is a list of basic blocks and parameters, and return type
data Function = Function {
  -- A map from the basic block ID to a basic block.
  functionBBMap :: M.Map BBId BasicBlock,
  -- The ID of the entry basic block.
  functionEntryBBId :: BBId,
  -- The map from a BB to the order.
  functionBBOrderingMap :: M.Map BBId BBOrder,
  -- The type of the function ([parameter types], return type)
  functionType :: ([IRType], IRType)
}

-- | Label for a function
type FunctionId = Label Function

-- TODO: use view patterns to extract only the values of the dict.
-- | Get the functions in the basic block in the order they were created
getBBFunctionsInOrder :: Function -> [BasicBlock]
getBBFunctionsInOrder Function { functionBBOrderingMap=bbToOrdering,
                                 functionBBMap=bbIdToBBMap} =
      map (bbIdToBBMap M.!) sortedKeys where
      sortedKeys = sortOn (bbToOrdering M.!) (M.keys bbIdToBBMap)

instance Pretty Function where
  pretty func = vcat . map pretty . getBBFunctionsInOrder $ func


-- A module consists of stuff that is global
data Module = Module {
  moduleFunctions :: [Function]
}

instance Pretty Module where
  pretty (Module funcs) = vcat (map pretty funcs)

-- Add a function to a module
addFunctionToModule :: Function -> Module -> Module
addFunctionToModule fn mod = Module { moduleFunctions = fn:(moduleFunctions mod) }



