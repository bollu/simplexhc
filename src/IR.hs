{-# LANGUAGE GADTs, RecordWildCards #-}
module IR where
import Data.Text.Prettyprint.Doc as PP
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.List (sortOn)


-- | A phantom type used to refer to function parameter labels
data Param

-- | Types that IR values can have
data IRType = IRTypeInt Int | IRTypeVoid -- ^ number of bits

instance Pretty IRType where
  pretty (IRTypeInt i) = pretty "int" <+> pretty i
  pretty IRTypeVoid = pretty "void"

-- | A label that uses the phantom @a as a type based discriminator
data Label a = Label { unLabel ::  String } deriving(Eq, Ord)
instance Pretty (Label a) where
  pretty (Label s) = pretty s

-- a Value, which can either be a constant, or a reference to an instruction.
data Value = ValueConstInt Int | ValueInstRef (Label Inst) | ValueParamRef (Label Param)

instance Pretty Value where
  pretty (ValueConstInt i) = pretty i <> pretty "#"
  pretty (ValueInstRef name) = pretty "%" <> pretty name
  pretty (ValueParamRef name) = pretty "%param." <> pretty name

-- | Instructions that we allow within a basic block.
data Inst  where
  InstAlloc :: Inst
  InstAdd :: Value -> Value -> Inst
  InstMul :: Value -> Value -> Inst
  InstL :: Value -> Value -> Inst
  InstAnd :: Value -> Value -> Inst
  InstLoad :: Value -> Inst 
  InstStore :: Value -> Value -> Inst 
  InstPhi :: NE.NonEmpty (BBLabel, Value) -> Inst

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
-- | Note that these are usually *unique*
type BBLabel = Label BasicBlock
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
  RetInstConditionalBranch Value BBLabel BBLabel |
  RetInstBranch BBLabel |
  RetInstSwitch {
    switchValue :: Value,
    switchDefaultBB :: BBLabel,
    switchBBs :: [(Value, BBLabel)]
  } |
  RetInstTerminal

instance Pretty RetInst where
  pretty (RetInstTerminal) = pretty "TERMINAL"
  pretty (RetInstBranch next) = pretty "branch" <+> pretty next
  pretty (RetInstConditionalBranch cond then' else') = pretty "branch if" <+> pretty cond <+> pretty "then" <+> pretty then' <+> pretty "else" <+> pretty else'
  pretty (RetInstSwitch val default' switches ) =
    vcat [pretty "switch on" <+> pretty val <+>
            brackets (pretty "default:" <+> pretty default'),
          indent 4 (vcat (map pretty switches))]



-- | Used to order basic blocks
type BBOrder = Int

-- | A function is a list of basic blocks and parameters, and return type
data Function = Function {
  -- A map from the basic block ID to a basic block.
  functionBBMap :: M.Map BBLabel BasicBlock,
  -- The ID of the entry basic block.
  functionEntryBBLabel :: BBLabel,
  -- The map from a BB to the order.
  functionBBOrderingMap :: M.Map BBLabel BBOrder,
  -- The type of the function ([parameter types], return type)
  functionType :: ([IRType], IRType),
  -- The parameters names of the function
  functionParamLabels :: [Label Param],
  -- The label of the function
  functionLabel :: FunctionLabel
}

-- | Label for a function
type FunctionLabel = Label Function

-- TODO: use view patterns to extract only the values of the dict.
-- | Get the functions in the basic block in the order they were created
getBBFunctionsInOrder :: Function -> [BasicBlock]
getBBFunctionsInOrder Function { functionBBOrderingMap=bbToOrdering,
                                 functionBBMap=bbIdToBBMap} =
    map (bbIdToBBMap M.!) unsortedKeys where
      sortedKeys :: [BBLabel]
      sortedKeys = sortOn  (bbToOrdering M.!) unsortedKeys
      unsortedKeys :: [BBLabel]
      unsortedKeys = (M.keys bbIdToBBMap)

instance Pretty Function where
  pretty (func@Function{functionType=(paramTypes, returnType),..}) =
    vcat [funcheader, indent 4 prettyBBS] where
      funcheader :: Doc a
      funcheader = pretty "fn" <+> pretty functionLabel <+> braces (params) <+> pretty "->" <+> pretty returnType
      formatParam :: IRType -> Label Param -> Doc a
      formatParam ty lbl = pretty lbl <+> colon <+> pretty ty
      params :: Doc a
      params = hsep (punctuate comma (zipWith formatParam paramTypes functionParamLabels))
      prettyBBS :: Doc a
      prettyBBS = vcat . map pretty . getBBFunctionsInOrder $ func


-- A module consists of stuff that is global
data Module = Module {
  moduleFunctions :: [Function]
}

instance Pretty Module where
  pretty (Module funcs) = vcat (map pretty funcs)

-- Add a function to a module
addFunctionToModule :: Function -> Module -> Module
addFunctionToModule fn mod = Module { moduleFunctions = fn:(moduleFunctions mod) }



