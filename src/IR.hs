{-# LANGUAGE GADTs, RecordWildCards #-}
module IR where
import Data.Text.Prettyprint.Doc as PP
import qualified Data.List.NonEmpty as NE
import qualified OrderedMap as M


-- | A phantom type used to refer to function parameter labels
data Param

-- | Types that IR values can have
data IRType = IRTypeInt Int | -- ^ number of bits
              IRTypeVoid |
              IRTypePointer IRType |
              IRTypeFunction [IRType] IRType |
              IRTypeStruct [IRType] deriving(Eq)

-- | The type of something that points to a block of memory from, say, malloc.
-- | consider this as void*.
irTypeMemoryPtr :: IRType
irTypeMemoryPtr = IRTypePointer (IRTypeInt 8)

-- | The type of a 32 bit integer. Handy alias to have.
irTypeInt32 :: IRType
irTypeInt32 = IRTypeInt 32

-- | The type of a boolean.
irTypeBool :: IRType
irTypeBool = IRTypeInt 1

instance Pretty IRType where
  pretty (IRTypeInt i) = pretty "int" <+> pretty i
  pretty IRTypeVoid = pretty "void"
  pretty (IRTypePointer ty) = hcat [parens (pretty ty), pretty "^"]
  pretty (IRTypeFunction params ret) =
    parens (hcat . punctuate comma $ pparams) <+> pretty "->" <+> pretty ret where
      pparams = map pretty params
  pretty (IRTypeStruct tys) = pretty "struct" <+>
    parens (hcat (punctuate comma (map pretty tys)))

-- | A label that uses the phantom @a as a type based discriminator
data Label a = Label { unLabel ::  String } deriving(Eq, Ord)
instance Pretty (Label a) where
  pretty (Label s) = pretty s

-- a Value which can be passed as an operand to an instruction
data Value = ValueConstInt Int
            | ValueInstRef (Label Inst)
            | ValueParamRef (Label Param)
            | ValueFnPointer (Label Function)
            | ValueGlobalRef (Label GlobalValue)
            | ValueSizeOf(IRType)
            | ValueUndef (IRType) -- ^An undef value of any chosen type

-- a GlobalValue, that is a value of a global variable.
data GlobalValue = GlobalValue { gvType :: IRType, gvValue :: Maybe Value }
type GlobalLabel = Label GlobalValue

instance Pretty GlobalValue where
  pretty (GlobalValue{..}) = pretty "@_" <+> pretty gvValue <+> colon <+> pretty gvType

instance Pretty Value where
  pretty (ValueConstInt i) = pretty i <> pretty "#"
  pretty (ValueInstRef name) = pretty "%" <> pretty name
  pretty (ValueParamRef name) = pretty "%param." <> pretty name
  pretty (ValueFnPointer name) = pretty "@fnptr." <> pretty name
  pretty (ValueGlobalRef name) = pretty "@" <> pretty name
  pretty (ValueSizeOf name) = pretty "sizeof" <> pretty name
  pretty (ValueUndef ty) = parens $ pretty "undef:" <> pretty ty

-- | Instructions that we allow within a basic block.
data Inst where
  InstAdd :: Value -> Value -> Inst
  InstMul :: Value -> Value -> Inst
  InstL :: Value -> Value -> Inst
  InstAnd :: Value -> Value -> Inst
  -- | Load a value from a memory location
  InstLoad :: Value -> Inst
  -- | Store a value
  InstStore :: Value -- ^Store location
               -> Value -- ^Value to store
               -> Inst
  -- | GetElementPtr
  InstGEP :: Value -- ^Root of GEP
             -> [Value] -- ^ Indexing expression
             -> Inst
  InstPhi :: NE.NonEmpty (BBLabel, Value) -> Inst
  -- | Call a function
  InstCall :: Value -- ^ Function name
              -> [Value] -- ^Parameters
              -> Inst
  -- | Allocate memory.
  InstMalloc :: IRType -- ^type to alloc
               -> Inst

instance Pretty Inst where
  pretty (InstAdd l r) = pretty "add" <+> pretty l <+> pretty r
  pretty (InstMul l r) = pretty "mul" <+> pretty l <+> pretty r
  pretty (InstL l r) = pretty "lessthan" <+> pretty l <+> pretty r
  pretty (InstAnd l r) = pretty "and" <+> pretty l <+> pretty r
  pretty (InstLoad op) = pretty "load" <+> pretty op
  pretty (InstGEP base offsets) =
    pretty "gep" <+>
    parens(pretty "base:" <+> pretty base) <+>
    hcat (map (brackets . pretty) offsets)

  pretty (InstStore slot val) = pretty "store" <+> pretty val <+>
                                pretty "in" <+> pretty slot
  pretty (InstPhi philist) =
    hcat (punctuate comma (NE.toList (fmap (\(bbid, val) ->
                                parens (pretty bbid <+> pretty val)) philist)))

  pretty (InstCall fn params) =
    pretty "call" <+> pretty fn <+>
      parens (hcat (punctuate comma (map pretty params)))

  pretty (InstMalloc mem) =
    pretty "malloc" <+> pretty mem

-- | Represents @a that is optionally named by a @Label a
data Named a = Named { namedName :: Label a, namedData :: a } | UnNamed { namedData :: a}

instance Pretty a => Pretty (Named a) where
  pretty (Named name data') = pretty name <+> pretty ":=" <+> pretty data'
  pretty (UnNamed data') = pretty data'


-- | Used to identify basic blocks
-- | Note that these are usually *unique*
type BBLabel = Label BasicBlock
-- | A basic block. Single-entry, multiple-exit.
data BasicBlock = BasicBlock { bbInsts :: [Named Inst], bbRetInst :: RetInst , bbLabel :: Label BasicBlock }

-- | Default basic block.
defaultBB :: BasicBlock
defaultBB = BasicBlock [] (RetInstVoid) (Label "bbundefined")

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
  RetInstReturn Value |
  RetInstVoid

instance Pretty RetInst where
  pretty (RetInstVoid) = pretty "ret void"
  pretty (RetInstBranch next) = pretty "branch" <+> pretty next
  pretty (RetInstConditionalBranch cond then' else') = pretty "branch if" <+> pretty cond <+> pretty "then" <+> pretty then' <+> pretty "else" <+> pretty else'
  pretty (RetInstSwitch val default' switches ) =
    vcat [pretty "switch on" <+> pretty val <+>
            brackets (pretty "default:" <+> pretty default'),
          indent 4 (vcat (map pretty switches))]
  pretty (RetInstReturn value) = pretty "return" <+> pretty value

-- | Used to order basic blocks
type BBOrder = Int

-- | A function is a list of basic blocks and parameters, and return type
data Function = Function {
  -- A map from the basic block ID to a basic block.
  functionBBMap :: M.OrderedMap BBLabel BasicBlock,
  -- The ID of the entry basic block.
  functionEntryBBLabel :: BBLabel,
  -- The map from a BB to the order.
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
getBBFunctionsInOrder Function {functionBBMap=bbIdToBBMap} = M.elems bbIdToBBMap

instance Pretty Function where
  pretty (func@Function{functionType=(paramTypes, returnType),..}) =
    vcat [funcheader, indent 4 prettyBBS] where
      funcheader :: Doc a
      funcheader = pretty "fn" <+> pretty functionLabel <+> parens (params) <+> pretty "->" <+> pretty returnType
      formatParam :: IRType -> Label Param -> Doc a
      formatParam ty lbl = pretty lbl <+> colon <+> pretty ty
      params :: Doc a
      params = hsep (punctuate comma (zipWith formatParam paramTypes functionParamLabels))
      prettyBBS :: Doc a
      prettyBBS = vcat . map pretty . getBBFunctionsInOrder $ func


-- A module consists of stuff that is global
data Module = Module {
  moduleFunctions :: [Function],
  moduleGlobals :: M.OrderedMap GlobalLabel GlobalValue
}

instance Pretty Module where
  pretty (Module funcs globals) = let
    mkGlobalPretty :: (GlobalLabel, GlobalValue) -> Doc a
    -- TODO: make GlobalLabel a newtype.
    mkGlobalPretty (lbl, GlobalValue{..}) =
       mkName lbl <+> colon <+> pretty gvType <+> mkRhs gvValue
    -- | Pretty name for the label
    mkName :: GlobalLabel -> Doc a
    mkName name = hcat [pretty "@", pretty name]
    -- | Pretty RHS
    mkRhs :: Maybe Value -> Doc a
    mkRhs Nothing = mempty
    mkRhs (Just v) = pretty ":=" <+> pretty v
    in vcat $ (map mkGlobalPretty (M.toList globals)) ++ (map pretty funcs)

-- Add a function to a module
addFunctionToModule :: Function -> Module -> Module
addFunctionToModule fn mod@Module{..} = mod { moduleFunctions = fn:moduleFunctions }



