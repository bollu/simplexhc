{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
module IRToLLVM where

-- | L = pure llvm-hs-pure imports
import qualified LLVM.AST as L
import qualified LLVM.AST.Global as L
import qualified LLVM.AST.Constant as LC
-- | RealL = "real world", not haskell pure types
import qualified LLVM.Module as RealL
import qualified LLVM.Context as RealL
import qualified LLVM.AST.CallingConvention as L

-- | We need this because type' is both in LLVM.AST and LLVM.AST.Global
import qualified LLVM.AST.Global
import qualified LLVM.AST.Attribute as L
import qualified LLVM.AST.Type as L

import Data.List(findIndex)

import qualified Data.ByteString.Char8 as C8 (pack)
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as B
         (ShortByteString, toShort, fromShort)
import Data.Char (chr)
import Data.Text.Prettyprint.Doc as PP
import ColorUtils
import qualified Data.Word as W
import qualified OrderedMap as M

import Debug.Trace

import IRBuilder
import IR

mapToList :: (Ord k, Pretty k, Pretty v) => (k -> v -> a) -> M.OrderedMap k v -> [a]
mapToList f m = map (uncurry f) (M.toList m)

data Context = Context {
  globalNameToType :: M.OrderedMap GlobalLabel IRType,
  functionNameToType :: M.OrderedMap FunctionLabel IRType,
  instNameToType :: M.OrderedMap (Label Inst) IRType,
  currentFunction :: Maybe Function
}

-- | Build the Context for the given Module
_buildContextForModule :: Module -> Context
_buildContextForModule (Module{..}) = Context {
  globalNameToType=fmap globalToType moduleGlobals,
  functionNameToType=M.fromList [(functionLabel f, functionToType f) | f <- moduleFunctions],
  instNameToType=mempty,
  currentFunction = Nothing
} where
    functionToType :: Function -> IRType
    functionToType Function{functionType=(paramsty, retty)} = IRTypeFunction paramsty retty

    globalToType :: GlobalValue -> IRType
    globalToType = gvType


-- | Given the map between instruction names to instructions,
-- | Given an instruction, construct its type.
constructInstType :: Context -> M.OrderedMap (Label Inst) Inst -> Inst -> IRType
constructInstType _ _ (InstAdd _ _) = irTypeInt32
constructInstType _ _ (InstMul _ _) = irTypeInt32
constructInstType _ _ (InstL _ _) = irTypeBool
constructInstType _ _ (InstAnd _ _) = irTypeBool
constructInstType ctx _ load@(InstLoad v) =
  case _constructValueType ctx v of
    IRTypePointer ty -> ty
    otherty -> error . docToString $
      pretty "unable to construct type for load: " <+> pretty load

constructInstType ctx _ (InstStore _ _) = IRTypeVoid
constructInstType ctx _ call@(InstCall fn _) =
  case _constructValueType ctx fn of
    IRTypeFunction _ retty -> retty
    otherty -> error . docToString $
      pretty "unabke to construct type for function call: " <+> pretty call


-- | Set the current function in the context
setFunctionInContext ::  Function -> Context -> Context
setFunctionInContext f ctx = ctx {
  currentFunction=Just f,
  instNameToType=instNameToType
} where
   -- | Basic blocks in the function.
    bbs :: [BasicBlock]
    bbs = M.elems . functionBBMap $ f

    -- | List of instructions in the function.
    namedInsts :: [Named Inst]
    namedInsts = bbs >>= bbInsts

    -- | given a named inst, try to create a map between a label inst and an inst
    mkNamedInstMap :: Named Inst -> M.OrderedMap (Label Inst) Inst
    mkNamedInstMap (UnNamed _) = mempty
    mkNamedInstMap (Named name inst) = M.fromList [(name, inst)]

    -- | map instruction name to the instruction
    instNameToInst :: M.OrderedMap (Label Inst) Inst
    instNameToInst = mconcat . map mkNamedInstMap $ namedInsts

    -- | map instruction name to type
    instNameToType :: M.OrderedMap (Label Inst) IRType
    instNameToType = fmap (constructInstType ctx instNameToInst) instNameToInst

-- | Get the parameter type from the parameter name
getParamTypeFromContext :: Context -> Label Param -> IRType
getParamTypeFromContext Context{
  currentFunction=Just (curfn@Function {
    functionParamLabels=paramLabels,
    functionType=(paramTypes, _)
  })
 } lbl = case findIndex (== lbl) paramLabels of
      Just i -> paramTypes !! i
      Nothing ->
         error . docToString $
          vcat [pretty "NO param name: ",
                pretty lbl, pretty "in function: ",
                pretty curfn]


bsToStr :: B.ByteString -> String
bsToStr = map (chr . fromEnum) . B.unpack

-- | Convert a 'String' to a 'ShortByteString'
strToShort :: String -> B.ShortByteString
strToShort = B.toShort . C8.pack

-- | Convert a 'String' to a 'AST.Name'
strToName :: String -> L.Name
strToName = L.Name . strToShort

-- | Convert a `IR.Label` to a `AST.Name`
labelToName :: Label a -> L.Name
labelToName = strToName . unLabel

-- | Convert an int to an integer
intToInteger :: Int -> Integer
intToInteger = fromIntegral

intToWord32 :: Int -> W.Word32
intToWord32 = fromIntegral


intToWord :: Int -> W.Word
intToWord = fromIntegral

-- | Convert a `IRType` to a LLVM Type.
irToLLVMType :: IRType -> L.Type
irToLLVMType (IRTypeInt i) = L.IntegerType . intToWord32 $ i
irToLLVMType (IRTypeVoid) = L.VoidType
irToLLVMType (IRTypePointer ty) = L.ptr (irToLLVMType ty)
irToLLVMType (IRTypeFunction params ret) =
  -- False for varargs.
  L.FunctionType (irToLLVMType ret) (map irToLLVMType params) False
irToLLVMType (IRTypeStruct fields) =
  L.StructureType isPacked (map irToLLVMType fields) where
    isPacked = False


-- | Convert a `RetInst` to a `Terminator`
_materializeRetInst :: Context -> RetInst -> L.Terminator
_materializeRetInst ctx r = error . docToString $
  pretty "unimplemented _materializeRetInst: " <+> pretty r

_constructValueType :: Context -> Value -> IRType
_constructValueType ctx (ValueConstInt _) = irTypeInt32
_constructValueType ctx (ValueInstRef name) = (instNameToType ctx) M.! name
_constructValueType ctx (ValueParamRef name) =  getParamTypeFromContext ctx name
_constructValueType ctx (ValueFnPointer fnname) = (functionNameToType ctx) M.! fnname
_constructValueType ctx  (ValueGlobalRef name) =  (globalNameToType ctx) M.! name

-- | Get Name of the reference  where  Value is a reference value.
_getValueReferenceName :: Value -> L.Name
_getValueReferenceName (ValueInstRef name) = (labelToName name)
_getValueReferenceName (ValueParamRef name) = (labelToName name)
_getValueReferenceName (ValueFnPointer fnname) = (labelToName fnname)
_getValueReferenceName (ValueGlobalRef name) = (labelToName name)
_getValueReferenceName v = error . docToString $
  pretty "value is not a reference:" <+> pretty v

-- | Materialize a Value into a Operand
_materializeValueToOperand ctx v =
  case v of
    ValueConstInt _ -> L.ConstantOperand $ _materializeValueToConstant ctx v
    reference -> L.LocalReference (irToLLVMType (_constructValueType  ctx v)) (_getValueReferenceName $ v)

-- | Materialize a Value to a Constant.
-- | Note: this is partial.
_materializeValueToConstant :: Context -> Value -> LC.Constant
_materializeValueToConstant _ (ValueConstInt i) = LC.Int (intToWord32 32) (intToInteger i)
_materializeValueToConstant _ v = error . docToString $
  pretty "unable to materialize value to constant: " <+> pretty v

-- | make an `Operand` into a `CallableOperand`
_makeOperandCallable :: L.Operand -> L.CallableOperand
_makeOperandCallable = Right

-- | Materialize an IR instruction
_materializeInst :: Context -> Inst -> L.Instruction
_materializeInst ctx (InstAdd v1 v2) = L.Add {
  L.nsw=False,
  L.nuw=False,
  L.operand0=_materializeValueToOperand ctx v1,
  L.operand1=_materializeValueToOperand ctx v2,
  L.metadata=[]
}


_materializeInst ctx (InstCall fnname fnparams) = L.Call {
  L.tailCallKind=Nothing,
  L.callingConvention= L.C,
  L.returnAttributes=[],
  L.function=_makeOperandCallable $ _materializeValueToOperand ctx fnname,
  L.arguments=args,
  L.functionAttributes=[],
  L.metadata=[]
} where
  paramAttribs :: [[L.ParameterAttribute]]
  paramAttribs = repeat []

  args :: [(L.Operand, [L.ParameterAttribute])]
  args = zip (map (_materializeValueToOperand ctx) fnparams) paramAttribs

_materializeInst ctx inst =  error . docToString $
  pretty "unable to materialize Inst: " <+> pretty inst

-- | Materialize a `Named a` by using `f` into a `L.Named b`
-- | We choose to not create the more obvious version without the `a -> b`
-- | since it is uncommon for us to use only a `Named a` in a module.
_materializeNamed :: (a -> b) -> Named a -> L.Named b
_materializeNamed f (Named labelName a) = (strToName . unLabel $ labelName) L.:=  (f a)
_materializeNamed f (UnNamed a) = L.Do (f a)

-- | Materialize a BasicBlock given the name and the basic block
materializeBB :: Context -> Label BasicBlock -> BasicBlock -> L.BasicBlock
materializeBB ctx bblabel bb@BasicBlock{..} = let
  name ::  L.Name
  name = labelToName bblabel

  insts :: [L.Named L.Instruction]
  insts = map (_materializeNamed (_materializeInst ctx)) bbInsts

  terminator :: L.Named L.Terminator
  terminator = (_materializeNamed (_materializeRetInst ctx)) (UnNamed bbRetInst)
  in L.BasicBlock name insts terminator

_materializeFunctionParams :: Function -> [L.Parameter]
_materializeFunctionParams Function{..} = let
    paramtys :: [L.Type]
    paramtys = map irToLLVMType (fst functionType)

    paramNames :: [L.Name]
    paramNames = map labelToName functionParamLabels

    attribs :: [L.ParameterAttribute]
    attribs = []
  in
    zipWith (\n t -> L.Parameter t n attribs) paramNames paramtys

_materializeFunction :: Context -> Function -> L.Definition
_materializeFunction ctx f = L.GlobalDefinition (L.functionDefaults {
  L.name=labelToName . functionLabel $ f,
  L.returnType=retty,
  -- False = vararg

  L.parameters=(_materializeFunctionParams f, False),
  L.basicBlocks =  mapToList (materializeBB fnctx) (functionBBMap f)
}) where
    retty :: L.Type
    retty = irToLLVMType . snd . functionType $ f

    -- | The updated context that points to the function.
    fnctx :: Context
    fnctx = setFunctionInContext f ctx


-- | Materialize a GlobalValue given its name. It may or may not
-- | contain a value.
_materializeGlobal :: Context -> GlobalLabel -> GlobalValue -> L.Definition
_materializeGlobal ctx label GlobalValue{ gvType=ty, gvValue=mVal} =
  L.GlobalDefinition (L.globalVariableDefaults {
    L.name=labelToName label,
    L.initializer=fmap (_materializeValueToConstant ctx) mVal,
    LLVM.AST.Global.type'=irToLLVMType ty
  })

_irmoduleToDefinitions :: IR.Module -> [L.Definition]
_irmoduleToDefinitions mod@Module {moduleFunctions=fs,
                            moduleGlobals=globalNameToVal} =
    (map (_materializeFunction ctx)fs) -- ++ mapToList (_materializeGlobal ctx) globalNameToVal
   where
    ctx = _buildContextForModule mod


type IRString = String
moduleToLLVMIRString :: IR.Module -> IO IRString
moduleToLLVMIRString irmod = let
  -- Module from llvm-hs-pure
  pureLLVMMod = _definitionsToModule . _irmoduleToDefinitions $ irmod
  in trace
  (show pureLLVMMod)
  (RealL.withContext $ \llvmCtx ->
      RealL.withModuleFromAST llvmCtx pureLLVMMod $ \llvmMod ->
        bsToStr <$> RealL.moduleLLVMAssembly llvmMod)

-- | Create a new module
_definitionsToModule :: [L.Definition] ->  L.Module
_definitionsToModule defs = L.Module {
      L.moduleName=B.toShort . C8.pack  $ "simplexhc",
      L.moduleSourceFileName=B.toShort . C8.pack $ "simplexhc-thinair",
      L.moduleDataLayout=Nothing,
      L.moduleTargetTriple=Nothing,
      L.moduleDefinitions=defs
}

