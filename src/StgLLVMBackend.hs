module StgLLVMBackend(IRString, getIRString) where
import StgLanguage
import ColorUtils


import qualified LLVM.AST as AST
import LLVM.AST (Named(..))
import qualified LLVM.AST.Global as G
import qualified LLVM.CodeModel as CodeModel
import qualified LLVM.Module as Module
import LLVM.AST.Constant
import LLVM.AST.Type
import LLVM.AST.Constant (Constant)
import LLVM.AST.AddrSpace
import LLVM.AST.Instruction (Named, Instruction(Mul), Terminator)
import LLVM.AST.Instruction(Instruction(..))
import LLVM.AST.Linkage
import LLVM.AST.Visibility
import LLVM.AST.DLL
import LLVM.AST.CallingConvention as CC
import LLVM.AST.ThreadLocalStorage
import LLVM.AST.Attribute
import LLVM.Target
import LLVM.Context
import qualified Data.ByteString.Char8 as C8 (pack)
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as B
         (ShortByteString, toShort, fromShort)
import Data.Char (chr)


import qualified Data.Map.Strict as M


import Control.Monad.Except

type IRString = String

bsToStr :: B.ByteString -> String
bsToStr = map (chr . fromEnum) . B.unpack

-- | Convert a 'String' to a 'ShortByteString'
strToShort :: String -> B.ShortByteString
strToShort = B.toShort . C8.pack

-- | Convert a 'String' to a 'AST.Name'
strToName :: String -> AST.Name
strToName = AST.Name . strToShort


-- | Construct the LLVM IR string corresponding to the program
getIRString :: Program -> IO IRString
getIRString program = 
  bsToStr <$> (withContext $
    \context ->
       (Module.withModuleFromAST context mod (Module.moduleLLVMAssembly)))
      where
        mod = mkModule (mkSTGDefinitions program builder)
        builder = mkBuilder program

-- | Create a new module
mkModule :: [AST.Definition] ->  AST.Module
mkModule defs = AST.Module {
      AST.moduleName=B.toShort . C8.pack  $ "simplexhc",
      AST.moduleSourceFileName=B.toShort . C8.pack $ "simplexhc-thinair",
      AST.moduleDataLayout=Nothing,
      AST.moduleTargetTriple=Nothing,
      AST.moduleDefinitions=defs
  }

-- mkBoxedThunk

-- arg stack
-- return stack
-- global code
-- heap

-- eval
-- enter

i32ty :: Type
i32ty = IntegerType 32

type BindingId = Int
-- | Builder that maintains context of what we're doing when constructing IR.
data Builder = Builder {
  bindings :: [Binding]
} deriving(Show)

mkBuilder :: Program -> Builder
mkBuilder binds = Builder {
  bindings = binds >>= collectBindingsInBinding
}

mkSwitchFunction :: Builder -> AST.Definition
mkSwitchFunction bs = AST.GlobalDefinition (G.functionDefaults {
  G.name = strToName "mainSwitch",
  G.returnType = AST.VoidType,
  G.parameters = ([], False),
  G.basicBlocks = []
})

continuationType :: Type
continuationType = undefined

-- | Create the main "switching" function.
mkSTGDefinitions :: Program -> Builder -> [AST.Definition]
mkSTGDefinitions p builder = [mkSwitchFunction builder]

-- | Tag a value
data ValueTag = ValueTagInt | ValueTagFloat deriving(Show, Enum, Bounded)

-- | Convert a 'ValueTag' to 'Int' for LLVM codegen
valueTagToInt :: ValueTag -> Int
valueTagToInt = fromEnum


