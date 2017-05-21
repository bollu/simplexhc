module StgLLVMBackend where
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

-- implement serialisation routines for things like enum tags, etc.
import StgMachine


import Control.Monad.Except

type IRString = String

getStgString :: Program -> IO (Either String IRString)
getStgString program = withContext $ \context -> runExceptT (Module.withModuleFromAST context mod Module.moduleLLVMAssembly) where
  mod = mkModule . mkSTGDefinitions $ program

mkModule :: [AST.Definition] ->  AST.Module
mkModule defs = AST.Module {
      AST.moduleName="simplexhc",
      AST.moduleSourceFileName="simplexhc-thinair",
      AST.moduleDataLayout=Nothing,
      AST.moduleTargetTriple=Nothing,
      AST.moduleDefinitions=defs
  }


mkSTGDefinitions :: Program -> [AST.Definition]
mkSTGDefinitions p = []


-- |Tag a value
data ValueTag = ValueTagInt | ValueTagFloat deriving(Show)

-- |Convert a value tag to an integer
valueTagToInt :: ValueTag -> Int
valueTagToInt (ValueTagInt) = 0
valueTagToInt (ValueTagFloat) = 1

