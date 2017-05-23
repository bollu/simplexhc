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
import qualified Data.ByteString.Char8 as C8 (pack)
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as B
         (ShortByteString, toShort, fromShort)
import Data.Char (chr)


-- implement serialisation routines for things like enum tags, etc.
import StgMachine


import Control.Monad.Except

type IRString = String

bsToStr :: B.ByteString -> String
bsToStr = map (chr . fromEnum) . B.unpack

getStgString :: Program -> IO IRString
getStgString program = 
  bsToStr <$> (withContext $ 
    \context -> 
       (Module.withModuleFromAST context mod (Module.moduleLLVMAssembly)))
      where
        mod = mkModule . mkSTGDefinitions $ program

mkModule :: [AST.Definition] ->  AST.Module
mkModule defs = AST.Module {
      AST.moduleName=B.toShort . C8.pack  $ "simplexhc",
      AST.moduleSourceFileName=B.toShort . C8.pack $ "simplexhc-thinair",
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

