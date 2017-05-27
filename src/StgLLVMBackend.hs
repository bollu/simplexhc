module StgLLVMBackend(IRString, getStgString) where
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
import qualified LoweringFFI as L


-- implement serialisation routines for things like enum tags, etc.
import StgMachine


import Control.Monad.Except

type IRString = String

bsToStr :: B.ByteString -> String
bsToStr = map (chr . fromEnum) . B.unpack

strToShort :: String -> B.ShortByteString
strToShort = B.toShort . C8.pack

strToName :: String -> AST.Name
strToName = AST.Name . strToShort

{-
getStgString :: Program -> IO IRString
getStgString _ = L.mkModule >>= L.getModuleString
-}

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

-- mkBoxedThunk

-- arg stack
-- return stack
-- global code
-- heap

-- eval
-- enter

i32ty :: Type
i32ty = IntegerType 32

data Code = CodeEval | CodeEnter deriving(Show, Enum)

stgGlobalCode :: AST.Definition
stgGlobalCode = AST.GlobalDefinition (G.globalVariableDefaults {
    G.name=strToName "gCode",
    G.type'=i32ty,
    G.isConstant=False
})

continuationType :: Type
continuationType = undefined

stgPrelude :: [AST.Definition]
stgPrelude = [stgGlobalCode]

mkSTGDefinitions :: Program -> [AST.Definition]
mkSTGDefinitions p = stgPrelude ++ []

-- |Tag a value
data ValueTag = ValueTagInt | ValueTagFloat deriving(Show)

-- |Convert a value tag to an integer
valueTagToInt :: ValueTag -> Int
valueTagToInt (ValueTagInt) = 0
valueTagToInt (ValueTagFloat) = 1


