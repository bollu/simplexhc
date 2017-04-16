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


import Control.Monad.Except

-- writeStgToFile :: Program -> FilePath -> IO ()
-- writeStgToFile program path = Module.writeLLVMAssemblyToFile mod (Module.File path) where
--  mod = mkModule . mkSTGDefinitions $ prog

type IRString = String
getStgString :: Program -> IO (Either String IRString)
getStgString program = withContext $ \context -> runExceptT (Module.withModuleFromAST context mod Module.moduleLLVMAssembly) where
  mod = mkModule . mkSTGDefinitions $ program

{-

mkDefinitions :: Free Lang () -> [AST.Definition]
mkDefinitions lang = [AST.GlobalDefinition (G.Function {
                    G.linkage=External,
                    G.visibility=Default,
                    G.dllStorageClass=Nothing,
                    G.callingConvention=CC.C,
                    G.returnAttributes=[],
                    G.functionAttributes=[],
                    G.section= Nothing,
                    G.comdat= Nothing,
                    G.alignment=0,
                    G.garbageCollectorName=Nothing,
                    G.prefix=Nothing,
                    G.personalityFunction=Nothing,
                    -- interesting stuff
                    G.returnType=i64,
                    G.name=AST.Name("main"),
                    G.basicBlocks=[mkBB lang],
                    G.parameters=([], False)
                  })]

-}

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
