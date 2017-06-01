import Test.Tasty
import Control.Monad
-- import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Ord
import Data.Map as M

import Control.Lens
import Data.Map.Lens

import StgLanguage
import StgParser
import StgMachine
import Stg



mkBoxedNumberString :: Int -> String
mkBoxedNumberString i = "Int { " ++  (show i) ++ "#" ++ "  }"

extractBoxedNumber :: MachineState -> Maybe Int
extractBoxedNumber state = (state ^. code ^? _CodeEval) >>= getFnApplication where
  -- Eval (x {})  ({ x -> #3 })
  getFnApplication :: (ExprNode, LocalEnvironment) -> Maybe Int
  getFnApplication ((ExprNodeFnApplication varname []), localEnv) = (varname `M.lookup` localEnv) >>= (^? _ValuePrimInt) 
  getFnApplication _ = Nothing


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [runSamplesTest]

runSamplesTest = testCase "running samples..." $ do
    return ()


doesStgProgramSucceedRun :: String -> Either ErrorString MachineState
doesStgProgramSucceedRun s = 
  case tryCompileString s of
    Left err -> Left err
    Right init -> case genMachineTrace init of
                    (states, Nothing) -> Right (last states)
                    (_, Just err) -> Left (show err)

