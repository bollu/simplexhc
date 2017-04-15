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
import Text.ParserCombinators.Parsec


runStgProgStr :: String -> Either ParseError (Either StgError MachineState)
runStgProgStr str = runProgram <$> (parseStgStr str) where
                  parseStgStr :: String -> Either ParseError Program
                  parseStgStr = tokenize >=> parseStg

                  runProgram :: Program -> Either StgError MachineState
                  runProgram = compileProgram >=> genFinalMachineState

assertFromStgString :: String -> (MachineState -> Assertion) -> Assertion
assertFromStgString str f = case runStgProgStr str of
                              Left parseErr -> assertFailure ("parse error: " ++ show parseErr)
                              Right (Left stgErr) -> assertFailure ("runtime error: " ++ show stgErr)
                              Right (Right state) -> f state



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
tests = testGroup "Tests" [scaffoldingTests, interpTests]

scaffoldingTests = testGroup "Test scaffolding tests" [testBoxExtractRawNumber]

testBoxExtractRawNumber = testCase 
                          "Unwrap & wrap a boxed int" $ 
                            assertFromStgString progStr (\ms -> 1 @?= 1)
  where
    progStr = ("define main = {} \\n {} -> " ++ mkBoxedNumberString 3 )

interpTests = testGroup "interpreter tests" [testSKK3]

testSKK3 = testCase "test S K K 3" $ [1, 2, 3] `compare` [1,2] @?= GT 

