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


parseStgExpr :: String -> Maybe ExprNode
parseStgExpr str =  case (tokenize >=> parseExpr) $ str of 
                      Left e -> Nothing
                      Right expr -> Just expr

mkBoxedNumber :: Int -> Maybe ExprNode
mkBoxedNumber i = parseStgExpr $ "{} \n {} -> " ++ "#" ++ (show i) ++ " {}"

extractBoxedNumber :: MachineState -> Maybe Int
extractBoxedNumber state = (state ^. code ^? _CodeEval) >>= getFnApplication where
  -- Eval (x {})  ({ x -> #3 })
  getFnApplication :: (ExprNode, LocalEnvironment) -> Maybe Int
  getFnApplication ((ExprNodeFnApplication varname []), localEnv) = (varname `M.lookup` localEnv) >>= (^? _ValuePrimInt) 
  getFnApplication _ = Nothing

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [interpTests]


interpTests = testGroup "interpreter tests" [testSKK3]

testSKK3 = testCase "test S K K 3" $ [1, 2, 3] `compare` [1,2] @?= GT 

