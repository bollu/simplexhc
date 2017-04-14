import Test.Tasty
-- import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

mkBoxedNumber :: Int -> Expr
mkBoxedNumber i = 

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [interpTests]


interpTests = testGroup "interpreter tests" [testSKK3]

testSKK3 = testCase "test S K K 3" $ [1, 2, 3] `compare` [1,2] @?= GT 

