import Test.Tasty
import Control.Monad
-- import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.Runners


import Data.Ord
import qualified Data.Map as M

import System.IO


import Control.Lens
import Data.Map.Lens

import StgLanguage
import StgParser
import StgPushEnterMachine
import Stg

import System.Directory
import Data.Either




mkBoxedNumberString :: Int -> String
mkBoxedNumberString i = "Int { " ++  (show i) ++ "#" ++ "  }"

extractBoxedNumber :: PushEnterMachineState -> Maybe Int
extractBoxedNumber state = (state ^. code ^? _CodeEval) >>= getFnApplication where
  -- Eval (x {})  ({ x -> #3 })
  getFnApplication :: (ExprNode, LocalEnvironment) -> Maybe Int
  getFnApplication ((ExprNodeFnApplication varname []), localEnv) = (varname `M.lookup` localEnv) >>= (^? _ValuePrimInt) 
  getFnApplication _ = Nothing


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [runSamplesTest]


{-

stgProgramsResource :: ResourceSpec [(FilePath, String)]
stgProgramsResource = ResourceSpec (acquirer, releaser) where
    acquirer = do
      programFiles <- listDirectory "./stg-programs/"
      forM programFiles (\f -> do 
                                          contents <- readFile $ "./stg-programs/" ++ f
                                          return (f, contents)
                                 )
    
    releaser = const (return ())

createStgProgramTestCase :: (FilePath, String) -> TestTree
createStgProgramTestCase (path, program) = testCase path assertion
    let mState = doesStgProgramSucceedRun contents
    in
    assertion = if isLeft mState 
                then do
                    putStr "\n"
                    putStrLn ("File: " ++ path)
                    putStrLn "Error: "
                    putStr err
                    assertFailure "execution failed."
                else do
                  putrStrLn $ path ++ " ran successfully"


runSamplesTest :: TestTree
runSamplesTest = WithResource stgProgramsResource 

-}

runSamplesTest :: TestTree
runSamplesTest = testCase  "running samples" $ do
    programFiles <- listDirectory "./stg-programs/"
    mRuns <- forM programFiles (\f -> do 
                                        contents <- readFile $ "./stg-programs/" ++ f
                                        let mState = doesStgProgramSucceedRun contents
                                        return (f, mState)) :: IO [(FilePath, Either ErrorString PushEnterMachineState)]
    let mErrors = filter (isLeft . snd)  mRuns

    if length mErrors == 0 then
      return ()
    else do
      forM_ mErrors (\(f, Left err) -> do
          putStr "\n"
          putStrLn ("file: " ++ f)
          putStrLn "error: "
          putStr err)

      assertFailure "program executions failed"



doesStgProgramSucceedRun :: String -> Either ErrorString PushEnterMachineState
doesStgProgramSucceedRun s = 
  case tryCompileString s of
    Left err -> Left err
    Right init -> case genMachineTrace init of
                    (states, Nothing) -> Right (last states)
                    (_, Just err) -> Left (show err)

