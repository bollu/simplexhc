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


-- main = defaultMain tests


stgProgramsResource :: IO [(FilePath, String)]
stgProgramsResource = do
      programFiles <- listDirectory "./stg-programs/"
      forM programFiles $ \f -> do
                                contents <- readFile $ "./stg-programs/" ++ f
                                return (f, contents)

mkTestFromFileData :: FilePath -> String -> TestTree
mkTestFromFileData filepath contents = testCase ("running " ++ filepath) $ do
  let mState = doesStgProgramSucceedRun contents
  case mState of
    Left e -> do
                assertFailure $  "error: " ++ e
    Right _ -> return ()

main :: IO ()
main = do
  filesWithContents <- stgProgramsResource
  let tests = fmap (uncurry mkTestFromFileData) filesWithContents
  let group = testGroup "example programs" tests
  defaultMain group


doesStgProgramSucceedRun :: String -> Either ErrorString PushEnterMachineState
doesStgProgramSucceedRun s = 
  case tryCompileString s of
    Left err -> Left err
    Right init -> case genMachineTrace init of
                    (states, Nothing) -> Right (last states)
                    (_, Just err) -> Left (show err)

