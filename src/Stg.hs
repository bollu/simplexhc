module Stg where

import StgLanguage
import StgParser
import StgPushEnterMachine
import qualified Text.Megaparsec as P
-- import StgLLVMBackend

type ErrorString = String

stringifyMegaparsecError :: (Ord a, P.ShowToken a) => Either (P.ParseError a P.Dec) b -> Either ErrorString b
stringifyMegaparsecError e = case e of 
                               Left err -> Left (P.parseErrorPretty err)
                               Right a -> Right a 

squashFrontendErrors :: Either ErrorString (Either StgError a) -> Either ErrorString a
squashFrontendErrors val = 
    case val of
      (Left parseErr) -> Left $ "pre-compile error:\n" ++ parseErr
      (Right (Left compileErr)) -> Left $ "compile error:\n" ++ show compileErr
      (Right (Right a)) -> Right a

parseString :: String -> Either ErrorString Program
parseString str = let
    mTokens :: Either ErrorString [Token]
    mTokens = stringifyMegaparsecError (tokenize str)
    mParsed :: Either ErrorString Program
    mParsed = mTokens >>= stringifyMegaparsecError . parseStg
  in
    mParsed

tryCompileString :: String -> Either ErrorString PushEnterMachineState
tryCompileString str =  squashFrontendErrors $ compileProgram <$> parseString str

