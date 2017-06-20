module Stg where

import StgLanguage
import StgParser
import StgPushEnterMachine
import Text.Trifecta as TR
-- note that this is a trifecta dependency
import qualified Text.PrettyPrint.ANSI.Leijen as PP
-- import StgLLVMBackend
--

type ErrorString = String

squashFrontendErrors :: Either ErrorString (Either StgError a) -> Either ErrorString a
squashFrontendErrors val = 
    case val of
      (Left parseErr) -> Left $ "pre-compile error:\n" ++ parseErr
      (Right (Left compileErr)) -> Left $ "compile error:\n" ++ show compileErr
      (Right (Right a)) -> Right a

parseString :: String -> Either ErrorString Program
parseString str = case parseStg str of
                      Success a -> Right a
                      Failure ErrInfo{ _errDoc = e } -> Left (PP.displayS (PP.renderPretty 0.8 80 e) "")

tryCompileString :: String -> Either ErrorString PushEnterMachineState
tryCompileString str =  squashFrontendErrors $ compileProgram <$> Stg.parseString str

