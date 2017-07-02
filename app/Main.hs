{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import StgLanguage
import StgParser
import StgPushEnterMachine
import StgLLVMBackend
import Stg
-- import StgLLVMBackend

import System.IO
import System.Environment
import System.Console.Haskeline
import Control.Monad.Trans.Class
import Control.Lens
import Control.Exception
import Control.Monad
import Data.List
import Data.Monoid


import Options.Applicative

data CommandLineOptions = CommandLineOptions {
  emitLLVM :: Bool,
  filepath :: String
}

filepathOpt :: Parser (String)
filepathOpt = strOption (long "file" <> short 'f' <> metavar "FILEPATH")

emitLLVMOpt :: Parser Bool
emitLLVMOpt = switch (long "emit-llvm")

commandLineOptionsParser :: Parser CommandLineOptions
commandLineOptionsParser = CommandLineOptions <$> emitLLVMOpt <*> filepathOpt

commandLineOptionsParserInfo :: ParserInfo CommandLineOptions
commandLineOptionsParserInfo = info commandLineOptionsParser infomod where
    infomod = fullDesc <> progDesc "STG -> LLVM compiler" <> header "simplexhc"

repl :: InputT IO ()
repl = do 
    lift . putStrLn $ "\n"
    line <- getInputLine ">"
    case line of
      Nothing -> repl
      Just (l) ->  do
        lift . compileAndRun $ l
        repl

  where
    compileAndRun :: String -> IO ()
    compileAndRun line = do

      putStrLn "interp: "
      let mInitState = tryCompileString line
      let mTrace = fmap genMachineTrace mInitState
      case mTrace of
          (Left err) -> putStrLn err
          (Right trace) -> putStr . getTraceString $ trace

getTraceString :: ([PushEnterMachineState], Maybe StgError) -> String
getTraceString (trace, mErr) = 
  traceStr ++ "\n\n\nFinal:\n==================================\n" ++ errStr where
  errStr = case mErr of
            Nothing -> "Success"
            Just err -> show err ++ machineFinalStateLogStr 
  traceStr = intercalate "\n\n==================================\n\n" (fmap show trace) 
  machineFinalStateLogStr = if length trace == 0 then "" else "\nlog:\n====\n" ++ show ((last trace) ^. currentLog)

runFileInterp :: String -> IO ()
runFileInterp fpath = do
    raw <- Prelude.readFile fpath
    let mInitState = tryCompileString raw
    let trace = fmap genMachineTrace mInitState
    case trace of
          (Left compileErr) -> do  
                                      putStrLn "compile error: "
                                      putStrLn  $ compileErr
          (Right trace) -> putStr . getTraceString $ trace

runFileLLVM :: String -> IO ()
runFileLLVM fpath = do
    raw <- Prelude.readFile fpath
    let mParse = parseString raw
    case mParse of
        (Left compileErr) -> do
                              putStrLn "compile error: "
                              putStrLn  $ compileErr
        (Right program) -> do
                             putStrLn "LLVM module: "
                             str <- getIRString program
                             putStr  str
main :: IO ()
main = do
    opts <- execParser commandLineOptionsParserInfo
    if filepath opts == ""
        then runInputT defaultSettings repl
        else if emitLLVM opts == False 
        then runFileInterp (filepath opts)
        else runFileLLVM (filepath opts)
