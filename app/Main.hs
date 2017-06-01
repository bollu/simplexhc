{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import StgLanguage
import StgParser
import StgMachine
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

getTraceString :: ([MachineState], Maybe StgError) -> String
getTraceString (trace, mErr) = 
  traceStr ++ "\n\n\nFinal:\n======\n" ++ errStr where
  errStr = case mErr of
            Nothing -> "Success"
            Just err -> show err
  traceStr = intercalate "\n\n=====\n\n" (fmap show trace) 

runFile :: String -> IO ()
runFile fpath = do
    raw <- Prelude.readFile fpath
    putStrLn "Interp:\n=====\n"
    let mInitState = tryCompileString raw
    
    let trace = fmap genMachineTrace mInitState
    case trace of
          (Left compileErr) -> do  
                                      putStrLn "compile error: "
                                      putStrLn  $ compileErr
          (Right trace) -> do
            putStr . getTraceString $ trace
            
main :: IO ()
main = do
    args <- getArgs
    if null args
        then runInputT defaultSettings repl
        else runFile (head args)
