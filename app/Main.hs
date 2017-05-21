{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import StgLanguage
import StgParser
import StgMachine

import System.IO
import System.Environment
import System.Console.Haskeline
import Control.Monad.Trans.Class
import Control.Lens
import Control.Exception
import Control.Monad

import qualified Text.Megaparsec as P

import Data.List
type ErrorString = String

stringifyMegaparsecError :: (Ord a, P.ShowToken a) => Either (P.ParseError a P.Dec) b -> Either ErrorString b
stringifyMegaparsecError e = case e of 
                               Left err -> Left (P.parseErrorPretty err)
                               Right a -> Right a 

compileString :: String -> Either ErrorString (Either StgError MachineState)
compileString str = let
    mTokens :: Either ErrorString [Token]
    mTokens = stringifyMegaparsecError (tokenize str)
    mParsed :: Either ErrorString Program 
    mParsed = mTokens >>= stringifyMegaparsecError . parseStg
  in
    compileProgram <$> mParsed


tryCompileString :: String -> Either ErrorString MachineState
tryCompileString str = 
    case compileString str of
      (Left parseErr) -> Left $ "pre-compile error:\n" ++ parseErr
      (Right (Left compileErr)) -> Left $ "compile error:\n" ++ show compileErr
      (Right (Right initState)) -> Right initState


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
