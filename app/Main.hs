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

-- v intercalate
import Data.List


repl :: InputT IO ()
repl = do 
    line <- getInputLine ">"
    case line of
        Nothing -> return ()
        Just ("exit") -> return ()
        Just ("quit") -> return ()
        Just(line) -> do
                        let tokens = tokenize line
                        lift . print $ tokens
                        let parsed = tokens >>= parseStg 
                        lift . print $ parsed
                        let compiled = case parsed of 
                                           Left err -> Nothing
                                           Right program ->  Just (compileProgram program)
                        let trace = (fmap . fmap) genMachineTrace compiled
                        lift . print $ trace
                        repl

runFile :: String -> IO ()
runFile fpath = do
    raw <- Prelude.readFile fpath
    let parsed = (tokenize >=> parseStg) raw
    print parsed
    let compiled = case parsed of 
                       Left err -> Nothing
                       Right program ->  Just (compileProgram program)

    putStrLn "trace:\n======\n"
    let trace = (fmap . fmap) genMachineTrace compiled
    case trace of
          Nothing -> return ()
          Just (Left compileErr) -> do  
                                      putStrLn "compile error: "
                                      putStrLn . show $ compileErr
          Just (Right (trace, mErr)) -> let 
                                      errStr = case mErr of
                                                Nothing -> "Success"
                                                Just err -> show err
                                      traceStr = intercalate "\n\n=====\n\n" (fmap show trace) 
                                    in
                                      putStrLn $ traceStr ++ "\n\n\nFinal:\n======\n" ++ errStr
main :: IO ()
main = do
    args <- getArgs
    if null args
        then runInputT defaultSettings repl
        else runFile (head args)
