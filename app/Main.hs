{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import StgLanguage
import StgParser

import System.IO
import System.Environment
import System.Console.Haskeline
import Control.Monad.Trans.Class
import Control.Lens


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
                        repl

main :: IO ()
main = do
    args <- getArgs
    print args
    runInputT defaultSettings repl
