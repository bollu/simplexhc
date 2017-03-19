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
import Control.Exception
import Control.Monad


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
-- runFile :: String -> IO ()
-- runFile fpath = do
--     mraw <- (try $ Prelude.readFile fpath :: IO (Either SomeException String))
--     case mraw of 
--         Left err -> print err
--         Right raw -> (tokenize >=> parse) raw
--     return ()


runFile :: String -> IO ()
runFile fpath = do
    raw <- Prelude.readFile fpath
    let parsed = (tokenize >=> parseStg) raw
    print parsed

main :: IO ()
main = do
    args <- getArgs
    if null args
        then runInputT defaultSettings repl
        else runFile (head args)