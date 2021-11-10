--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Hal
--

module Hal where

import Errors
import Control.Exception
import Debug.Trace
import GHC.IO.Handle
import GHC.IO.Handle.FD
import Parser
import Basic
import Types
import System.Posix.Internals (lstat)
import Lexer

readExpr :: String -> Value
readExpr [] = List []
readExpr input = case parse (spaces >> parseExpr) input of
    Left (x, str) -> x
    Right (Error err) -> String $ "No match: " ++ show err

prompt :: IO String
prompt = do
    putStr "> "
    hFlush stdout
    getLine

interactive :: IO ()
interactive = do
    line <- prompt
    print (eval (readExpr line))
    interactive

hal :: [String] -> IO ()
hal [] = interactive
hal args = pure ()