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
import Control.Monad.Except
import LispError

readExpr :: String -> ThrowsError Value
readExpr [] = throwError Empty
readExpr input
    | Right (x, []) <- parse (spaces >> parseExpr) input =
        Right x
    | Left err <- parse (spaces >> parseExpr) input =
        throwError $ Parsing err
    | otherwise = throwError $ Parsing $ Error input

prompt :: IO String
prompt = putStr "> " >> hFlush stdout >> getLine

interactive :: IO ()
interactive = do
    line <- prompt
    let evaled = fmap show $ readExpr line >>= eval
    putStrLn $ extractValue $ trapError evaled
    interactive

hal :: [String] -> IO ()
hal [] = interactive
hal args = pure ()