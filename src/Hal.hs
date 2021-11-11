--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Hal
--

module Hal where

import GHC.IO.Handle
import GHC.IO.Handle.FD
import Parser
import Basic
import Types
import Lexer
import Control.Monad.Except
import LispError
import Errors
import Control.Exception

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
    putStrLn (extractValue $ trapError False evaled) >> interactive

catchRead :: IOError -> IO String
catchRead _ = throw InvalidPath

evalLines :: [String] -> [String]
evalLines [] = [""]
evalLines (x:xs) = do
    let evaled = fmap show $ readExpr x >>= eval
    extractValue (trapError True evaled) : evalLines xs

evalFile :: String -> IO String
evalFile path = do
    input <- lines <$> readFile path `catch` catchRead
    return $ last $ filter (not . null) $ evalLines $ filter (not . null) input

readFiles :: [String] -> IO [String]
readFiles [] = pure [""]
readFiles (x:xs) = do
    evaled <- evalFile x
    recurse <- readFiles xs
    return $ evaled : recurse

hal :: Bool -> [String] -> IO ()
hal _ [] = interactive
hal False args = do
    result <- readFiles args
    putStrLn $ last $ filter (not . null) result
hal True args = readFiles args >> interactive