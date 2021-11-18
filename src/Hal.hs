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
import Environment

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

printAndExec :: Env -> Either Error (Env, Value) -> IO ()
printAndExec _ (Right (env, val)) = print val >> interactive env
printAndExec env evaled =
    putStrLn (extractValue $ trapError False $ fmap (show . snd) evaled) >> interactive env

interactive :: Env -> IO ()
interactive env = do
    line <- prompt
    let evaled = readExpr line >>= eval env
    printAndExec env evaled

catchRead :: IOError -> IO String
catchRead _ = throw InvalidPath

extract :: Env -> Either Error (Env, Value) -> (Env, String)
extract _ (Right (env, result)) = (env, show result)
extract env x = (env, extractValue $ trapError True $ fmap show x)

execFiles :: Env -> [String] -> (Env, [String])
execFiles env [] = (env, [])
execFiles env (x:xs) = do
    let evaled = readExpr x >>= eval env
    let (envv, val) = extract env evaled
    let (envvv, res) = execFiles envv xs
    (envvv, val : res)

openFiles :: Env -> [String] -> IO (Env, [String])
openFiles env [] = pure (env, [""])
openFiles env (x:xs) = do
    input <- lines <$> readFile x `catch` catchRead
    let (envv, result) = execFiles env $ filter (not . null) input
    (envvv, recurse) <- openFiles envv xs
    pure (envvv, result ++ recurse)

hal :: Bool -> [String] -> IO ()
hal _ [] = interactive emptyEnv
hal False args = do
    (env, result) <- openFiles emptyEnv args
    let filtered = filter (not . null) result
    if null filtered
        then pure ()
        else putStrLn $ last filtered
hal True args = do
    (env, _) <- openFiles emptyEnv args
    interactive env