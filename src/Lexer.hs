--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Lexer
--

module Lexer where
import Types
import Control.Monad.Except
import LispError
import Unpack
import Data.List
import Lists
import Environment
import Debug.Trace

eval :: Env -> Value -> ThrowsError (Env, Value)
eval env val@(String _) = Right (env, val)
eval env val@(Number _) = Right (env, val)
eval env val@(Boolean _) = Right (env, val)
eval env (Atom name) = getVar name env
eval env (List [Atom "quote", val]) = Right (env, val)
eval env (List [Atom "define", def@(Atom var), form])
    | Right (envv, val) <- eval env form = Right (setVar var val envv, def)
    | left <- eval env form = left
eval env cond@(List [Atom "if", pred, conseq, alt])
    | Right (envv, Boolean False) <- eval env pred = eval envv alt
    | Right (envv, Boolean True) <- eval env pred = eval envv conseq
    | otherwise = throwError $ BadSpecialForm "Unrecognized special form" cond
eval env (List (Atom func : args))
    | Right list <- mapM (eval env) args
    , Right val <- apply func $ map snd list = Right (env, val)
    | Right list <- mapM (eval env) args
    , Left err <- apply func $ map snd list = Left err
    | Left err <- mapM (eval env) args = Left err
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [Value] -> ThrowsError Value
apply func args = maybe
    (throwError $ NotFunction "Unrecognized builtin function args" func)
    ($ args)
    (lookup func builtins)

numBuiltins :: [(String, [Value] -> ThrowsError Value)]
numBuiltins = [
        ("+", numInfBinop (+) 0),
        ("-", numInfBinop (-) 0),
        ("*", numInfBinop (*) 1),
        ("div", numBinop div),
        ("mod", numBinop mod),
        ("quotient", numBinop quot),
        ("remainder", numBinop rem),
        ("=", numBoolBinop (==)),
        (">", numBoolBinop (>)),
        ("<", numBoolBinop (<)),
        (">=", numBoolBinop (>=)),
        ("<=", numBoolBinop (<=)),
        ("/=", numBoolBinop (/=))
    ]

boolBuiltins :: [(String, [Value] -> ThrowsError Value)]
boolBuiltins = [
        ("&&", boolBoolBinop (&&)),
        ("||", boolBoolBinop (||))
    ]

strBuiltins :: [(String, [Value] -> ThrowsError Value)]
strBuiltins = [
        ("string=?", strBoolBinop (==)),
        ("string>?", strBoolBinop (>)),
        ("string<?", strBoolBinop (<)),
        ("string>=?", strBoolBinop (>=)),
        ("string<=?", strBoolBinop (<=))
    ]

builtins :: [(String, [Value] -> ThrowsError Value)]
builtins = numBuiltins ++ boolBuiltins ++ strBuiltins ++ listBuiltins

numInfBinop :: (Integer -> Integer -> Integer) -> Integer -> [Value] -> ThrowsError Value
numInfBinop _ _ [] = throwError $ NumArgs 1 []
numInfBinop op def [x] = mapM unpackNum [Number def, x] >>= Right . Number . foldl1' op
numInfBinop op _ params = mapM unpackNum params >>= Right . Number . foldl1' op

numBinop :: (Integer -> Integer -> Integer) -> [Value] -> ThrowsError Value
numBinop _ [] = throwError $ NumArgs 2 []
numBinop op params@[_,_] = mapM unpackNum params >>= Right . Number . foldl1' op
numBinop _ params = throwError $ NumArgs 2 params

numBoolBinop :: (Integer -> Integer -> Bool) -> [Value] -> ThrowsError Value
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [Value] -> ThrowsError Value
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [Value] -> ThrowsError Value
boolBoolBinop = boolBinop unpackBool

boolBinop :: (Value -> ThrowsError a) -> (a -> a -> Bool) -> [Value] -> ThrowsError Value
boolBinop unpack op [x]
    | Right _ <- unpack x = Right $ Boolean True
    | Left err <- unpack x = Left err
boolBinop unpack op [x,y]
    | Right a <- unpack x
    , Right b <- unpack y = Right $ Boolean $ op a b
    | Left a <- unpack x = Left a
    | Left b <- unpack y = Left b
boolBinop unpack op params = throwError $ NumArgs 2 params