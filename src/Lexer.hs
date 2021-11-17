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

eval :: Value -> ThrowsError Value
eval val@(String _) = Right val
eval val@(Number _) = Right val
eval val@(Boolean _) = Right val
eval cond@(List [Atom "if", pred, conseq, alt])
    | Right (Boolean False) <- eval pred = eval alt
    | Right (Boolean True) <- eval pred = eval conseq
    | otherwise = throwError $ BadSpecialForm "Unrecognized special form" cond
eval cond@(List [Atom "atom?", expr])
    | Right (Atom _) <- eval expr = Right (Boolean True)
    | Right (List []) <- eval expr = Right (Boolean True)
    | Right _ <- eval expr = Right (Boolean False)
    | otherwise = throwError $ BadSpecialForm "Unrecognized special form" cond
eval (List [Atom "quote", val]) = Right val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

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

arithBuiltins :: [(String, [Value] -> ThrowsError Value)]
arithBuiltins = [
        ("eq?", eq)
    ]

builtins :: [(String, [Value] -> ThrowsError Value)]
builtins = numBuiltins ++ boolBuiltins ++ strBuiltins ++ listBuiltins ++ arithBuiltins

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
    | Left x <- unpack x = Left x
boolBinop unpack op [x,y]
    | Right x <- unpack x
    , Right y <- unpack y = Right $ Boolean $ op x y
    | Left x <- unpack x = Left x
    | Left y <- unpack y = Left y
boolBinop unpack op params = throwError $ NumArgs 2 params

eq :: [Value] -> ThrowsError Value
eq [Boolean arg1, Boolean arg2] = return $ Boolean $ arg1 == arg2
eq [Number arg1, Number arg2] = return $ Boolean $ arg1 == arg2
eq [String arg1, String arg2] = return $ Boolean $ arg1 == arg2
eq [Atom arg1, Atom arg2] = return $ Boolean $ arg1 == arg2
eq [Pair x xs, Pair y ys] = eq [List $ x ++ [xs], List $ y ++ [ys]]
eq [List arg1, List arg2]
    | null arg1 && null arg2 = return $ Boolean True
    | otherwise = return $ Boolean False
eq [_, _] = return $ Boolean False
eq badArgList = throwError $ NumArgs 2 badArgList