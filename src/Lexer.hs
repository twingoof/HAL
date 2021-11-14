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

numPrimitives :: [(String, [Value] -> ThrowsError Value)]
numPrimitives = [
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

boolPrimitives :: [(String, [Value] -> ThrowsError Value)]
boolPrimitives = [
        ("&&", boolBoolBinop (&&)),
        ("||", boolBoolBinop (||))
    ]

strPrimitives :: [(String, [Value] -> ThrowsError Value)]
strPrimitives = [
        ("string=?", strBoolBinop (==)),
        ("string>?", strBoolBinop (>)),
        ("string<?", strBoolBinop (<)),
        ("string>=?", strBoolBinop (>=)),
        ("string<=?", strBoolBinop (<=))
    ]

primitives :: [(String, [Value] -> ThrowsError Value)]
primitives = numPrimitives ++ boolPrimitives ++ strPrimitives

eval :: Value -> ThrowsError Value
eval val@(String _) = Right val
eval val@(Number _) = Right val
eval val@(Boolean _) = Right val
eval (List [Atom "quote", val]) = Right val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [Value] -> ThrowsError Value
apply func args = maybe
    (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    (lookup func primitives)

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