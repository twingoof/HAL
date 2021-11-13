--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Lexer
--

module Lexer where
import Types
import Data.List (unwords)
import Control.Exception
import Control.Monad.Except
import LispError

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

primitives :: [(String, [Value] -> ThrowsError Value)]
primitives = [
        ("+", numInfBinop (+) 0),
        ("-", numInfBinop (-) 0),
        ("*", numInfBinop (*) 1),
        ("div", numBinop div),
        ("mod", numBinop mod),
        ("quotient", numBinop quot),
        ("remainder", numBinop rem)
    ]

numInfBinop :: (Integer -> Integer -> Integer) -> Integer -> [Value] -> ThrowsError Value
numInfBinop _ _ [] = throwError $ NumArgs 1 []
numInfBinop op def [x] = mapM unpackNum [Number def, x] >>= Right . Number . foldl1 op
numInfBinop op _ params = mapM unpackNum params >>= Right . Number . foldl1 op

numBinop :: (Integer -> Integer -> Integer) -> [Value] -> ThrowsError Value
numBinop _ [] = throwError $ NumArgs 2 []
numBinop op params@[x,xs] = mapM unpackNum params >>= Right . Number . foldl1 op
numBinop _ params = throwError $ NumArgs 2 params

unpackNum :: Value -> ThrowsError Integer 
unpackNum (Number n) = Right n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                            if null parsed
                                then throwError $ TypeMismatch "Number" $ String n
                                else Right $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum not = throwError $ TypeMismatch "Number" not