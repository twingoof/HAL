--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Unpack
--

module Unpack where
import Types
import LispError
import Control.Monad.Except

unpackNum :: Value -> ThrowsError Integer 
unpackNum (Number n) = Right n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                            if null parsed
                                then throwError $ TypeMismatch "Number" $ String n
                                else Right $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum not = throwError $ TypeMismatch "Number" not

unpackStr :: Value -> ThrowsError String
unpackStr (String s) = Right s
unpackStr (Number n) = Right $ show n
unpackStr (Boolean b) = Right $ show b
unpackStr not = throwError $ TypeMismatch "string" not

unpackBool :: Value -> ThrowsError Bool
unpackBool (Boolean b) = Right b
unpackBool not = throwError $ TypeMismatch "boolean" not