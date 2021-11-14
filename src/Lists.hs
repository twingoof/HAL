--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Lists
--

module Lists where
import Types
import LispError
import Control.Monad.Except

car :: [Value] -> ThrowsError Value
car [List (x:_)] = Right x
car [Pair (x:_) _] = Right x
car [bad] = throwError $ TypeMismatch "Pair" bad
car bad = throwError $ NumArgs 1 bad

cdr :: [Value] -> ThrowsError Value
cdr [List (_:xs)] = Right $ List xs
cdr [Pair [_] y] = Right y
cdr [Pair (_:xs) y] = Right $ Pair xs y
cdr [bad] = throwError $ TypeMismatch "Pair" bad
cdr bad = throwError $ NumArgs 1 bad

cons :: [Value] -> ThrowsError Value
cons [x, List []] = Right $ List [x]
cons [x, List xs] = Right $ List $ x : xs
cons [x, Pair y ys] = Right $ Pair (x : y) ys
cons [x,y] = Right $ Pair [x] y
cons bad = throwError $ NumArgs 2 bad