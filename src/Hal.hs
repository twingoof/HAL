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
import Parser

evalExpr :: String -> Maybe String
evalExpr input 
    | Just c <- eval symbol input = Just [c]
    | otherwise = Nothing

printMaybe :: Maybe String -> IO ()
printMaybe (Just str) = putStrLn str
printMaybe Nothing = pure ()

interactive :: IO ()
interactive = do
    line <- getLine
    printMaybe $ evalExpr line
    interactive

hal :: [String] -> IO ()
hal [] = interactive
hal args = pure ()