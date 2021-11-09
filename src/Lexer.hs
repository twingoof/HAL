--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Lexer
--

module Lexer where
import Types
import Data.List (unwords)


showVal :: Value -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Boolean True) = "#t"
showVal (Boolean False) = "#f"
showVal (List contents) = "(" ++ unwordList contents ++ ")"
showVal (Pair head tail) = "(" ++ unwordList head ++ "." ++ showVal tail ++ ")"

unwordList :: [Value] -> String
unwordList = unwords . map showVal