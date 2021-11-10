--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Lexer
--

module Lexer where
import Types
import Data.List (unwords)
import Foreign.C (isValidErrno)


unwordList :: [Value] -> String
unwordList = unwords . map showVal

showVal :: Value -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Boolean True) = "#t"
showVal (Boolean False) = "#f"
showVal (List contents) = "(" ++ unwordList contents ++ ")"
showVal (Pair head tail) = "(" ++ unwordList head ++ "." ++ showVal tail ++ ")"

eval :: Value -> Value 
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Boolean _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [Value] -> Value
apply func args = maybe (Boolean False) ($ args) $ lookup func primitives

primitives :: [(String, [Value] -> Value)]
primitives = [("+", numBinop (+)),
            ("-", numBinop (-)),
            ("*", numBinop (*)),
            ("/", numBinop div),
            ("mod", numBinop mod),
            ("quotient", numBinop quot),
            ("remainder", numBinop rem)]

numBinop :: (Integer -> Integer -> Integer) -> [Value] -> Value
numBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: Value -> Integer 
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                            if null parsed
                                then 0
                                else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0