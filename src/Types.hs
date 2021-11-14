--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Types
--

module Types where

import Parser
import Basic
import Control.Applicative

data Value =
    Number Integer | Boolean Bool | String String |
    List [Value] | Pair [Value] Value | Atom String
    deriving (Eq)

instance Show Value where show = showVal

showVal :: Value -> String
showVal (String str) = "\"" ++ str ++ "\""
showVal (Atom name) = name
showVal (Number num) = show num
showVal (Boolean True) = "#t"
showVal (Boolean False) = "#f"
showVal (List list) = "(" ++ unwordList list ++ ")"
showVal (Pair head tail) = "(" ++ unwordList head ++ " . " ++ showVal tail ++ ")"

unwordList :: [Value] -> String
unwordList = unwords . map showVal

funcParseString :: Data Value
funcParseString [] = Left (Error [])
funcParseString str
    | Right (c, str) <- parse (char '"') str
    , Right (x, str) <- parse (many (noneOf "\"")) str
    , Right (c, str) <- parse (char '"') str =
        Right (String x, str)
    | otherwise = Left (Error str)

parseString :: Parser Value
parseString = Parser funcParseString

getAtom :: String -> Value
getAtom "#t" = Boolean True
getAtom "#f" = Boolean False
getAtom str = Atom str

funcParseAtom :: Data Value
funcParseAtom [] = Left (Error [])
funcParseAtom str
    | Right (c, str) <- parse (letter <|> symbol) str
    , Right (rest, str) <- parse (many (letter <|> digit <|> symbol)) str =
        Right (getAtom (c : rest), str)
    | Right (c, str) <- parse (letter <|> symbol) str =
        Right (Atom [c], str)
    | otherwise = Left (Error str)

parseAtom :: Parser Value
parseAtom = Parser funcParseAtom

funcParseNumber :: Data Value
funcParseNumber [] = Left (Error [])
funcParseNumber str
    | Right (n, str) <- parse (many digit) str =
        Right (Number (read n :: Integer), str)
    | otherwise = Left (Error str)

parseNumber :: Parser Value
parseNumber = Parser funcParseNumber

funcParseQuoted :: Data Value
funcParseQuoted [] = Left (Error [])
funcParseQuoted str
    | Right (c, str) <- parse (char '\'') str
    , Right (x, str) <- parse parseExpr str =
        Right (List [Atom "quote", x], str)
    | otherwise = Left (Error str)

parseQuoted :: Parser Value
parseQuoted = Parser funcParseQuoted

funcParseList :: Data Value
funcParseList [] = Left (Error [])
funcParseList str@(')':xs) = Right (List [], str)
funcParseList str
    | Right (a, str) <- parse (sepBy parseExpr spaces) str =
        Right (List a, str)
    | otherwise = Left (Error str)

parseList :: Parser Value
parseList = Parser funcParseList

funcParsePair :: Data Value
funcParsePair str
    | Right (head, str) <- parse (endBy parseExpr spaces) str
    , Right (tail, str) <- parse (char '.' >> spaces >> parseExpr) str =
        Right (Pair head tail, str)
    | otherwise = Left (Error str)

parsePair :: Parser Value
parsePair = Parser funcParsePair

funcParseParens :: Data Value
funcParseParens [] = Left (Error [])
funcParseParens str
    | Right (c, str) <- parse (char '(') str
    , Right (x, str) <- parse (parsePair <|> parseList) str
    , Right (c, str) <- parse (char ')') str =
        Right (x, str)
    | otherwise = Left (Error str)

parseParens :: Parser Value
parseParens = Parser funcParseParens

parseExpr :: Parser Value
parseExpr = parseAtom <|> parseString <|> parseNumber
    <|> parseQuoted <|> parseParens