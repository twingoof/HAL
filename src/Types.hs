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
import Debug.Trace

data Value =
    Number Integer |
    Boolean Bool |
    String String |
    List [Value] |
    Pair [Value] Value |
    Atom String
    deriving (Eq)

instance Show Value where show = showVal

funcParseString :: Data Value
funcParseString [] = Right (Error [])
funcParseString str
    | Left (c, str) <- parse (char '"') str
    , Left (x, str) <- parse (many (noneOf "\"")) str
    , Left (c, str) <- parse (char '"') str =
        Left (String x, str)
    | otherwise = Right (Error str)

parseString :: Parser Value
parseString = Parser funcParseString

getAtom :: String -> Value
getAtom "#t" = Boolean True
getAtom "#f" = Boolean False
getAtom str = Atom str

funcParseAtom :: Data Value
funcParseAtom [] = Right (Error [])
funcParseAtom str
    | Left (c, str) <- parse (letter <|> symbol) str
    , Left (rest, str) <- parse (many (letter <|> digit <|> symbol)) str =
        Left (getAtom (c : rest), str)
    | Left (c, str) <- parse (letter <|> symbol) str =
        Left (Atom [c], str)
    | otherwise = Right (Error str)

parseAtom :: Parser Value
parseAtom = Parser funcParseAtom

funcParseNumber :: Data Value
funcParseNumber [] = Right (Error [])
funcParseNumber str
    | Left (n, str) <- parse (many digit) str =
        Left (Number (read n :: Integer), str)
    | otherwise = Right (Error str)

parseNumber :: Parser Value
parseNumber = Parser funcParseNumber

funcParseQuoted :: Data Value
funcParseQuoted [] = Right (Error [])
funcParseQuoted str
    | Left (c, str) <- parse (char '\'') str
    , Left (x, str) <- parse parseExpr str =
        Left (List [Atom "quote", x], str)
    | otherwise = Right (Error str)

parseQuoted :: Parser Value
parseQuoted = Parser funcParseQuoted

funcParseList :: Data Value
funcParseList [] = Right (Error [])
funcParseList str
    | Left (a, str) <- parse (sepBy parseExpr spaces) str =
        Left (List a, str)
    | otherwise = Right (Error str)

parseList :: Parser Value
parseList = Parser funcParseList

funcParsePair :: Data Value
funcParsePair str
    | Left (head, str) <- parse (endBy parseExpr spaces) str
    , Left (tail, str) <- parse (char '.' >> spaces >> parseExpr) str =
        Left (Pair head tail, str)
    | otherwise = Right (Error str)

parsePair :: Parser Value
parsePair = Parser funcParsePair

funcParseParens :: Data Value
funcParseParens [] = Right (Error [])
funcParseParens str
    | Left (c, str) <- parse (char '(') str
    , Left (x, str) <- parse (parseList <|> parsePair) str
    , Left (c, str) <- parse (char ')') str =
        Left (x, str)
    | otherwise = Right (Error str)

parseParens :: Parser Value
parseParens = Parser funcParseParens

parseExpr :: Parser Value
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> parseParens

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