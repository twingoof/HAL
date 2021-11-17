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
    | Right (c1, x) <- parse (char '"') str
    , Right (val, y) <- parse (many (noneOf "\"")) x
    , Right (c2, z) <- parse (char '"') y =
        Right (String val, z)
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
    | Right (c, x) <- parse (letter <|> symbol) str
    , Right (rest, y) <- parse (many (letter <|> digit <|> symbol)) x =
        Right (getAtom (c : rest), y)
    | Right (c, x) <- parse (letter <|> symbol) str =
        Right (Atom [c], x)
    | otherwise = Left (Error str)

parseAtom :: Parser Value
parseAtom = Parser funcParseAtom

funcParseNumber :: Data Value
funcParseNumber [] = Left (Error [])
funcParseNumber str
    | Right (n, x) <- parse (many digit) str =
        Right (Number (read n :: Integer), x)
    | otherwise = Left (Error str)

parseNumber :: Parser Value
parseNumber = Parser funcParseNumber

funcParseQuoted :: Data Value
funcParseQuoted [] = Left (Error [])
funcParseQuoted str
    | Right (c, x) <- parse (char '\'') str
    , Right (val, y) <- parse parseExpr x =
        Right (List [Atom "quote", val], y)
    | otherwise = Left (Error str)

parseQuoted :: Parser Value
parseQuoted = Parser funcParseQuoted

funcParseList :: Data Value
funcParseList [] = Left (Error [])
funcParseList str@(')':xs) = Right (List [], str)
funcParseList str
    | Right (a, x) <- parse (sepBy parseExpr spaces) str =
        Right (List a, x)
    | otherwise = Left (Error str)

parseList :: Parser Value
parseList = Parser funcParseList

funcParsePair :: Data Value
funcParsePair str
    | Right (head, x) <- parse (endBy parseExpr spaces) str
    , Right (tail, y) <- parse (char '.' >> spaces >> parseExpr) x =
        Right (Pair head tail, y)
    | otherwise = Left (Error str)

parsePair :: Parser Value
parsePair = Parser funcParsePair

funcParseParens :: Data Value
funcParseParens [] = Left (Error [])
funcParseParens str
    | Right (c1, x) <- parse (char '(') str
    , Right (x, y) <- parse (parsePair <|> parseList) x
    , Right (c2, z) <- parse (char ')') y =
        Right (x, z)
    | otherwise = Left (Error str)

parseParens :: Parser Value
parseParens = Parser funcParseParens

parseExpr :: Parser Value
parseExpr = parseAtom <|> parseString <|> parseNumber
    <|> parseQuoted <|> parseParens