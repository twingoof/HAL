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

data Type =
    Number Integer |
    Boolean Bool |
    String String |
    List [Type] |
    Pair [Type] Type |
    Atom String
    deriving (Show, Eq)

funcParseString :: Data Type
funcParseString [] = Right (Error [])
funcParseString str = case parse (char '"') str of
    Right err -> Right err
    Left (c, str) -> case parse (many (noneOf "\"")) str of
        Right err -> Right err
        Left (x, str) -> case parse (char '"') str of
            Right err -> Right err
            Left (c, str) -> Left (String x, str)

parseString :: Parser Type
parseString = Parser funcParseString

funcParseAtom :: Data Type
funcParseAtom [] = Right (Error [])
funcParseAtom str = case parse (letter <|> symbol) str of
    Right err -> Right err
    Left (c, str) -> case parse (many (letter <|> digit <|> symbol)) str of
        Right err -> Right err
        Left (rest, str) -> case c : rest of
            "#t" -> Left (Boolean True, str)
            "#f" -> Left (Boolean False, str)
            _ -> Left (Atom (c : rest), str)

parseAtom :: Parser Type
parseAtom = Parser funcParseAtom

funcParseNumber :: Data Type
funcParseNumber [] = Right (Error [])
funcParseNumber str = case parse (many digit) str of
    Right err -> Right err
    Left (n, str) -> Left (Number (read n :: Integer), str)

parseNumber :: Parser Type
parseNumber = Parser funcParseNumber

funcParseQuoted :: Data Type
funcParseQuoted [] = Right (Error [])
funcParseQuoted str = case parse (char '\'') str of
    Right err -> Right err
    Left (c, str) -> case parse parseExpr str of
        Right err -> Right err
        Left (x, str) -> Left (List [Atom "quote", x], str)

parseQuoted :: Parser Type
parseQuoted = Parser funcParseQuoted

funcParseList :: Data Type
funcParseList [] = Right (Error [])
funcParseList str = case parse (sepBy parseExpr spaces) str of
    Right err -> Right err
    Left (a, str) -> Left (List a, str)

parseList :: Parser Type
parseList = Parser funcParseList

funcParsePair :: Data Type
funcParsePair str = Right (Error [])

parsePair :: Parser Type
parsePair = Parser funcParsePair

funcParseParens :: Data Type
funcParseParens [] = Right (Error [])
funcParseParens str = case parse (char '(') str of
    Right err -> Right err
    Left (c, str) -> case parse (parseList <|> parsePair) str of
        Right err -> Right err
        Left (x, str) -> case parse (char ')') str of
            Right err -> Right err
            Left (c, str) -> Left (x, str)

parseParens :: Parser Type
parseParens = Parser funcParseParens

parseExpr :: Parser Type
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> parseParens