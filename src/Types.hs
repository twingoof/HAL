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

data Type =
    Number Integer |
    Boolean Bool |
    String String |
    List [Type] |
    Pair [Type] Type |
    Atom String
    deriving Show

funcParseString :: Data Type
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
