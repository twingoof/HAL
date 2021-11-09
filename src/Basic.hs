--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Basic
--

module Basic where
import Parser
import Data.Char
import Control.Applicative

funcOneOf :: [Char] -> Data Char
funcOneOf _ [] = Right (Error [])
funcOneOf arr str@(x:xs)
    | x `elem` arr = Left (x, xs)
    | otherwise = Right (Error str)

oneOf :: [Char] -> Parser Char
oneOf arr = Parser (funcOneOf arr)

funcSkipMany :: Char -> Data ()
funcSkipMany _ [] = Right (Error [])
funcSkipMany c str
    | head str == c = funcSkipMany c $ tail str
    | otherwise = Left ((), str)

skipMany :: Char -> Parser ()
skipMany c = Parser (funcSkipMany c)

funcNoneOf :: [Char] -> Data Char
funcNoneOf _ [] = Right (Error [])
funcNoneOf arr str@(x:xs)
    | x `notElem` arr = Left (x, xs)
    | otherwise = Right (Error str)

noneOf :: [Char] -> Parser Char
noneOf arr = Parser (funcNoneOf arr)

funcChar :: Char -> Data Char
funcChar _ [] = Right (Error [])
funcChar c str
    | head str == c = Left (c, tail str)
    | otherwise = Right (Error str)

char :: Char -> Parser Char
char c = Parser (funcChar c)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany ' '

funcDigit :: Data Char
funcDigit [] = Right (Error [])
funcDigit str
    | isDigit $ head str = Left (head str, tail str)
    | otherwise = Right (Error str)

digit :: Parser Char
digit = Parser funcDigit

funcLetter :: Data Char
funcLetter [] = Right (Error [])
funcLetter str
    | isAlpha $ head str = Left (head str, tail str)
    | otherwise = Right (Error str)

letter :: Parser Char
letter = Parser funcLetter

funcSepBy :: Parser a -> Parser b -> Data [a]
funcSepBy _ _ [] = Right (Error [])
funcSepBy p1 p2 str = case parse p1 str of
    Right err -> Right err
    Left (a, str) -> case parse (many (p2 >> p1)) str of
        Right err -> Left ([a], str)
        Left (b, str) -> Left (a:b, str)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p1 p2 = Parser (funcSepBy p1 p2)

funcEndBy :: Parser a -> Parser b -> Data a
funcEndBy _ _ [] = Right (Error [])
funcEndBy p1 p2 str = case parse p1 str of
    Right err -> Right err
    Left (a, str) -> case parse p2 str of
        Right err -> Right err
        Left (_, str) -> Left (a, str)

endBy :: Parser a -> Parser b -> Parser [a]
endBy p1 p2 = many (Parser (funcEndBy p1 p2))