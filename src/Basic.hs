--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Basic
--

module Basic where
import Parser
import Data.Char

funcOneOf :: [Char] -> Data Char
funcOneOf _ [] = Right (Error [])
funcOneOf arr str@(x:xs)
    | x `elem` arr = Left (x, xs)
    | otherwise = Right (Error str)

oneOf :: [Char] -> Parser Char
oneOf arr = Parser (funcOneOf arr)

funcSkipMany :: Char -> Data ()
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
funcDigit str
    | isDigit $ head str = Left (head str, tail str)
    | otherwise = Right (Error str)

digit :: Parser Char
digit = Parser funcDigit

funcLetter :: Data Char
funcLetter str
    | isAlpha $ head str = Left (head str, tail str)
    | otherwise = Right (Error str)

letter :: Parser Char
letter = Parser funcLetter