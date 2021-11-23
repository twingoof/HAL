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
funcOneOf _ [] = Left (Error [])
funcOneOf arr str@(x:xs)
    | x `elem` arr = Right (x, xs)
    | otherwise = Left (Error str)

oneOf :: [Char] -> Parser Char
oneOf arr = Parser (funcOneOf arr)

funcSkipMany :: Char -> Data ()
funcSkipMany _ [] = Right ((), [])
funcSkipMany c str
    | head str == c = funcSkipMany c $ tail str
    | otherwise = Right ((), str)

skipMany :: Char -> Parser ()
skipMany c = Parser (funcSkipMany c)

funcNoneOf :: [Char] -> Data Char
funcNoneOf _ [] = Left (Error [])
funcNoneOf arr str@(x:xs)
    | x `notElem` arr = Right (x, xs)
    | otherwise = Left (Error str)

noneOf :: [Char] -> Parser Char
noneOf arr = Parser (funcNoneOf arr)

funcChar :: Char -> Data Char
funcChar _ [] = Left (Error [])
funcChar c str
    | head str == c = Right (c, tail str)
    | otherwise = Left (Error str)

char :: Char -> Parser Char
char c = Parser (funcChar c)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany ' '

funcDigit :: Data Char
funcDigit [] = Left (Error [])
funcDigit str
    | isDigit $ head str = Right (head str, tail str)
    | otherwise = Left (Error str)

digit :: Parser Char
digit = Parser funcDigit

funcLetter :: Data Char
funcLetter [] = Left (Error [])
funcLetter str
    | isAlpha $ head str = Right (head str, tail str)
    | otherwise = Left (Error str)

letter :: Parser Char
letter = Parser funcLetter

funcSepBy :: Parser a -> Parser b -> Data [a]
funcSepBy _ _ [] = Left (Error [])
funcSepBy p1 p2 str
    | Right (a, x) <- parse p1 str
    , Right (b, y) <- parse (many (p2 >> p1)) x =
        Right (a:b, y)
    | Right (a, x) <- parse p1 str =
        Right ([a], x)
    | otherwise = Left (Error str)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p1 p2 = Parser (funcSepBy p1 p2)

funcEndBy :: Parser a -> Parser b -> Data a
funcEndBy _ _ [] = Left (Error [])
funcEndBy p1 p2 str
    | Right (a, x) <- parse p1 str
    , Right (_, y) <- parse p2 x =
        Right (a, y)
    | otherwise = Left (Error str)

endBy :: Parser a -> Parser b -> Parser [a]
endBy p1 p2 = many $ Parser (funcEndBy p1 p2)