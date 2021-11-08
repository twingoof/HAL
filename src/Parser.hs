--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Parser
--

module Parser where
import Control.Applicative
import Data.Char

data Type =
    Number Integer |
    Boolean Bool |
    String String |
    List [Type] |
    Pair [Type] Type |
    Atom String
    deriving Show

newtype Error = Error String
    deriving Show

type Data a = String -> Either (a, String) Error

newtype Parser a = Parser {
    parse :: Data a
}

functorParser :: (a -> b) -> Parser a -> Data b
functorParser _ _ [] = Right (Error [])
functorParser fct parser str = case parse parser str of
    Left (a, res) -> Left (fct a, str)
    Right err -> Right err

instance Functor Parser where
    fmap fct parser =
        Parser (functorParser fct parser)

applicativeParser :: Parser (a -> b) -> Parser a -> Data b
applicativeParser _ _ [] = Right (Error [])
applicativeParser fct parser str = case parse parser str of
    Right err -> Right err
    Left (a, res) -> case parse fct res of
        Right err -> Right err
        Left (b, final) -> Left (b a, final)

funcParserBind :: Parser a -> (a -> Parser b) -> Data b
funcParserBind parser fct str = case parse parser str of
        Right err -> Right err
        Left (a, res) -> parse (fct a) res

parserBind :: Parser a -> (a -> Parser b) -> Parser b
parserBind parser fct = Parser (funcParserBind parser fct)

instance Applicative Parser where
    pure a = Parser (\x -> Left (a, x))
    (<*>) fct parser = Parser (applicativeParser fct parser)
    p1 *> p2 = p1 `parserBind` const p2

monadParser :: Parser a -> (a -> Parser b) -> Data b
monadParser _ _ [] = Right (Error [])
monadParser parser fct str = case parse parser str of
    Right err -> Right err
    Left (a, res) -> case parse (fct a) res of
        Right err -> Right err
        final@(Left b) -> final

instance Monad Parser where
    (>>=) parser fct = Parser (monadParser parser fct)
    (>>) = (*>)

alternativeParser :: Parser a -> Parser a -> Data a
alternativeParser _ _ [] = Right (Error [])
alternativeParser p1 p2 str
    | Right (Error err1) <- parse p1 str
    , Right (Error err2) <- parse p2 str =
        Right (Error (err1 ++ err2))
    | Right err <- parse p1 str =
        parse p2 str
    | otherwise = parse p1 str

funcMany :: Parser a -> Data [a]
funcMany parser str = case parse parser str of
    Left (a, str) -> case funcMany parser str of
        Left (b, final) -> Left (a : b, final)
        Right err -> Left ([a], str)
    Right err -> Right err

instance Alternative Parser where
    empty = Parser (Right . Error)
    (<|>) f1 f2 = Parser (alternativeParser f1 f2)
    many parser = Parser (funcMany parser)

funcOneOf :: [Char] -> Data Char
funcOneOf _ [] = Right (Error [])
funcOneOf arr str@(x:xs)
    | x `elem` arr = Left (x, xs)
    | otherwise = Right (Error str)

oneOf :: [Char] -> Parser Char
oneOf arr = Parser (funcOneOf arr)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

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

spaces :: Parser ()
spaces = skipMany ' '

funcChar :: Char -> Data Char
funcChar _ [] = Right (Error [])
funcChar c str
    | head str == c = Left (c, tail str)
    | otherwise = Right (Error str)

char :: Char -> Parser Char
char c = Parser (funcChar c)

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

readExpr :: String -> String
readExpr [] = []
readExpr input = case parse (spaces >> symbol) input of
    Left x -> "Found value"
    Right (Error err) -> "No match " ++ err