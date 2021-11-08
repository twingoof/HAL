--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Parser
--

module Parser where
import Text.ParserCombinators.ReadP (satisfy)
import Control.Applicative

data Type =
    Number Integer |
    Boolean Bool |
    String String |
    List [Type] |
    Pair [Type] Type |
    Atom String

newtype Error = Error String

type Data a = String -> Either (a, String) Error

newtype Parser a = Parser {
    eval :: Data a
}

functorParser :: (a -> b) -> Parser a -> Data b
functorParser _ _ [] = Right (Error [])
functorParser fct parser str = case eval parser str of 
    Left (a, res) -> Left (fct a, str)
    Right err -> Right err

instance Functor Parser where
    fmap fct parser =
        Parser (functorParser fct parser)

applicativeParser :: Parser (a -> b) -> Parser a -> Data b
applicativeParser _ _ [] = Right (Error [])
applicativeParser fct parser str = case eval parser str of
    Left (a, res) -> case eval fct res of 
        Left (b, res2) -> Left (b a, res2)
        Right err -> Right err
    Right err -> Right err

parserBind :: Parser a -> (a -> Parser b) -> Parser b
parserBind parser fct = Parser (\str -> case eval parser str of
        Left (a, res) -> eval (fct a) res
        Right err -> Right err
    )

instance Applicative Parser where
    pure a = Parser (\x -> Left (a, x))
    (<*>) fct parser =
        Parser (applicativeParser fct parser)
    p1 *> p2 = p1 `parserBind` const p2

monadParser :: Parser a -> (a -> Parser b) -> Data b
monadParser _ _ [] = Right (Error [])
monadParser parser fct str = case eval parser str of
    Left (a, res) -> case eval (fct a) res of
        res2@(Left b) -> res2
        Right err -> Right err
    Right err -> Right err

instance Monad Parser where
    (>>=) parser fct = Parser (monadParser parser fct)
    (>>) = (*>)

funcParseType :: Data Type
funcParseType str = Right (Error str)

parseType :: Parser Type
parseType = Parser funcParseType

funcParseSymbol :: Data Char
funcParseSymbol [] = Right (Error [])
funcParseSymbol str@(x:xs)
    | x `elem` "!#$%&|*+-/:<=>?@^_~" = Left (x, xs)
    | otherwise = Right (Error str)

parseSymbol :: Parser Char
parseSymbol = Parser funcParseSymbol

funcParseSpaces :: Data ()
funcParseSpaces (' ':xs) = funcParseSpaces xs
funcParseSpaces str = Left ((), str)

parseSpaces :: Parser ()
parseSpaces = Parser funcParseSpaces
readExpr :: String -> String
readExpr input = case eval (parseSpaces >> parseSymbol) input of
    Left x -> "Found value"
    Right (Error err) -> "No match " ++ err