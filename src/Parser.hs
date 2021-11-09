--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Parser
--

module Parser where
import Control.Applicative
import Data.Char

newtype Error = Error String
    deriving (Show, Eq)

type Data a = String -> Either (a, String) Error

newtype Parser a = Parser {
    parse :: Data a
}

functorParser :: (a -> b) -> Parser a -> Data b
functorParser _ _ [] = Right (Error [])
functorParser fct parser str
    | Left (a, res) <- parse parser str =
        Left (fct a, str)
    | otherwise = Right (Error str)

instance Functor Parser where
    fmap fct parser =
        Parser (functorParser fct parser)

applicativeParser :: Parser (a -> b) -> Parser a -> Data b
applicativeParser _ _ [] = Right (Error [])
applicativeParser fct parser str
    | Left (a, res) <- parse parser str
    , Left (b, final) <- parse fct res =
        Left (b a, final)
    | otherwise = Right (Error str)

funcParserBind :: Parser a -> (a -> Parser b) -> Data b
funcParserBind _ _ [] = Right (Error [])
funcParserBind parser fct str
    | Left (a, res) <- parse parser str =
        parse (fct a) res
    | otherwise = Right (Error str)

parserBind :: Parser a -> (a -> Parser b) -> Parser b
parserBind parser fct = Parser (funcParserBind parser fct)

instance Applicative Parser where
    pure a = Parser (\x -> Left (a, x))
    (<*>) fct parser = Parser (applicativeParser fct parser)
    p1 *> p2 = p1 `parserBind` const p2

monadParser :: Parser a -> (a -> Parser b) -> Data b
monadParser _ _ [] = Right (Error [])
monadParser parser fct str
    | Left (a, res) <- parse parser str
    , final@(Left b) <- parse (fct a) res =
        final
    | otherwise = Right (Error str)

instance Monad Parser where
    (>>=) parser fct = Parser (monadParser parser fct)
    (>>) = (*>)

alternativeParser :: Parser a -> Parser a -> Data a
alternativeParser _ _ [] = Right (Error [])
alternativeParser p1 p2 str
    | Right _ <- parse p1 str
    , Right (Error err) <- parse p2 str =
        Right (Error err)
    | Right err <- parse p1 str =
        parse p2 str
    | otherwise = parse p1 str

funcMany :: Parser a -> Data [a]
funcMany parser [] = Left ([], [])
funcMany parser str
    | Left (a, str) <- parse parser str
    , Left (b, final) <- funcMany parser str =
        Left (a : b, final)
    | Left (a, str) <- parse parser str =
        Left ([a], str)
    | otherwise = Right (Error str)

instance Alternative Parser where
    empty = Parser (Right . Error)
    (<|>) f1 f2 = Parser (alternativeParser f1 f2)
    many parser = Parser (funcMany parser)
