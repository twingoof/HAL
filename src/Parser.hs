--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Parser
--

module Parser where
import Control.Applicative
import Control.Monad.Except
import Control.Exception

newtype ParserError = Error String

type Data a = String -> Either ParserError (a, String)

newtype Parser a = Parser {
    parse :: Data a
}

functorParser :: (a -> b) -> Parser a -> Data b
functorParser _ _ [] = Left (Error [])
functorParser fct parser str
    | Right (a, res) <- parse parser str =
        Right (fct a, str)
    | otherwise = Left (Error str)

instance Functor Parser where
    fmap fct parser =
        Parser (functorParser fct parser)

applicativeParser :: Parser (a -> b) -> Parser a -> Data b
applicativeParser _ _ [] = Left (Error [])
applicativeParser fct parser str
    | Right (a, res) <- parse parser str
    , Right (b, final) <- parse fct res =
        Right (b a, final)
    | otherwise = Left (Error str)

funcParserBind :: Parser a -> (a -> Parser b) -> Data b
funcParserBind _ _ [] = Left (Error [])
funcParserBind parser fct str
    | Right (a, res) <- parse parser str =
        parse (fct a) res
    | otherwise = Left (Error str)

parserBind :: Parser a -> (a -> Parser b) -> Parser b
parserBind parser fct = Parser (funcParserBind parser fct)

instance Applicative Parser where
    pure a = Parser (\x -> Right (a, x))
    (<*>) fct parser = Parser (applicativeParser fct parser)
    p1 *> p2 = p1 `parserBind` const p2

monadParser :: Parser a -> (a -> Parser b) -> Data b
monadParser _ _ [] = Left (Error [])
monadParser parser fct str
    | Right (a, res) <- parse parser str
    , final@(Right b) <- parse (fct a) res =
        final
    | otherwise = Left (Error str)

instance Monad Parser where
    (>>=) parser fct = Parser (monadParser parser fct)
    (>>) = (*>)

alternativeParser :: Parser a -> Parser a -> Data a
alternativeParser _ _ [] = Left (Error [])
alternativeParser p1 p2 str
    | Left _ <- parse p1 str
    , Left (Error err) <- parse p2 str =
        Left (Error err)
    | Left err <- parse p1 str =
        parse p2 str
    | otherwise = parse p1 str

funcMany :: Parser a -> Data [a]
funcMany parser [] = Right ([], [])
funcMany parser str
    | Right (a, str) <- parse parser str
    , Right (b, final) <- funcMany parser str =
        Right (a : b, final)
    | Right (a, str) <- parse parser str =
        Right ([a], str)
    | otherwise = Left (Error str)

instance Alternative Parser where
    empty = Parser (Left . Error)
    (<|>) f1 f2 = Parser (alternativeParser f1 f2)
    many parser = Parser (funcMany parser)
