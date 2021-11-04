--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Parser
--

module Parser where
import Text.ParserCombinators.ReadP (satisfy)

data Type =
    Number Integer |
    Boolean Bool |
    String String |
    List [Type] |
    Pair [Type] Type |
    Atom String

type Data a = String -> Maybe a

newtype Parser a = Parser {
    eval :: Data a
}

funcParseType :: Data Type
funcParseType _ = Nothing

parseType :: Parser Type
parseType = Parser funcParseType