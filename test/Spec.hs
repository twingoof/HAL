--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Spec
--

import Test.HUnit
import Basic
import Parser
import Control.Applicative
import Types
import Lexer
import LispError
import Errors
import Hal (readExpr)

tests :: Test
tests = TestList [
    --- basic.hs
    TestLabel "oneOf" testOneOf,
    TestLabel "skipMany" testSkipMany,
    TestLabel "noneOf" testNoneOf,
    TestLabel "char" testChar,
    TestLabel "digit" testDigit,
    TestLabel "letter" testLetter,
    TestLabel "sepBy" testSepBy,
    TestLabel "endBy" testEndBy,
    --- types.hs
    TestLabel "parseString" testParseString,
    TestLabel "parseAtom" testParseAtom,
    TestLabel "parseNumber" testParseNumber,
    TestLabel "parseQuoted" testParseQuoted,
    TestLabel "parseList" testParseList,
    TestLabel "parseParens" testParseParens,
    TestLabel "parsePair" testParsePair,
    --- Lexer.hs
    TestLabel "atomBuiltins" testAtomBuiltins
    ]

--- basic.hs

testOneOf :: Test
testOneOf = TestCase (do
        assertEqual "found" (Right ('b', "onjour")) (parse (oneOf "abc") "bonjour")
        assertEqual "Not found" (Left (Error "bonjour")) (parse (oneOf "xyz") "bonjour")
    )

testSkipMany :: Test
testSkipMany = TestCase (do
        assertEqual "skipped" (Right ((), "bonjour")) (parse (skipMany ' ') "   bonjour")
        assertEqual "nothing" (Right ((), "bonjour")) (parse (skipMany ' ') "bonjour")
    )

testNoneOf :: Test
testNoneOf = TestCase (do
        assertEqual "found" (Left (Error "bonjour")) (parse (noneOf "abc") "bonjour")
        assertEqual "Not found" (Right ('b', "onjour")) (parse (noneOf "xyz") "bonjour")
    )

testChar :: Test
testChar = TestCase (do
        assertEqual "equal" (Right ('b', "onjour")) (parse (char 'b') "bonjour")
        assertEqual "Not equal" (Left (Error "bonjour")) (parse (char 'g') "bonjour")
    )

testLetter :: Test
testLetter = TestCase (do
        assertEqual "found" (Right ('b', "onjour")) (parse letter "bonjour")
        assertEqual "Not found" (Left (Error "123")) (parse letter "123")
    )

testDigit :: Test
testDigit = TestCase (do
        assertEqual "found" (Right ('1', "23")) (parse digit "123")
        assertEqual "Not found" (Left (Error "bonjour")) (parse digit "bonjour")
    )

testSepBy :: Test
testSepBy = TestCase (do
        assertEqual "splitted" (Right (["bonjour", "hello", "aurevoir", "bye"], "")) (parse (sepBy (many letter) spaces) "bonjour hello aurevoir bye")
        assertEqual "no split" (Right (["bonjour"], "")) (parse (sepBy (many letter) spaces) "bonjour")
        assertEqual "error split" (Left (Error "bonjour hello aurevoir bye")) (parse (sepBy (many letter) digit) "bonjour hello aurevoir bye")
        assertEqual "error parse" (Left (Error "bonjour hello aurevoir bye")) (parse (sepBy (many digit) letter) "bonjour hello aurevoir bye")
    )

testEndBy :: Test
testEndBy = TestCase (do
        assertEqual "success" (Right (["bonjour","hello","aurevoir"],"bye")) (parse (endBy (many letter) spaces) "bonjour hello aurevoir bye")
        assertEqual "fail" (Left (Error "bonjour")) (parse (endBy (many letter) spaces) "bonjour")
    )

--- types.hs

testParseString :: Test
testParseString = TestCase (do
        assertEqual "found" (Right (String "bonjour","")) (parse parseString "\"bonjour\"")
        assertEqual "not found" (Left (Error "\"bonjour")) (parse parseString "\"bonjour")

    )

testParseAtom :: Test
testParseAtom = TestCase (do
        assertEqual "found true" (Right (Boolean True,"")) (parse parseAtom "#t")
        assertEqual "found false" (Right (Boolean False,"")) (parse parseAtom "#f")
        assertEqual "found atom" (Right (Atom "@123test","")) (parse parseAtom "@123test")
        assertEqual "not found" (Left (Error "123no")) (parse parseAtom "123no")

    )

testParseNumber :: Test
testParseNumber = TestCase (do
        assertEqual "found" (Right (Number 123,"")) (parse parseNumber "123")
        assertEqual "not found" (Left (Error "abc")) (parse parseNumber "abc")
    )

testParseQuoted :: Test
testParseQuoted = TestCase (do
        assertEqual "found" (Right (List [Atom "quote", Atom "bonjour"],"")) (parse parseQuoted "'bonjour")
        assertEqual "not found" (Left (Error "bonjour")) (parse parseQuoted "bonjour")
    )

testParseList :: Test
testParseList = TestCase (do
        assertEqual "all good" (Right (List [Atom "une", Atom "string", Number 123], "")) (parse parseList "une string 123")
        assertEqual "error" (Left (Error "")) (parse parseList "")
    )

testParseParens :: Test
testParseParens = TestCase (do
        assertEqual "success list" (Right (List [Atom "une", Atom "string", Number 456], "")) (parse parseParens "(une string 456)")
        assertEqual "success pair" (Right (Pair [Atom "une"] (Atom "string"), "")) (parse parseParens "(une . string)")
        assertEqual "error" (Left (Error "")) (parse parseList "")
    )

testParsePair :: Test
testParsePair = TestCase (do
        assertEqual "success" (Right (Pair [Atom "une"] (Atom "string"), "")) (parse parsePair "une . string")
        assertEqual "too many element" (Right (Pair [Atom "une"] (Atom "string"), " . longue")) (parse parsePair "une . string . longue")
        assertEqual "error" (Left (Error "")) (parse parsePair "")
    )

--- Lexer.hs

testAtomBuiltins :: Test
testAtomBuiltins = TestCase (do
        assertEqual "string atom" (Right (Boolean True)) (eval (List [Atom "atom?", String "pouet"]))
        assertEqual "number atom" (Right (Boolean True)) (eval (List [Atom "atom?", Number 667]))
        assertEqual "boolean false atom" (Right (Boolean True)) (eval (List [Atom "atom?", Boolean True]))
        assertEqual "boolean true atom" (Right (Boolean True)) (eval (List [Atom "atom?", Boolean False]))
        assertEqual "empty list atom" (Right (Boolean True)) (eval (List [Atom "atom?", List []]))
        assertEqual "non empty list atom" (Right (Boolean False)) (eval (List [Atom "atom?", List [Number 1, Number 2, Number 3]]))
    )

main :: IO ()
main = do _ <- runTestTT tests
          putStrLn "DONE"
