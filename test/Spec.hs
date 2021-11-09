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
import Test.HUnit.Base (assertEqual)
import Test.HUnit.Lang (assertEqual)

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
    TestLabel "parsePair" testParsePair
    ]

--- basic.hs

testOneOf :: Test
testOneOf = TestCase (do
        assertEqual "found" (Left ('b', "onjour")) (parse (oneOf "abc") "bonjour")
        assertEqual "Not found" (Right (Error "bonjour")) (parse (oneOf "xyz") "bonjour")
    )

testSkipMany :: Test
testSkipMany = TestCase (do
        assertEqual "skipped" (Left ((), "bonjour")) (parse (skipMany ' ') "   bonjour")
        assertEqual "nothing" (Left ((), "bonjour")) (parse (skipMany ' ') "bonjour")
    )

testNoneOf :: Test
testNoneOf = TestCase (do
        assertEqual "found" (Right (Error "bonjour")) (parse (noneOf "abc") "bonjour")
        assertEqual "Not found" (Left ('b', "onjour")) (parse (noneOf "xyz") "bonjour")
    )

testChar :: Test
testChar = TestCase (do
        assertEqual "equal" (Left ('b', "onjour")) (parse (char 'b') "bonjour")
        assertEqual "Not equal" (Right (Error "bonjour")) (parse (char 'g') "bonjour")
    )

testLetter :: Test
testLetter = TestCase (do
        assertEqual "found" (Left ('b', "onjour")) (parse letter "bonjour")
        assertEqual "Not found" (Right (Error "123")) (parse letter "123")
    )

testDigit :: Test
testDigit = TestCase (do
        assertEqual "found" (Left ('1', "23")) (parse digit "123")
        assertEqual "Not found" (Right (Error "bonjour")) (parse digit "bonjour")
    )

testSepBy :: Test
testSepBy = TestCase (do
        assertEqual "splitted" (Left (["bonjour", "hello", "aurevoir", "bye"], "")) (parse (sepBy (many letter) spaces) "bonjour hello aurevoir bye")
        assertEqual "no split" (Left (["bonjour"], "")) (parse (sepBy (many letter) spaces) "bonjour")
        assertEqual "error split" (Right (Error " hello aurevoir bye")) (parse (sepBy (many letter) digit) "bonjour hello aurevoir bye")
        assertEqual "error parse" (Right (Error "bonjour hello aurevoir bye")) (parse (sepBy (many digit) letter) "bonjour hello aurevoir bye")
    )

testEndBy :: Test
testEndBy = TestCase (do
        assertEqual "success" (Left (["bonjour","hello","aurevoir"],"bye")) (parse (endBy (many letter) spaces) "bonjour hello aurevoir bye")
        assertEqual "fail" (Right (Error "")) (parse (endBy (many letter) spaces) "bonjour")
    )

--- types.hs

testParseString :: Test
testParseString = TestCase (do
        assertEqual "found" (Left (String "bonjour","")) (parse parseString "\"bonjour\"")
        assertEqual "not found" (Right (Error "")) (parse parseString "\"bonjour")

    )

testParseAtom :: Test
testParseAtom = TestCase (do
        assertEqual "found true" (Left (Boolean True,"")) (parse parseAtom "#t")
        assertEqual "found false" (Left (Boolean False,"")) (parse parseAtom "#f")
        assertEqual "found atom" (Left (Atom "@123test","")) (parse parseAtom "@123test")
        assertEqual "not found" (Right (Error "123no")) (parse parseAtom "123no")

    )

testParseNumber :: Test
testParseNumber = TestCase (do
        assertEqual "found" (Left (Number 123,"")) (parse parseNumber "123")
        assertEqual "not found" (Right (Error "abc")) (parse parseNumber "abc")
    )

testParseQuoted :: Test
testParseQuoted = TestCase (do
        assertEqual "found" (Left (List [Atom "quote", Atom "bonjour"],"")) (parse parseQuoted "'bonjour")
        assertEqual "not found" (Right (Error "bonjour")) (parse parseQuoted "bonjour")
    )

testParseList :: Test
testParseList = TestCase (do
        assertEqual "all good" (Left (List [Atom "une", Atom "string", Number 123], "")) (parse parseList "une string 123")
        assertEqual "error" (Right (Error "")) (parse parseList "")
    )

testParseParens :: Test
testParseParens = TestCase (do
        assertEqual "success list" (Left (List [Atom "une", Atom "string", Number 456], "")) (parse parseParens "(une string 456)")
        assertEqual "success pair" (Left (Pair [Atom "une"] (Atom "string"), "")) (parse parseParens "(une . string)")
        assertEqual "error" (Right (Error "")) (parse parseList "")
    )

testParsePair :: Test
testParsePair = TestCase (do
        assertEqual "success" (Left (Pair [Atom "une"] (Atom "string"), "")) (parse parsePair "une . string")
        assertEqual "too many element" (Left (Pair [Atom "une"] (Atom "string"), " . longue")) (parse parsePair "une . string . longue")
        assertEqual "error" (Right (Error "")) (parse parsePair "")
    )

main :: IO ()
main = do _ <- runTestTT tests
          putStrLn "DONE"
