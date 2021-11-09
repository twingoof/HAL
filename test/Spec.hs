import Test.HUnit
import Basic
import Parser
import Control.Applicative

tests :: Test
tests = TestList [
    TestLabel "oneOf" testOneOf,
    TestLabel "skipMany" testSkipMany,
    TestLabel "noneOf" testNoneOf,
    TestLabel "char" testChar,
    TestLabel "digit" testDigit,
    TestLabel "letter" testLetter,
    TestLabel "sepBy" testSepBy
    ]

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

main :: IO ()
main = do _ <- runTestTT tests
          putStrLn "DONE"
