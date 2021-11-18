--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Types
--

module Types where

import Parser
import Basic
import Control.Applicative
import Control.Monad.Except
import Control.Exception
import Errors

type ThrowsError = Either Error

data Error = NumArgs Integer [Value]
    | TypeMismatch String Value
    | Parsing ParserError
    | BadSpecialForm String Value
    | NotFunction String String
    | UnboundVar String String
    | Empty
    deriving (Eq)

instance Show Error where show = showError

showError :: Error -> String
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parsing (Error err)) = "Parse error at " ++ err
showError (BadSpecialForm str val) = str ++ ": " ++ show val
showError (NotFunction str func)    = str ++ ": " ++ func
showError (UnboundVar str val)  = str ++ ": " ++ val
showError _ = []

trapError :: (MonadError a m, Show a) => Bool -> m String -> m String
trapError False action = action `catchError` (return . show)
trapError True action = action `catchError` (throw . LispException . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left err) = throw $ LispException ""

type Env = [(String, Value)]

emptyEnv :: Env
emptyEnv = []

getVar :: String -> Env -> ThrowsError (Env, Value)
getVar name env
    | Just val <- lookup name env = Right (env, val)
    | otherwise = throwError $ UnboundVar "Getting an unbound variable" name

deleteVar :: String -> Env -> Env
deleteVar name = filter (\(x,y) -> x /= name)

setVar :: String -> Value -> Env -> Env
setVar name value env = (name, value) : deleteVar name env

bindVars :: [(String, Value)] -> Env -> Env
bindVars [] env = env
bindVars ((x,y):xs) env = bindVars xs $ setVar x y env

concatEnv :: Env -> Env -> Env
concatEnv [] to = to
concatEnv (val@(x,y):xs) to
    | Just _ <- lookup x to = to
    | otherwise = val : concatEnv xs to

data Value =
    Number Integer | Boolean Bool | String String |
    List [Value] | Pair [Value] Value | Atom String |
    Builtin ([Value] -> ThrowsError Value) |
    Func {
        params :: [String],
        vaargs :: Maybe String,
        body :: [Value],
        closure :: Env
    }

instance Eq Value where x == y = eqValue x y

eqValue :: Value -> Value -> Bool
eqValue (Number x) (Number y) = x == y
eqValue (Boolean x) (Boolean y) = x == y
eqValue (String x) (String y) = x == y
eqValue (List x) (List y) = x == y
eqValue (Pair x xx) (Pair y yy) = x == y && xx == yy
eqValue (Atom x) (Atom y) = x == y
eqValue _ _ = False

instance Show Value where show = showVal

showVal :: Value -> String
showVal (String str) = "\"" ++ str ++ "\""
showVal (Atom name) = name
showVal (Number num) = show num
showVal (Boolean True) = "#t"
showVal (Boolean False) = "#f"
showVal (List list) = "(" ++ unwordList list ++ ")"
showVal (Pair head tail) = "(" ++ unwordList head ++ " . " ++ showVal tail ++ ")"
showVal (Builtin _) = "#<procedure>"
showVal Func {params = args, vaargs = vaargs, body = body} =
   "(lambda (" ++ unwords (map show args) ++
    (case vaargs of
         Nothing -> ""
         Just arg -> " . " ++ arg)
    ++ ") " ++ unwords (map show body) ++ ")"

unwordList :: [Value] -> String
unwordList = unwords . map showVal

funcParseString :: Data Value
funcParseString [] = Left (Error [])
funcParseString str
    | Right (c1, x) <- parse (char '"') str
    , Right (val, y) <- parse (many (noneOf "\"")) x
    , Right (c2, z) <- parse (char '"') y =
        Right (String val, z)
    | otherwise = Left (Error str)

parseString :: Parser Value
parseString = Parser funcParseString

getAtom :: String -> Value
getAtom "#t" = Boolean True
getAtom "#f" = Boolean False
getAtom str = Atom str

funcParseAtom :: Data Value
funcParseAtom [] = Left (Error [])
funcParseAtom str
    | Right (c, x) <- parse (letter <|> symbol) str
    , Right (rest, y) <- parse (many (letter <|> digit <|> symbol)) x =
        Right (getAtom (c : rest), y)
    | Right (c, x) <- parse (letter <|> symbol) str =
        Right (Atom [c], x)
    | otherwise = Left (Error str)

parseAtom :: Parser Value
parseAtom = Parser funcParseAtom

funcParseNumber :: Data Value
funcParseNumber [] = Left (Error [])
funcParseNumber str
    | Right (n, x) <- parse (many digit) str =
        Right (Number (read n :: Integer), x)
    | otherwise = Left (Error str)

parseNumber :: Parser Value
parseNumber = Parser funcParseNumber

funcParseQuoted :: Data Value
funcParseQuoted [] = Left (Error [])
funcParseQuoted str
    | Right (c, x) <- parse (char '\'') str
    , Right (val, y) <- parse parseExpr x =
        Right (List [Atom "quote", val], y)
    | otherwise = Left (Error str)

parseQuoted :: Parser Value
parseQuoted = Parser funcParseQuoted

funcParseList :: Data Value
funcParseList [] = Left (Error [])
funcParseList (x:str@(')':xs))
    | Right (a, []) <- parse parseExpr [x] =
        Right (List [a], str)
funcParseList str@(')':xs) = Right (List [], str)
funcParseList str
    | Right (a, x) <- parse (sepBy parseExpr spaces) str =
        Right (List a, x)
    | otherwise = Left (Error str)

parseList :: Parser Value
parseList = Parser funcParseList

funcParsePair :: Data Value
funcParsePair str
    | Right (head, x) <- parse (endBy parseExpr spaces) str
    , Right (tail, y) <- parse (char '.' >> spaces >> parseExpr) x =
        Right (Pair head tail, y)
    | otherwise = Left (Error str)

parsePair :: Parser Value
parsePair = Parser funcParsePair

funcParseParens :: Data Value
funcParseParens [] = Left (Error [])
funcParseParens str
    | Right (c1, x) <- parse (char '(') str
    , Right (val, y) <- parse (parsePair <|> parseList) x
    , Right (c2, z) <- parse (char ')') y =
        Right (val, z)
    | otherwise = Left (Error str)

parseParens :: Parser Value
parseParens = Parser funcParseParens

parseExpr :: Parser Value
parseExpr = parseAtom <|> parseString <|> parseNumber
    <|> parseQuoted <|> parseParens