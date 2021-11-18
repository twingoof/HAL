--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Lexer
--

module Lexer where
import Types
import Control.Monad.Except
import Unpack
import Data.List
import Lists
import Debug.Trace
import Data.Maybe

eval :: Env -> Value -> ThrowsError (Env, Value)
eval env val@(String _) = Right (env, val)
eval env val@(Number _) = Right (env, val)
eval env val@(Boolean _) = Right (env, val)
eval env (Atom name) = getVar name env
eval env (List [Atom "quote", val]) = Right (env, val)
eval env (List [Atom "define", def@(Atom var), form])
    | Right (envv, val) <- eval env form = Right (setVar var val envv, def)
    | left <- eval env form = left
eval env (List (Atom "define":List (def@(Atom var):params):body)) =
    Right (setVar var (makeNormalFunc params body) env, def)
eval env (List (Atom "define":Pair (def@(Atom var):params) vaargs:body)) =
    Right (setVar var (makeVaargs vaargs params body) env, def)
eval env (List (Atom "lambda":List params:body)) =
    Right (env, makeNormalFunc params body)
eval env (List (Atom "lambda":Pair params vaargs:body)) =
    Right (env, makeVaargs vaargs params body)
eval env (List (Atom "lambda":vaargs@(Atom _):body)) =
    Right (env, makeVaargs vaargs [] body)
eval env cond@(List [Atom "if", pred, conseq, alt])
    | Right (envv, Boolean False) <- eval env pred = eval envv alt
    | Right (envv, Boolean True) <- eval env pred = eval envv conseq
    | otherwise = throwError $ BadSpecialForm "Unrecognized special form" cond
eval env cond@(List [Atom "atom?", expr])
    | Right (envv, List []) <- eval env expr = Right (envv, Boolean True)
    | Right (envv, List x) <- eval env expr = Right (envv, Boolean False)
    | Right (envv,_) <- eval env expr = Right (envv, Boolean True)
    | otherwise = throwError $ BadSpecialForm "Unrecognized special form" cond
eval env (List (func@(Atom _):args))
    | Right (envv, func) <- eval env func
    , Right tab <- mapM (eval envv) args
    , Right val <- apply func (map snd tab) envv =
        Right (fst $ last tab, val)
    | Right (envv, func) <- eval env func
    , Right tab <- mapM (eval env) args
    , Left err <- apply func (map snd tab) envv =
        throwError err
    | Right (envv, func) <- eval env func
    , Left err <- mapM (eval env) args =
        throwError err
    | Left err <- eval env func =
        throwError err
eval env val@(List _) = Right (env, val)
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: Value -> [Value] -> Env -> ThrowsError Value
apply (Builtin func) args _ = func args
apply (Func params vaargs body) args closure
    | length params /= length args && isNothing vaargs = throwError $ NumArgs (toInteger $ length params) args
    | otherwise = do
        let env = bindVars (zip params args) closure
        let envv = case vaargs of
                Nothing -> env
                Just name -> setVar name (List $ drop (length params) args) env
        case last <$> mapM (eval envv) body of
            Right (_, res) -> Right res
            Left err -> throwError err
apply err _ _ = throwError $ NotFunction "Unrecognized special form" $ show err

numBuiltins :: [(String, [Value] -> ThrowsError Value)]
numBuiltins = [
        ("+", numInfBinop (+) 0),
        ("-", numInfBinop (-) 0),
        ("*", numInfBinop (*) 1),
        ("div", numBinop div),
        ("mod", numBinop mod),
        ("quotient", numBinop quot),
        ("remainder", numBinop rem),
        ("=", numBoolBinop (==)),
        (">", numBoolBinop (>)),
        ("<", numBoolBinop (<)),
        (">=", numBoolBinop (>=)),
        ("<=", numBoolBinop (<=)),
        ("/=", numBoolBinop (/=))
    ]

boolBuiltins :: [(String, [Value] -> ThrowsError Value)]
boolBuiltins = [
        ("&&", boolBoolBinop (&&)),
        ("||", boolBoolBinop (||))
    ]

strBuiltins :: [(String, [Value] -> ThrowsError Value)]
strBuiltins = [
        ("string=?", strBoolBinop (==)),
        ("string>?", strBoolBinop (>)),
        ("string<?", strBoolBinop (<)),
        ("string>=?", strBoolBinop (>=)),
        ("string<=?", strBoolBinop (<=))
    ]

arithBuiltins :: [(String, [Value] -> ThrowsError Value)]
arithBuiltins = [
        ("eq?", eq)
    ]

builtinEnv :: Env
builtinEnv = bindVars (map make builtins) emptyEnv
    where make (var, func) = (var, Builtin func)

builtins :: [(String, [Value] -> ThrowsError Value)]
builtins = numBuiltins ++ boolBuiltins ++ strBuiltins ++ listBuiltins ++ arithBuiltins

numInfBinop :: (Integer -> Integer -> Integer) -> Integer -> [Value] -> ThrowsError Value
numInfBinop _ _ [] = throwError $ NumArgs 1 []
numInfBinop op def [x] = mapM unpackNum [Number def, x] >>= Right . Number . foldl1' op
numInfBinop op _ params = mapM unpackNum params >>= Right . Number . foldl1' op

numBinop :: (Integer -> Integer -> Integer) -> [Value] -> ThrowsError Value
numBinop _ [] = throwError $ NumArgs 2 []
numBinop op params@[_,_] = mapM unpackNum params >>= Right . Number . foldl1' op
numBinop _ params = throwError $ NumArgs 2 params

numBoolBinop :: (Integer -> Integer -> Bool) -> [Value] -> ThrowsError Value
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [Value] -> ThrowsError Value
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [Value] -> ThrowsError Value
boolBoolBinop = boolBinop unpackBool

boolBinop :: (Value -> ThrowsError a) -> (a -> a -> Bool) -> [Value] -> ThrowsError Value
boolBinop unpack op [x]
    | Right _ <- unpack x = Right $ Boolean True
    | Left err <- unpack x = Left err
boolBinop unpack op [x,y]
    | Right a <- unpack x
    , Right b <- unpack y = Right $ Boolean $ op a b
    | Left a <- unpack x = Left a
    | Left b <- unpack y = Left b
boolBinop unpack op params = throwError $ NumArgs 2 params

makeFunc :: Maybe String -> [Value] -> [Value] -> Value
makeFunc vaargs params = Func (map showVal params) vaargs

makeNormalFunc :: [Value] -> [Value] -> Value
makeNormalFunc = makeFunc Nothing

makeVaargs :: Value -> [Value] -> [Value] -> Value
makeVaargs = makeFunc . Just . showVal

eq :: [Value] -> ThrowsError Value
eq [Boolean arg1, Boolean arg2] = return $ Boolean $ arg1 == arg2
eq [Number arg1, Number arg2] = return $ Boolean $ arg1 == arg2
eq [String arg1, String arg2] = return $ Boolean $ arg1 == arg2
eq [Atom arg1, Atom arg2] = return $ Boolean $ arg1 == arg2
eq [Pair x xs, Pair y ys] = eq [List $ x ++ [xs], List $ y ++ [ys]]
eq [List arg1, List arg2]
    | null arg1 && null arg2 = return $ Boolean True
    | otherwise = return $ Boolean False
eq [_, _] = return $ Boolean False
eq badArgList = throwError $ NumArgs 2 badArgList
