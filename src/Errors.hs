--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Erros
--

module Errors where

import Control.Exception
import System.Exit
import Types

data HalException =
    InvalidPath
    deriving Show

instance Exception HalException

data LispError = NumArgs Integer [Value]
    | TypeMismatch String Value
    | Parsing String
    | BadSpecialForm String Value
    | NotFunction String String
    | UnboundVar String String

instance Show LispError where show = showError

showError :: LispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parsing err) = "Parse error at " ++ err
showError (BadSpecialForm str val) = str ++ ": " ++ show val
showError (NotFunction str func)    = str ++ ": " ++ func
showError (UnboundVar str val)  = str ++ ": " ++ val

exit :: IO ()
exit = exitWith $ ExitFailure 84

catchException :: HalException -> IO ()
catchException InvalidPath = putStrLn "One filepath provided is incorrect" >> exit
