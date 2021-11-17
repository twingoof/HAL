--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- LispError
--

module LispError where
import Parser
import Types
import Control.Monad.Except
import Errors
import Control.Exception

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