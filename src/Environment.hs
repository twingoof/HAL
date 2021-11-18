--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Environment
--

module Environment where
import Types
import LispError
import Control.Monad.Except

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
