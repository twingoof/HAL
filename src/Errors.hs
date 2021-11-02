--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Erros
--

module Errors where

import Control.Exception
import System.Exit

data ParsingException =
    NoArg
    deriving Show

instance Exception ParsingException

exit :: IO ()
exit = exitWith $ ExitFailure 84

catchException :: ParsingException -> IO ()
catchException _ = exit