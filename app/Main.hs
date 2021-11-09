--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Main
--

module Main where
import Hal
import System.Environment
import Control.Exception
import Errors

main :: IO ()
main = handle catchException $
    getArgs >>= hal
