--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Main
--

module Main where
import Hal
import Control.Exception
import Errors
import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment
import Data.List

data Flag = Interactive | Help
    deriving (Eq,Ord,Enum,Show,Bounded)

flags :: [OptDescr Flag]
flags = [
        Option ['i'] [] (NoArg Interactive) "Enable interactive mode",
        Option ['h'] ["help"] (NoArg Help) "Print this help message"
    ]

usage :: String
usage = "Usage: hal [-hi] [file ...]"

setOpt :: ([Flag], [String], [String]) -> IO ([Flag], [String])
setOpt (args,fs,[])
    | Help `elem` args = putStrLn (usageInfo usage flags) >> exitSuccess
    | otherwise = return (nub args, fs)
setOpt (_,_,errs) =
    hPutStrLn stderr (concat errs ++ usageInfo usage flags) >> exit

parse :: [String] -> IO ([Flag], [String])
parse argv = setOpt $ getOpt Permute flags argv

launchHal :: ([Flag], [String]) -> IO ()
launchHal (args, files) = handle catchException $
    hal (Interactive `elem` args) files

main :: IO ()
main = getArgs >>= parse >>= launchHal
