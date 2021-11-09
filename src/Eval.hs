--
-- EPITECH PROJECT, 2021
-- B-FUN-501-NAN-5-1-HAL-victor.trencic [WSL: Ubuntu]
-- File description:
-- Eval
--

module Eval where
import Types

showValue :: Value -> String
showValue (String str) = "\"" ++ str ++ "\""
showValue (Atom name) = name
showValue (Number num) = show num
showValue (Boolean True) = "#t"
showValue (Boolean False) = "#f"
showValue (List _) = ""
showValue (Pair _ _) = ""