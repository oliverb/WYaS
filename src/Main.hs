module Main where

import Parser

main :: IO ()
main = putStrLn (readExpr "\"fo\\\"o\"")