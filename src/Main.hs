module Main where

import Parser

main :: IO ()
main = putStrLn (readExpr "!2")