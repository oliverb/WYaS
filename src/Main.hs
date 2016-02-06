module Main where

import Parser

main :: IO ()
main = putStrLn (readExpr "(a '(quoted ,(dotted . list)) test)")