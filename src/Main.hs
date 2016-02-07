module Main where

import Control.Monad.Except
import System.Environment

import Types
import Evaluation
import Parser

main :: IO ()
main = do args <- getArgs
          evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
          putStrLn $ extractValue $ trapError evaled
