{- |
Module      :  $Header$
Description :  Implements lispll REPL

Implements lispll REPL
-}
module Main (
    main
)where

import Control.Monad.Except
import System.Environment
import System.IO

import Types
import Evaluation
import Parser

-- | Program entry point.
--
-- The lispll executable can either be invoked without arguments, triggering the interactive REPL,
-- or with a string containing a Lisp expression. In the latter case the program prints the result
-- of evaluating that expression end exits.
main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> runRepl
               1 -> evalAndPrint $ args !! 0
               otherwise -> putStrLn "program takes only 0 or 1 argument"

runRepl :: IO ()
runRepl = until_ (=="quit") (readPrompt "lispll>>> ") evalAndPrint

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError $ (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action
