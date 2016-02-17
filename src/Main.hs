{- |
Module      :  $Header$
Description :  Implements lispll REPL

Implements lispll REPL
-}
module Main (
    main,
    evalString -- exposed for testing module
)where

import Control.Monad.Except
import System.Environment
import System.IO

import Types
import Evaluation
import Parser
import Environment
import Primitives
import Utils

-- | Program entry point.
--
-- The lispll executable can either be invoked without arguments, triggering the interactive REPL,
-- or with a string containing a Lisp expression. In the latter case the program prints the result
-- of evaluating that expression end exits.
main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> runRepl
               1 -> runOne $ args !! 0
               otherwise -> putStrLn "Program takes only 0 or 1 argument"
