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
-- or with a path to a Lisp source file. In the latter case the program prints the result
-- of evaluating that expression end exits.
main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args
