module Main (
    main
) where

import Test.HUnit
import qualified System.Exit as Exit

import qualified EvaluationTests

tests = TestList [EvaluationTests.testList]

main :: IO ()
main = do count <- runTestTT $ tests
          if failures count > 0 then Exit.exitFailure else return ()