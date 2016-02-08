module EvaluationTests (
    testList
) where

import Test.HUnit

import Parser
import Evaluation
import Types

interpret :: String -> LispVal
interpret lisp = extractValue (readExpr lisp >>= eval)

test1 = TestCase $ assertEqual "Testing +" (interpret "(+ 1 2)") (Number 3)

testList = [TestLabel "test" test1]