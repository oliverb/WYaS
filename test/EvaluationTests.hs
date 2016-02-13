module EvaluationTests (
    testList
) where

import Test.HUnit

import Parser
import Evaluation
import Types

interpret :: String -> LispVal
interpret lisp = extractValue (readExpr lisp >>= eval)

interpretationTestData :: [(String, String, LispVal)]
interpretationTestData = [
    ("primitve +", "(+ 1 2)", Number 3),
    ("if - true condition", "(if (equal? (+ 1 2) 3) #t #f)", Bool True),
    ("if - false condition", "(if (equal? (+ 1 2) 4) #t #f)", Bool False),
    ("cond - second clause", "(cond (#f (muh 2)) (#t \"hi\"))", String "hi")
    ]

createInterpretationTest (desc, lisp, expectedValue) =
    TestCase $ assertEqual desc expectedValue (interpret lisp)

testList = TestList $ map createInterpretationTest interpretationTestData