module EvaluationTests (
    testList
) where

import Control.Monad.Except
import Test.HUnit

import Parser
import Evaluation
import Types
import Utils

interpret :: String -> IO String
interpret lisp = primitiveBindings >>= flip evalString lisp


interpretationTestData :: [(String, String, String)]
interpretationTestData = [
    ("primitive +",
        "(+ 1 2)",
        "3"),
    ("if - true condition",
        "(if (equal? (+ 1 2) 3) #t #f)",
        "#t"),
    ("if - false condition",
        "(if (equal? (+ 1 2) 4) #t #f)",
        "#f"),
    ("if - non-boolean condition",
        "(if (+ 1 2) #t #f)",
        "Invalid type: expected Expected boolean in <if> predicate, found 3"),
    ("cond - second clause",
        "(cond (#f (muh 2)) (#t \"hi\"))",
        "\"hi\""),
    ("cond - else",
        "(cond (#f (muh 3)) ((equal? (+ 1 2) 4) #f) (else #t))",
        "#t"),
    ("cond - subexpr",
        "(cond (#f (muh 3)) ((equal? (+ 1 2) 3) #f) (else #t))",
        "#f"),
    ("case - second clause",
        "(case (+ 1 2) (#t 3) (3 'test))",
        "test"),
    ("case - else",
        "(case (equal? (+ 1 2) 3) (2 #t) (else #f))",
        "#f"),
    ("bool? - plain bool",
        "(bool? #f)",
        "#t"),
    ("bool? - plain non-bool",
        "(bool? 'true)",
        "#f"),
    ("bool? - boolean expression",
        "(bool? (equal? (+ 2 3) 5))",
        "#t")
    ]


createInterpretationTest (desc, lisp, expectedValue) =
    TestCase $ do val <- interpret lisp
                  assertEqual desc expectedValue val


testList = TestList $ map createInterpretationTest interpretationTestData