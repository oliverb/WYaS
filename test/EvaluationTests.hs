module EvaluationTests (
    testList
) where

import Test.HUnit

import Parser
import Evaluation
import Types

-- For testing purposes we make LispError in instance of Eq where we, in effect, only compare the type
-- constructor but not the contained values.
-- This allows us to provoke and check e.g. a TypeMismatch error without having to specify the exact
-- error string.
instance Eq LispError where
    (==) = cmpError

cmpError :: LispError -> LispError -> Bool
cmpError (NumArgs _ _) (NumArgs _ _) = True
cmpError (TypeMismatch _ _) (TypeMismatch _ _) = True
cmpError (Parser _) (Parser _) = True
cmpError (BadSpecialForm _ _) (BadSpecialForm _ _) = True
cmpError (NotFunction _ _) (NotFunction _ _) = True
cmpError (UnboundVar _ _) (UnboundVar _ _) = True
cmpError (Default _) (Default _) = True
cmpError _ _ = False

interpret :: String -> ThrowsError LispVal
interpret lisp = readExpr lisp >>= eval

interpretationTestData :: [(String, String, ThrowsError LispVal)]
interpretationTestData = [
    ("primitve +", "(+ 1 2)", Right $ Number 3),
    ("if - true condition", "(if (equal? (+ 1 2) 3) #t #f)", Right $ Bool True),
    ("if - false condition", "(if (equal? (+ 1 2) 4) #t #f)", Right $ Bool False),
    ("if - non-boolean condition", "(if (+ 1 2) #t #f)", Left $ TypeMismatch "" (Bool True)),
    ("cond - second clause", "(cond (#f (muh 2)) (#t \"hi\"))", Right $ String "hi"),
    ("cond - else", "(cond (#f (muh 3)) ((equal? (+ 1 2) 4) #f) (else #t))", Right $ Bool True),
    ("cond - subexpr", "(cond (#f (muh 3)) ((equal? (+ 1 2) 3) #f) (else #t))", Right $ Bool False),
    ("case - second clause", "(case (+ 1 2) (#t 3) (3 'test))", Right $ Atom "test"),
    ("case - else", "(case (equal? (+ 1 2) 3) (2 #t) (else #f))", Right $ Bool False),
    ("bool? - plain bool", "(bool? #f)", Right $ Bool True),
    ("bool? - plain non-bool", "(bool? 'true)", Right $ Bool False),
    ("bool? - boolean expression", "(bool? (equal? (+ 2 3) 5))", Right $ Bool True)
    ]

createInterpretationTest (desc, lisp, expectedValue) =
    TestCase $ assertEqual desc expectedValue (interpret lisp)

testList = TestList $ map createInterpretationTest interpretationTestData