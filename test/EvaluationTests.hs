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
    ("cond - second clause", "(cond (#f (muh 2)) (#t \"hi\"))", Right $ String "hi")
    ]

createInterpretationTest (desc, lisp, expectedValue) =
    TestCase $ assertEqual desc expectedValue (interpret lisp)

testList = TestList $ map createInterpretationTest interpretationTestData