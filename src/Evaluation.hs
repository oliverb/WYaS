{- |
Module      :  $Header$
Description :  Exposes logic for interpretation of LispVal expressions

Exposes logic for interpretation of LispVal expressions
-}
module Evaluation (
    eval,
    primitiveBindings
)where

import Control.Monad.Except

import Types
import Environment
import Primitives

-- | Evaluate a LispVal expression
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
     do result <- eval env pred
        case result of
             Bool False -> eval env alt
             Bool True  -> eval env conseq
             otherwise  -> throwError $ TypeMismatch "Expected boolean in <if> predicate" result
eval env (List (Atom "cond":clauses)) = checkAndEvalClauses env (eval env) clauses
eval env (List (Atom "case":keyExpr:clauses)) =
    do key <- eval env keyExpr
       let predicate expr = do val <- eval env expr
                               liftThrows $ eqv [val, key]
       checkAndEvalClauses env predicate clauses
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
     makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
     makeVarArgs varargs env params body >>= defineVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
     makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
     makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
     makeVarArgs varargs env [] body
eval env (List (function : args)) = do
     func <- eval env function
     argVals <- mapM (eval env) args
     apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

makeFunc varargs env params body = return $ Func (map show params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . show


checkAndEvalClauses :: Env -> (LispVal -> IOThrowsError LispVal) -> [LispVal] -> IOThrowsError LispVal
checkAndEvalClauses env _ [List (Atom "else":exprs)] =
    case length exprs of
        0 -> throwError $ BadSpecialForm "Expected expressions after else clause in cond/case construct" (Atom "else")
        otherwise -> foldM (\_ expr -> eval env expr) (Atom "else") exprs
checkAndEvalClauses env check ((List (cond:exprs)):otherClauses) =
    do result <- check cond
       case result of
            Bool True -> foldM (\_ expr -> eval env expr) result exprs
            Bool False -> checkAndEvalClauses env check otherClauses
            otherwise -> throwError $ TypeMismatch "Expected boolean when checking cond/case clause predicate" result


apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
      if num params /= num args && varargs == Nothing
         then throwError $ NumArgs (num params) args
         else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            num = toInteger . length
            evalBody env = liftM last $ mapM (eval env) body
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing -> return env
