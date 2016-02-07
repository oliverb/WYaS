module Evaluation where

import Control.Monad.Except

import Types

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("list?", multiArgPredicate isList),
              ("symbol?", multiArgPredicate isSymbol),
              ("symbol->string", symbolToString),
              ("string->symbol", stringToSymbol)]
              
symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom name] = return $ String name
symbolToString [notAtom] = throwError $ TypeMismatch "atom" $ notAtom
symbolToString val@(_) = throwError $ NumArgs 1 val

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol ([String name]) = return $ Atom name
stringToSymbol [notString] = throwError $ TypeMismatch "atom" $ notString
stringToSymbol val@(_) = throwError $ NumArgs 1 val

multiArgPredicate :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
multiArgPredicate p xs = return $ Bool $ all p xs

isList :: LispVal -> Bool
isList (List _) = True
isList _ = False

isSymbol :: LispVal -> Bool
isSymbol (Atom _) = True
-- unsure
isSymbol (List [Atom "quote", _]) = True
isSymbol _ = False
              
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []            = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum