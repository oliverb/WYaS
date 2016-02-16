{-# LANGUAGE ExistentialQuantification #-}
{- |
Module      :  $Header$
Description :  Collection of built-in functions

Collection of built-in functions
-}
module Primitives (
    primitiveBindings,
    eqv
) where

import Control.Monad.Except

import Types
import Environment

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
     where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("string?", multiArgPredicate isString),
              ("make-string", makeString),
              ("string-length", stringLength),
              ("stringRef", stringRef),
              ("list?", multiArgPredicate isList),
              ("symbol?", multiArgPredicate isSymbol),
              ("bool?", multiArgPredicate isBool),
              ("symbol->string", symbolToString),
              ("string->symbol", stringToSymbol),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eqv?", eqv),
              ("eq?", eqv),
              ("equal?", equal)]
              
              
stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [String s, Number n] = return $ Character $ s !! (fromInteger n)
stringRef args@[_, _] = throwError $ TypeMismatch "string and integer" $ List args
stringRef badArgList = throwError $ NumArgs 2 badArgList
              
stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [String s] = return $ Number . toInteger $ length s
stringLength [val] = throwError $ TypeMismatch "string" val
stringLength badArgList = throwError $ NumArgs 1 badArgList
              
makeString :: [LispVal] -> ThrowsError LispVal
makeString [Number n] = return $ String $ take (fromInteger n) $ repeat '#'
makeString [Number n, Character c] = return $ String $ take (fromInteger n) $ repeat c
makeString args@[_] = throwError $ TypeMismatch "integer" $ List args
makeString args@[_, _] = throwError $ TypeMismatch "integer and char" $ List args
makeString badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ compareLists eqv arg1 arg2
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

compareLists :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> [LispVal] -> Bool
compareLists cmp xs ys = (length xs == length ys) && (all cmpPair $ zip xs ys)
    where cmpPair (x, y) = case cmp [x, y] of
                            Left err -> False
                            Right (Bool val) -> val

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

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

isString :: LispVal -> Bool
isString (String _) = True
isString _ = False

isBool :: LispVal -> Bool
isBool (Bool _) = True
isBool _ = False

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

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
             `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [(List arg1), (List arg2)] = return $ Bool $ compareLists equal arg1 arg2
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool