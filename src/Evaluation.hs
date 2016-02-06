module Evaluation where

import Types

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("list?", Bool . (all isList)),
              ("symbol?", Bool . (all isSymbol)),
              ("symbol->string", symbolToString),
              ("string->symbol", stringToSymbol)]
              
symbolToString :: [LispVal] -> LispVal
symbolToString ([Atom name]) = String name   

stringToSymbol :: [LispVal] -> LispVal
stringToSymbol ([String name]) = Atom name     
              
isList :: LispVal -> Bool
isList (List _) = True
isList _ = False

isSymbol :: LispVal -> Bool
isSymbol (Atom _) = True
-- unsure
isSymbol (List [Atom "quote", _]) = True
isSymbol _ = False
              
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then 0
                              else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0