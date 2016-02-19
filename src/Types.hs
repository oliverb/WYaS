{- |
Module      :  $Header$
Description :  Exports data types for representation of Lisp expressions and error handling

Exports data types for representation of Lisp expressions and error handling
-}
module Types (
    Env,
    LispVal(..),
    LispError(..),
    ThrowsError,
    IOThrowsError,
    trapError,
    extractValue,
    liftThrows,
    runIOThrows
)where

import Control.Monad.Except
import Data.IORef
import System.IO (Handle)
import Text.ParserCombinators.Parsec (ParseError)


-- | Represents the state of the lisp interpreter, which in essence is a map binding
-- names to Lisp expressions
type Env = IORef [(String, IORef LispVal)]


-- | Represents any lisp expressions including the parsed program before evluation
data LispVal = Atom String
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Port Handle
             | PrimitiveFunc  ([LispVal] -> ThrowsError LispVal)
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env }


instance Show LispVal where
    show = showVal

    
-- | Definition of exceptions possibly arising during parsing or evaluation
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
               

instance Show LispError where
    show = showError

    
--instance Error LispError where
--    noMsg = Default "An error has occured"
--    strMsg = Default


-- | Monad for failable computations possibly returning LispErrors
type ThrowsError = Either LispError


-- | Monad for failable IO actions possibly returning LispErrors
type IOThrowsError = ExceptT LispError IO


-- | Lift a failable computation to one with side effects
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val


-- TODO think about this some more
-- | Execute a failable action and return the computed String/error
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue


-- | Execute an action returning a String while wrapping any occuring errors
trapError :: MonadError e m => Show e => m String -> m String
trapError action = catchError action (return . show)

-- | Extract a value from a completed failable computation.
-- Generates a runtime error when called on a failed computation!
extractValue :: ThrowsError a -> a
extractValue (Right val) = val


unwordsAndShowList :: [LispVal] -> String
unwordsAndShowList = unwords . map show

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (Number int) = show int
showVal (String str) = show str
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character c) = show c
showVal (Float d) = show d
showVal (List xs) = "(" ++ unwordsAndShowList xs ++ ")"
showVal (DottedList xs x) = "(" ++ unwordsAndShowList xs ++ " . " ++ show x ++ ")"
showVal (Port _) = "<IO port>"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (IOFunc _) = "<IO primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"


showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsAndShowList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

