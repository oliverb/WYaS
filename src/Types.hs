{- |
Module      :  $Header$
Description :  Exports data types for representation and evaluation of Lisp programs

Exports data types for representation and evaluation of Lisp programs
-}
module Types (
    LispVal(..),
    LispError(..),
    ThrowsError,
    IOThrowsError,
    Env,
    nullEnv,
    trapError,
    extractValue
)where

import Control.Monad.Except
import Data.IORef

import Text.ParserCombinators.Parsec hiding (spaces)

-- TODO add vectors
data LispVal = Atom String
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | List [LispVal]
             | DottedList [LispVal] LispVal
             deriving (Eq)

instance Show LispVal where
    show = showVal

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

type Env = IORef [(String, IORef LispVal)]

type ThrowsError = Either LispError

type IOThrowsError = ExceptT LispError IO

-- Helper action to create an empty environment
nullEnv :: IO Env
nullEnv = newIORef []

trapError action = catchError action (return . show)

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

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsAndShowList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

