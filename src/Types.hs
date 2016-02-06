module Types where

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
    show (Atom name) = name
    show (Number int) = show int
    show (String str) = str
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (Character c) = show c
    show (Float d) = show d
    show (List xs) = "(" ++ (unwords $ map show xs) ++ ")"
    show (DottedList xs x) = "(" ++ (unwords $ map show xs) ++ " . " ++ (show x) ++ ")"
