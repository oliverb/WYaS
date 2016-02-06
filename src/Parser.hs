module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             deriving (Show, Eq)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr = parseCharacter
         <|> parseAtom
         <|> parseString
         <|> parseNumber

escapedCharacter :: Parser Char
escapedCharacter = char '\\' >> (choice escapeCharParsers)
    where escapeChars = [('"', '"'), ('n', '\n'), ('r', '\r'), ('t', '\t'), ('\\', '\\')]
          escapeCharParsers = map (\(c, ec) -> char c >> return ec) escapeChars

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (escapedCharacter <|> noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

-- no semi-standard characters
-- TODO check if symbols are complete for this purpose
parseCharacter :: Parser LispVal
parseCharacter = do string "#\\"
                    c <- spaceLiteral <|> newlineLiteral <|> letter <|> digit <|> symbol <|> (return ' ')
                    return $ Character c
                    where spaceLiteral = try (string "space") >> return ' '
                          newlineLiteral = try (string "newline") >> return '\n'

-- TODO: add other number formats (#b, #o, #d, #x)
parseNumber :: Parser LispVal
parseNumber = many1 digit >>= return . Number . read

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val