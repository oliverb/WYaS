{- |
Module      :  $Header$
Description :  Exposes parsing logic to convert Lisp expressions into a LispVal representation

Exposes parsing logic to convert Lisp expressions into a LispVal representation
-}
module Parser (
    readExpr
) where

import Control.Monad
import Control.Monad.Except
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)

import Types

-- | Parses a Lisp expression into its LispVal representation
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseCharacter
         <|> parseString
         <|> parseNumber
         <|> parseFloat
         <|> parseQuotation
         <|> parseList


parseList :: Parser LispVal
parseList = do char '('
               head <- sepEndBy parseExpr spaces
               tail <- parseDot
               char ')'
               return $ combinedList head tail
               where combinedList head Nothing = List head
                     combinedList head (Just tail) = DottedList head tail
                     parseDot = option Nothing (char '.' >> spaces >> parseExpr >>= return . Just)


parseQuotation :: Parser LispVal
parseQuotation = choice quoteParsers
    where quoteParsers = map quoteParser [(",@", "unquote-splicing"), (",", "unqote"),
                                          ("\'", "quote"), ("`", "quasiquote")]
          quoteParser (sugar, symbol) = do try $ string sugar
                                           x <- parseExpr
                                           return $ List [Atom symbol, x]

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

parseFloat :: Parser LispVal
parseFloat = do intPart <- many1 digit
                char '.'
                fractionalPart <- many1 digit
                return . Float . toFloat $ intPart ++ "." ++ fractionalPart
                where toFloat = fst . head . readFloat
