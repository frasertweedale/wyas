module Parser
  (
    parseExpr
  , parseExprList
  ) where

import Control.Applicative ((<$>), (<*>), (<|>))
import Text.ParserCombinators.Parsec hiding (spaces, (<|>))

import Types

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> do
    _ <- char '('
    x <- try parseList <|> parseDottedList
    _ <- char ')'
    return x

parseExprList :: Parser [LispVal]
parseExprList = endBy parseExpr spaces

parseStringChar :: Parser Char
parseStringChar =
  (char '\\' >> (
        (char 'n' >> return '\n')
    <|> (char 'r' >> return '\r')
    <|> (char 't' >> return '\t')
    <|> char '\\'
    <|> char '"'
    ))
  <|> noneOf "\""

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  s <- many parseStringChar
  _ <- char '"'
  return $ String s

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t"  -> Bool True
    "#f"  -> Bool False
    _     -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = DottedList
  <$> endBy parseExpr spaces
  <*> (char '.' >> spaces >> parseExpr)

parseQuoted :: Parser LispVal
parseQuoted = List . (Atom "quote" :) . (:[]) <$> (char '\'' >> parseExpr)
