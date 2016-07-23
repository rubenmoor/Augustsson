module SimpleParser
  ( parseExpr
  )where

import           Data.Maybe         (isJust)
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String (Parser)

data Expr
  = ESum    [Expr]
  | ENumber Integer
  | EString String
  deriving (Show)

parseExpr :: String -> Either ParseError Expr
parseExpr = parse pMain ""

pMain :: Parser Expr
pMain = spaces *> pExpr <* eof

pExpr :: Parser Expr
pExpr = pEString <|> pENumber <|> pESum <?> "expression"

pEString :: Parser Expr
pEString  =
  EString <$> (char '\"' *> many (noneOf "\"") <* char '\"' <?> "string")

pENumber :: Parser Expr
pENumber = do
    negative <- isJust <$> optionMaybe (char '-')
    num      <- read   <$> many1 digit
    pure . ENumber $ if negative then -1 * num else num

pESum :: Parser Expr
pESum = ESum <$> (string "SUM(" *> pExpr `sepBy1` char ',' <* char ')')
