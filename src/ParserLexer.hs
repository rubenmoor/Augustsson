{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE StandaloneDeriving        #-}

module ParserLexer
  ( parseExpr
  , typeCheck
  , evaluate
  , ATExpr ((:::))
  ) where

-- parser / lexer

import           Data.Functor          (($>))
import           Data.Functor.Identity (Identity)
import           Data.Maybe            (isJust)
import           Text.Parsec
import qualified Text.Parsec.Expr      as Ex
import qualified Text.Parsec.Language  as Lex
import           Text.Parsec.String    (Parser)
import qualified Text.Parsec.Token     as Lex

-- type checker

import           Control.Monad.Except  (MonadError, throwError)

-- evaluator

import Control.Monad (foldM)

-- expression tree

data Expr
  = ENumber Integer
  | EString String
  | EList   [Expr]
  | ESum    Expr
  deriving (Show)

-- lexer

lexer :: Lex.GenTokenParser String a Identity
lexer = Lex.makeTokenParser Lex.emptyDef

pParens :: Parser a -> Parser a
pParens = Lex.parens lexer

pCommaSep :: Parser a -> Parser [a]
pCommaSep = Lex.commaSep lexer

pReservedOp :: String -> Parser ()
pReservedOp = Lex.reservedOp lexer

pWhiteSpace :: Parser ()
pWhiteSpace = Lex.whiteSpace lexer

-- parser

parseExpr :: String -> Either ParseError Expr
parseExpr = parse pMain ""

pMain :: Parser Expr
pMain = pWhiteSpace *> pExpr <* eof

pExpr :: Parser Expr
pExpr = Ex.buildExpressionParser opTable term <?> "expression"
  where
    opTable = [ [Ex.Prefix (pReservedOp "SUM" $> ESum)]
              ]
    term    = pEString <|> pENumber <|> pEList <?> "term"

pEString :: Parser Expr
pEString  =
  EString <$> (char '\"' *> many (noneOf "\"") <* char '\"' <?> "string")

pENumber :: Parser Expr
pENumber = do
    negative <- isJust <$> optionMaybe (char '-')
    num      <- read   <$> many1 digit
    pure . ENumber $ if negative then -1 * num else num

pEList :: Parser Expr
pEList = EList <$> pParens (pCommaSep pExpr)

-- type checker

data TExpr a where
    TExprNumber :: Integer         -> TExpr Integer
    TExprString :: String          -> TExpr String
    TExprList   :: [TExpr b]       -> TExpr [b]
    TExprSum    :: TExpr [Integer] -> TExpr Integer

deriving instance Show (TExpr a)

data TType a where
    TTypeNumber     :: TType Integer
    TTypeString     :: TType String
    TTypeNumberList :: TType [Integer]
    TTypeStringList :: TType [String]

deriving instance Show (TType a)

data ATExpr = forall a. Show a => TExpr a ::: TType a

deriving instance Show ATExpr

-- infer types
typeCheck :: MonadError String m => Expr -> m ATExpr
typeCheck (ENumber x) = pure $ TExprNumber x ::: TTypeNumber
typeCheck (EString s) = pure $ TExprString s ::: TTypeString
typeCheck (EList  ls) = traverse typeCheck ls >>= \case
    []                      -> pure $ TExprList [] ::: TTypeNumberList
    (te ::: TTypeNumber):es -> do tes <- traverse checkTypeNumber es
                                  pure $ TExprList (te:tes) ::: TTypeNumberList
    (te ::: TTypeString):es -> do tes <- traverse checkTypeString es
                                  pure $ TExprList (te:tes) ::: TTypeStringList
    _                       -> throwError "Only list of ENumbers or EStrings allowed"
  where
    checkTypeNumber :: MonadError String m => ATExpr -> m (TExpr Integer)
    checkTypeNumber (e ::: TTypeNumber) = pure e
    checkTypeNumber _                   = throwError "EList of ENumbers not homogeneous"

    checkTypeString :: MonadError String m => ATExpr -> m (TExpr String)
    checkTypeString (e ::: TTypeString) = pure e
    checkTypeString _                   = throwError "EList of EStrings not homogeneous"
typeCheck (ESum    e) = typeCheck e >>= \case
    te ::: TTypeNumberList -> pure $ TExprSum te ::: TTypeNumber
    _                      -> throwError "Sum requires EList of ENumbers"

-- evaluation

evaluate :: TExpr a -> a
evaluate (TExprNumber x)  = x
evaluate (TExprString s)  = s
evaluate (TExprList   ls) = map evaluate ls
evaluate (TExprSum    xs) = sum $ evaluate xs
