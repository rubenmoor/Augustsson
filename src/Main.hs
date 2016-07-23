{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad        (unless)
import           Control.Monad.Except (runExcept, throwError)
import           Data.Monoid          ((<>))

-- import SimpleParser (parseExpr)
import           ParserLexer          (stripAnnotation, evaluate, parseExpr,
                                       typeCheck)

main :: IO ()
main = do
    input <- getLine
    unless (null input) $ do
        -- print $ parseExpr input
        let result = runExcept $ do
                expr   <- either mkParseError pure $ parseExpr input
                atExpr <- either mkTypeError  pure $ typeCheck expr
                let safeExpr = stripAnnotation atExpr
                eval   <- either mkEvalError  pure $ evaluate safeExpr -- cannot fail
                pure eval
        case result of
            Left  err -> putStrLn err
            Right res -> print res
        main
  where
    mkParseError err = throwError $ "Parse error: " <> show err
    mkTypeError  err = throwError $ "Type error: "  <> show err
    mkEvalError  err = throwError $ "Eval error: "  <> show err
