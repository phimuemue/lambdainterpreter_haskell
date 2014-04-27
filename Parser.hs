module Parser where

import Data.List
import Text.ParserCombinators.Parsec

import Expression

variableIdentifier :: Parser String
variableIdentifier = do r <- many1 (noneOf " .\\()")
                        return r

singleLambdaParser :: Parser Expression
singleLambdaParser = (do char '\\'
                         spaces
                         vl <- (endBy1 variableIdentifier spaces)
                         char '.'
                         spaces
                         e <- lambdaParser
                         return $ foldr (\x e -> abstraction x e) e vl)
                     <|> do e <- ((do char '('
                                      e' <- lambdaParser
                                      char ')'
                                      return e')
                                 <|> do v <- variableIdentifier
                                        return $ variable v)
                            return e

lambdaParser :: Parser Expression
lambdaParser = do l <- endBy1 singleLambdaParser spaces
                  return $ foldl1 (\f x -> application f x) l

parseLambda s = case parse lambdaParser "(unknown)" s of
                Right a -> Just a
                Left _ -> Nothing
