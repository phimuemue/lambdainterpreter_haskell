module Parser where

import Text.ParserCombinators.Parsec

import Expression

variableIdentifier :: Parser String
variableIdentifier = many1 (noneOf " .\\()")

singleLambdaParser :: Parser Expression
singleLambdaParser = 
    (do char '\\'
        spaces
        vl <- (endBy1 variableIdentifier spaces)
        char '.'
        spaces
        e <- lambdaParser
        return $ foldr (\x f -> abstraction x f) e vl)
    <|> (do char '('
            e <- lambdaParser
            char ')'
            return e)
    <|> do v <- variableIdentifier
           return $ variable v

lambdaParser :: Parser Expression
lambdaParser = do l <- endBy1 singleLambdaParser spaces
                  return $ foldl1 (\f x -> application f x) l

parseLambda :: String -> Maybe Expression
parseLambda s = case parse lambdaParser "(unknown)" s of
                Right a -> Just a
                Left _ -> Nothing
