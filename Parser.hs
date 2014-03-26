module Parser where

import Text.ParserCombinators.Parsec

import Expression

variableParser :: Parser Expression
variableParser = do r <- many1 (noneOf " .\\()")
                    return (Variable r)

applicationParser :: Parser Expression
applicationParser = do content <- endBy1 vl spaces
                       return $ foldl1 (\a b -> application a b) content
                    where vl = (try variableParser <|> try parensLambdaParser)
                        
abstractionParser :: Parser Expression
abstractionParser = do char '\\'
                       spaces
                       varlist <- (endBy1 variableParser spaces)
                       char '.'
                       spaces
                       body <- lambdaParser
                       spaces
                       return $ foldr (\(Variable v) a -> abstraction v a) 
                                body varlist

parensLambdaParser :: Parser Expression
parensLambdaParser = do char '('
                        spaces
                        content <- lambdaParser
                        spaces
                        char ')'
                        _ <- spaces
                        return content

lambdaParser :: Parser Expression
lambdaParser = (try applicationParser) 
               <|> (try variableParser)
               <|> (try abstractionParser)
               <|> (try parensLambdaParser)

parseLambda s = case parse lambdaParser "(unknown)" s of
                Right a -> Just a
                Left _ -> Nothing
