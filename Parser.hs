module Parser where

import Text.ParserCombinators.Parsec

import Expression

variableParser :: Parser Expression
variableParser = do r <- many1 (noneOf " .\\()")
                    return (Variable r)

applicationParser :: Parser Expression
applicationParser = do content <- sepBy1 (try variableParser <|> try parensLambdaParser) spaces
                       return $ foldl1 (\a b -> Application a b) content
                        
abstractionParser :: Parser Expression
abstractionParser = do char '\\'
                       spaces
                       varlist <- (sepBy1 variableParser spaces)
                       spaces
                       char '.'
                       spaces
                       body <- lambdaParser
                       return $ foldr (\(Variable v) a -> Abstraction v a) body varlist

parensLambdaParser :: Parser Expression
parensLambdaParser = do char '('
                        spaces
                        content <- lambdaParser
                        spaces
                        char ')'
                        return content

lambdaParser :: Parser Expression
lambdaParser =     (try applicationParser) 
               <|> (try variableParser)
               <|> (try abstractionParser)
               <|> (try parensLambdaParser)

parseLambda s = case parse lambdaParser "(unknown)" s of
                Right a -> Just a
                Left _ -> Nothing
