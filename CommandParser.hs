module CommandParser where

import Text.ParserCombinators.Parsec

import Parser
import Command

bindingParser = do string "let"
                   spaces
                   name <- many1 (noneOf ".\\() =")
                   spaces
                   char '='
                   spaces
                   expr <- lambdaParser
                   return $ LetStmt name expr

simpleExpressionParser = do expr <- lambdaParser
                            return $ SimpleExpression expr

loadParser = do string "load"
                spaces
                name <- many1 (noneOf " ")
                return $ LoadCmd name

commandParser =     (try bindingParser)
                <|> (try loadParser)
                <|> (try simpleExpressionParser)

parseCommand s = case parse commandParser "(unknown)" s of
                   Right e -> Just e
                   Left _ -> Nothing
