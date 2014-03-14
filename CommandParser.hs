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

setParser = do string "set"
               spaces
               flag <- many1 (noneOf " ")
               return $ SetCmd flag

commandParser =     (try bindingParser)
                <|> (try loadParser)
                <|> (try setParser)
                <|> (try simpleExpressionParser)

parseCommand s = case parse commandParser "(unknown)" s of
                   Right e -> Just e
                   Left _ -> Nothing
