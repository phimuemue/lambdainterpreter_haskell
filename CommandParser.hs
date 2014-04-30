module CommandParser where

import Text.ParserCombinators.Parsec

import Parser
import Command

bindingParser :: Parser Command
bindingParser = do string "let"
                   spaces
                   name <- many1 (noneOf ".\\() =")
                   spaces
                   char '='
                   spaces
                   expr <- lambdaParser
                   return $ LetStmt name expr

simpleExpressionParser :: Parser Command
simpleExpressionParser = do expr <- lambdaParser
                            return $ SimpleExpression expr

loadParser :: Parser Command
loadParser = do string "load"
                spaces
                name <- many1 (noneOf " ")
                return $ LoadCmd name

setParser :: Parser Command
setParser = do string "set"
               spaces
               flag <- many1 (noneOf " ")
               return $ SetCmd flag

-- TODO: Improve commandParser (i.e. not so many try's)
commandParser :: Parser Command
commandParser =     (try bindingParser)
                <|> (try loadParser)
                <|> (try setParser)
                <|> (try simpleExpressionParser)

parseCommand :: String -> Maybe Command
parseCommand s = case parse commandParser "(unknown)" s of
                   Right e -> Just e
                   Left _ -> Nothing
