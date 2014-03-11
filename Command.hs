module Command where

import Expression

data Command = 
    SimpleExpression Expression
    | LoadCmd String
    | EmptyCmd
    | LetStmt String Expression deriving Show
