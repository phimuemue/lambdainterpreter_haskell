module Command where

import Expression

data Command = 
    SimpleExpression Expression
    | EmptyCmd
    | LetStmt String Expression deriving Show
