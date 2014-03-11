module Command where

import Expression

data Command = 
    SimpleExpression Expression
    | LetStmt String Expression deriving Show
