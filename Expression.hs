module Expression where

import Data.Char
import qualified Data.Map as Map

data Expression = 
    -- Variable may be expandable (by abbrev. from environment)
    Variable (Maybe Expression) String
    -- Abstractions may be eta-convertible
    | Abstraction Bool String Expression
    -- Applications may be beta-convertible
    | Application Bool Expression Expression deriving (Eq, Ord)

-- smart constructors for expressions
variable :: String -> Expression
variable x = Variable Nothing x
abstraction :: String -> Expression -> Expression
abstraction x f = Abstraction False x f
application :: Expression -> Expression -> Expression
application f x = Application False f x

instance Show Expression where
    show e = to_s False e where
        to_s plbd term = case term of
            Variable _ s -> s
            Abstraction _ x f@(Abstraction _ _ _) -> 
               (if plbd then " " else "\\")++ x ++ to_s True f
            Abstraction _ x f -> 
               (if plbd then " " else "\\") ++ x ++ ". " ++ to_s True f
            Application _ f@(Abstraction _ _ _) g@(Application _ _ _) -> 
               "(" ++ to_s False f ++ ") (" ++ to_s False g ++")"
            Application _ f@(Abstraction _ _ _) g@(Abstraction _ _ _) ->
                "(" ++ to_s False f ++ ") (" ++ to_s False g ++")"
            Application _ f@(Abstraction _ _ _) x -> 
               "(" ++ to_s False f ++ ") " ++ to_s False x
            Application _ f g@(Application _ _ _) ->
               to_s False f ++ " (" ++ to_s False g ++")"
            Application _ f g@(Abstraction _ _ _) ->
               to_s False f ++ " (" ++ to_s False g ++")"
            Application _ f x ->
               to_s False f ++ " " ++ to_s False x

