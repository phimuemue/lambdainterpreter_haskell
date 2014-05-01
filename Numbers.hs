module Numbers where

import Expression

stringIsNumber :: String -> Bool
stringIsNumber "" = True --for simplicity, assume "" represents 0
stringIsNumber (h:t) = '0'<=h && h<='9' && stringIsNumber t

numToExpr :: Int -> Expression
numToExpr n = abstraction "f" $ abstraction "x" (aux n)
              where aux 0 = variable "x"
                    aux n = application (variable "f") $ aux (n-1)
