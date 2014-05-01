module Numbers where

import Expression

numToExpr :: Int -> Expression
numToExpr n = abstraction "f" $ abstraction "x" (aux n)
              where aux 0 = variable "x"
                    aux n = application (variable "f") $ aux (n-1)
