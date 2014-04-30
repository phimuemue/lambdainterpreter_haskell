module InteractiveTagger where

import Expression
import UserInput

import Data.Map as Map

confirmationMsg :: String -> [String] -> IO Char
confirmationMsg heading subs =
    getSingleKeyPress $
    "  " ++ heading ++
    concat ["\n   " ++ s | s <- subs]
    
confirmVariableSubs :: String -> Expression -> IO Expression
confirmVariableSubs v e =
    do c <- confirmationMsg ("Substitute " ++ v ++ "?") [v, show e]
       case c of
            'n' -> return $ variable v
            _   -> return e

confirmBetaReduction :: Environment -> Expression -> Expression -> IO Expression
confirmBetaReduction env f x =
    do c <- confirmationMsg "Beta-reduce?" [show f, show x]
       case c of
            'n' -> do f' <- internalIteractiveTags env f
                      x' <- internalIteractiveTags env x
                      return $ application f' x'
            _   -> return $ betaReduce $ application f x

confirmEtaReduction :: Environment -> String -> Expression -> IO Expression
confirmEtaReduction env x f =
    do c <- confirmationMsg "Eta-convert?" [show f, x]
       case c of
            'n' -> do f' <- internalIteractiveTags env f
                      return $ abstraction x f'
            _   -> return $ etaReduce $ abstraction x f
    

internalIteractiveTags :: Environment -> Expression -> IO Expression
internalIteractiveTags env expr = case expr of
    Variable _ v -> case Map.lookup v env of
                    Nothing -> return $ variable v
                    Just e -> confirmVariableSubs v e
    Application _ f x -> if betaDirectlyReducible expr
                         then confirmBetaReduction env f x
                         else (do f' <- internalIteractiveTags env f
                                  x' <- internalIteractiveTags env x
                                  return $ application f' x')
    Abstraction _ x f -> if etaDirectlyReducible expr
                         then confirmEtaReduction env x f
                         else (do f' <- internalIteractiveTags env f
                                  return $ abstraction x f')

interactiveTags :: Environment -> Expression -> IO Expression
interactiveTags env expr = do putStrLn $ "! " ++ (show expr)
                              internalIteractiveTags env expr
