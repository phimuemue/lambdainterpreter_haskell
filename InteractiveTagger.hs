module InteractiveTagger where

import Expression
import UserInput

import Data.Map as Map

confirmVariableSubs :: String -> Expression -> IO Expression
confirmVariableSubs v e =
    do c <- getSingleKeyPress $ "  Substitute " ++ v ++ "?"
       case c of
            'n' -> return $ variable v
            _   -> return e

confirmBetaReduction :: (Map.Map String Expression) -> Expression -> Expression -> IO Expression
confirmBetaReduction env f x =
    do c <- getSingleKeyPress $ "  Beta-reduce?\n  " ++ (show f) ++ "\n  " ++ (show x)
       case c of
            'n' -> do f' <- internalIteractiveTags env f
                      x' <- internalIteractiveTags env x
                      return $ application f' x'
            _   -> return $ betaReduce $ application f x

confirmEtaReduction :: (Map.Map String Expression) -> String -> Expression -> IO Expression
confirmEtaReduction env x f =
    do c <- getSingleKeyPress $ "  Eta-convert?\n  " ++ (show f) ++ "\n  " ++ x
       case c of
            'n' -> do f' <- internalIteractiveTags env f
                      return $ abstraction x f'
            _   -> return $ etaReduce $ abstraction x f
    

internalIteractiveTags :: (Map.Map String Expression) -> Expression -> IO Expression
internalIteractiveTags env expr = case expr of
    Variable _ v -> case Map.lookup v env of
                    Nothing -> return $ variable v
                    Just e -> confirmVariableSubs v e
    Application _ f x -> if betaDirectlyReducible expr
                         then confirmBetaReduction env f x
                         else (do f' <- internalIteractiveTags env f
                                  x' <- internalIteractiveTags env x
                                  return $ application f' x')
    Abstraction _ x f -> if etaReducible expr
                         then confirmEtaReduction env x f
                         else (do f' <- internalIteractiveTags env f
                                  return $ abstraction x f')

interactiveTags :: (Map.Map String Expression) -> Expression -> IO Expression
interactiveTags env expr = do putStrLn $ "! " ++ (show expr)
                              internalIteractiveTags env expr
