module InteractiveTagger where

import Expression
import Settings
import UserInput
import Taggers

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

confirmBetaReduction :: Settings -> Expression -> Expression -> IO Expression
confirmBetaReduction settings f x =
    do c <- confirmationMsg "Beta-reduce?" [show f, show x]
       case c of
            'n' -> do f' <- internalIteractiveTags settings f
                      x' <- internalIteractiveTags settings x
                      return $ application f' x'
            _   -> return $ betaReduce $ application f x

confirmEtaReduction :: Settings -> String -> Expression -> IO Expression
confirmEtaReduction settings x f =
    do c <- confirmationMsg "Eta-convert?" [show f, x]
       case c of
            'n' -> do f' <- internalIteractiveTags settings f
                      return $ abstraction x f'
            _   -> return $ etaReduce $ abstraction x f
    

internalIteractiveTags :: Settings -> Expression -> IO Expression
internalIteractiveTags settings expr = case expr of
    Variable _ v -> case variableAbbreviationTag settings expr of
                    Nothing -> return $ variable v
                    Just e -> confirmVariableSubs v e
    Application _ f x -> if betaDirectlyReducible expr
                         then confirmBetaReduction settings f x
                         else (do f' <- internalIteractiveTags settings f
                                  x' <- internalIteractiveTags settings x
                                  return $ application f' x')
    Abstraction _ x f -> if etaDirectlyReducible expr
                         then confirmEtaReduction settings x f
                         else (do f' <- internalIteractiveTags settings f
                                  return $ abstraction x f')

interactiveTags :: Settings -> Expression -> IO Expression
interactiveTags settings expr = do putStrLn $ "! " ++ (show expr)
                                   internalIteractiveTags settings expr
