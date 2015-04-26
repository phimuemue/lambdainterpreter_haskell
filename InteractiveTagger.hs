module InteractiveTagger where

import Expression
import Settings
import UserInput
import Taggers

confirmationMsg :: String -> [String] -> IO Char
confirmationMsg heading subs =
    getSingleKeyPress $ "  " ++ heading ++ concat (map ("\n  " ++) subs)
    
confirmVariableSubs :: String -> Expression -> IO TaggedExpression
confirmVariableSubs v e =
    do c <- confirmationMsg ("Substitute " ++ v ++ "?") [v, show e]
       case c of
            'n' -> return $ Nothing
            _   -> return $ Just e

confirmBetaReduction :: Settings -> Expression -> Expression -> IO TaggedExpression
confirmBetaReduction settings f x =
    do c <- confirmationMsg "Beta-reduce?" [show f, show x]
       case c of
            'n' -> do f' <- internalIteractiveTags settings f
                      x' <- internalIteractiveTags settings x
                      return $ lrApplication False f f' x x'
            _   -> return $ Just $ betaReduce $ application f x

confirmEtaReduction :: Settings -> String -> Expression -> IO TaggedExpression
confirmEtaReduction settings x f =
    do c <- confirmationMsg "Eta-convert?" [show f, x]
       case c of
            'n' -> do f' <- internalIteractiveTags settings f
                      return $ lrAbstraction False x f f'
            _   -> return $ Just $ etaReduce $ abstraction x f
    

internalIteractiveTags :: Settings -> Expression -> IO TaggedExpression
internalIteractiveTags settings expr = case expr of
    Variable _ v -> case variableAbbreviationTag settings expr of
                    Nothing -> return $ Nothing
                    Just e -> confirmVariableSubs v e
    Application _ f x -> if betaDirectlyReducible expr
                         then confirmBetaReduction settings f x
                         else (do f' <- internalIteractiveTags settings f
                                  x' <- internalIteractiveTags settings x
                                  return $ lrApplication False f f' x x')
    Abstraction _ x f -> if etaDirectlyReducible expr
                         then confirmEtaReduction settings x f
                         else (do f' <- internalIteractiveTags settings f
                                  return $ lrAbstraction False x f f')

interactiveTags :: Settings -> Expression -> IO TaggedExpression
interactiveTags settings expr = do putStrLn $ "! " ++ (show expr)
                                   internalIteractiveTags settings expr
