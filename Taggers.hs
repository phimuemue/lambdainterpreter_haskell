module Taggers where

import Expression
import Numbers
import Settings

import Data.Map as Map
import Data.Char
import Data.Maybe
import Data.Either.Combinators

data TaggedExpression = Either Expression Expression

-- we need functions that automatically combines TaggedExpressions into new ones

lrApplication :: Bool -> Either Expression Expression -> Either Expression Expression -> Either Expression Expression
lrApplication b (Right f) (Right x) = Right $ Application b f x
lrApplication b (Left f) (Right x) = Right $ Application b f x
lrApplication b (Right f) (Left x) = Right $ Application b f x
lrApplication b (Left f) (Left x)  = Left $ Application b f x

lrAbstraction :: Bool -> String -> Either Expression Expression -> Either Expression Expression
lrAbstraction b x (Right f) = Right $ Abstraction b x f
lrAbstraction b x (Left f) = Left $ Abstraction b x f

-- tagging functions: Take an untagged expression and return maybe a tagged one

variableAbbreviationTag :: Settings -> Expression -> Either Expression Expression
variableAbbreviationTag settings (Variable b v) = 
    if knowNumbers settings && stringIsNumber v
    then Right $ numToExpr $ read v
    else if isJust v' then Right $ fromJust v' else Left $ Variable b v 
    where v' = Map.lookup v $ environment settings
variableAbbreviationTag _ _ = undefined


allAbbreviationTags :: Settings -> Expression -> Either Expression Expression
allAbbreviationTags settings term = case term of
    Application b f x -> lrApplication b f' x'
                         where f' = allAbbreviationTags settings f
                               x' = allAbbreviationTags settings x
    Variable _ v -> variableAbbreviationTag settings term
    Abstraction b x f -> if x `Map.member` environment settings
                         -- TODO: This is currently a bug!
                         -- if the argument to a function has same name as a 
                         -- defined macro, nothing is done within the body
                         -- of the function. However, there might be other
                         -- macro names in the functions that could be expanded
                         then Left term
                         else if isRight f'
                              then Right $ Abstraction b x (fromRight' f')
                              else Left term
                         where f' = allAbbreviationTags settings f

allOutermostTags :: Expression -> Expression
allOutermostTags term = case term of
    -- eta-reducible abstraction
    Abstraction _ x e@(Application _ _ (Variable _ y)) -> 
        if x==y
        then Abstraction True x e
        else Abstraction False x $ allOutermostTags e
    -- beta-reducible application
    Application _ e@(Abstraction _ _ _) y -> 
        Application True e y
    -- "normal stuff"
    Variable _ x -> variable x
    Abstraction _ x f -> Abstraction False x $ allOutermostTags f
    Application _ f x -> Application False tf tx
                         where tf = allOutermostTags f
                               tx = allOutermostTags x

normalOrderTags :: Expression -> Either Expression Expression
normalOrderTags term = case term of
    Application _ e@(Abstraction _ _ _) y -> Right $ Application True e y
    Application _ f x -> if isRight f'
                         then lrApplication False f' (Left x)
                         else lrApplication False f' x'
                         where f' = normalOrderTags f
                               x' = normalOrderTags x
    Abstraction _ x f -> lrAbstraction False x f'
                         where f' = normalOrderTags f
    e -> Left e

-- applyTags takes a (possibly) tagged expression and returns an untagged one
applyTags :: Expression -> Expression
applyTags term = case term of
    Variable Nothing x -> variable x
    Variable (Just e) _ -> e
    Application False f x -> application (applyTags f) (applyTags x)
    Application True (Abstraction _ x f) y -> replaceVariable f x y
    Application True f x -> error $ "Disallowed tagged application.\n" ++ (show f) ++ "\n" ++ (show x)
    Abstraction False x f -> abstraction x (applyTags f)
    Abstraction True _ (Application _ f _) -> applyTags f
    Abstraction True _ _ -> error "Disallowed tagged abstraction."

nextVarName :: String -> String
nextVarName [] = "a"
nextVarName (h:t) = if ord h == ord 'z'
                  then 'a':(nextVarName t)
                  else chr (1 + ord h):t

containsVariable :: Expression -> String -> Bool
containsVariable term v = case term of
    Variable _ x -> v == x
    Abstraction _ _ f -> containsVariable f v
    Application _ f x -> containsVariable f v || containsVariable x v

freeVariables :: Expression -> [String]
freeVariables term =
    aux term [] where
    aux t bound_vars = case t of
        Variable _ v -> if v `elem` bound_vars then [] else [v]
        Abstraction _ x f -> aux f (x:bound_vars)
        Application _ f x -> (aux f bound_vars)++(aux x bound_vars)

usedVariables :: Expression -> [String]
usedVariables term =
    aux [] term where
    aux acc t = case t of
        Variable _ x -> x:acc
        Abstraction _ _ f -> aux acc f
        Application _ f x -> aux (aux acc f) x

unusedVariable :: Expression -> String
unusedVariable term =
    checkVarname usedVars "a" where
    usedVars = usedVariables term
    checkVarname used nvn = if nvn `elem` used 
                            then checkVarname used (nextVarName nvn)
                            else nvn

replaceVariable :: Expression -> String -> Expression -> Expression
replaceVariable term v subs = case term of
    Variable _ x -> if x==v then subs else variable x
    Application _ f x -> application 
                         (replaceVariable f v subs)
                         (replaceVariable x v subs)
    Abstraction _ x f -> if v == nx 
                         then abstraction x f 
                         else abstraction nx (replaceVariable nf v subs)
                         where xIsFree = x `elem` freeVariables subs
                               nv = unusedVariable f
                               nx = if xIsFree then nv else x
                               nf = if xIsFree 
                                    then replaceVariable f x (variable nv) 
                                    else f

alphaEquiv :: Expression -> Expression -> Bool
alphaEquiv e f =
    aux e f (Map.fromList []) (Map.fromList []) where
    aux :: Expression -> Expression -> 
           Map.Map String String -> Map.Map String String -> 
           Bool
    aux e1 e2 cn1 cn2 = case (e1, e2) of
        (Variable _ x1, Variable _ x2) -> 
            (findVarMapping x1 x1 cn1) == (findVarMapping x2 x2 cn2)
        (Application _ f1 x1, Application _ f2 x2) -> 
            (aux f1 f2 cn1 cn2) && (aux x1 x2 cn1 cn2)
        (Abstraction _ x1 f1, Abstraction _ x2 f2) -> aux f1 f2 nn1 nn2 where
                                                  y = show $ Map.size cn1
                                                  nn1 = Map.insert x1 y cn1
                                                  nn2 = Map.insert x2 y cn2
        (_, _) -> False
        where findVarMapping = Map.findWithDefault

betaDirectlyReducible :: Expression -> Bool
betaDirectlyReducible term = case term of
    Application _ (Abstraction _ _ _) _ -> True
    _ -> False

betaReduce :: Expression -> Expression
betaReduce term = applyTags $ allOutermostTags term

etaDirectlyReducible :: Expression -> Bool
etaDirectlyReducible term = case term of
    Abstraction _ x (Application _ (Variable _ z) (Variable _ y)) 
        -> x==y && y/=z
    Abstraction _ x (Application _ _ (Variable _ y)) -> x==y
    _ -> False

etaReducible :: Expression -> Bool
etaReducible term = case term of
    Variable _ _ -> False
    Abstraction _ x (Application _ (Variable _ z) (Variable _ y)) 
        -> x==y && y/=z
    Abstraction _ x (Application _ _ (Variable _ y)) -> x==y
    Abstraction _ _ f -> etaReducible f
    Application _ f x -> (etaReducible f) || (etaReducible x)

etaReduce :: Expression -> Expression
etaReduce term = allOutermostTags $ aux term
    where aux t = case t of
                     Abstraction _ x (Application _ f v@(Variable _ y)) -> 
                        if (x==y) && (not $ x `elem` freeVariables f) 
                        then f 
                        else abstraction x (etaReduce (application f v))
                     Abstraction _ x f -> abstraction x (etaReduce f)
                     Application _ f x -> if etaReducible f
                                          then application (etaReduce f) x
                                          else application f (etaReduce x)
                     e -> e

containsAbbreviations :: Expression -> Environment -> Bool
containsAbbreviations term env = case term of
   Application _ f x -> 
       (containsAbbreviations f env) || (containsAbbreviations x env)
   Variable _ v -> v `Map.member` env
   Abstraction _ x f -> if x `Map.member` env 
                        then containsAbbreviations f $ Map.delete x env
                        else containsAbbreviations f env

applyAbbreviations :: Expression -> Environment -> Expression
applyAbbreviations term env = case term of
   Application b f x -> Application b 
                        (applyAbbreviations f env)
                        (applyAbbreviations x env)
   Variable _ v -> (case Map.lookup v env of
                       Just subs -> subs
                       Nothing -> variable v)
   Abstraction b x f -> (if x `Map.member` env
                         then Abstraction b x f
                         else Abstraction b x (applyAbbreviations f env))

