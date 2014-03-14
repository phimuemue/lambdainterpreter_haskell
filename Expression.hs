module Expression where

import Data.Char
import qualified Data.Map as Map
import Control.Monad
import Data.Maybe

data Expression = 
    Variable String
    | Abstraction String Expression
    | Application Expression Expression deriving (Eq, Ord)

instance Show Expression where
    show e = to_s False e where
             to_s plbd (Variable s) = s
             to_s plbd (Abstraction x f@(Abstraction y b)) = (if plbd then " " else "\\")++ x ++ to_s True f
             to_s plbd (Abstraction x f) = (if plbd then " " else "\\") ++ x ++ ". " ++ to_s True f
             to_s plbd (Application f@(Abstraction _ _) g@(Application _ _)) = "(" ++ to_s False f ++ ") (" ++ to_s False g ++")"
             to_s plbd (Application f@(Abstraction _ _) g@(Abstraction _ _)) = "(" ++ to_s False f ++ ") (" ++ to_s False g ++")"
             to_s plbd (Application f@(Abstraction _ _) x) = "(" ++ to_s False f ++ ") " ++ to_s False x
             to_s plbd (Application f g@(Application _ _)) = to_s False f ++ " (" ++ to_s False g ++")"
             to_s plbd (Application f g@(Abstraction _ _)) = to_s False f ++ " (" ++ to_s False g ++")"
             to_s plbd (Application f x) = to_s False f ++ " " ++ to_s False x

nextVarName [] = "a"
nextVarName (h:t) = if ord h == ord 'z'
                  then 'a':(nextVarName t)
                  else chr (1 + ord h):t

containsVariable term v = case term of
    Variable x -> v == x
    Abstraction _ f -> containsVariable f v
    Application f x -> containsVariable f v || containsVariable x v

freeVariables term = 
    aux term [] where
    aux term bound_vars = case term of
        Variable v -> if v `elem` bound_vars then [] else [v]
        Abstraction x f -> aux f (x:bound_vars)
        Application f x -> (aux f bound_vars)++(aux x bound_vars)

usedVariables term =
    aux [] term where
    aux acc term = case term of
        Variable x -> x:acc
        Abstraction _ f -> aux acc f
        Application f x -> aux (aux acc f) x

unusedVariable term =
    checkVarname used "a" where
    used = usedVariables term
    checkVarname used nvn = if nvn `elem` used 
                            then checkVarname used (nextVarName nvn)
                            else nvn

replaceVariable term v subs = case term of
    Variable x -> if x==v then subs else Variable x
    Application f x -> Application (replaceVariable f v subs) (replaceVariable x v subs)
    Abstraction x f -> if v == nx then Abstraction x f else Abstraction nx (replaceVariable nf v subs)
                       where Abstraction nx nf = if x `elem` (freeVariables subs) 
                                                 then let nv = unusedVariable f in Abstraction nv (replaceVariable f x (Variable nv))
                                                 else Abstraction x f

alphaEquiv :: Expression -> Expression -> Bool
alphaEquiv e1 e2 =
    aux e1 e2 (Map.fromList []) (Map.fromList []) where
    aux :: Expression -> Expression -> Map.Map String String -> Map.Map String String -> Bool
    aux e1 e2 cn1 cn2 = case (e1, e2) of
        (Variable x1, Variable x2) -> (Map.findWithDefault x1 x1 cn1) == (Map.findWithDefault x2 x2 cn2)
        (Application f1 x1, Application f2 x2) -> (aux f1 f2 cn1 cn2) && (aux x1 x2 cn1 cn2)
        (Abstraction x1 f1, Abstraction x2 f2) -> aux f1 f2 nn1 nn2 where
                                                  y = show $ Map.size cn1
                                                  nn1 = Map.insert x1 y cn1
                                                  nn2 = Map.insert x2 y cn2
        (_, _) -> False

betaReducible term = case term of
    Variable _ -> False
    e@(Application (Abstraction _ _) _) -> not (alphaEquiv e (betaReduce e))
    Application f x -> (betaReducible f) || (betaReducible x)
    Abstraction x f -> betaReducible f

betaReduce term = case term of
    Application (Abstraction x f) y -> replaceVariable f x y
    Application f x -> if betaReducible f
                       then Application (betaReduce f) x
                       else Application f (betaReduce x)
    Abstraction x f -> Abstraction x (betaReduce f)
    e -> e

etaReducible term = case term of
    Variable _ -> False
    Abstraction x (Application _ (Variable y)) -> x==y
    Abstraction _ f -> etaReducible f
    Application f x -> (etaReducible f) || (etaReducible x)

etaReduce term = case term of
    --e@(Abstraction x (Application (Variable f) (Variable y))) -> if (x==y && f/=y) then (Variable f) else Abstraction x (etaReduce e)
    e@(Abstraction x (Application f (Variable y))) -> if x==y then f else Abstraction x (etaReduce e)
    Abstraction x f -> Abstraction x (etaReduce f)
    Application f x -> if etaReducible f
                         then Application (etaReduce f) x
                         else Application f (etaReduce x)
    e -> e

applyAbbreviations term env = case term of
   Application f x -> Application (applyAbbreviations f env) (applyAbbreviations x env)
   Variable v -> (case Map.lookup v env of
                       Just subs -> subs
                       Nothing -> Variable v)
   Abstraction x f -> (case Map.lookup x env of
                            Just _ -> Abstraction x f
                            Nothing -> Abstraction x (applyAbbreviations f env))

-- TODO: Make this nicer!
simplifyStep term env =
   let abbrevsExpanded = applyAbbreviations term env in
   if abbrevsExpanded /= term 
   then abbrevsExpanded 
   else
   if betaReducible term 
   then betaReduce term 
   else if etaReducible term 
   then etaReduce term
   else abbrevsExpanded

simplifyComplete term env =
    let newexpr = simplifyStep term env in
    if alphaEquiv newexpr term 
    then newexpr
    else simplifyComplete newexpr env
