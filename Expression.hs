module Expression where

import Data.Char
import qualified Data.Map as Map
import Control.Monad
import Data.Maybe

data Expression = 
    Variable String
    -- abstractions may be eta-convertible
    | Abstraction Bool String Expression
    -- applications may be beta-convertible
    | Application Bool Expression Expression deriving (Eq, Ord)

-- smart constructors for expressions
variable x = Variable x
abstraction x f = Abstraction False x f
application f x = Application False f x

instance Show Expression where
    show e = to_s False e where
        to_s plbd term = case term of
            Variable s -> s
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

allTags :: Expression -> Expression
allTags term = case term of
    -- eta-reducible abstraction
    Abstraction _ x e@(Application _ f (Variable y)) -> 
        Abstraction (x==y) x $ allTags e
    -- beta-reducible application
    Application _ e@(Abstraction _ x f) y -> 
        Application True (allTags e) (allTags y)
    -- "normal stuff"
    Variable x -> Variable x
    Abstraction _ x f -> Abstraction False x $ allTags f
    Application _ f x -> Application False tf tx
                         where tf = allTags f
                               tx = allTags x

normalOrderTags :: Expression -> Expression
normalOrderTags term = case term of
    Application _ e@(Abstraction _ x f) y -> Application True e y
    Application _ f x -> if betaReducible f
                         then application (normalOrderTags f) x
                         else application f (normalOrderTags x)
    Abstraction _ x f -> abstraction x (normalOrderTags f)
    e -> e

-- applyTags takes a (possibly) tagged expression and returns an untagged one
applyTags :: Expression -> Expression
applyTags term = case term of
    Variable x -> Variable x
    Application False f x -> application (applyTags f) (applyTags x)
    Application True (Abstraction _ x f) y -> replaceVariable f x y
    Application True _ y -> error "Tagged application without abstraction as first part."
    Abstraction False x f -> abstraction x (applyTags f)
    Abstraction True x (Application _ f y) -> applyTags f
    Abstraction True x _ -> error "Tagged abstraction without application as second part."

nextVarName [] = "a"
nextVarName (h:t) = if ord h == ord 'z'
                  then 'a':(nextVarName t)
                  else chr (1 + ord h):t

containsVariable term v = case term of
    Variable x -> v == x
    Abstraction _ _ f -> containsVariable f v
    Application _ f x -> containsVariable f v || containsVariable x v

freeVariables term = 
    aux term [] where
    aux term bound_vars = case term of
        Variable v -> if v `elem` bound_vars then [] else [v]
        Abstraction _ x f -> aux f (x:bound_vars)
        Application _ f x -> (aux f bound_vars)++(aux x bound_vars)

usedVariables term =
    aux [] term where
    aux acc term = case term of
        Variable x -> x:acc
        Abstraction _ _ f -> aux acc f
        Application _ f x -> aux (aux acc f) x

unusedVariable term =
    checkVarname used "a" where
    used = usedVariables term
    checkVarname used nvn = if nvn `elem` used 
                            then checkVarname used (nextVarName nvn)
                            else nvn

replaceVariable term v subs = case term of
    Variable x -> if x==v then subs else Variable x
    Application _ f x -> application (replaceVariable f v subs) (replaceVariable x v subs)
    Abstraction _ x f -> if v == nx then abstraction x f else abstraction nx (replaceVariable nf v subs)
                         where Abstraction False nx nf = if x `elem` (freeVariables subs) 
                                                 then let nv = unusedVariable f in abstraction nv (replaceVariable f x (Variable nv))
                                                 else abstraction x f

alphaEquiv :: Expression -> Expression -> Bool
alphaEquiv e1 e2 =
    aux e1 e2 (Map.fromList []) (Map.fromList []) where
    aux :: Expression -> Expression -> Map.Map String String -> Map.Map String String -> Bool
    aux e1 e2 cn1 cn2 = case (e1, e2) of
        (Variable x1, Variable x2) -> (Map.findWithDefault x1 x1 cn1) == (Map.findWithDefault x2 x2 cn2)
        (Application _ f1 x1, Application _ f2 x2) -> (aux f1 f2 cn1 cn2) && (aux x1 x2 cn1 cn2)
        (Abstraction _ x1 f1, Abstraction _ x2 f2) -> aux f1 f2 nn1 nn2 where
                                                  y = show $ Map.size cn1
                                                  nn1 = Map.insert x1 y cn1
                                                  nn2 = Map.insert x2 y cn2
        (_, _) -> False

betaReducible term = case term of
    Variable _ -> False
    e@(Application _ (Abstraction _ _ _) _) -> not (alphaEquiv e (betaReduce e))
    Application _ f x -> (betaReducible f) || (betaReducible x)
    Abstraction _ x f -> betaReducible f

betaReduce term = applyTags $ normalOrderTags term

--betaReduce term = normalOrderTags $ aux term
--    where aux term = case term of
--                     Application _ (Abstraction _ x f) y -> replaceVariable f x y
--                     Application _ f x -> if betaReducible f
--                                        then application (betaReduce f) x
--                                        else application f (betaReduce x)
--                     Abstraction _ x f -> abstraction x (betaReduce f)
--                     e -> e

etaReducible term = case term of
    Variable _ -> False
    Abstraction _ x (Application _ _ (Variable y)) -> x==y
    Abstraction _ _ f -> etaReducible f
    Application _ f x -> (etaReducible f) || (etaReducible x)

etaReduce term = normalOrderTags $ aux term
    where aux term = case term of
                     e@(Abstraction _ x (Application _ f (Variable y))) -> if (x==y) && (not $ x `elem` freeVariables f) 
                                                                       then f 
                                                                       else abstraction x (etaReduce (application f (Variable y)))
                     Abstraction _ x f -> abstraction x (etaReduce f)
                     Application _ f x -> if etaReducible f
                                          then application (etaReduce f) x
                                          else application f (etaReduce x)
                     e -> e

containsAbbreviations term env = case term of
   Application _ f x -> (containsAbbreviations f env) || (containsAbbreviations x env)
   Variable v -> v `Map.member` env
   Abstraction _ x f -> if x `Map.member` env 
                        then containsAbbreviations f $ Map.delete x env
                        else containsAbbreviations f env

applyAbbreviations term env = case term of
   Application b f x -> Application b (applyAbbreviations f env) (applyAbbreviations x env)
   Variable v -> (case Map.lookup v env of
                       Just subs -> subs
                       Nothing -> Variable v)
   Abstraction b x f -> (case Map.lookup x env of
                            Just _ -> Abstraction b x f
                            Nothing -> Abstraction b x (applyAbbreviations f env))

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
