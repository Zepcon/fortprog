module Unify where

import Type
import Subs
import Vars

-- ds berechnet die Unstimmigkeitsmenge zweier Terme
-- und gibt sie als Paar zurück
-- Unstimmigkeitsmenge leer: Nothing zurückgeben
-- ?! Umgang mit anonymen Variablen (_)

ds :: Term -> Term -> Maybe (Term, Term)
ds (Var "_") v2 = Nothing
ds v1 (Var "_") = Nothing
ds (Var v1) (Var v2) = if v1 == v2 then Nothing
                                    else Just(Var v1, Var v2)
ds (Comb combName xs1) (Comb combName2 xs2) | length xs1 /= length xs2 = Just((Comb combName xs1),(Comb combName2 xs2))
                                            | combName /= combName2 = Just((Comb combName xs1),(Comb combName2 xs2))
                                            | otherwise = dsHelp xs1 xs2
ds t1 t2 = Just(t1,t2)

dsHelp :: [Term] -> [Term] -> Maybe (Term, Term)
dsHelp [] [] = Nothing
dsHelp (x1:xs1) (y1:ys1) = case ds x1 y1 of
                            Nothing -> dsHelp xs1 ys1
                            Just (x1,y2) -> Just (x1,y2)


v1 = (Var "A")
t1 = (Comb "f" [Var "A", Var "B", Var ""])
t2 = (Comb "f" [Var "A", Comb "e" [Var "A", Var "X"]])
t3 = (Comb "f" [Var "A", Comb "d" [Var "A", Var "C"]])
t4 = (Comb "f" [Var "A", Var "a", (Comb "g" [Var "B", Var "", Comb "h" [Var "C", Var "D", Comb "true" []]])])
t5 = (Comb "f" [Var "A", Var "a", (Comb "g" [Var "B", Var "_", Comb "h" [Var "C", Var "E", Comb "true" []]])])

z1 = (Comb "f" [Var "A", Var "a", (Comb "g" [Var "B", Var "_",                                     Comb "h" [Var "C", Var "D", Comb "true" []]])])
z2 = (Comb "f" [Var "A", Var "a", (Comb "g" [Var "B", Comb "h" [Var "C", Var "E", Comb "true" []], Comb "h" [Var "C", Var "E", Comb "true" []]])])

-- unify bestimmt aufbauend auf ds den allgemeinsten Unifikator für zwei Terme
-- sofern die beiden Terme unifizierbar sind
-- sonst: Nothing
unify :: Term -> Term -> Maybe Subst
unify t1 t2 = unifyHelp (Subst []) t1 t2

unifyHelp :: Subst -> Term -> Term -> Maybe Subst
unifyHelp sigma t1 t2 = case (ds (apply sigma t1) (apply sigma t2)) of
                         Nothing -> Just sigma
                         Just(Var v,t) -> if not (occur (Var v) t) then unifyHelp (compose((single v t)) sigma) t1 t2 else Nothing
                         Just(t1,t2) -> Nothing

occur :: Term -> Term -> Bool
occur (Var a) (Var b) = False
occur (Var a) x =  (elem a (allVars x))

