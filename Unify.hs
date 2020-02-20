module Unify where

import Type
-- import Subs

-- ds berechnet die Unstimmigkeitsmenge zweier Terme
-- und gibt sie als Paar zurück
-- Unstimmigkeitsmenge leer: Nothing zurückgeben
-- ?! Umgang mit anonymen Variablen (_)

ds :: Term -> Term -> Maybe (Term, Term)

ds (Var v1) (Var v2) = if v1 == v2 then Nothing
                                   else Just((Var v1),(Var v2))
ds (Comb combName1 xs1) (Comb combName2 xs2) | length xs1 /= length xs2 = Just((Comb combName1 xs1),(Comb combName2 xs2))
                                             | combName1  /= combName2  = Just((Comb combName1 xs1),(Comb combName2 xs2))
                                             | otherwise = undefined --kleinster Index mit nicht-Nothing

ds t1 t2 = Just(t1,t2)


-- data Term = Var VarName | Comb CombName [Term]
-- 1. Fall: t = t'
-- Problem: Gleichheit von Termen überprüfen

-- 3. Fall: t = f(t1;...; tn) und t' = g(s1;...; sm) und (n;m >= 0)
-- 3.2: f = g und m = n und ti = si für alle i < k und tk != sk
-- ds(t; t') = ds(tk; sk)
-- Intuitiv bedeutet diese Definition: ds(t; t') enthält die Teilterme von t und t' an der
-- linkesten innersten Position, an denen t und t' verschieden sind.



-- unify bestimmt aufbauend auf ds den allgemeinsten Unifikator für zwei Terme
-- sofern die beiden Terme unifizierbar sind
-- sonst: Nothing
--unify :: Term -> Term -> Maybe Subst
