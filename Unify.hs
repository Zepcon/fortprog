module Unify where

import Type
-- import Subs

-- ds berechnet die Unstimmigkeitsmenge zweier Terme
-- und gibt sie als Paar zurück
-- Unstimmigkeitsmenge leer: Nothing zurückgeben
-- ?! Umgang mit anonymen Variablen (_)

ds :: Term -> Term -> Maybe (Term, Term)

ds (Var v1) (Var v2) = if v1 == v2 then Nothing
                                   else Just(v1,v2)
ds (Comb combName xs1) (Comb combName2 xs2) | length xs1 != length xs2 = Just((Comb combName xs1),(Comb combName2 xs2))
                                            | combName != combName2 = Just((Comb combName xs1),(Comb combName2 xs2))
                                            | otherwise --kleinster Index mit nicht-Nothing
ds t1 t2 = Just(t1,t2)


-- data Term = Var VarName | Comb CombName [Term]
-- 1. Fall: t = t'
-- Problem: Gleichheit von Termen überprüfen

-- 2. Fall: t oder t' Variablen und t != t'
-- ds(t; t') = {t;t'}

-- 3. Fall: t = f(t1;...; tn) und t' = g(s1;...; sm) und (n;m >= 0)
-- 3.1: f != g oder m != n
-- ds(t; t') = {t;t'}
-- 3.2: f = g und m = n und ti = si für alle i < k und tk != sk
-- ds(t; t') = ds(tk; sk)


-- Sind t; t0 Terme, dann ist ds(t; t0) definiert durch
-- 1. Falls t = t': ds(t; t') = Nothing;
-- 2. Falls t oder t' Variable und t != t': ds(t; t') = {t;t'}
-- 3. Falls t = f(t1;...; tn) und t' = g(s1;...; sm) und (n;m >= 0):
--     Falls f != g oder m != n: ds(t; t') = {t;t'}
--     Falls f = g und m = n und ti = si für alle i < k und tk != sk: ds(t; t') =
--            ds(tk; sk)
-- Intuitiv bedeutet diese Definition: ds(t; t') enthält die Teilterme von t und t' an der
-- linkesten innersten Position, an denen t und t' verschieden sind.



-- unify bestimmt aufbauend auf ds den allgemeinsten Unifikator für zwei Terme
-- sofern die beiden Terme unifizierbar sind
-- sonst: Nothing
--unify :: Term -> Term -> Maybe Subst
