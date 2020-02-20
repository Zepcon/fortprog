module Unify where

import Type
-- import Subs

-- ds berechnet die Unstimmigkeitsmenge zweier Terme
-- und gibt sie als Paar zurück
-- Unstimmigkeitsmenge leer: Nothing zurückgeben
-- ?! Umgang mit anonymen Variablen (_)

ds :: Term -> Term -> Maybe (Term, Term)
-- data Term = Var VarName | Comb CombName [Term]
ds (Var v1) (Var v2) = if v1 == v2 then Nothing
           --           else undefined

-- Sind t; t0 Terme, dann ist ds(t; t0) definiert durch
-- 1. Falls t = t': ds(t; t') = Nothing;
-- 2. Falls t oder t' Variable und t != t': ds(t; t') = {t;t'}
-- 3. Falls t = f(t1;...; tn) und t' = g(s1;...; sm) (n;m >= 0):
--     Falls f != g oder m != n: ds(t; t') = {t;t'}
--     Falls f = g und m = n und ti = si für alle i < k und tk != sk: ds(t; t') =
--            ds(tk; sk)
-- Intuitiv bedeutet diese Definition: ds(t; t') enthält die Teilterme von t und t' an der
-- linkesten innersten Position, an denen t und t' verschieden sind.



-- unify bestimmt aufbauend auf ds den allgemeinsten Unifikator für zwei Terme
-- sofern die beiden Terme unifizierbar sind
-- sonst: Nothing
--unify :: Term -> Term -> Maybe Subst
