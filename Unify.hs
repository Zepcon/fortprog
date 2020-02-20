module Unify where

import Type
import Subs

-- ds berechnet die Unstimmigkeitsmenge zweier Terme
-- und gibt sie als Paar zurück
-- Unstimmigkeitsmenge leer: Nothing zurückgeben
-- ?! Umgang mit anonymen Variablen (_)

ds :: Term -> Term -> Maybe (Term, Term)





-- unify bestimmt aufbauend auf ds den allgemeinsten Unifikator für zwei
-- sofern die beiden Terme unifizierbar sind
-- sonst: Nothing
unify :: Term -> Term -> Maybe Subst
