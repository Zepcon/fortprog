module Unify(ds,unify) where

import Type
import Subst
import Vars

-- ds berechnet die Unstimmigkeitsmenge zweier Terme
-- und gibt sie als Paar zurück
-- Unstimmigkeitsmenge leer: Nothing zurückgeben
ds :: Term -> Term -> Maybe (Term, Term)
ds (Var "_") _ = Nothing  -- anonyme Variablen können alles sein, also auch jeder Term, also Nothing
ds _ (Var "_") = Nothing  -- same here
ds (Var v1) (Var v2) = if v1 == v2 then Nothing  -- gleiche Variable
                                    else Just(Var v1, Var v2)  -- ungleiche Variable, also ds
ds (Comb combName xs1) (Comb combName2 xs2) | length xs1 /= length xs2 = Just((Comb combName xs1),(Comb combName2 xs2))  -- ungleiche Länge impliziert ds
                                            | combName /= combName2 = Just((Comb combName xs1),(Comb combName2 xs2))  -- ungleiche Namen ebenso
                                            | otherwise = dsHelp xs1 xs2  -- weiter die Termlisten checken
ds t1 t2 = Just(t1,t2)  -- letzte Möglichkeit entspricht ds

-- checke für Termlisten, ob diese ds bilden könnten
-- zipwith, find, msum
dsHelp :: [Term] -> [Term] -> Maybe (Term, Term)
dsHelp _ [] = Nothing
dsHelp [] _ = Nothing
dsHelp (x1:xs1) (y1:ys1) = case ds x1 y1 of
                            Nothing -> dsHelp xs1 ys1  -- so weit, so gut
                            Just (x2,y2) -> Just (x2,y2)  -- ds gefunden

-- unify bestimmt aufbauend auf ds den allgemeinsten Unifikator für zwei Terme
-- sofern die beiden Terme unifizierbar sind
-- sonst: Nothing
unify :: Term -> Term -> Maybe Subst
unify t1 t2 = unifyHelp (empty) t1 t2

-- Hilfe, um Substitution zu bauen
unifyHelp :: Subst -> Term -> Term -> Maybe Subst
unifyHelp sigma t1 t2 = case (ds (apply sigma t1) (apply sigma t2)) of  -- bilden von ds nach Anwendung von Substitutionen
                         Nothing -> Just sigma  -- Substitution gefunden
                         Just(Var v,t) -> if not (occur (Var v) t)  -- occur check für Variable in Term
                                           then unifyHelp (compose((single v t)) sigma) t1 t2  -- Rekursion auf erweiterte Substitution
                                           else Nothing  -- occur check erfolgreich, also Fail
                         Just(t, Var v) -> if not (occur (Var v) t) -- gleicher Fall nochmal für andere Reihenfolge
                                           then unifyHelp (compose((single v t)) sigma) t1 t2  -- Rekursion auf erweiterte Substitution
                                           else Nothing
                         Just(_,_) -> Nothing  -- beides Terme, also fail

-- checke, ob eine Variable in einem Term vorkommt
occur :: Term -> Term -> Bool
occur (Var a) (Comb _ x) =  (elem a (allVars x))  -- deswegen Ergänzung in Vars gemacht
occur _ _ = False -- wir wollen nur Variable in Term wissen, deswegen Alternativen egal, also false


-- Test gedöns (wird noch weggeräumt)
{- v1 = (Var "A")
t1 = (Comb "f" [Var "A", Var "B", Var ""])
t2 = (Comb "f" [Var "A", Comb "e" [Var "A", Var "X"]])
t3 = (Comb "f" [Var "A", Comb "d" [Var "A", Var "C"]])
t4 = (Comb "f" [Var "A", Var "a", (Comb "g" [Var "B", Var "", Comb "h" [Var "C", Var "D", Comb "true" []]])])
t5 = (Comb "f" [Var "A", Var "a", (Comb "g" [Var "B", Var "_", Comb "h" [Var "C", Var "E", Comb "true" []]])])

z1 = (Comb "f" [Var "A", Var "a", (Comb "g" [Var "B", Var "_",                                     Comb "h" [Var "C", Var "D", Comb "true" []]])])
z2 = (Comb "f" [Var "A", Var "a", (Comb "g" [Var "B", Comb "h" [Var "C", Var "E", Comb "true" []], Comb "h" [Var "C", Var "E", Comb "true" []]])]) -}
