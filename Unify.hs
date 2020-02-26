module Unify where

import Type
import Subst
import Vars

-- ds berechnet die Unstimmigkeitsmenge zweier Terme
-- und gibt sie als Paar zurück
-- Unstimmigkeitsmenge leer: Nothing zurückgeben
ds :: Term -> Term -> Maybe (Term, Term)
ds (Var "_") _ = Nothing  -- Anonyme Variablen können alles sein, also auch jeder Term, also Nothing
ds _ (Var "_") = Nothing  -- Same here
ds (Var v1) (Var v2) = if v1 == v2 then Nothing  -- gleiche Variable
                                    else Just(Var v1, Var v2)  -- ungleiche Variable, also ds
ds (Comb combName xs1) (Comb combName2 xs2) | length xs1 /= length xs2 = Just((Comb combName xs1),(Comb combName2 xs2))  -- Ungleiche Länge impliziert ds
                                            | combName /= combName2 = Just((Comb combName xs1),(Comb combName2 xs2))  -- Ungleiche Namen ebenso
                                            | otherwise = dsHelp xs1 xs2  -- Weiter die Termlisten checken
ds t1 t2 = Just(t1,t2)  -- Letzte Möglichkeit entspricht ds

-- checke für Termlisten ob diese ds bilden könnten
-- zipwith, find, msum
dsHelp :: [Term] -> [Term] -> Maybe (Term, Term)
dsHelp _ [] = Nothing
dsHelp [] _ = Nothing
dsHelp (x1:xs1) (y1:ys1) = case ds x1 y1 of
                            Nothing -> dsHelp xs1 ys1  -- so weit so gut
                            Just (x2,y2) -> Just (x2,y2)  -- ds gefunden

-- Test gedöns (wird noch weggeräumt)
{- v1 = (Var "A")
t1 = (Comb "f" [Var "A", Var "B", Var ""])
t2 = (Comb "f" [Var "A", Comb "e" [Var "A", Var "X"]])
t3 = (Comb "f" [Var "A", Comb "d" [Var "A", Var "C"]])
t4 = (Comb "f" [Var "A", Var "a", (Comb "g" [Var "B", Var "", Comb "h" [Var "C", Var "D", Comb "true" []]])])
t5 = (Comb "f" [Var "A", Var "a", (Comb "g" [Var "B", Var "_", Comb "h" [Var "C", Var "E", Comb "true" []]])])

z1 = (Comb "f" [Var "A", Var "a", (Comb "g" [Var "B", Var "_",                                     Comb "h" [Var "C", Var "D", Comb "true" []]])])
z2 = (Comb "f" [Var "A", Var "a", (Comb "g" [Var "B", Comb "h" [Var "C", Var "E", Comb "true" []], Comb "h" [Var "C", Var "E", Comb "true" []]])]) -}

-- unify bestimmt aufbauend auf ds den allgemeinsten Unifikator für zwei Terme
-- sofern die beiden Terme unifizierbar sind
-- sonst: Nothing
unify :: Term -> Term -> Maybe Subst
unify t1 t2 = unifyHelp (Subst []) t1 t2

-- Hilfe um Substitution zu bauen
unifyHelp :: Subst -> Term -> Term -> Maybe Subst
unifyHelp sigma t1 t2 = case (ds (apply sigma t1) (apply sigma t2)) of  -- Bilden von ds nach Anwendung von Substitutionen
                         Nothing -> Just sigma  -- Substitution gefunden
                         Just(Var v,t) -> if not (occur (Var v) t)  -- occur check für Variable in Term
                                           then unifyHelp (compose((single v t)) sigma) t1 t2  -- Rekursion auf erweiterte Substitution
                                           else Nothing  -- Occur check erfolgreich, also Fail
                         Just(t, Var v) -> if not (occur (Var v) t) -- Gleicher Fall nochmal für andere Reihenfolge
                                           then unifyHelp (compose((single v t)) sigma) t1 t2  -- Rekursion auf erweiterte Substitution
                                           else Nothing
                         Just(_,_) -> Nothing  -- Beides Terme, also fail

-- checke ob eine Variable in einem Term vorkommt
occur :: Term -> Term -> Bool
occur (Var a) (Comb _ x) =  (elem a (allVars x))  -- deswegen Ergänzung in Vars gemacht
occur _ _ = False -- Wir wollen nur Variable in Term wissen, deswegen Alternativen egal, also false
