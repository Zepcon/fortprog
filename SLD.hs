module SLD where

import Type
import Subst
import Rename

-- data Rule = Rule Term [Term]
-- data Goal = Goal [Term]
data SLDTree = SLDTree Goal [(Subst, SLDTree)]
   deriving Show

-- data Prog = Prog [Rule]
-- data Goal = Goal [Term]
-- data SLDTree = SLDTree Goal [(Subst, SLDTree]
-- data Subst = Subst [(VarName,Term)]
sld :: Prog -> Goal -> SLDTree
sld (Prog []) (Goal []) = SLDTree (Goal []) []
sld (Prog _) (Goal []) = SLDTree (Goal []) []
sld (Prog []) (Goal x) = SLDTree (Goal x) []
sld (Prog rs) (Goal y) = undefined --let a = (map rename rs) in map 
                           
helper :: Rule -> Goal -> Maybe Subst
helper = undefined
-- Regelkopf mit Literal unifizieren
   -- wenn es geht, also wenn man einen Unifikator findet, dann fügt man es als Kind hinzu

-- neuer Resolutionsschritt dann für den Rest von dem Goal
   -- Dabei Unifikator von dem Vorherigen auf alles anwenden, also auf die ganze Anfrage

-- später dann Aus der Liste von Maybe Subst weiter filtern (Nothing taucht im Baum nicht auf)


-- konstruiert den SLD-Baum zu einem Programm und einer Anfrage mittels FIRST
--sld p g = undefined

done = undefined

{- sld progAppend goalAnfrage = case (done) of
                       True -> [] [(sigma,[])]
                       False -> append [(sigma,sld)] -}


-- prog:
-- append ([],L,L).
-- append([E|R],L,[E|RL]) :- append(R,L,RL).
--
-- goal:
-- append(X,Y,[1,2])

-- SLD-Baum kann dann auf verschiedene Weisen durchlaufen werden,
-- um alle Lösungen für eine Anfrage zu finden.
-- durch die Belegung der freien Variablen der ursprünglichen Anfrage
-- Suchstrategien können mithilfe des Typs ausgedrückt werden:
-- -- type Strategy = SLDTree -> [Subst]
--
-- -- Tiefensuche
-- dfs :: Strategy
-- dfs = undefined
--
-- -- Breitensuche
-- bfs :: Strategy
-- bfs = undefined
--
-- -- iterative Tiefensuche
-- idfs :: Strategy
-- idfs = undefined
