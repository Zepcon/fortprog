module SLD where

import Type
import Subst
import Rename
import Unify
import Vars
import Data.Maybe

data SLDTree = SLDTree Goal [(Subst, SLDTree)]
   deriving Show

sld :: Prog -> Goal -> SLDTree
sld (Prog _) (Goal []) = SLDTree (Goal []) []
sld (Prog rs) g = let rs' = map (\x -> rename x (allVars g)) rs  -- Renamed Rule List
                      mps = map (\r -> helper r g) rs'  -- Maybe Pairs
                      fmps = filter (isJust) mps  -- filtered Maybe Pairs
                      fps = helper2 fmps  -- filtered pairs
                      tl = map (\(x,y) -> (x, sld (Prog rs) y)) fps -- tree List
                  in SLDTree g tl


helper2 :: [Maybe (Subst,Goal)] -> [(Subst, Goal)]
helper2 [] = []
helper2 (Nothing:xs) = [] ++ helper2 (xs)
helper2 (Just (x,y):xs) = [(x,y)] ++ (helper2 xs)

helper :: Rule -> Goal -> Maybe (Subst,Goal)
helper (Rule t ts) (Goal (g:gs)) = case unify t g of
                                    Nothing -> Nothing
                                    Just s  -> Just(s, Goal (map (apply s) (ts ++ gs)))


-- Regelkopf mit Literal unifizieren
   -- wenn es geht, also wenn man einen Unifikator findet, dann fügt man es als Kind hinzu

-- neuer Resolutionsschritt dann für den Rest von dem Goal
   -- Dabei Unifikator von dem Vorherigen auf alles anwenden, also auf die ganze Anfrage

-- später dann Aus der Liste von Maybe Subst weiter filtern (Nothing taucht im Baum nicht auf)


-- konstruiert den SLD-Baum zu einem Programm und einer Anfrage mittels FIRST
--sld p g = undefined

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
