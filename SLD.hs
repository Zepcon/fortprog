module SLD where

import Type
import Subst
import Rename
import Unify

data SLDTree = SLDTree Goal [(Subst, SLDTree)]
   deriving Show

sld :: Prog -> Goal -> SLDTree
sld (Prog []) (Goal []) = SLDTree (Goal []) []
sld (Prog _) (Goal []) = SLDTree (Goal []) []
sld (Prog []) (Goal x) = SLDTree (Goal x) []
sld (Prog rs) (Goal g) =let rs' = map rename rs
                            mps = map (\r -> helper r g) rs'
                            fmps = filter (/= Nothing) mps
                            fps = helper2 fmps
                        in 


helper2 :: [Maybe (Subst,Goal)] -> [(Subst, Goal)]
helper2 [] = []
helper2 [(Just (x,y))] = [(x,y)]
helper2 (Just ((x,y):xs)) = helper2ac [] (Just (x,y):xs)
helper2 _ = error "What just happened."
 where
  helper2ac :: [(Subst, Goal)] -> [Maybe (Subst, Goal)] -> [(Subst, Goal)]
  helper2ac acc [] = acc
  helper2ac acc (Just (x,y):xs) = acc ++ [(x,y)] ++ (helper2 xs)
  

helper :: Rule -> Goal -> Maybe (Subst,Goal)
helper (Rule t ts) (Goal (g:gs) = case unify t g of
                                    Nothing -> Nothing
                                    Just s  -> (s, Goal (map (apply s) (ts ++ gs)))


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
