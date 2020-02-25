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
                      tl = map (\(x,y) -> (x, sld (Prog rs) y)) fps  -- tree List
                  in SLDTree g tl  -- alles zusammenbauen

-- Mache aus dem Maybe eine normale Tupel Liste
helper2 :: [Maybe (Subst,Goal)] -> [(Subst, Goal)]
helper2 [] = []
helper2 (Nothing:xs) = [] ++ helper2 (xs)
helper2 (Just (x,y):xs) = [(x,y)] ++ (helper2 xs)

-- Unfizieren von Rule und Goal
helper :: Rule -> Goal -> Maybe (Subst,Goal)
helper (Rule t ts) (Goal (g:gs)) = case unify t g of
                                    Nothing -> Nothing  -- Kein Unfikator gefunden
                                    Just s  -> Just (s, Goal (map (apply s) (ts ++ gs)))  -- Unfikator auf restliche Regl und Goal zusammen anwenden

-- Liste von Kindern -> DFS mappen -> Konkatenieren um Liste zu erhalten

type Strategy = SLDTree -> [Subst]

-- Tiefensuche
-- SLDTree Goal [(Subst, SLDTree)]
-- Goal [Term]
dfs :: Strategy
dfs (SLDTree x []) = []
dfs (SLDTree x sldt) = concatMap dfsHelp sldt

dfsHelp :: (Subst, SLDTree) -> [Subst]
dfsHelp (s, SLDTree (Goal []) []) = [s]
dfsHelp (s, SLDTree (Goal x) []) = []
dfsHelp (s, sldt) = map (compose s) (dfs sldt)

type Queue = [(Subst, SLDTree)]


bfs :: Strategy
bfs (SLDTree x []) = []
bfs (SLDTree x sldt) = undefined



-- -- Breitensuche
-- bfs :: Strategy
-- bfs = undefined
