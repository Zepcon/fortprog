module SLD(sld,dfs,bfs,solve,Strategy) where

import Type
import Subst
import Rename
import Pretty
import Unify
import Vars
import Data.Maybe

-- Pretty Instanz Copyright Kaan
instance Pretty SLDTree where
  pretty (SLDTree (Goal (r:rs)) subsAndTrees) = pretty r ++ "\n" ++ pretty (SLDTree (Goal (rs)) subsAndTrees)
  pretty (SLDTree   (Goal [])   subsAndTrees) = pretty' subsAndTrees
   where
    pretty' :: [(Subst, SLDTree)] -> String
    pretty'           []          = []
    pretty' ((subs, sldTree): ts) = "(" ++ pretty subs ++ ", " ++ pretty sldTree ++ pretty' ts ++ ")"

data SLDTree = SLDTree Goal [(Subst, SLDTree)]
   deriving Show

sld :: Prog -> Goal -> SLDTree
sld (Prog _) (Goal []) = SLDTree (Goal []) []  -- keine Anfrage
sld (Prog []) (Goal gs) = SLDTree (Goal gs) []  -- kein Programm für das Goal vorhanden
sld r go = sld' r [] go  -- initial leere Liste von verbotenen Substitutionen
 where
  sld' :: Prog -> [VarName] -> Goal -> SLDTree  -- in Liste zusätzlich die Namen der Variablen in den Substitutionen mitführen
  sld' (Prog rs) nosub g = let novars = (allVars g) ++ nosub
                               rs' = map (\x -> rename x novars) rs  -- umbenanntes Programm
                               mps = map (\r1 -> helper r1 g) rs'  -- Maybe Pairs
                               fmps = filter isJust mps  -- Nothings sind uninteressant
                               fps = map fromJust fmps  -- normale Paare aus den Maybes machen
                               tl = map (\(sub,goal) -> (sub, sld' (Prog rs') (novars ++ (allVars sub)) goal)) fps  -- tree List, bisherige Variablennamen auch zu den Verbotenen dazunehmen
                           in SLDTree g tl  -- alles zusammenbauen

-- unfizieren von Rule und Goal
helper :: Rule -> Goal -> Maybe (Subst,Goal)
helper _ (Goal []) = Nothing
helper (Rule t ts) (Goal (g:gs)) = case unify t g of  -- checken, ob wir eine Unifikation finden können
                                    Nothing -> Nothing  -- kein Unfikator gefunden
                                    Just s  -> Just (s, (Goal (map (apply s) (ts ++ gs))))  -- Unfikator auf restliche Regl und Goal zusammen anwenden

type Strategy = SLDTree -> [Subst]

-- Tiefensuche
dfs :: Strategy
dfs (SLDTree _ []) = []
dfs (SLDTree _ sldt) = concatMap dfsHelp sldt

dfsHelp :: (Subst, SLDTree) -> [Subst]
dfsHelp (s, SLDTree (Goal []) []) = [s]  -- Lösung gefunden
dfsHelp (_, SLDTree (Goal _) []) = []  -- Goal nicht leer aber keine weiteren Ebenen, also leer, weil nicht lösbar
dfsHelp (s, SLDTree _ x) = concatMap (dfsHelp2 s) x  -- Goal nicht leer aber noch mehr Ebenen vorhanden, also tiefer gehen
 where
   dfsHelp2 :: Subst -> (Subst, SLDTree) -> [Subst]  -- Substitution auf jeden Subtree anwenden, welcher kommt
   dfsHelp2 sub2 (sub,tree) = dfsHelp (compose sub sub2, tree)

type Queue = [(Subst, SLDTree)]

-- alle Substitutionen in SLD Baum mit bfs
bfs :: Strategy
bfs sldt = bfsHelp [(empty, sldt)]  -- initial leere Subsitution und den Tree

-- reduzieren der Queue auf die Substitution
bfsHelp :: Queue -> [Subst]
bfsHelp [] = []
bfsHelp ((sub, (SLDTree (Goal []) [])): []) = [sub]  -- erstes Element in Queue ist Ergebnis, queue hat nicht mehr Elemente, keine subTrees
bfsHelp ((_, (SLDTree (Goal _) [])): []) = []  -- erstes Element in Queue ist kein Ergebnis, queue hat nicht mehr Elemente, keine SubTrees
bfsHelp ((sub, (SLDTree (Goal []) [])): xs) = [sub] ++ (bfsHelp xs)  -- erstes Element ist Lösung, queue hat weitere Elemente, keine SubTrees, Lösung hinzufügen und weitere Trees der Ebene abhandeln
bfsHelp ((_, (SLDTree (Goal _) [])): xs) = [] ++ (bfsHelp xs)  -- erstes Element ist kein Ergebnis, Queue hat weitere Elemente, keine subTrees also nächsten aus der Ebene abhandeln
bfsHelp ((sub, (SLDTree (Goal _) subTrees)): xs) = bfsHelp (bfsHelp2 subTrees xs sub)  -- wir haben Subtrees, also kein Ergebnis, also Ebenen der Subtrees in Queue packen

-- Subtrees zu einer Queue hinzufügen
bfsHelp2 :: [(Subst, SLDTree)] -> Queue -> Subst -> Queue
bfsHelp2 [] queue _ = queue  -- nichts mehr hinzufügen
bfsHelp2 ((sub, (SLDTree g xs)): ts) queue sub2 = bfsHelp2 ts (queue ++ [(compose sub sub2,SLDTree g xs)]) sub2
-- wir gehen eine Ebene tiefer, wenden die Subsitution an und packen alle Trees auf dieser Ebene in die Queue

solve :: Strategy -> Prog -> Goal -> [Subst]
solve stra pro go = let res = stra (sld pro go)  -- resolution mit programm und goal unter gegebener strategie anwenden
                        res' = map (restrictTo (allVars go)) res  -- jede Substitution auf Variablen einschränken, welche im Goal vorkommen
                    in res'
