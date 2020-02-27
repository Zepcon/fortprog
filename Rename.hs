module Rename(rename) where

import Type
import Vars
import Subst

-- Variablen von Rule neu bennenen, falls diese in der Liste von verbotenen Variablen vorkommen
rename :: Rule -> [VarName] -> Rule
rename r@(Rule t ts) novars = Rule (apply (createSub (allVars r) novars) t) (map (apply (createSub (allVars r) novars)) ts)

-- bilde bestehende Variablen auf neue ab, Unterstrich kann ignoriert werden
createSub :: [VarName] -> [VarName] -> Subst
createSub r1 novars = let fr1 = filter (\a -> not(a == "_")) r1  -- filtered rule 1, anonyme Variablen werden ignoriert
                          partres = zipWith single fr1 ( map(\x -> (Var x)) (filter (\x -> (not (elem x novars))) freshVars))
                          -- Listen von Substiutionen bauen, mit (filteredRule, neuerVariable)
                      in foldl compose empty partres  -- komponieren der Substitutionen zu einer


{-
Test- Beispiele:
r1 = Rule (Var "B") [Var "A", Comb "." [Var "U"]]
r2 = Rule (Var "U") [Var "B", Comb "." [Var "D"]] -}
