module Rename where

import Type
import Vars
import Subst

-- Variablen von rule neu benennen, falls diese auch in der zweiten Rule vorkommen
rename :: Rule -> [VarName] -> Rule
rename (Rule (Var "_") []) x = Rule (Var "_") [] 
rename r@(Rule t ts) r2 = Rule (apply (createSub (allVars r) r2) t) (map (apply (createSub (allVars r) r2)) ts)

-- Bilde bestehende Variablen auf neue ab, Unterstrich kann ignoriert werden
createSub :: [VarName] -> [VarName] -> Subst
createSub r1 r2 = Subst (filter (\ (a,_) -> not(a == "_"))(zip r1 (map(\x -> (Var x))(take (length r1) (filter (\x -> (not (elem x r2))) freshVars)))))


{- 
Test- Beispiele:
r1 = Rule (Var "B") [Var "A", Comb "." [Var "U"]]
r2 = Rule (Var "U") [Var "B", Comb "." [Var "D"]] -}