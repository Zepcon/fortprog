module Rename where

import Type
import Vars

-- Variante einer Regel = Regel mit neuen Variablen
-- Variablen einer Regel umbenennen, Rule = Rule Term [Term]
rename :: Rule -> Rule
rename (Rule (Var "_") []) = Rule (Var "_") []
rename (Rule (Var x) []) = Rule (Var (head (take 1 (filter (\y -> (not (y == x))) freshVars)))) []  -- keine Klammern wegnehmen!
rename (Rule (Var x) y) = Rule (renameTerm (Var x)) (map renameTerm y)
rename (Rule (Comb a x) y) = Rule (Comb a (map renameTerm x)) (map renameTerm y)

renameTerm :: Term -> Term
renameTerm (Var x) = Var (head (take 1 (filter (\y -> (not (y == x))) freshVars)))
renameTerm (Comb a x) = Comb a (map renameTerm x)


r1 = Rule (Var "A") [Var "B", Comb "." [Var "C"]]
r2 = Rule (Var "A") []
