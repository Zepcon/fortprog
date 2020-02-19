module Subs where

{- Subsitution: Ordnet jeder Variablen einen Term zu -}

import Type

-- Substitutionen, zugeordnet zu Variablen
data Subst = Subst [(VarName,Term)]
 deriving Show

-- empty Substitution
empty :: Subst
empty = Subst []

-- 1 Variable auf 1 Term
single :: VarName -> Term -> Subst
single x y = Subst [(x,y)]

-- Substitution auf Term anwenden
apply :: Subst -> Term -> Term
apply (Subst []) a = a
apply (Subst ((x,y):z)) (Var a) = if x == a then y else apply (Subst z) (Var a)
apply s (Comb a x) = Comb a (map (apply s) x)
--apply (Subst ((x,y):z)) (Comb _ a) = undefined

-- Komposition von Substitutionen
compose :: Subst -> Subst -> Subst
-- apply (compose s2 s1) t == apply s2 (apply s1 t)
compose s1 (Subst []) = s1
compose (Subst []) s2 = s2
compose s1 (Subst s2) = undefined


-- Subst example:
-- Subst [("A",Var "B"), ("C", Var "D")]
-- Subst [("A",Var "B"), ("C", Comb "." [Comb "true" [], Comb "." [Comb "g" [Var "C"], Comb "[]" []]])]

-- Term example:
-- Var "A"
-- Comb "." [Var "B"]
-- (Comb "." [Var "K", Comb "." [Var "L", Var "M", Var "N", Var "O"]])
-- (Comb "." [Var "E", Comb "h" [Var "F", Comb "i" [Var "G"]]])
