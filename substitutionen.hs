module Subs where

{- Subsitution: Ordnet jeder Variablen einen Term zu -}

import Type

-- Substitutionen, zugeordnet zu Variablen
data Subst = Empty | Subst [(VarName,Term)]
 deriving Show

-- empty Substitution
empty :: Subst
empty = Empty

-- 1 Variable auf 1 Term
single :: VarName -> Term -> Subst
single x y = Subst [(x,y)]

-- Substitution auf Term anwenden
apply :: Subst -> Term -> Term
apply s [] = []
apply s [Var(a)] = undefined --Subsitution nur auf die Variable anwenden
apply s (Comb _ []) = []
apply s (Comb _ x) = undefined --Subsitution nur auf die Variable anwenden
