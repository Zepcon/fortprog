module Subs where

{- Subsitution: Ordnet jeder Variablen einen Term zu -}

import Type
import Pretty
import Vars

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
compose s2 (Subst []) = s2
compose (Subst []) s1 = s1
compose (Subst s2) (Subst s1) = memberHelp (Subst s2) (Subst (map help s1))
 where
  help :: (VarName, Term) -> (VarName, Term)
  help (v,t) = (v, apply (Subst s2) t)

-- Übertrage Tupel von einer Substitution in eine andere
memberHelp :: Subst -> Subst -> Subst
memberHelp (Subst []) a = a
memberHelp a (Subst []) = a
memberHelp (Subst ((x,y):z)) (Subst b) = if not (varMember x (Subst b)) 
                                          then (memberHelp (Subst z) (Subst (b ++ [(x,y)]))) 
                                          else (memberHelp (Subst z) (Subst b))

-- Checken ob Tupel Variable bereits in anderer Substitution vorkommt
varMember :: VarName -> Subst -> Bool
varMember _ (Subst []) = False
varMember a (Subst ((x,y):z)) = if a == x then True else (varMember a (Subst z))

-- Compose Beispiele
-- (compose (single "A" (Var "B")) (single "A" (Var "C")))
-- (compose (single "D" (Var "E")) (single "F" (Comb "f" [Var "D", Comb "true" []])))
-- (compose (single "G" (Var "H")) (single "I" (Var "J")))

-- Subst example:
-- Subst [("A",Var "B"), ("C", Var "D")]
-- Subst [("A",Var "B"), ("C", Comb "." [Comb "true" [], Comb "." [Comb "g" [Var "C"], Comb "[]" []]])]

-- Term example:
-- Var "A"
-- Comb "." [Var "B"]
-- (Comb "." [Var "K", Comb "." [Var "L", Var "M", Var "N", Var "O"]])
-- (Comb "." [Var "E", Comb "h" [Var "F", Comb "i" [Var "G"]]])


instance Pretty Subst where
  -- data Subst = Subst [(VarName,Term)]
  pretty (Subst []) = []
  pretty (Subst s) = "{" ++ prettySubst s ++ "}"

    prettySubst :: Subst -> String
    prettySubst s =

   -- ghci> pretty (compose (single "A" (Var "B")) (single "A" (Var "C")))
   -- Subst [("A",Var "C")]
   -- "{A -> C}"

   -- ghci> pretty (compose (single "D" (Var "E")) (single "F" (Comb "f" [Var "D", Comb "true" []])))
   -- "{F -> f(E, true), D -> E}"
   -- ghci> pretty (compose (single "G" (Var "H")) (single "I" (Var "J")))
   -- "{I -> J, G -> H}"




-- Subst example:
-- Subst [("A",Var "B"), ("C", Var "D")]
-- Subst [("A",Var "B"), ("C", Comb "." [Comb "true" [], Comb "." [Comb "g" [Var "C"], Comb "[]" []]])]

-- Term example:
-- Var "A"
-- Comb "." [Var "B"]
-- (Comb "." [Var "K", Comb "." [Var "L", Var "M", Var "N", Var "O"]])
-- (Comb "." [Var "E", Comb "h" [Var "F", Comb "i" [Var "G"]]])
