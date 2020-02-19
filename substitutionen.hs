module Subs where

{- Subsitution: Ordnet jeder Variablen einen Term zu -}

import Type

-- Substitutionen, zugeordnet zu Variablen
data Subst = Empty | Subst [(VarName,Term)]
 deriving Show

-- empty Substitution
empty :: Subst
empty = Empty

single :: VarName -> Term -> Subst