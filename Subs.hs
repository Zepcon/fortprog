module Subs where

{- Subsitution: Ordnet jeder Variablen einen Term zu -}

import Type
import Pretty
import Vars
import Data.List

-- Substitutionen, Zuordnung Variablen zu Termen
data Subst = Subst [(VarName,Term)] -- eigentlich Abbildung, hier als Tupel
 deriving Show

-- empty Substitution
empty :: Subst
empty = Subst []

-- 1 Variable auf 1 Term
single :: VarName -> Term -> Subst
single x y = Subst [(x,y)]

-- Substitution auf Term anwenden
apply :: Subst -> Term -> Term
apply (Subst []) a = a  -- leere Substitution, also bleibt Term
apply (Subst ((x,y):z)) (Var a) = if x == a then y else apply (Subst z) (Var a)  -- Anwendung gefunden oder Rekursion
apply s (Comb a x) = Comb a (map (apply s) x)  -- Mappen von partieller Anwendung

-- Komposition von Substitutionen
compose :: Subst -> Subst -> Subst
compose s2 (Subst []) = s2
compose (Subst []) s1 = s1
compose (Subst s2) (Subst s1) = memberHelp (Subst s2) (Subst (map help s1)) -- nach dem mappen fehlende Tupel übertragen
 where
  help :: (VarName, Term) -> (VarName, Term)
  help (v,t) = (v, apply (Subst s2) t) -- erst komplett s2 über s1 mappen, um Eigenschaft zu gewährleisten

-- Übertrage Tupel von einer Substitution in eine andere, falls diese fehlen sollten
memberHelp :: Subst -> Subst -> Subst
memberHelp (Subst []) a = a
memberHelp a (Subst []) = a
memberHelp (Subst ((x,y):z)) (Subst b) = if not (varMember x (Subst b))  -- Checke für gegebenen Variablen Namen, ob dieser in einer Substitution vorkommt
                                          then (memberHelp (Subst z) (Subst (b ++ [(x,y)])))  -- wenn nein, dann füge hinzu
                                          else (memberHelp (Subst z) (Subst b))  -- wenn schon drin, alles gucci

-- Checken ob Tupel Variable bereits in anderer Substitution vorkommt
-- lookup Funktion verwenden
-- Maybe und Nothing verwenden
varMember :: VarName -> Subst -> Bool
varMember _ (Subst []) = False
varMember a (Subst ((x,_):z)) = if a == x 
                                 then True  -- gefunden!
                                 else (varMember a (Subst z))

-- Substitution auf Variablen einschränken
restrictTo :: [VarName] -> Subst -> Subst
restrictTo [] a = a
restrictTo x a = Subst (restrictHelp [] x a)
 where
   -- Baue die Liste für die Subst mit Akkumulator zusammen
   restrictHelp :: [(VarName,Term)] -> [VarName] -> Subst -> [(VarName,Term)]
   restrictHelp acc [] _ = acc
   restrictHelp acc (a1:as) x1 = if (varMember a1 x1) 
                                then (restrictHelp ((giveTuple a1 x1) : acc) as x1)  -- gefunden, also gib Tupel damit wir es dazupacken können
                                else (restrictHelp acc as x1)
   
-- Wenn Variable drin, gib das Tupel zurück
giveTuple :: VarName -> Subst -> (VarName, Term)
giveTuple _ (Subst []) = ("Hello",Var "hello") -- mache ich noch weg
giveTuple a (Subst ((x,y):z)) = if a == x 
                                 then (x,y) 
                                 else (giveTuple a (Subst z))


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

-- Auch Substitutionen wollen schön sein
instance Pretty Subst where
  pretty (Subst []) = "{ }"
  pretty (Subst a) = prettyHelp (Subst a)
   where
    prettyHelp :: Subst -> String
    prettyHelp (Subst z) = "{" ++ (intercalate ", " (map prettyTuple z)) ++ "}"  -- Abbildungen mit Komma separieren und Klammern für Schönheit

-- Tupel als Abbildung im Prolog Sinne machen
prettyTuple :: (VarName, Term) -> String
prettyTuple (x,y) = x ++ " -> " ++ (pretty y)  -- Tupel als Abbildungen bauen


-- Alle Variablen aus einer Substitution erhalten
instance Vars Subst
 where
  allVars (Subst []) = [""]
  allVars (Subst ((x,y):xs)) =  x : allVars y ++ helpVars xs  -- Einmal Term und einmal Restliste untersuchen
   where
    helpVars [] = []
    helpVars [(x1,y1)] = x1 : allVars y1  -- Nur Term untersuchen
    helpVars ((x1,y1):xs1) = x1 : allVars y1 ++ helpVars xs1  -- weiterhin sowohl Term als auch Restliste untersuchen


-- Subst example:
-- Subst [("A",Var "B"), ("C", Var "D")]
-- Subst [("A",Var "B"), ("C", Comb "." [Comb "true" [], Comb "." [Comb "g" [Var "C"], Comb "[]" []]])]

-- Term example:
-- Var "A"
-- Comb "." [Var "B"]
-- (Comb "." [Var "K", Comb "." [Var "L", Var "M", Var "N", Var "O"]])
-- (Comb "." [Var "E", Comb "h" [Var "F", Comb "i" [Var "G"]]])
