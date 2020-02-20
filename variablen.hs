module Vars where

import Type

class Vars a where
 allVars :: a -> [VarName]

-- Variablen von Termen ausgeben
instance Vars Term where
  -- Variablen werden als Liste von Variablen ausgegeben
 allVars (Var a) = [a]
 allVars (Comb _ []) = []
 -- ist der Term von Comb nicht leer, wird mithilfe der varHelp-Hilfsfunktion
 -- allVars auf alle Elemente der Termliste gemappt
 allVars (Comb _ x) = varHelp [] x
   where
    varHelp :: [VarName] -> [Term] -> [VarName]
    varHelp acc [] = acc
    varHelp acc y = acc ++ (concat (map allVars y))

-- Variablen von Regeln ausgeben
instance Vars Rule where
 -- bei einem nicht leeren Term wird allVars wieder auf alle
 -- Elemente der Termliste gemappt
 -- Variablen werden als Liste zurückgegeben und mit der gemappten Liste konkateniert
 allVars (Rule (Var a) y) = [a] ++ (concat (map allVars y))
 -- beim Comb Term wird wieder allVars auf alle Termlistenelemente gemappt
 allVars (Rule (Comb _ x) y) = (concat (map allVars x)) ++ (concat (map allVars y))

-- Variablen von Programmen ausgeben
instance Vars Prog where
 allVars (Prog x) = concat (map allVars x)

-- Variablen von Anfragen ausgeben
instance Vars Goal where
 allVars (Goal x) = concat (map allVars x)

-- erzeugt eine unendliche Liste von gültigen Prolog-Variablen
freshVars :: [VarName]
freshVars = help 0 []
 where
  help :: Int -> [VarName] -> [VarName]
  help a x = ("A" ++ (show a)) : (help (a+1) x)



{- Test-Beispiele

Term:
Var "A"
Comb "F" [Var "B"]

Rule:
Rule (Comb "." [Comb "true" [], Var "D"]) [(Comb "." [Var "H"]), (Comb "f" [Var "B", Var "_", Comb "true" []])]

Prog:
Prog [Rule (Comb "." [Comb "true" [], Var "D"]) [(Comb "." [Var "H"]), (Comb "f" [Var "B", Var "_", Comb "true" []])], Rule (Comb "." [Comb "true" [], Var "D"]) [(Comb "." [Var "H"]), (Comb "f" [Var "B", Var "_", Comb "true" []])]]

Goal:
Goal [(Comb "." [Comb "[]" [], Comb "[]" []]), (Comb "." [Var "H"]), (Comb "." [Var "I", Comb "true" [], Comb "j" [Var "J"]])]
 -}
