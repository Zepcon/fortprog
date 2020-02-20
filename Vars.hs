module Vars where

import Type

class Vars a where
 allVars :: a -> [VarName]

-- Enthaltene Variablen von Termen ausgeben
instance Vars Term where
 allVars (Var a) = [a] -- nur eine Variable
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
 -- Variable wird mit der gemappten Liste konkateniert
 allVars (Rule (Var a) y) = [a] ++ (concat (map allVars y))
 -- beim Comb Term wird wieder allVars auf alle Termlistenelemente gemappt und konkateniert
 allVars (Rule (Comb _ x) y) = (concat (map allVars x)) ++ (concat (map allVars y))

-- Variablen von Programmen ausgeben
instance Vars Prog where
 allVars (Prog x) = concat (map allVars x)

-- Variablen von Anfragen ausgeben
instance Vars Goal where
 allVars (Goal x) = concat (map allVars x)

-- allVars für Listen (in Unify benötigt)
instance Vars a => Vars [a] where
  allVars a = concat (map allVars a)

-- erzeugt eine unendliche Liste von gültigen Prolog-Variablen
freshVars :: [VarName]
freshVars = help 0 []
 where
  -- Momentanen counter hinter das A hängen für neue Variable
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
