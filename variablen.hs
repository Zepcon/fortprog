module Vars where

import Type

class Vars a where
 allVars :: a -> [VarName]

instance Vars Term where
 allVars (Var a) = [a]
 allVars (Comb _ []) = []
 allVars (Comb _ x) = varHelp [] x
   where
    varHelp :: [VarName] -> [Term] -> [VarName]
    varHelp acc [] = acc
    varHelp acc y = acc ++ (concat (map allVars y))

instance Vars Rule where
 allVars (Rule (Var a) []) = [a]
 allVars (Rule (Comb _ []) []) = []
 allVars (Rule (Comb _ x) y) = varHelp [] x y
  where
   varHelp :: [VarName] -> [Term] -> [Term] -> [VarName]
   varHelp acc [] [] = acc
   varHelp acc x1 [] = acc ++ (concat (map allVars x1))
   varHelp acc [] x1 = acc ++ (concat (map allVars x1))
   varHelp acc x1 y1 = acc ++ (concat (map allVars x1)) ++ (concat (map allVars y1))


instance Vars Prog where
 allVars (Prog x) = concat (map allVars x)
   

instance Vars Goal where
 allVars (Goal x) = concat (map allVars x)


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
