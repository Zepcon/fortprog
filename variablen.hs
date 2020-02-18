module Vars where

import Type

class Vars a where
 allVars :: a -> [VarName]

instance Vars Term where
 allVars (Var a) = [a]
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
   varHelp acc x [] = acc ++ (concat (map allVars x))
   varHelp acc [] x = acc ++ (concat (map allVars x))
   varHelp acc x y = acc ++ (concat (map allVars x)) ++ (concat (map allVars y))


instance Vars Prog where
 allVars Prog [] = []
 allVars Prog  x = varHelp [] x
  where
   varHelp :: [VarName] -> [Term] -> [VarName]
   varHelp acc [] = acc
   varHelp acc [Rule (Var a) []] = acc ++ [a]
   

instance Vars Goal where
 allVars = undefined


 -- unendliche Liste
freshVars :: [VarName]
freshVars = undefined

{-
- Variablen beginnen mit einem Großbuchstaben
- Variablen stehen für beliebige andere Objekte
- Regeln oder Fakten mit Variablen repräsentieren unendlich viele Regeln
 -}
