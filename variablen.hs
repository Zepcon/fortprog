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
 allVars (Var a) [] = [a]
 allVars (Comb _ x) y = varHelp [] x y
  where
   varHelp :: [VarName] -> Term -> [Term] -> [VarName]
   varHelp acc [] [] = acc
   varHelp acc (Var a) [] = acc ++ [a]
   varHelp acc (Comb _ x) [] = acc ++ allVars(x)
   varHelp acc [] x = acc ++ (concat (map allVars y))
   varHelp acc (Var a) y = acc ++ [a] ++ (concat (map allVars y))
   varHelp acc (Comb _ x) y = acc ++ allVars(x) ++ (concat (map allVars y))


instance Vars Prog where
 allVars = undefined

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
