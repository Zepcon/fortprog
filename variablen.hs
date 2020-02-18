module Vars where

import Type

class Vars a where
 allVars :: a -> [VarName]

instance Vars Term where
 allVars (Var a) = [a]
 allVars (Comb _ x) = varHelp [] x
   where
    varHelp :: [VarName] -> [Term] -> [VarName]
    varHelp x ([Var a]) = x ++ [a]
    varHelp x [] = x
    varHelp x ([Comb _ y]) = x ++ (concat (map allVars y))


instance Vars Rule where
 allVars = undefined

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