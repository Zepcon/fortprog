module Pretty(Pretty(..)) where

import Data.List
import Type

class Pretty a where
 pretty :: a -> String

-- Darstellung von einem Term in gültiger Prolog Syntax
instance Pretty Term where
 pretty (Var x) = x  -- eine Variable bleibt eine Variable
 pretty (Comb x []) = x  -- ein leerer Term wird nicht weiter berücksichtigt
 pretty (Comb "." [a,b]) = "[" ++ prettyList a b ++ "]"  -- für den Spezialfall "." wird die Hilfsfunktion prettyList aufgerufen
  where
   prettyList :: Term -> Term -> String
   prettyList a1 (Comb "[]" []) = pretty a1
   prettyList a1 (Comb "." [c,d]) = pretty a1 ++ ", " ++ prettyList c d  -- Komma Separierung
   prettyList a1 b1 = pretty a1 ++ "| " ++ pretty b1  -- Separierung mit Pipe
 pretty (Comb f x) = f ++ "(" ++ (intercalate ", " (map pretty x)) ++ ")" -- Name von f wird behalten, map über Liste mit anschließender Separierung mit Klammern


 {- Test Beispiele
 pretty (Comb "true" [])
 pretty (Comb "." [Comb "1" [], Comb "." [Comb "2" [], Comb "." [Comb "3" [], Comb "[]" []]]])
 pretty (Comb "." [Var "E", Comb "h" [Var "F", Comb "i" [Var "G"]]])
 pretty (Comb "." [Var "I", Comb "true" [], Comb "j" [Var "J"]])
 -}
