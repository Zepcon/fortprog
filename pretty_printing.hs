module Pretty where

import Data.List
import Type

class Pretty a where
 pretty :: a -> String

instance Pretty Term where
 -- eine Variable bleibt eine Variable
 pretty (Var x) = x
 -- ein leerer Term wird nicht weiter berücksichtigt
 pretty (Comb x []) = x
 -- für den Spezialfall "." wird die Hilfsfunktion prettyList aufgerufen,
 pretty (Comb "." [a,b]) = "[" ++ prettyList a b ++ "]"
  where
   prettyList :: Term -> Term -> String
   -- die pretty bei einer leeren Restliste nur auf deren Kopf anwendet
   prettyList a1 (Comb "[]" []) = pretty a1
   -- die pretty nur auf deren Kopf anwendet und für den Spezialfall in der Restliste wiederum prettyList auf diese anwendet
   prettyList a1 (Comb "." [c,d]) = pretty a1 ++ ", " ++ prettyList c d
   -- die pretty auf den Kopf und die Restliste anwendet, wenn die Restliste kein Spezialfall ist
   prettyList a1 b1 = pretty a1 ++ "|" ++ pretty b1
-- der Name f wird übernommen, mit map wird pretty auf jedes Termelement angewendet
-- und mit intercalate wird die von map zurückgegebene Liste in einen String umgewandelt und mit ", " separiert
 pretty (Comb f x) = f ++ "(" ++ (intercalate ", " (map pretty x)) ++ ")"
