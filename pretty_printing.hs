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
 pretty (Comb "." [(Comb f t),xs]) = "[" ++ prettyList (Comb f t) xs ++ "]"
  where
   prettyList :: Term -> Term -> String
   -- die pretty bei einer leeren Restliste nur auf deren Kopf anwendet
   prettyList (Comb f t) (Comb "[]" []) = pretty (Comb f t)
   -- die pretty nur auf deren Kopf anwendet und für den Spezialfall in der Restliste wiederum prettyList auf diese anwendet
   prettyList (Comb f t) (Comb "." [a,b]) = pretty (Comb f t) ++ ", " ++ prettyList a b
   -- die pretty auf den Kopf und die Restliste anwendet, wenn die Restliste kein Spezialfall ist
   prettyList (Comb f t) xs = pretty (Comb f t) ++ "|" ++ pretty xs
-- der Name f wird übernommen, mit map wird pretty auf jedes Termelement angewendet
-- und mit intercalate wird die von map zurückgegebene Liste in einen String umgewandelt und mit ", " separiert
 pretty (Comb f x) = f ++ "(" ++ (intercalate ", " (map pretty x)) ++ ")"
