-- Typeclass pretty
import Data.List

--module Pretty where

import Type

class Pretty a where
 pretty :: a -> String

instance Pretty Term where
 pretty (Var x) = x
 pretty (Comb x []) = x
 pretty (Comb "." [(Comb f t),xs]) = "[" ++ prettyList (Comb f t) xs ++ "]"
  where
   prettyList :: Term -> Term -> String
   prettyList (Comb f t) (Comb "[]" []) = pretty (Comb f t)
   prettyList (Comb f t) (Comb "." [a,b]) = pretty (Comb f t) ++ "," ++ prettyList a b
   prettyList (Comb f t) xs = pretty (Comb f t) ++ "|" ++ pretty xs

 pretty (Comb f x) = f ++ "(" ++ (intercalate ", " (map pretty x)) ++ ")"
