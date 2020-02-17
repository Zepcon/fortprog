-- Typeclass pretty
import Data.List

--module Pretty where

import Type

class Pretty a where
 pretty :: a -> String

instance Pretty Term where
 pretty (Var x) = x
 pretty (Comb x []) = x
 -- pretty (Comb "." (x:y)) = undefined
 pretty (Comb f x) = f ++ "(" ++ (intercalate ", " (map pretty x)) ++ ")"
