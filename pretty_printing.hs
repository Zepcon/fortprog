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
 pretty (Comb "." ((Comb f t):xs)) = undefined
 pretty (Comb f x) = f ++ "(" ++ (intercalate ", " (map pretty x)) ++ ")"

 -- *Main> pretty (Var "A")
 -- "A"
 -- *Main> pretty (Comb "true" [])
 -- "true"
 -- *Main> pretty (Comb "f" [Var "B", Var "_", Comb "true" []])
 -- "f(B, _, true)"
*Main> pretty (Comb "." [Comb "true" [], Comb "[]" []])
 ".(true, [])"
 "[true]"
*Main> pretty (Comb "." [Comb "true" [], Comb "." [Comb "g" [Var "C"], Comb "[]" [)
 ".(true, .(g(C), []))"
 "[true, g(C)]"
*Main> pretty (Comb "." [Comb "true" [], Var "D"])
 ".(true, D)"
 "[true|D]"
*Main> pretty (Comb "." [Comb "true" [], Comb "h" [Var "E", Comb "i" [Var "F"]]])
 ".(true, h(E, i(F)))"
 "[true|h(E, i(F))]"
*Main> pretty (Comb "." [Comb "true" [], Comb "." [Comb "true" [], Comb "true" []])
 ".(true, .(true, true))"
 "[true, true|true]"
*Main> pretty (Comb "." [Comb "." [Comb "true" [], Comb "[]" []], Comb "[]" []])
 ".(.(true, []), [])"
  "[[true]]"
 -- *Main> pretty (Comb "." [Var "G"])
 -- ".(G)"
 -- *Main> pretty (Comb "." [Var "H", Comb "true" [], Comb "j" [Var "I"]])
 -- ".(H, true, j(I))"
