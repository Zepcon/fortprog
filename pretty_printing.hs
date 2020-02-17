-- Typeclass pretty

import Type

class Pretty a where
 pretty :: a -> String

instance Pretty Term where
 pretty (Var x) = x
 pretty (Comb x []) = x
 pretty (Comb x )