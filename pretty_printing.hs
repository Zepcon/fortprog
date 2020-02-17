-- Typeclass pretty

import Type

class Pretty a where
 pretty :: a -> String

instance (Pretty a) => Pretty (Term a) where
 pretty (Var a) = a
 pretty (Comb a []) = ""
