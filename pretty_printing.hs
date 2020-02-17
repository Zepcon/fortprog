-- Typeclass pretty

import Type

class Pretty a where
 pretty :: a -> String

instance Pretty a => Pretty (Term) where
 pretty (Var x) = x