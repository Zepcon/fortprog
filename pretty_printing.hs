-- Typeclass pretty

import Type

class Pretty a where
 pretty :: a -> String