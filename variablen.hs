module Variables where

import Type

class Vars a where
 allVars :: a -> [VarName]

instance Vars Term where
 allVars = undefined

instance Vars Rule where
 allVars = undefined

instance Vars Prog where
 allVars = undefined

instance Vars Goal where
 allVars = undefined


 -- unendliche Liste
freshVars :: [VarName]
freshVars = undefined