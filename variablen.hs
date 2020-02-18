module Vars where

import Type

class Vars a where
 allVars :: a -> [VarName]

instance Vars Term where
 allVars (Var a) = [a]
 allVars (Comb _ []) = []
 allVars (Comb _ x) = varHelp [] x
   where
    varHelp :: [VarName] -> [Term] -> [VarName]
    varHelp acc [] = acc
    varHelp acc y = acc ++ (concat (map allVars y))

instance Vars Rule where
 allVars (Rule (Var a) []) = [a]

 --
 -- warning: [-Wincomplete-patterns]
 --     Pattern match(es) are non-exhaustive
 --     In an equation for `allVars':
 --         Patterns not matched: (Rule (Var _) (_:_))

 allVars (Rule (Comb _ []) []) = []
 allVars (Rule (Comb _ x) y) = varHelp [] x y
  where
   varHelp :: [VarName] -> [Term] -> [Term] -> [VarName]
   varHelp acc [] [] = acc
   varHelp acc x1 [] = acc ++ (concat (map allVars x1))
   varHelp acc [] x1 = acc ++ (concat (map allVars x1))
   varHelp acc x1 y1 = acc ++ (concat (map allVars x1)) ++ (concat (map allVars y1))


-- instance Vars Prog where
--  allVars Prog [] = []
--  allVars Prog  x = varHelp [] x
--   where
--    varHelp :: [VarName] -> [Term] -> [VarName]
--    varHelp acc [] = acc
--    varHelp acc [Rule (Var a) []] = acc ++ [a]


instance Vars Goal where
  allVars (Goal []) = []
  --
  -- Goal (Var _:_:_))
  --             (Goal (Comb _ []:_:_))
  --             (Goal (Comb _ (_:_):_:_))
  --    |

  allVars (Goal [Var(a)]) = [a]
  allVars (Goal [Comb _ []]) = []
  allVars (Goal [Comb _ x]) = varHelp [] x --Hilfsfunktion
    where
      varHelp :: [VarName] -> [Term] -> [VarName]
      varHelp acc [] = acc
      varHelp acc y = acc ++ (concat (map allVars y))



 -- unendliche Liste
freshVars :: [VarName]
--freshVars = [ c | c `elem` ['A'..'Z'], c ++ (show x1) | x1 <- [1..]::[Integer]]  
