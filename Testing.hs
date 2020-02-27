module Testing where

import Type
import SLD
import Pretty -- Warning, wenn Aufruf ohne pretty


sldt :: SLDTree
sldt = sld pro go

pro :: Prog
pro = Prog [rule1, rule2]

rule1:: Rule
rule1 = Rule (Comb "append" [Comb "[]" [], Var "L", Var "L"]) []

rule2:: Rule
rule2 = Rule (Comb "append" [Comb "." [Var "E", Var "R"], Var "L" ,Comb "." [Var "E", Var "RL"]])
          [Comb "append" [Var "R", Var "L", Var "RL"]]

go :: Goal
go = Goal [Comb "append" [Var "X", Var "Y", Comb "." [Comb "1" [], Comb "." [Comb "2" [], Comb "[]" []]]]]
-- go = Goal [Comb "append" [Comb "[]" [], Comb "[]" [], Comb "[]" []]]
-- go = Goal [Comb "append" [Var "X", Comb "[]" [], Comb "." [Comb "1" [], Comb "[]" []]]]


------------------------------------------------------------------------------





-- =(f(A,B),f(f(C),g(A))).

goal = Goal [Comb "." [Comb "f" [Var "A", Var "B"], Comb "f" [Comb "f" [Var "C"]], Comb "g" [Var"A"]]]
