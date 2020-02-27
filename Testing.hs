module Testing where

import Type
import SLD
import Pretty -- Warning, wenn Aufruf ohne pretty


sldt :: SLDTree
sldt = sld pro go

pro :: Prog
pro = Prog [rule1, rule2]

-- append([],L,L).
-- append([E|R],L,[E|RL]) :- append(R,L,RL).

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
-- sldt2 :: SLDTree
-- sldt2 = sld pro2 go2
--
-- pro2 :: Prog
-- pro2 = Prog [r1, r2, r3]
--
-- -- ehemann(christine, heinz).
-- -- ehemann(maria, fritz).
-- -- ehemann(monika, herbert).
-- -- ehemann(angelika, hubert).
-- r1 :: Rule
-- r1 = Rule (Comb "ehemann" [Var "christine" Var "heinz"])
-- r2 = Rule (Comb "ehemann" [Var "maria" Var "fritz"])
-- r3 = Rule (Comb "ehemann" [Var "monika" Var "herbert"])
-- r4 = Rule (Comb "ehemann" [Var "angelika" Var "hubert"])
--
-- -- mutter(herbert, christine).
-- -- mutter(angelika, christine).
-- -- mutter(hubert, maria).
-- -- mutter(susanne, monika).
-- -- mutter(norbert, monika).
-- -- mutter(andreas, angelika).
-- --
-- -- vater(K, V) :- ehemann(M, V), mutter(K, M).
-- --
-- -- elter(K, E) :- vater(K, E).
-- -- elter(K, E) :- mutter(K, E).
-- --
-- -- vorfahre(N, V) :- vorfahre(N, V2), vorfahre(V2, V).
-- -- vorfahre(N, V) :- elter(N, V).
