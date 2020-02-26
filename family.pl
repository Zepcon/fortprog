ehemann(christine, heinz).
ehemann(maria,     fritz).
ehemann(monika,    herbert).
ehemann(angelika,  hubert).
ehemann(claudia,   karl).

mutter(herbert,  christine).
mutter(angelika, christine).
mutter(hubert,   maria).
mutter(karl,     maria).
mutter(susanne,  monika).
mutter(norbert,  monika).
mutter(andreas,  angelika).
mutter(anna,     claudia).

frau(christine).
frau(maria).
frau(monika).
frau(angelika).
frau(claudia).
frau(anna).

vater(Kind,Vater) :- ehemann(Mutter,Vater), mutter(Kind,Mutter).

grossvater(E,G) :- vater(E,V), vater(V,G).
grossvater(E,G) :- mutter(E,M), vater(M,G).

grossmutter(Person, Grossmutter) :- mutter(Person, X), mutter(X,Grossmutter).
grossmutter(Person, Grossmutter) :- vater(Person, X), mutter(X,Grossmutter).
grossmutter(Person, Grossmutter) :- grossvater(Person, Grossvater), ehemann(Grossmutter, Grossvater).

tante(Person, Tante) :- geschwister(Tante, X), vater(Person,X), frau(Tante).
tante(Person, Tante) :- geschwister(Tante, X), mutter(Person,X), frau(Tante).
