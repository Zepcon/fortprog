% Definition der Ehefrau-Ehemann-Relation:
ehemann(christine, heinz).
ehemann(maria,     fritz).
ehemann(monika,    herbert).
ehemann(angelika,  hubert).
ehemann(claudia,   karl).

% Definition der Kind-Mutter-Relation:
mutter(herbert,  christine).
mutter(angelika, christine).
mutter(hubert,   maria).
mutter(karl,     maria).
mutter(susanne,  monika).
mutter(norbert,  monika).
mutter(andreas,  angelika).
mutter(anna,     claudia).

vater(Kind,Vater) :- ehemann(Mutter,Vater), mutter(Kind,Mutter).

grossvater(E,G) :- vater(E,V), vater(V,G).
grossvater(E,G) :- mutter(E,M), vater(M,G).

% elternteil(Person, Elter)
elternteil(P, M) :- mutter(P, M).
elternteil(P, V) :- vater(P, V).

% grossmutter(Person, Grossmutter)
grossmutter(P, G) :- elternteil(P, E), mutter(E, G).

% geschwister(Geschwister1, Geschwister2)
geschwister(G1, G2) :- mutter(G1, M), mutter(G2, M), G1 \= G2.

% bruder(Person, Bruder)
bruder(P, B) :- geschwister(P, B), mann(B).

% schwester(Person, Schwester)
schwester(P, S) :- geschwister(P, S), frau(S).

% tante(Person, Tante).
tante(P, T) :- elternteil(P, E), schwester(E, T).
tante(P, T) :- elternteil(P, E), bruder(E, B), ehemann(T, B).


frau(christine).
frau(maria).
frau(monika).
frau(angelika).
frau(susanne).

mann(heinz).
mann(fritz).
mann(herbert).
mann(hubert).
mann(karl).
mann(norbert).
mann(andreas).
