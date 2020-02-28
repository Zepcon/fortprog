% Beispiel call: 8 Köpfe und 20 Beine
% horseman(s(s(s(s(s(s(s(s(o)))))))), s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(o)))))))))))))))))))), Men, Horses).
% Lösung: 6 Reiter und 2 Pferde
% Addition zweier Peano-Zahlen
add(o   , Y, Y   ) .
add(s(X), Y, s(Z)) :- add(X,Y,Z).

% Multiplikation zweier Peano-Zahlen
mult(o   , _, o) .
mult(s(X), Y, Z) :- mult(X, Y, A), add(A, Y, Z).

horseman(Heads, Feet, Men, Horses) :- add(Men, Horses, Heads), add(Men, Horses, Heads),add(MF, HF, Feet), mult(s(s(o)), Men, MF), mult(s(s(s(s(o)))), Horses, HF).
