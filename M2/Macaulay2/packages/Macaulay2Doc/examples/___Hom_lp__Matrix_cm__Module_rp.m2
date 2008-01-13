R = QQ[a..d];
I = ideal(a*b,c*d);
J = I + ideal(a*d);
f = inducedMap(module J,module I)
g = Hom(R^3,f)
ker g
image g
