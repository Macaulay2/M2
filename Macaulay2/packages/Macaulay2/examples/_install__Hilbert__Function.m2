R = ZZ/101[a..g];
I = ideal random(R^1, R^{3:-3});
hf = poincare ideal(a^3,b^3,c^3)
installHilbertFunction(I, hf)
gbTrace=3
time poincare I
time gens gb I;
R = QQ[a..d];
I = ideal random(R^1, R^{3:-3});
time hf = poincare I
S = QQ[a..d,MonomialOrder=>Eliminate 2]
J = substitute(I,S)
installHilbertFunction(J, hf)
gbTrace=3
time gens gb J;
selectInSubring(1,gens gb J)
