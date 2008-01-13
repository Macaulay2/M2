R = QQ[a..i, MonomialOrder => Eliminate 3];
I = ideal(a^2, b-f, d^4, i - b);
selectInSubring(1, gens gb I)
R = QQ[a..i, MonomialOrder => {Eliminate 3,4,2}];
d^3 - a*e^4 + b^2*i + a*c*d*f +a*c^2*g + a*c*g
I = ideal(a..i)
selectInSubring(1, gens gb I);
selectInSubring(2, gens gb I);
selectInSubring(3, gens gb I);
