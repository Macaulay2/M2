R = ZZ/101[a..d];
S = coimage map(R, R, {a^4, a^3*b, a*b^3, b^4});
presentation S
h =  hilbertPolynomial S
hilbertPolynomial(S, Projective=>false)
apply(5, k-> h(k))
apply(5, k-> hilbertFunction(k,S))
