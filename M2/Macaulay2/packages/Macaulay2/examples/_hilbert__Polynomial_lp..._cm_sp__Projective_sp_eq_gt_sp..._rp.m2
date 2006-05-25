R = ZZ/101[a..d];
S = coimage map(R, R, {a^4, a^3*b, a*b^3, b^4});
hilbertPolynomial S
hilbertPolynomial(S, Projective=>false)
