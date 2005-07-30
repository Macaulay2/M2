R = QQ[a..d];
M = module monomialCurveIdeal(R, {1,3,4});
h = hilbertPolynomial M
hilbertPolynomial(M, Projective=>false)
