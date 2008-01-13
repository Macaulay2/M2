R = QQ[a..d];
I = monomialCurveIdeal(R, {1,3,4});
h = hilbertPolynomial I
hilbertPolynomial (R/I)
hilbertPolynomial(I, Projective=>false)
apply(5, k-> h(k))
apply(5, k-> hilbertFunction(k,I))
