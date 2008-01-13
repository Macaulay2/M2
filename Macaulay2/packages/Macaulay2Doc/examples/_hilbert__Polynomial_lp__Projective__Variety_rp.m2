R = QQ[a..d];
I = monomialCurveIdeal(R, {1,3,4});
V = Proj(R/I)
h = hilbertPolynomial V
hilbertPolynomial(V, Projective=>false)
apply(5, k-> h(k))
apply(5, k-> hilbertFunction(k,V))
