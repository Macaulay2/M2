R = QQ[x_0..x_3]
C = Proj(R/monomialCurveIdeal(R, {1,3,4}));
P = hilbertPolynomial C
euler P
