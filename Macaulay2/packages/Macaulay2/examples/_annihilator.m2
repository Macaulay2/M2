R = QQ[a..d];
J = monomialCurveIdeal(R,{1,3,4})
M = Ext^2(R^1/J, R)
annihilator M
A = R/(a*b,a*c,a*d)
ann a
annihilator(M, Strategy=>Quotient)
