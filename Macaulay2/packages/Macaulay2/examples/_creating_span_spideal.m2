R = QQ[a..d];
I = ideal (a^2*b-c^2, a*b^2-d^3, c^5-d)
J = monomialIdeal (a^2*b, b*c*d, c^5)
monomialCurveIdeal(R,{1,2,3})
