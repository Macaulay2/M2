R = QQ[a..d];
P3 = Proj R
I = monomialCurveIdeal(R,{1,3,4})
G = sheaf module I
Hom(OO_P3,G(3))
HH^0(G(3))
