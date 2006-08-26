S = QQ[a..d];
I = monomialCurveIdeal(S,{1,3,4})
R = S/I
X = Proj R
IX = sheaf (module I ** R)
Ext^1(IX,OO_X)
Hom(IX,OO_X)
