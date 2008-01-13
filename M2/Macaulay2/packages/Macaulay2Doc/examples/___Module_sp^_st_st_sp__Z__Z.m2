R = QQ[a..d];
I = monomialCurveIdeal(R,{1,3,4})
M = Ext^1(I,R^{-4})
M^**2
