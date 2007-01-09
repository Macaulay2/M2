R = QQ[a..d];
I = monomialCurveIdeal(R,{1,3,4})
X = variety I
KX = sheaf(Ext^1(I,R^{-4}) ** ring X)
K2 = KX^**2
prune K2
