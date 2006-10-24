A = QQ[x,y];
gens A
kk = toField(QQ[t]/(t^3-t-1));
B = kk[x,y,z];
generators B
generators(B, CoefficientRing => QQ)
