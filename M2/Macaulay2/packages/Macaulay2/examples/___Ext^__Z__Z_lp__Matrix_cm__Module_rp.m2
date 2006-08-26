R = ZZ/32003[a..d];
I = monomialCurveIdeal(R,{1,3,4})
M1 = R^1/I
M2 = R^1/ideal(I_0,I_1)
f = inducedMap(M1,M2)
Ext^1(f,R)
g = Ext^2(f,R)
source g == Ext^2(M1,R)
target g == Ext^2(M2,R)
Ext^3(f,R)
