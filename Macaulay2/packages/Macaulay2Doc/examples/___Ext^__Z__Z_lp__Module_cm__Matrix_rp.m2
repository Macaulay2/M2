R = ZZ/32003[a..d];
I = monomialCurveIdeal(R,{1,3,4})
M = R^1/I
f = inducedMap(R^1,module I)
Ext^1(M,f)
g = Ext^2(M,f)
source g == Ext^2(M,source f)
target g == Ext^2(M,target f)
Ext^3(f,R)
