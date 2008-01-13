R = ZZ/32003[a..d];
I = monomialCurveIdeal(R,{1,3,4})
J = ideal(a^3,b^3,c^3-d^3)
I = intersect(I,J)
removeLowestDimension I
topComponents I
radical I
minimalPrimes I
