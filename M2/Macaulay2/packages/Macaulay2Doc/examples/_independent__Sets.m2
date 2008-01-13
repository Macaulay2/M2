R = QQ[a..h];
I = minors(2,genericMatrix(R,a,2,4))
inI = ideal leadTerm I
independentSets I
independentSets inI
I = ideal"abc,bcd,cde,adf,cgh,b3f,a3g"
minimalPrimes I
independentSets I
L = independentSets(I, Limit=>1)
support L_0
rsort toList(set gens R - set support L_0)
