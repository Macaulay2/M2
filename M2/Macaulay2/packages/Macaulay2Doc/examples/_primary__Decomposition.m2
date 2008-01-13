R = QQ[a..i];
I = permanents(2,genericMatrix(R,a,3,3))
C = primaryDecomposition I;
I == intersect C
#C
C/toString/print;
C/codim
C/degree
associatedPrimes I / print;
