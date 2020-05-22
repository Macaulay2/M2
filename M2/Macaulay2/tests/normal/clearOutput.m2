debuggingMode = false;
S = QQ[t, y_0 .. y_8, a..i, MonomialOrder => Eliminate 10];
t = symbol a
clearAll
assert( symbol a === first (a .. j))
S = QQ[x, y, z, a..j, MonomialOrder => Eliminate 2];
assert instance(a,S)
