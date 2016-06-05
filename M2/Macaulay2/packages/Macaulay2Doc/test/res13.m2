-- git issue 329.  This used to crash (LengthLimit => 0 is the issue).
S = ZZ/101[a..e]
matrix"a,b,c; b,c,d"
R = S/minors(2, matrix"a,b,c; b,c,d")
M = coker vars R
I = ideal vars R
C = res (R^1/I, LengthLimit => dim M)
assert(length C == 0)
