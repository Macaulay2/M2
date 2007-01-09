R = QQ[a..d];
f = map(R^1,coker vars R,{{1_R}})
isWellDefined f
isWellDefined map(coker vars R, R^1, {{1_R}})
A = ZZ/5[a]
factor(a^3-a-2)
B = A/(a^3-a-2);
isWellDefined map(A,B)
isWellDefined map(B,A)
