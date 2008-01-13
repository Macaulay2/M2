M = ZZ^2 ++ ZZ^3
M^[0]
M^[1]
M^[1,0]
R = QQ[a..d];
M = (a => image vars R) ++ (b => coker vars R)
M^[a]
isWellDefined oo
M^[b]
isWellDefined oo
isWellDefined(M^{2})
C = res coker vars R
D = (a=>C) ++ (b=>C)
D^[a]
