R = QQ[x,y];
M = image vars R
N = coker presentation M
f = map(M,N,1)
isWellDefined f
isIsomorphism f
g = map(M,cover M,1)
isWellDefined g
isIsomorphism g
h = map(cover M,M,1)
isWellDefined h
