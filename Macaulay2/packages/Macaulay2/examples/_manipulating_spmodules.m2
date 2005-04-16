R = QQ[x,y];
I = ideal vars R
M = image vars R
N = prune M
f = N.cache.pruningMap
isIsomorphism f
f^-1
source f
target f
super M
cover N
M ++ N
M ** N
M^3
I^3
