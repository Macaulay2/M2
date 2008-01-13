R = ZZ/101[a,b,c];
M = R^1/(a^2,b^2,c^2)
isQuotientModule M
f = M_{0}
N = image f
M == N
isQuotientModule N
