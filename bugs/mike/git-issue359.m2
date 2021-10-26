-- git issue 359.  For awhile, this gave the incorrect answer of 12.
k = ZZ/101
R = k[x_0..x_5]
M = random(R^6, R^{6:-1})
M = M - transpose M
S = R/pfaffians(6,M)
N = sub(M,S)
assert(rank N == 4)

