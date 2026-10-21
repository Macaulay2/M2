-- skew commutative settings are reindexed
kk = ZZ/17
R = kk[a..d, SkewCommutative => true]
I = ideal {a, b}
R' = prune(R/I)
assert(isSkewCommutative R')
assert(R'.SkewCommutative == {0, 1})

-- if they are all pruned away the ring is commutative now
S = kk[a..d, SkewCommutative => {1, 2, 3}]
J = ideal {a^2 + 1, b, c, d}
S' = prune(S/J)
assert(isCommutative S')

-- mix of skew and commuting variables reindexed correctly
T = kk[a..d, SkewCommutative => {1, 2, 3}]
K = ideal {c, d}
T' = prune(T/K)
assert(isSkewCommutative T')
assert(T'.SkewCommutative == {1})
