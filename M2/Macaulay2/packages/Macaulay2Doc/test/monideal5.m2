-- monomial ideal difference
R = QQ[a..d]
M = monomialIdeal(a^2,a*c,a*d,b^3)
N = monomialIdeal(a^3,a*c,d*b^2)
assert(M - N == monomialIdeal(a^2,a*d,b^3))
assert(N - M == monomialIdeal(d*b^2))

-- monomial ideal decomposition
R = QQ[x];
assert ( irreducibleDecomposition monomialIdeal x == {monomialIdeal x} )
assert ( irreducibleDecomposition monomialIdeal 0_R == {monomialIdeal 0_R} )
