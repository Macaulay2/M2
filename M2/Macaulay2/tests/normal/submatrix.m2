S = ZZ/101 [a, b, c, d]
B = matrix {{0}, {0}, {0}, {0}, {0}, {0}, {a}, {0}, {0}, {0}, {c}, {0}}
B0 = submatrix(B,{6..11},)  -- BUG!!
B = submatrix(B,{6..11},{0})
assert(rank source B0 == 1)
assert(rank target B0 == 6)
assert(rank source B == 1)
assert(rank target B == 6)
assert(B == B0)

-- c.f. https://groups.google.com/g/macaulay2/c/P7t43OPwO0E/m/TStzslnIAAAJ
S = ZZ/101[a,b,c,d]
M = matrix"a,b;c,d"
assert(M_{0,1,0,1} == M | M) -- produces the matrix with two columns equal to the first column of M, as it should
assert(M^{0,1,0,1} == M || M) -- produces a matrix with a row of zeros followed by the first row of M -- wrong!

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test submatrix.out"
-- End:
