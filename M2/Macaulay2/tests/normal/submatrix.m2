S = ZZ/101 [a, b, c, d]
B = matrix {{0}, {0}, {0}, {0}, {0}, {0}, {a}, {0}, {0}, {0}, {c}, {0}}
B0 = submatrix(B,{6..11},)  -- BUG!!
B = submatrix(B,{6..11},{0})
assert(rank source B0 == 1)
assert(rank target B0 == 6)
assert(rank source B == 1)
assert(rank target B == 6)
assert(B == B0)

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test submatrix.out"
-- End:
