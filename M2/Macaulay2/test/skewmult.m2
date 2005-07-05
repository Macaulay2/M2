S = (ZZ/101 [a, b, c, d, SkewCommutative => true])/(a*b,c*d)
A = matrix {{0, c, 0, b, 0, a}, {c, 0, b, 0, a, 0}, {b, a, 0, 0, 0, 0}}
B = matrix {{a}, {0}, {0}, {0}, {c}, {0}}
A * B
assert(A*B==0)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test skewmult.out"
-- End:
