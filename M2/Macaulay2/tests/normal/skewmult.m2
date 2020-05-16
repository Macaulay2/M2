S = (ZZ/101 [a, b, c, d, SkewCommutative => true])/(a*b,c*d)

A = matrix {{c, a}, {b, 0}}
B = matrix {{a}, {c}}

A * B
assert(A*B==0)

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test skewmult.out"
-- End:
