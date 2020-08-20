k=ZZ/101
R=k[x]/x^2
C = res (coker vars R, LengthLimit => 3 )
n = transpose C.dd_1
D = res (coker n, LengthLimit => 3)
print betti D
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test ggbetti.out"
-- End:
