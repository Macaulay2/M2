X = Proj(QQ[x_0 .. x_4] / ( sum(5,i->x_i^5) - 5 * product(5,i->x_i) ))
b = (p,q) -> rank HH^p cotangentSheaf_q X
assert(
     table(0..3, 0..1, b)
     ==
     ((1, 0), (0, 1), (0, 101), (1, 0))
     )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test calabi-yau.out"
-- End:
