R = ZZ[x,y]
i = ideal(6*x^3, 9*x*y, 8*y^2)
j1 = ideal(-3, x^2)
j2 = ideal(4*y)
assert( intersect(i:j1,i:j2) == i:(j1 + j2) )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test ragnar.out"
-- End:
