R = ZZ[x,y,z]
f = y^2 - y - x^3 + x^2
f = homogenize(f,z)
T = R/f
dim T
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test crash2.out"
-- End:
