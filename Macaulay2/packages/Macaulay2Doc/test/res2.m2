R = ZZ/101[x,y]/(x^2-y^3)
I = ideal(x,y)
M = Hom(I,I)
C = res M
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test res2.out"
-- End:
