R = ZZ/101[x,y]/(x^2-y^2)
I = ideal(x,y)
M = Hom(I,I)
C = res M
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test res4.out"
-- End:
