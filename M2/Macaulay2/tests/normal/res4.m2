R = ZZ/101[x,y]/(x^2-y^2)
I = ideal(x,y)
M = Hom(I,I)
C = res M
M = Hom(I,I)
D = res(M, Strategy=>3, LengthLimit=>6)
assert(length D > 4)

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test res4.out"
-- End:
