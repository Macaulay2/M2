R = ZZ[symbol a..symbol f]
I = monomialIdeal(a^3,b^2*c,a*c*d)     
J = saturate(I,a)
J1 = monomialIdeal(1_R)
assert(J == monomialIdeal(1_R))
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test monideal4.out"
-- End:
