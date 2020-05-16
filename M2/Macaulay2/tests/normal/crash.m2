-- this one is from David Eisenbud, makes version 0.4.48 crash

R = ZZ/32003[a..d]
C = resolution cokernel substitute(
     random(R^{0},R^{-1,-1}),
     R/monomialCurveIdeal(R,{1,3,4})
     )
try transpose C.dd_2 % C.dd_1
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test crash.out"
-- End:
