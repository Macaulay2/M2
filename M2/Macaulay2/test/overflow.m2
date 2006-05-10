
stderr << currentFileName << ": test deferred" << endl
exit 0

S = ZZ/101[x,y,z, MonomialOrder => Eliminate 2, MonomialSize => 16 ];
ourpoints = ideal(y^5-x^4, x*y^2-1, x^5-y^3, x^5+y^5+z^5-1)
gb ourpoints
-- tell Bernd when this starts working


R = QQ[x,MonomialOrder => RevLex]
x^7							    -- monomial overflow


end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test overflow.out"
-- End:
