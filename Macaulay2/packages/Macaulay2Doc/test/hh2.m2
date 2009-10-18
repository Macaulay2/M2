A = ZZ/101[a,b,c,d]
R = A/(a^4+b^4+c^4+d^4)
X = Proj R
H = HH^0((cotangentSheaf(2,X))(>=0));
basis(0,H)
betti res (coker lift(presentation H,A) ** coker presentation R )
q = hilbertSeries H
h = hilbertFunction(0,H)
assert( h == 1 )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test hh2.out"
-- End:
