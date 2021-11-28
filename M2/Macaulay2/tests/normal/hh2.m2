A = ZZ/101[a,b,c,d]
R = A/(a^4+b^4+c^4+d^4)
X = Proj R

hodgeDiamond = table(3,3,(p,q) -> timing ((p,q) => hh^(p,q) X))
assert( {{1, 0, 1}, {0, 20, 0}, {1, 0, 1}} === applyTable(hodgeDiamond, last @@ last) )
print new MatrixExpression from hodgeDiamond

H = HH^0((cotangentSheaf(2,X))(>=0));
basis(0,H)
betti res (coker lift(presentation H,A) ** coker presentation R )
q = hilbertSeries H
h = hilbertFunction(0,H)
assert( h == 1 )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test hh2.out"
-- End:
