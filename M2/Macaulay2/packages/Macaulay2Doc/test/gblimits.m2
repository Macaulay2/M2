-- this was a problem with autoreducing gb computations that are continued 
-- after having been stopped with a limit option
compactMatrixForm = false
R = QQ[x_1..x_4,MonomialOrder => Lex]
g = (x_1^2 + x_1*x_4+1,x_1*x_2*x_3*x_4+1)
gens gb(I = ideal g, BasisElementLimit => 2)
gens gb I
gens gb ideal g
assert( oo === ooo )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test gblimits.out"
-- End:
