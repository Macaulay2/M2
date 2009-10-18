A = ZZ[x]
B = A/8
f = 6*x
g = 2*x+4*x
f-g
assert( f-g == 0 )
f==g
assert( f == g )

R = QQ[a]/a^2[x]
assert( a_R^2 == 0 )
R = QQ[a]/a^2[x][y]
assert( a_R^2 == 0 )
R = QQ[a]/a^2[x][y]/y^3
assert( a_R^2 == 0 )

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test quotientring.out"
-- End:
