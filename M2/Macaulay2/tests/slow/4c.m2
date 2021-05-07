errorDepth = 0

-- taken from:
-- Bernd Sturmfels, FOUR COUNTEREXAMPLES IN COMBINATORIAL ALGEBRAIC GEOMETRY.

S = ZZ/101[a,b,c,d,e,f]
I = ideal( a*(c+d) - b*f,
     b*(b+f) - c*e,
     c*(a+c+f) - d*(c+d),
     d*f - e*(b+f),
     e*e - a*(a+c+f) )
assert( codim I == 4 )
assert( pdim coker gens I == 4 )
assert( # decompose I == 1 )

S = QQ[a,b,c,d,e,f]
I = ideal( a*(c+d) - b*f,
     b*(b+f) - c*e,
     c*(a+c+f) - d*(c+d),
     d*f - e*(b+f),
     e*e - a*(a+c+f) )
assert( codim I == 4 )
assert( pdim coker gens I == 4 )
J = trim( minors(4,jacobian I) + I )
assert( codim J == 6 )
assert( 
     hilbertPolynomial coker gens I == 
     -17 * projectiveHilbertPolynomial 0 + 11 * projectiveHilbertPolynomial 1
     )
R = S/I
C = res (coker vars R, LengthLimit => 5)
betti C
assert( regularity C == 1 )
assert( rank C_5 == 281 )
assert( tally degrees C_3 === tally splice {51 : {3}, 1 : {4}} )
assert( tally degrees C_5 === tally splice {216 : {5}, 65 : {6}} )
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test/slow 4c.out"
-- End:
