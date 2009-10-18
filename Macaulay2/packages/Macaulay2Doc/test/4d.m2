-- taken from:
-- Bernd Sturmfels, FOUR COUNTEREXAMPLES IN COMBINATORIAL ALGEBRAIC GEOMETRY.

R = QQ[v,w,x,y,z]
S = QQ[a,b,c,d,e,f,g,h,i]
p = map(R,S, {z, v*z, w*z, x*z, v*w*x*y*z, v*w*x^2*y^2*z,
	  v*w^2*x^2*y^3*z, v*w^2*x^3*y^4*z, v*w^2*x^3*y^5*z})
IX = kernel p
assert( degree IX == 18 )
assert( codim IX == 4 )
assert( pdim coker gens IX == 4 )
inIX = ideal leadTerm gens gb IX
assert( codim inIX == 4 )
assert( pdim coker gens inIX == 5 )
S' = QQ[f,g,h,i,a,b,c,d,e]
IX' = (map(S',S)) IX
inIX' = ideal leadTerm gens gb IX'
assert( codim inIX' == 4 )
assert( pdim coker gens inIX' == 4 )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test 4d.out"
-- End:
