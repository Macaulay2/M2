R = ZZ/101[a,r..z]
f = a  * id_(R^1)
g = a^2* id_(R^1)
assert( degree f == {1} )
assert isHomogeneous f
assert( degree g == {2} )
assert isHomogeneous g
h = g // f
assert( degree h == {1} )
assert isHomogeneous h

chk = (f,g) -> assert( target f == target g and (h := f//g; target h == source g and source h == source f and f % g + g * h == f ))
M = R^2
N = R^3
chk(map(M,N,0),map(M,M,1))
chk(map(M,M,1),map(M,N,0))
chk(map(M,M,{{r,s},{t,u}}),map(M,N,{{r,s,t},{u,v,w}}))


f = map(R^2,,{{x},{y}})
g = map(R^2,,{{x},{z}})
(q,r) = quotientRemainder(f,g)
assert( g*q+r == f )
assert( degree g == {0} )
assert( degree q == {0} )
assert( degree f == {0} )

R = ZZ[x,d,WeylAlgebra=>{x=>d}]
f = matrix{{x*d,d*x,x,d}}
g = matrix{{x,d}}
(q,r) = quotientRemainder(f,g)
assert(g*q+r == f)
g = matrix{{x}}
(q,r) = quotientRemainder(f,g)
assert(g*q+r == f)
g = matrix{{d}}
(q,r) = quotientRemainder(f,g)
assert(g*q+r == f)

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test lift.out"
-- End:
