R = ZZ/101[x,y,z,t,u,v]
M = coker matrix {{x,y,z,t^2,u^2,v^2}}
N = coker matrix {{x^2,y*z+t^3,t^2*u^2}}
f = inducedMap(M,N)
g = res f
C = source g
D = target g
assert( g * C.dd == D.dd * g )
h = Hom(g,R^1)
assert( h * (source h).dd == (target h).dd * h )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test resmap.out"
-- End:
