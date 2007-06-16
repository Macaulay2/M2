-----------------------------------------------------------------------------

kk=ZZ/101
R=kk[a,b,c,SkewCommutative=>true]
m=map(R^{-1,0},R^{-2,-1},matrix{{a,0},{b*c,a}})
betti m
F=res(coker m, LengthLimit=>5)
betti F
assert( prune coker F.dd_2 == prune image F.dd_1 )


-- and now 'prune' for coherent sheaves

S = QQ[x]
X = Proj S
n = ideal vars S

F = OO_X(8)
degrees F
F' = prune F
degrees F'				
assert( first first degrees F' <= 0 ) -- too much, but that's okay

F = OO_X(-8)
degrees F
F' = prune F
degrees F'
assert( module F' == S^{0} )  -- on the nose, okay
-----------------------------------------------------------------------------

S = QQ[x..z]
X = Proj S
n = ideal vars S

F = S^{8}
M = n^3 * F
M' = module prune sheaf_X M
degrees M'
assert ( M' == F )

-----------------------------------------------------------------------------

-- a conceptual problem with the design:
R = QQ[x]
M = coker matrix {{x,x}}
N = prune M
assert( target N.cache.pruningMap === M )
P = prune N						    -- N is already pruned!
assert( target P.cache.pruningMap === N )

-----------------------------------------------------------------------------
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test prune.out"
-- End:
