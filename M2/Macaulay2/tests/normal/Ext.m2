-- total Ext
R = ZZ/103[x,y]/(x^3,y^2)
N = cokernel random (R^1, R^{-2,-2})
E = Ext(N,N);
S = ring E;
E
E2 = Ext^2(N,N)
E3 = Ext^3(N,N)
assert( rank source basis( {-2,-4}, E) === rank source basis( {-4}, E2 ) )
assert( rank source basis( {-2,-3}, E) === rank source basis( {-3}, E2 ) )
assert( rank source basis( {-2,-2}, E) === rank source basis( {-2}, E2 ) )
assert( rank source basis( {-3,-4}, E) === rank source basis( {-4}, E3 ) )
assert( rank source basis( {-3,-3}, E) === rank source basis( {-3}, E3 ) )
assert( rank source basis( {-3,-2}, E) === rank source basis( {-2}, E3 ) )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test Ext.out"
-- End:
