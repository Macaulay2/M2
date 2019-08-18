f = map(QQ^3,QQ^6, {{1,2,3,4,5,6},{4,5,3,2,4,6},{1,4,5,3,6,7}})
M = ker f
g = gens ker f
assert( f * g == 0 )
assert( prune homology (f,g) == 0 )
assert( source g == QQ^3 )
assert( target g == QQ^6 )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test linalg.out"
-- End:
