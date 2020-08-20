R = ZZ[a,b][x,y];
f = a+x;
substitute(f, { a => b } )
a' = a + 0_R
leadMonomial a'
baseName a'
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test subst4.out"
-- End:
