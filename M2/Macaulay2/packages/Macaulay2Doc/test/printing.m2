assert( net "T\n 2,3" == net T_(2,3) )

printWidth = 190
long = w -> demark(" ", 200 : w)
p = wrap net { long "abc", long "abcd", long "abcde" }
assert ( width p <= printWidth )

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test printing.out"
-- End:
