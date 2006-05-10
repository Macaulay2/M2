assert( net "T\n 2,3" == net T_(2,3) )

printWidth = 190
long = w -> demark(" ", 200 : w)
p = new ParagraphList from { long "abc", long "abcd", long "abcde" }
assert ( width net p < printWidth )

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test printing.out"
-- End:
