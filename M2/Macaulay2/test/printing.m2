assert( net "T\n 2,3" == net T_(2,3) )

printWidth = 190
long = w -> concatenate between(" ", 200 : w)
p = new ParagraphList from { long "abc", long "abcd", long "abcde" }
assert ( width net p < printWidth )

-- Local Variables:
-- compile-command: "make printing.okay "
-- End:
