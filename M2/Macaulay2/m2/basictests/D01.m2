assert = x -> if not x then error "assertion failed "

-- an error here would mean a symbol got stored in the wrong bucket
scan(dictionaryPath, d -> scan(pairs d, (nam,sym) -> assert ( d#nam === sym ) ) )
-- try to defeat any caching of hash code
scan(dictionaryPath, d -> scan(pairs d, (nam,sym) -> assert ( d#(concatenate toList nam) === sym ) ) )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/basictests D01.okay"
-- End:
