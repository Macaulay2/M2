-- we want some hash codes to stay the same forever, so cached example output gets the same hashcodes
-- if the assertions here fail, it means some new types were created too early in the file Macaulay2/d/tokens.d

assert = x -> if not x then error "assertion failed "
assert ( hash {("a","bcd"),("bcd","a")} == -2128549206 )

-- try even harder to keep hash codes the same
assert ( hash Nothing == 1000069 )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/basictests hashcodes.okay"
-- End:
