assert( userSymbols() === {} )
x = 4
assert( userSymbols() === {symbol x, symbol o1, symbol o2} )
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test user.okay "
-- End:
