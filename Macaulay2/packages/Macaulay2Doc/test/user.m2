assert( userSymbols() === {} )
x = 4
xx = 4
assert( userSymbols() === {symbol x, symbol xx} )
x = symbol x
assert( userSymbols() === {symbol xx} )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test user.out"
-- End:
