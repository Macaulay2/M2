assert( userSymbols() === {} )
x = 4
xx = 4
assert( userSymbols() === {symbol xx} )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test user.out"
-- End:
