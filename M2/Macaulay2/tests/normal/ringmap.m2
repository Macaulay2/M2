-- test map

f = map(ZZ/5,ZZ)
assert(class f 4 === ZZ/5)
assert(f 4 == -1)
assert(kernel f === ideal 5)
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test ringmap.out"
-- End:
