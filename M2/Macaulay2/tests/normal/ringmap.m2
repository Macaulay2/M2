-- test map

<< "-- this doesn't work yet, but perhaps the presence of" << endl
<< "-- the test will spur us to make it work" << endl
exit 0

errorDepth = 1

f = map(ZZ/5,ZZ)
assert(class f 4 === ZZ/5)
assert(f 4 == -1)
kernel f
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test ringmap.out"
-- End:
