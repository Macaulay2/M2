(a,b,c) = (4,5,6)
assert( a == 4 and b == 5 and c == 6 )
(a,b,c) = (4,5)
assert( a == 4 and b == 5 and c == null )
(a,b,c) = 4
assert( a == 4 and b == null and c == null )
(a,b,c) = (4,5,6,7)
assert( a == 4 and b == 5 and c == (6,7) )
(a,b,c) = (4,5,6,7,8)
assert( a == 4 and b == 5 and c == (6,7,8) )
(a,b,c) = ()
assert( a == null and b == null and c == null )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test parallel.out"
-- End:
