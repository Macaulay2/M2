A = ZZ[x]
B = A/8
f = 6*x
g = 2*x+4*x
f-g
assert( f-g == 0 )
f==g
assert( f == g )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test quotientring.out"
-- End:
