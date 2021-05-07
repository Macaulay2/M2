A = ZZ/32003[x, y, z, t, Degrees => {2,1,1,1}]
B = A/(x*y+z^3+z*t^2)
f = matrix{{x}}
assert( substitute(f,0) == 0 )

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test subst2.out"
-- End:
