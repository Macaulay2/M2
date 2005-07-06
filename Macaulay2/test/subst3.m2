R = QQ[x]
c = promote(1/2,R)

assert( substitute ( c , {x => 0_R} ) == c )

-- the following fails (Macaulay2-0-8-58)
-- since we can't compare a QQ to a R:
-- assert( substitute ( c , {x => 0_QQ} ) == c )

-- the following fails (Macaulay2-0-8-58) since it tries to 
-- make a ring map from R to ZZ, instead of R to QQ, or R to R:
-- assert( substitute ( c , {x => 0} ) == c )

-- Over a finite field this works
kk = GF(25)
R = kk[x]
c = promote(1/2,R)
debug Macaulay2Core
F = sub2(ring c,{x=>0})
F c
F c == c
c0 = substitute(c, {x => 0})
c1 = substitute(c, {x => 0_kk})
c2 = substitute(c, {x => 0_R})
-- assert(c0 == c) -- this fails (Macaulay2-0-8-58)
assert(c1 == c) -- this works
assert(c2 == c) -- this works
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test subst3.out"
-- End:
