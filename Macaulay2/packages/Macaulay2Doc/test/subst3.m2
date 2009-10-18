kk = GF(25,Variable => a)
ambient kk
b = promote(1/2,kk)

f = map(kk,kk,{a})
assert( f 1_kk == 1_kk )
assert( f a == a )

f = map(kk,kk)
assert( f 1_kk == 1_kk )
assert( f a == a )

R = kk[x]
c = promote(1/2,R)

h = map(kk,R)
assert( h (a*1_R) == a )
assert( substitute(a*1_R, {x => 0_kk}) == a )

assert( h c == b )					    -- this fails now!
assert( substitute(c, {x => 0_kk}) == b )		    -- this fails now, too!
assert( substitute ( c , {x => 0_R} ) == c )		    -- this fails now, too!

-- =============================================================================

R = QQ[x]
c = promote(1/2,R)
assert( substitute ( c , {x => 0_QQ} ) == c )
assert( substitute ( c , {x => 0_QQ} ) == 1/2 )

-- the following fails (Macaulay2-0-8-58) since it tries to 
-- make a ring map from R to ZZ, instead of R to QQ, or R to R:
-- assert( substitute ( c , {x => 0} ) == c )

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test subst3.out"
-- End:
