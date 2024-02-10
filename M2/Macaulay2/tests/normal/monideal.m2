-- Copyright 1995 by Michael Stillman

R = ZZ/101[symbol a..symbol f]
T = (degreesRing R)_0
mi = monomialIdeal matrix {{a^3, a*b*d^4, a*c*e, b*d*f^2, c^2*d^3, c^5}}
mi2 = monomialIdeal matrix {{a^2, c^7, b*d*e*f}}
F = monomialIdeal matrix {{d^4}}

assert(mi : F == monomialIdeal matrix {{a^3, a*b, a*c*e, b*f^2, c^2}})
assert(mi : F == saturate(mi, monomialIdeal matrix {{d}}))
assert(radical mi == monomialIdeal matrix {{a, c, b*d*f}})
assert(mi*mi == monomialIdeal (generators mi ** generators mi))
assert(intersect(mi, mi2) == monomialIdeal matrix {{
    a^3, a^2*c^5, c^7, a^2*c^2*d^3, a^2*b*d^4, a^2*c*e, a*b*c*d*e*f, 
    b*c^5*d*e*f, b*c^2*d^3*e*f, a*b*d^4*e*f, a^2*b*d*f^2, b*d*e*f^2}})
assert(mi + mi2 == monomialIdeal (generators mi | generators mi2))
borel1 = borel monomialIdeal matrix {{b*c*d}}
assert(borel1 == monomialIdeal matrix {{a^3, a^2*b, a*b^2, b^3, a^2*c, a*b*c, b^2*c,
    a*c^2, b*c^2, a^2*d, a*b*d, b^2*d, a*c*d, b*c*d}})
mi3 = borel monomialIdeal matrix {{d*e*f^2}}
assert isBorel mi3
assert(saturate(mi3, monomialIdeal matrix {{a,b,c,d,e}}) == monomialIdeal matrix {{a,b,c,d}})
assert(monomialIdeal independentSets mi2 == monomialIdeal matrix {{b*d*e, b*d*f, b*e*f, d*e*f}})
assert( independentSets ideal vars R === {1_R} )

assert(codim mi2 == 3)
assert(poincare mi2 == 1 - T^2 - T^4 + T^6 - T^7 + T^9 + T^11 - T^13)
assert(poincare mi2 == poincare resolution cokernel generators mi2)

-- primary decomposition of a monomial ideal.
-- routines:
   -- topComponents I
   -- assprimes I
   -- primaryDecomposition I

-- 
I1 = monomialIdeal matrix{{a,b^3,c^4, b^2*c, b*c^3}}
I2 = monomialIdeal matrix{{c^2,b^3,d^4, b*c*d}}
I3 = monomialIdeal matrix{{a,b}}
I4 = monomialIdeal matrix{{a^3,b^3,c^3,d^3}}
I = intersect(intersect(I1,I2),intersect(I3,I4))

-- Bug in newly added code 29 Dec 2022), fixed today.
R = ZZ/101[a,b,c,d]
I1 = monomialIdeal(a^2*b*c,b^3*c,b*d^3)
I2 = monomialIdeal(a*b*c^2,b*c^2*d,c^3*d)
assert(hash I1 == hash I2)
assert(I1 =!= I2)
assert(# unique{I1, I2} == 2)

end--
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test monideal.out"
-- End:
