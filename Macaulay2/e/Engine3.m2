needs "Engine.m2"

-- Test of monomial orders
-- Creation, and their use.

-- Graded reverse lex order
makeRing monomialOrder(RevLex=>6)  
assert(leadTerm(a+b^100) == b^100)
assert(leadTerm(a*c + b^2) == b^2)
assert(leadTerm(a*b-1_R) == a*b)
assert(leadTerm(a^3 - b^3) == a^3)
assert(leadTerm(a^3 - b^4) == -b^4)
assert(leadTerm(a+b+c+d+e+f) == a)

-- Lex order (not graded)
makeRing monomialOrder(Lex=>6)
assert(leadTerm(a+b^100) == a)
assert(leadTerm(a*c + b^2) == a*c)
assert(leadTerm(a*b-1_R) == a*b)
assert(leadTerm(a^3 - b^3) == a^3)
assert(leadTerm(a+b+c+d+e+f) == a)

-- Weighted reverse lex order
assert(try (makeRing monomialOrder(RevLex=>{0,0,0,0,0,0})) else true)
makeRing monomialOrder(RevLex=>{1,1,1,6,6,10})
assert(leadTerm(a+b+c+d+e+f) == f)
assert(leadTerm(a^5 + d) == d)
assert(leadTerm(a^6 + d) == a^6)
assert(leadTerm(c*d-b^2) == c*d)
assert(leadTerm(a*c-b^2) == -b^2)

-- Weight function
makeRing monomialOrder(Weights=>{-1,-1,-1,-1,-1,-1},RevLex=>6)
see R 
c+a^2+b^2
assert(leadTerm(a^2+b^2+c) == c)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
-- End:
