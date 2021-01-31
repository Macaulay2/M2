--status: this old test depends on internal things and probably should be deleted


----------------------------------
-- Test of engine monomial code --
----------------------------------

needs "raw-util.m2"
errorDepth = 0

-- These monomials use the internal 'varpower' representation
-- and are not connected to any ring or monoid.  Monomials can have
-- negative exponents, etc.
-- rawVarMonomial  TESTED
-- rawMakeMonomial  TESTED
-- ===  TESTED
-- rawMonomialIsOne (m == 1)  TESTED
-- rawCompareMonomial -- needs a Monoid. NOT TESTED HERE
-- rawMonomialDivides -- needs a Monoid. NOT TESTED HERE
-- rawMonomialDivide -- needs a Monoid. NOT TESTED HERE
-- /  TESTED (computes a*b^-1)
-- *  TESTED
-- rawColon  TESTED
-- ^  TESTED
-- rawLCM  TESTED
-- rawGCD  TESTED
-- rawSaturate TESTED
-- rawRadical TESTED
-- rawSyzygy TESTED
-- hash  TESTED, I guess
-- toString  TESTED
-- rawSparseListFormMonomial TESTED

a = rawVarMonomial 0
b = rawVarMonomial 1
c = rawVarMonomial 2
a2 = rawVarMonomial(0,2)
b2 = rawVarMonomial(1,2)
x = rawVarMonomial(3,4)
x' = rawVarMonomial(3,4)
t = {(2,3),(3,4)}
y = rawMakeMonomial t
o = rawMakeMonomial{}
ab = rawMakeMonomial{(0,1),(1,1)}

assert( hash a2 === hash a^2 )

assert(a*b === ab)
assert(rawVarMonomial(5,-100) * rawVarMonomial(5,100) === o)

assert(o == 1)
assert(a2 != 1)
assert(a2 / a === a)
assert(a2 / b2 == rawVarMonomial(0,2) * rawVarMonomial(1,-2))
assert( rawColon(ab,a) == b )
assert( rawColon(ab,a^2) == b )
assert( rawColon(a2,a) == a )
assert( rawColon(a2,a^2) == o )
assert( rawColon(a2,b2) == a2 )
assert( rawColon(a^10, a^5) == a^5 )
assert( rawColon(a^(-1), a^11) == o )
assert( rawColon(a^(-1), a^(-2)) == a )

assert( rawLCM(a*b,a*c) === a*b*c )
assert( rawLCM(a*b^3,a*b*c^4) === a*b^3*c^4 )
assert( rawLCM(a^2*b^3,a^3*b*c^4) === a^3*b^3*c^4 )
assert( rawLCM(a^-2*b^3,a^-3*b*c^4) === a^-2*b^3*c^4 )

assert( rawGCD(a*b,a*c) === a )
assert( rawGCD(a*b^3,a*b*c^4) === a*b )
assert( rawGCD(a^2*b^3,a^3*b*c^4) === a^2*b )
assert( rawGCD(a^-2*b^3,a^-3*b*c^4) === a^-3*b )

rawSyzygy(a*b, a*c)
p = rawSyzygy(a^3*b, b^4*c*rawVarMonomial(10,15))
p#0 === rawMakeMonomial(rawSparseListFormMonomial(p#0))
rawSaturate(a^3*b^2*c,b*c)
rawSaturate(a^3*b^2*c,b*c^2)
rawSaturate(a^3*c,b*c^2)
rawSaturate(a*b^232131*c,a*c)
rawSaturate(a*b^232131*c,b)

assert(rawRadical(a*b^5*c^2*a) === a*b*c)
rawRadical(a^-2*b^5*c^2) -- should this return a^-1*b*c ??

lastone = 2^31-1
atop = rawVarMonomial(0,lastone)
assert try (rawVarMonomial(0,lastone+1); false) else true
assert try (atop*a; false) else true
assert(try (rawVarMonomial(-1,4); false) else true)
a' = rawVarMonomial(1000,-1)
a'top = a'^lastone
a'realtop = a'top * a'
assert(try (a'realtop * a' ;false) else true)
assert(rawSparseListFormMonomial rawVarMonomial(lastone,-1) == {(2147483647, -1)})
assert(rawSparseListFormMonomial rawVarMonomial(1000,0) == {})
assert(rawSparseListFormMonomial (a*a') == {(0,1),(1000,-1)})

assert( toString x === "d4" )
assert( x === x' )
assert not mutable x
assert( toString y === "d4c3" )
assert( x =!= y )
assert( not (x === y) )
assert( rawSparseListFormMonomial y ===  t )
assert ( x == x' )
assert ( not (x == y))
assert not ( x == 1 )
assert ( o == 1 )

assert( a * b == ab )
assert( ab * o == ab )
assert( ab / b === a )
assert( ab / o === ab )
assert( ab / ab === o )
assert( a^2 == a2 )
assert( o^-555 == o )
assert( gcd(ab,ab) == ab )
assert( gcd(ab,a2) == a )
assert( gcd(a2,b2) == o )
assert( gcd(a^5*b^3*c^7 , a^7*b^2*c) == a^5*b^2*c )

assert( rawSyzygy(ab,b2) == (b,a) )

-- check that error conditions are caught in the front end
needs "raw-util.m2"
assert try (rawVarMonomial(-1,4);false) else true
assert try (rawMakeMonomial{(3,4),(1,2)};false) else true
print "ERROR: error message for   rawMakeMonomial{(3,4),(1,2)}  is bad"
--  rawMakeMonomial{(3,4),(1,2)}

lastone = 2^31-1
atop = rawVarMonomial(0,lastone)
a = rawVarMonomial(0,1)
assert try (atop * a;false) else true
print "ERROR: overflow for rawMonomialPower not checked"
--     atop^lastone

-- Local Variables:
-- compile-command: "M2 -e errorDepth=0 --stop -e 'load \"raw-monomial.m2\"' -e 'exit 0' "
-- End:
