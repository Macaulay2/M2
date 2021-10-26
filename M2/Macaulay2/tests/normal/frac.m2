temp = z -> (
     R := ZZ/101[x,y,w];
     oldw = w;
     x/y;
     )

temp()

assert( w === oldw )

-----------------------------------------------------------------------------

-- From: Allen Knutson <allenk@Math.Berkeley.EDU>
-- Subject: Reproducible segfault in rational function arithmetic
-- To: Macaulay2@math.uiuc.edu
-- Date: Sat, 19 Jul 2003 10:39:01 -0700 (PDT)

R = ZZ[d,e];
d + (e/d)						   -- this may crash or give "zero divisor found"

-----------------------------------------------------------------------------

R = ZZ[x]
F = frac R
f = x * (x^2 + 1)
g = (x^3+1) * (x^2 + 1)
assert(numerator (f/g) == x)

-- We should have canonical forms for fractions
-- This is more a wish than a bug report
-- It depends on having a coefficient ring where the orbits of the group of units acting on the ring
--    have canonical representatives.  E.g., if the coefficient ring is a field, we can make the coefficient
--    of the leading term be 1.
-- assert(numerator((-f)/(-g)) == numerator(f/g))

-----------------------------------------------------------------------------
debug Core
R = QQ[x]
b = x^3*(x-1)*(1/2)
c = x^5-1
f = b/c
g = (2*b)/(2*c)
gcd(b,c)
rawGCD(raw b, raw c)
assert( f === g )
-- assert( numerator f === numerator g ) -- (commented out, see wish above)
f
debug Core
h = rawGCD(raw numerator f, raw denominator f)
-----------------------------------------------------------------------------
R = QQ[x]
f1 = x^3*(x-1)*(1/2)
g1 = x^5-1
gcd(f1,g1)
rawGCD(raw f1, raw g1)
assert(rawGCD(raw f1,raw g1) =!= raw(1/8*x-1/8))
assert(rawGCD(raw f1,raw g1) === raw(x-1))
-----------------------------------------------------------------------------
R = ZZ[x]
F = frac R
f = ((x^3*(x-1)*(1/2))/((x^5-1)))
f = 1/2 * ((x^3*(x-1))/((x^5-1)))
g = ((x^3*(x-1))/(2*(x^5-1)))
assert( f === g )
assert( numerator f === numerator g )
-----------------------------------------------------------------------------
R = QQ[x,y]/(y)
F = frac R
f = ((x^3*(x-1)*(1/2))/((x^5-1)))
g = ((x^3*(x-1))/(2*(x^5-1)))
assert( f === g )
assert( numerator f === numerator g )
-----------------------------------------------------------------------------
A = ZZ/101[symbol a,symbol b]/(a*b)
B = frac A
b/b
(-a-b)/(a+b)
getNonUnit B
assert try (a/b; false) else true
getNonUnit B
isField B
-----------------------------------------------------------------------------
A = ZZ/101[a,b]/(a*b)
L = toField A
assert try (1/(a+b); false) else true
assert(getNonUnit L == a+b)
assert not isField A
B = frac A

A = ZZ/101[a,b]/(a*b)
1/(a+b)
B = frac A
getNonUnit B
isField B

T = QQ[symbol x,symbol y]/(x^3-1,y-3)
F = frac T
use F
assert try ((x-1)/(x^2+x+1) + (x^2+x+1)/(x-1); false ) else true
getNonUnit F
assert(class getNonUnit F === F)

(1/x)^3

-----------------------------------------------------------------------------
T = QQ[symbol x,symbol y]/(x^3*y^3)
F = frac T
use F
(x-1)/(x^2+x+1) + (x^2+x+1)/(x-1) -- We see that the system is getting 
                                  -- close to discovering a zero divisor...
getNonUnit F

(1/x)^3
assert(null === getNonUnit F)
assert isField F
assert try ((1/y)^3 + (1/x)^3; false) else true
getNonUnit F
assert not isField F
-----------------------------------------------------------------------------
 A = ZZ/101[symbol a,symbol b]/(a*b)
 (a+b)/(a-b)
 assert try (a/b;false) else true			    -- should detect a non-unit here
-----------------------------------------------------------------------------
F = frac (ZZ[x,y]/x^2)
try (1_F/x)^2
assert( getNonUnit F === x )

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test frac.out"
-- End:
