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

-- we should have canonical forms for fractions
assert(numerator((-f)/(-g)) == numerator(f/g))
-- Local Variables:
-- compile-command: "make frac.okay "
-- End:
