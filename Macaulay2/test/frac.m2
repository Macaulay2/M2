-- if getenv "USER" == "dan" then exit 0


temp = z -> (
     R := ZZ/101[x,y,w];
     oldw = w;
     x/y;
     )

temp()

assert( w === oldw )

-----------------------------------------------------------------------------

R = ZZ[x]
F = frac R
f = x * (x^2 + 1)
g = (x^3+1) * (x^2 + 1)
assert(numerator (f/g) == x)

-- we should have canonical forms for fractions
assert(numerator((-f)/(-g)) == numerator(f/g))
