-- gcd problems:
--   doc is lacking
--   rawGCDRingElement is not being called from the front end.
--     and in fact seems to be problematic...
-- GF code
--   works if the ring is declared via GF
--   but we can't seem to use rawGCDRingElement to get the minpoly in?
--   
-- 

R = ZZ[x,y,z]
F = (3*x^3-y*x+17*y^4)^3 * (x+y+1)^4
G = (3*x^3-y*x+17*y^4)^2 * (x-y+1)^4
gcd(F,G) == (3*x^3-y*x+17*y^4)^2

R = QQ[x,y,z]
F = (3*x^3-y*x+17*y^4)^3 * (x+y+1)^4
G = (3*x^3-y*x+17*y^4)^2 * (x-y+1)^4
gcd(F,G) == (3*x^3-y*x+17*y^4)^2
gcd(1/2*F,1/3*G) == (3*x^3-y*x+17*y^4)^2

K = QQ[a]/(a^6-a^3-1)
toField K
R = K[t]
F = t^6-t^3-1
G = (t-a)*(t+a)
gcd(F,G)  -- via syzygies.
F//(t-a)
gcd(F//(t-a), t-a-1)
debug Core
rawGCD(raw(F//(t-a)), raw(t-a-1), raw (ideal K)_0)
K = QQ[a,b]/(a^6-a^3-1,b^2-a)
toField K
R = K[t]
F = t^6-t^3-1
G = (t-a)*(t+a)
gcd(F,G)  -- works, uses syzygies
F//(t-a)
gcd(F//(t-a), t-a-1)
gcd((t-a)^3*(t-b), (t-a)*(t-b)^3) == (t-a)*(t-b)

-- Example: computing gcd's over Galois fields given via toField
-- 
loadPackage "ConwayPolynomials"
A = GF(9, Variable=>a)
R = A[x]
F = (a*x^3-2*a*x^2-x-1)*(x-a)
G = (a*x^3-2*a*x^2-x-1)*(x-a^2)
assert(gcd(F,G) == a^-1 * (a*x^3-2*a*x^2-x-1))
-- this one is not implemented yet: factor F

-- Now a bivariate example
R = A[x,y]
F = (a*x^3-2*a*x^2*y-x-1)^4*(x-a*y-a^2)^2
G = (a*x^3-2*a*x^2*y-x-1)*(x-a^2-y^3)^8
assert(gcd(F,G) == a^-1 * (a*x^3-2*a*x^2*y-x-1))
-- factor F -- not implemented yet 

-- What about one that we construct ourselves?
A = GF(3,4, Variable=>a)
R = A[x]
F = (a*x^3-2*a*x^2-x-1)*(x-a)
G = (a*x^3-2*a*x^2-x-1)*(x-a^2)
assert(gcd(F,G) == a^-1 * (a*x^3-2*a*x^2-x-1)) -- this calls the gcd via syzygies routine
factor F -- this one is not implemented yet
value oo == F

A = toField(ZZ/3[a]/(a^4-a^3-1))
R = A[x]
F = (a*x^3-2*a*x^2-x-1)*(x-a)
G = (a*x^3-2*a*x^2-x-1)*(x-a^2)
assert(gcd(F,G) == a^-1 * (a*x^3-2*a*x^2-x-1)) -- this calls the gcd via syzygies routine
factor F -- this one works!
value oo == F

-- Now let's use this to factor a polynomial over GF(9)
A = toField(ZZ/3[a]/(a^4+a+2))
R = A[x,y]
F = (a*x^3-2*a*x^2*y-x-1)^4*(x-a*y-a^2)^2
G = (a*x^3-2*a*x^2*y-x-1)*(x-a^2-y^3)^8
assert(gcd(F,G) == a^-1 * (a*x^3-2*a*x^2*y-x-1)) -- not implemented yet
factor F -- NOT CORRECT!!

A = GF(3,20, Variable => a)
R = A[x]
F = (a*x^3-2*a*x^2-x-1)*(x-a)
G = (a*x^3-2*a*x^2-x-1)*(x-a^2)
gcd(F,G) -- gives ERROR
factor F -- gives ERROR

-- let's try to use UPolynomials to get this to work.
loadPackage "ConwayPolynomials"
path = prepend("~/src/M2/Macaulay2/packages/development/", path)
loadPackage "UPolynomials"
A = GF(3,20, Variable => a)
A1 = ambient A; toField A1; setUFD A1;
R = A1[x]; setUFD R
F = (a*x^3-2*a*x^2-x-1)*(x-a)
G = (a*x^3-2*a*x^2-x-1)*(x-a^2)
gcd(F,G) -- uses syzygies
time myGCD(F,G) -- this works
assert(myGCD(F,G) == a^-1 * (a*x^3-2*a*x^2-x-1)) -- this calls the gcd via syzygies routine
-- now let's factor a polynomial over A
factor F  -- this works for some reason!
value oo == F
gcd(F,G) == myGCD(F,G) -- this works
-- let's try factoring F using the resultants, computing gcds over ZZ/3:
F
C1 = ZZ/3[x]; setUFD C1;
R1 = C1[a]; setUFD R1
F1 = sub(F,R1)
G1 = sub((ideal A1)_0, R1)
F1 = sub(F1, x => x-a-a^3-a^5-a^9) % G1
use C1; use R1
time myResultant(F1,G1)
factor oo
time apply(oo//toList/toList/first, H -> if first degree H > 0 then myGCD(sub(sub(H, x=>x+a+a^3+a^5+a^9),ring F), F) else null)
Fa = product select(oo, f -> f =!= null)
use coefficientRing ring F
F == a * Fa -- true.  This is currently a very slow way to factor here!!
-- since myGCD works, we can use Cantor-Zassenhaus to factor polynomials over A in one variable.