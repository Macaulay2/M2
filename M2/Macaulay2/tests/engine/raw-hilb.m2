--status: this old test depends on internal things and probably should be deleted


--------------------------
-- Hilbert functions -----
--------------------------
needs "raw-util.m2"

R = polyring(rawQQ(), (symbol a .. symbol g))
M = rawMatrix1(R^1, 3, (a^2, a*b, b^2), 0)
f = rawHilbert M
rawFactor f
t = (rawRing f)_0
f1 = f // (1-t)
f1 % (1-t)
f2 = f1 // (1-t)
f2 % (1-t)

R = ZZ[symbol a..symbol g]
M = matrix{{a^2, a*b, b^2}}
poincare coker M
poincare coker M
Mi = monomialIdeal M
assert(poincare Mi == poincare coker M)

needs "raw-util.m2"
R = polyring(rawZZp 32003, (symbol a .. symbol h))
M = rawMatrix1(R^1, 8, (a^2, a*b, b^2*c, c^4, c^3*a*g*h^4, h^7*a, f*g*h, e^12*h^3), 0)
f = rawHilbert M
t = (rawRing f)_0
answer = 1-2*t^2-t^3+2*t^5+3*t^6-2*t^7-t^8-2*t^9+6*t^10-8*t^11+7*t^12-7*t^13+6*t^14-3*t^15+3*t^17-2*t^19+t^20-4*t^21-2*t^22+11*t^23-9*t^24+7*t^25-6*t^26+2*t^27
assert(f === answer)
assert(f % (1-t)^3 == 0)
assert(f % (1-t)^4 != 0)
f // (1-t)^3

R = ZZ/32003[symbol a..symbol h]
M = matrix{{a^2, a*b, b^2*c, c^4, c^3*a*g*h^4, h^7*a, f*g*h, e^12*h^3}}
poincare coker M
hf = hilbertSeries coker M
hf = reduceHilbert hf
hf = numerator hf
substitute(hf, (ring hf)_0 => 1)
assert(hilbertPolynomial coker M === new ProjectiveHilbertPolynomial from {0 => 2352, 1 => -449, 2 => -65, 3 => 38, 4 => 1})
assert(degree coker M === 1)
assert(codim coker M === 3)


R = ZZ/32003[symbol a .. symbol d]
I = ideal(a*d-b*c, b^2-a*c, c^2-b*d)
hf = hilbertSeries I
reduceHilbert hf
T = (ring numerator hf)_0
assert(numerator reduceHilbert hf == 1 + 2*T)
