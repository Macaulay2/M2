-- Continuation of raw test routines for the engine

-----------------------------------------
-- MonomialIdeal's ----------------------
-----------------------------------------
--restart
needs "raw-util.m2"

R = polyring(rawZZ(), (symbol a .. symbol g))
M = rawMatrix1(R^1, 4, (a*b,a*c-c^2,b^3*f, c*d*f*g), false, 0)
I = rawMonomialIdeal(M,0)  -- ERROR: need to grab the lead term...
M1 = rawMonomialIdealToMatrix I
J = rawMonomialIdeal(M1,0)  -- ERROR: need to grab the lead term...
assert(J === I)
assert(M1 === rawMonomialIdealToMatrix J)
assert(rawNumgens I === 4)

I = rawMonomialIdeal(rawMatrix1(R^1,4,(a^3*b^2,a*b*c^3,a^2*d^3,a^2*b^2*c^2*d^2),false,0),0)
rawMonomialIdealToMatrix I
assert(rawNumgens rawRadical I === 2)

I = rawMonomialIdeal(rawMatrix1(R^1,3,(a^2*b,a*b*c,e*d),false,0),0)
J = rawMonomialIdeal(rawMatrix1(R^1,3,(a,b*c,c*d),false,0),0)
m = rawLeadMonomial(a*b-b^2)
rawMonomialIdealToMatrix (I + J)
rawMonomialIdealToMatrix (I * J)
rawMonomialIdealToMatrix rawColon(I,J)
rawMonomialIdealToMatrix rawColon(I,m)
rawMonomialIdealToMatrix rawIntersect(I,J)
rawMonomialIdealToMatrix rawSaturate(I,J)
rawMonomialIdealToMatrix rawSaturate(I,m)
rawMonomialIdealToMatrix rawStronglyStableClosure(I)
rawMonomialIdealToMatrix rawAssociatedPrimes I
assert(rawCodimension I === 2)
L = rawSaturate(I,m)
rawCodimension L -- is 8??
rawMonomialIdealToMatrix rawAssociatedPrimes L
B = rawStronglyStableClosure rawMonomialIdeal(rawMatrix1(R^1, 1, 1:(a*b^2*c),false,0), 0)
rawMonomialIdealToMatrix B
assert rawIsStronglyStable B

-- +, *, rawIntersect, rawColon, rawSaturate, rawStronglyStableClosure, 
-- rawIsStronglyStable, rawMaximalIndependentSets, rawCodimension

R = ZZ[symbol a..symbol f]
I = monomialIdeal(a^3,b^2*c,a*c*d)     
assert(I === monomialIdeal matrix{{a^3,b^2*c, a*c*d}})
assert(I === monomialIdeal matrix{{a^3,b^2*c, a^7, b^2*c, b^2*c^2, a*c*d}})
assert(numgens I == 3)
minprimes I
primaryDecomposition I  -- FAILS: attempted to modify immutable hash table
saturate(I,a) -- an ideal expects I.generators to be there, but it is not!
I : a -- also fails, same problem.

borel I
isBorel I -- returns a function??
poincare I
poincare borel I
res I
intersect(I,monomialIdeal(a^3,b^3,c^3,d^3))
topPrimes I  -- should this be the name, instead of minprimes??

--------------------------
-- Hilbert functions -----
--------------------------
needs "raw-util.m2"

R = polyring(rawQQ(), (symbol a .. symbol g))
M = rawMatrix1(R^1, 3, (a^2, a*b, b^2), true,0)
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
M = rawMatrix1(R^1, 8, (a^2, a*b, b^2*c, c^4, c^3*a*g*h^4, h^7*a, f*g*h, e^12*h^3), true,0)
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
hilbertSeries coker M
assert(hilbertPolynomial coker M === new ProjectiveHilbertPolynomial from {0 => 2352, 1 => -449, 2 => -65, 3 => 38, 4 => 1})
assert(degree coker M === 1)
assert(codim coker M === 3)
