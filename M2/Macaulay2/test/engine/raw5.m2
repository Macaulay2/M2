-- Continuation of raw test routines for the engine

-----------------------------------------
-- MonomialIdeal's ----------------------
-----------------------------------------
restart
needs "raw-util.m2"

R = polyring(rawZZ(), (symbol a .. symbol g))
M = rawMatrix1(R^1, 4, (a*b,a*c-c^2,b^3*f, c*d*f*g), false)
I = rawMonomialIdeal(M,0)  -- ERROR: need to grab the lead term...
M1 = rawMonomialIdealToMatrix I
J = rawMonomialIdeal(M1,0)  -- ERROR: need to grab the lead term...
assert(J === I)
assert(M1 === rawMonomialIdealToMatrix J)
assert(rawNumgens I === 4)

I = rawMonomialIdeal(rawMatrix1(R^1,4,(a^3*b^2,a*b*c^3,a^2*d^3,a^2*b^2*c^2*d^2),false),0)
rawMonomialIdealToMatrix I
assert(rawNumgens rawRadical I === 2)

I = rawMonomialIdeal(rawMatrix1(R^1,3,(a^2*b,a*b*c,e*d),false),0)
J = rawMonomialIdeal(rawMatrix1(R^1,3,(a,b*c,c*d),false),0)
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
B = rawStronglyStableClosure rawMonomialIdeal(rawMatrix1(R^1, 1, singleton(a*b^2*c),false), 0)
rawMonomialIdealToMatrix B
assert rawIsStronglyStable B

-- +, *, rawIntersect, rawColon, rawSaturate, rawStronglyStableClosure, 
-- rawIsStronglyStable, rawMaximalIndependentSets, rawCodimension

     
     





