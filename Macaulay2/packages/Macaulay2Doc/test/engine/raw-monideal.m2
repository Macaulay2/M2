--status: this old test depends on internal things and probably should be deleted


-- Continuation of raw test routines for the engine

-----------------------------------------
-- MonomialIdeal's ----------------------
-----------------------------------------
--restart
needs "raw-util.m2"

R = polyring(rawZZ(), (symbol a .. symbol g))
M = rawMatrix1(R^1, 4, (a*b,a*c-c^2,b^3*f, c*d*f*g), 0)
I = rawMonomialIdeal(M,0)  -- ERROR: need to grab the lead term...
M1 = rawMonomialIdealToMatrix I
J = rawMonomialIdeal(M1,0)  -- ERROR: need to grab the lead term...
assert(J === I)
assert(M1 === rawMonomialIdealToMatrix J)
assert(rawNumgens I === 4)

I = rawMonomialIdeal(rawMatrix1(R^1,4,(a^3*b^2,a*b*c^3,a^2*d^3,a^2*b^2*c^2*d^2),0),0)
rawMonomialIdealToMatrix I
assert(rawNumgens rawRadical I === 2)

I = rawMonomialIdeal(rawMatrix1(R^1,3,(a^2*b,a*b*c,e*d),0),0)
J = rawMonomialIdeal(rawMatrix1(R^1,3,(a,b*c,c*d),0),0)
m = rawLeadMonomial(7,a*b-b^2)
rawMonomialIdealToMatrix (I + J)
rawMonomialIdealToMatrix (I * J)
rawMonomialIdealToMatrix rawColon(I,J)
rawMonomialIdealToMatrix rawColon(I,m)
rawMonomialIdealToMatrix rawIntersect(I,J)
rawMonomialIdealToMatrix rawSaturate(I,J)
rawMonomialIdealToMatrix rawSaturate(I,m)
rawMonomialIdealToMatrix rawStronglyStableClosure(I)
rawMonomialIdealToMatrix rawMonomialMinimalPrimes(I,8,-1)
rawMonomialIdealToMatrix rawMaximalIndependentSets(I,-1)
rawMonomialIdealToMatrix rawMaximalIndependentSets(I,1)
rawMonomialIdealToMatrix rawMaximalIndependentSets(I,2)
rawMonomialIdealToMatrix rawMaximalIndependentSets(I,3)
rawMonomialIdealToMatrix rawMaximalIndependentSets(I,4)
rawMonomialIdealToMatrix rawMaximalIndependentSets(I,5)

assert(rawCodimension I === 2)
L = rawSaturate(I,m)
rawCodimension L -- is 8??
rawMonomialIdealToMatrix rawMonomialMinimalPrimes (L,8,-1)
B = rawStronglyStableClosure rawMonomialIdeal(rawMatrix1(R^1, 1, 1:(a*b^2*c),0), 0)
rawMonomialIdealToMatrix B
assert rawIsStronglyStable B

-- +, *, rawIntersect, rawColon, rawSaturate, rawStronglyStableClosure, 
-- rawIsStronglyStable, rawMaximalIndependentSets, rawCodimension

R = ZZ[symbol a..symbol f]
I = monomialIdeal(a^3,b^2*c,a*c*d)     
assert(I === monomialIdeal matrix{{a^3,b^2*c, a*c*d}})
assert(I === monomialIdeal matrix{{a^3,b^2*c, a^7, b^2*c, b^2*c^2, a*c*d}})
assert(numgens I == 3)
independentSets I
primaryDecomposition I
saturate(I,a)
I : a

independentSets(ideal I, Limit => 1)
borel I
isBorel I -- returns a function??
poincare I
poincare borel I
res I
intersect(I,monomialIdeal(a^3,b^3,c^3,d^3))
independentSets I

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test/engine raw-monideal.out"
-- End:
