-- MES
load "raw-util.m2"
-- This file tests most of the raw* routines, in the case when the
-- ring is a singly graded polynomial ring over ZZ.
-- raw* routines not yet tested here, for this ring:

///
-- Ring routines not yet functional
rGF -- can't do GF yet, until quotients are connected.
rC = rawCC(.0000000001, trivmonoid) -- NOT FUNCTIONAL
rbigRR = rawBigRR(trivmonoid) -- NOT FUNCTIONAL
rbigCC = rawBigCC(trivmonoid) -- NOT FUNCTIONAL
rawSolvableAlgebra -- NOT FUNCTIONAL
rawFractionRing -- not quite functional, needs gb's
rawLocalRing -- NOT FUNCTIONAL, needs gb's?
rawColonRing -- NOT FUNCTIONAL, needs gbs.
///

--  
-- Test monoid creation
M = degmonoid 4
M = degmonoid 1
degmonoid 1
degmonoid 10
singlemonoid{x,y,z}
x = symbol x
singlemonoid toList apply(1..10, i -> x_i)

-- Making rings
rZ = rawZZ()
rQ = rawQQ()
rZp = rawZZp(5)
rR = rawRR(.0000000001)

R1 = rawPolynomialRing(rawZZ(), singlemonoid{x,y,z})
R2 = rawPolynomialRing(rawZZp(7), singlemonoid{x,y,z})
R3 = rawPolynomialRing(rawRR(.000000000001), singlemonoid{x,y,z})
R4 = rawPolynomialRing(rawQQ(), singlemonoid{x,y,z,w,a,b})
A =  rawPolynomialRing(rawZZ(), singlemonoid{r,s})
stderr << "warning: flattening not rewritten yet" << endl
-- B =  rawPolynomialRing(A, singlemonoid{x,y,z})
-- C =  rawPolynomialRing(B, singlemonoid{X,Y})

R5 = rawSkewPolynomialRing(R1,{0,1}) -- BUG: skew comm not displayed

WP = rawPolynomialRing(rawQQ(), singlemonoid{x,y,Dx,Dy})
W = rawWeylAlgebra(WP, {0,1}, {2,3}, -1)
WP2 = rawPolynomialRing(rawQQ(), singlemonoid{x,y,Dx,Dy,h})
W2 = rawWeylAlgebra(WP2, {0,1}, {2,3}, 4)


rawSchurRing R1

-----------------
-- RingElement --
-----------------
-- Start with a simple ring
R1 = polyring(rawZZ(), {symbol x, symbol y, symbol z})

rawFromNumber(R1, 3)
assert(rawFromNumber(R1, 0) == 0)
assert(size rawFromNumber(R1,0) === 0)
assert(size rawFromNumber(R1,1) === 1)
rawFromNumber(R1,1.4) -- this is 1.  Is this a feature or a bug?
-- rawFromNumber(R1,1+ii) -- gives an error now, good.
f = rawFromNumber(R1,3)
assert (try (rawToInteger f) else true)
time scan(1..10000, i -> if i =!= 0 then assert(i == rawToInteger rawLeadCoefficient rawFromNumber(R1,i)))
time scan(1..10000, i -> assert(i == rawToInteger rawLeadCoefficient rawFromNumber(R1,i)))
time (
     i = 1;
     while i < 10000 do (
       assert(i == rawToInteger rawLeadCoefficient rawFromNumber(R1,i));
       i = i+1;
       ))

x100 = rawRingVar(R1,0,100)
x1000 = rawRingVar(R1,0,1000)
assert(x1000 * x1000 == rawRingVar(R1,0,2000))
assert(x^100 == x100)
assert(x100 === x^100)
-x
x+y
assert(x+y+z-x-y-z == 0)
assert(size(x+y+z-x-y-z) == 0)
assert(size(x+y+z-x-y-z^2) == 2)
f = (x^2+x*y+y^2)
g = ((3*x^10+x*y-3)*f)//(3*x^10+x*y-3)
assert(f == g)
assert(3*x === x+x+x)
(x^5+y^5) % (x-5)
(x^5+y^5) % (x-y-5)
rawDivMod(x^5+y^5+z^5, x)
rawDivMod((x+y-2)*(x^4*y^4*z^4-x*y-x*z-3), x+y-2)
rawRing f === R1
assert not rawIsHomogeneous(x+y-1)
assert rawIsHomogeneous(x+y-z)
rawMultiDegree(x*y-3)
rawDegree(f, {1,0,0})
rawDegree(f, {1,1,1})
rawTermCount f
pf = x^4-y*z-3*x-6
assert(z^2 * rawHomogenize(f, 2, 4, {1,1,1}) 
     === rawHomogenize(f, 2, 6, {1,1,1}))
rawHomogenize(f,2,{1,7,1})
rawMakeMonomial{(0,2),(1,3)}
assert try (rawMakeMonomial{(1,2),(0,3)};false) else true
assert(rawTerm(R1,rawFromNumber(rawZZ(),-23),rawMakeMonomial{(0,2),(1,3)})
 === -23*x^2*y^3)
f = (x+y^2+z^3)^9
rawTermCount f
rawGetTerms(f,2,4)
rawGetTerms(f,-2,-1)
-- rawCoefficients(f, rawMakeMonomial{(2,21),(0,2)}) -- expects 3 arguments
<< "rawCoefficients(f, rawMakeMonomial{(2,21),(0,2)}) -- expects 3 arguments" << endl;
assert(1231212 == rawLeadCoefficient(1231212*f))
rawLeadMonomial(y*z*f) -- strange: comes out with indets a,b,c.
(cs,ms) = rawPairs(f)
cs
ms
rawPairs((x+y)^3)
assert(f === sum apply(#cs, i -> rawTerm(R1,cs#i, ms#i)))

-- rawNumerator   -- test once fraction rings are defined
-- rawDenominator
-- rawFraction

--------------------------
-- RingElement, other rings
--------------------------
-- RawRingElement interface
-- arithmetic via operators: - (unary and binary), +, *, //, %, ^, ===
--  also: rawInvert (?)
-- creation from a ring: rawRingVar, rawFromNumber, rawTerm
-- general
--  rawRing, rawToString, rawHash(?), rawIsZero, rawDivMod
-- polynomial type routines: 
--  rawIsHomogeneous
--  rawDegree(f, wts)
--  rawMultiDegree
--  rawTermCount
--  rawGetTerms
--  rawHomogenize
--  rawLeadMonomial
--  rawLeadCoefficient
--  rawPairs
--  rawCoefficient
-- moving between rings
--  rawPromote
--  rawLift
--  rawToInteger
-- fractions
--  rawNumerator
--  rawDenominator
--  rawFromNumber
-- missing from this interface:
--  differentiation, divide by variable, 
--  get lead coefficient, monomial, in a given variable (?).
--  gcd's
--  random elements
--  evaluation, interpolation
--  what else?
-- Switching to numeric types is messy
-- For evaluation, and so on, see RawRingMap

A = polyring(rawZZ(), {symbol a, symbol b, symbol c})
stderr << "warning: flattening not rewritten yet" << endl
-- B = polyring(A, {symbol x, symbol y, symbol z})
-- a = rawPromote(B,a)
-- ring a === B
-- b = rawPromote(B,b)
-- c = rawPromote(B,c)
-- f = (a+b+1)*(x^2-y*z^5-1) -- display is a bit off
-- rawFromNumber(B,3462346246246246263287642) * c
-- assert(rawTermCount f === 3)
-- assert(rawGetTerms(f,1,1) === (a+b+1)*x^2)
-- assert((a+b+x+y)^2 === (a+b+x+y)*(a+b+x+y))
-- assert (rawDegree(f, {1,0,0}) == (0,2))
-- assert (rawDegree(f, {0,0,1}) == (0,5))
-- 
-- assert try rawDegree(f, {1,0,0,3,4,5}) else true 
-- assert (rawMultiDegree f == {6})
-- 
-- assert not rawIsHomogeneous f
-- assert rawIsHomogeneous (a^100*x^2-b*x*z-z^2)
-- rawHomogenize(a*x-y^3-1, 2, {1,1,1})

--------------------------
-- free module routines --
--------------------------
-- rawRing, rawRank, rawMultiDegree
-- rawFreeModule(R,5), rawFreeModule(R,(0,1,2,3))
-- rawDirectSum, rawTensor
-- rawDual, rawSymmetricPower, rawExteriorPower, rawSubmodule
-- toString
-- TO BE TESTED: rawFreeModule mat, rawGetSchreyer
-- TO BE WRITTEN: hash 
R = polyring(rawZZ(), (symbol a .. symbol f))
F = rawFreeModule(R,5)
assert(rawRank F === 5)
assert(rawMultiDegree F === {0,0,0,0,0})
assert(rawRing F === R)
G = rawFreeModule(R, (1,2,3,4))
assert(rawMultiDegree G === {1,2,3,4})
H = rawFreeModule(R,0)
assert(rawRank H === 0)
assert(rawMultiDegree H === {})
assert try (rawFreeModule(R,-1);false) else true
rawRank H
G = rawFreeModule(R,(0,0,1,1,-3))
assert(toString G === "free(rank 5 degrees = {1, 1, t, t, t^(-3)})")

F = rawFreeModule(R,(0,10,100,1000))
G = rawFreeModule(R,(1,2,3))
H = rawTensor(F,G)
assert(F ** G === H)
assert(rawRank H === rawRank F * rawRank G)
assert(rawMultiDegree H === 
     {1, 2, 3, 11, 12, 13, 101, 102, 103, 1001, 1002, 1003})
assert(rawMultiDegree rawDual F === - rawMultiDegree F)
assert(rawMultiDegree rawDual H === - rawMultiDegree H)
assert(rawMultiDegree rawExteriorPower(3,F) === {110,1010,1100,1110})
assert(rawSubmodule(F, (0,0,1,1,2,2)) === rawFreeModule(R,(0,0,10,10,100,100)))
S2F = rawSymmetricPower(2,F)
assert(rawRank S2F === 10)
assert(toString S2F === 
     "free(rank 10 degrees = {1, t10, t100, t1000, t20, t110, t1010, t200, t1100, t2000})")

assert(rawMultiDegree rawExteriorPower(3,F) === {110,1010,1100,1110})
assert(rawSubmodule(F, (0,0,1,1,2,2)) === rawFreeModule(R,(0,0,10,10,100,100)))
assert(rawRank rawSubmodule(F, (0,1)) === 2)
assert(rawRank rawSubmodule(F, ()) === 0)

assert(rawRank rawSymmetricPower(-1,F) === 0)
assert(rawSymmetricPower(0,F) === R^1)
assert(rawSymmetricPower(1,F) === F)
assert(rawRank rawExteriorPower(-1,F) == 0)
assert(rawExteriorPower(0,F) == R^1)
assert(rawExteriorPower(1,F) == F)

assert(R^4 == rawFreeModule(R,4))
assert(R^4 === rawFreeModule(R,4))
R^{-1,-1,2} == rawFreeModule(R,(1,1,-2))

---------------------
-- Matrix routines --
---------------------
-- To test
-- rawTarget, rawSource, rawRing, rawMultiDegree, rawNumberOfRows, rawNumberOfColumns
-- rawMatrixEntry
-- rawMatrix1, rawMatrix2, rawSparseMatrix1, rawSparseMatrix2, rawMatrixRemake1, rawMatrixRemake2
-- rawIsZero, rawIsEqual, ===
-- rawIsHomogeneous, +, -, negate(-), * (mult, scalar ult)
-- rawConcat, rawDirectSum, rawTensor, rawDual, rawReshape, rawFlip
-- rawSubmatrix, rawIdentity, rawZero
-- rawKoszul, [rawKoszulMonoms]
-- rawSymmetricPower, rawExteriorPower, rawSortColumns
-- rawMinors, rawPfaffians
-- rawMatrixDiff, rawMatrixContract, rawHomogenize
-- rawCoefficients, rawMonomials, rawInitial
-- rawEliminateVariables
-- rawKeepVariables, rawDivideByVariable
-- rawHilbert
--
-- [mutable matrix routines]
-- rawMatrixRowSwap, rawMatrixColSwap
-- rawMatrixRowChange, rawMatrixColChange
-- rawMatrixRowScale, rawMatrixColumnScale
R1 = rawPolynomialRing(rawZZ(), singlemonoid{x,y,z})
x = rawRingVar(R1,0,1)
y = rawRingVar(R1,1,1)
z = rawRingVar(R1,2,1)
F = rawFreeModule(R1,5)

R2 = rawPolynomialRing(rawZZ(), singlemonoid toList (vars 0 .. vars 15))
a = rawRingVar(R2,0,1)
b = rawRingVar(R2,1,1)
F = rawFreeModule(R2,4)
elems = toList apply(0..3, j -> toList apply(0..3, i -> rawRingVar(R2,i+j,1)))
m = rawMatrix1(F,4,toSequence flatten elems,false)



  --------------------------
  -- creation of matrices --
  --------------------------
R = polyring(rawZZ(), (vars 0 .. vars 15))
F = rawFreeModule(R,5)
G = rawFreeModule(R,10)
m = rawSparseMatrix1(F,15,{1,3,4},{3,2,1},(a^2,b^2+a*c,b-2*a*c),false)
m1 = rawSparseMatrix1(F,15,{1,3,4},{3,2,1},(a^2,b^2+a*c,b-2*a*c),true)
<< "make sure mutable matrices and immutable matrices are not ==" << endl;
--assert(m != m1)
rawTarget m == F
rawSource m == G
rawMultiDegree m === {0}

assert rawIsZero(m - m)
assert rawIsZero(m - m1)

m = rawSparseMatrix2(F,G,{7},{1,3,4},{3,2,1},(a^2,b^2+a*c,b-2*a*c),true)
assert(rawMultiDegree m  === {7})
m1 = rawMatrixRemake2(F,G,rawMultiDegree m, m, true)
m2 = rawMatrixRemake2(F,G,{13}, m, false)
assert(rawMultiDegree m2 === {13})

elems = splice apply(0..3, j -> apply(0..3, i -> rawRingVar(R,i+j,1)))
m = rawMatrix1(F,5,elems,false)
p1 = rawMatrix1(F,5,toSequence flatten entries m,false)
p2 = rawMatrix2(F,F,{0},toSequence flatten entries m,false)
p1 == p2

2*m
a*m

m = rawMatrix1(R^4,4,(a,b,c,d, b,e,f,g, c,f,h,i, d,g,i,j),false)
rawDual m
rawTarget m
rawSource m
rawMultiDegree m
rawMatrixEntry(m,1,1)

m2 = m*m

rawSubmatrix(m2,singleton 1)
rawSubmatrix(m2,singleton 2)
assert(m2*m2 == m*m*m*m)
m2-m
assert(rawIsHomogeneous(m) == true)
assert(rawIsHomogeneous(m2-m) == false)
(a+b^2+a*b)*m

rawConcat(m,m,m)
rawConcat(m,m2)

rawDirectSum(m,m2)
rawTensor(m,m)
assert(m === rawDual m)

-- test rawReshape
-- rawFlip

rawSubmatrix(m,(1,2))
rawSubmatrix(m,(0,1),(1,2))
a*rawIdentity(F)
rawZero(F,F)
-- rawKoszul
-- is IM2_Matrix_koszul_monoms connected?
m1 = rawSubmatrix(m,singleton 0,(0,1,2,3))
rawSymmetricPower(3,m1)
rawExteriorPower(2,m,0)
rawExteriorPower(2,m,1)
rawExteriorPower(3,m,0)
rawSortColumns(m,1,1)
rawMinors(2,m,0)

m = rawMatrix1(R^4,4,(0_R,b,c,d, -b,0_R,f,g, -c,-f,0_R,i, -d,-g,-i,0_R),false)
rawPfaffians(4,m)

-- TODO: 
--  compress
--  uniquify
--  remove_content

-- test:
-- rawMatrixDiff
-- rawMatrixContract
-- rawHomogenize
-- coeffs
-- rawCoefficients
-- rawMonomials
-- rawInitial
-- rawEliminateVariables
-- rawKeepVariables
-- rawDivideByVariable

-- TODO:
--  min_leadterms
--  auto_reduce
--  reduce
--  reduce_by_ideal
--  module_tensor
--  kbasis
--  kbasis_all
--  truncate
--  dimension (what is this supposed to do)

-- test:
--  rawHilbert



-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test/engine raw3.okay "
-- End:
