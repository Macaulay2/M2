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
rawQuotientRing -- NOT FUNCTIONAL, needs gbs.
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
rZp = rawZZp(5,trivmonoid)
rR = rawRR(.0000000001, trivmonoid)

R1 = rawPolynomialRing(rawZZ(), singlemonoid{x,y,z})
R2 = rawPolynomialRing(rawZZp(7,trivmonoid), singlemonoid{x,y,z})
R3 = rawPolynomialRing(rawRR(.000000000001,trivmonoid), singlemonoid{x,y,z})
R4 = rawPolynomialRing(rawQQ(), singlemonoid{x,y,z,w,a,b})
A =  rawPolynomialRing(rawZZ(), singlemonoid{r,s})
B =  rawPolynomialRing(A, singlemonoid{x,y,z})
C =  rawPolynomialRing(B, singlemonoid{X,Y})

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
rawMonomialMake{(1,3),(0,2)}
assert try (rawMonomialMake{(0,3),(1,2)};false) else true
assert(rawTerm(R1,rawFromNumber(rawZZ(),-23),rawMonomialMake{(1,3),(0,2)})
 === -23*x^2*y^3)
f = (x+y^2+z^3)^9
rawTermCount f
rawGetTerms(f,2,4)
rawGetTerms(f,-2,-1)
-- rawCoefficients(f, rawMonomialMake{(2,21),(0,2)}) -- expects 3 arguments
<< "rawCoefficients(f, rawMonomialMake{(2,21),(0,2)}) -- expects 3 arguments" << endl;
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
B = polyring(A, {symbol x, symbol y, symbol z})
a = rawPromote(B,a)
ring a === B
b = rawPromote(B,b)
c = rawPromote(B,c)
f = (a+b+1)*(x^2-y*z^5-1) -- display is a bit off
rawFromNumber(B,3462346246246246263287642) * c
assert(rawTermCount f === 3)
assert(rawGetTerms(f,1,1) === (a+b+1)*x^2)
assert((a+b+x+y)^2 === (a+b+x+y)*(a+b+x+y))
assert (rawDegree(f, {1,0,0}) == (0,2))
assert (rawDegree(f, {0,0,1}) == (0,5))

assert try rawDegree(f, {1,0,0,3,4,5}) else true 
assert (rawMultiDegree f == {6})

assert not rawIsHomogeneous f
assert rawIsHomogeneous (a^100*x^2-b*x*z-z^2)
rawHomogenize(a*x-y^3-1, 2, {1,1,1})

--------------------------
-- free module routines --
--------------------------
R1 = rawPolynomialRing(rawZZ(), singlemonoid{x,y,z})
x = rawRingVar(R1,0,1)
y = rawRingVar(R1,1,1)
z = rawRingVar(R1,2,1)
F = rawFreeModule(R1,5)
G = rawFreeModule(R1,(0,0,1,1,-3))
toString G
assert(rank F === 5)
assert(rawRing F === R1)

-- test Schreyer order later, after matrices have been tested
-- rawFreeModule(rawMatrix)
rawGetSchreyer G

assert(rawMultiDegree G === {0,0,1,1,-3})
rawMultiDegree F

H = rawDirectSum(F,G)
H2 = rawTensor(F,G)
rawMultiDegree H2

F = rawFreeModule(R1,(0,10,100,1000))
G = rawFreeModule(R1,(1,2,3))
H = rawTensor(F,G)
assert(rawRank H === rawRank F * rawRank G)
assert(rawMultiDegree H === 
     {1, 2, 3, 11, 12, 13, 101, 102, 103, 1001, 1002, 1003})

assert(rawMultiDegree rawDual F === - rawMultiDegree F)
assert(rawMultiDegree rawDual H === - rawMultiDegree H)

rawSymmetricPower(5,F)
rawExteriorPower(2,F)
assert(rawMultiDegree rawExteriorPower(3,F) === {110,1010,1100,1110})
assert(rawSubmodule(F, (0,0,1,1,2,2)) === rawFreeModule(R1,(0,0,10,10,100,100)))

---------------------
-- Vector routines --
---------------------
R1 = rawPolynomialRing(rawZZ(), singlemonoid{x,y,z})
x = rawRingVar(R1,0,1)
y = rawRingVar(R1,1,1)
z = rawRingVar(R1,2,1)
F = rawFreeModule(R1,5)
v = rawVector(F,(x,x,y,y,z))
assert(rawZero(F) == v-v)
assert(rawIsZero(v-v))
assert(not rawIsZero(v))
assert((-v) + v == rawZero(F))
assert(rawModuleEntry(F,0)*x
 + rawModuleEntry(F,1)*x + rawModuleEntry(F,2)*y +
  rawModuleEntry(F,3)*y + rawModuleEntry(F,4)*z === v)

-- r*v missing BUG: IMPLEMENT!!
-- rawDegree BUG: IMPLEMENT!!
-- lead monomial BUG: IMPLEMENT!!

rawTerm(F,(x+y)^3,3) === rawModuleEntry(F,3) * ((x+y)^3)
G = rawFreeModule(R1,(0,1,2))
v = rawTerm(G,x^2,2) + rawTerm(G,y+z,1) + rawTerm(G,rawFromNumber(R1,3),0)
v = rawTerm(G,x^2,0) + rawTerm(G,y+z,1) + rawTerm(G,rawFromNumber(R1,3),2)
assert rawIsHomogeneous v
assert(rawMultiDegree v === {2})
assert(v === rawHomogenize(v,2,2,{1,1,1}))
rawHomogenize(v,2,3,{1,0,1})
rawHomogenize(v,2,{1,0,1})
rawGetTerms(v,0,0)
rawGetTerms(v,-1,-1)
rawGetTerms(v,0,-1) === v

v
assert(rawVectorEntry(v,0) === x^2)
assert(rawVectorEntry(v,1) === y+z)
assert(rawVectorEntries(v) === (x^2, y+z, rawFromNumber(R1,3)))
assert(rawFreeModule v === G)
assert(rawTermCount v === 3)
assert(rawLeadComponent v === 2)
rawLeadCoefficient v -- NOTE: this maybe should be rawLeadVectorEntry(v).
                     -- or should just not be present in the interface.

---------------------
-- Matrix routines --
---------------------
R1 = rawPolynomialRing(rawZZ(), singlemonoid{x,y,z})
x = rawRingVar(R1,0,1)
y = rawRingVar(R1,1,1)
z = rawRingVar(R1,2,1)
F = rawFreeModule(R1,5)
v = rawTerm(F,x+y,1) + rawTerm(F,z^3-2,2)
w = rawTerm(F,x+y^4,0) + rawTerm(F,z^3-2*x*y,1)
m = rawMatrix(F, (v,w))
assert rawIsZero(m - m)
vars 0
vars 15

R2 = rawPolynomialRing(rawZZ(), singlemonoid toList (vars 0 .. vars 15))
a = rawRingVar(R2,0,1)
b = rawRingVar(R2,1,1)
F = rawFreeModule(R2,4)
m = rawMatrix(F,apply(0..3, j -> sum apply(4, i -> rawTerm(F,rawRingVar(R2,i+j,1),i))))
2*m
a*m

rawDual m
rawTarget m
rawSource m
rawMultiDegree m
rawMatrixEntry(m,1,1)
m2 = m*m
rawMatrixColumn(m2,1)
rawMatrixColumn(m2,2)
assert(m2*m2 == m*m*m*m)
m2-m
rawIsHomogeneous(m)
(a+b^2+a*b)*m
v = rawTerm(F,a,2)
m * v
m
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
m1 = rawSubmatrix(m,0,(0,1,2,3))
rawSymmetricPower(3,m1)
rawExteriorPower(2,m,0)
rawExteriorPower(2,m,1)
rawExteriorPower(3,m,0)
rawSortColumns(m,1,1)
rawMinors(2,m,0)
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


