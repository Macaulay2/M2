-- MES
needs "raw-util.m2"
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

-- tests rawTerm, since that is called from promote
M = monoid [a..d]
f = M_0
g = M_0 * M_3^4
R = ZZ M
assert(promote(f,R) == a)
assert(promote(g,R) == a*d^4)

-----------------
-- RingElement --
-----------------
-- Start with a simple ring
needs "raw-util.m2"
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

assert(2 == rawLeadCoefficient(2*x))

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
assert((x^5+y^5) % (x-5) === y^5+5^5)
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
rawCoefficient(f, rawMakeMonomial{(0,2),(2,21)})
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
B = polyring(A, {symbol x, symbol y, symbol z})
a = rawPromote(B,a)
ring a === B
b = rawPromote(B,b)
c = rawPromote(B,c)
f = (a+b+1)*(x^2-y*z^5-1) -- display is a bit off
f^2
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

--------------------------------------
-- poly rings over other poly rings --
--------------------------------------
needs "raw-util.m2"
A = polyring(rawZZ(), 1 : symbol x)
B = polyring(A, 1 : symbol y)
A === ring x
x1 = rawPromote(B,x)
f = (x1+y)^3
rawLeadCoefficient(x1+y)
g = x1*y^3+(1+x1)*y

assert(2 == rawTermCount g)
rawLeadCoefficient g
rawLeadMonomial g 
rawGetTerms(g,0,0) -- the lead term
rawGetTerms(g,1,1)
rawGetTerms(g,2,2)
rawPairs g
rawLeadMonomial y
rawCoefficient(g, rawLeadMonomial(y))
rawCoefficient(g, rawMakeMonomial{(0,1)})
h1 = rawTerm(B,(x+1)^3,rawMakeMonomial{(0,3)})
h2 = rawPromote(B, (x+1)^3) * y^3 
assert(h1 == h2)



needs "raw-util.m2"
A = polyring(rawZZ(), (symbol r, symbol s))
B = rawPolynomialRing(A, singlemonoid(symbol x, symbol y, symbol z))
C = rawPolynomialRing(B, singlemonoid(symbol X, symbol Y, symbol Z))
X = C_0
Y = C_1
ring X === C
x = rawPromote(C,B_0)
ring x === C
((1_C+x)*X+Y)^3
r = rawPromote(C, rawPromote(B,A_0))
s = rawPromote(C, rawPromote(B,A_1))
(r+s+x+X)^3

---------------------------------
-- fraction fields --------------
---------------------------------
needs "raw-util.m2"
A = polyring(rawZZ(), {symbol x, symbol y, symbol z})
B = rawFractionRing A
f = rawFraction(B,x,y)
f1 = rawFraction(B,y,x)
f*f1
assert((f-f)*f1 == 0)
assert(f * f1 == 1)
assert(1//f == f1)
assert(rawPromote(B,x) == rawFraction(B,x,1_A))

C = polyring(B, {symbol s, symbol t})
x1 = rawPromote(C,rawPromote(B,x))
y1 = rawPromote(C,rawPromote(B,y))
z1 = rawPromote(C,rawPromote(B,z))
s
f = s//(x1*y1)
g = f^2 + s^2
assert(g - s^2 == f^2)



g = rawFraction(B,x^2+y*z,y)
h = rawFraction(B, 5*(3*x^2-x-y+1_A)^2*(y+z), 10*(3*x^2-x-y+1_A)^2*(x+y^2-1_A))
h + 1_B//h
---------------------------------
-- quotient rings ---------------
---------------------------------
needs "raw-util.m2"
A = polyring(rawZZp(101), {symbol x, symbol y, symbol z})
M = mat{{x^2-y*z-1_A}}
B = rawQuotientRing(A,M)
x = rawRingVar(B,0,1)
y = rawRingVar(B,1,1)
z = rawRingVar(B,2,1)
assert(x*x == y*z+1)
assert(x^2 == y*z+1)
assert(x^4 == y^2*z^2+2*y*z+1)

needs "raw-util.m2"
A = polyring(rawZZ(), {symbol x, symbol y, symbol z})
M = mat{{3*x^2-y*z-1_A}}
B = rawQuotientRing(A,M)
x = rawRingVar(B,0,1)
y = rawRingVar(B,1,1)
z = rawRingVar(B,2,1)
x*x
5*x*x
5*x^2
3*x^2
assert(x^2 + x^2 == -x^2 + y*z + 1)
assert(x^2 + x^2 + x^2 == y*z+1)
assert(2*x^2 == x^2 + x^2)

needs "raw-util.m2"
A = polyring(rawZZ(), {symbol x, symbol y})
M = mat{{24336_A, 2*y, 4*x-7800, y^2+2*x-3900, x^2+2*x-3900}}
B = rawQuotientRing(A,M)
x = rawRingVar(B,0,1)
y = rawRingVar(B,1,1)
f = 4*rawRingVar(A,0,1)
rawPromote(B, f)
-2*x
-3*x
-4*x
4*x
rawRing x
x*x
assert(4*x^2 == 0)
assert(x^4 == 0)
assert(2*x^3 == 0)
assert(312*x == 0)

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
m = rawMatrix1(F,4,toSequence flatten elems,false,0)



  --------------------------
  -- creation of matrices --
  --------------------------
R = polyring(rawZZ(), (vars 0 .. vars 15))
F = rawFreeModule(R,5)
G = rawFreeModule(R,10)
m = rawSparseMatrix1(F,15,{1,3,4},{3,2,1},(a^2,b^2+a*c,b-2*a*c),false,0)
m1 = rawSparseMatrix1(F,15,{1,3,4},{3,2,1},(a^2,b^2+a*c,b-2*a*c),true,0)
assert(-(-m) == m)
assert((m-m) == m + (-m)) -- CURRENTLY FAILS BECAUSE OF HASH CODES
--<< "make sure mutable matrices and immutable matrices are not ==" << endl;
assert(not(m == m1))
rawTarget m == F
rawSource m == G
rawMultiDegree m === {0}

assert rawIsZero(m - m)
assert rawIsZero(m - m1)

m = rawSparseMatrix2(F,G,{7},{1,3,4},{3,2,1},(a^2,b^2+a*c,b-2*a*c),true,0)
assert(rawMultiDegree m  === {7})
m1 = rawMatrixRemake2(F,G,rawMultiDegree m, m, true, 0)
m2 = rawMatrixRemake2(F,G,{13}, m, false, 0)
assert(rawMultiDegree m2 === {13})

elems = splice apply(0..3, j -> apply(0..3, i -> rawRingVar(R,i+j,1)))
m = rawMatrix1(F,5,elems,false,0)
p1 = rawMatrix1(F,5,toSequence flatten entries m,false,0)
p2 = rawMatrix2(F,F,{0},toSequence flatten entries m,false,0)
p1 == p2

2*m
a*m

m = rawMatrix1(R^4,4,(a,b,c,d, b,e,f,g, c,f,h,i, d,g,i,j),false,0)
rawDual m
rawTarget m
rawSource m
rawMultiDegree m
rawMatrixEntry(m,1,1)

m = rawMatrix1(R^4,4,(a,b,c,d, b^2+c*d,e^2,f^2,g^2, c^3,f^3,h^3,0_R, d^4,g^4,i^4,j^4),false,0)
assert(m == rawDual rawDual m)
m2 = m*m

rawSubmatrix(m2,1: 1)
rawSubmatrix(m2,1: 2)
assert(m2*m2 == m*m*m*m)
m2-m
assert(rawIsHomogeneous(m) == false)
assert(rawIsHomogeneous(m2-m) == false)
(a+b^2+a*b)*m

rawConcat(m,m,m)
rawConcat(m,m2)

rawDirectSum(m,m2)
mm = rawTensor(m,m)
mm1 = rawSubmatrix(mm,(0,1,2,3),(0,2,4,6))
mm2 = mat{{a^2,a*c,a*b,b*c},
           {a*b,a*f,b^2,b*f},
	   {a*c,a*h,b*c,b*h},
	   {a*d,a*i,b*d,b*i}}
--assert(mm1 == mm2) -- WHAT????
-- assert(m - rawDual m == 0)

-- test rawReshape
-- rawFlip

rawSubmatrix(m,(1,2))
rawSubmatrix(m,(0,1),(1,2))
a*rawIdentity(F,false,0)
rawZero(F,F,false,0)
-- rawKoszul
-- is IM2_Matrix_koszul_monoms connected?
m1 = rawSubmatrix(m,1: 0,(0,1,2,3))
rawSymmetricPower(3,m1)
rawExteriorPower(2,m,0)
rawExteriorPower(2,m,1)
rawExteriorPower(3,m,0)
rawSortColumns(m,1,1)
rawMinors(2,m,0)

m = rawMatrix1(R^4,4,(0_R,b,c,d, -b,0_R,f,g, -c,-f,0_R,i, -d,-g,-i,0_R),false,0)
--<< "equality checking of matrices is screwed up, since hash values are not being set correctly" << endl;
assert(rawPfaffians(4,m) == rawMatrix1(R^1,1,1: (d*f-c*g+b*i), false,0))

R2 = polyring(rawZZ(), (symbol x, symbol y, symbol z))
m = mat {{x+y+z, x*y+y*z+z*x, x*y*z-1, (x+y+z)^5+x*(x*y*z-1) + 13}}
ch = mat{{-x^4-4*x^3*y-4*x^3*z-6*x^2*y^2-12*x^2*y*z-6*x^2*z^2
	    -4*x*y^3-12*x*y^2*z-12*x*y*z^2-x*y*z-4*x*z^3
	    -y^4-4*y^3*z-6*y^2*z^2-4*y*z^3-z^4+1},
         {0_R2},
	 {y+z},
	 {1_R2}}
m * ch

-- matrix operations
R = polyring(rawZZ(), (symbol a .. symbol f))
m = mat{{a^2-1,a*b-b-c^3, a*b*c-d^100}}
mh = rawHomogenize(m,4,(2,1,1,1,1,1))
assert(mh == mat{{-e^4+a^2, -c^3-b*e^2+a*b, -d^100+a*b*c*e^96}})

---------------------
-- random matrices --
---------------------
needs "raw-util.m2"
mr = rawMatrixRandom(rawZZ(),2,3,.5,0,false,0)
mr = rawMatrixRandom(rawZZ(),10,15,.5,0,false,0)
R = polyring(rawZZp(32003), (symbol x, symbol y))
mr = rawMatrixRandom(R,10,15,.5,0,false,0)
mr = rawMatrixRandom(R,10,15,.5,1,false,0)
mr = rawMatrixRandom(R,10,10,.5,1,false,0)

-------------------------------
-- row and column operations --
-------------------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol a .. symbol f))
m = rawMatrix1(R^3,3,(3_R,2_R,17_R,1_R,-5_R,13_R,0_R,2_R,1_R),true,0)
assert(3_R === rawMatrixEntry(m,0,0))
rawMatrixEntry(m,1,2,15_R)
assert(15_R === rawMatrixEntry(m,1,2))
m
-- now let's do row and column operations on this
m1 = rawConcat(m,rawIdentity(R^3,false,0))
m = rawMatrixRemake1(R^3,m1,true,0)
rawMatrixRowScale(m,3_R,1,true)  -- OK now
m
rawMatrixRowChange(m,1,-1_R,0,true) -- OK now
m
rawMatrixRowChange(m,1,9_R,2,true) -- OK now
m
rawMatrixRowChange(m,2,-1_R,1,true) -- OK now
rawMatrixRowChange(m,2,-1_R,1,true) -- OK now
m
rawMatrixRowSwap(m,1,2)
rawMatrixRowSwap(m,1,2)
m



rawMatrixRowScale(m,2_R,0,true)  -- OK now
m
rawMatrixRowChange(m,0,-17_R,2,true) -- still messed up?
m
m
m = rawMatrix1(R^3,3,(a,b,c,a^2,b^2,c^2,a*b-1,b*c-1,c*d-1),true,0)
assert(a === rawMatrixEntry(m,0,0))
rawMatrixEntry(m,1,2,c^3)
assert(c^3 === rawMatrixEntry(m,1,2))
m
rawMatrixEntry(m,2,1,0_R)
assert(rawMatrixEntry(m,2,1) == 0_R)
m
rawMatrixRowSwap(m,0,1)
assert try (rawMatrixRowSwap(m,2,3); false) else true
rawMatrixRowSwap(m,1,2)
rawMatrixColumnSwap(m,1,2)

m

rawMatrixRowChange(m,1,a^5,0,true) 
rawMatrixRowChange(m,1,a^5,0,true) 
assert try (rawMatrixRowChange(m,1,a^5,0);false) else true

  
rawMatrixColumnChange(m,1,a^5,0,true) -- 
m
rawMatrixRowScale(m,f,2,true) -- wrong error message
rawMatrixColumnScale(m,f,2,true) -- wrong error message
m
m1 = rawMatrixRemake1(rawTarget m,m,false,0)
m * m1
assert try (rawMatrixEntry(m1,2,1,0_R); false) else true
------------------
-- rawGCD --------
------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol x,symbol y,symbol z))
f = (7*x+2*y+3*z)^2*(13*x^2+y-5)
g = (9*x-2*y+3*z)^2*(13*x^2+y-5)
assert(rawGCD(f,g) == 13*x^2+y-5)

R = polyring(rawZZp(17), (symbol x,symbol y,symbol z))
f = (7*x+2*y+3*z)^2*(13*x^2+y-5)
g = (9*x-2*y+3*z)^2*(13*x^2+y-5)
h = rawGCD(f,g)
assert(rawGCD(f,g) == x^2+4*y-3)

R = polyring(rawQQ(), (symbol x,symbol y,symbol z))
f = (7*x+2*y+3*z)^2*(13*x^2+y-5)
g = (9*x-2*y+3*z)^2*(13*x^2+y-5)
assert(rawGCD(f,g) == 13*x^2+y-5)
rawGCD(f,g)
f1 = (7*x+2*y+3*z)^2*(13*x^2+y-5)

------------------------
-- rawPseudoRemainder --
------------------------
R = polyring(rawZZ(), (symbol x,symbol y,symbol z))
f = (x+1)*y-3
g = y^2+y+1
rawPseudoRemainder(g,f)
rawPseudoRemainder(f,g)

f = 13*x^2+x+1
g = x^7
rawPseudoRemainder(g,f)

R = polyring(rawZZp(17), (symbol x,symbol y,symbol z))
f = (x+1)*y-3
g = y^2+y+1
rawPseudoRemainder(g,f)
rawPseudoRemainder(f,g)

R = polyring(rawQQ(), (symbol x,symbol y,symbol z))
f = (x+1)*y-3
g = y^2+y+1
rawPseudoRemainder(g,f)

f = x^2*y^2+x-2
g = x-3
rawPseudoRemainder(g,f)
rawPseudoRemainder(f,g)

---------------
-- rawFactor --
---------------
testfactor = (f) -> (
     -- test factorization for f
     -- assert: first factor is in base ring/field, first exponent is 1
     -- product is f
     g := rawFactor f;
     assert(g_1_0 === 1);
     assert(#g_0 === #g_1);
     assert(f === product apply(#g_0, i -> (g_0_i)^(g_1_i)))
     )

needs "raw-util.m2"
R = polyring(rawZZ(), (symbol x,symbol y,symbol z))
f = (x+y)*(x-y)
rawFactor f
testfactor f

R = polyring(rawQQ(), (symbol x,symbol y,symbol z))
f = (x+y)*(x-y)
rawFactor f
testfactor f

R = polyring(rawZZp(17), (symbol x,symbol y,symbol z))
f = (x+y)*(x-y)
rawFactor f
testfactor f

needs "raw-util.m2"
R = polyring(rawQQ(), (symbol x,symbol y,symbol z))
f = (x+3*y-14)^3*(x^2+y^4+z^7-x*y-13*x*z^2+12)
time rawFactor f
testfactor f
f = (x+3*y-14)^10*(x^2+y^4+z^7-x*y-13*x*z^2+12)^3;
--time rawFactor f -- 5.63 sec 1 Gz G4 tibook 1/19/03
--testfactor f

R = polyring(rawZZ(), (symbol x,symbol y,symbol z))
f = (x+3*y-14)^15*(x^2+y^4+z^7-x*y-13*x*z^2+12)^3;
--time rawFactor f -- 32.72 sec 1 Gz G4 tibook 1/19/03
--testfactor f
f1 = rawMatrixDiff(mat{{x}},mat{{f}});
--f2 = rawGCD(f,rawMatrixEntry(f1,0,0)); -- CRASHES!!
<< "rawGCD crashes on this one!" << endl;

needs "raw-util.m2"
R = polyring(rawZZp(17), (symbol x,symbol y,symbol z))
f = (x+3*y-14)^15*(x^2+y^4+z^7-x*y-13*x*z^2+12)^3;
time rawFactor f -- .13 sec 1 Gz G4 tibook 1/19/03
testfactor f

R = polyring(rawZZ(), 1: symbol x)
f = x^20+13*x^19+7*x^15+12*x^12-x^10-x^8+x^4+13*x-20
g = x^20+17*x^19+7*x^15+12*x^12-x^10-x^8+x^4+13*x-20
h = x^20+21*x^19+7*x^15+12*x^12-x^10-x^8+x^4+13*x-20
F = f*g*h
--time rawFactor F -- 4.1 sec 1 Gz G4 tibook 1/19/03
--testfactor F

F = f^2*g^2*h^3;
time rawFactor F -- 1.41 sec 1 Gz G4 tibook 1/19/03
testfactor F

F = f^2*g^2*h^2;
--time rawFactor F -- 4.25 sec 1 Gz G4 tibook 1/19/03
--testfactor F

R = polyring(rawZZ(), (symbol x .. symbol z))
f = 20_R
assert(f1 = rawFactor f; #f1_0 === 1 and f1_1_0 === 1)
assert(rawFactor (0_R) === (1: (0_R), 1: 1))
assert(rawFactor (4_R) === (1: (4_R), 1: 1))
assert(rawFactor (4*x^3) === ((4_R, x), (1,3)))

-------------------
-- rawCharSeries --
-------------------
R = polyring(rawZZ(), (symbol x .. symbol z))
I = rawMatrix1(R^1, 2, (x*y^2+1, x*z+y+1), false,0)
rawCharSeries(I)
rawIdealReorder I

I = rawMatrix1(R^1, 2, (x*y+x+1, y*z-x), false, 0)
rawCharSeries(I)
rawIdealReorder I

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
