--status: this old test depends on internal things and probably should be deleted


----------------------------------
-- Test of engine matrix code ----
----------------------------------
-- this file includes tests when the base ring is
-- a polynomial ring.

needs "raw-util.m2"
errorDepth = 0


-- tested in many places: m == 0, rawIsEqual, 
-- rawTarget, rawSource
-- rawNumberOfRows, rawNumberOfColumns
-- rawMultiDegree
-- rawIsDense
-- rawIsZero, rawIsEqual, === 

-- toString, hash
-- rawMatrixEntry (can give error)
-- rawIsDense

-----------------
-- rawIdentity --
-----------------
needs "raw-util.m2"
errorDepth = 0


-- bug here...
R = polyring(rawZZ(),1:symbol x)
R = raw (ZZ[symbol x])
f = mat{{R_0}}
F = rawFreeModule(R,1)
G = rawFreeModule(R,1:-2)
h = rawMatrixRemake2(F,G,1:3,f,0)
rawMultiDegree h
--


R = polyring(rawZZ(), (symbol a .. symbol g))
r5 = rawIdentity(R^5,0)

assert try (rawMatrixEntry(r5,1,2,a+b); false) else true
F = R^0
rawIdentity(F,0)

R = ZZ[symbol a .. symbol g]
F = R^5
map(F,F,1) -- rawIdentity is called

-------------------
-- rawZero --------
-------------------
needs "raw-util.m2"
errorDepth = 0
R = polyring(rawZZ(), (symbol a .. symbol g))
p = rawZero(R^0,R^100,0)
assert(p == 0)
assert(rawIsZero p)
assert(rawNumberOfRows p === 0)
assert(rawNumberOfColumns p === 100)
assert(rawMultiDegree p === {0})

p = rawZero(R^100,R^0,0) -- DISPLAY is BAD
assert(p == 0)
assert(rawNumberOfRows p === 100)
assert(rawNumberOfColumns p === 0)
assert(rawMultiDegree p === {0})

p = rawZero(R^0,R^100,0)
assert(p == 0)
assert(rawNumberOfRows p === 0)
assert(rawNumberOfColumns p === 100)
assert(rawMultiDegree p === {0})

p = rawZero(R^30,R^500,0)
assert(p == 0)
assert(rawNumberOfRows p === 30)
assert(rawNumberOfColumns p === 500)
assert(rawMultiDegree p === {0})
assert(not rawIsDense p)

assert(try (rawZero(R^4, (rawZZ())^3,0); false) else true)

----------------------------------------
-- rawMatrix1, rawMatrix2             --
-- rawSparseMatrix1, rawSparseMatrix2 --
----------------------------------------
needs "raw-util.m2"
errorDepth = 0
R = polyring(rawZZ(), (vars 0 .. vars 15))
F = rawFreeModule(R,5)
G = rawFreeModule(R,10)
m = rawSparseMatrix1(F,15,{1,3,4},{3,2,1},(a^2,b^2+a*c,b-2*a*c),0)
assert(-(-m) == m)
assert((m-m) == m + (-m))
assert(rawTarget m == F)
assert(rawSource m == rawFreeModule(R,splice(0,2,2,2,11:0)))
assert(rawMultiDegree m === {0})
assert rawIsZero(m - m)
assert(not rawIsDense m)

m2 = rawSparseMatrix2(F,G,{7},{1,3,4},{3,3,1},(a^2,b^2+a*c,b-2*a*c),0)
assert(rawMultiDegree m2  === {7})
m3 = rawMatrixRemake2(F,G,rawMultiDegree m2, m2, 0)
m4 = rawMatrixRemake2(F,G,{13}, m2, 0)
assert(rawMultiDegree m4 === {13})

elems = splice apply(0..3, j -> apply(0..3, i -> rawRingVar(R,i+j)))
m = rawMatrix1(F,5,elems,0)
p1 = rawMatrix1(F,5,toSequence flatten entries m,0)
p2 = rawMatrix2(F,F,{0},toSequence flatten entries m,0)
p1 == p2

---------------------------------------
-- rawMatrixRemake, rawMatrixRemake2 --
---------------------------------------
needs "raw-util.m2"
errorDepth = 0
R = polyring(rawZZ(), (vars 0 .. vars 15))
m = rawMatrix2(R^3, R^4, 1:0, (a,b,c,d,e,f,g,h,i,j,k,l), 0)
G = rawSource m
m1 = rawMatrixRemake1(R^3, m, 0)
G1 = rawSource m1
m2 = rawMatrixRemake2(R^3, R^4, 1:0, m1, 0)
assert (m2 === m)
assert(rawSource m2 === rawSource m)
assert(rawTarget m2 === rawTarget m)
assert(m-m2 == 0)
assert(G1 === rawFreeModule(R, (1,1,1,1)))
assert not isHomogeneous m
assert isHomogeneous m1
assert try (rawMatrixRemake2(R^3,(rawZZ())^4, 1:0, m, 0);false) else true
----------------------------
-- scalar multiplication, --
-- matrix mult,           --
-- add, subtract, negate  --
----------------------------
needs "raw-util.m2"
errorDepth = 0
R = polyring(rawZZ(), (vars 0 .. vars 15))
m = rawMatrix1(R^4,4,(a,b,c,d, b^2+c*d,e^2,f^2,g^2, c^3,f^3,h^3,0_R, d^4,g^4,i^4,j^4),0)
m2 = m*m
m3 = m2*m
m4 = m2*m2
m4b = m3*m
m4c = m*m3
assert(m4 == m4b)
assert(m4b == m4c)
assert(rawMultiDegree m2 == {0})
assert(rawTarget m == rawTarget m2)
assert(rawSource m == rawSource m2)
assert not rawIsHomogeneous m4

m = rawMatrix1(rawFreeModule(R,(0,-1,-2,-3)),4,
               (a,b,c,d, b^2+c*d,e^2,f^2,g^2, c^3,f^3,h^3,0_R, d^4,g^4,i^4,j^4),
	      0)
assert rawIsHomogeneous m
m2 = m*m
m3 = m2*m
m4 = m2*m2
m4b = m3*m
m4c = m*m3
assert(m4 == m4b)
assert(m4b == m4c)
assert(rawMultiDegree m2 == {0})
assert(rawTarget m == rawTarget m2)
assert(rawSource m == rawSource m2)
assert not rawIsHomogeneous m2  

assert(m+m == 2*m)
assert(m + 3*m == 4*m)
assert(m + m*m == m*m + m)
assert(a*((b^2-b*c)*m) == (a*b^2-a*b*c)*m)
assert(rawIsHomogeneous (a*m))
assert(rawMultiDegree (a^5*m) == {5})
assert(rawTarget (a^5*m) == rawTarget m)
assert(rawSource (a^5*m) == rawSource m)
assert(a*(b*m)-b*(a*m) == 0)

-------------------
-- rawConcat ------
-------------------
m1 = rawMatrix1(R^3,3,(a,b,c,d,e,f,g,h,i),0)
m2 = rawMatrix1(R^3,3,(a+1_R,b+1_R,c+1_R,d^2,e^2,f^2,g^3,h^3,i^3),0)
m3 = rawConcat(m1,m2,m1)
assert(m2 == rawSubmatrix(m3,(3,4,5)))
assert(m1 == rawSubmatrix(m3,(6,7,8)))
assert(m1 == rawSubmatrix(m3,(0,1,2)))
assert(try (rawConcat(m1,rawDual m3); false) else true)

M = rawFreeModule(R,(-1,-2))
N = rawFreeModule(R,(3,4))
P = rawFreeModule(R,(0,2))
f = rawMatrix2(M,N,1:0,(0_R,0_R,0_R,0_R),0)
g = rawMatrix2(M,P,1:0,(0_R,0_R,0_R,0_R),0)
assert ( rawTarget f === rawTarget g )
assert ( rawSource f ++ rawSource g === rawSource rawConcat(f,g) )
assert ( rawTarget f === rawTarget rawConcat(f,g) )

-------------------
-- rawDirectSum ---
-------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol a .. symbol h))
m1 = rawMatrix1(R^2,2,(a,b,c,d), 0)
m2 = rawMatrix1(R^2,2,(e,f,g,h), 0)
m3 = rawDirectSum(m1,m2,m1)
m4 = rawSubmatrix(m3,(2,3),(2,3))

rawSubmatrix(m3,(2,3),(2,3)) == m2
rawSubmatrix(m3,(4,5),(4,5)) == m1
-----------------------------
-- rawTensor, rawFlip  ------
-----------------------------
m1 = rawMatrix1(R^2,2,(a,b,c,d), 0)
m2 = rawMatrix1(R^2,2,(e,f,g,h), 0)
m3 = rawTensor(m1,m2)
m4 = rawTensor(m2,m1)
m3a = rawMatrix1(R^4,4,(a*e, a*f, b*e, b*f, 
	              a*g, a*h, b*g, b*h, 
                      c*e, c*f, d*e, d*f, 
                      c*g, c*h, d*g, d*h),
                 0)
assert(m3 == m3a)
assert(isHomogeneous m3)
m4a = rawMatrix1(R^4,4,(a*e, b*e, a*f, b*f, 
          c*e, d*e, c*f, d*f, 
          a*g, b*g, a*h, b*h, 
          c*g, d*g, c*h, d*h),
      0)
assert(m4a == m4)     

f1 = rawFlip(rawTarget m1, rawTarget m2)
f2 = rawFlip(rawSource m1, rawSource m2)
assert(m4a == f2*m3a*f1)

---------------------
-- rawWedgeProduct --
---------------------
assert instance(rawWedgeProduct,Function)

-------------------
-- rawDual --------
-------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol a .. symbol j))

m = rawMatrix1(R^4,4,(a,b,c,d, b^2+c*d,e^2,f^2,g^2, c^3,f^3,h^3,0_R, d^4,g^4,i^4,j^4),0)
assert(m == rawDual rawDual m)
for i from 0 to rawNumberOfRows m - 1 do
  for j from 0 to rawNumberOfColumns m - 1 do
    assert(rawMatrixEntry(rawDual m,i,j) == rawMatrixEntry(m,j,i))

m = rawMatrix1(R^4,4,(a,b,c,d, b^2+c*d,e^2,f^2,g^2, c^3,f^3,h^3,0_R, d^4,g^4,i^4,j^4),0)
assert(m - rawDual rawDual m == 0)
for i from 0 to rawNumberOfRows m - 1 do
  for j from 0 to rawNumberOfColumns m - 1 do
    assert(rawMatrixEntry(rawDual m,i,j) == rawMatrixEntry(m,j,i))

---------------------
-- rawRandomConstantMatrix --
---------------------
needs "raw-util.m2"
mr = rawRandomConstantMatrix(rawZZ(),2,3,.5,0,0)
mr = rawRandomConstantMatrix(rawZZ(),20,30,.5,0,0)
mr = rawRandomConstantMatrix(rawZZ(),20,30,.98,0,0)
mr = rawRandomConstantMatrix(rawZZ(),30,30,.5,1,0)
mr = rawRandomConstantMatrix(rawZZ(),30,30,1.0,1,0)

mr = rawRandomConstantMatrix(rawZZ(),10,15,.5,0,0)
R = polyring(rawZZp(32003), (symbol x, symbol y))
mr = rawRandomConstantMatrix(R,10,15,.5,0,0)
mr = rawRandomConstantMatrix(R,10,15,.5,1,0)
mr = rawRandomConstantMatrix(R,10,10,.5,1,0)


-------------------
-- rawReshape -----
-------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol a .. symbol l))
m = rawMatrix2(R^2, R^6, {0}, (a,b,c,d,e,f,g,h,i,j,k,l),0)
m2 = rawMatrix2(R^3, R^4, {0}, (a,b,c,d,e,f,g,h,i,j,k,l),0)
m3 = rawMatrix2(R^12, R^1, {0}, (a,b,c,d,e,f,g,h,i,j,k,l),0)
n2 = rawReshape(rawDual m,R^3,R^4)
n2 == m2
n2
m2
-- ERROR: this next test is flawed, FIX IT!
--assert(rawIsEqual(m,m2) and rawIsEqual(m,m3))

rawReshape(m,R^4,R^3)
rawReshape(rawDual m,R^1,R^12)

-------------------
-- rawSubmatrix ---
-- 2 forms --------
-------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol a .. symbol j))
m1 = rawMatrix1(R^2,2,(a,b,c,d), 0)
assert(rawMatrixEntry(rawSubmatrix(m1,1: 1, 1: 0), 0,0) == c)

-------------------
-- rawKoszul ------
-- (2 forms??) ----
-------------------
rawKoszul
rawKoszulMonomials

needs "raw-util.m2"
R = polyring(rawZZ(), splice (symbol x, symbol y, symbol z,symbol a .. symbol d))
m1 = rawMatrix1(R^1,3,(a^2*x,a*b*y, b^2*z), 0)
m2 = rawMatrix1(R^1,3,(a^2*b*x*y,a^2*b^2*x*z, a*b^2*y*z), 0)
m3 = rawMatrix1(R^1,1,1:(a^2*b^2*x*y*z), 0)
rawKoszulMonomials(3,m1,m2)
rawKoszulMonomials(3,m2,m3)
rawKoszulMonomials(3,m1,m3)


A = ZZ[symbol a..symbol d]
B = A[symbol x .. symbol z]

m1 = matrix{{a^2*x,a*b*y, b^2*z}}
m2 = matrix{{a^2*b*x*y,a^2*b^2*x*z, a*b^2*y*z}}
m3 = matrix{{a^2*b^2*x*y*z}}
map(B,rawKoszulMonomials(numgens B, raw m1, raw m2))
map(B,rawKoszulMonomials(0, raw m1, raw m2))
-----------------------
-- rawSymmetricPower --
-----------------------
needs "raw-util.m2"
R = polyring(rawQQ(), (symbol x, symbol y, symbol z))
m = mat{{x,y,z}}
rawSymmetricPower(-2,m)
rawSymmetricPower(-1,m)
rawSymmetricPower(0,m)
rawSymmetricPower(1,m)
rawSymmetricPower(2,m)
rawSymmetricPower(3,m)
rawSymmetricPower(4,m)
rawSymmetricPower(5,m)
m6 = rawSymmetricPower(6,m)
assert(rawTarget m6 == R^1)
assert(rawSource m6 == rawFreeModule(R,28:6))
assert(rawRank rawSource m6 === 28)
assert(rawRank rawTarget m6 === 1)

---------------------------------
-- rawExteriorPower, rawMinors --
---------------------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (vars 0 .. vars 15))
M = rawMatrix1(R^4, 4, (a,b,c,0_R,e,f,0_R,h,i,j,k,l,m,n,o,0_R), 0)
rawExteriorPower(2,M,0) == rawExteriorPower(2,M,1)
rawExteriorPower(2,M,0) == rawExteriorPower(2,M,1)
rawExteriorPower(3,M,0) == rawExteriorPower(3,M,1)
rawExteriorPower(4,M,0) == rawExteriorPower(4,M,1)
rawExteriorPower(5,M,0) == 0
rawExteriorPower(5,M,1) == 0

assert (rawMinors(3,M,0,-1,null,null) == rawMinors(3,M,1,-1,null,null))

-------------------
-- rawPfaffians ---
-------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (vars 0 .. vars 9))
m = rawMatrix1(R^4,4,(0_R,b,c,d, -b,0_R,f,g, -c,-f,0_R,i, -d,-g,-i,0_R),0)
m1 = rawPfaffians(4,m)
m2 = rawMatrix1(R^1,1,1: (d*f-c*g+b*i), 0)
assert(rawTarget m1 == rawTarget m2)
assert(rawSource m1 == rawSource m2)
assert(rawMultiDegree m1 == rawMultiDegree m2)
assert(m1 == m2)

-----------------------
-- rawMatrixDiff ------
-- rawMatrixContract --
-----------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol x .. symbol z))
x = rawRingVar(R,0)
y = rawRingVar(R,1)

assert(rawMatrixDiff(mat{{x}}, mat{{x^3+7*x+2}}) == mat{{3*x^2+7}})
assert(rawMatrixContract(mat{{2*x}}, mat{{x^3+7*x+2}}) == mat{{2*x^2+14}})

R = ZZ[symbol x]
diff(matrix{{x}}, matrix{{x^3+7*x+2}}) == matrix{{3*x^2+7}}
contract(matrix{{x}}, matrix{{x^3+7*x+2}}) == matrix{{x^2+7}}
--------------------
-- rawSortColumns --
--------------------
rawSortColumns

-------------------------------------
-- rawIsHomogeneous, rawHomogenize --
-------------------------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol a .. symbol g))
m = mat{{a^2-a^3*b*c, a^4*c*d+a^3*b+1, a*b*c*d}}
assert not rawIsHomogeneous m
assert rawIsHomogeneous rawHomogenize(m,6,(1,1,1,1,1,1,1))
m1 = rawHomogenize(m,6,(-1,-1,-1,-1,-1,-1,1))
m2 = rawHomogenize(m,6,(1,1,1,1,1,1,-1))
assert(m1 == m2)
assert(m1 == mat{{-a^3*b*c*g^3+a^2, a^4*c*d*g^6+a^3*b*g^4+1, a*b*c*d}})
assert(rawHomogenize(m,6,(1,1,1,1,1,1,1))
      == mat{{-a^3*b*c+a^2*g^3, a^4*c*d+a^3*b*g^2+g^6, a*b*c*d}})

---------------------
-- rawCoefficients --
-- rawMonomials -----
---------------------
-- a simple example first:
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol x .. symbol z))
m = mat{{x*y+x*z+x^2*y*z}}
mons = rawMonomials(1:0,m)
coeffs = rawCoefficients(1:0, mons, m)
mons * coeffs
print "WARNING: in rawMonomials, monomials should be sorted"
------------------
needs "raw-util.m2"
R = ZZ/101[symbol a .. symbol d]
f = (a+b)^4*(c+d)^2
m = rawMonomials((0,1),mat{{raw f}})
m = matrix{{f}}
coefficients(m,Variables=>{a,b})
------------------
A = ZZ[symbol a,symbol b]
B = A[symbol x, symbol y]
m = matrix{{a*x+b*y}}
m2 = matrix{{x^2,x*y,y^2}}
m3 = matrix{{x^3,x^2*y,x*y^2,y^3}}
M = m ** m2
print "coefficients interface is NOT complete"
--coefficients((x,y), m3, M)
-------------------
-- rawInitial -----
-------------------
-- test this for up and down orders, and also
-- for Schreyer orders
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol a .. symbol g))
m = mat{{a^2-a^3*b*c, a^4*c*d+a^3*b+1, a*b*c*d}}
rawInitial(1,m)

R = ZZ[symbol a .. symbol g, MonomialOrder => { Weights => {1,1}, Lex => 7, Position => Up }]
m = matrix{{a^2-a^3*b*c, a^4*c*d+a^3*b+1, a*b*c*d, a+b^2}}
assert(leadTerm(m) == matrix{{-a^3*b*c, a^4*c*d, a*b*c*d, b^2}})

R = ZZ[symbol a .. symbol c, MonomialOrder => { Position => Up }]
m = matrix{{a},{b},{c}}
assert(leadTerm m == matrix{{0},{0},{c}})

R = ZZ[symbol a .. symbol c, MonomialOrder => { Position => Down }]
m = matrix{{a},{a},{a}}
assert(leadTerm m == matrix{{a},{0},{0}})

R = ZZ[symbol a .. symbol c, MonomialOrder => { RevLex => 3, Position => Down }, Global => false]
m = matrix{{1+b+c}}
leadTerm m
assert(leadTerm m - matrix{{1_R}} == 0)

needs "raw-util.m2"
R = ZZ[symbol a..symbol d][symbol x,symbol y,symbol z]
f = matrix{{(a+b+c)*x^3 + x^2*y}}
leadTerm f
assert(leadTerm(0,f) == f)
assert(leadTerm(1,f) == matrix{{(a+b+c)*x^3}})
assert(leadTerm(2,f) == matrix{{a*x^3}})
assert(leadTerm(3,f) == leadTerm(2,f))

R = ZZ[symbol a .. symbol d, MonomialOrder => { RevLex => 1, GRevLex => 2, RevLex => 1}, Global=>false]
f = matrix{{1+a+b+c+d+b*d}}
assert(leadTerm(0,f) == f)
assert(leadTerm(1,f) == matrix{{b+b*d+c+1+d}})
assert(leadTerm(2,f) == matrix{{b+b*d}})
assert(leadTerm(3,f) - matrix{{b}} == 0)

-- Schreyer order initial terms
R = ZZ[symbol a..symbol d, MonomialOrder => Lex]
F = source schreyerOrder matrix{{c,d^3,a^2}}
g = map(F, R^1, {{a},{b},{c}})
assert(leadTerm g - matrix{{0},{0},{c}} == 0)
---------------------------
-- rawEliminateVariables --
---------------------------

----------------------
-- rawKeepVariables --
----------------------

-------------------------
-- rawDivideByVariable --
-------------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol a .. symbol g))
m = mat{{a^2-a^3*b*c, a^4*c*d+a^3*b+1, a*b*c*d}}
(m1,d1) = rawDivideByVariable(m,0,-1)
assert(d1 === 2)
<< " ERROR: since m is not homogeneous" << endl;
--assert(m1 == mat{{1_R-a*b*c, a^4*c*d+a^3*b+1_R, b*c*d}})
  -- ERROR: since m is not homogeneous, the 
  -- result m1 has non-zero degrees.

rawDivideByVariable(m,0,100)
rawDivideByVariable(m,0,1)
rawDivideByVariable(m,0,0)
rawDivideByVariable(m,1,-1)
rawDivideByVariable(m,4,-1)
rawDivideByVariable(m-m,1,-1)

-----------------------
-- rawMatrixCompress --
-----------------------
assert instance(rawMatrixCompress,Function)

---------------------
-- rawModuleTensor --
---------------------

------------------------
-- rawTopCoefficients --
------------------------
assert instance(rawTopCoefficients,Function)

---------------------
-- rawBasis --
---------------------
needs "raw-util.m2"
R = polyring(rawZZp 101, (symbol a .. symbol c))
m = mat{{0_R}}
b1 = rawBasis(m, {1}, {1}, {1}, {0,1,2}, false, -1)
assert(b1 == mat{{a,b,c}})

needs "raw-util.m2"
R = polyring(rawZZp 101, (symbol a .. symbol c))
m = mat{{0_R}}
b1 = rawBasis(m, {2}, {2}, {1}, {0,1,2}, false, -1)
assert(b1 == mat{{a^2, a*b, a*c, b^2, b*c, c^2}})

b1 = rawBasis(m, {2}, {3}, {1}, {0,1,2}, false, -1)

m = mat{{a^2,b^2}}
b1 = rawBasis(m, {2}, {3}, {1}, {0,1,2}, false, -1)
assert(b1 == mat{{a*b,a*b*c,a*c,a*c^2,b*c,b*c^2,c^2,c^3}})

m = mat{{a^3,b^3,a*b*c,c^3}}
b1 = rawBasis(m, {}, {}, {1}, {0,1,2}, false, -1)

m = mat{{a^3,b^3}}
rawBasis(m, {}, {}, {1}, {0,1}, false, -1)

m = mat{{a^7,b^7}}
b1 = rawBasis(m, {}, {}, {1}, {0,1}, false, 10)
assert(rawNumberOfColumns b1 == 10)

m = rawMatrix1(R^{-1}, 2, (a^3,b^2), 0)
mb = rawBasis(m, {2}, {2}, {1}, {0,1}, false, -1)
assert(mb == mat{{a,b}})

rawBasis(mat{{0_R}}, {-1}, {-1}, {1}, {0,1,2}, false, -1)
rawBasis(mat{{0_R}}, {1}, {1}, {1}, {0,1,2}, false, -1)

R = ZZ/101[symbol a .. symbol c]
m = coker matrix{{0_R}}
basis(1,coker matrix{{0_R}})
m = raw presentation m
rawBasis(m,{1},{1},{1},(0,1,2),false,-1)
rawTarget m
rawSource m
m = mat{{0_(raw R)}}
rawBasis(m,{1},{1},{1},(0,1,2),false,-1)
rawBasis(m,{2},{2},{1},(0,1,2),false,-1)
rawBasis(m,{2},{2},{1},{0,1,2},false,-1)
m
rawTarget m
rawSource m
-- A truncation example
needs "raw-util.m2"
R = polyring(rawZZp 101, (symbol a .. symbol c))
m = rawMatrix1(R^{-1,-3,-6}, 3, (a^3,0_R,0_R,
	                         0_R,b^3,0_R,
				 0_R,0_R,a*b), 0)
mb = rawBasis(m, {2}, {2}, {1}, {0,1}, true, -1)
mb = rawBasis(m, {3}, {3}, {1}, {0,1}, true, -1)
mb = rawBasis(m, {4}, {4}, {1}, {0,1}, true, -1)
mb = rawBasis(m, {5}, {5}, {1}, {0,1}, true, -1)
mb = rawBasis(m, {6}, {6}, {1}, {0,1}, true, -1)
mb = rawBasis(m, {7}, {7}, {1}, {0,1}, true, -1)
mb = rawBasis(m, {100}, {100}, {1}, {0,1}, true, -1)
mb = rawBasis(m, {1000}, {1000}, {1}, {0,1}, true, -1)

-- entire basis of a multi-graded ring
needs "raw-util.m2"
R = polyring3(rawZZp 101, (symbol a, symbol b, symbol c), rawMonomialOrdering { Lex => 3}, {(1,1,0),(0,1,1),(0,1,2)})
m = mat{{a^2,b^3,c^3}}
mb = rawBasis(m, {}, {}, {1,1,1}, {0,1,2}, false, -1)

-- Multigraded examples
needs "raw-util.m2"
R = polyring3(rawZZp 101, (symbol a, symbol b, symbol c), rawMonomialOrdering { Lex => 3}, {(1,1,0),(0,1,1),(0,1,2)})
m = mat{{0_R}}
mb = rawBasis(m, {1,3,3}, {1,3,3}, {1,1,1}, {0,1,2}, false, -1)
assert (mb == mat{{a*b*c}})
mb = rawBasis(m, {2,6,6}, {2,6,6}, {1,1,1}, {0,1,2}, false, -1)

R = polyring3(rawZZp 101, (symbol a .. symbol d), rawMonomialOrdering { Lex => 4}, ((1,-3),(1,-5),(0,1),(0,1)))
assert try (b1 = rawBasis(mat{{0_R}}, {}, {3,2}, {3,2}, {0,1,2}, false, -1);false) else true
assert try(b1 = rawBasis(mat{{0_R}}, {3,2}, {3,2}, {1,6}, {0,1,2,3}, false, -1);false) else true
b1 = rawBasis(mat{{0_R}}, {3,2}, {3,2}, {6,1}, {0,1,2,3}, false, -1)

-- Over ZZ
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol x, symbol y))
A = rawQuotientRing(R, mat{{2*x,3*y,x*y}})
mb = rawBasis(mat{{0_A}}, {2}, {2}, {1}, {0,1}, false, -1)

-- Polynomial ring over polynomial ring
A = ZZ/101[symbol a .. symbol c]
B = A[symbol x, symbol y]
basis(2,B)
B = raw B
b1 = rawBasis(mat{{0_B}}, {2},{2},{1},(0,1),false,-1)
f = a*x^2 + b*x*y + c*y^2
rawCoefficients((0,1),b1,mat{{raw f}})

print "WARNING: rawBasis: test for quotient rings"

R = ZZ/101[symbol a .. symbol c]
m = mat{{0_(raw R)}}
rawBasis(m,{1},{1},{1},{0,1,2},false,-1)

------------------------------
-- rawRemoveScalarMultiples --
------------------------------
needs "raw-util.m2"
R = ZZ/32003[symbol a..symbol d]
m = matrix{{3*a^2-6*a*b, 2*a^2-4*a*b, -a^2+a*b}}
m1 = rawRemoveScalarMultiples raw m
assert(m1 === raw matrix{{2*a^2-4*a*b, -a^2+a*b}})

m = matrix{{3*a^2-6*a*b, 2*a^2-4*a*b, -a^2+2*a*b, 0_R, 1_R}}
m1 = rawRemoveScalarMultiples raw m
assert(m1 === raw matrix{{-a^2+2*a*b, 1_R}})

------------------------------
-- rawRemoveMonomialFactors --
------------------------------
needs "raw-util.m2"
R = ZZ/32003[symbol a..symbol d]
m = matrix{{a^2*b*c*(a^3-b^3-c^3)}}
m1 = rawRemoveMonomialFactors(raw m,true)
assert(m1 == mat{{raw(a*b*c*(a^3-b^3-c^3))}})
m1 = rawRemoveMonomialFactors(raw m,false)
assert(m1 == mat{{raw(a^3-b^3-c^3)}})

-- Local Variables:
-- compile-command: "M2 -e errorDepth=0 --stop -e 'load \"raw-matrix.m2\"' -e 'exit 0' "
-- End:
