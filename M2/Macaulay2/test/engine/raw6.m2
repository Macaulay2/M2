-- Continuation of raw test routines for the engine

-----------------------------------------
-- Matrix -------------------------------
-----------------------------------------
--restart
needs "raw-util.m2"

R = polyring(rawZZ(), (symbol a .. symbol g))

-- tested in many places: m == 0, rawIsEqual, raw
-- rawNumberOfRows, rawNumberOfColumns, rawIsMutable,
-- rawMultiDegree
-- rawIsDense
-- rawIsZero, rawIsEqual, === 

-----------------
-- rawIdentity --
-----------------
-- Also rawMatrixEntry (setting), rawMatrixRemake1

r5 = rawIdentity(R^5,false,0) -- immutable r5
assert try (rawMatrixEntry(r5,1,2,a+b); false) else true
r5

r5 = rawIdentity(R^5,true,0) -- mutable r5
rawMatrixEntry(r5,1,2,a+b)
r5

m5 = rawMatrixRemake1(rawTarget r5, r5, false, 0)
assert try (rawMatrixEntry(m5,3,2,a-b); false) else true

F = R^0
rawIdentity(F,false,0)

-------------------
-- rawZero --------
-------------------
p = rawZero(R^0,R^100,true,0)
assert(p == 0)
assert(rawNumberOfRows p === 0)
assert(rawNumberOfColumns p === 100)
assert(rawMultiDegree p === {0})
assert(rawIsMutable p)

p = rawZero(R^100,R^0,true,0) -- DISPLAY is BAD
assert(p == 0)
assert(rawNumberOfRows p === 100)
assert(rawNumberOfColumns p === 0)
assert(rawMultiDegree p === {0})
assert(rawIsMutable p)

p = rawZero(R^0,R^100,false,0)
assert(p == 0)
assert(rawNumberOfRows p === 0)
assert(rawNumberOfColumns p === 100)
assert(rawMultiDegree p === {0})
assert(not rawIsMutable p)

p = rawZero(R^30,R^50,true,0)
assert(p == 0)
assert(rawNumberOfRows p === 30)
assert(rawNumberOfColumns p === 50)
assert(rawMultiDegree p === {0})
assert(rawIsMutable p)
rawMatrixEntry(p,1,3,a)
rawMatrixEntry(p,4,7,b)

p = rawZero(R^30,R^50,true,0)
assert(p == 0)
assert(rawNumberOfRows p === 30)
assert(rawNumberOfColumns p === 50)
assert(rawMultiDegree p === {0})
assert(rawIsMutable p)
rawMatrixEntry(p,1,3,a)
rawMatrixEntry(p,4,7,b)
assert(rawMatrixEntry(p,1,2) == 0)
assert(rawMatrixEntry(p,0,3) == 0)
assert(rawMatrixEntry(p,1,3) == a)
assert(rawMatrixEntry(p,4,7) == b)
assert(not rawIsDense p)

assert(try (rawZero(R^4, (rawZZ())^3,true,0); false) else true)

----------------------------------------
-- rawMatrix1, rawMatrix2             --
-- rawSparseMatrix1, rawSparseMatrix2 --
----------------------------------------
R = polyring(rawZZ(), (vars 0 .. vars 15))
F = rawFreeModule(R,5)
G = rawFreeModule(R,10)
m = rawSparseMatrix1(F,15,{1,3,4},{3,2,1},(a^2,b^2+a*c,b-2*a*c),false,0)
m1 = rawSparseMatrix1(F,15,{1,3,4},{3,2,1},(a^2,b^2+a*c,b-2*a*c),true,0)
assert(-(-m) == m)
assert((m-m) == m + (-m))
--<< "make sure mutable matrices and immutable matrices are not ==" << endl;
assert(not(m == m1))
assert(rawIsZero(m-m1))
assert(rawTarget m == F)
assert(rawSource m == rawFreeModule(R,splice(0,2,2,2,11:0)))
assert(rawMultiDegree m === {0})
assert rawIsZero(m - m)
assert rawIsZero(m - m1)
assert(not rawIsDense m)

m2 = rawSparseMatrix2(F,G,{7},{1,3,4},{3,3,1},(a^2,b^2+a*c,b-2*a*c),true,0)
assert(rawMultiDegree m2  === {7})
m3 = rawMatrixRemake2(F,G,rawMultiDegree m2, m2, true, 0)
m4 = rawMatrixRemake2(F,G,{13}, m2, false, 0)
assert(rawMultiDegree m4 === {13})

elems = splice apply(0..3, j -> apply(0..3, i -> rawRingVar(R,i+j,1)))
m = rawMatrix1(F,5,elems,false,0)
p1 = rawMatrix1(F,5,toSequence flatten entries m,false,0)
p2 = rawMatrix2(F,F,{0},toSequence flatten entries m,false,0)
p1 == p2

---------------------------------------
-- rawMatrixRemake, rawMatrixRemake2 --
---------------------------------------



----------------------------
-- scalar multiplication, --
-- matrix mult,           --
-- add, subtract, negate  --
----------------------------
m = rawMatrix1(R^4,4,(a,b,c,d, b^2+c*d,e^2,f^2,g^2, c^3,f^3,h^3,0_R, d^4,g^4,i^4,j^4),false,0)
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
	       false,0)
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
<< "-- ERROR: want this to be homogeneous!!" << endl;
--assert rawIsHomogeneous m2  

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
m1 = rawMatrix1(R^3,3,(a,b,c,d,e,f,g,h,i),false,0)
m2 = rawMatrix1(R^3,3,(a+1_R,b+1_R,c+1_R,d^2,e^2,f^2,g^3,h^3,i^3),false,0)
m3 = rawConcat(m1,m2,m1)
assert(m2 == rawSubmatrix(m3,(3,4,5)))
assert(m1 == rawSubmatrix(m3,(6,7,8)))
assert(m1 == rawSubmatrix(m3,(0,1,2)))
assert(try (rawConcat(m1,rawDual m3); false) else true)

M = rawFreeModule(R,(-1,-2))
N = rawFreeModule(R,(3,4))
P = rawFreeModule(R,(0,2))
f = rawMatrix2(M,N,1:0,(0_R,0_R,0_R,0_R),false,0)
g = rawMatrix2(M,P,1:0,(0_R,0_R,0_R,0_R),false,0)
assert ( rawTarget f === rawTarget g )
assert ( rawSource f ++ rawSource g === rawSource rawConcat(f,g) )
assert ( rawTarget f === rawTarget rawConcat(f,g) )

-------------------
-- rawDirectSum ---
-------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol a .. symbol h))
m1 = rawMatrix1(R^2,2,(a,b,c,d), false, 0)
m2 = rawMatrix1(R^2,2,(e,f,g,h), false, 0)
m3 = rawDirectSum(m1,m2,m1)
m4 = rawSubmatrix(m3,(2,3),(2,3))

rawSubmatrix(m3,(2,3),(2,3)) == m2
rawSubmatrix(m3,(4,5),(4,5)) == m1
-----------------------------
-- rawTensor, rawFlip  ------
-----------------------------
m1 = rawMatrix1(R^2,2,(a,b,c,d), false, 0)
m2 = rawMatrix1(R^2,2,(e,f,g,h), false, 0)
m3 = rawTensor(m1,m2)
m4 = rawTensor(m2,m1)
m3a = rawMatrix1(R^4,4,(a*e, a*f, b*e, b*f, 
	              a*g, a*h, b*g, b*h, 
                      c*e, c*f, d*e, d*f, 
                      c*g, c*h, d*g, d*h),
                 false,0)
assert(m3 == m3a)
assert(isHomogeneous m3)
m4a = rawMatrix1(R^4,4,(a*e, b*e, a*f, b*f, 
          c*e, d*e, c*f, d*f, 
          a*g, b*g, a*h, b*h, 
          c*g, d*g, c*h, d*h),
      false,0)
assert(m4a == m4)     

f1 = rawFlip(rawTarget m1, rawTarget m2)
f2 = rawFlip(rawSource m1, rawSource m2)
assert(m4a == f2*m3a*f1)

-------------------
-- rawDual --------
-------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol a .. symbol j))

m = rawMatrix1(R^4,4,(a,b,c,d, b^2+c*d,e^2,f^2,g^2, c^3,f^3,h^3,0_R, d^4,g^4,i^4,j^4),false,0)
assert(m == rawDual rawDual m)
for i from 0 to rawNumberOfRows m - 1 do
  for j from 0 to rawNumberOfColumns m - 1 do
    assert(rawMatrixEntry(rawDual m,i,j) == rawMatrixEntry(m,j,i))

m = rawMatrix1(R^4,4,(a,b,c,d, b^2+c*d,e^2,f^2,g^2, c^3,f^3,h^3,0_R, d^4,g^4,i^4,j^4),true,0)
assert(m - rawDual rawDual m == 0)
for i from 0 to rawNumberOfRows m - 1 do
  for j from 0 to rawNumberOfColumns m - 1 do
    assert(rawMatrixEntry(rawDual m,i,j) == rawMatrixEntry(m,j,i))

---------------------
-- rawMatrixRandom --
---------------------

-------------------
-- rawReshape -----
-------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol a .. symbol l))
m = rawMatrix2(R^2, R^6, {0}, (a,b,c,d,e,f,g,h,i,j,k,l), false,0)
m2 = rawMatrix2(R^3, R^4, {0}, (a,b,c,d,e,f,g,h,i,j,k,l), false,0)
m3 = rawMatrix2(R^12, R^1, {0}, (a,b,c,d,e,f,g,h,i,j,k,l), false,0)
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
-------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol a .. symbol j))
m1 = rawMatrix1(R^2,2,(a,b,c,d), false, 0)
assert(rawMatrixEntry(rawSubmatrix(m1,singleton 1, singleton 0), 0,0) == c)

-------------------
-- rawKoszul ------
-- (2 forms??) ----
-------------------

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
M = rawMatrix1(R^4, 4, (a,b,c,0_R,e,f,0_R,h,i,j,k,l,m,n,o,0_R), false, 0)
rawExteriorPower(2,M,0) == rawExteriorPower(2,M,1)
rawExteriorPower(2,M,0) == rawExteriorPower(2,M,1)
rawExteriorPower(3,M,0) == rawExteriorPower(3,M,1)
rawExteriorPower(4,M,0) == rawExteriorPower(4,M,1)
rawExteriorPower(5,M,0) == 0
rawExteriorPower(5,M,1) == 0

assert (rawMinors(3,M,0) == rawMinors(3,M,1))

-------------------
-- rawPfaffians ---
-------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (vars 0 .. vars 9))
m = rawMatrix1(R^4,4,(0_R,b,c,d, -b,0_R,f,g, -c,-f,0_R,i, -d,-g,-i,0_R),false,0)
m1 = rawPfaffians(4,m)
m2 = rawMatrix1(R^1,1,singleton (d*f-c*g+b*i), false,0)
assert(rawTarget m1 == rawTarget m2)
assert(rawSource m1 == rawSource m2)
assert(rawIsMutable m1 == rawIsMutable m2)
assert(rawMultiDegree m1 == rawMultiDegree m2)
assert(m1 == m2)

-------------------
-- rawMatrixDiff --
-------------------

-----------------------
-- rawMatrixContract --
-----------------------

--------------------
-- rawSortColumns --
--------------------

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
---------------------

-------------------
-- rawMonomials ---
-------------------

-------------------
-- rawInitial -----
-------------------

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

---------------------
-- rawModuleTensor --
--------------------- -- not written


---------------------------------------
-- mutable matrix routines ------------
---------------------------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol a .. symbol g))

r5 = rawIdentity(R^5,true,0) -- mutable r5

  -----------------------
  -- rawMatrixEntry -----
  -----------------------
  rawMatrixEntry(r5,1,2,a+b)
  assert(rawMatrixEntry(r5,1,2) === a+b)

  rawMatrixEntry(r5,0,0,a^5)
  assert try(rawMatrixEntry(r5,5,5,a^5);false) else true
  rawMatrixEntry(r5,0,0,0_R)
  assert(rawMatrixEntry(r5,0,0) === 0_R)

  ----------------------
  -- rawMatrixRowSwap --
  ----------------------
  m = rawMatrix1(R^3, 4, (a,b,c,d,e,f,g,0_R,a+b,c+d,0_R,e+f), true, 0)
  rawMatrixRowSwap(m, 0,1)
  m
  rawMatrixRowSwap(m, 0,1)
  m0 = rawMatrixRemake1(rawTarget m, m, false, 0)
  assert(m0 ==  rawMatrix1(R^3, 4, (a,b,c,d,e,f,g,0_R,a+b,c+d,0_R,e+f), false, 0))

  rawMatrixRowSwap(m, 0,0)
  m
  assert try(rawMatrixRowSwap(m, 0,4); false) else true
  
  ----------------------
  -- rawMatrixColSwap --
  ----------------------

  ------------------------
  -- rawMatrixRowChange --
  ------------------------

  ------------------------
  -- rawMatrixColChange --
  ------------------------

  -----------------------
  -- rawMatrixRowScale --
  -----------------------

  --------------------------
  -- rawMatrixColumnScale --
  --------------------------

  ----------------------
  -- rawInsertColumns --
  ----------------------

  ----------------------
  -- rawInsertRows -----
  ----------------------

  ----------------------
  -- rawDeleteColumns --
  ----------------------

  ----------------------
  -- rawDeleteRows -----
  ----------------------

  -------------------------------
  -- rawMatrixColumnOperation2 --
  -------------------------------

  -------------------------------
  -- rawMatrixRowOperation2 -----
  -------------------------------

  -------------------------------
  -- rawSortColumns -------------
  -------------------------------

  -------------------------------
  -- rawPermuteRows -------------
  -------------------------------

  -------------------------------
  -- rawRowChange ---------------
  -------------------------------

  -------------------------------
  -- rawColumnChange-------------
  -------------------------------
