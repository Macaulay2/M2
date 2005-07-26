-- Test of the routines for mutable matrices

-- Test: 
--   rawMutableIdentity
--   rawMutableMatrix (2 forms)
--   rawNumberOfRows, rawNumberOfColumns
--   rawMatrixEntry (2 forms for mutable matrices)
--   rawMatrixRowSwap, rawMatrixColSwap
--   rawMatrixRowChange, rawMatrixColChange
--   rawMatrixRowScale, rawMatrixColumnScale
needs "raw-util.m2"
R = ZZ[vars(0..11)]
id10 = rawMutableIdentity(raw R, 10, true); id10
m = genericMatrix(R,a,3,4)
p = rawMutableMatrix(raw m,true); p
assert(rawColumnDotProduct(p, 1, 1) == raw(d^2+e^2+f^2))
assert(rawColumnDotProduct(p, 1,2) == raw(d*g+e*h+f*i))
assert(rawNumberOfColumns p == 4)
assert(rawNumberOfRows p == 3)
assert(raw m === rawMatrix p)


rawMatrixRowSwap(p,0,1); p
assert(rawMatrix p - raw transpose matrix{{b,a,c},{e,d,f},{h,g,i},{k,j,l}} == 0)
rawMatrixColumnSwap(p,1,2); p
assert(rawMatrix p == raw matrix{{b,h,e,k},{a,g,d,j},{c,i,f,l}})
p
rawMatrixRowScale(p, raw (5_R), 2, false); p
rawMatrixColumnScale(p, raw (7_R), 1, false); p
rawMatrixRowChange(p, 0, raw c, 1, false); p
rawMatrixColumnChange(p, 1, raw d, 0, false); p

rawMatrixColumnOperation2(p,1,2,raw e,raw f,raw g,raw h,false) -- WRONG!
p



p = rawMutableMatrix(raw R, 3,4,true)
rawSetMatrixEntry(p, 1,3, raw(a+b))
toString p
p1 = rawMutableMatrix (raw m, true)
raw m === rawMatrix p1
p = rawMutableMatrix(raw ((transpose m) * m), true)
toString p

p = rawMutableMatrix(raw R, 10, 20, true); p      
scan(50, i -> (rawSetMatrixEntry(p,random 10, random 20, raw (random 100)_R)))
p

map(R, rawMatrix p)

rawMatrixEntry(p,0,0)
rawMatrixEntry(p,0,1)

rawMutableIdentity(raw R, 10, true)

--------------------------------------------
-- Test of mutable dense matrices over RR --
--------------------------------------------
needs "raw-util.m2"
p = rawMutableIdentity(raw RR, 10, true)
rawSetMatrixEntry(p,3,5,rawFromNumber(raw RR, 3.5))
p

p = rawMutableMatrix(raw RR, 10, 20, true); p      
scan(50, i -> (rawSetMatrixEntry(p,random 10, random 20, rawFromNumber(raw RR, random 1.0))))
p
map(RR,rawMatrix p)

--------------------------------------------
-- Test of mutable sparse matrices ---------
--------------------------------------------
needs "raw-util.m2"
R = ZZ[a..d]
p = rawMutableIdentity(raw R, 10, false)
p
rawSetMatrixEntry(p,1,3,raw(a+b))
p

R = ZZ[vars(0..11)]
m = genericMatrix(R,a,3,4)
p = rawMutableMatrix(raw m,false); p
assert(rawNumberOfColumns p == 4)
assert(rawNumberOfRows p == 3)

rawMatrixColumnOperation2(p,1,2,raw e,raw f,raw g,raw h,false) -- WRONG!
p

error "the rest of this test file is not working yet, Mike will fix it"

det m
assert(rawToInteger (2 * rawMatrixEntry(p,0,0)) == det m)

rawMatrixColumnSwap(p,3,9); p
rawMatrixRowSwap(p,4,9); p
reduceRowsColsGcd(p,9,9); p
reduceRowsColsGcd(p,8,8); p

reduceGCD(p,9,9,8)
p
reduceGCD(p,9,9,7)
p
reduceRowsCols(p,8,8); p
p

gcdCoefficients(2001,1007)
gcd(2001,1007)


rawMatrixColumnChange(p,8,raw (-42_R),9,false)
p
m = map(R,p)
toString m
p = rawMutableMatrix(p, true)
p
rawMatrix
rawMinors(5,p,0)
rawMatrixEntry(oo,0,0)


R
reduceRowsCols(p,9,9)
///
     for r from 0 to nrows-1 do
         if r =!= i then (
	     a := 
     )
///

---------------------------------------
-- mutable matrix routines ------------
---------------------------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol a .. symbol g))

<< "change these routines to call the correct raw routines with correct argument" << endl
r5 = rawIdentity(R^5,0) -- mutable r5

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
  m0 = rawMatrixRemake1(rawTarget m, m, 0)
  assert(m0 ==  rawMatrix1(R^3, 4, (a,b,c,d,e,f,g,0_R,a+b,c+d,0_R,e+f), 0))

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

  -------------------------------
  -- rawSolve -------------------
  -------------------------------
  load "raw-util.m2"
  m = mat table(4,4, (i,j) -> rawFromNumber(raw RR, random 1.0))
  b0 = mat table(4,1, (i,j) -> rawFromNumber(raw RR, random 1.0))
  A = rawMutableMatrix(m,true)
  b = rawMutableMatrix(b0,true)
  x = rawMutableMatrix(b0,true)
  p
  rawSolve(A,b,x)
  x
  x1 = rawMatrix b
  m*(rawMatrix x)
  rawMinors(4,m,0,-1,null,null)
