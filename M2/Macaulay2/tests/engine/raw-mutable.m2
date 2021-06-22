--status: this old test depends on internal things and probably should be deleted


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
R = RR_53
p = rawMutableIdentity(raw R, 10, true)
rawSetMatrixEntry(p,3,5,rawFromNumber(raw R, 3.5))
p

p = rawMutableMatrix(raw R, 10, 20, true); p
scan(50, i -> (rawSetMatrixEntry(p,random 10, random 20, rawFromNumber(raw R, random 1.0))))
p
map(R,rawMatrix p)

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

rawMatrixColumnOperation2(p,1,2,raw e,raw f,raw g,raw h,false)
p


---------------------------------------
-- mutable matrix routines ------------
---------------------------------------
needs "raw-util.m2"
R = polyring(rawZZ(), (symbol a .. symbol g))

r5 = rawMutableIdentity(R,5,true) -- mutable r5

  -----------------------
  -- rawMatrixEntry -----
  -----------------------
  rawSetMatrixEntry(r5,1,2,a+b)
  assert(rawMatrixEntry(r5,1,2) === a+b)

  rawSetMatrixEntry(r5,0,0,a^5)
  assert try(rawSetMatrixEntry(r5,5,5,a^5);false) else true
  rawSetMatrixEntry(r5,0,0,0_R)
  assert(rawMatrixEntry(r5,0,0) === 0_R)

  ----------------------
  -- rawMatrixRowSwap --
  ----------------------
  m = rawMatrix1(R^3, 4, (a,b,c,d,e,f,g,0_R,a+b,c+d,0_R,e+f), 0)
  m = rawMutableMatrix(m)
  rawMatrixRowSwap(m, 0,1)
  m
  rawMatrixRowSwap(m, 0,1)
  m0 = rawMatrix m
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
  -- rawInsertRows -----
  -- rawDeleteColumns --
  -- rawDeleteRows -----
  ----------------------
  M = mutableMatrix(map(ZZ^5, ZZ^7, (i,j) -> 3*i^3 + j^2 +3),Dense=>false)
  rawInsertColumns(raw M,3,4)
  M
  rawDeleteColumns(raw M,8,9)
  M

  rawInsertRows(raw M,5,6)
  M
  rawDeleteRows(raw M,1,1)
  M

  M = mutableMatrix(map(ZZ^5, ZZ^7, (i,j) -> 3*i^3 + j^2 +3),Dense=>true)
  rawInsertColumns(raw M,3,4)
  M
  rawDeleteColumns(raw M,8,9)
  M

  rawInsertRows(raw M,5,6)
  M
  rawDeleteRows(raw M,1,1)
  M

  rawDeleteColumns(raw M,3,6)
  M
  rawDeleteRows(raw M,4,9)
  M

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
  needs "raw-util.m2"
  R = RR_53
  m = mat table(4,4, (i,j) -> rawFromNumber(raw R, random 1.0))
  b0 = mat table(4,1, (i,j) -> rawFromNumber(raw R, random 1.0))
  A = rawMutableMatrix(m,true)
  b = rawMutableMatrix(b0,true)
  x = rawMutableMatrix(b0,true)
  p
  x = rawLinAlgSolve(A,b)
  x
  x1 = rawMatrix b
  m*(rawMatrix x)
  rawMinors(4,m,1,-1,null,null)
