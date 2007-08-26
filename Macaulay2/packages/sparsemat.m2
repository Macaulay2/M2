-- Mike will rewrite this package

new QQ := QQ -> QQ.pop()

-------------------------
-- SparseMutableMatrix --
-------------------------
-- Routines to access this engine data type
--

if SparseMutableMatrix === symbol SparseMutableMatrix
then SparseMutableMatrix = new Type of MutableHashTable


-------------------------------------------
-- Creation of new SparseMutableMatrix's --
-------------------------------------------

-- newSparseMatrix: the way to grab a SparseMutableMatrix from the engine.
newSparseMatrix = method()

newSparseMatrix Ring := (R) -> (
     M := new SparseMutableMatrix;
     M.handle = newHandle();
     M.ring = R;
     M)

sparseMutableMatrix = method()

sparseMutableMatrix(Ring,ZZ,ZZ) := (R,r,c) -> (
     sendgg(ggPush R, ggPush r, ggPush c, ggsparsematrix);
     newSparseMatrix R)

sparseMutableMatrix(Matrix) := (m) -> (
     sendgg(ggPush m, ggsparsematrix);
     newSparseMatrix ring m)

matrix(SparseMutableMatrix) := options -> (m) -> (
     sendgg(ggPush m, ggmatrix);
     getMatrix ring m)

-------------
-- Display --
-------------
net SparseMutableMatrix := f -> (
	  R := ring f;
	  m := stack toSequence apply(
	       lines sendgg(ggPush f,ggsee,ggpop), x -> concatenate("| ",x,"|"));
	  m)

SparseMutableMatrix#(Standard,AfterPrint) = SparseMutableMatrix#(Standard,AfterNoPrint) = f -> (
     R := ring f;
     << endl;				  -- double space
     << concatenate(interpreterDepth:"o") << lineNumber << " : Matrix";
     << " " << (expression R)^(numrows f) << " <--- " << (expression R)^(numcols f);
     << endl;
     )


numcols = method()
numcols SparseMutableMatrix := (m) -> (sendgg(ggPush m, ggnumcols); new ZZ)

numrows = method()
numrows SparseMutableMatrix := (m) -> (sendgg(ggPush m, ggnumrows); new ZZ)

----------------------
-- Special matrices --
----------------------

iden = (R,n) -> ( 
     sendgg(ggPush R, ggPush n, ggiden);
     newSparseMatrix R)

sparsemat = (n,m,percentagezero,maxentry) -> (
    M := sparseMutableMatrix(ZZ,n,m);
    randomentry := () -> (
        x := random 1.0;
        if x < percentagezero then 0
        else (random (2*maxentry)) - maxentry);
    scan(n, r-> scan(m, c-> (
	a := randomentry();
	if a != 0 then setEntry(M,r,c,a))));
    M)

------------------------------------------
-- Obtaining and setting matrix entries --
------------------------------------------

setEntry = (m,r,c,a) -> (
     sendgg(ggPush m, ggPush r, ggPush c, ggPush a, ggSetEntry);)

SparseMutableMatrix _ Sequence := (m,s) -> (
     if #s =!= 2 then error "expected row and column indices";
     r := s#0;
     c := s#1;
     if class r =!= ZZ or class c =!= ZZ then
         error "expected integer row and column indices";
     sendgg(ggPush m, ggPush r, ggPush c, ggelem);
     new ring m)

getEntry = method()
getEntry(SparseMutableMatrix,ZZ,ZZ) := (m,r,c) -> (
     sendgg(ggPush m, ggPush r, ggPush c, ggelem);
     new ring m)

-----------------------------------------
-- Row/Column change of basis matrices --
-----------------------------------------

getRowChange = (m) -> (sendgg(ggPush m, gggetRowChange); newSparseMatrix ring m)
getColumnChange = (m) -> (sendgg(ggPush m, gggetColChange); newSparseMatrix ring m)
setRowChange = (m,n) -> (sendgg(ggPush m, ggPush n, ggsetRowChange);)
setColumnChange = (m,n) -> (sendgg(ggPush m, ggPush n, ggsetColChange);)

-------------------------------
-- Row and column operations --
-------------------------------

rscale = (m,a,r) -> (sendgg(ggPush m, ggPush r, ggPush a, ggRowScale);)
cscale = (m,a,c) -> (sendgg(ggPush m, ggPush c, ggPush a, ggColumnScale);)

-- raxy = (m,r1,a,r) -> (sendgg(ggPush m, ggPush r1, ggPush a, ggPush r, ggRowAddMultiple);)
-- caxy = (m,c1,a,c) -> (sendgg(ggPush m, ggPush c1, ggPush a, ggPush c, ggColumnAddMultiple);)
raxy = (m,a,r1,r) -> (sendgg(ggPush m, ggPush r1, ggPush a, ggPush r, ggRowAddMultiple);)
caxy = (m,a,c1,c) -> (sendgg(ggPush m, ggPush c1, ggPush a, ggPush c, ggColumnAddMultiple);)

rflip = (m,i,j) -> (sendgg(ggPush m, ggPush i, ggPush j, ggRowInterchange);)
cflip = (m,i,j) -> (sendgg(ggPush m, ggPush i, ggPush j, ggColumnInterchange);)

creduce = (m,i,j) -> (sendgg(ggPush m, ggPush i, ggPush j, ggPush 0, ggreduce);)
cgcdreduce = (m,i,j) -> (sendgg(ggPush m, ggPush i, ggPush j, ggPush 1, ggreduce);)

csort = method()
csort(SparseMutableMatrix,ZZ,ZZ) := (m,lo,hi) -> (
     sendgg(ggPush m, ggPush lo, ggPush hi, ggsortcolumns))

--------------------
-- Multiplication --
--------------------

dot = (m,i,j) -> (
     sendgg(ggPush m, ggPush i, ggPush j, ggmult);
     new ring m)



findOne = (m,clo,chi) -> (
     sendgg(ggPush m, ggPush clo, ggPush chi, ggfindGoodUnitPivot);
     best := eePopInt();
     c := eePopInt();
     r := eePopInt();
     if c === -1 then false else {r,c,best})

-------------------------------
-- Harrison Tsai's routines  --
-------------------------------
reducePivots = method()
reducePivots SparseMutableMatrix := (m) -> (
     sendgg(ggPush m, ggreducepivots);
     )

reduceCompress = method()
reduceCompress(Matrix) := (m) -> (
     msparse := sparseMutableMatrix m;
     sendgg(ggPush msparse, ggreducepivots);
     mout := compress matrix msparse;
     colCounter := numgens source mout - 1;
     rowCounter := numgens target mout - 1;
     if (mout == id_(target mout)) then (mout = null)
     else (
     	  while (mout_colCounter == (id_(target mout))_rowCounter) do (
	       colCounter = colCounter - 1;
	       rowCounter = rowCounter - 1;
	       );
     	  mout = cokernel gens gb (mout_{0..colCounter})^{0..rowCounter};
	  );
     mout
     )
     
///
--------------------------
-- Test of this package --
--------------------------
load "sparsemat.m2"

-- First tests: test the sparse matrix routines on simple examples
-- including the row and column change matrices
-- Do this over several rings...

-- First ring: ZZ
  m = sparseMutableMatrix(ZZ,4,6)
  assert(ring m == ZZ)

  -- Set some of the entries in this matrix:
  setEntries = (m, p) -> scan(p, x -> setEntry splice(m,toSequence x))
  setEntries(m, {{0,0,1},{0,1,3},{1,1,67},{1,2,45},{0,4,12},{2,4,56},{2,5,-5},{3,5,1}})
  manswer =  matrix {{1, 3, 0, 0, 12, 0}, 
		     {0, 67, 45, 0, 0, 0}, 
		     {0, 0, 0, 0, 56, -5}, 
		     {0, 0, 0, 0, 0, 1}}
  assert(matrix m == manswer)

  -- Now get the entries back:
  scan(numrows m, r -> scan(numcols m, c -> assert(m_(r,c) == manswer_(r,c))))

  -- Set the row and column operation matrices
  mrows = iden(ZZ,4)  
  mcols = iden(ZZ,6)
  assert(matrix mrows == id_(ZZ^4))
  assert(matrix mcols == id_(ZZ^6))
  setRowChange(m,mrows)
  setColumnChange(m,mcols)
  see getRowChange m
  see getColumnChange m
  
  -- now do some row and column operations on m:
  rflip(m,0,1)
  rflip(m,0,1)
  assert(matrix m == manswer)
  assert(matrix mrows == id_(ZZ^4))
  assert(matrix mcols == id_(ZZ^6))

  cflip(m,1,4)
  cflip(m,1,4)
  assert(matrix m == manswer)
  assert(matrix mrows == id_(ZZ^4))
  assert(matrix mcols == id_(ZZ^6))

  -- raxy, caxy
  raxy(m,3,0,1)
  caxy(m,2,1,3)
  see m
  see mrows
  see mcols
  assert(transpose matrix mrows * manswer * matrix mcols == matrix m)
  caxy(m,-2,1,3)
  raxy(m,-3,0,1)
  assert(matrix m == manswer)
  assert(matrix mrows == id_(ZZ^4))
  assert(matrix mcols == id_(ZZ^6))

  -- scaling
  see m
  rscale(m,-1,0)
  cscale(m,-1,3)
  see m
  see mrows
  see mcols
  assert(transpose matrix mrows * manswer * matrix mcols == matrix m)
  rscale(m,-1,0)
  cscale(m,-1,3)
  assert(matrix m == manswer)
  assert(matrix mrows == id_(ZZ^4))
  assert(matrix mcols == id_(ZZ^6))

  -- creduce
  see m
  creduce(m,0,1)
  see m
  assert(transpose matrix mrows * manswer * matrix mcols == matrix m)
  see mrows    
  see mcols 
  
  -- cgcdreduce
  cgcdreduce(m,1,2)
  see m
  assert(transpose matrix mrows * manswer * matrix mcols == matrix m)
  see mcols
  
  -- csort
  see m  
  csort(m,0,5)
  see m
  assert(transpose matrix mrows * manswer * matrix mcols == matrix m)
  see mcols  

  -- saling with zero
  see m
  rscale(m,0,2)  -- BUG
  see m

  cscale(m,0,1)
  see m
  assert(transpose matrix mrows * manswer * matrix mcols == matrix m)
  

  -- some pathological type things (setting elements to 0, row/column out of range)
  assert(try (rscale(m,1,-1);false) else true)
  assert(try (rscale(m,1,4);false) else true)
  assert(try (cscale(m,1,-1);false) else true)
  assert(try (cscale(m,1,6);false) else true)
  see m

-- Test: Hermite normal form, and multipliers
  m = matrix{{1,1,2},{3,3,4}}
  hermite m
  mh = gethermite m
  mc = gethermiteRows m
  assert(mh == m * mc)

  m = matrix{{1,1,1,1},{0,1,2,3}}  
  hermite m
  mh = gethermite m
  mc = gethermiteRows m
  assert(mh == m * mc)

  -- try random matrices.  First a small one:
  m = matrix sparsemat(10,15,.5,5)
  time hermite m
  mh = gethermite m
  mc = gethermiteRows m
  assert(mh == m * mc)

  m = matrix sparsemat(40,40,.5,5)
  time hermite m
  mh = gethermite m
  mc = gethermiteRows m;
  assert(mh == m * mc)

  -- Test from Havas et al paper 1998:
  m = map(ZZ^10, ZZ^10, (j,i) -> (i+1)^3 * (10-j)^2 + i + (10-j) + 1)
  time hermite m
  mh = gethermite m
  mc = gethermiteRows m;
  assert(mh == m * mc)
  m
  time answer = hermiteLLL m
  time gens gb m
  time syz m
---------------------------------------------------------------
--- Below this line: not incorporated into final tests yet. ---
---------------------------------------------------------------

setEntry(m,2,3,5423532); see m
a = m_(2,3)
assert(a == 5423532)

R = ZZ[vars(0..23)]
m1 = sparseMutableMatrix(R,4,6);see m1
assert(ring m1 == R)
scan(0..3, r-> scan(0..5, c-> setEntry(m1,r,c,R_(4*c+r))))

see m1;cflip(m1,1,2);see m1
cscale(m1,a^10,1);see m1
caxy(m1,a,4,3);see m1

rscale(m1,10_R,1);see m1
raxy(m1,a,2,3);see m1


-- Test 2
makeTestMatrix = () -> (
  m := sparseMutableMatrix(ZZ,3,4);
  setEntry(m,1,1,10);
  setEntry(m,1,2,7);
  setEntry(m,2,2,5);
  setEntry(m,2,3,8);
  m)
-- test column reduction:
m = makeTestMatrix()
creduce(m,2,3)
see m 

m = makeTestMatrix()
creduce(m,2,1)
see m

m = makeTestMatrix()
creduce(m,1,2)
see m
creduce(m,2,1)
see m
rflip(m,1,2)
see m
creduce(m,2,1)
see m

-- Test 3
m = sparsemat(10,10,.9,5)

see m
rflip(m,2,9); see m
cscale(m,-1,9); see m
creduce(m,9,4); see m
caxy(m,-2,9,4); see m
cgcdreduce(m,9,4); see m
rflip(m,0,9); see m
cgcdreduce(m,4,5); see m
cgcdreduce(m,4,9); see m


-- Test 4
-- Test of row and column change matrices
m = sparsemat(4,5,.5,5)
mr = iden(ZZ,4)
mc = iden(ZZ,5)
setRowChange(m,mr)
setColumnChange(m,mc)

see getRowChange m
see getColumnChange m

rscale(m,123,3)
raxy(m,41,1,2)
rflip(m,1,2)

-- Test 5: Partial Smith normal form
m = sparsemat(10,10,.5,5)
  -- 1 in (9,8)
  co(m,10,10,9,8)
  -- 1 in (3,1)
  co(m,9,9,3,1)
  -- 1 in (7,7)
  co(m,8,8,7,7)
  -- 3,4 in columns 4,6
  gc(m,6,4)
  co(m,7,7,6,6)
  -- row 4 has gcd 1:
  rflip(m,4,5)
  gc(m,5,4)
  co(m,6,6,5,5)
  -- now the numbers are getting larger
  gc(m,4,2)
  co(m,5,5,4,4)
  -- still larger, although largest is only 1.3 mil.
  gc(m,3,1)
  gc(m,3,2)
  co(m,4,4,3,3)
  -- oops, now coefficients are getting larger and larger,
  -- no sparsity left. (actually, none for a while now)
  gc(m,2,1)
  co(m,3,3,2,2)
  -- only 2 by 2 left now:
  gc(m,1,0)
  
-- 
n = 10
m = sparsemat(n,n,.5,5)
mm = matrix m
time gens gb mm;
findOne(m,0,9)
     

n = 10
m = sparsemat(n,n,.5,5)
x = findOne(m,0,9)
co(m,10,10,x#0,x#1)
x = findOne(m,0,8)
co(m,9,9,x#0,x#1)
x = findOne(m,0,7)
co(m,8,8,x#0,x#1)
x = findOne(m,0,6)
co(m,7,7,x#0,x#1)
x = findOne(m,0,5)

-- 
n = 40
m = sparsemat(n,n,.5,5)
s = n
x = findOne(m,0,s-1)
while x =!= false do (co(m,s,s,x#0,x#1); s=s-1; x = findOne(m,0,s-1))

n = 10
m = sparsemat(n,n,.5,5)
s = n
do1 = () -> (x = findOne(m,0,s-1); if x =!= false then (co2(m,s,s,x#0);s=s-1) else false)
do2 = () -> (co2(m,s,s,s-1); s=s-1)
do3 = () -> (co3(m,s,s,s-1); s=s-1)

n = 100
m = sparsemat(n,n,.9,5)
s = n
x = findOne(m,0,s-1)

while x =!= false do (co(m,s,s,x#0,x#1); s=s-1; x = findOne(m,0,s-1))

-- Test
R = ZZ/7[x]
m = (x+1)^3 * id_(R^5)
ms = sparseMutableMatrix m
R = ZZ[vars(0..24)]
m = genericMatrix(R,a,4,6)
ms = sparseMutableMatrix m
matrix ms

-- Test 'Smith' normal form:
n = 45
m = sparsemat(n,n,.5,5)
mm = matrix m
gbTrace = 1
time smith mm;
time gens gb mm;

-- Test 'Smith' normal form:
n = 100
m = sparsemat(n,n,.9,1)
mm = matrix m
gbTrace = 1
time smith mm;
time gens gb mm;

-- 
n = 10
m = sparsemat(n,n,.1,5)
mm = matrix m
dot(m,7,9)
dot(m,9,9)

-- Gram-Schmidt
m = matrix{{1,0,0},{0,1,0},{0,1,1}}
isLLL m
integralLLL(sparseMutableMatrix m, 3)
isLLL oo
n = 5
m = sparsemat(n,n,.1,5)
mm = matrix m
gram mm
isLLL mm
integralLLL(m,5)
-- LLL test:
mmm = sparsemat(4,2,.1,5)


-- Test over ZZ/32003[x]
R = ZZ/32003[x]
sparsem = (R,x,m,n,deg) -> (
     sum(deg+1, d -> x^d ** random(R^m,R^n)))
m = sparsem(R,x,4,3,3)
smith m
getsmith m
///     
