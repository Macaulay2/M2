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

--AfterPrint SparseMutableMatrix := AfterNoPrint SparseMutableMatrix := f -> (
--     R := ring f;
--     << endl;				  -- double space
--     << "o" << lineNumber() << " : Matrix";
--     << " " << (expression R)^(numrows f) << " <--- " << (expression R)^(numcols f);
--     << endl;
--     )


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
     R := ring m;
     msparse := sparseMutableMatrix m;
     sendgg(ggPush msparse, ggreducepivots);
     mout := compress matrix msparse;
     colCounter := numgens source mout - 1;
     rowCounter := numgens target mout - 1;
     --if (mout == id_(target mout)) then (mout = null)
     --else (
     while (rowCounter >= 0 and colCounter >= 0 and
	  mout_colCounter == (id_(target mout))_rowCounter) do (
	  colCounter = colCounter - 1;
	  rowCounter = rowCounter - 1;
	  );
     if (rank source mout == 0) then mout = mout
     else if (colCounter == -1 and rowCounter == -1) then mout = gens R^0
     else (
	  if (colCounter == -1) then colCounter = 0;
	  if (rowCounter == -1) then rowCounter = 0;
	  mout = compress mout_{0..colCounter}^{0..rowCounter};
	  );
     mout
     )
