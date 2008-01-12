--		Copyright 2005, 2008 by Daniel R. Grayson and Michael E. Stillman

MutableMatrix = new Type of HashTable
MutableMatrix.synonym = "mutable matrix"
raw MutableMatrix := m -> m.RawMutableMatrix
ring MutableMatrix := m -> m.Ring
MutableMatrix == ZZ := (m,i) -> raw m == i
ZZ == MutableMatrix := (i,m) -> raw m == i
entries MutableMatrix := m -> (
     R := ring m;
     applyTable(entries raw m, r -> new R from r))
toString MutableMatrix := m -> "mutableMatrix " | toString entries m
net MutableMatrix := m -> (
     m = raw m;
     if m == 0 then return "0";
     stack toSequence apply(lines toString m, x -> concatenate("| ",x,"|")))
map(Ring,RawMutableMatrix) := opts -> (R,m) -> (
     new MutableMatrix from {
	  symbol Ring => R,
	  symbol RawMutableMatrix => m,
	  symbol cache => new CacheTable
	  }
     )
new Matrix from MutableMatrix := (typeofMatrix       ,m) -> map(ring m,rawMatrix        raw m)
new MutableMatrix from Matrix := (typeofMutableMatrix,m) -> map(ring m,rawMutableMatrix raw m)

mutableMatrix = method(Options => {Dense => true}, TypicalValue=>MutableMatrix)
mutableMatrix Matrix := o -> m -> map(ring m, rawMutableMatrix(raw m, o.Dense))
mutableMatrix List := o -> m -> (m1 := matrix m; map(ring m1, rawMutableMatrix(raw m1, o.Dense)))
mutableMatrix MutableMatrix := o -> (m) -> map(ring m, rawMutableMatrix(raw m, o.Dense))
mutableMatrix(Ring,ZZ,ZZ) := o -> (R,nrows,ncols) -> map(R,rawMutableMatrix(raw R,nrows,ncols,o.Dense))
mutableMatrix(RingFamily,ZZ,ZZ) := o -> (R,nrows,ncols) -> mutableMatrix(default R,nrows,ncols,o)

matrix MutableMatrix := o -> m -> map(ring m, rawMatrix raw m)

clean(RR,MutableMatrix) := (epsilon,M) -> map(ring M, clean(epsilon,raw M))
norm(RR,MutableMatrix) := (p,M) -> new RR from norm(p,raw M)
norm(MutableMatrix) := (M) -> new RR from norm(numeric(precision M,infinity),raw M)

mutableIdentity = method(Options => {Dense => true}, TypicalValue=>MutableMatrix)
mutableIdentity(Ring,ZZ) := o -> (R,nrows) -> 
  map(R,rawMutableIdentity(raw R,nrows,o.Dense))
mutableIdentity(RingFamily,ZZ) := o -> (R,nrows) -> mutableIdentity(R,nrows,o)

MutableMatrix _ Sequence := (m,rc) -> (
     n := (raw m)_rc;
     if class n === RawRingElement then new ring m from n
     else if class n === RawMutableMatrix then map(ring m, n)
     else error "internal error"
     )
MutableMatrix == MutableMatrix := (m,n) -> raw m == raw n

- MutableMatrix := (m) -> map(ring m, - raw m)
MutableMatrix + MutableMatrix := (m,n) -> map(ring m, raw m + raw n)
MutableMatrix - MutableMatrix := (m,n) -> map(ring m, raw m - raw n)
MutableMatrix * MutableMatrix := (m,n) -> map(ring m, raw m * raw n)
RingElement * MutableMatrix := (f,n) -> map(ring f, raw f * raw n)

MutableMatrix _ Sequence = (M,ij,val) -> ((raw M)_ij = raw val; val)

numRows = method()
numRows(RawMutableMatrix) := (m) -> rawNumberOfRows m
numRows(MutableMatrix) := (m) -> rawNumberOfRows raw m

numColumns = method()
numColumns(RawMutableMatrix) := (m) -> rawNumberOfColumns m
numColumns(MutableMatrix) := (m) -> rawNumberOfColumns raw m

rowSwap = method()
rowSwap(RawMutableMatrix,ZZ,ZZ) := (m,i,j) -> (rawMatrixRowSwap(m,i,j);m)
rowSwap(MutableMatrix,ZZ,ZZ) := (m,i,j) -> (rawMatrixRowSwap(raw m,i,j);m)

columnSwap = method()
columnSwap(RawMutableMatrix,ZZ,ZZ) := (m,i,j) -> (rawMatrixColumnSwap(m,i,j);m)
columnSwap(MutableMatrix,ZZ,ZZ) := (m,i,j) -> (rawMatrixColumnSwap(raw m,i,j);m)

rowPermute = method()
rowPermute(RawMutableMatrix,ZZ,List) := (m,start,p) -> (rawPermuteRows(m,start,p);m)
rowPermute(MutableMatrix,ZZ,List) := (m,start,p) -> (rawPermuteRows(raw m,start,p);m)

columnPermute = method()
columnPermute(RawMutableMatrix,ZZ,List) := (m,start,p) -> (rawPermuteColumns(m,start,p);m)
columnPermute(MutableMatrix,ZZ,List) := (m,start,p) -> (rawPermuteColumns(raw m,start,p);m)

rowMult = method()
rowMult(MutableMatrix,ZZ,RingElement) := (A,r,f) -> (rawMatrixRowScale(raw A,raw f,r,false);A)
rowMult(MutableMatrix,ZZ,ZZ) := (A,r,f) -> (rawMatrixRowScale(raw A,raw f,r,false);A)
rowMult(MutableMatrix,ZZ,QQ) := (A,r,f) -> (rawMatrixRowScale(raw A,raw f,r,false);A)
rowMult(MutableMatrix,ZZ,RR) := (A,r,f) -> (rawMatrixRowScale(raw A,raw f,r,false);A)
rowMult(MutableMatrix,ZZ,CC) := (A,r,f) -> (rawMatrixRowScale(raw A,raw f,r,false);A)

columnMult = method()
columnMult(MutableMatrix,ZZ,RingElement) := (A,r,f) -> (rawMatrixColumnScale(raw A,raw f,r,false);A)
columnMult(MutableMatrix,ZZ,ZZ) := (A,r,f) -> (rawMatrixColumnScale(raw A,raw f,r,false);A)
columnMult(MutableMatrix,ZZ,QQ) := (A,r,f) -> (rawMatrixColumnScale(raw A,raw f,r,false);A)
columnMult(MutableMatrix,ZZ,RR) := (A,r,f) -> (rawMatrixColumnScale(raw A,raw f,r,false);A)
columnMult(MutableMatrix,ZZ,CC) := (A,r,f) -> (rawMatrixColumnScale(raw A,raw f,r,false);A)

rowAdd = method()
rowAdd(MutableMatrix,ZZ,RingElement,ZZ) := (A,r,f,i) -> (rawMatrixRowChange(raw A,r,raw f,i,false);A)
rowAdd(MutableMatrix,ZZ,ZZ,ZZ) := (A,r,f,i) -> (rawMatrixRowChange(raw A,r,raw f,i,false);A)
rowAdd(MutableMatrix,ZZ,QQ,ZZ) := (A,r,f,i) -> (rawMatrixRowChange(raw A,r,raw f,i,false);A)
rowAdd(MutableMatrix,ZZ,RR,ZZ) := (A,r,f,i) -> (rawMatrixRowChange(raw A,r,raw f,i,false);A)
rowAdd(MutableMatrix,ZZ,CC,ZZ) := (A,r,f,i) -> (rawMatrixRowChange(raw A,r,raw f,i,false);A)

columnAdd = method()
columnAdd(MutableMatrix,ZZ,RingElement,ZZ) := (A,r,f,i) -> (rawMatrixColumnChange(raw A,r,raw f,i,false);A)
columnAdd(MutableMatrix,ZZ,ZZ,ZZ) := (A,r,f,i) -> (rawMatrixColumnChange(raw A,r,raw f,i,false);A)
columnAdd(MutableMatrix,ZZ,QQ,ZZ) := (A,r,f,i) -> (rawMatrixColumnChange(raw A,r,raw f,i,false);A)
columnAdd(MutableMatrix,ZZ,RR,ZZ) := (A,r,f,i) -> (rawMatrixColumnChange(raw A,r,raw f,i,false);A)
columnAdd(MutableMatrix,ZZ,CC,ZZ) := (A,r,f,i) -> (rawMatrixColumnChange(raw A,r,raw f,i,false);A)

fillMatrix = method( Options => { UpperTriangular => false, Density => 1.0 } )
fillMatrix(MutableMatrix) := opts -> M -> (rawMutableMatrixFillRandomDensity(raw M,opts.Density,if opts.UpperTriangular then 1 else 0); M)
fillMatrix(MutableMatrix,ZZ) := opts -> (M,n) -> (rawMutableMatrixFillRandom(raw M,n); M)

randomMutableMatrix = method(Options=>{Dense=>false}, TypicalValue=>MutableMatrix)

randomMutableMatrix(ZZ,ZZ,RR,ZZ) := options -> (n,m,percentagezero,maxentry) -> (
    M := mutableMatrix(ZZ,n,m,options);
    randomentry := () -> (
        x := random 1.0;
        if x < percentagezero then 0
        else (random (2*maxentry)) - maxentry);
    scan(n, r-> scan(m, c-> (
	a := randomentry();
	if a != 0 then M_(r,c) = a)));
    M)

LU = method()
LU MutableMatrix := (A) -> (
     nrows := rawNumberOfRows raw A;
     L := mutableMatrix(ring A,0,0,Dense=>true);
     U := mutableMatrix(ring A,0,0,Dense=>true);
     p := rawLU(raw A, raw L, raw U);
     (p, L, U))
LU Matrix := (A) -> (
     (p,L,U) := LU mutableMatrix A;
     (p, matrix L,matrix U))

solve = method(Options => { ClosestFit => false, MaximalRank => false })
solve(MutableMatrix,MutableMatrix) := opts -> (A,b) -> (
     x := mutableMatrix(ring A,0,0,Dense=>true);
     if opts.ClosestFit
     then rawLeastSquares(raw A,raw b,raw x,opts.MaximalRank)
     else rawSolve(raw A,raw b,raw x);
     x)
solve(Matrix,Matrix) := opts -> (A,b) -> (
     matrix solve(mutableMatrix(A,Dense=>true),
                  mutableMatrix(b,Dense=>true),
		  opts))

eigenvalues = method(Options => {Hermitian => false})
eigenvalues(MutableMatrix) := o -> (A) -> (
     k := ring A;
     if not instance(k,InexactField) then error "eigenvalues requires matrices over RR or CC";
     e := if o.Hermitian 
          then mutableMatrix(RR_(k.precision),0,0)
          else mutableMatrix(CC_(k.precision),0,0);
     rawEigenvalues(raw A,raw e,o.Hermitian);
     e)
eigenvalues(Matrix) := o -> (A) -> (
     matrix eigenvalues(mutableMatrix(A,Dense=>true),o))

eigenvectors = method(Options => {Hermitian => false})
eigenvectors(MutableMatrix) := o -> (A) -> (
     k := ring A;
     if not instance(k,InexactField) then error "eigenvalues requires matrices over RR or CC";
     e := if o.Hermitian 
          then mutableMatrix(RR_(k.precision),0,0, Dense=>true)
          else mutableMatrix(CC_(k.precision),0,0, Dense=>true);
     v := if instance(k,RealField) and o.Hermitian
          then mutableMatrix(RR_(k.precision),0,0, Dense=>true)
	  else mutableMatrix(CC_(k.precision),0,0, Dense=>true);
     rawEigenvectors(raw A,raw e,raw v,o.Hermitian);
     (e,v))
eigenvectors(Matrix) := o -> (A) -> (
     (e,v) := eigenvectors(mutableMatrix(A,Dense=>true),o);
     (matrix e, matrix v))

SVD = method(Options=>{DivideConquer=>false})
SVD MutableMatrix := o -> A -> (
     k := ring A;
     if not instance(k,InexactField) then error "eigenvalues requires matrices over RR or CC";
     Sigma := mutableMatrix(RR_(k.precision),0,0,Dense=>true);
     U := if instance(k,RealField) then mutableMatrix(RR_(k.precision),0,0) else mutableMatrix(CC_(k.precision),0,0,Dense=>true);
     VT := if instance(k,RealField) then mutableMatrix(RR_(k.precision),0,0) else mutableMatrix(CC_(k.precision),0,0,Dense=>true);
     rawSVD(raw A, raw Sigma, raw U, raw VT, o.DivideConquer);
     (Sigma,U,VT))
SVD Matrix := o -> A -> (
     k := ring A;
     if not instance(k,InexactField) then error "eigenvalues requires matrices over RR or CC";
     A = mutableMatrix(A,Dense=>true);
     (Sigma,U,VT) := SVD(A,o);
     (matrix Sigma,matrix U,matrix VT))
     
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
