--		Copyright 2005, 2008 by Daniel R. Grayson and Michael E. Stillman

needs "matrix.m2"

MutableMatrix = new Type of HashTable
MutableMatrix.synonym = "mutable matrix"
raw MutableMatrix := m -> m.RawMutableMatrix
ring MutableMatrix := m -> m.Ring
MutableMatrix == ZZ := (m,i) -> raw m == i
ZZ == MutableMatrix := (i,m) -> raw m == i
entries MutableMatrix := m -> (
     R := ring m;
     applyTable(entries raw m, r -> promote(r,R)))
toString MutableMatrix := m -> "mutableMatrix " | toString entries m
precision MutableMatrix := precision @@ ring
expression MutableMatrix := m -> MatrixExpression {applyTable(entries m, expression), mutable => true}
texMath MutableMatrix := m -> texMath expression m
net MutableMatrix := m -> net expression m

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
norm(InexactField,MutableMatrix) := (p,M) -> norm(numeric(precision M, p), M)
norm(MutableMatrix) := (M) -> new RR from norm(numeric(precision M,infinity),raw M)

mutableIdentity = method(Options => {Dense => true}, TypicalValue=>MutableMatrix)
mutableIdentity(Ring,ZZ) := o -> (R,nrows) -> 
  map(R,rawMutableIdentity(raw R,nrows,o.Dense))
mutableIdentity(RingFamily,ZZ) := o -> (R,nrows) -> mutableIdentity(default R,nrows,o)

MutableMatrix _ Sequence := (m,rc) -> (
     n := (raw m)_rc;
     if class n === RawRingElement then promote(n,ring m)
     else if class n === RawMutableMatrix then map(ring m, n)
     else error "internal error"
     )
MutableMatrix == MutableMatrix := (m,n) -> raw m == raw n

- MutableMatrix := (m) -> map(ring m, - raw m)
MutableMatrix + MutableMatrix := (m,n) -> map(ring m, raw m + raw n)
MutableMatrix - MutableMatrix := (m,n) -> map(ring m, raw m - raw n)
MutableMatrix * MutableMatrix := (m,n) -> map(ring m, raw m * raw n)
RingElement * MutableMatrix := (f,n) -> map(ring f, raw f * raw n)
MutableMatrix * RingElement := (n,f) -> map(ring f, raw n * raw f)
ZZ * MutableMatrix := (f,n) -> map(ring f, raw (f_(ring n)) * raw n)
MutableMatrix * ZZ := (n,f) -> map(ring f, raw n * raw (f_(ring n)))
RR * MutableMatrix := (f,n) -> map(ring f, raw (f_(ring n)) * raw n)
MutableMatrix * RR := (n,f) -> map(ring f, raw n * raw (f_(ring n)))

MutableMatrix _ Sequence = (M,ij,val) -> (
     val = promote(val,ring M);
     (raw M)_ij = raw val; 
     val)

transpose MutableMatrix := (f) -> map(ring f, rawDual raw f)

lift(MutableMatrix,InexactNumber) := opts -> (M,RR) -> lift(M,default RR,opts)
lift(MutableMatrix,InexactNumber') :=
lift(MutableMatrix,RingElement) := 
lift(MutableMatrix,Number) := Matrix => opts -> (f,S) -> (
     R := ring f;
     if R === S then return f;
     lift(f, R, S, opts))     

promote(MutableMatrix,InexactNumber) := (M,RR) -> promote(M,default RR)
promote(MutableMatrix,InexactNumber') :=
promote(MutableMatrix,RingElement) := 
promote(MutableMatrix,Number) := Matrix => (f,S) -> (
     R := ring f;
     if R === S then return f;
     promote(f, R, S))

--------------------------------
-- submatrices -----------------
--------------------------------
listZ := v -> ( if not all(v,i -> instance(i, ZZ)) then error "expected list of integers"; v )
MutableMatrix _ List := Matrix => (f,v) -> submatrix(f,listZ splice v)	-- get some columns
MutableMatrix ^ List := Matrix => (f,v) -> submatrix(f,listZ splice v,) -- get some rows
submatrix(MutableMatrix,VisibleList,VisibleList) := (m,rows,cols) -> map(ring m,rawSubmatrix(raw m, listZ toList splice rows, listZ toList splice cols))
submatrix(MutableMatrix,VisibleList            ) := (m,cols     ) -> map(ring m,rawSubmatrix(raw m, listZ toList splice cols))
submatrix(MutableMatrix,Nothing    ,VisibleList) := (m,null,cols) -> submatrix(m,cols)
submatrix(MutableMatrix,VisibleList,Nothing    ) := (m,rows,cols) -> (
     rows = splice rows; 
     map((ring m, rawSubmatrix(raw m, listZ toList rows, 0 .. numColumns m - 1))))
--------------------------------
numRows(RawMutableMatrix) := (m) -> rawNumberOfRows m
numRows(MutableMatrix) := (m) -> rawNumberOfRows raw m

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
rowMult(MutableMatrix,ZZ,RingElement) := 
rowMult(MutableMatrix,ZZ,Number) := (A,r,f) -> (
     f = promote(f,ring A);
     rawMatrixRowScale(raw A,raw f,r,false);
     A)

columnMult = method()
columnMult(MutableMatrix,ZZ,Number) :=
columnMult(MutableMatrix,ZZ,RingElement) := (A,r,f) -> (
     f = promote(f,ring A);
     rawMatrixColumnScale(raw A,raw f,r,false);
     A)

rowAdd = method()
rowAdd(MutableMatrix,ZZ,Number,ZZ) :=
rowAdd(MutableMatrix,ZZ,RingElement,ZZ) := (A,r,f,i) -> (
     f = promote(f,ring A);
     rawMatrixRowChange(raw A,r,raw f,i,false);
     A)

columnAdd = method()
columnAdd(MutableMatrix,ZZ,Number,ZZ) :=
columnAdd(MutableMatrix,ZZ,RingElement,ZZ) := (A,r,f,i) -> (
     f = promote(f,ring A);
     rawMatrixColumnChange(raw A,r,raw f,i,false);
     A)

fillMatrix = method( Options => { 
	  UpperTriangular => false, 
	  Density => 1.0,
	  Height => 10
	  } )
fillMatrix(MutableMatrix) := opts -> M -> (
     s := randomHeight;
     randomHeight = opts.Height;			    -- silly global variable
     rawMutableMatrixFillRandomDensity(raw M,opts.Density,if opts.UpperTriangular then 1 else 0);
     randomHeight := s;
     M)
fillMatrix(MutableMatrix,ZZ) := opts -> (M,n) -> (
     s := randomHeight;
     randomHeight = opts.Height;			    -- silly global variable
     rawMutableMatrixFillRandom(raw M,n); 
     randomHeight := s;
     M)

randomMutableMatrix = method(Options=>{Dense=>false}, TypicalValue=>MutableMatrix)

randomMutableMatrix(ZZ,ZZ,RR,ZZ) := options -> (n,m,percentagezero,maxentry) -> (
    M := mutableMatrix(ZZ,n,m,options);
    randomentry := () -> (
        x := random 1.0;
        if x < percentagezero then 0
        else (random (2*maxentry+1)) - maxentry);
    scan(n, r-> scan(m, c-> (
	a := randomentry();
	if a != 0 then M_(r,c) = a)));
    M)

LUdecomposition = method()
LUdecomposition MutableMatrix := (A) -> (
     if not isField ring A then
       error("LU not implemented over ring " | toString ring A);
     nrows := rawNumberOfRows raw A;
     L := mutableMatrix(ring A,0,0,Dense=>true);
     U := mutableMatrix(ring A,0,0,Dense=>true);
     p := rawLU(raw A, raw L, raw U);
     (p, L, U))
LUdecomposition Matrix := (A) -> (
     (p,L,U) := LUdecomposition mutableMatrix A;
     (p, matrix L,matrix U))

solve = method(Options => { ClosestFit => false,
	                    MaximalRank => false,
			    Precision=>0,
			    Invertible=>false })

solve(MutableMatrix,MutableMatrix) := opts -> (A,b) -> (
     R := ring A;
     if not isField R then
       error("solve not implemented over ring " | toString ring A);
     if opts.ClosestFit then (
         if (opts#Precision !=0) then (
		     A=mutableMatrix(promote(matrix(A), CC_(opts#Precision)));
		     b=mutableMatrix(promote(matrix(b), CC_(opts#Precision)))
	         );
         x := mutableMatrix(ring A,0,0,Dense=>true);
         rawLeastSquares(raw A,raw b,raw x,opts.MaximalRank);
         x)
     else (
         ans := if opts.Invertible then
                    rawLinAlgSolveInvertible(raw A, raw b)
                else
                    rawLinAlgSolve(raw A, raw b);
         if ans === null then null else map(R, ans)
         )
     )

solve(Matrix,Matrix) := opts -> (A,b) -> (
     if not isBasicMatrix A or not isBasicMatrix b then
       error "expected matrices between free modules";
     if ultimate(coefficientRing, ring A) === ZZ then (
         return (b // A);
        );
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
     new VerticalList from flatten entries e)
eigenvalues(Matrix) := o -> (A) -> eigenvalues(mutableMatrix(numeric A,Dense=>true),o)

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
     (new VerticalList from flatten entries e,v))
eigenvectors(Matrix) := o -> (A) -> (
     (e,v) := eigenvectors(mutableMatrix(numeric A,Dense=>true),o);
     (e, matrix v))

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
     (VerticalList flatten entries matrix Sigma,matrix U,matrix VT))

QRDecomposition = method()
QRDecomposition MutableMatrix := A -> (
     k := ring A;
     if k =!= RR_53 then error "currently, QRDecomposition is only defined for matrices over RR_53";
     Q := mutableMatrix(k,0,0,Dense=>true);
     R := mutableMatrix(k,0,0,Dense=>true);
     rawQR(raw A, raw Q, raw R, true -* ReturnQR was a bad option name *- );
     (Q,R))
QRDecomposition Matrix := A -> (
     k := ring A;
     if k =!= RR_53 then error "currently, QRDecomposition is only defined for matrices over RR_53";
     A = mutableMatrix(A,Dense=>true);
     (Q,R) := QRDecomposition A;
     (matrix Q,matrix R))

rank MutableMatrix := (M) -> (
    if isField ring M then
      rawLinAlgRank raw M
    else
      rank matrix M
    )

determinant MutableMatrix := opts -> (M) -> (
    if numRows M =!= numColumns M then error "expected a square matrix";
    if isField ring M then
      promote(rawLinAlgDeterminant raw M, ring M)
    else
      determinant matrix M
    )

inverse MutableMatrix := (M) -> (
     if numRows M =!= numColumns M then error "expected square matrix";
     if isField ring M then
       map(ring M, rawLinAlgInverse raw M)
     else
       mutableMatrix inverse matrix M
     )

nullSpace = method()
nullSpace(MutableMatrix) := (M) -> map(ring M, rawLinAlgNullSpace raw M)

MutableMatrix ^ ZZ := (A, r) -> (
     if r == 0 then 
       return mutableIdentity(ring A, numRows A);
     if r < 0 then (
	  r = -r;
	  A = inverse A;
	  );
     result := A;
     if r > 1 then for i from 2 to r do result = result * A;
     result     
     )

rowRankProfile = method()
rowRankProfile MutableMatrix := (A) -> rawLinAlgRankProfile(raw A, true)

columnRankProfile = method()
columnRankProfile MutableMatrix := (A) -> rawLinAlgRankProfile(raw A, false)

reducedRowEchelonForm = method()
reducedRowEchelonForm Matrix := (M) -> (
    matrix reducedRowEchelonForm(mutableMatrix M)
    )
reducedRowEchelonForm MutableMatrix := (M) -> map(ring M, rawLinAlgRREF raw M)
     
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
