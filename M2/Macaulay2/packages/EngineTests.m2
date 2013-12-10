newPackage(
        "EngineTests",
        Version => "0.1", 
        Date => "29 Aug 2011",
	    Authors => {
            {Name => "Michael E. Stillman", 
		        Email => "mike@math.cornell.edu", 
		        HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"},
	        {Name => "Jakob Kroeker", 
		        Email => "Jakob Kröker <kroeker@math.uni-hannover.de>", 
		        HomePage => "" }
            },
        Headline => "a test suite for the Macaulay2 engine",
	    PackageExports => {"FastLinearAlgebra"},
        DebuggingMode => true
        )

export { jordanForm,
    ringOps, 
    testMutableMatrices,
    testFrac,
    testGF,
    maxFLINTPrime,
    maxFFPACKPrime,
    testDeterminant,
    testMult,
    testInverse,
    testNullspace,
    testRank,
    testRankProfile,
    testSolve,
    testLinearAlgebra,
    testPromote,
    testClean,
    testNorm,
    hasFlint,
    hasFFPACK
    }

maxFLINTPrime = 18446744073709551521
maxFFPACKPrime = 33554393

debug Core
hasFlint = try (ZZp(101, Strategy=>"FLINT"); true) else false;
hasFFPACK = try (ZZp(101, Strategy=>"FFPACK"); true) else false;

--load (EngineTests#"source directory"|"EngineTests/test-gbZZ.m2")
--load (EngineTests#"source directory"|"EngineTests/test-linalg.m2")

jordanBlock = (R, diag, n) -> (
    m := mutableMatrix(R,n,n);
    for i from 0 to n-1 do m_(i,i) = diag;
    for i from 0 to n-2 do m_(i,i+1) = 1;
    m
    )
jordanForm = method()
jordanForm(Ring, List) := (R, L) -> (
    -- L is a list of (eigenvalue, size)
    directSum apply(L, x -> matrix jordanBlock(R, x#0, x#1))
    )

changeOfBasis = method()
changeOfBasis(Ring, ZZ) := (R,n) -> (
    -- returns a pair of matrices S, S^-1
    S := random(R^n, R^n);
    S1 := mutableMatrix S;
    Sinv := inverse S1;
    (S1, Sinv)
    )

checkLU = method()
checkLU(List,Matrix,Matrix) := (P,L,U) -> (
     R := ring L;
     Q := id_(R^#P) _ P;
     Q*L*U)
checkLU(List,MutableMatrix,MutableMatrix) := (P,L,U) -> (
     R := ring L;
     Q := mutableMatrix(id_(R^#P) _ P);
     Q*L*U)
--checkLU Matrix := (M) -> norm (checkLU time LUdecomposition M - M)

beginDocumentation()

doc ///
Key
  EngineTests
Headline
  a test suite for the Macaulay2 engine
Description
  Text
Caveat
SeeAlso
///

///
testFracRaw = () -> (
     debug Core;
     K = rawFractionOverNoetherNormalization(raw R, 1);
     f = (raw 1_K) / (raw x);
     g = 1_K / y;
     assert(f + g == (raw (x+y)) / raw (x*y));
     )
testFrac = () -> (
     R = ZZ/101[y,x,MonomialOrder=>{1,1}]/(y^3-x^2*y^2-x-3);
     K = frac R;
     assert(1/x + 1/y == (x+y)/(x*y));
     assert(1/x - 1/y == (-x+y)/(x*y));
     1/y;
     1(y-x);
     x/y^3;
     )
///

--load (EngineTests#"source directory"|"EngineTests/test-tower-rings.m2")

-- Tests of mutable matrix operations over ZZ/p

-- Test row and column operations on dense and sparse mutable matrices,
-- over the rings: ZZ, ZZ/p, QQ, QQ[x,y,z], RRR, CCC, frac QQ[x,y]

--TODO ringOpsZZp
ringOpsZZp = (p) -> (
     -- test basic arithmetic over ZZ/p
     -- we do this by doing operations on 1x1 mutable matrices.
     -- 
     kk := ZZ/p;
     -- test: from_int
     -- test: addition, subtraction, negation
     -- test: subtract_multiple
     -- test: multiplication
     -- test: powers (can't do yet?)
     -- test: set_from_mpz, set_from_mpq
     -- test: invert, divide (not possible yet?)
     -- test: discreteLog
     -- test? syzygy, elem_text_out, text_out, compare_elems
     -- what is the primitive root? characteristic?
     for i from 0 to p-1 do
       for j from 0 to p-1 do (
	    -- check that i+j mod p == i_kk + j_kk
	    a := mutableMatrix(kk,1,1); a_(0,0) = i_kk;
	    b := mutableMatrix(kk,1,1); b_(0,0) = i_kk;
	    )
     )

-----------------------------------------------------------------
-- Test of MutableMatrix elementary row and column operations ---
-----------------------------------------------------------------
testops = (R) -> (
  << "testops..." << endl;
  -- tests: numRows, numColumns, ==, 
  -- rowSwap, columnSwap, 
  -- rowAdd, columnAdd
  -- rowMult, columnMult
  -- rowPermute,columnPermute (TODO: make sure these are correct, not just the same for dense and sparse)
  m := mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
  assert(numRows m == 5);
  assert(numColumns m == 6);
  --
  m1 := matrix rowSwap(m, 1,2);
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
  m2 := matrix rowSwap(m, 1,2);
  assert(m1 == m2);
  --
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
  m1 = matrix columnSwap(m, 1,2);
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
  m2 = matrix columnSwap(m, 1,2);
  assert(m1 == m2);
  --
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
  m1 = matrix rowAdd(m, 1,-13,3);
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
  m2 = matrix rowAdd(m, 1,-13,3);
  assert(m1 == m2);
  --
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
  m1 = matrix columnAdd(m, 1,-13,3);
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
  m2 = matrix columnAdd(m, 1,-13,3);
  assert(m1 == m2);
  --
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
  m1 = matrix rowMult(m, 1, 14);
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
  m2 = matrix rowMult(m, 1, 14);
  assert(m1 == m2);
  --
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
  m1 = matrix columnMult(m, 1, 14);
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
  m2 = matrix columnMult(m, 1, 14);
  assert(m1 == m2);
  --
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
  m1 = matrix columnPermute(m,1,{2,0,1});
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
  m2 = matrix columnPermute(m,1,{2,0,1});  
  assert(m1 == m2);
  --
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
  m1 = matrix rowPermute(m,1,{2,0,1});  
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
  m2 = matrix rowPermute(m,1,{2,0,1});  
  assert(m1 == m2);
  )

debug Core
testops0 = (R) -> (
    << "testops0..." << endl;
    -- test whether operations work on matrices with 0x0 matrix
    -- also rx0 and 0xr
    m1 := mutableMatrix(R, 0, 0);
    assert(numColumns m1 == 0);
    assert(numRows m1 == 0);
    assert(m1 == 0);
    assert(rawIsZero raw m1);
    assert(m1 == m1);
    m2 := m1 + m1;
    assert(m2 == m1);
    m2 = m1-m1;
    assert(m2==m1);
    rawInsertColumns(raw m1,0,1);
    assert((numRows m1, numColumns m1) == (0,1));
    rawInsertRows(raw m1,0,2);
    assert((numRows m1, numColumns m1) == (2,1));
    )

debug Core
testops2 = (R) -> (
  -- testing:
  --   rawInsertRows, rawInsertColumns
  --   rawDeleteRows, rawDeleteColumns
  << "testops2..." << endl;  
  -- do the same operations on sparse and dense matrices, the answers should be the same
  nrows := 5;
  ncols := 6;
  E := map(R^nrows,R^ncols, (i,j) -> 100*i+j);
  for c from 0 to ncols do (
      m := mutableMatrix(E, Dense=>false);
      assert(not rawMutableMatrixIsDense(raw m));
      rawInsertColumns(raw m,c,2);
      m1 := matrix m;
      m = mutableMatrix(E, Dense=>true);
      assert(rawMutableMatrixIsDense(raw m));
      rawInsertColumns(raw m,c,2);
      m2 := matrix m;
      assert(m1 == m2);
      );
  --
  for r from 0 to nrows-1 do (
      m := mutableMatrix(E, Dense=>false);
      rawInsertRows(raw m,r,2);
      m1 := matrix m;
      m = mutableMatrix(E, Dense=>true);
      rawInsertRows(raw m,r,2);
      m2 := matrix m;
      assert(m1 == m2);
      );
  -- delete using (pfirst, plast), where 0 <= pfirst <= plast < ncols
  for p in subsets(splice{0..ncols}, 2) do (
      pfirst := p#0;
      plast := p#1 - 1;
      m := mutableMatrix(E, Dense=>false);
      rawDeleteColumns(raw m,pfirst, plast);
      m1 := matrix m;
      m = mutableMatrix(E, Dense=>true);
      rawDeleteColumns(raw m,pfirst, plast);
      m2 := matrix m;
      assert(m1 == m2);
      );
  -- delete using (pfirst, plast), where 0 <= pfirst <= plast < nrows
  for p in subsets(splice{0..nrows}, 2) do (
      pfirst := p#0;
      plast := p#1 - 1;
      m := mutableMatrix(E, Dense=>false);
      rawDeleteRows(raw m,pfirst,plast);
      m1 := matrix m;
      m = mutableMatrix(E, Dense=>true);
      rawDeleteRows(raw m,pfirst,plast);
      m2 := matrix m;
      assert(m1 == m2);
      )
  )

testops3 = (R) -> (
  << "testops3..." << endl;
  -- testing:
  -- rawMatrixRowOperation2, rawMatrixColumnOperation2
  -- rawSortColumns2, 
  -- rawColumnDotProduct
  m := mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
  rawMatrixColumnOperation2(raw m, 1, 2, raw promote(1,R), 
       raw promote(-1,R), 
       raw promote(2,R), 
       raw promote(5,R),
       false);
  m1 := matrix m;
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
  rawMatrixColumnOperation2(raw m, 1, 2, raw promote(1,R), 
       raw promote(-1,R), 
       raw promote(2,R), 
       raw promote(5,R),
       false);
  m2 := matrix m;
  assert(m1 == m2);
  -- rawMatrixRowOperation2
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
  rawMatrixRowOperation2(raw m, 1, 2, raw promote(1,R), 
       raw promote(-1,R), 
       raw promote(2,R), 
       raw promote(5,R),
       false);
  m1 = matrix m;
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
  rawMatrixRowOperation2(raw m, 1, 2, raw promote(1,R), 
       raw promote(-1,R), 
       raw promote(2,R), 
       raw promote(5,R),
       false);
  m2 = matrix m;
  assert(m1 == m2);
  -- rawColumnDotProduct
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
  n := matrix m;
  n1 := matrix for i from 0 to 5 list for j from 0 to 5 list promote(rawColumnDotProduct(raw m, i,j), R);
  assert((transpose n * n) == n1);
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
  n = matrix m;
  n1 = matrix for i from 0 to 5 list for j from 0 to 5 list promote(rawColumnDotProduct(raw m, i,j), R);
  assert((transpose n * n) == n1)
  )

testops4 = (R) -> (
  << "testops4..." << endl;
  m := mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
  m1 := matrix columnPermute(m,1,{2,0,1});
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
  m2 := matrix columnPermute(m,1,{2,0,1});  
  assert(m1 == m2);
  --
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
  m1 = matrix rowPermute(m,1,{2,0,1});  
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
  m2 = matrix rowPermute(m,1,{2,0,1});  
  assert(m1 == m2);
  )

testops5 = (R) -> (
     << "testops5 (submatrix, scalar mult)..." << endl;
     -- submatrix, scalar mult
     m := mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
     assert(2*m == m+m);
     assert(3*m == m+m+m);
     m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
     assert(2*m == m+m);
     assert(3*m == m+m+m);
     -- submatrices for dense matrix types:
     m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
     submatrix(m, {1,2,4,2}, {3,2,1});
     submatrix(matrix m, {1,2,4,3}, {3,2,1});
     assert(submatrix(m, {1,2,4,3}, {3,2,1}) == mutableMatrix submatrix(matrix m, {1,2,4,3}, {3,2,1}));
     assert(submatrix(m, {3,2,1}) == mutableMatrix submatrix(matrix m, {3,2,1}));
     -- submatrices for sparse matrix types:
     m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
     submatrix(m, {1,2,4,3}, {3,2,1});
     submatrix(matrix m, {1,2,4,3}, {3,2,1});
     assert(submatrix(m, {1,2,4,3}, {3,2,1}) == mutableMatrix(submatrix(matrix m, {1,2,4,3}, {3,2,1}), Dense=>false));
     assert(submatrix(m, {3,2,1}) == mutableMatrix(submatrix(matrix m, {3,2,1}), Dense=>false));
     )

<< "warning: not testing transpose of sparse mutable matrices yet" << endl;
testTranspose = (R) -> (
    --M := mutableMatrix(R, 3, 5);
    M := mutableMatrix(R, 3, 5, Dense=>true);
    fillMatrix M;
    N := transpose M;
    N2 := transpose N;
    N3 := transpose N2;
    assert(M == N2);
    assert(N == N3);
    assert(numRows M == numColumns N);
    assert(numRows N == numColumns M);
    for r from 0 to numRows M - 1 do for c from 0 to numColumns M - 1 do (
        assert(M_(r,c) == N_(c,r));
        );
    -- Now test trivial cases
    M0 := mutableMatrix(R, 0, 0);
    M1 := transpose M0;
    assert(M0 == M1);
    M0 = mutableMatrix(R, 0, 4);
    M1 = transpose M0;
    assert(numColumns M1 == 0);
    assert(numRows M1 == 4);
    )

testMutableMatrices = (R) -> (
     << "testing matrix ops for " << describe R << endl;
     testops0 R;
     testops R; 
     testops2 R; 
     testops3 R; 
     testops4 R;
     testops5 R;
     testTranspose R;
     << "tests passed for " << raw R << endl;
     )

TEST ///
  testMutableMatrices(ZZ/101)
///

testGF = (strategy) -> (
    R := null;
    assert(strategy===null or strategy==="Old" or strategy==="Givaro" or strategy==="CompleteGivaro");
    low := 1;
    hi := i -> 20;
    -- This upper bound for CompleteGivaro is made to match the default SizeLimit of 10000.
    if strategy === "CompleteGivaro" then (
        low = 2; -- this is an ERROR: it should be able to handle low==1. 
        hi = p -> if p == 2 then 13 
        else if p == 3 then 10 
        else if p == 5 then 5 
        else if p == 7 then 4
        else if p < 23 then 3
        else if p < 100 then 2
        else 1;
        );
    for i from low to hi 2 do (
        << "doing " <<  (2,i) << endl;
        R = GF(2^i, Strategy=>strategy);
        testMutableMatrices R;
        );
    for i from low to hi 3 do (
        R = GF(3^i, Strategy=>strategy);
        testMutableMatrices R;
        );
    for i from low to hi 5 do (
        R = GF(5^i, Strategy=>strategy);
        testMutableMatrices R;
        );
    for i from low to hi 7 do (
        R = GF(7,i, Strategy=>strategy);
        testMutableMatrices R;
        );
    )

TEST ///
  testGF null
  testGF "Old"  
  if hasFFPACK then testGF "Givaro"
--  testGF "CompleteGivaro" -- this one fails, since it doesn't fall back to a different representation if
    -- the size is too big
///

TEST ///
  testMutableMatrices(QQ)
///

TEST ///
  testMutableMatrices(QQ[x,y])
///

TEST ///
  testMutableMatrices(frac(QQ[x,y]))
///

TEST ///
  testMutableMatrices(RR_53)
///

TEST ///
  testMutableMatrices(RR_100)
///

TEST ///
  testMutableMatrices(CC_53)
///

TEST ///
  testMutableMatrices(CC_100)
///

TEST ///
  debug Core
  if hasFlint then testMutableMatrices(ZZp(101, Strategy=>"FLINT"))
///

TEST ///
  debug Core
  if hasFFPACK then testMutableMatrices(ZZp(101, Strategy=>"FFPACK"))
///

///
rings = {ZZ, ZZ/101, ZZ/2, GF(4), GF(25), QQ, QQ[x,y], frac(QQ[x,y]), RR_53, RR_100, CC_53, CC_100}
rings/testMutableMatrices
///

-----------------------------------------------------------------
-- Test of MutableMatrix linear algebra over fields -------------
-----------------------------------------------------------------

debug FastLinearAlgebra

-- the following function finds an n x n matrix S, and its inverse, S^-1 
-- over the ring kk.
-- The ring kk should be a field, for which det, rawLinAlgInverse are defined
randomFullRank = (R,n) -> (
    S := mutableMatrix(R,n,n);
    fillMatrix S;
    while det S == 0 do (
        S = mutableMatrix(R,n,n);
        fillMatrix S;
        );
    (S, inverse S)
    --Sinv := map(R, rawLinAlgInverse raw S);
    --(S, Sinv)
    )

randomJordanForm = (R, L) -> (
    M := mutableMatrix jordanForm(R,L);
    (S,Sinv) := randomFullRank(R, numColumns M);
    S * M * Sinv
    )

testLinAlgZZpFFPACK = () -> (
    kk := ZZp(101, Strategy=>"FFPACK");
    S := mutableMatrix(kk,3,3);
    fillMatrix S;
    D := mutableMatrix jordanForm(kk, splice{{1,3},4:{0,1},{2,3}});
    M := S * D;
    
    )

testRankBIG = (R) -> (
     << "testrank (TODO:finish it)..." << endl;
     m1 := random(R^5, R^11);
     m2 := random(R^11, R^6);
     m := mutableMatrix(m1 * m2);
     assert(5 == rank m); -- this can fail every now and then.
     N := 300;
     M := 200;
     m3 := mutableMatrix(ZZ, N, M);
     m4 := mutableMatrix(ZZ, M, N);
     fillMatrix m3;
     fillMatrix m4;
     m3 = time mutableMatrix sub(matrix m3, QQ);
     m4 = time mutableMatrix sub(matrix m4, QQ);
     time (m3*m4); -- MUCH faster than next line!
     time m5 := mutableMatrix((matrix m3) * (matrix m4));
     assert(m3*m4 == m5);
     debug Core;
     time rawLinAlgDeterminant raw m5;
     time det m5; -- doesn't work yet
     time rank m5; -- crashes: duplicate large block deallocation
     -- test of inverse:
     m1 = mutableMatrix(QQ, 4, 4);
     fillMatrix m1 ;
     inverse m1; -- doesn't work yet
     m2 = map(QQ, rawLinAlgInverse raw m1);
     m1*m2 == mutableIdentity(QQ, 4);
     )

///
-- simpler bug
     debug Core;
     R := ZZFlint;
     N = 100
     N = 70
     N = 50
     N = 20
     tally for i from 1 to 10000 list (
       m = mutableMatrix(R, N, N);
       fillMatrix m;
       rank m
       )
///

testRankFailing = () -> (
     debug Core;
     R := ZZFlint;
     (N,M) := (100,70);
     --(N,M) := (500,372);
     m3 := mutableMatrix(R, N, M);
     m4 := mutableMatrix(R, M, N);
     fillMatrix m3;
     fillMatrix m4;
     m5 := m3*m4;
     assert(0 == det m5);
     rank m5;  -- crash for R==ZZFlint
     m6 := m4*m3;
     rank m3;
     rank m4;
     rank m6 ;-- crash for R==ZZFlint
     rawLinAlgDeterminant raw m5;
     rawLinAlgRank raw m5; -- 
     )
     


benchMult = (R,N) -> (
    A := mutableMatrix(R,N,N);
    B := mutableMatrix(R,N,N);
    fillMatrix A;
    fillMatrix B;
    time A1 := matrix A;
    time B1 := matrix B;
    C := time A*B;
    C1 := time A1*B1;
    C1 = mutableMatrix C1;
    if C != C1 then << "ERROR: matrix mult is WRONG" << endl;
    (A,B,C,C1)
    )
///
  benchMult(ZZ,3)
  benchMult(ZZ,100);
  benchMult(ZZ,300);

  benchMult(ZZ/101,300);

  debug Core
  R = ZZp(101, Strategy=>"FLINT")
  benchMult(R,300); -- fails, due to row/col major encoding (I think).

  debug Core
  R = ZZp(101, Strategy=>"FFPACK")
  benchMult(R,10);
  oo#2 == oo#3

  benchMult(R,600);
  benchMult(RR_100,100);  -- WRONG
  (A,B,C,C1) = benchMult(RR_53,10);  -- some error.  Which is better?
  clean(1.0*10^-13, C-C1)
  clean(1.0*10^-13, matrix(C-C1))
  (A,B,C,C1) = benchMult(RR_53,100);
  flatten entries (C-C1);
  oo/abs//max
  (A,B,C,C1) = benchMult(RR_200,100);
  flatten entries (C-C1);
  oo/abs//max

  (A,B,C,C1) = benchMult(RR_200,300);
  flatten entries (C-C1);
  oo/abs//max

  (A,B,C,C1) = benchMult(RR_53,300);

  debug Core
  R2 = ZZp(33554393, Strategy=>"FLINT");
  benchMult(R2, 400); -- the multiply routine is the flint one: but it is multiplying the wrong order.
    -- This is caused by row/column major order problem in my (MES) code.
///

testDeterminant2x2 = (R) -> (
    for i from 1 to 10 do (
        M := mutableMatrix(R, 2, 2);
        fillMatrix M;
        assert(determinant M == M_(0,0) * M_(1,1) - M_(0,1) * M_(1,0));
        );
    )
testDeterminant3x3 = (R) -> (
    for i from 1 to 10 do (
        M := mutableMatrix(R, 3, 3);
        fillMatrix M;
        ans := M_(0,0) * M_(1,1) * M_(2,2) +
           M_(1,0) * M_(2,1) * M_(0,2) +
           M_(2,0) * M_(0,1) * M_(1,2) -
           M_(2,0) * M_(1,1) * M_(0,2) -
           M_(1,0) * M_(0,1) * M_(2,2) -
           M_(0,0) * M_(2,1) * M_(1,2);
        assert(determinant M == ans)
        );
    )
testDeterminantViaMult = (R) -> (
    for i from 1 to 10 do (
        M := mutableMatrix jordanForm(R, splice{{i-1,3},{i+13,2},7:{i,1}});
       assert((i-1)^3 * (i+13)^2 * i^7 == det M);
        S := mutableMatrix(R, numRows M, numColumns M);
        fillMatrix(S, Density=>.3);
        assert(ring det S === R);
        assert(det(S*M*S) == det(S)^2 * det(M));
        );
    )
testDeterminant = (R) -> (
    testDeterminant2x2 R;
    testDeterminant3x3 R;
    testDeterminantViaMult R;
    )

testMultSimple = (R) -> (
    E := {{1_R, 2_R, 3_R}, {7_R, 0_R, 0_R}};
    m1 := mutableMatrix E;
    m2 := mutableMatrix transpose E;
    assert(matrix(m1*m2) == (matrix m1) * (matrix m2));
    assert(matrix(m2*m1) == (matrix m2) * (matrix m1));
    m3 := mutableMatrix(R,0,3);
    m4 := mutableMatrix(R,3,4);
    m5 := m3*m4;
    assert(numRows m5 == 0);
    assert(numColumns m5 == 4);
    )
testMultrs = (R,r,s) -> (
    for i from 1 to 10 do (
        M := mutableMatrix(R, r,s);
        N := mutableMatrix(R, s,r);
        fillMatrix M;
        fillMatrix N;
        MN := M*N;
        NM := N*M;
        assert(MN == mutableMatrix((matrix M) * (matrix N)));
        assert(NM == mutableMatrix((matrix N) * (matrix M)));
        );
    )
testMultAddSub = (R1) -> (
    A := mutableMatrix(R1,3,6);
    B := mutableMatrix(R1,6,5);
    C := mutableMatrix(R1,3,5);
    fillMatrix A; fillMatrix B; fillMatrix C;
    C1 := matrix C;
    rawLinAlgAddMult(raw C,raw A,raw B);
    assert(C1 + (matrix A) * (matrix B) == matrix C);
    C = mutableMatrix C1;
    rawLinAlgSubMult(raw C,raw A,raw B);
    assert(C1 - (matrix A) * (matrix B) == matrix C);
    )
testMult = (R) -> (
  testMultSimple R;
  testMultrs(R,2,3);
  testMultrs(R,5,10);
  testMultrs(R,10,3);
  testMultAddSub R;
  )

testInverse = (R) -> (
    m1 := mutableMatrix(R,0,0);
    --assert(inverse m1 == 0);
    m2 := mutableMatrix(R,1,1);
    --inverse m2 -- gives an unknown engine error
    for i from 2 to 20 do (
        for j from 1 to 5 do (
            (S,Sinv) := randomFullRank(R,i);
            assert(S*Sinv == mutableIdentity(R,i));
            assert(Sinv*S == mutableIdentity(R,i));
        ))
    )

randomMatrixWithKernel = (R, ncols, rk) -> (
    -- returns a pair (M, X), such that M*X == 0, and the
    -- columns of X are a basis for ker M.
    MAXN := if char R > 0 then char R - 1 else 50;
    L1 := for i from 0 to rk-1 list {(1 + random MAXN)_R, 1};
    L2 := splice {(ncols-rk):{0,1}};
    L := join(L1,L2);
    A := mutableMatrix jordanForm(R, L);
    (S,Sinv) := randomFullRank(R, ncols);
    M := Sinv*A*S;
    X := mutableMatrix(R,ncols,ncols-rk);
    for i from 0 to ncols-rk-1 do X_(rk+i,i) = 1;
    (M, Sinv*X)
    )
testNullspace1 = (R) -> (
    -- we expect that R has the usual linear algebra routines defined
    (S,Sinv) := randomFullRank(R,10);
    D := mutableMatrix jordanForm(R, splice{{1,3},4:{0,1},{2,3}});
    M := S * D * Sinv;
    r := rank M;
    assert(r == rank D);
    assert(det M == det D);
    V := nullSpace M;
    assert(M * V == 0);
    assert(numColumns M == r + numColumns V)
    )
testNullspace = (R) -> (
    E := map(R^2, R^3, {{1, 4, 5}, {2, 3, 6}});
    M := mutableMatrix E;
    X := nullSpace M;
    assert(M*X == 0);
    assert(numColumns X == 1);
 
    testNullspace1 R;
        
    -- Now: create matrices with known null space, and check them
    for j from 1 to 15 do (
    for i from 0 to j do (
      (M,X1) := randomMatrixWithKernel(R, j, i);
      X2 := nullSpace M;
      Xall := mutableMatrix((matrix X1) | (matrix X2));
      assert(M*X2 == 0);
      assert(numColumns X2 == j-i);
      assert(rank Xall == j-i);
      ))
    )

testRank = (R) -> (
    -- make a random square matrix, with known rank.  Compute rank.
    M := mutableMatrix(R,0,0);
    assert(rank M == 0);
    for i from 1 to 20 do (
        for j from 0 to i do (
            (M,X) := randomMatrixWithKernel(R, i, j);
            assert(rank M == j);
        ))
    )

testRankProfile = (R) -> (
    -- WRITE ME WRITE ME
    M1 := mutableMatrix matrix(R, {{2, 16, 29}, {-18, 24, 12}, {-41, 7, -31}});
    rowRankProfile M1
    );

testLUBoundaryCases = (kk) -> (
    -- this should be in e/unit-tests too?
    a0 := map(kk^0,kk^0,0);
    a1 := map(kk^0,kk^5,0);
    a2 := map(kk^5,kk^0,0);
    b0 := mutableMatrix a0;
    b1 := mutableMatrix a1;
    b2 := mutableMatrix a2;
    (P,L,U) := LUdecomposition a0;
    assert(#P == 0 
        and numrows L == 0 
        and numcols L == 0 
        and numrows U == 0 
        and numcols U == 0);
    (P,L,U) = LUdecomposition a1;
    assert(#P == 0 
        and numrows L == 0 
        and numcols L == 0 
        and numrows U == 0 
        and numcols U == 5);
    (P,L,U) = LUdecomposition a2;
    assert(#P == 5 
        and numrows L == 5 
        and numcols L == 0 
        and numrows U == 0 
        and numcols U == 0);
    -- same for mutable matrices
    (P,L,U) = LUdecomposition b0;
    assert(#P == 0 
        and numrows L == 0 
        and numcols L == 0 
        and numrows U == 0 
        and numcols U == 0);
    (P,L,U) = LUdecomposition b1;
    assert(#P == 0 
        and numrows L == 0 
        and numcols L == 0 
        and numrows U == 0 
        and numcols U == 5);
    (P,L,U) = LUdecomposition b2;
    assert(#P == 5 
        and numrows L == 5 
        and numcols L == 0 
        and numrows U == 0 
        and numcols U == 0);
    -- now LU of zero matrices
    -- Note that this is documented in 'viewHelp "LUdecomposition"'
    z0 := mutableMatrix(kk,2,4);
    (P,L,U) = LUdecomposition z0;
    assert((numrows L, numcols L) == (2,2)); -- not yet
    assert((numrows U, numcols U) == (2,4)); -- not yet
    z1 := mutableMatrix(kk,4,2);
    (P,L,U) = LUdecomposition z1;
    assert((numrows L, numcols L) == (4,2)); -- not yet
    assert((numrows U, numcols U) == (2,2)); -- not yet
    -- Now test boundary cases for the derivative routines:
    -- determinant:
    assert(det a0 == 1_kk);
    assert(det b0 == 1_kk);
    -- rank:
    assert(rank a0 == 0);
    assert(rank a1 == 0);
    assert(rank a2 == 0);
    assert(rank b0 == 0);
    assert(rank b1 == 0);
    assert(rank b2 == 0);
    -- transpose
    assert(transpose a1 == a2);
    assert(transpose a0 == a0);
    assert(transpose b1 == b2);
    assert(transpose b0 == b0);
    -- rank profile
    assert(rowRankProfile b0 == {});
    assert(rowRankProfile b1 == {});
    assert(rowRankProfile b2 == {});
    assert(columnRankProfile b0 == {});
    assert(columnRankProfile b1 == {});
    assert(columnRankProfile b2 == {});
    -- solve
    s2 := solve(b2, mutableMatrix(kk,5,2));
    assert(numrows s2 == 0);    
    assert(numcols s2 == 2);
    s2 = solve(b2, mutableMatrix(kk,5,0));
    assert(s2 == b0);
    -- inverse
    assert(inverse b0 == b0);
    -- nullSpace
    n0 := nullSpace b0;
    assert(n0 == b0);
    n1 := nullSpace b1;
    assert(rank n1 == 5);
    n2 := nullSpace b2;
    assert(n2 == b0);
    -- multiplication
    assert(a1 * a2 == a0);
    assert(b1 * b2 == b0);
    assert(b2 * b1 == mutableMatrix(kk,5,5));
    )    

testSolveSimple = (R) -> (
    M := mutableMatrix map(R^2, R^2, {{1, 4}, {2, 3}});
    B := mutableMatrix map(R^2, R^1, {{3}, {7}});
    -- square M, B has one col
    X := solve(M,B);
    assert(M*X-B == 0);
    -- square M, B has 3 cols
    B2 := mutableMatrix map(R^2, R^3, {{1,0,3},{0,1,7}});
    X = solve(M,B2);
    assert(M*X-B2 == 0);
    -- M is 2x3, B is 2x1
    M = mutableMatrix map(R^2, R^3, {{1,4,7},{1,8,9}});
    B = mutableMatrix map(R^2, R^1, {{1},{3}});
    X = solve(M,B);
    assert(M*X-B == 0);
    -- M is 2x3, B is 2x1
    M = mutableMatrix transpose map(R^2, R^3, {{1,4,7},{1,8,9}});
    B = mutableMatrix map(R^3, R^1, {{5},{32},{41}});
    X = solve(M,B);
    assert(M*X-B == 0);
    )
testSolve = (R) -> (
    testSolveSimple R;
    return;
    -- now for more complicated examples
    -- FAILING TEST: crashes
    debug Core;
    R = ZZp(101, Strategy=>"FFPACK");
    N := 90;
    M := mutableMatrix(R, N, N);
    fillMatrix M;
    B := mutableMatrix(R, N, 5);
    fillMatrix B;
    time rawLinAlgSolve(raw M, raw B, true);
    )

testLUoverRR = () -> (
    R := RR_53;
    M := mutableMatrix(R,10,10);
    fillMatrix M;
    (P,L,U) := LUdecomposition M;
    assert(norm(M - checkLU(P,L,U)) < 1e-59);

    M = mutableMatrix(R,100,100);
    fillMatrix M;
    time (P,L,U) = LUdecomposition M;
    assert(norm(M - checkLU(P,L,U)) < 1e-58);

    M = mutableMatrix(R,500,500);
    fillMatrix M;
    time (P,L,U) = LUdecomposition M;
    assert(norm(M - checkLU(P,L,U)) < 1e-58);
    )

testLUoverRRR = () -> (
    R := RR_200;
    M := mutableMatrix(R,10,10);
    fillMatrix M;
    (P,L,U) := LUdecomposition M;
    assert(norm(M - checkLU(P,L,U)) < 1e-59);

    M = mutableMatrix(R,100,100);
    fillMatrix M;
    time (P,L,U) = LUdecomposition M;
    assert(norm(M - checkLU(P,L,U)) < 1e-58);

    M = mutableMatrix(R,500,500);
    fillMatrix M;
    time (P,L,U) = LUdecomposition M;
    assert(norm(M - checkLU(P,L,U)) < 1e-58);
    )
testLUoverCCC = () -> (
    R := CC_200;
    M := mutableMatrix(R,10,10);
    fillMatrix M;
    (P,L,U) := LUdecomposition M;
    assert(norm(M - checkLU(P,L,U)) < 1e-59);
    
    M = mutableMatrix(R,100,100);
    fillMatrix M;
    time (P,L,U) = LUdecomposition M;
    assert(norm(M - checkLU(P,L,U)) < 1e-58);

    M = mutableMatrix(R,500,500);
    fillMatrix M;
    time (P,L,U) = LUdecomposition M;
    assert(norm(M - checkLU(P,L,U)) < 1e-58);
    time (L*U);
    time (L1 := matrix L);
    time (U1 := matrix U);
    time (L1*U1);
    )
testSolveOverRRR = () -> (
    R := RR_200;
    M := mutableMatrix(R,10,10);
    fillMatrix M;
    B := mutableMatrix(R,10,1);
    fillMatrix B;
    time X := solve(M,B);
    assert(norm(M*X-B) < 1e-59);
    
    M = mutableMatrix(R,100,100);
    fillMatrix M;
    B = mutableMatrix(R,100,1);
    fillMatrix B;
    time X = solve(M,B);
    assert(norm(M*X-B) < 1e-57)
    )

testLinearAlgebra = (R) -> (
    -- ONLY TESTS DENSE MATRIX CASE
    testLUBoundaryCases R;
    testDeterminant R;
    testMult R;
    testInverse R;
    testNullspace R;
    testRank R;
    testRankProfile R; -- NOT WRITTEN YET
    testSolve R; -- NOT DONE
    )
    
---------------------------------
-- linear algebra: engine ZZ/p --
---------------------------------
TEST ///
  debug Core
  R = ZZ/101
  M = mutableMatrix matrix(R, {{0,3,2,1,4,7},{0,0,4,3,7,2},{0,0,0,1,7,4},{0,0,0,0,0,3}})
  b = transpose mutableMatrix matrix(R, {{11,9,11,3}})
  LUdecomposition M
  assert(rank M == 4)
  assert(rawLinAlgRankProfile(raw M, false) == {1,2,3,5})
  L = solve(M, b)
  assert(M*L - b  == 0)
  N = nullSpace M 
  assert(M * N == 0)

  L = mutableMatrix(R,4,4)
  fillMatrix L
  M1 = L*M
  b1 = L*b
  
  rank M1
  (P,L,U) = LUdecomposition M1
  assert(L*U == M1)

  rawLinAlgRankProfile(raw M1, false) == {1,2,3,5}
  M1 * solve(M1,b1) == b1
  M1 * nullSpace M1 == 0
  
  M = mutableMatrix matrix(R, {{1,2,3},{1,4,6},{4,1,2}})
  det M
  id3 = mutableIdentity(R,3)
  assert(M * solve(M,id3) == id3)
  assert(solve(M,id3) * M == id3)

  assert(M * inverse M == id3)
  assert((inverse M) * M == id3)
///

TEST ///
  debug Core
  R = ZZ/101
  testLinearAlgebra R;
///

--------------------------------
-- ffpack linear algebra: ZZ/p -
--------------------------------
if hasFFPACK then 
TEST ///
  debug Core
  R = ZZp(2, Strategy=>"FFPACK");
  testDeterminant R;
  testMult R;
  {*
  testInverse R; -- FAILS
  testRank R;  -- FAILS
  *}
///

if hasFFPACK then 
TEST ///
  debug Core
  R = ZZp(3, Strategy=>"FFPACK");
  testDeterminant R;
  testMult R;
  testInverse R;
  testRank R;
  testNullspace R;
///

if hasFFPACK then 
TEST ///
  debug Core
  R = ZZp(5, Strategy=>"FFPACK");
  testDeterminant R;
  testMult R;
  testInverse R;
  testRank R;
  testNullspace R;    
  testSolve R;
///

if hasFFPACK then 
TEST ///
  debug Core
  R = ZZp(101, Strategy=>"FFPACK");
  testDeterminant R;
  testMult R;
  testInverse R;
  testRank R;
  testNullspace R;
///

if hasFFPACK then 
TEST ///
  debug Core
  R = ZZp(30000001, Strategy=>"FFPACK");
  testDeterminant R
  testMult R
  testInverse R;
  testRank R;
  testNullspace R;
///

if hasFFPACK then 
TEST ///
  debug Core
  R = ZZp(maxFFPACKPrime, Strategy => "FFPACK")
  testDeterminant R
  testMult R
  testInverse R;
  testRank R;
  testNullspace R;
  --R = ZZp(33554467, Strategy => "FFPACK") -- this should not work
///

--------------------------------
-- engine ZZ/p -----------------
--------------------------------
TEST ///
  debug Core
  R = ZZp(2)
  testDeterminant R
  testMult R
  testNullspace R;
  testRank R;
///

TEST ///
  debug Core
  R = ZZp 3
  testDeterminant R
  testMult R
  testNullspace R;
  testRank R;
  testInverse R;
///

TEST ///
  debug Core
  R = ZZp 101
  testDeterminant R
  testMult R
  testNullspace R;
  testRank R;
  testInverse R;
///

TEST ///
  debug Core
  R = ZZp 32749
  testDeterminant R
  testMult R
  testNullspace R;
  testRank R;
  testInverse R;
///

--------------------------------
-- flint linear algebra: ZZ/p --
--------------------------------
  -- Not written yet: nullSpace for left side
  -- solveLinear: somehow the wrong one is being called
  -- rankProfile
  -- LUdecomposition
if hasFlint then 
TEST ///
  debug Core
  R = ZZp(2, Strategy => "FLINT")
  testDeterminant R
  testMult R
  testNullspace R;   -- the fillMatrix is not working here...
  testRank R;
  
 -- testSolve R  -- calling wrong rawLinAlgSolve: not implemented!

///

if hasFlint then 
TEST ///
  debug Core
  R = ZZp(3, Strategy => "FLINT")
  testDeterminant R
  testMult R
  testNullspace R;
  testRank R;
  testInverse R;
///

if hasFlint then 
TEST ///
  debug Core
  R = ZZp(5, Strategy => "FLINT")
  testDeterminant R
  testMult R
  testNullspace R;
  testRank R;
  testInverse R;
///

if hasFlint then 
TEST ///
  debug Core
  R = ZZp(101, Strategy => "FLINT")
  testDeterminant R
  testMult R
  testNullspace R;
  testRank R;
  testInverse R;
///

if hasFlint then 
TEST ///
  -- largest prime < 2^62
  debug Core
  R = ZZp(4611686018427387847, Strategy => "FLINT")
  testDeterminant R
  testMult R
  testNullspace R;
  testRank R;
  testInverse R;  
///

if hasFlint then 
TEST ///
  -- largest prime < 2^63
  debug Core
  R = ZZp(9223372036854775783, Strategy => "FLINT")
  testDeterminant R
  testMult R
  testNullspace R;
  testRank R;
  testInverse R;  
///


if hasFlint then 
TEST ///
  debug Core
  R = ZZp(maxFLINTPrime, Strategy => "FLINT")
  testDeterminant R
  testMult R
  testNullspace R;
  testRank R;
  testInverse R;  
///

--------------------------------
-- Engine GF -------------------
--------------------------------
TEST ///
  debug Core
  R = GF(3,2)
  testDeterminant R
  testMult R
  testNullspace R;
  testRank R;
  testInverse R;  
///

TEST ///
  debug Core
  R = GF(5,12)
  testDeterminant R
  testMult R
  testNullspace R;
  testRank R;
  testInverse R;  
///

--------------------------------
-- Givaro GF -------------------
--------------------------------
if hasFFPACK then
TEST ///
  debug Core
  R = GF(3,2, Strategy=>"Givaro")
  testDeterminant R
  testMult R
  testNullspace R;
  testRank R;
  testInverse R;  
///

if hasFFPACK then
TEST ///
  debug Core
  R = GF(5,12)
  testDeterminant R
  testMult R
  testNullspace R;
  testRank R;
  testInverse R;  
///


if hasFlint then 
TEST ///
  -- Most of this code is designed for fields...
  debug Core
  R = ZZFlint
  testDeterminant R
  testMult R
///

if hasFlint then 
TEST ///
  -- Flint QQ
  debug Core
  R = QQFlint
  testDeterminant R
  testMult R
  testRank R
  testNullspace R;
///

-----------------------------------
-- tests over approximate fields --
-----------------------------------
testClean = (R) -> (
    -- R should be an RR or CC
    -- this tests "clean" for: Matrix, MutableMatrix, poly ring.
    M := mutableMatrix(R, 10, 10);
    fillMatrix M;
    N := M^4 - M*M*M^2;
    for i from 0 to numRows N - 1 do N_(i,i) = 1.0 + N_(i,i);
    N2 := matrix N;
    assert(norm clean(.00001, N - mutableIdentity(R, numRows N)) == 0);
    assert(norm clean(.00001, N2 - matrix mutableIdentity(R, numRows N2)) == 0);
    -- this next test is actually not correct: 
    --   clean does NOT truncate values, only ones that are close to 0
    --assert(norm(clean(.00001, N) - mutableIdentity(R, numRows N)) == 0)
    P := R[getSymbol "x", getSymbol "y"];
    x := P_0;
    y := P_1;
    f := 1.01*x^2+3.2*y^2-y;
    assert(clean(.0001, f^4 - (f^2)^2) == 0)
    )
testNorm = (R) -> (
    -- R should be an RR or CC
    M := mutableMatrix(R, 10, 10);
    fillMatrix M;
    a := norm matrix M;
    b := norm M;
    ans := (flatten entries M)/abs//max;
    assert(a == ans);
    assert(b == ans);
    )

TEST ///
  testClean(RR_53)
  testClean(RR_100)
  testClean(RR_200)
  testClean(RR_54)

  testNorm(RR_53)
  testNorm(RR_100)
  testNorm(RR_200)
  testNorm(RR_54)

  testClean(CC_53)
  testClean(CC_100)
  testClean(CC_200)
  testClean(CC_54)

  testNorm(CC_53)
  testNorm(CC_100)
  testNorm(CC_200)
  testNorm(CC_54)
///






testLinearAlgebraOverField = (R) -> (
    -- fields to test this over:
    --  ZZ/p (internal)
    --  ZZ/p (FLINT)
    --  ZZ/p (FFPACK)
    --  GF (internal)
    --  GF (GIVARO?)
    --  GF (FLINT?)
    --  QQ (internal, GMP)
    --  QQ (FLINT)
    --  rational function fields
    --
    -- test the following functions:
    -- determinant
    -- rank
    -- inverse
    -- mult
    -- nullspace
    -- solveLinear
    -- rankProfile (row and column)
    -- addmultipleto, subtractmultipleto
    -- LU decomposition
    << "testing matrix linear algebra for " << describe R << endl;
    << "tests passed for " << raw R << endl;
    )

testPromote = () -> (
    -- test basic promote routines
    -- Part1: source: RR_53
    -- Part2: source: RR_100
    -- Part3: source: CC_53
    -- Part4: source: CC_100
    R1 := RR_53;
    C1 := CC_53;
    R := RR_100;
    S := RR_200;
    T := CC_100;
    U := CC_200;
    ---- Part1: promote from R1 ----
      m := matrix{{1.234 _ R1}};
      m1 := promote(m,C1);
      assert(lift(m1,R1) == m); -- NOT YET
      m1 = promote(m,R);
      assert(promote(m1,R1) == m);
      m1 = promote(m,S);
      assert(promote(m1,R1) == m);
      m1 = promote(m,T);
      assert(lift(m1,R1) == m); -- NOT YET
      m1 = promote(m,R1);
      assert(m1 == m);
    ---- Part 2: promote from R ----
      m = matrix{{1.234 _ R}};
      m1 = promote(m,C1);
      assert(lift(m1,R) == m); -- NOT YET
      m1 = promote(m,R);
      assert(promote(m1,R) == m);
      m1 = promote(m,S);
      assert(promote(m1,R) == m);
      m1 = promote(m,T);
      assert(lift(m1,R) == m); -- NOT YET
      m1 = promote(m,R1);
      assert(promote(m1,R) == m);
    ---- Part 3: promote from T ----
      m = matrix{{1.234 _ T}};
      m1 = promote(m,C1);
      assert(promote(m1,T) == m);
      m1 = promote(m,T);
      assert(m1 == m);
      m1 = promote(m,U);
      assert(promote(m1,T) == m);
      assert(try (promote(m,R); false) else true);
      assert(try (promote(m,S); false) else true);
      assert(try (promote(m,R1); false) else true);
    ---- Part 4: promote from C1 ----
      m = matrix{{1.234 _ C1}};
      m1 = promote(m,C1);
      assert(promote(m1,C1) == m);
      m1 = promote(m,T);
      assert(promote(m1,C1) == m);
      m1 = promote(m,U);
      assert(promote(m1,C1) == m);
      assert(try (promote(m,R); false) else true);
      assert(try (promote(m,S); false) else true);
      assert(try (promote(m,R1); false) else true);
    )

TEST ///
    testPromote()
///

testPromoteLift = () -> (
    R53 := RR_53;
    C53 := CC_53;
    R := RR_100;
    S := RR_200;
    T := CC_100;
    U := CC_200;
    m := matrix{{1.234 _ R}};
    mS := promote(m, S);
    mT := promote(m, T);
    mU := promote(m,U);
    mR53 := promote(m,R53);
    mC53 := promote(m,C53);
    n := mS;
    assert(m == promote(n, R));
    assert(mS == promote(n, S));
    assert(mT == promote(n, T));
    assert(mU == promote(n,U));
    assert(mR53 == promote(n,R53));
    assert(mC53 == promote(n,C53));
    assert(m == lift(mS, R)); -- fails in 1.6
    assert(m == lift(mT,R));
    m1 := matrix{{1+ii}};
    assert(try (lift(m1,RR_53); false) else true); -- should fail (as it does).
    x := (5.2)_R53;
    sub(x, R53); -- CRASH
    phi := map(R53, R53, {});
    phi (4.2); -- CRASH
    -- these work over infinite precision
    x = (5.2)_R;
    sub(x, R); -- ok
    phi = map(RR_300, RR_200, {});
    phi (4.2p200); -- ok
    -- try polynomial rings over these
    A := RR_100[getSymbol"x", getSymbol"y"];
    f := map(A, RR_100, {});
    f (1.2p100);
    sub(ideal x_A, A);
    -- try polynomial rings over  RR_53
    A = RR_53[getSymbol"x", getSymbol"y"];
    f = map(A, RR_53, {});
    f (1.2);
    sub(ideal x_A, A);
    )

TEST ///
 debug EngineTests
 testPromoteLift()
///

testRingMapEval = (R,C) -> (
    x := getSymbol "x";
    PC := C[x];
    x = PC_0;
    F := matrix{{x^3}};
    P := matrix{{0.1_R}};
    (map(C,PC,P)) F;
    map(R,PC); -- works, but should not
    (map(R,PC,P)) F -- crashes
    )

--TEST 
///
  debug EngineTests
  R = RR; C = CC;
  testRingMapEval(RR,CC)
  testRingMapEval(RR_100,CC_100)
///


TEST ///
  testMutableMatrices ZZ
///

TEST ///
  R = RR_100
  M = mutableMatrix(R,5,5)
  fillMatrix M
  N = matrix M
  det N
  det M
  M*M - mutableMatrix(N*N)
  inverse M
  

  R = CC_100
  M = mutableMatrix(R,5,5)
  fillMatrix M
  N = matrix M
  det N
  det M
  det(N*N) - (det N)^2
  det(M*M) - (det M)^2
  M*M - mutableMatrix(N*N)
  
  R = RR_53
  M = mutableMatrix(R,5,5)
  fillMatrix M
  N = matrix M
  det N
  det M
  M*M - mutableMatrix(N*N)
///

if hasFlint and hasFFPACK then 
TEST ///
  -- Which rings have linear algebra routines defined?
  debug Core
  hasLinAlgRank = (R) -> (
      M = mutableMatrix(R, 4, 4);
      fillMatrix M;
      rawLinAlgRank raw M
      );

  hasEngineLinearAlgebra(ZZ)
  assert hasEngineLinearAlgebra(ZZFlint)
  assert hasEngineLinearAlgebra(QQ)
  assert hasEngineLinearAlgebra(ZZp(101, Strategy=>"FLINT"))
  assert hasEngineLinearAlgebra(ZZp(101, Strategy=>"FFPACK"))
  hasEngineLinearAlgebra(ZZ/101)
  hasEngineLinearAlgebra (GF(2^3, Strategy=>null))
  hasEngineLinearAlgebra (GF(2^3, Strategy=>"Givaro"))
  hasEngineLinearAlgebra (GF(2^3, Strategy=>"Old"))

  hasLinAlgRank ZZ  -- NO
  hasLinAlgRank QQ  -- NO
  hasLinAlgRank (ZZp(101, Strategy=>"FLINT")) -- yes, this one works!
  hasLinAlgRank (ZZp(101, Strategy=>"FFPACK")) -- yes, this one works!
  hasLinAlgRank (ZZp(101, Strategy=>null)) -- NO

  debug Core
  initializeEngineLinearAlgebra QQ
///

TEST /// 
  -- of clean
  R = CC_100 
  M = matrix{{0.0001_R+ii_R}}
  M = mutableMatrix{{0.0001_R+ii_R}}
  clean_0.001 M
///

TEST ///
  -- eigenvalues
  M = mutableMatrix(CC_100,3,3)
  M = matrix fillMatrix M
  eigenvalues M
  LUdecomposition M
///

TEST ///
-- of rawDiscreteLog
debug Core
kk = ZZ/32003
L = for i from 1 to 32002 list rawDiscreteLog raw (i_kk);
a = 2_kk;
assert(1 == rawDiscreteLog (raw a))
L2 = for i from 0 to 32001 list a^(L#i);
L3 = toList (1..32002);
assert(L2 == L3)

///
end

restart
loadPackage "EngineTests"
check EngineTests

testMutableMatrices(ZZ/101)

-- BUG!! in usual Matrix submatrix code!!
R = ZZ/101
m = matrix mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
m^{1,1}
----------------------
R = ZZp 101
     m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
     assert(2*m == m+m);
     assert(3*m == m+m+m);
     m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
     assert(2*m == m+m);
     assert(3*m == m+m+m);

debug Core
rawSubmatrix(raw m, {0,1,2,3,4},{0,1,2,3,4})
m = mutableMatrix(map(R^3,R^20, (i,j) -> 100*i+j), Dense=>true)
rawSubmatrix(raw m, {0,2,0,2},{0,2,5,7,11,30})
rawSubmatrix(raw m, {0,2,3,2},{0,2,5,7,11,10})

debug EngineTests
testMutableMatrices(ZZ/101)
testMutableMatrices(ZZ/2)
testMutableMatrices(GF 4)

(67108819, 67108837, 67108859, 67108879, 67108913, 67108919, 67108933, 67108957, 67108961, 67108981)

kk = ZZp 67108819
testMutableMatrices kk

kk = ZZp 67108981

kk = ZZp 32003
testMutableMatrices kk

kk = ZZp 1049599
kk = ZZp 1073742851

-- Question: how do we get 
--kk = GF (1073742851, 1, Strategy=>"Givaro")
--testMutableMatrices kk

--kk = GF (1049599, 1, Strategy=>"CompleteGivaro")
--testMutableMatrices kk

kk = GF(2,4,Strategy=>"Old")
testMutableMatrices kk -- fails, since rank is not yet defined for this type of ring

kk = GF(2,4,Strategy=>"Givaro")
testMutableMatrices kk

kk = GF(2,4,Strategy=>"CompleteGivaro")
testMutableMatrices kk

kk = GF(2,12,Strategy=>"New")
testMutableMatrices kk

kk = GF(5,12,Strategy=>"New")
testMutableMatrices kk
