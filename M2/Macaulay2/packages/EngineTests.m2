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

export { ringOps, 
     testMutableMatrices,
     testFrac,
     testGF
     }

--load (EngineTests#"source directory"|"EngineTests/test-gbZZ.m2")
--load (EngineTests#"source directory"|"EngineTests/test-linalg.m2")

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

-- Test tow and column operations on dense and sparse mutable matrices,
-- over the rings: ZZ, ZZ/p, QQ, QQ[x,y,z], RRR, CCC, frac QQ[x,y]

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

debug FastLinearAlgebra

testRank = (R) -> (
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
     (N,M) := (500,372);
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

testDeterminant = (R) -> (
    M := mutableMatrix(R, 2, 2);
    fillMatrix M;
    assert(determinant M == M_(0,0) * M_(1,1) - M_(0,1) * M_(1,0));
    )

testMultSimple = (R) -> (
    E := {{1_R, 2_R, 3_R}, {7_R, 0_R, 0_R}};
    m1 := mutableMatrix E;
    m2 := mutableMatrix transpose E;
    assert(matrix(m1*m2) == (matrix m1) * (matrix m2));
    assert(matrix(m2*m1) == (matrix m2) * (matrix m1));
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
  R = ZZp(101, "Choose"=>"FLINT")
  benchMult(R,300); -- fails, due to row/col major encoding (I think).

  debug Core
  R = ZZp(101, "Choose"=>"FFPACK")
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
  R2 = ZZp(33554393, "Choose"=>"FLINT");
  benchMult(R2, 400); -- the multiply routine is the flint one: but it is multiplying the wrong order.
    -- This is caused by row/column major order problem in my (MES) code.
///

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
    rawLinAlgRank raw C
    )

testMult = (R) -> (
    R1 := ZZp(33554393, "Choose"=>"FFPACK");
    R2 := ZZp(33554393, "Choose"=>"FLINT");
    R3 := ZZFlint;
    testMultSimple R;
    testMultAddSub R1;
    testMultAddSub R2;
    testMultAddSub R3;
    testMultAddSub QQ;
    N := 500;
    M := mutableMatrix(R1, N, N);
    fillMatrix M;
    time M*M;
    (matrix M) * (matrix M) == matrix(M*M)
    )

testSolveSimple = (R) -> (
    E := map(R^2, R^2, {{1, 4}, {2, 3}});
    B := map(R^2, R^1, {{3}, {7}});
    M := mutableMatrix E;
    B = mutableMatrix B;
    X := map(R, rawLinAlgSolve(raw M,raw B, true));
    assert(M*X-B == 0)
    )

testSolve = (R) -> (
    testSolveSimple R;
    -- now for more complicated examples
    -- FAILING TEST: crashes
    debug Core;
    R = ZZp(101, "Choose"=>"FFPACK");
    N := 90;
    M := mutableMatrix(R, N, N);
    fillMatrix M;
    B := mutableMatrix(R, N, 5);
    fillMatrix B;
    time rawLinAlgSolve(raw M, raw B, true);
    )

testNullspace = (R) -> (
    debug Core;
    --R := ZZp(32003, "Choose"=>"FFPACK");
    E := map(R^2, R^3, {{1, 4, 5}, {2, 3, 6}});
    M := mutableMatrix E;
    X := map(R, rawLinAlgNullSpace(raw M, true));
    assert((matrix M) * (matrix X) == 0);
    assert(M * X == 0);
    )

testRankProfile = (R) -> (
    error "not written yet"
    );

testMutableMatrices = (R) -> (
     << "testing " << describe R << endl;
     testops0 R;
     testops R; 
     testops2 R; 
     testops3 R; 
     testops4 R;
     testops5 R;
     testTranspose R;
     --testRank R;
     << "tests passed for " << raw R << endl;
     )

testPromoteLift = () -> (
    R := RR_100;
    S := RR_200;
    T := CC_100;
    U := CC_200;
    m := matrix{{1.234 _ R}};
    mS := promote(m, S);
    mT := promote(m, T);
    mU := promote(m,U);
    n := mS;
    assert(m == promote(n, R));
    assert(mS == promote(n, S));
    assert(mT == promote(n, T));
    assert(mU == promote(n,U));
    assert(m == lift(mS, R)); -- fails in 1.6
    assert(m == lift(mT,R));
    m1 := matrix{{1+ii}};
    assert(try (lift(m1,RR_53); false) else true); -- should fail (as it does).
    )

TEST ///
 debug EngineTests
 testPromoteLift()
///

TEST ///
  testMutableMatrices ZZ
///

TEST ///
  testMutableMatrices(ZZ/101)
///

testGF = (strategy) -> (
    R := null;
    assert(strategy===null or strategy==="New" or strategy==="Givaro" or strategy==="CompleteGivaro");
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
  testGF "New"  
  testGF "Givaro"
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
  hasFlint := try (ZZp(101, "Choose"=>"FLINT"); true) else false;
  if hasFlint then testMutableMatrices(ZZp(101, "Choose"=>"FLINT"))
///

TEST ///
  debug Core
  hasFFPACK := try (ZZp(101, "Choose"=>"FFPACK"); true) else false;
  if hasFFPACK then testMutableMatrices(ZZp(101, "Choose"=>"FFPACK"))
///

///
rings = {ZZ, ZZ/101, ZZ/2, GF(4), GF(25), QQ, QQ[x,y], frac(QQ[x,y]), RR_53, RR_100, CC_53, CC_100}
rings/testMutableMatrices
///

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
  assert hasEngineLinearAlgebra(ZZp(101, "Choose"=>"FLINT"))
  assert hasEngineLinearAlgebra(ZZp(101, "Choose"=>"FFPACK"))
  hasEngineLinearAlgebra(ZZ/101)
  hasEngineLinearAlgebra (GF(2^3, Strategy=>null))
  hasEngineLinearAlgebra (GF(2^3, Strategy=>"Givaro"))
  hasEngineLinearAlgebra (GF(2^3, Strategy=>"New"))

  --hasLinAlgRank ZZ  -- NO
  --hasLinAlgRank QQ  -- NO
  hasLinAlgRank (ZZp(101, "Choose"=>"FLINT")) -- yes, this one works!
  hasLinAlgRank (ZZp(101, "Choose"=>"FFPACK")) -- yes, this one works!
  --hasLinAlgRank (ZZp(101, "Choose"=>null)) -- NO

  debug Core
  initializeEngineLinearAlgebra QQ
///

TEST ///
  debug Core

  hasLinAlg1 = (fcn, R) -> (
      M = mutableMatrix(R, 4, 4);
      fillMatrix M;
      fcn raw M
      );

  hasLinAlg = (fcn, R) -> (
      M = mutableMatrix(R, 4, 4);
      fillMatrix M;
      try (fcn raw M; true) else false
      );

  R = ZZp(101, "Choose"=>"FFPACK")
  hasLinAlg1(rawLinAlgDeterminant, R)
  assert hasLinAlg(rawLinAlgDeterminant, R)
  assert hasLinAlg(rawLinAlgInverse, R)

  R1 = ZZp(101, "Choose"=>"FLINT")
  assert hasLinAlg(rawLinAlgDeterminant, R1)

  --hasLinAlg(rawLinAlgDeterminant, ZZ/101)
  --hasLinAlg(rawLinAlgDeterminant, QQ)
  --hasLinAlg(rawLinAlgDeterminant, ZZ)
  
  
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

kk = GF(2,4,Strategy=>"New")
testMutableMatrices kk -- fails, since rank is not yet defined for this type of ring

kk = GF(2,4,Strategy=>"Givaro")
testMutableMatrices kk

kk = GF(2,4,Strategy=>"CompleteGivaro")
testMutableMatrices kk

kk = GF(2,12,Strategy=>"New")
testMutableMatrices kk

kk = GF(5,12,Strategy=>"New")
testMutableMatrices kk
