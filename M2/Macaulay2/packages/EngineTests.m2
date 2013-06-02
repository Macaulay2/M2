newPackage(
        "EngineTests",
        Version => "0.1", 
        Date => "29 Aug 2011",
        Authors => {{Name => "Mike Stillman", 
                  Email => "", 
                  HomePage => ""}},
        Headline => "a test suite for the Macaulay2 engine",
	    PackageExports => {"FastLinearAlgebra"},
        DebuggingMode => true
        )

export { ringOps, 
     testMutableMatrices,
     testFrac
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
     << "testrank..." << endl;
     m1 := random(R^5, R^11);
     m2 := random(R^11, R^6);
     m := mutableMatrix(m1 * m2);
     assert(5 == rank m); -- this can fail every now and then.
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

testMult = (R) -> (
    -- start with matrices M and N (same size)
    -- compute M * (transpose N), N * (transpose M).  These should be the same up to transpose
    )
testSolve = (R) -> (
    E = map(R^2, R^2, {{1, 4}, {2, 3}})
    B = map(R^2, R^1, {{3}, {7}})
    M = mutableMatrix E
    B = mutableMatrix B
    rawLinAlgSolve(raw M,raw B, true) -- doesn't seem to be implemented yet.
    )

testMutableMatrices = (R) -> (
     << "testing " << describe R << endl;
     testops0 R;
     testops R; 
     testops2 R; 
     testops3 R; 
     testops4 R;
     testops5 R; -- Not working on 1.6 for R=ZZ
     testTranspose R;
     --testRank R;
     << "tests passed for " << describe R << endl;
     )

TEST ///
  testMutableMatrices ZZ
///

TEST ///
  testMutableMatrices(ZZ/101)
///

TEST ///
  testMutableMatrices(GF 4)
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
  -- FAILS: row-major versus column-major representation
  debug Core
  hasFlint := try (ZZp(101, "Choose"=>"FLINT"); true) else false;
  if hasFlint then testMutableMatrices(ZZp(101, "Choose"=>"FLINT"))
///

TEST ///
  -- passes
  debug Core
  hasFFPACK := try (ZZp(101, "Choose"=>"FFPACK"); true) else false;
  if hasFFPACK then testMutableMatrices(ZZp(101, "Choose"=>"FFPACK"))
///

///
rings = {ZZ, ZZ/101, ZZ/2, GF(4), GF(25), QQ, QQ[x,y], frac(QQ[x,y]), RR_53, RR_100, CC_53, CC_100}
rings/testMutableMatrices
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
