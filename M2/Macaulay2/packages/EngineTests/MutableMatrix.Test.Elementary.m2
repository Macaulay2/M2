-- -*- coding: utf-8 -*-


--needsPackage "MutableMatrix.Test.Base"

export {
   "testMutableMatrices",
   "testGF"
}

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
  testMutableMatrices ZZ
///


TEST ///
  testMutableMatrices(ZZ/101)
///

testGF = (strategy) -> (
    R := null;
    assert(strategy===null or strategy==="Old" or strategy==="Flint" or strategy==="FlintBig" or strategy==="New");
    low := 1;
    hi := i -> 20;
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
  testGF "New"  
  testGF "Flint"
  testGF "FlintBig"
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
  testMutableMatrices(ZZp(101, Strategy=>"Flint"))
///

TEST ///
  debug Core
  if hasFFPACK then testMutableMatrices(ZZp(101, Strategy=>"Ffpack"))
///


TEST /// 
  -- of clean
  R = CC_100 
  M = matrix{{0.0001_R+ii_R}}
  M = mutableMatrix{{0.0001_R+ii_R}}
  clean_0.001 M
///



///
rings = {ZZ, ZZ/101, ZZ/2, GF(4), GF(25), QQ, QQ[x,y], frac(QQ[x,y]), RR_53, RR_100, CC_53, CC_100}
rings/testMutableMatrices
///
