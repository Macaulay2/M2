-- Test tow and column operations on dense and sparse mutable matrices,
-- over the rings: ZZ, ZZ/p, QQ, QQ[x,y,z], RRR, CCC, frac QQ[x,y]


testops = (R) -> (
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
  assert(numRows m == 5);
  assert(numColumns m == 6);
  --
  m1 = matrix rowSwap(m, 1,2);
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
  m2 = matrix rowSwap(m, 1,2);
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
  -*
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
    *-
  )

debug Core
testops2 = (R) -> (
  --
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
  rawInsertColumns(raw m,1,2);
  m1 = matrix m;
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
  rawInsertColumns(raw m,1,2);
  m2 = matrix m;
  assert(m1 == m2);
  --
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
  rawInsertRows(raw m,1,2);
  m1 = matrix m;
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
  rawInsertRows(raw m,1,2);
  m2 = matrix m;
  assert(m1 == m2);
  --
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
  rawDeleteColumns(raw m,1,3);
  m1 = matrix m;
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
  rawDeleteColumns(raw m,1,3);
  m2 = matrix m;
  assert(m1 == m2);
  --
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
  rawDeleteRows(raw m,1,3);
  m1 = matrix m;
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
  rawDeleteRows(raw m,1,3);
  m2 = matrix m;
  assert(m1 == m2);
  )

testops3 = (R) -> (
  -- rawMatrixColumnOperation2, rawMatrixRowOperation2, rawSortColumns2, rawColumnDotProduct
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>false);
  rawMatrixColumnOperation2(raw m, 1, 2, raw promote(1,R), 
       raw promote(-1,R), 
       raw promote(2,R), 
       raw promote(5,R),
       false);
  m1 = matrix m;
  m = mutableMatrix(map(R^5,R^6, (i,j) -> 100*i+j), Dense=>true);
  rawMatrixColumnOperation2(raw m, 1, 2, raw promote(1,R), 
       raw promote(-1,R), 
       raw promote(2,R), 
       raw promote(5,R),
       false);
  m2 = matrix m;
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
  n = matrix m;
  n1 = matrix for i from 0 to 5 list for j from 0 to 5 list promote(rawColumnDotProduct(raw m, i,j), R);
  assert((transpose n * n) == n1)
  )

testops4 = (R) -> (
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

rings = {ZZ, ZZ/101, QQ, QQ[x,y], frac(QQ[x,y]), RR_53, RR_100, CC_53, CC_100}
rings/testops
rings/testops2
rings/testops3
rings/testops4

end

load "~/src/M2/Macaulay2/packages/Macaulay2Doc/test/mutmat-elemops.m2"
R = ZZ/101

testops4 ZZ
testops = testops4tmp


testops ZZ
testops(ZZ/101)
testops(QQ)
testops(QQ[x,y])
testops(frac(QQ[x,y]))
testops(RR_53) -- crashed in r9764
testops(RR_100) -- crashed in r9764
testops(CC_53) -- crashed in r9764
testops(CC_100) -- crashed in r9764

testops2 ZZ
testops2(ZZ/101)
testops2(QQ)
testops2(QQ[x,y])
testops2(frac(QQ[x,y]))
testops2(RR_53)  -- crashes
testops2(RR_100)
testops2(CC_53) 
testops2(CC_100) 

m = mutableMatrix(map(ZZ^5,ZZ^6, (i,j) -> 100*i+j), Dense=>false)
m1 = matrix columnPermute(m,1,{2,0,1}) -- WRONG in r9764, crashes for me now (pre r9765)
m = mutableMatrix(map(ZZ^5,ZZ^6, (i,j) -> 100*i+j), Dense=>true)
m2 = matrix columnPermute(m,1,{2,0,1}) -- OK
assert(m1 == m2)  -- FAILS!!


