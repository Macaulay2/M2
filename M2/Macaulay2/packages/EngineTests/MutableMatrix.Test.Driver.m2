-- -*- coding: utf-8 -*-

load "EngineTests/MutableMatrix.Test.Elementary.m2"
load "EngineTests/MutableMatrix.Test.ReduceByPivots.m2"



end

--
restart

load "MutableMatrix.Test.Elementary.m2"


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

kk = GF(2,4,Strategy=>"Old")
testMutableMatrices kk

kk = GF(2,4,Strategy=>"Flint")
testMutableMatrices kk

kk = GF(2,4,Strategy=>"FlintBig")
testMutableMatrices kk

kk = GF(2,12,Strategy=>"New")
testMutableMatrices kk

kk = GF(5,12,Strategy=>"New")
testMutableMatrices kk

