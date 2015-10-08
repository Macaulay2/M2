-- Place these tests into EngineTests.m2 once ARing's are really ready.

restart
debug Core
-------------------------------
R = rawARingZZFlint()
M = rawMutableMatrix(R, 3, 4, true)
rawMutableMatrixFillRandomDensity(M,1.0,0);
N = rawMutableMatrix(M, true)
M
rawInsertColumns(M, 2, 3)
M
rawInsertRows(M, 1, 4)
M
rawDeleteColumns(M, 2,4); M
rawDeleteRows(M, 1,4); M
assert(M == N)
-------------------------------
R = ZZp(101, "Choose"=>"FLINT")
M = rawMutableMatrix(raw R, 3, 4, true)
rawMutableMatrixFillRandomDensity(M,1.0,0);
N = rawMutableMatrix(M, true)
M
rawInsertColumns(M, 2, 3)
M
rawInsertRows(M, 1, 4)
M
rawDeleteColumns(M, 2,4); M
rawDeleteRows(M, 1,4); M
assert(M == N)
-------------------------------
R = ZZ/101[vars(0..11)]
M = mutableMatrix genericMatrix(R,a,3,4)
N = mutableMatrix M
M
rawInsertColumns(raw M, 2, 3)
M
rawInsertRows(raw M, 1, 4)
M
rawDeleteColumns(raw M, 2,4); M
rawDeleteRows(raw M, 1,4); M
assert(M == N)
-------------------------------
-- Test for solving:
R = ZZ/32003
M = mutableMatrix {{0_R, 0, 0, 0, 1, 0, 0}, {0_R, 0, 0, 0, 0, 0, 1}}
B = mutableMatrix {{1_R, 0}, {0, 1_R}}
X = mutableMatrix(R, 0,0)
debug Core
rawSolve(raw M,raw B,raw X)
X
