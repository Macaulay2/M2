-- Test Cone_basics 1
-- pointed, simplicial, dim, ambDim
TEST ///
C = posHull matrix {{12,3},{3,5}}
assert(dim C == 2)
assert(ambDim C == 2)
assert(isPointed C)
assert(isSimplicial C)
assert(not isSmooth C)
///
-- Test Cone_basics 2
-- pointed, simplicial, dim, ambDim
TEST ///
C = posHull matrix {{1,0,0},{0,-1,1}}
assert(dim C == 2)
assert(ambDim C == 2)
assert(not isPointed C)
assert(not isSimplicial C)
D = dualCone C
assert(not isFullDimensional D)
assert(isSmooth C)
///
-- Test Cone_basics 3
-- pointed, simplicial, dim, ambDim
TEST ///
C = posHull transpose matrix {{1,0,0},{1,1,0},{1,1,1},{1,0,1}}
assert(dim C == 3)
assert(ambDim C == 3)
assert(isPointed C)
assert(not isSimplicial C)
assert(not isSmooth C)
///
-- Test 8
-- Checking posHull
TEST ///
C = posHull(matrix{{1,0},{0,1},{0,0}},matrix{{0},{0},{1}});
assert(halfspaces C == matrix{{1,0,0},{0,1,0}})
assert(C#"number of rays" == 2)
///

-- Test 16
-- Checking isPointed
TEST ///
C = posHull matrix {{1,1,1,1},{1,-1,1,-1},{1,1,-1,-1}};
assert isPointed C
C = posHull {C,matrix{{-1},{0},{-1}}};
assert not isPointed C
///

-- Test 17
-- Checking isSmooth
TEST ///
C = posHull matrix {{1,0,0},{-1,2,3},{1,1,2}};
assert isSmooth C
C = posHull {C,matrix{{1},{0},{2}}};
assert not isSmooth C
C = posHull matrix {{1,2},{2,1},{1,2}};
assert not isSmooth C
C = posHull matrix {{1,1,-1,-1},{1,2,1,-1},{1,3,0,-1}};
assert isSmooth C
C = posHull {C,matrix{{1},{0},{1}}};
assert isSmooth C
///

