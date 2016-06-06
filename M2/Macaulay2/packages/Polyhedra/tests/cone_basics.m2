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
