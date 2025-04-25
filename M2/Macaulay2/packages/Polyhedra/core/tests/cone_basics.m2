-- Test Cone_basics 1
-- pointed, simplicial, dim, ambDim
TEST ///
C = coneFromVData matrix {{12,3},{3,5}}
assert(dim C == 2)
assert(ambDim C == 2)
assert(isPointed C)
assert(isSimplicial C)
assert(not isSmooth C)
///
-- Test Cone_basics 2
-- pointed, simplicial, dim, ambDim
TEST ///
C = coneFromVData matrix {{1,0,0},{0,-1,1}}
assert(dim C == 2)
assert(ambDim C == 2)
assert(not isPointed C)
assert(isSimplicial C)
D = dualCone C
assert(not isFullDimensional D)
assert(isSmooth C)
///
-- Test Cone_basics 3
-- pointed, simplicial, dim, ambDim
TEST ///
C = coneFromVData transpose matrix {{1,0,0},{1,1,0},{1,1,1},{1,0,1}}
assert(dim C == 3)
assert(ambDim C == 3)
assert(isPointed C)
assert(not isSimplicial C)
assert(not isSmooth C)
///
-- Test 6
-- Checking intersections that give cones
TEST ///
C = coneFromHData matrix {{1,2},{2,1}};
R1 = rays C;
R1 = set apply(numColumns R1, i -> R1_{i});
R2 = set {matrix{{2},{-1}},matrix{{-1},{2}}};
assert(isSubset(R1,R2) and isSubset(R2,R1))
assert(linSpace C == 0)
assert(dim C == 2)
assert(ambDim C == 2)
///
-- Test 8
-- Checking coneFromVData
TEST ///
C = coneFromVData(matrix{{1,0},{0,1},{0,0}},matrix{{0},{0},{1}});
assert(halfspaces C == matrix{{1,0,0},{0,1,0}})
assert(numColumns rays C == 2)
///
-- Test 12
-- Checking dualCone
TEST ///
C1 = coneFromHData matrix {{0,1,2,3},{1,2,3,0},{2,3,0,1}};
C2 = coneFromHData matrix {{7,-5,1,1},{0,1,4,-3},{0,9,-6,1},{0,-3,2,9},{-7,5,-1,-1}};
C3 = dualCone C1;
assert(C3 == C2)
///

-- Test 16
-- Checking isPointed
TEST ///
C = coneFromVData matrix {{1,1,1,1},{1,-1,1,-1},{1,1,-1,-1}};
assert isPointed C
C = coneFromVData {C,matrix{{-1},{0},{-1}}};
assert not isPointed C
///

-- Test 17
-- Checking isSmooth
TEST ///
C = coneFromVData matrix {{1,0,0},{-1,2,3},{1,1,2}};
assert isSmooth C
C = coneFromVData {C,matrix{{1},{0},{2}}};
assert not isSmooth C
C = coneFromVData matrix {{1,2},{2,1},{1,2}};
assert not isSmooth C
C = coneFromVData matrix {{1,1,-1,-1},{1,2,1,-1},{1,3,0,-1}};
assert not isSmooth C
C = coneFromVData {C,matrix{{1},{0},{1}}};
assert isSmooth C
///

-- Test 10
-- Checking contains for cones
TEST ///
C1 = coneFromVData matrix {{1,0,0},{0,1,0},{0,0,1}};
C2 = coneFromVData matrix {{1},{1},{1}};
assert contains(C1,C2)
assert(not contains(C2,C1))
C2 = coneFromVData {C2, matrix {{1,-1,0,0},{0,0,1,-1},{0,0,0,0}}};
assert contains(C2,C1)
assert(not contains(C1,C2))
///

-- Test 12
-- Checking dualCone
TEST ///
C1 = coneFromVData matrix {{1,2,3},{2,3,1},{3,1,2}};
C2 = coneFromVData matrix {{-5,7,1},{1,-5,7},{7,1,-5}};
C1 = dualCone C1;
assert(C1 == C2)
///

-- Test 37
-- Checking hilbertBasis
TEST ///
C = coneFromVData matrix {{1,2},{2,1}};
H = hilbertBasis C;
L = {matrix {{1},{1}},matrix {{2},{1}},matrix {{1},{2}}};
assert(set H === set L)
C = coneFromVData matrix {{1,1,0},{0,3,0},{0,0,1}};
H = hilbertBasis C;
L = {matrix {{1},{0},{0}},matrix {{1},{1},{0}},matrix {{1},{2},{0}},matrix {{1},{3},{0}},matrix {{0},{0},{1}}};
assert(set H === set L)
///

-- A simplicial polytope
TEST ///
P = convexHull transpose matrix {{0,0},{2,0},{2,3},{-1,4}}
assert(isSimplicial P)
///

--Test for interiorVector for cone without rays
TEST ///
C = coneFromVData(map(ZZ^(4),ZZ^1,0), matrix{{1},{1},{1},{1}})
assert(interiorVector C==matrix{{0},{0},{0},{0}})
///