-- Test 0
-- Checking convexHull basics
TEST ///
P = convexHull matrix {{3,1,0,2},{0,2,2,1},{1,-1,2,0}};
assert(numColumns vertices P == 3)
assert(dim P == 2)
assert(ambDim P == 3)
assert(rays P == 0)
assert(linSpace P == 0)
M = promote(matrix {{3,4,1}},QQ);
v = promote(matrix {{10}},QQ);
assert(hyperplanes P == (M,v) or hyperplanes P == (-M,-v))

Q = convexHull transpose matrix {{1,2,3}}
C1 = coneFromVData transpose matrix {{1,0,0}}
C2 = coneFromVData  transpose matrix {{0,1,0},{1,0,0}}
I = intersection(Q+C1, C2)
assert(dim I == -1)
assert(isEmpty I)
///

-- Test 1
-- Checking convexHull basics
TEST ///
P = convexHull matrix {{3,1,0,2},{0,2,2,1},{1,-1,2,0}};
P = convexHull {P,(matrix{{4},{0},{-2}},matrix{{1,0,0},{0,1,-1},{0,0,0}})};
assert(dim P == 3)
assert(image linSpace P == image promote(matrix {{0},{1},{0}},QQ))
assert(hyperplanes P == (0,0))
///

-- Test 2
-- Checking convexHull halfspaces
TEST ///
P = convexHull (matrix{{1},{1}},matrix{{1,0},{0,1}});
M1 = matrix {{-1,0},{0,-1}};
v = matrix {{-1},{-1}};
assert(halfspaces P == (promote(M1,QQ),promote(v,QQ)))
///
-- Test 3
-- Checking convexHull and polyhedronFromHData
TEST ///
P2 =  convexHull matrix {{1,-2,-1,2},{2,1,-2,-1}};
M = matrix{{3,1},{-3,-1},{1,-3},{-1,3}};
v = matrix{{5},{5},{5},{5}};
assert(polyhedronFromHData(M,v) == P2)
///

-- Test 4
-- Checking polyhedronFromHData
TEST ///
P = polyhedronFromHData (matrix{{1,0},{0,1},{-1,0},{0,-1}},matrix{{1},{2},{3},{4}});
V1 = vertices P;
V1 = set apply(numColumns V1, i -> V1_{i});
V2 = {matrix{{1},{2}},matrix{{1},{-4}},matrix{{-3},{2}},matrix{{-3},{-4}}};
V2 = set apply(V2, v->promote(v, QQ))
assert(isSubset(V1,V2) and isSubset(V2,V1))
///


-- Test 11
-- Checking equality for polyhedra and cones
TEST ///
P = convexHull matrix {{1,1,-1,-1},{1,-1,1,-1}};
Q = polyhedronFromHData(matrix{{1,0},{-1,0},{0,1},{0,-1}},matrix{{1},{1},{1},{1}});
assert(P == Q)
C1 = coneFromVData matrix {{1,2},{2,1}};
C2 = coneFromHData matrix {{2,-1},{-1,2}};
assert(C1 == C2)
///

-- Test 9
-- Checking contains for polyhedra
TEST ///
P1 = convexHull matrix {{0,1,1,0},{0,0,1,1}};
P2 = convexHull matrix {{0,2,0},{0,0,2}};
assert contains(P2,P1)
assert(not contains(P1,P2))
P1 = convexHull(matrix {{0,1,1,0},{0,0,1,1},{0,0,0,0}},matrix {{0},{0},{1}});
P2 = convexHull matrix {{0,1,1,0},{0,0,1,1},{1,2,3,4}};
assert(not contains(P2,P1))
assert contains(P1,P2)
///

-- Test 14
-- Checking bipyramid, faces and fVector
TEST ///
P = convexHull matrix {{0,-1,1,0,0,1,-1},{0,0,0,1,-1,-1,1}};
P = bipyramid P;
F2 = set {matrix{{-1_QQ},{0},{0}},matrix{{1_QQ},{0},{0}},matrix{{0_QQ},{1},{0}},matrix{{0_QQ},{-1},{0}},matrix{{1_QQ},{-1},{0}},matrix{{-1_QQ},{1},{0}},matrix{{0_QQ},{0},{1}},matrix{{0_QQ},{0},{-1}}};
desired = sort matrix {elements F2};
assert(desired == vertices P)
assert(fVector P == {8,18,12,1})
///

-- Test 15
-- Checking isEmpty
TEST ///
P = polyhedronFromHData(matrix{{1,1,1},{-1,0,0},{0,-1,0},{0,0,-1}},matrix{{1},{0},{0},{0}});
assert not isEmpty P
P = intersection {P, polyhedronFromHData(matrix{{-1,-1,-1}},matrix{{-2}})};
assert isEmpty P
///

-- Test 38
-- Checking latticePoints
TEST ///
P = convexHull matrix {{1,-1,0,0},{0,0,1,-1}};
LP = latticePoints P;
LP1 = {matrix {{1},{0}},matrix {{-1},{0}},matrix {{0},{1}},matrix {{0},{-1}},matrix {{0},{0}}};
assert(set LP === set LP1)
P = polyhedronFromHData(matrix {{-6,0,0},{0,-6,0},{0,0,-6},{1,1,1}},matrix{{-1},{-1},{-1},{1}});
assert(latticePoints P == {})
///

-- Test 38
-- Checking if Polyhedra live over QQ
TEST ///
M = matrix {{1,1,1,1,-1,-1,-1,-1},{1,1,-1,-1,1,1,-1,-1},{1,-1,1,-1,1,-1,1,-1}};
P = convexHull M;
assert(ring vertices P === QQ)
v = matrix {{1},{1},{1}};
I = (M,v);
P = polyhedronFromHData I;
l = facets P;
assert(ring l#0 === QQ)
assert(ring l#1 === QQ)
///
