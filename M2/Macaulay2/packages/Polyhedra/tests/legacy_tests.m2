


-- Test 5
-- Checking polar
TEST ///
P = convexHull matrix {{1,1,-1,-1},{1,-1,1,-1}};
Q = convexHull matrix {{1,-1,0,0},{0,0,1,-1}};
P = polar P;
assert(P == Q)
P = convexHull(matrix {{1,-1,1,-1},{1,1,-1,-1},{1,2,3,4}},matrix {{0,0},{0,0},{1,-1}});
Q = convexHull matrix {{1,-1,0,0},{0,0,1,-1},{0,0,0,0}};
P = polar P;
assert(P == Q)
///


-- Test 7
-- Checking intersection that give a not pointed cone and intersection for lists
TEST ///
C = intersection matrix {{1,2,1},{2,1,1}};
assert(image linSpace C == image matrix{{1},{1},{-3}})
assert(ambDim C == 3)
P = intersection {hypercube 3,C,(matrix{{1,1,1}},matrix{{1}})};
V = matrix {{1/3,1,0,1,1,-1,-1/3},{1/3,0,1,1,-1,1,-1/3},{-1,-1,-1,-1,1,1,1}};
assert(vertices P == V);
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


-- Test 11
-- Checking equality for polyhedra and cones
TEST ///
P = convexHull matrix {{1,1,-1,-1},{1,-1,1,-1}};
Q = intersection(matrix{{1,0},{-1,0},{0,1},{0,-1}},matrix{{1},{1},{1},{1}});
assert(P == Q)
C1 = posHull matrix {{1,2},{2,1}};
C2 =intersection matrix {{2,-1},{-1,2}};
assert(C1 == C2)
///


-- Test 13
-- Checking faces and minkSummandCone
TEST ///
P = convexHull matrix {{0,-1,1,0,0,1,-1},{0,0,0,1,-1,-1,1}};
F1 = faces(1,P);
F2 = {convexHull matrix{{-1,0},{1,1}},convexHull matrix{{0,1},{1,0}},convexHull matrix{{1,1},{0,-1}},convexHull matrix{{1,0},{-1,-1}},convexHull matrix{{0,-1},{-1,0}},convexHull matrix{{-1,-1},{0,1}}};
assert(set F1 === set F2)
(C,L,M) = minkSummandCone(P);
assert(rays(C)*M == matrix{{1_QQ,1},{1,1},{1,1},{1,1},{1,1},{1,1}})
L1 = {convexHull matrix{{0,1},{0,0}},convexHull matrix{{0,0},{0,1}},convexHull matrix{{0,1},{0,-1}},convexHull matrix{{0,0,1},{0,1,0}},convexHull matrix{{0,1,1},{0,0,-1}}};
assert(set values L === set L1)
///

-- Test 14
-- Checking bipyramid, faces and fVector
TEST ///
P = convexHull matrix {{0,-1,1,0,0,1,-1},{0,0,0,1,-1,-1,1}};
P = bipyramid P;
F1 = set apply(faces(3,P), f -> vertices f);
F2 = set {matrix{{-1_QQ},{0},{0}},matrix{{1_QQ},{0},{0}},matrix{{0_QQ},{1},{0}},matrix{{0_QQ},{-1},{0}},matrix{{1_QQ},{-1},{0}},matrix{{-1_QQ},{1},{0}},matrix{{0_QQ},{0},{1}},matrix{{0_QQ},{0},{-1}}};
assert(isSubset(F1,F2) and isSubset(F2,F1))
assert(fVector P == {8,18,12,1})
///

-- Test 15
-- Checking isEmpty
TEST ///
P = intersection(matrix{{1,1,1},{-1,0,0},{0,-1,0},{0,0,-1}},matrix{{1},{0},{0},{0}});
assert not isEmpty P
P = intersection {P,(matrix{{-1,-1,-1}},matrix{{-2}})};
assert isEmpty P
///

-- Test 18
-- Checking is Face
TEST ///
C1 = posHull matrix {{1,1,1,1},{1,-1,0,0},{0,0,1,-1}};
C2 = posHull matrix {{1,1},{1,-1},{0,0}};
assert not isFace(C2,C1)
C2 = posHull matrix {{1},{1},{1}};
assert not isFace(C2,C1)
C2 = posHull matrix {{1},{0},{-1}};
assert isFace(C2,C1)
C2 = posHull matrix {{0},{0},{0}};
assert isFace(C2,C1)
///

-- Test 19
-- Checking isFace
TEST ///
P1 = convexHull matrix {{1,1,1,1,-1,-1,-1,-1},{1,1,-1,-1,1,1,-1,-1},{1,-1,1,-1,1,-1,1,-1}};
P2 = intersection(matrix {{1,0,0},{-1,0,0}},matrix {{-1},{-1}});
assert isFace(P2,P1)
P2 = convexHull matrix {{1,1,1},{1,1,-1},{1,-1,1}};
assert not isFace(P2,P1)
P2 = intersection {P2,{matrix{{0,1,0}},matrix{{1}}}};
assert isFace(P2,P1)
///

-- Test 20
-- Checking isCompact
TEST ///
P = intersection(matrix {{1,0,0},{-1,0,0},{0,1,0},{0,-1,0},{0,0,1}},matrix {{1},{2},{3},{4},{5}});
assert not isCompact P
P = intersection {P, (matrix {{0,0,-1}},matrix {{6}})};
assert isCompact P
P = intersection {P, {matrix {{1,1,1}},matrix {{0}}}};
assert isCompact P
///

-- Test 21
-- Checking tailCone
TEST ///
P = intersection(matrix {{1,0},{-1,0},{0,1}},matrix {{1},{2},{3}});
C = posHull matrix {{0},{-1}};
assert(tailCone P == C)
P = intersection (matrix{{2,1,1},{1,2,1},{1,1,2}},matrix{{2},{2},{2}});
C = posHull matrix{{1,1,-3},{1,-3,1},{-3,1,1}};
assert(tailCone P == C)
///

-- Test 22
-- Checking smallestFace for polyhedra
TEST ///
P = convexHull matrix {{1,1,1,1,-1,-1,-1,-1},{1,1,-1,-1,1,1,-1,-1},{1,-1,1,-1,1,-1,1,-1}};
F1 = convexHull matrix {{1,1,-1,-1},{1,-1,1,-1},{1,1,1,1}};
F2 = convexHull matrix {{1,1},{1,1},{-1,1}};
assert(smallestFace(matrix{{0},{0},{0}},P) == P)
assert(smallestFace(matrix{{1/2},{1/3},{1}},P) == F1)
assert(smallestFace(matrix{{1},{1},{3/4}},P) == F2)
///

-- Test 23
-- Checking smallestFace for cones
TEST ///
C = posHull matrix {{1,0,0},{0,1,0},{0,0,1}};
F1 = posHull matrix {{1,0},{0,1},{0,0}};
F2 = posHull matrix {{0},{0},{1}};
assert(smallestFace(matrix{{1},{2},{3}},C) == C)
assert(smallestFace(matrix{{2},{3},{0}},C) == F1)
assert(smallestFace(matrix{{0},{0},{5}},C) == F2)
///

-- Test 24
-- Checking inInterior for polyhedra and cones
TEST ///
P = convexHull matrix {{1,1,-1,-1},{1,-1,1,-1}};
assert not inInterior(matrix{{2},{1}},P)
assert not inInterior(matrix{{1},{0}},P)
assert inInterior(matrix{{0},{0}},P)
C = posHull matrix {{1,0,0},{0,1,0},{0,0,1}};
assert not inInterior(matrix{{0},{0},{0}},C)
assert not inInterior(matrix{{-1},{0},{0}},C)
assert inInterior(matrix{{1},{2},{3}},C)
///

-- Test 25
-- Checking interiorPoint
TEST ///
P = convexHull matrix {{1,-1,0,0},{0,0,1,-1}};
p = matrix {{0_QQ},{0}};
assert(interiorPoint P == p)
///

-- Test 26
-- Checking interiorVector
TEST ///
C = posHull matrix {{1,2,3},{2,3,1},{3,1,2}};
p = matrix {{1},{1},{1}};
assert(interiorVector C == p)
///

-- Test 27
-- Checking commonFace for polyhedra
TEST ///
P1 = convexHull matrix {{1,1,1,1,-1},{1,1,-1,-1,0},{1,-1,1,-1,0}};
P2 = intersection (matrix {{-1,0,0},{0,1,0},{0,-1,0},{0,0,1}},matrix {{-1},{1},{1},{1}});
assert not commonFace(P1,P2)
P2 = intersection {P2,(matrix {{0,0,-1}},matrix {{1}})};
assert commonFace(P1,P2)
///

-- Test 28
-- Checking commonFace for cones
TEST ///
C1 = posHull matrix {{1,2},{2,1}};
C2 = posHull matrix {{1,1},{1,0}};
assert not commonFace(C1,C2)
C1 = posHull matrix {{1,1},{2,1}};
assert commonFace(C1,C2)
///

-- Test 29
-- Checking areCompatible
TEST ///
C1 = posHull matrix {{1,0,0},{0,1,0},{0,0,1}};
C2 = posHull matrix {{1,1,0},{1,0,1},{0,-1,-1}};
assert not (areCompatible(C1,C2))#0
C2 = posHull {matrix {{1,0},{0,1},{0,0}}, C2};
assert (areCompatible(C1,C2))#0
///

-- Test 30
-- Checking fan and Fan basics
TEST ///
C = posHull matrix {{1,0,0},{0,1,0},{0,0,1}};
F = fan C;
assert(set maxCones F === set {C})
assert(ambDim(F) == 3)
assert(dim F == 3)
assert(F#"number of generating cones" == 1)
assert not isComplete F
///

-- Test 31
-- Checking fan and addCone
TEST ///
C = posHull matrix {{1,0,0},{0,1,0},{0,0,1}};
C1 = posHull matrix {{1,0,0},{0,-1,0},{0,0,1}};
C2 = posHull matrix {{-1,0,0},{0,1,0},{0,0,1}};
C3 = posHull matrix {{1,0,0},{0,1,0},{0,0,-1}};
F = fan {C,C1,C2,C3};
assert(set maxCones F === set {C,C1,C2,C3})
assert(ambDim(F) == 3)
assert(F#"number of generating cones" == 4)
assert(F#"isPure")
L = set {matrix {{1},{0},{0}},matrix {{-1},{0},{0}},matrix {{0},{1},{0}},matrix {{0},{-1},{0}},matrix {{0},{0},{1}},matrix {{0},{0},{-1}}};
assert(L === rays(F))
C = posHull matrix {{-1,0},{0,1},{0,0}};
F1 = addCone(C,F);
assert(F == F1)
///

-- Test 32
-- Checking fan, skeleton, isComplete, isPure, addCone, isPolytopal, polytope
TEST ///
C = posHull matrix {{1,0,0},{0,1,0},{0,0,1}};
C1 = posHull matrix {{1,0,0},{0,-1,0},{0,0,1}};
C2 = posHull matrix {{-1,0,0},{0,1,0},{0,0,1}};
C3 = posHull matrix {{1,0,0},{0,1,0},{0,0,-1}};
F = fan {C,C1,C2,C3};
L = {posHull matrix {{1,0},{0,-1},{0,0}},posHull matrix {{0,0},{0,-1},{1,0}},posHull matrix {{-1,0},{0,1},{0,0}},posHull matrix {{-1,0},{0,0},{0,1}},posHull matrix {{1,0},{0,0},{0,-1}},posHull matrix {{0,0},{1,0},{0,-1}},posHull matrix {{1,0},{0,1},{0,0}},posHull matrix {{1,0},{0,0},{0,1}},posHull matrix {{0,0},{1,0},{0,1}}};
assert(set L === set cones(2,F))
F1 = fan {posHull matrix {{1},{0},{0}},posHull matrix {{-1},{0},{0}},posHull matrix {{0},{1},{0}},posHull matrix {{0},{-1},{0}},posHull matrix {{0},{0},{1}},posHull matrix {{0},{0},{-1}}};
assert(skeleton(1,F) == F1)
assert not isComplete F
assert isPure F
C = posHull matrix {{-1,0,0},{0,-1,0},{0,0,-1}};
C1 = posHull matrix {{-1,0,0},{0,1,0},{0,0,-1}};
C2 = posHull matrix {{1,0,0},{0,-1,0},{0,0,-1}};
C3 = posHull matrix {{-1,0,0},{0,-1,0},{0,0,1}};
F = addCone({C,C1,C2,C3},F);
assert(F#"number of generating cones" == 8)
assert isPure F
assert isComplete F
assert isSmooth F
assert isPolytopal F
assert(normalFan polytope F == F)
///

-- Test 33
-- Checking isSmooth and smoothSubfan
TEST ///
C1 = posHull matrix {{1,2},{2,1}};
C2 = posHull matrix {{1,0},{2,1}};
C3 = posHull matrix {{1,2},{0,1}};
F = fan {C1,C2,C3};
assert not isSmooth F
F1 = fan {C2,C3};
assert(smoothSubfan F == F1)
///

-- Test 34
-- Checking normalFan
TEST ///
P = convexHull matrix {{1,0,0},{0,1,0}};
F = normalFan P;
L = {posHull matrix {{1,0},{0,1}},posHull matrix {{1,-1},{0,-1}},posHull matrix {{0,-1},{1,-1}}};
assert(F == fan L)
P =  convexHull (matrix {{1,0,0},{0,1,0}},matrix {{1},{1}});
F = normalFan P;
L = {posHull matrix {{1,0},{0,1}},posHull matrix {{1,1},{0,-1}},posHull matrix {{0,-1},{1,1}}};
assert(F == fan L)
///

-- Test 35
--Checking ccRefinement
TEST ///
M = matrix {{1,-1,0,0},{0,0,1,-1},{1,1,1,1}};
F = ccRefinement M;
F1 = fan {posHull matrix {{1,0,0},{0,1,0},{1,1,1}},posHull matrix {{-1,0,0},{0,1,0},{1,1,1}},posHull matrix {{-1,0,0},{0,-1,0},{1,1,1}},posHull matrix {{1,0,0},{0,-1,0},{1,1,1}}};
assert(F == F1)
///

-- Test 36
-- Checking imageFan
TEST ///
C = posHull matrix {{1,1,-1,-1},{1,-1,1,-1},{1,1,1,1}};
F = imageFan(matrix {{1,0,0},{0,1,0}},C);
F1 = fan {posHull matrix {{1,1},{1,-1}},posHull matrix {{1,-1},{1,1}},posHull matrix {{-1,-1},{1,-1}},posHull matrix {{1,-1},{-1,-1}}};
assert(F == F1)
F = imageFan(matrix {{1,2,0},{0,0,1}},C);
F1 = fan {posHull matrix {{-3,-1},{1,1}},posHull matrix {{-1,1},{1,1}},posHull matrix {{1,3},{1,1}}};
assert(F == F1)
///

-- Test 37
-- Checking hilbertBasis
TEST ///
C = posHull matrix {{1,2},{2,1}};
H = hilbertBasis C;
L = {matrix {{1},{1}},matrix {{2},{1}},matrix {{1},{2}}};
assert(set H === set L)
C = posHull matrix {{1,1,0},{0,3,0},{0,0,1}};
H = hilbertBasis C;
L = {matrix {{1},{0},{0}},matrix {{1},{1},{0}},matrix {{1},{2},{0}},matrix {{1},{3},{0}},matrix {{0},{0},{1}}};
assert(set H === set L)
///

-- Test 38
-- Checking latticePoints
TEST ///
P = convexHull matrix {{1,-1,0,0},{0,0,1,-1}};
LP = latticePoints P;
LP1 = {matrix {{1},{0}},matrix {{-1},{0}},matrix {{0},{1}},matrix {{0},{-1}},matrix {{0},{0}}};
assert(set LP === set LP1)
P = intersection(matrix {{-6,0,0},{0,-6,0},{0,0,-6},{1,1,1}},matrix{{-1},{-1},{-1},{1}});
assert(latticePoints P == {})
///

-- Test 39
-- Checking minkowskiSum
TEST ///
P1 = convexHull matrix {{1,0,0},{0,1,0}};
P2 = convexHull matrix {{-1,0,0},{0,-1,0}};
P1 = minkowskiSum(P1,P2);
P2 = convexHull matrix {{1,1,0,0,-1,-1},{-1,0,-1,1,0,1}};
assert(P1 == P2)
P1 = convexHull matrix {{1,1,-1,-1},{1,-1,1,-1},{0,0,0,0}};
P2 = convexHull matrix {{0,0},{0,0},{1,-1}};
P1 = minkowskiSum(P1,P2);
P2 = convexHull matrix {{1,1,1,1,-1,-1,-1,-1},{1,1,-1,-1,1,1,-1,-1},{1,-1,1,-1,1,-1,1,-1}};
assert(P1 == P2)
///

-- Test 40
-- Checking directProduct
TEST ///
P1 = convexHull matrix {{1,-1}};
P2 = convexHull matrix {{1,1,-1,-1},{1,-1,1,-1}};
P1 = directProduct(P1,P2);
P2 = convexHull matrix {{1,1,1,1,-1,-1,-1,-1},{1,1,-1,-1,1,1,-1,-1},{1,-1,1,-1,1,-1,1,-1}};
assert(P1 == P2)
C1 = posHull matrix {{1,2},{2,1}};
C2 = posHull matrix {{1,0},{0,1}};
C1 = directProduct(C1,C2);
C2 = posHull matrix {{1,2,0,0},{2,1,0,0},{0,0,1,0},{0,0,0,1}};
assert(C1 == C2)
F1 = normalFan hypercube 1;
F2 = normalFan hypercube 2;
F3 = normalFan hypercube 3;
assert(F3 == directProduct(F1,F2))
///

-- Test 41
-- Checking affineImage for polyhedra
TEST ///
P = convexHull matrix {{1,1,-1,-1},{1,-1,1,-1}};
A = matrix {{1,2},{3,4}};
v = matrix {{-1},{1}};
P = affineImage(A,P,v);
Q = convexHull matrix {{2,-2,0,-4},{8,0,2,-6}};
assert(P == Q)
P = intersection(matrix{{-1,0,0},{0,-1,0},{0,0,-1}},matrix{{1},{1},{1}});
A = matrix {{0,2,0},{1,0,1},{0,0,2}};
v = matrix {{1},{1},{1}};
P = affineImage(A,P,v);
Q = convexHull(matrix{{-1},{-1},{-1}},matrix{{0,2,0},{1,0,1},{0,0,2}});
assert(P == Q)
///

-- Test 42
-- Checking affineImage for cones
TEST ///
C = posHull matrix {{1,1,2},{1,2,1},{2,1,1}};
A = matrix {{1,-1,0},{0,1,-1},{-1,0,1}};
C = affineImage(A,C);
C1 = posHull matrix {{0,-1,1},{-1,1,0},{1,0,-1}};
assert(C == C1)
///

-- Test 43
-- Checking affinePreimage for polyhedra
TEST ///
P = convexHull matrix {{1,1,-1,-1},{1,-1,1,-1}};
A = matrix {{1,2},{3,4}};
v = matrix {{-1},{1}};
P = affinePreimage(A,P,v);
Q = convexHull matrix {{0,-2,-4,-6},{0,1,3,4}};
assert(P == Q)
P = intersection(matrix{{-1,0,0},{0,-1,0},{0,0,-1}},matrix{{1},{1},{1}});
A = matrix {{0,2,0},{1,0,1},{0,0,2}};
v = matrix {{1},{1},{1}};
P = affinePreimage(A,P,v);
Q = convexHull(matrix{{-1},{-1},{-1}},matrix{{1,0,-1},{0,1,0},{0,0,1}});
assert(P == Q)
///

-- Test 44
-- Checking affinePreimage for cones
TEST ///
C = posHull matrix {{1,1,2},{1,2,1},{2,1,1}};
A = matrix {{1,-1,0},{0,1,-1},{-1,0,0}};
C = affinePreimage(A,C);
C1 = posHull matrix {{-2,-1,-1},{-3,-3,-2},{-4,-4,-4}};
assert(C == C1)
///

-- Test 45
-- Checking pyramid
TEST ///
P = intersection(matrix {{1,0},{-1,0},{0,1},{0,-1}},matrix {{1},{1},{1},{1}});
P = pyramid P;
Q = convexHull matrix {{1,1,-1,-1,0},{1,-1,1,-1,0},{0,0,0,0,1}};
assert(P == Q)
///

-- Test 46
-- Checking crossPolytope
TEST ///
P = crossPolytope(3,2);
Q = convexHull matrix {{2,-2,0,0,0,0},{0,0,2,-2,0,0},{0,0,0,0,2,-2}};
assert(P == Q)
///

-- Test 47
-- Checking cyclicPolytope
TEST ///
P = cyclicPolytope(3,5);
Q = convexHull matrix {{0,1,2,3,4},{0,1,4,9,16},{0,1,8,27,64}};
assert(P == Q)
///

-- Test 48
-- Checking emptyPolyhedron
TEST ///
P = emptyPolyhedron 2;
assert(dim P == -1)
assert(ambDim P == 2)
///

-- Test 49
-- Checking hypercube
TEST ///
P = hypercube (3,3);
Q = convexHull matrix {{3,3,3,3,-3,-3,-3,-3},{3,3,-3,-3,3,3,-3,-3},{3,-3,3,-3,3,-3,3,-3}};
assert(P == Q)
///

-- Test 50
-- Checking hirzebruch
TEST ///
F = hirzebruch 3;
F1 = fan {posHull matrix{{1,0},{0,1}},posHull matrix{{1,0},{0,-1}},posHull matrix{{0,-1},{1,3}},posHull matrix{{0,-1},{-1,3}}};
assert(F == F1)
///

-- Test 51
-- Checking newtonPolytope
TEST ///
R = QQ[a,b,c];
f = a^2*b+b^3*c^2+c^4*a^3+a*b*c+a^5*c^6;
P =newtonPolytope f;
Q = convexHull matrix {{2,0,3,1,5},{1,3,0,1,0},{0,2,4,1,6}};
assert(P == Q)
///

-- Test 52
-- Checking posOrthant
TEST ///
C1 = posOrthant 3;
C2 = intersection matrix {{1,0,0},{0,1,0},{0,0,1}};
assert(C1 == C2)
///

-- Test 53
-- Checking statePolytope
TEST ///
R = QQ[a,b,c];
I = ideal(a^2-b,a*b-c);
(L,P) = statePolytope I;
Q = convexHull matrix {{21,3,1,1,6,2},{0,9,7,4,0,2},{0,0,2,4,5,5}};
L1 = { {{b^2,a*b,a^2}}, {{a*c,a*b,a^2,b^3}}, {{b,a^3}}, {{c^2,a*b,a*c,a^2}}, {{c,b}}, {{a^2,c}}};
L = apply(L,entries);
assert(P == Q)
assert(set L === set L1)
///

-- Test 54
-- Checking stdSimplex
TEST ///
P = stdSimplex 2;
Q = intersection(matrix{{-1,0,0},{0,-1,0},{0,0,-1}},matrix{{0},{0},{0}},matrix{{1,1,1}},matrix{{1}});
assert(P == Q)
///

-- Test 55
-- Checking equality of polyhedral objects
TEST ///
L1 = set {posOrthant 3, hypercube 2, crossPolytope 4, hirzebruch 5};
L2 = set {hirzebruch 5, posOrthant 3, hypercube 2, crossPolytope 4};
assert(L1 === L2)
///

-- Test 56
-- Checking vertexEdgeMatrix and vertexFacetMatrix
TEST ///
P = convexHull matrix {{0,-1,1,-1,1},{0,-1,-1,1,1},{-1,1,1,1,1}};
M = matrix {{0,1,2,3,4,5,6,7,8},{1,1,1,0,1,1,0,0,0},{2,1,0,1,0,0,0,1,0},{3,0,0,0,1,0,1,1,0},{4,0,1,1,0,0,0,0,1},{5,0,0,0,0,1,1,0,1}};
N = matrix {{0,1,2,3,4,5},{1,1,1,1,1,0},{2,1,0,1,0,1},{3,0,1,1,0,1},{4,1,0,0,1,1},{5,0,1,0,1,1}};
assert(vertexEdgeMatrix P == M)
assert(vertexFacetMatrix P == N)
///

-- Test 57
-- Checking minFace and maxFace
TEST ///
P = hypercube 3;
w = matrix {{1},{2},{1}};
F1 = convexHull matrix {{1},{1},{1}};
F2 = convexHull matrix {{-1},{-1},{-1}};
assert(F1 == maxFace(w,P))
assert(F2 == minFace(w,P))
C = posHull matrix {{2,-1,1},{-1,1,1},{0,-1,1}};
C1 = posHull matrix {{-1,2},{1,-1},{-1,0}};
assert(C1 == minFace(w,C))
///

-- Test 58
-- Checking proximum
TEST ///
P = crossPolytope 3;
p = matrix {{1},{2},{3}};
q = matrix {{0_QQ},{1},{0}};
assert(q == proximum(p,P))
p = matrix {{1},{1/2},{1}};
q = matrix {{1/2},{0},{1/2}};
assert(q == proximum(p,P))
P = convexHull map(QQ^3,QQ^3,1);
p = matrix {{2},{2},{0}};
q = matrix {{1/2},{1/2},{0}};
assert(q == proximum(p,P))
///

-- Test 59
-- Checking triangulate
TEST ///
P = crossPolytope 3;
L = triangulate P;
L = apply(L,convexHull);
L1 = {convexHull{matrix{{1},{0},{0}},matrix{{0},{1},{0}},matrix{{0},{0},{1}},matrix{{0},{0},{0}}},
     convexHull{matrix{{-1},{0},{0}},matrix{{0},{1},{0}},matrix{{0},{0},{1}},matrix{{0},{0},{0}}},
     convexHull{matrix{{1},{0},{0}},matrix{{0},{-1},{0}},matrix{{0},{0},{1}},matrix{{0},{0},{0}}},
     convexHull{matrix{{1},{0},{0}},matrix{{0},{1},{0}},matrix{{0},{0},{-1}},matrix{{0},{0},{0}}},
     convexHull{matrix{{-1},{0},{0}},matrix{{0},{-1},{0}},matrix{{0},{0},{1}},matrix{{0},{0},{0}}},
     convexHull{matrix{{-1},{0},{0}},matrix{{0},{1},{0}},matrix{{0},{0},{-1}},matrix{{0},{0},{0}}},
     convexHull{matrix{{1},{0},{0}},matrix{{0},{-1},{0}},matrix{{0},{0},{-1}},matrix{{0},{0},{0}}},
     convexHull{matrix{{-1},{0},{0}},matrix{{0},{-1},{0}},matrix{{0},{0},{-1}},matrix{{0},{0},{0}}}};
assert(set L === set L1)
///

-- Test 60
-- Checking volume
TEST ///
P = hypercube 3;
assert(volume P == 8)
P = crossPolytope 3;
assert(volume P == 4/3)
///

-- Test 61
-- Checking incompCones
TEST ///
L = {posHull matrix{{1,0},{1,1}},posHull matrix{{1,0},{0,-1}},posHull matrix{{-1,0},{0,1}},posHull matrix{{1,1},{0,1}},posHull matrix {{1,2},{2,1}}};
assert(set incompCones L === set {(L#0,L#4),(L#3,L#4)})
L = L_{0..3}|{hirzebruch 3};
assert(incompCones L == {(L#0,L#4),(L#2,L#4),(L#3,L#4)})
assert(set incompCones(L#2,L#4) === set {(L#2,posHull matrix {{0,-1},{1,3}}),(L#2,posHull matrix {{0,-1},{-1,3}})})
L = {posHull matrix {{-1,0},{0,1}},posHull matrix {{-1,0},{0,-1}},posHull matrix {{0,-1},{-1,3}},posHull matrix {{0,-1},{1,3}}};
L = {(L#0,L#2),(L#0,L#3),(L#1,L#2)};
assert(set incompCones(normalFan hypercube 2,hirzebruch 3) === set L)
///

-- Test 62
-- Checking isNormal for Cones
TEST ///
P = convexHull transpose matrix {{0,0,0},{1,0,0},{0,1,0},{1,1,3}};
Q = hypercube 2;
assert not isNormal P
assert isNormal Q
///

-- Test 63
-- Checking sublatticeBasis and toSublattice
TEST ///

-- new answer:
assert((sublatticeBasis matrix{{2,4,2,4},{1,2,2,3}}) === matrix({{2, 4}, {2, 3}}) );
-- assert(sublatticeBasis matrix{{2,4,2,4},{1,2,2,3}} == matrix {{2,2},{1,2}})

-- new answer:
assert( (sublatticeBasis convexHull matrix {{1,2,2},{0,-1,2}}) === map(ZZ^2,ZZ^2,{{0, 1}, {-1, 2}}) );
-- assert(sublatticeBasis convexHull matrix {{1,2,2},{0,-1,2}} == matrix {{-1,1},{1,0}})

assert(toSublattice convexHull matrix {{2,0},{0,3}} == convexHull matrix {{0,1}})
///

-- Test 64
-- Checking Scaling
TEST ///
assert(3/2 * hypercube(2,2) == hypercube(2,3))
///

-- Test 65
-- Checking ehrhart
TEST ///
P = convexHull transpose matrix {{0,0,0},{1,0,0},{0,1,0},{0,1,0}};
assert(ehrhart P == (1/2)*x^2+(3/2)*x+1)
///

-- Test 66
-- Checking isLatticePolytope
TEST ///
P = intersection(matrix{{2,0},{0,-3},{-3,0},{0,2}},matrix{{1},{1},{1},{1}})
assert not isLatticePolytope P
P = intersection(matrix{{2,0},{0,-3},{-3,0},{0,2}},matrix{{4},{6},{3},{6}})
assert isLatticePolytope P
///

-- Test 67
-- Checking isVeryAmple
TEST ///
P = convexHull matrix {{0,1,0,0,1,0,1,2,0,0},{0,0,1,0,1,0,2,2,0,-1},{0,0,0,1,2,0,1,2,0,-1},{0,0,0,0,-1,1,0,-1,0,1},{0,0,0,0,0,0,-1,-1,1,1}}
assert not isNormal P
assert isVeryAmple P
///

-- Test 68
-- Checking minkowskiSum for higher dimensions
TEST ///
p = convexHull transpose matrix {{1,0,0,0,0}}

p1 = convexHull transpose matrix {{0,0,0,0,0},{0,-1,0,0,0}}
p2 = convexHull transpose matrix {{0,0,0,0,0},{0,0,-1,0,0}}
p3 = convexHull transpose matrix {{0,0,0,0,0},{0,0,0,-1,0}}
p4 = convexHull transpose matrix {{0,0,0,0,0},{0,0,0,0,-1}}

r1 = convexHull transpose matrix {{0,0,0,0,0},{1,-1,-1,0,0}}
r2 = convexHull transpose matrix {{0,0,0,0,0},{1,-1,0,-1,0}}
r3 = convexHull transpose matrix {{0,0,0,0,0},{1,-1,0,0,-1}}
p = minkowskiSum(p,p1)
assert (numColumns vertices p == 2)
p = minkowskiSum(p,p2)
assert (numColumns vertices p == 4)
p = minkowskiSum(p,p3)
assert (numColumns vertices p == 8)
p = minkowskiSum(p,p4)
assert (numColumns vertices p == 16)
p = minkowskiSum(p,r1)
assert (numColumns vertices p == 32)
p = minkowskiSum(p,r2)
assert (numColumns vertices p == 56)
p = minkowskiSum(p,r3)
assert (numColumns vertices p == 92)
///

-- Test 69
-- Checking minkowskiSum for higher dimensions
-- One polyhedron with tail cone
-- Currently fails
TEST ///
nef = posHull matrix {{1, 1, 1, 1, 2, 1, 2, 2, 2, 2}, {0, 1, 0, 0, 1, 0, 1, 1, 0, 1}, {0, 0, 1, 0, 1, 0, 1, 0, 1, 1}, {0, 0, 0, 1, 1, 0, 0, 1, 1, 1}, {0, 0, 0, 0, 0, 1, 1, 1, 1, 1}}
p = convexHull transpose matrix {{1,0,0,0,0}}
p = p+nef
assert(numColumns rays p == 10)
p1 = convexHull transpose matrix {{0,0,0,0,0},{0,-1,0,0,0}}
p = p+p1
assert(numColumns rays p == 10)
///
