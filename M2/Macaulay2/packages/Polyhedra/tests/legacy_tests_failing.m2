






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

-- Test 45
-- Checking pyramid
TEST ///
P = intersection(matrix {{1,0},{-1,0},{0,1},{0,-1}},matrix {{1},{1},{1},{1}});
P = pyramid P;
Q = convexHull matrix {{1,1,-1,-1,0},{1,-1,1,-1,0},{0,0,0,0,1}};
assert(P == Q)
///

-- Test 50
-- Checking hirzebruch
TEST ///
F = hirzebruch 3;
F1 = fan {posHull matrix{{1,0},{0,1}},posHull matrix{{1,0},{0,-1}},posHull matrix{{0,-1},{1,3}},posHull matrix{{0,-1},{-1,3}}};
assert(F == F1)
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

-- Test 55
-- Checking equality of polyhedral objects
TEST ///
L1 = set {posOrthant 3, hypercube 2, crossPolytope 4, hirzebruch 5};
L2 = set {hirzebruch 5, posOrthant 3, hypercube 2, crossPolytope 4};
assert(L1 === L2)
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
desired = {(L#2,posHull matrix {{0,-1},{1,3}}),(L#2,posHull matrix {{0,-1},{-1,3}})};
computed = incompCones(L#2,L#4);
assert(sort computed == sort desired)
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


