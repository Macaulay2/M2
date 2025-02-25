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


TEST ///
A = transpose matrix{{0,0,0}, {1,0,0}, {0,1,0}, {1,1,3}}
P = convexHull A
assert not isVeryAmple P
///

TEST ///
raysP = transpose matrix {{1,0,0}}
linealityP = transpose matrix {{0,1,0}}
P = polyhedron coneFromVData(raysP, linealityP)
A = matrix{{1,0,0},{0,1,0}}
Q = affineImage(A,P)
assert(linealitySpace Q == promote(transpose matrix {{0,1}}, QQ))
///
