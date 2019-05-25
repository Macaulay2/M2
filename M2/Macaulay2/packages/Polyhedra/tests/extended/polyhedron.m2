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
