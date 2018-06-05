-- Test Square Triangualtion
TEST ///
P = convexHull matrix {{1,0,1,0},{1,1,0,0}};
C1 = matrix {{0,1,0},{0,0,1}}; 
C2 = matrix {{1,0,1},{0,1,1}};
w = matrix {{1,0,1,0}};
S = regularSubdivision (P,w);
assert(S#0 == convexHull C1)
assert(S#1 == convexHull C2)
///


-- Test Square Subdivision
TEST ///
P = convexHull matrix {{1,0,1,0},{1,1,0,0}};
w = matrix {{1,1,0,0}};
S = regularSubdivision (P,w);
assert(S#0 == P)
///
