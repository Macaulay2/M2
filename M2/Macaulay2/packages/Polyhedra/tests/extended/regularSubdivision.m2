-- Test Square Triangulation Input Polytope
TEST ///
P = convexHull matrix {{1,0,1,0},{1,1,0,0}};
C1 = matrix {{0,1,1},{0,0,1}}; 
C2 = matrix {{0,1,0},{0,0,1}};
w = matrix {{1,0,1,0}};
S = regularSubdivision (P,w);
assert(S#0 == convexHull C1)
assert(S#1 == convexHull C2)
///


-- Test Square Subdivision Input Polytope
TEST ///
P = convexHull matrix {{1,0,1,0},{1,1,0,0}};
w = matrix {{1,1,0,0}};
S = regularSubdivision (P,w);
assert(S#0 == P)
///


--- Test Square Triangulation Input Points 
TEST ///
M = matrix {{1,0,1,0},{1,1,0,0}}; 
w = matrix {{1,0,0,1}};
C1 = matrix {{1,0,1},{1,1,0}}; 
C2 = matrix {{1,0,0},{1,1,0}};
S = regularSubdivision (M,w); 
assert(convexHull M_(S#0) == convexHull C1)
assert(convexHull M_(S#1) == convexHull C2)
///


---Test Square Subdivision Input Points 
TEST ///
M = matrix {{1,0,1,0},{1,1,0,0}}; 
w = matrix {{1,1,0,0}};
S = regularSubdivision (M,w);
assert (convexHull M_(S#0) == convexHull M)
///
