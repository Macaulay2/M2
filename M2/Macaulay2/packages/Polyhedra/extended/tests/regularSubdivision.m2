-- Test Square Triangulation Input Polytope
TEST ///
P = convexHull matrix {{1,0,1,0},{1,1,0,0}};
C1 = matrix {{0,1,0},{0,0,1}};
C2 = matrix {{1,0,1},{0,1,1}}; 
w = matrix {{1,0,0,1}};
S = regularSubdivision (P,w);
assert(S#0 == convexHull C1)
assert(S#1 == convexHull C2)

-- Test Square Subdivision Input Polytope
P = convexHull matrix {{1,0,1,0},{1,1,0,0}};
w = matrix {{1,1,0,0}};
S = regularSubdivision (P,w);
assert(S#0 == P)

--- Test Square Triangulation Input Points 
M = matrix {{1,0,1,0},{1,1,0,0}}; 
w = matrix {{1,0,0,1}};
C1 = matrix {{1,0,1},{1,1,0}}; 
C2 = matrix {{0,1,0},{1,0,0}};
S = regularSubdivision (M,w); 
assert(convexHull M_(S#0) == convexHull C1)
assert(convexHull M_(S#1) == convexHull C2)

---Test Square Subdivision Input Points 
M = matrix {{1,0,1,0},{1,1,0,0}}; 
w = matrix {{1,1,0,0}};
S = regularSubdivision (M,w);
assert (convexHull M_(S#0) == convexHull M)

A = matrix {
 {0, -1, 2, 3, 4, -5, 6}, 
 {0, 1, -4, 9, 16, 25, 36}, 
 {0, 1, 8, -27, 64, 125, -216}};
weights = matrix{{24235/1467081, 77731/5029992, 17659/838332, 0, 0, 0, 0}};
tri0 = regularSubdivision(A, weights)
ans = {{0, 1, 2, 3}, {0, 1, 3, 6}, {0, 2, 3, 6}, {1, 2, 3, 4}, {1, 2, 4, 5},
{1, 3, 4, 5}, {1, 3, 5, 6}, {2, 3, 4, 6}, {3, 4, 5, 6}}
assert(ans == tri0)

M0 = matrix{{0,0,1,1},{0,1,0,1}};
RS0 = regularSubdivision(M0, matrix {{1,0,0,0}})
RS1 = regularSubdivision(M0, matrix {{0,1,0,0}})
RS2 = regularSubdivision(M0, matrix {{0,0,1,0}})
RS3 = regularSubdivision(M0, matrix {{0,0,0,1}})
ans0 = {{0,1,2},{1,2,3}}
ans1 = {{0,1,3},{0,2,3}}
assert(RS0 == ans0)
assert(RS1 == ans1)
assert(RS2 == ans1)
assert(RS3 == ans0)
///
