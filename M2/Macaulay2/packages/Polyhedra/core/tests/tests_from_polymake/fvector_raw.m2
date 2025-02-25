-- Test cayley_polytope/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,1},{0,0,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test cayley_polytope/c2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1,0,1,0},{0,0,1,1,0,0,1,1,0,0,1},{1,1,1,1,0,0,0,0,0,0,0},{0,0,0,0,1,1,1,1,0,0,0}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {11,25,22,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test cayley_polytope/c1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1,0,1,0},{0,0,1,1,0,0,1,1,0,0,1},{1,1,1,1,0,0,0,0,0,0,0},{0,0,0,0,1,1,1,1,0,0,0},{0,0,0,0,0,0,0,0,1,1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {11,25,22,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test cayley_polytope/c4.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,1,1,1,3},{0,0,1,1,0,0,4},{1,1,1,1,0,0,0},{0,0,0,0,1,0,0},{0,0,0,0,0,1,0}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {7,19,26,19,7,1};
computed = fVector P;
assert(desired == computed)
///

-- Test cayley_polytope/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0},{0,0,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test cayley_polytope/c3.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,1,1,3},{0,0,1,1,0,4},{1,1,1,1,0,0},{0,0,0,0,1,0},{0,0,0,0,0,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,13,13,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test print_constraints/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,1,1,0,0,0},{1,0,0,1,1,0},{0,1,0,1,0,1},{0,0,1,0,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,12,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test delpezzo/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,0,0,0,-1,0,0,0,1,-1},{0,1,0,0,0,-1,0,0,1,-1},{0,0,1,0,0,0,-1,0,1,-1},{0,0,0,1,0,0,0,-1,1,-1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {10,40,60,30,1};
computed = fVector P;
assert(desired == computed)
///

-- Test delpezzo/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,0,0,0,0,-1,0,0,0,0,1},{0,1,0,0,0,0,-1,0,0,0,1},{0,0,1,0,0,0,0,-1,0,0,1},{0,0,0,1,0,0,0,0,-1,0,1},{0,0,0,0,1,0,0,0,0,-1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {11,50,100,85,26,1};
computed = fVector P;
assert(desired == computed)
///

-- Test delpezzo/3.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,0,0,0,0,0,-1,0,0,0,0,0,1},{0,1,0,0,0,0,0,-1,0,0,0,0,1},{0,0,1,0,0,0,0,0,-1,0,0,0,1},{0,0,0,1,0,0,0,0,0,-1,0,0,1},{0,0,0,0,1,0,0,0,0,0,-1,0,1},{0,0,0,0,0,1,0,0,0,0,0,-1,1}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {13,72,220,365,306,102,1};
computed = fVector P;
assert(desired == computed)
///

-- Test delpezzo/0.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,0,0,-1,0,0,1,-1},{0,1,0,0,-1,0,1,-1},{0,0,1,0,0,-1,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test cell_from_subdivision/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1},{0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1},{0,0,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0},{0,1,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0},{1,1,1,0,1,1,0,1,0,1,0,1,1,0,1,0,1,0,1},{1,1,1,1,0,1,1,0,1,0,1,0,1,1,0,1,0,1,0},{1,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0}};
raysP = map(QQ^7, QQ^0, 0);
linealityP = map(QQ^7, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {19,75,123,103,47,11,1};
computed = fVector P;
assert(desired == computed)
///

-- Test cell_from_subdivision/1-in.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},{0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1},{0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1},{0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,0,0,0,1,1,1,0,0,0,1,0,0,0,1,0},{1,0,1,1,0,1,1,0,0,1,0,1,1,0,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,0,0,1,0,0},{1,1,0,1,1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,0,1,0,1,0,1,0,0,1,0,0,0,1,0,0,0},{1,1,1,0,1,1,0,1,0,0,1,1,0,1,0,0,1,0,0,0,1,1,0,1,0,0,1,0,0,0,1,0,0,0,0}};
raysP = map(QQ^7, QQ^0, 0);
linealityP = map(QQ^7, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {35,210,350,245,84,14,1};
computed = fVector P;
assert(desired == computed)
///

-- Test cell_from_subdivision/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1},{0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1,1},{0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,1,1,1,0,0},{0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,0,0,0,1,1,1,0,0,0,0,0},{1,0,1,1,0,1,1,0,0,1,0,1,1,0,0,1,0,0,1,0,1,1,0,0,1,0,0,1,0,1},{1,1,0,1,1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,1,0,1,0,1,0,0,1,0,1,0},{1,1,1,0,1,1,0,1,0,0,1,1,0,1,0,0,1,0,0,1,1,0,1,0,0,1,0,0,0,0}};
raysP = map(QQ^7, QQ^0, 0);
linealityP = map(QQ^7, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {30,166,299,239,92,16,1};
computed = fVector P;
assert(desired == computed)
///

-- Test gc_closure/2-0.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,1/2},{0,0,11/2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test gc_closure/3-0.poly
-- Checking fVector
TEST ///
verticesP = matrix {{3/2,-1/2,1/2,1/2,1/2,1/2,1/2,1/2},{1/2,1/2,3/2,-1/2,1/2,1/2,1/2,1/2},{1/2,1/2,1/2,1/2,3/2,-1/2,1/2,1/2},{1/2,1/2,1/2,1/2,1/2,1/2,3/2,-1/2}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,24,32,16,1};
computed = fVector P;
assert(desired == computed)
///

-- Test gc_closure/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,0,0,1,0,1,0,1},{1,1,0,0,0,0,1,1},{0,0,0,0,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test gc_closure/4.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,0,0,1},{0,0,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test gc_closure/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1/2,1},{0,5,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test gc_closure/3.poly
-- Checking fVector
TEST ///
verticesP = matrix {{2/3,2/3,0,1/3,1/2,1/2,2/3,2/3,1/3,2/3,1/3,1/3,1/3,2/3,1/2,1/3,1/3,2/3,1,1/3,2/3,1/2,1/2,1/2},{2/3,2/3,1/2,1/3,0,1/2,1/3,1/3,2/3,1/3,1/3,1/3,1/3,2/3,1/2,2/3,2/3,1/3,1/2,2/3,2/3,1/2,1/2,1},{1/3,2/3,1/2,2/3,1/2,0,1/3,2/3,1/3,1/3,1/3,2/3,1/3,1/3,1/2,2/3,1/3,2/3,1/2,2/3,2/3,1/2,1,1/2},{2/3,1/3,1/2,2/3,1/2,1/2,2/3,1/3,1/3,1/3,1/3,1/3,2/3,1/3,0,1/3,2/3,2/3,1/2,2/3,2/3,1,1/2,1/2}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {24,88,96,32,1};
computed = fVector P;
assert(desired == computed)
///

-- Test gc_closure/4-0.poly
-- Checking fVector
TEST ///
verticesP = matrix {{3/2,-1/2,1/2,1/2},{1/2,1/2,3/2,-1/2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test gc_closure/1-0.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test SCHLEGEL_VERTEX_COLORS/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {16,32,24,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test VISUAL_DUAL_FACE_LATTICE/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test compress_incidence/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,1,3/2,5/2,5/2},{0,1/2,5/2,5/2,0,1/2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test compress_incidence/4.poly
-- Checking fVector
TEST ///
verticesP = matrix {{5/3,-1/3,-1,1,1/3},{1/3,-5/3,1,-1,5/3}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {5,5,1};
computed = fVector P;
assert(desired == computed)
///

-- Test compress_incidence/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1},{-1},{-1}};
raysP = matrix {{1,0,0},{0,1,0},{0,0,1}};
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,6,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test compress_incidence/5.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,15}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {2,1};
computed = fVector P;
assert(desired == computed)
///

-- Test compress_incidence/3.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1},{0,0}};
raysP = matrix {{0},{1}};
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test lawrence/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,1,2,2,3,3,0,0,0,0,0,0},{2,3,1,3,1,2,0,0,0,0,0,0},{3,2,3,1,2,1,0,0,0,0,0,0},{1,0,0,0,0,0,1,0,0,0,0,0},{0,1,0,0,0,0,0,1,0,0,0,0},{0,0,1,0,0,0,0,0,1,0,0,0},{0,0,0,1,0,0,0,0,0,1,0,0},{0,0,0,0,1,0,0,0,0,0,1,0},{0,0,0,0,0,1,0,0,0,0,0,1}};
raysP = map(QQ^9, QQ^0, 0);
linealityP = map(QQ^9, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {12,66,220,465,612,472,195,36,1};
computed = fVector P;
assert(desired == computed)
///

-- Test rand_sphere/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{3330883453830711/4503599627370496,853624150927223/2251799813685248,-2356553346050111/18014398509481984,8539723551892617/9007199254740992,-851543628576541/1125899906842624,5316837853882195/9007199254740992,-6538305985494021/9007199254740992,-1749267935005635/4503599627370496,7424579411304029/9007199254740992,-6887392800778413/18014398509481984},{-2298030817950255/18014398509481984,7698990389472477/9007199254740992,5054138723166979/9007199254740992,-4611252879029533/72057594037927936,7431314160210763/36028797018963968,3306083656494553/4503599627370496,-5119504026856145/9007199254740992,3819013085252451/4503599627370496,4143898238679627/9007199254740992,-5765394822723937/9007199254740992},{-5952332624486941/9007199254740992,-1596589986555531/4503599627370496,-7361860108925987/9007199254740992,-1402721951416725/4503599627370496,-1397989901669917/2251799813685248,3023262654016433/9007199254740992,-6977355308041419/18014398509481984,3248144300909891/9007199254740992,-1486048987021867/4503599627370496,-6002567455236229/9007199254740992}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {10,24,16,1};
computed = fVector P;
assert(desired == computed)
///

-- Test rand_sphere/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{3/32,7/64,3/4,-3/16,-5/32,-3/4,-1/4,1/64,-7/8,-1,5/8,5/32,-3/4,3/64,-7/32,-3/8,3/8,-1,-1/32},{5/8,5/8,3/8,-1/2,1,1/2,-3/32,-1/4,7/256,-1/4,-3/4,-7/128,5/8,-3/8,-7/128,-7/8,5/8,3/16,1},{-3/4,-3/4,-5/8,-3/4,1/4,1/4,1,1,-1/2,-3/16,3/8,-1,7/256,7/8,1,-3/8,3/4,5/64,5/16}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {19,51,34,1};
computed = fVector P;
assert(desired == computed)
///

-- Test zonotope/6.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,0,0,0,0,1,1,1,1,0,0,1,1,0,0,1},{0,0,2,0,2,2,0,0,2,2,2,2,2,0,0,0},{0,4,4,0,0,4,4,0,0,4,0,4,0,4,0,4},{0,0,8,8,0,8,0,8,0,0,8,0,8,8,0,8}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {16,32,24,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test zonotope/8.poly
-- Checking fVector
TEST ///
verticesP = matrix {{2,4,5,-1,1,4,3,2,5,0,-5,0,-2,1,-3,4,-7,-3,-2,-4,5,-9,-4,2,5,-11,2,-1,-10,-12,2,-7,0,3,-2,-4,0,0,-9,-12,0,-12,-3,-6,2,-1,-7,-6,-7,-9,-4,-4,-1,-11,-9,-12,-9,-11,-3,-5,-8,-6,-7,-10,-5,-3,-8,-4,-7,-3,-6,-9},{-2,0,1,-4,-1,3,4,-3,-1,6,-3,-3,-3,1,-4,2,-3,7,-1,-4,2,-1,7,-2,0,0,5,7,-1,1,4,-1,-2,0,6,-1,0,4,6,4,2,2,-3,-4,4,6,6,7,1,5,6,1,3,3,-2,3,5,1,-2,6,4,-3,5,3,4,4,2,5,3,2,0,-1},{0,-1,0,0,-3,0,0,2,2,1,-1,4,-3,-5,2,-3,1,0,-5,2,3,-2,0,5,5,2,-3,-2,2,-1,4,-4,7,7,3,-6,9,6,0,0,8,2,6,4,-6,-5,-2,2,-6,2,-4,-8,-8,3,5,-3,-3,5,9,5,5,7,-5,-5,7,8,7,-7,-7,10,10,8}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {72,138,68,1};
computed = fVector P;
assert(desired == computed)
///

-- Test zonotope/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1/2,-1/2,-1/2,1/2,-1/2,1/2,-1/2,1/2,-1/2,1/2,-1/2,1/2,-1/2,1/2,-1/2,1/2},{-1,-1,1,1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1},{-2,-2,-2,-2,2,2,2,2,2,-2,-2,-2,-2,2,2,2},{-4,-4,-4,-4,-4,4,4,4,4,4,4,4,4,-4,-4,-4}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {16,32,24,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test zonotope/4.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,1,-1,0,0},{1,-1,0,0,1,-1},{0,0,-1,1,-1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test zonotope/9.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,2,0,2,0,2,0,2,0,2,0,2},{0,0,2,2,4,4,0,0,2,2,4,4},{0,0,2,2,0,0,-2,-2,-4,-4,-2,-2}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {12,18,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test zonotope/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-7/2,7/2,-3/2,3/2},{-5,5,-2,2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test zonotope/3.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,-1,1},{-3/2,3/2,-7/2,7/2},{-2,2,-5,5}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test zonotope/7.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,2,2,0,1,0},{-1,-1,0,1,1,0},{0,-1,-2,-1,-2,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test representations/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test SIMPLICITY/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,0,0},{1,0,0,0,0},{0,0,1,0,2}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {5,6,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test SIMPLICITY/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,0},{1,0,0,0},{0,0,1,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,6,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test VERTEX_LABELS/Q4.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-4/29,-1/2,0,1/19,1/12,19/30,-7/12,1/2,2/3,1/6,4/29,3/19,2/3,2/29,0,0,1/2,7/12,-1/19,-2/29,-19/30,-1/12,-1/2,-2/3,-1/6,-2/3,-3/19},{2/29,-1/2,0,3/19,1/6,2/3,2/3,1/2,-19/30,-1/12,-2/29,-1/19,7/12,4/29,0,0,-1/2,-2/3,-3/19,-4/29,-2/3,-1/6,1/2,19/30,1/12,-7/12,1/19},{-5/29,2,1,3/19,1/4,19/10,-7/4,2,-19/10,-1/4,-5/29,-3/19,7/4,5/29,0,-1,-2,-7/4,3/19,5/29,19/10,1/4,-2,-19/10,-1/4,7/4,-3/19},{8/29,-1/2,0,5/19,1/4,-1/2,-1/2,-1/2,-1/2,1/4,8/29,5/19,-1/2,8/29,1/3,0,-1/2,-1/2,5/19,8/29,-1/2,1/4,-1/2,-1/2,1/4,-1/2,5/19}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {27,54,36,9,1};
computed = fVector P;
assert(desired == computed)
///

-- Test VERTEX_LABELS/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test 4ti2/birkhoff2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,0},{0,1},{0,1},{1,0}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {2,1};
computed = fVector P;
assert(desired == computed)
///

-- Test 4ti2/zsolve.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,1,1,1,-1,-1,-1,-1},{-1,-1,1,1,1,1,-1,-1},{-1,1,1,-1,1,-1,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test 4ti2/hilb2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,2,2,1,0},{0,0,2,2,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {5,5,1};
computed = fVector P;
assert(desired == computed)
///

-- Test 4ti2/hilb1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,1,2,1},{0,0,1,1,1,2},{0,0,0,3,3,3}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,9,5,1};
computed = fVector P;
assert(desired == computed)
///

-- Test 4ti2/simplex.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,5/2,0},{0,0,5/2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test 4ti2/groeb2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,0,0,1,0,0},{0,1,0,0,1,0},{0,0,1,0,0,1},{0,1,1,0,0,0},{1,0,0,0,0,1},{0,0,0,1,1,0},{0,0,0,0,1,1},{0,0,1,1,0,0},{1,1,0,0,0,0}};
raysP = map(QQ^9, QQ^0, 0);
linealityP = map(QQ^9, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,15,18,9,1};
computed = fVector P;
assert(desired == computed)
///

-- Test 4ti2/ratsimplex.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,5/2,0},{0,0,5/2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test 4ti2/zsolve2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1},{0},{0}};
raysP = matrix {{1},{0},{0}};
linealityP = matrix {{0,0},{1,0},{0,1}};
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {2,1};
computed = fVector P;
assert(desired == computed)
///

-- Test 4ti2/ratcube.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1/2,1/2,-1/2,1/2},{-1/2,-1/2,1/2,1/2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test 4ti2/groeb-drl.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,1,0,-1,-1},{0,1,1,-1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {5,5,1};
computed = fVector P;
assert(desired == computed)
///

-- Test 4ti2/zsolve1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,3},{2,-1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {2,1};
computed = fVector P;
assert(desired == computed)
///

-- Test 4ti2/groeb-dl.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,1,0,-1,-1},{0,1,1,-1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {5,5,1};
computed = fVector P;
assert(desired == computed)
///

-- Test 4ti2/groeb-l.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,1,0,-1},{0,1,1,-1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test 4ti2/groeb1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,4}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {2,1};
computed = fVector P;
assert(desired == computed)
///

-- Test 4ti2/sparse.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,0,0,0,0,0},{0,1,0,0,0,0},{0,0,1,0,0,0},{0,0,0,1,0,0},{0,0,0,0,1,0}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,15,20,15,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test POSITIVE/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1},{0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {16,32,24,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test POSITIVE/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,-1,0,0,0,0,0,0},{0,0,1,-1,0,0,0,0},{0,0,0,0,1,-1,0,0},{0,0,0,0,0,0,1,-1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,24,32,16,1};
computed = fVector P;
assert(desired == computed)
///

-- Test split_compatibility_graph/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1},{-1,-1,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test split_compatibility_graph/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,2,1,0},{0,0,1,3,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {5,5,1};
computed = fVector P;
assert(desired == computed)
///

-- Test split_compatibility_graph/3.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test porta/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{3,5/3,1},{3,1,5/2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test porta/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{3,1,5/3},{3,5/2,1},{0,0,0},{2,4,1},{3,5,2}};
raysP = matrix {{0},{0},{1},{0},{0}};
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,6,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test VISUAL_MIN_MAX_FACE/in1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test VISUAL_MIN_MAX_FACE/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test VISUAL_MIN_MAX_FACE/in2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,910872158600853/1125899906842624,-910872158600853/1125899906842624,1/2,-1/2,1/2,-1/2,910872158600853/1125899906842624,-910872158600853/1125899906842624,0,0},{910872158600853/1125899906842624,910872158600853/1125899906842624,1/2,1/2,0,0,0,0,-1/2,-1/2,-910872158600853/1125899906842624,-910872158600853/1125899906842624},{1/2,-1/2,0,0,910872158600853/1125899906842624,910872158600853/1125899906842624,-910872158600853/1125899906842624,-910872158600853/1125899906842624,0,0,1/2,-1/2}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {12,30,20,1};
computed = fVector P;
assert(desired == computed)
///

-- Test spherize/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{5200308914369309/9007199254740992,-5200308914369309/9007199254740992,5200308914369309/9007199254740992,-5200308914369309/9007199254740992,5200308914369309/9007199254740992,-5200308914369309/9007199254740992,5200308914369309/9007199254740992,-5200308914369309/9007199254740992},{-5200308914369309/9007199254740992,-5200308914369309/9007199254740992,5200308914369309/9007199254740992,5200308914369309/9007199254740992,-5200308914369309/9007199254740992,-5200308914369309/9007199254740992,5200308914369309/9007199254740992,5200308914369309/9007199254740992},{-5200308914369309/9007199254740992,-5200308914369309/9007199254740992,-5200308914369309/9007199254740992,-5200308914369309/9007199254740992,5200308914369309/9007199254740992,5200308914369309/9007199254740992,5200308914369309/9007199254740992,5200308914369309/9007199254740992}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test spherize/1-in.poly
-- Checking fVector
TEST ///
verticesP = matrix {{2,-2,2,-2,2,-2,2,-2},{-2,-2,2,2,-2,-2,2,2},{-2,-2,-2,-2,2,2,2,2}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test random_edge_epl/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,0,0,1,0,1,0,1},{3/4,1,0,1/4,0,1/4,1,3/4},{3/16,1/4,0,1/16,1,15/16,3/4,13/16}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test free_sum_decomposition/2-in.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,-1,0,0,0,0},{0,1,1,-1,0,0},{0,0,0,0,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,12,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test free_sum_decomposition/3-in.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,1,0,1,1,0,0,-4},{1,0,1,-2,1,1,-3,1},{0,0,-1,1,1,1,1,-3}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test AFFINE_HULL/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,0},{1,0,0,0},{0,0,1,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,6,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test VISUAL_GRAPH/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test schlegel_params/1-in.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {16,32,24,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test schlegel_params/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {16,32,24,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test DUAL_BOUNDED_H_VECTOR/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-593934448380038187629003561336244223315615416320/581295835759584558808312033228316509435714702879,-779973149963902010794106307206482951439877406720/1148181310580490847889443245285958332842937431117,-834911336348711971508916753479457864880196943872/1033279699092212080961238485377359834950104619125,-451351638156153469675449537298017167288402182144/661074666395213201954148779083788761647422101087,-11875211446039377815154684977628597209263677374464/28146830865538587708960983240939498061618358464803,-1066404108435779557262026998475926048840047656960/2663035629934843887354436756168725949136781115679,-286667771360306766448084578481195066846882037760/1070022436834922326869526480917927343294110940591,-101857867966402892552331775300708148248320147456/313078221326677069773728886384385920072137617303,-247647508815704899421824604945096989996604194816/4715350869261984712862393959738846535578964133343,-231354719769776038518069933565989679675543126016/448263694500983649316778464851002479369071878087,1173995576268867024101222385236247095587712794624/1063363920455119185533336766028459859555986812289,10919588709302286410864254853031789456928047890432/201086830318867823857028347393140421252269033251,7737382246988530702401719335728566856035340386304/302903591957417203012304489947529736347874762775,-208965238551936641108002108112096334628087922688/969015032110259826352173485095262980905288420413,-859290202182082173684458899037542304891078180864/5007207875503957243795218944333844953080912307477,-10197621225091634689632545083717720162533638144/34796618359701895270882545993358473009022808147,-164860432541172988012500288848677826630810337280/369527502856320347152283004429443120261713839807,-4788459856255095265270714418435572229295055044608/17794019300086801999024109977398818324480077290739,-121768151542727502417327058247524448316084453376/214725024102500251121085398955994353664526185507,-4695507374188243411505217745040521455703542464512/8239742805158896400827962926559115961918687751871,-85837091223178700317543387027914465534606835712/134535282588166625496077801426867389035530698279,-580435442581073966343298317053478125353981444096/601721064087771031599502474452759076861582344741,-4399086184007617290603389456832361923188004749312/5587846899827315228018162025780624481889822155377,-8946398221587728817626161242544128840325987827712/11324914804645713522973305338138304500811523904485,-2380243102799262291488880640371567860996900913152/2500718746193551593444075481086648309079292861499,-2101645623657309528239208136369819998420526432256/2575644869312600520837378718018929479143740845201,-1478893849222614588217813826634995473583454552064/1537865163739176575553792502864197330470575328727,-1650966813993801114766861815173619324152071585792/1477194555638248894623263976557986753178854006673},{-22818873475658756052383747193627849323869896704/581295835759584558808312033228316509435714702879,976998916289555033383810311596041748929302233088/1148181310580490847889443245285958332842937431117,7586383608487890572942933955429067934453511225344/9299517291829908728651146368396238514550941572125,1714126851978983796318065960745923606019303276544/1983223999185639605862446337251366284942266303261,9175789268262439599570487075487982327846387318784/28146830865538587708960983240939498061618358464803,2118822138322137696317075292329089245204883439616/2663035629934843887354436756168725949136781115679,-1035135536219048502230860487599639901808833331200/1070022436834922326869526480917927343294110940591,-2151310265872916459076542960878097013057267957760/2191547549286739488416102204690701440504963321121,-4629211660552619529874925514650714422269913858048/4715350869261984712862393959738846535578964133343,-30783432736003875979750701542967690016264814592/64037670642997664188111209264428925624153125441,-293529744116052320987137638985152910776380424192/1063363920455119185533336766028459859555986812289,1818696859734868465859909813538812606321256300544/201086830318867823857028347393140421252269033251,-8797965056266683176252475579552797508633440026624/2726132327616754827110740409527767627130872864975,950884402381782865481050691257735479637701558272/969015032110259826352173485095262980905288420413,4910313066152228029763916645661983662521781846016/5007207875503957243795218944333844953080912307477,200489647855657583570731698405731449071592275968/521949275395528429063238189900377095135342122205,-264064968691283956425736981033314113190331678720/369527502856320347152283004429443120261713839807,3237718166968952441032952974617210505523697287168/17794019300086801999024109977398818324480077290739,-690405230055092879748857330835757135958038806528/1503075168717501757847597792691960475651683298549,-3750862582827464504403021450241614596972344246272/8239742805158896400827962926559115961918687751871,-103079032796038331763922255286326302856768913408/134535282588166625496077801426867389035530698279,-399722854162767122647551157340747551288997183488/6618931704965481347594527218980349845477405792151,-4191403943173474617574437652497846669758179573760/5587846899827315228018162025780624481889822155377,-8544545583914319080093878921340161755887043608576/11324914804645713522973305338138304500811523904485,-1570702742277753177347654238016256171368196866048/2500718746193551593444075481086648309079292861499,3067136313122161735618404786018771451266004418560/7726934607937801562512136154056788437431222535603,8017251242771651453532827175785619515969990819840/13840786473652589179984132525777775974235177958543,-210009175232972847106184266082692189666278375424/1477194555638248894623263976557986753178854006673},{292015572736985309319079896593271243862274211840/581295835759584558808312033228316509435714702879,406448713537773738248476345065189171331470458880/1148181310580490847889443245285958332842937431117,-2419017046828958004959018541626998047413981151232/9299517291829908728651146368396238514550941572125,605841361880050239986422753184209142956366495744/1983223999185639605862446337251366284942266303261,26376580871057029152502082943918053696927447384064/28146830865538587708960983240939498061618358464803,1829625818811552520540966823773129831638487793664/2663035629934843887354436756168725949136781115679,355357895910219463637798838165319385714427691008/1070022436834922326869526480917927343294110940591,-400465054874798630522403667777820192393459138560/2191547549286739488416102204690701440504963321121,2282559012109495953069810963444182641650457116672/4715350869261984712862393959738846535578964133343,361986275197950306925085973513804365362903384064/448263694500983649316778464851002479369071878087,4294441423989885021499182554914770621993064595456/3190091761365357556600010298085379578667960436867,-122506588092093177848048518135056155612152856576/201086830318867823857028347393140421252269033251,470849944371399163319599630506760597200342876160/109045293104670193084429616381110705085234914599,127886593915226235025779439534246475699425640448/969015032110259826352173485095262980905288420413,-1505216801932142153130468954670392960833913094144/5007207875503957243795218944333844953080912307477,-613844172024470845003066640846993936060801613824/521949275395528429063238189900377095135342122205,-254819809966568204935607306891143779378352095232/369527502856320347152283004429443120261713839807,127281167770811709473005771620679312072431894528/124433701399208405587581188653138589681678862173,1197126379723621998890849251275759964883179798528/1503075168717501757847597792691960475651683298549,6562719208971136758614765103246540865130763124736/8239742805158896400827962926559115961918687751871,42417100986611552576579593815379552102260932608/134535282588166625496077801426867389035530698279,3814345438454256446950114483520119135859390808064/6618931704965481347594527218980349845477405792151,-2796281770744980650596513001246509959046460604416/5587846899827315228018162025780624481889822155377,-1110906015150491879853805652350360710054221447168/2264982960929142704594661067627660900162304780897,-162999463660340098562820645803440974937427279872/2500718746193551593444075481086648309079292861499,-7039471309045169588769443884563022662595116007424/7726934607937801562512136154056788437431222535603,-7720896340977146716264447239622474994632773074944/13840786473652589179984132525777775974235177958543,319771745064582453583006035182977561718765387776/1477194555638248894623263976557986753178854006673}};
raysP = matrix {{1,1,1,1,1,1},{251112891175366054364274580161127/1742729142036330334599966324909420,-34025299559200432416283693893587/344714124034586031771472840661500,-166072625277962181545693392506793/1856806596362728607273125232693112,128406741685128773958560033805701/869050463315085459389094472964712,6841838086192401798777624145777/204686637756790519442275767776536,2131537604361729521355148099871/57850743173374257358441466703168},{-28909925018099237201050958950312/118822441502477068268179522152915,-11929325663257510348818223910588/86178531008646507942868210165375,24333199372073205901955950761380/232100824545341075909140654086639,-7193367473035672524891855261596/325893923743157047270910427361767,-1842628407698070068212035118957/4498607423226165262467599291792,-47544614283236920077152578537201/115701486346748514716882933406336}};
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {34,51,19,1};
computed = fVector P;
assert(desired == computed)
///

-- Test DUAL_BOUNDED_H_VECTOR/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,0,1,0},{0,0,1,0,0,1},{0,0,0,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,9,5,1};
computed = fVector P;
assert(desired == computed)
///

-- Test VISUAL_VERTEX_COLORS/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test VISUAL_VERTEX_COLORS/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,12,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test lattice_bipyramid/simpyr2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,3,0,0,1},{0,0,3,0,1},{0,0,0,3,-5}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {5,9,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test lattice_bipyramid/cubepyr1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,1},{-1,-1,-1,-1,1,1,1,1,-1,1},{0,0,0,0,0,0,0,0,2,-2}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {10,28,30,12,1};
computed = fVector P;
assert(desired == computed)
///

-- Test lattice_bipyramid/simplex.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,3,0},{0,0,3}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test lattice_bipyramid/cube.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test lattice_bipyramid/simpyr1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,3,0,1,1},{0,0,3,1,1},{0,0,0,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {5,9,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test vertex_barycenter/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1/2,-1/2,3/4,2,100},{2/3,2/3,2/3,3,200},{3/4,-3/4,1/2,4,300}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {5,9,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test vertex_barycenter/cr.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,-1,0,0,0,0,0,0},{0,0,1,-1,0,0,0,0},{0,0,0,0,1,-1,0,0},{0,0,0,0,0,0,1,-1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,24,32,16,1};
computed = fVector P;
assert(desired == computed)
///

-- Test TRIANGULATION/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test rand_aof/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,12,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test lattice_automorphisms/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,1,1,1,1,-1,-1,-1,-1},{1,0,1,-1,0,1,1,-3,-1,1},{-1,-1,1,1,0,0,1,1,-1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {10,15,7,1};
computed = fVector P;
assert(desired == computed)
///

-- Test lattice_automorphisms/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,1,0,-1,-1},{1,1,0,-1,-1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test VertexPerm/3p.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,-1,-1,1},{-1,-1,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test VertexPerm/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,12,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test VertexPerm/4.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test VertexPerm/4p.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,1,-1,1,-1,1,-1,-1},{1,-1,1,1,-1,-1,1,-1},{1,-1,-1,-1,1,1,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test VertexPerm/2p.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,1,1,1,0,0,1,1,0,1},{1,1,1,0,0,0,0,1,0,1},{0,0,1,0,1,1,0,0,0,0},{0,0,1,0,1,1,0,1,1,1},{0,1,1,1,0,1,0,0,1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {10,31,42,28,9,1};
computed = fVector P;
assert(desired == computed)
///

-- Test VertexPerm/1p.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,0,0,-1,0,0},{0,1,0,0,-1,0},{0,0,1,0,0,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,12,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test VertexPerm/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,0,1,0,1,1,1,0,1,1},{1,0,0,0,1,1,0,0,1,1},{0,1,0,0,0,0,0,1,0,1},{1,1,0,1,0,1,0,1,0,1},{0,0,0,1,0,1,1,1,1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {10,31,42,28,9,1};
computed = fVector P;
assert(desired == computed)
///

-- Test VertexPerm/5.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1},{-1,-1,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test VertexPerm/3.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1},{-1,-1,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test VertexPerm/5p.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,-1,-1,1},{-1,-1,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test congruent_polytopes/24cell-non-regular.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,-1,0,1,0,0,1,1,0,1,0,-1,-1,0,0,1,0,0,-1,0,-1,0,0,-1},{0,0,0,-1,0,0,0,-1,0,0,-1,0,0,-1,0,-1,0,-1,1,1,1,1,1,1},{-1,1,1,0,0,0,0,0,1,-1,1,1,0,0,-1,-1,-1,0,0,0,1,-1,0,0},{-1,1,-1,-1,-1,1,-1,0,0,0,0,0,1,0,0,0,1,1,1,-1,0,0,0,0}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {24,96,96,24,1};
computed = fVector P;
assert(desired == computed)
///

-- Test congruent_polytopes/24-cell.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,-1/2,-1/2,-1/2,-1/2,-1/2,-1/2,-1/2,-1/2,0,0,0,0,0,0,1/2,1/2,1/2,1/2,1/2,1/2,1/2,1/2,1},{0,-1/2,-1/2,-1/2,-1/2,1/2,1/2,1/2,1/2,-1,0,0,0,0,1,-1/2,-1/2,-1/2,-1/2,1/2,1/2,1/2,1/2,0},{0,-1/2,-1/2,1/2,1/2,-1/2,-1/2,1/2,1/2,0,-1,0,0,1,0,-1/2,-1/2,1/2,1/2,-1/2,-1/2,1/2,1/2,0},{0,-1/2,1/2,-1/2,1/2,-1/2,1/2,-1/2,1/2,0,0,-1,1,0,0,-1/2,1/2,-1/2,1/2,-1/2,1/2,-1/2,1/2,0}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {24,96,96,24,1};
computed = fVector P;
assert(desired == computed)
///

-- Test congruent_polytopes/polar_24-cell.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,-1,-1,-1,-1,-1,-1},{-1,0,-1,-1,0,1,1,1,-1,0,0,1,0,0,1,0,-1,0,0,1,0,0,0,-1},{-1,-1,0,1,1,-1,0,1,0,0,0,0,1,-1,0,1,0,-1,0,0,1,0,-1,0},{0,-1,-1,0,-1,0,-1,0,0,-1,1,0,0,0,1,1,1,1,1,0,0,-1,0,0}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {24,96,96,24,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/1c.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-5,0,6,0,5,-5},{6,-5,0,5,-5,0},{0,6,5,-5,0,-5},{5,0,-5,0,-5,6},{-5,5,0,-5,6,0},{0,-5,-5,6,0,5}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,15,20,15,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/13.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,9007199254740992/7286977268806823,-1,29472066294954575717671549009920/77158871278881291906588116670641,0,0,29472066294954575717671549009920/77158871278881291906588116670641},{6223821613304664/8566355544790271,0,-6223821613304664/8566355544790271,-90705693242519399071460246945792/77158871278881291906588116670641,0,0,90705693242519399071460246945792/77158871278881291906588116670641},{0,0,0,0,1,-1,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {7,15,10,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/17.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1},{1}};
raysP = matrix {{-1,0},{0,-1}};
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,2,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/8.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0},{1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,1,1,1,0},{1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0,1,1,1,0,0,0,1,1,1,0,1,1,1,0,1},{0,1,0,0,1,0,0,1,1,0,1,0,0,1,1,0,1,1,0,1,1,0,0,1,1,0,1,1,0,1,1,1,0,1,1},{0,0,1,0,0,1,0,1,0,1,0,1,0,1,0,1,1,0,1,1,0,1,0,1,0,1,1,0,1,1,1,0,1,1,1},{0,0,0,1,0,0,1,0,1,1,0,0,1,0,1,1,0,1,1,1,0,0,1,0,1,1,0,1,1,1,0,1,1,1,1}};
raysP = map(QQ^7, QQ^0, 0);
linealityP = map(QQ^7, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {35,210,350,245,84,14,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/1h.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-5,-5,0,0,5,6},{0,6,-5,5,-5,0},{-5,0,6,-5,0,5},{6,5,0,0,-5,-5},{0,-5,5,-5,6,0},{5,0,-5,6,0,-5}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,15,20,15,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-5,-5,0,0,5,6},{0,6,-5,5,-5,0},{-5,0,6,-5,0,5},{6,5,0,0,-5,-5},{0,-5,5,-5,6,0},{5,0,-5,6,0,-5}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,15,20,15,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/19.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/4.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,0,1,1,1},{1,1,1,0,0,0},{1,1,2,1,1,2},{1,2,1,1,2,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,9,5,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/3-cube+action.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/1b.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-5,0,6,0,5,-5},{6,-5,0,5,-5,0},{0,6,5,-5,0,-5},{5,0,-5,0,-5,6},{-5,5,0,-5,6,0},{0,-5,-5,6,0,5}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,15,20,15,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/1f.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-5,0,6,0,5,-5},{6,-5,0,5,-5,0},{0,6,5,-5,0,-5},{5,0,-5,0,-5,6},{-5,5,0,-5,6,0},{0,-5,-5,6,0,5}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,15,20,15,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/18.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,0,1,1,0,0,0,1,1},{0,0,1,0,1,0,1,1,0,0},{0,1,1,0,0,1,0,0,0,1},{1,1,0,0,0,0,0,1,1,0},{1,0,0,1,0,1,1,0,0,0}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {10,30,30,10,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/1d.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-5,0,6,0,5,-5},{6,-5,0,5,-5,0},{0,6,5,-5,0,-5},{5,0,-5,0,-5,6},{-5,5,0,-5,6,0},{0,-5,-5,6,0,5}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,15,20,15,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/3a.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,-1,0},{1,0,0,-1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/14.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,1},{0,0,1,1},{0,0,0,0},{0,0,0,0},{0,0,0,0}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/1a.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-5,0,6,0,5,-5},{6,-5,0,5,-5,0},{0,6,5,-5,0,-5},{5,0,-5,0,-5,6},{-5,5,0,-5,6,0},{0,-5,-5,6,0,5}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,15,20,15,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/1e.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-5,0,6,0,5,-5},{6,-5,0,5,-5,0},{0,6,5,-5,0,-5},{5,0,-5,0,-5,6},{-5,5,0,-5,6,0},{0,-5,-5,6,0,5}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,15,20,15,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/1g.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-5,-5,0,0,5,6},{0,6,-5,5,-5,0},{-5,0,6,-5,0,5},{6,5,0,0,-5,-5},{0,-5,5,-5,6,0},{5,0,-5,6,0,-5}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,15,20,15,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,1},{0,1,0},{1,0,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/3.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,0,0,1},{0,-1,1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/2a.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,1},{0,1,0},{1,0,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/12.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test action/7.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1},{-1,-1,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test edge_middle/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,-1,1,0,-1,1,0,-1,-1,1,1,0},{-1,0,0,1,-1,-1,-1,1,0,1,0,1},{-1,-1,-1,-1,0,0,1,0,1,0,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {12,24,14,1};
computed = fVector P;
assert(desired == computed)
///

-- Test edge_middle/1-in.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test centroid_volume/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {64,192,240,160,60,12,1};
computed = fVector P;
assert(desired == computed)
///

-- Test centroid_volume/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,1,-1,0},{-1,-1,1,1,2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {5,5,1};
computed = fVector P;
assert(desired == computed)
///

-- Test centroid_volume/3.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,12,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test permutahedron/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4},{2,2,3,3,4,4,1,1,3,3,4,4,1,1,2,2,4,4,1,1,2,2,3,3},{3,4,2,4,2,3,3,4,1,4,1,3,2,4,1,4,1,2,2,3,1,3,1,2},{4,3,4,2,3,2,4,3,4,1,3,1,4,2,4,1,2,1,3,2,3,1,2,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {24,36,14,1};
computed = fVector P;
assert(desired == computed)
///

-- Test permutahedron/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,2,-2,2,-2,2,-2,2,-2,2,-2,2,-2,2,-2,2,-2,3,-3,3,-3,3,-3,3,-3,3,-3,3,-3,3,-3,3,-3},{2,2,-2,-2,2,2,-2,-2,3,3,-3,-3,3,3,-3,-3,1,1,-1,-1,1,1,-1,-1,3,3,-3,-3,3,3,-3,-3,1,1,-1,-1,1,1,-1,-1,2,2,-2,-2,2,2,-2,-2},{3,3,3,3,-3,-3,-3,-3,2,2,2,2,-2,-2,-2,-2,3,3,3,3,-3,-3,-3,-3,1,1,1,1,-1,-1,-1,-1,2,2,2,2,-2,-2,-2,-2,1,1,1,1,-1,-1,-1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {48,72,26,1};
computed = fVector P;
assert(desired == computed)
///

-- Test permutahedron/3.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5},{2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,1,1,1,1,1,1,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,1,1,1,1,1,1,2,2,2,2,2,2,4,4,4,4,4,4,5,5,5,5,5,5,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,5,5,5,5,5,5,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4},{3,3,4,4,5,5,2,2,4,4,5,5,2,2,3,3,5,5,2,2,3,3,4,4,3,3,4,4,5,5,1,1,4,4,5,5,1,1,3,3,5,5,1,1,3,3,4,4,2,2,4,4,5,5,1,1,4,4,5,5,1,1,2,2,5,5,1,1,2,2,4,4,2,2,3,3,5,5,1,1,3,3,5,5,1,1,2,2,5,5,1,1,2,2,3,3,2,2,3,3,4,4,1,1,3,3,4,4,1,1,2,2,4,4,1,1,2,2,3,3},{4,5,3,5,3,4,4,5,2,5,2,4,3,5,2,5,2,3,3,4,2,4,2,3,4,5,3,5,3,4,4,5,1,5,1,4,3,5,1,5,1,3,3,4,1,4,1,3,4,5,2,5,2,4,4,5,1,5,1,4,2,5,1,5,1,2,2,4,1,4,1,2,3,5,2,5,2,3,3,5,1,5,1,3,2,5,1,5,1,2,2,3,1,3,1,2,3,4,2,4,2,3,3,4,1,4,1,3,2,4,1,4,1,2,2,3,1,3,1,2},{5,4,5,3,4,3,5,4,5,2,4,2,5,3,5,2,3,2,4,3,4,2,3,2,5,4,5,3,4,3,5,4,5,1,4,1,5,3,5,1,3,1,4,3,4,1,3,1,5,4,5,2,4,2,5,4,5,1,4,1,5,2,5,1,2,1,4,2,4,1,2,1,5,3,5,2,3,2,5,3,5,1,3,1,5,2,5,1,2,1,3,2,3,1,2,1,4,3,4,2,3,2,4,3,4,1,3,1,4,2,4,1,2,1,3,2,3,1,2,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {120,240,150,30,1};
computed = fVector P;
assert(desired == computed)
///

-- Test weighted_digraph_polyhedron/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0},{0},{0},{0}};
raysP = matrix {{-1,-1,-1,-1},{-1,0,0,-1},{-1,-1,0,0},{0,0,0,0}};
linealityP = matrix {{1},{1},{1},{1}};
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {5,8,5,1};
computed = fVector P;
assert(desired == computed)
///

-- Test SPECIAL_FACETS/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,0,0,1,-1},{0,1,-1,-1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {5,5,1};
computed = fVector P;
assert(desired == computed)
///

-- Test dual_linear_program/2-max.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0},{-1},{-1}};
raysP = matrix {{1},{0},{0}};
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {2,1};
computed = fVector P;
assert(desired == computed)
///

-- Test dual_linear_program/3-0.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test dual_linear_program/2-min.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0},{1},{1}};
raysP = matrix {{1},{0},{0}};
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {2,1};
computed = fVector P;
assert(desired == computed)
///

-- Test dual_linear_program/1-max.poly
-- Checking fVector
TEST ///
verticesP = matrix {{79/48,1,37/6,3},{65/48,2,43/6,0},{31/24,0,0,4},{0,0,31/3,0},{0,0,0,65/2},{0,31/2,0,0},{-53/12,-7,-52/3,1}};
raysP = matrix {{1,1,0},{1,0,0},{0,1,0},{2,1,0},{3,27/2,1},{0,0,1},{-2,1,0}};
linealityP = map(QQ^7, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {7,11,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test dual_linear_program/1-min.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1},{0},{0},{4},{43/2},{0},{3}};
raysP = matrix {{1,1,0},{1,0,0},{0,1,0},{2,1,0},{3,27/2,1},{0,0,1},{-2,1,0}};
linealityP = map(QQ^7, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,6,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test dual_linear_program/3-max.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0},{1},{0},{5},{0},{1}};
raysP = matrix {{1,0,0},{1,0,0},{0,1,0},{0,1,0},{0,0,1},{0,0,1}};
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,6,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test dual_linear_program/4-0.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0},{0,0,1}};
raysP = matrix {{1},{1}};
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test dual_linear_program/3-min.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1},{0},{5},{0},{1},{0}};
raysP = matrix {{1,0,0},{1,0,0},{0,1,0},{0,1,0},{0,0,1},{0,0,1}};
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,6,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test dual_linear_program/1-0.poly
-- Checking fVector
TEST ///
verticesP = matrix {{29,7,41/2,40,40,40,40,40},{7,7,21/2,21/2,7,21/2,7,245/24},{22,0,0,39/2,0,0,11,165/8},{2,2,3,3,2,3,2,35/12}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test CUBICAL/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {16,32,24,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test minkowski_cone/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{2,1,-1,-2,-1,1},{0,1,1,0,-1,-1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test minkowski_cone/s2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,-2,-1},{0,0,-1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test minkowski_cone/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{2,1,-1,-2,-1,1},{0,1,1,0,-1,-1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test minkowski_cone/s1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,-1,-2},{0,1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test SCHLEGEL_SOLID/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {16,32,24,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test TRIANGULATION_BOUNDARY/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,0},{0,0,1,0},{0,0,0,2}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,6,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test TRIANGULATION_BOUNDARY/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,-1,0,0,0,0,0,0},{0,0,1,-1,0,0,0,0},{0,0,0,0,1,-1,0,0},{0,0,0,0,0,0,1,-1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,24,32,16,1};
computed = fVector P;
assert(desired == computed)
///

-- Test 2-face-sizes-simple/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test 2-face-sizes-simple/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{5/8,-5/8,0,0,0,0,0,0},{0,0,5/8,-5/8,0,0,0,0},{0,0,0,0,5/8,-5/8,0,0},{0,0,0,0,0,0,5/8,-5/8}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,24,32,16,1};
computed = fVector P;
assert(desired == computed)
///

-- Test truncation/11-in.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,1,-1,0,0},{0,1,0,0,-1,0},{1,0,0,0,0,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,12,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test truncation/6.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1/4,-1/4,-1/4,-1/4,1/4,1/4,1/4,1/4,1/4,1/2,1/2,1/2,1/2,1/4,1/4,1/4,-1/4,-1/4,-1/4,-1/4,-1/2,-1/2,-1/2,-1/2},{-1/2,-1/2,-1/4,1/4,-1/2,-1/2,-1/4,1/4,1/2,-1/4,1/4,1/4,-1/4,1/2,1/4,-1/4,1/2,1/2,1/4,-1/4,1/4,1/4,-1/4,-1/4},{1/4,-1/4,1/2,1/2,-1/4,1/4,1/2,1/2,1/4,-1/4,-1/4,1/4,1/4,-1/4,-1/2,-1/2,1/4,-1/4,-1/2,-1/2,-1/4,1/4,1/4,-1/4}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {24,48,26,1};
computed = fVector P;
assert(desired == computed)
///

-- Test truncation/11.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,-1/2,0,1/2,1/2,1/2,1/2,0,-1/2,-1/2,0,-1/2},{-1/2,0,1/2,0,1/2,0,-1/2,1/2,1/2,0,-1/2,-1/2},{1/2,1/2,1/2,1/2,0,-1/2,0,-1/2,0,-1/2,-1/2,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {12,24,14,1};
computed = fVector P;
assert(desired == computed)
///

-- Test truncation/9-in.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1},{-1,-1,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test truncation/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1,1,1/2},{0,0,1,1,0,0,1,1,1/2,1},{0,0,0,0,1,1,1,1/2,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {10,15,7,1};
computed = fVector P;
assert(desired == computed)
///

-- Test truncation/2-in.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test truncation/4.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,0,1,0,0,1},{0,0,1,1,0,1},{1,0,0,0,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,12,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test truncation/9.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,0,0,-1},{0,1,-1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test truncation/5-in.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test truncation/1-in.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test truncation/8-in.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1},{0,0},{0,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {2,1};
computed = fVector P;
assert(desired == computed)
///

-- Test truncation/3-in.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test truncation/7-in.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1},{0,0},{0,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {2,1};
computed = fVector P;
assert(desired == computed)
///

-- Test truncation/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,1,0,1,1,2/3,1,1,0,0,1/3},{0,1,1,0,0,1,0,1/3,0,1,2/3,1},{0,0,0,1,1,1,0,0,1/3,2/3,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {12,18,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test truncation/5.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,1,0,1,1,0},{0,0,0,1/2,1,1,1/2,1},{1,1/2,0,0,0,1/2,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,16,10,1};
computed = fVector P;
assert(desired == computed)
///

-- Test truncation/3.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,0,0,1/2,1,1,1,1/2,1,1,1/2,1,1,1,1/2},{0,1,0,1,0,1/4,0,3/4,1,1,0,0,1/4,1,3/4,1},{0,0,1,1,0,0,1/4,0,0,1/4,3/4,1,1,3/4,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {16,24,10,1};
computed = fVector P;
assert(desired == computed)
///

-- Test truncation/4-in.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test truncation/6-in.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1/2,1/2,1/2,1/2,-1/2,-1/2,-1/2,-1/2,0,0,0,0},{1/2,-1/2,0,0,1/2,-1/2,0,0,1/2,1/2,-1/2,-1/2},{0,0,1/2,-1/2,0,0,1/2,-1/2,1/2,-1/2,1/2,-1/2}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {12,24,14,1};
computed = fVector P;
assert(desired == computed)
///

-- Test truncation/7.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1/2,1},{0,0},{0,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {2,1};
computed = fVector P;
assert(desired == computed)
///

-- Test FAR_HYPERPLANE/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,0},{1,0,0,0},{0,0,1,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,6,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test compose/in1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test compose/in2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{3/2,-3/2,0,0,0,0},{0,0,3/2,-3/2,0,0},{0,0,0,0,3/2,-3/2}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,12,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test facets_from_incidence/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1/3,-1/3,0,0,-1/3,0,0,1/3,1/3,2/3,2/3,2/3,2/3,1/3,1/3,0,0,0,0,-1/3,-2/3,-2/3,-2/3,-2/3},{-2/3,0,-2/3,-1/3,2/3,1/3,2/3,-2/3,2/3,0,1/3,0,-1/3,0,0,2/3,1/3,-1/3,-2/3,0,1/3,0,0,-1/3},{0,-2/3,-1/3,-2/3,0,-2/3,-1/3,0,0,1/3,0,-1/3,0,2/3,-2/3,1/3,2/3,2/3,1/3,2/3,0,1/3,-1/3,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {24,36,14,1};
computed = fVector P;
assert(desired == computed)
///

-- Test facets_from_incidence/3.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,-468/2581,-1716/5077,-156/359,-1/2,-156/359,-1716/5077,-468/2581,0,15600/2029},{156/329,1092/2581,1716/5077,468/2513,0,-468/2513,-1716/5077,-1092/2581,-156/329,0},{173/329,1333/2581,2581/5077,1265/2513,1/2,1265/2513,2581/5077,1333/2581,173/329,1873/2029}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {10,10,1};
computed = fVector P;
assert(desired == computed)
///

-- Test graph_from_face_lattice/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {16,32,24,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test graph_from_face_lattice/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {16,32,24,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test splits_in_subdivision/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test FACE_SIMPLICITY/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,1,0},{1,0,0,1,0},{0,0,1,1,2}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {5,9,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test FACE_SIMPLICITY/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,0},{1,0,0,0},{0,0,1,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,6,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test SCHLEGEL_CONSTRUCTION/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test product/3-B.poly
-- Checking fVector
TEST ///
verticesP = matrix {{2},{2}};
raysP = matrix {{1,1},{-1,1}};
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test product/2-B.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,2,2},{0,2,0,2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test product/4-A.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1},{0,1}};
raysP = matrix {{1},{0}};
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test product/1-A.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,1},{0,1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test product/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,0,0,0,0,0,0,1,1,1,1},{0,0,0,0,1,1,1,1,0,0,0,0},{0,0,2,2,0,0,2,2,0,0,2,2},{0,2,0,2,0,2,0,2,0,2,0,2}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {12,24,19,7,1};
computed = fVector P;
assert(desired == computed)
///

-- Test product/4.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1},{0,1},{2,2},{2,2}};
raysP = matrix {{1,0,0},{0,0,0},{0,1,1},{0,-1,1}};
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {5,10,10,5,1};
computed = fVector P;
assert(desired == computed)
///

-- Test product/3-A.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1},{0,1}};
raysP = matrix {{1},{0}};
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test product/4-B.poly
-- Checking fVector
TEST ///
verticesP = matrix {{2},{2}};
raysP = matrix {{1,1},{-1,1}};
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test product/1-B.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,2,2},{0,2,0,2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test product/3.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1},{0,1},{2,2},{2,2}};
raysP = matrix {{1,0,0},{0,0,0},{0,1,1},{0,-1,1}};
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {5,10,10,5,1};
computed = fVector P;
assert(desired == computed)
///

-- Test product/2-A.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,1},{0,1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test mapping_polytope/2-B.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,-1}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {2,1};
computed = fVector P;
assert(desired == computed)
///

-- Test mapping_polytope/1-A.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,-1}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {2,1};
computed = fVector P;
assert(desired == computed)
///

-- Test mapping_polytope/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1/2,0,1/2,0,1,1/2,1/2,0},{0,1/2,0,-1/2,0,0,-1/2,1/2,0},{1/2,0,0,0,1/2,0,1/2,1/2,1},{1/2,0,0,0,-1/2,0,1/2,-1/2,0}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {9,18,15,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test mapping_polytope/1-B.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0},{0,0,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test mapping_polytope/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,1,-1,1,-1,1,-1},{2,0,0,2,-2,0,-2,0},{0,-2,0,2,0,2,-2,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test mapping_polytope/2-A.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0},{0,0,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test CUBICALITY/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,0},{0,0,1,0},{0,0,0,2}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,6,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test CUBICALITY/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {16,32,24,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test canonical_coord/3R.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,-468/2581,-1716/5077,-156/359,-1/2,-156/359,-1716/5077,-468/2581,0,15600/2029},{156/329,1092/2581,1716/5077,468/2513,0,-468/2513,-1716/5077,-1092/2581,-156/329,0},{173/329,1333/2581,2581/5077,1265/2513,1/2,1265/2513,2581/5077,1333/2581,173/329,1873/2029}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {10,10,1};
computed = fVector P;
assert(desired == computed)
///

-- Test canonical_coord/2R.poly
-- Checking fVector
TEST ///
verticesP = matrix {{2,3/2},{3,2},{4,5/2},{5,3}};
raysP = matrix {{1,-1,0,0},{-1,1,0,0},{2,-2,1,0},{-2,2,-1,0}};
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {6,7,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test canonical_coord/4P.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1}};
raysP = matrix {{1}};
linealityP = map(QQ^1, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {2,1};
computed = fVector P;
assert(desired == computed)
///

-- Test canonical_coord/1R.poly
-- Checking fVector
TEST ///
verticesP = matrix {{2,3/2},{3,2},{4,5/2},{5,3}};
raysP = matrix {{1,-1,0},{3/2,2,0},{2,9/2,1},{3,-7/2,-1}};
linealityP = matrix {{1},{-1},{2},{-2}};
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {5,9,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test canonical_coord/5P.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0},{0}};
raysP = matrix {{0,1},{1,1}};
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {3,3,1};
computed = fVector P;
assert(desired == computed)
///

-- Test GRAPH_CONNECTED/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {2,1};
computed = fVector P;
assert(desired == computed)
///

-- Test pile/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},{-3/2,-3/2,-3/2,-3/2,-3/2,-3/2,-1/2,-1/2,-1/2,-1/2,-1/2,-1/2,1/2,1/2,1/2,1/2,1/2,1/2,3/2,3/2,3/2,3/2,3/2,3/2,-3/2,-3/2,-3/2,-3/2,-3/2,-3/2,-1/2,-1/2,-1/2,-1/2,-1/2,-1/2,1/2,1/2,1/2,1/2,1/2,1/2,3/2,3/2,3/2,3/2,3/2,3/2,-3/2,-3/2,-3/2,-3/2,-3/2,-3/2,-1/2,-1/2,-1/2,-1/2,-1/2,-1/2,1/2,1/2,1/2,1/2,1/2,1/2,3/2,3/2,3/2,3/2,3/2,3/2},{-5/2,-3/2,-1/2,1/2,3/2,5/2,-5/2,-3/2,-1/2,1/2,3/2,5/2,-5/2,-3/2,-1/2,1/2,3/2,5/2,-5/2,-3/2,-1/2,1/2,3/2,5/2,-5/2,-3/2,-1/2,1/2,3/2,5/2,-5/2,-3/2,-1/2,1/2,3/2,5/2,-5/2,-3/2,-1/2,1/2,3/2,5/2,-5/2,-3/2,-1/2,1/2,3/2,5/2,-5/2,-3/2,-1/2,1/2,3/2,5/2,-5/2,-3/2,-1/2,1/2,3/2,5/2,-5/2,-3/2,-1/2,1/2,3/2,5/2,-5/2,-3/2,-1/2,1/2,3/2,5/2},{19/18,11/18,7/18,7/18,11/18,19/18,5/6,7/18,1/6,1/6,7/18,5/6,5/6,7/18,1/6,1/6,7/18,5/6,19/18,11/18,7/18,7/18,11/18,19/18,17/18,1/2,5/18,5/18,1/2,17/18,13/18,5/18,1/18,1/18,5/18,13/18,13/18,5/18,1/18,1/18,5/18,13/18,17/18,1/2,5/18,5/18,1/2,17/18,19/18,11/18,7/18,7/18,11/18,19/18,5/6,7/18,1/6,1/6,7/18,5/6,5/6,7/18,1/6,1/6,7/18,5/6,19/18,11/18,7/18,7/18,11/18,19/18}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {72,174,139,37,1};
computed = fVector P;
assert(desired == computed)
///

-- Test simplex/7l.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,0,1},{0,0,2,2},{0,3,3,3}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {4,6,4,1};
computed = fVector P;
assert(desired == computed)
///

-- Test simplex/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,0,0},{0,0,1,0,0},{0,0,0,1,0},{0,0,0,0,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {5,10,10,5,1};
computed = fVector P;
assert(desired == computed)
///

-- Test simplex/6f.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,0,0,0,0,0,-1},{0,1,0,0,0,0,-1},{0,0,1,0,0,0,-1},{0,0,0,1,0,0,-1},{0,0,0,0,1,0,-1},{0,0,0,0,0,1,-1}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {7,21,35,35,21,7,1};
computed = fVector P;
assert(desired == computed)
///

-- Test simplex/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,5/8,0,0,0},{0,0,5/8,0,0},{0,0,0,5/8,0},{0,0,0,0,5/8}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {5,10,10,5,1};
computed = fVector P;
assert(desired == computed)
///

-- Test triang_boundary/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,12,6,1};
computed = fVector P;
assert(desired == computed)
///

-- Test triang_boundary/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,1,0,1,0,1,1/2},{0,0,1,1,0,0,1,1,1/2},{0,0,0,0,1,1,1,1,3/2}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {9,16,9,1};
computed = fVector P;
assert(desired == computed)
///

-- Test birkhoff/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,0,0,1,0,0,0,0,1,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0},{0,1,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,1},{0,0,1,0,0,1,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,1,0},{0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,0,0,1,1,0,0,1,0,0},{0,1,1,0,0,0,0,0,0,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0},{1,0,0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,1,0},{0,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0,0,1},{0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,1,1,0,0,0},{0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0},{0,0,1,1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0},{1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,1,1,0,0},{0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,1,1},{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1},{0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0},{0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0},{1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}};
raysP = map(QQ^16, QQ^0, 0);
linealityP = map(QQ^16, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {24,240,978,1968,2176,1392,528,120,16,1};
computed = fVector P;
assert(desired == computed)
///

-- Test birkhoff/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,0,0,0,1,0,1,0,0,0,0,0},{0,0,1,0,0,1,0,0,0,0,1,0},{0,1,0,0,0,0,0,0,1,0,0,1},{0,0,0,1,0,0,0,1,0,1,0,0},{0,1,0,0,0,1,0,1,0,0,0,0},{1,0,0,1,0,0,0,0,0,0,0,1},{0,0,1,0,0,0,1,0,0,1,0,0},{0,0,0,0,1,0,0,0,1,0,1,0},{0,0,1,1,0,0,0,0,1,0,0,0},{0,1,0,0,1,0,0,0,0,1,0,0},{1,0,0,0,0,0,0,1,0,0,1,0},{0,0,0,0,0,1,1,0,0,0,0,1},{0,0,0,0,0,0,0,0,0,1,1,1},{0,0,0,0,0,0,1,1,1,0,0,0},{0,0,0,1,1,1,0,0,0,0,0,0},{1,1,1,0,0,0,0,0,0,0,0,0}};
raysP = map(QQ^16, QQ^0, 0);
linealityP = map(QQ^16, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {12,66,220,492,768,840,624,288,64,1};
computed = fVector P;
assert(desired == computed)
///

-- Test split_polyhedron/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{2,4},{4,2},{4,2},{2,4}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {2,1};
computed = fVector P;
assert(desired == computed)
///

-- Test split_polyhedron/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{4,4/3,20/3,4,4,4/3,4/3,4/3,20/3,4/3,4,4,4/3,4,4,20/3,4,20/3,4,4/3,20/3,28/3},{4/3,4,4,4,4/3,20/3,4,20/3,4/3,4,20/3,20/3,28/3,20/3,4,4,4/3,4/3,4/3,4,4,4/3},{4/3,4,4/3,4/3,4,4,20/3,20/3,4/3,4,4/3,4,4,4/3,20/3,4,20/3,4,20/3,28/3,4,4/3},{28/3,20/3,4,20/3,20/3,4,4,4/3,20/3,20/3,4,4/3,4/3,4,4/3,4/3,4,4,4,4/3,4/3,4},{20/3,28/3,4,20/3,20/3,4,4,20/3,4/3,20/3,4,4/3,4,4/3,4/3,4/3,4,4,4/3,4,4,4/3},{4,4/3,4/3,4/3,4,4,20/3,4/3,20/3,4,4/3,4,4/3,4,20/3,4,20/3,4,28/3,20/3,4/3,4},{4,4/3,4,4,4/3,20/3,4,4/3,20/3,4,20/3,20/3,20/3,28/3,4,4,4/3,4/3,4,4/3,4/3,4},{4/3,4,20/3,4,4,4/3,4/3,20/3,4/3,4/3,4,4,4,4/3,4,20/3,4,20/3,4/3,4,28/3,20/3}};
raysP = map(QQ^8, QQ^0, 0);
linealityP = map(QQ^8, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {22,60,52,14,1};
computed = fVector P;
assert(desired == computed)
///

-- Test split_polyhedron/3.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1592262918131443/2251799813685248,4200624567719415541598157031017/20282409603651670423947251286016,1592262918131443/2251799813685248,4200624567719415541598157031017/20282409603651670423947251286016,1592262918131443/2251799813685248,4200624567719415541598157031017/20282409603651670423947251286016,1592262918131443/2251799813685248,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,12738103345051545/9007199254740992,12738103345051545/9007199254740992,1592262918131443/2251799813685248,1592262918131443/2251799813685248,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,1592262918131443/2251799813685248,1592262918131443/2251799813685248,12738103345051545/9007199254740992,1592262918131443/2251799813685248,12738103345051545/9007199254740992,12738103345051545/9007199254740992,1592262918131443/2251799813685248,4200624567719415541598157031017/20282409603651670423947251286016,24483034171371085553952995391895/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,24483034171371085553952995391895/20282409603651670423947251286016,12738103345051545/9007199254740992,1592262918131443/2251799813685248,1592262918131443/2251799813685248,12738103345051545/9007199254740992,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,24483034171371085553952995391895/20282409603651670423947251286016,1592262918131443/2251799813685248,1592262918131443/2251799813685248,12738103345051545/9007199254740992,12738103345051545/9007199254740992,1592262918131443/2251799813685248,24483034171371085553952995391895/20282409603651670423947251286016,24483034171371085553952995391895/20282409603651670423947251286016,12738103345051545/9007199254740992,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,9553577508788659/4503599627370496,38824863540916338353528385288599/20282409603651670423947251286016,1592262918131443/2251799813685248,1592262918131443/2251799813685248,38824863540916338353528385288599/20282409603651670423947251286016,1592262918131443/2251799813685248,1592262918131443/2251799813685248,9553577508788659/4503599627370496,1592262918131443/2251799813685248,1592262918131443/2251799813685248,44765443775022755566307833752773/20282409603651670423947251286016,24483034171371086679852902234519/10141204801825835211973625643008,24483034171371086679852902234519/10141204801825835211973625643008,53166692910461591153103775185303/20282409603651670423947251286016,9553577508788659/4503599627370496,38824863540916338353528385288599/20282409603651670423947251286016,1592262918131443/2251799813685248,9553577508788659/4503599627370496,1592262918131443/2251799813685248,38824863540916338353528385288599/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,12738103345051545/9007199254740992,1592262918131443/2251799813685248,24483034171371085553952995391895/20282409603651670423947251286016,24483034171371085553952995391895/20282409603651670423947251286016,1592262918131443/2251799813685248,12738103345051545/9007199254740992,12738103345051545/9007199254740992,1592262918131443/2251799813685248,24483034171371085553952995391895/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,1592262918131443/2251799813685248,12738103345051545/9007199254740992,12738103345051545/9007199254740992,1592262918131443/2251799813685248,24483034171371085553952995391895/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,24483034171371085553952995391895/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,34483405944844081/18014398509481984,48966068342742172439602104088983/20282409603651670423947251286016,47221509289895627/18014398509481984,48966068342742172439602104088983/20282409603651670423947251286016,34483405944844081/18014398509481984,47221509289895627/18014398509481984,34483405944844081/18014398509481984,34483405944844081/18014398509481984,1592262918131443/1125899906842624,1592262918131443/1125899906842624,1592262918131443/1125899906842624,1592262918131443/1125899906842624,12738103345051545/4503599627370496,53166692910461588901303961500055/20282409603651670423947251286016,53166692910461588901303961500055/20282409603651670423947251286016,24483034171371085553952995391895/10141204801825835211973625643008,38824863540916336101728571603351/20282409603651670423947251286016,19107155017577317/9007199254740992,38824863540916336101728571603351/20282409603651670423947251286016,19107155017577317/9007199254740992,38824863540916336101728571603351/20282409603651670423947251286016,19107155017577317/9007199254740992,38824863540916336101728571603351/20282409603651670423947251286016,19107155017577317/9007199254740992,21745302599792537/9007199254740992},{3730904090310553/18014398509481984,1592262918131443/2251799813685248,3730904090310553/18014398509481984,1592262918131443/2251799813685248,21745302599792537/18014398509481984,12738103345051545/9007199254740992,21745302599792537/18014398509481984,12738103345051545/9007199254740992,12738103345051545/9007199254740992,1592262918131443/2251799813685248,3730904090310553/18014398509481984,3730904090310553/18014398509481984,21745302599792537/18014398509481984,3730904090310553/18014398509481984,12738103345051545/9007199254740992,1592262918131443/2251799813685248,12738103345051545/9007199254740992,1592262918131443/2251799813685248,21745302599792537/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,21745302599792537/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,9553577508788659/4503599627370496,38824863540916338353528385288599/20282409603651670423947251286016,24483034171371086679852902234519/10141204801825835211973625643008,53166692910461591153103775185303/20282409603651670423947251286016,38824863540916338353528385288599/20282409603651670423947251286016,12738103345051545/9007199254740992,12738103345051545/9007199254740992,34483405944844083/18014398509481984,48966068342742174691401917774231/20282409603651670423947251286016,38824863540916338353528385288599/20282409603651670423947251286016,12738103345051545/4503599627370496,47221509289895627/18014398509481984,9553577508788659/4503599627370496,38824863540916338353528385288599/20282409603651670423947251286016,48966068342742172439602104088983/20282409603651670423947251286016,53166692910461588901303961500055/20282409603651670423947251286016,19107155017577317/9007199254740992,47221509289895627/18014398509481984,21745302599792537/9007199254740992,12738103345051545/9007199254740992,34483405944844081/18014398509481984,34483405944844083/18014398509481984,12738103345051545/9007199254740992,34483405944844081/18014398509481984,19107155017577317/9007199254740992,21745302599792537/9007199254740992,1592262918131443/2251799813685248,34483405944844081/18014398509481984,9553577508788659/4503599627370496,47221509289895627/18014398509481984,19107155017577317/9007199254740992,1592262918131443/2251799813685248,1592262918131443/2251799813685248,34483405944844081/18014398509481984,19107155017577317/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,6369051672525773/9007199254740992,34483405944844083/18014398509481984,21745302599792537/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,39759701109274521/18014398509481984,6369051672525773/9007199254740992,3730904090310553/18014398509481984,6369051672525773/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,34483405944844083/18014398509481984,6369051672525773/9007199254740992,1592262918131443/2251799813685248,9553577508788659/4503599627370496,3730904090310553/18014398509481984,3730904090310553/18014398509481984,6369051672525773/9007199254740992,6369051672525773/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,6369051672525773/9007199254740992,12738103345051545/9007199254740992,12738103345051545/9007199254740992,12738103345051545/9007199254740992,1592262918131443/2251799813685248,1592262918131443/2251799813685248,1592262918131443/2251799813685248,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,6369051672525773/9007199254740992,12738103345051545/9007199254740992,1592262918131443/2251799813685248,6369051672525773/9007199254740992,1592262918131443/2251799813685248,12738103345051545/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,6369051672525773/9007199254740992,12738103345051545/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,21745302599792537/18014398509481984,21745302599792537/18014398509481984,12738103345051545/9007199254740992,21745302599792537/18014398509481984,12738103345051545/9007199254740992,3730904090310553/18014398509481984,6369051672525773/9007199254740992,3730904090310553/18014398509481984,6369051672525773/9007199254740992,12738103345051545/9007199254740992,21745302599792537/18014398509481984,21745302599792537/18014398509481984,12738103345051545/9007199254740992,6369051672525773/9007199254740992,3730904090310553/18014398509481984,6369051672525773/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984},{9553577508788659/4503599627370496,38824863540916340605328198973847/20282409603651670423947251286016,9553577508788659/4503599627370496,38824863540916340605328198973847/20282409603651670423947251286016,9553577508788659/4503599627370496,9553577508788659/4503599627370496,38824863540916340605328198973847/20282409603651670423947251286016,38824863540916340605328198973847/20282409603651670423947251286016,6369051672525773/4503599627370496,24483034171371087805752809077143/10141204801825835211973625643008,34483405944844083/18014398509481984,48966068342742174691401917774231/20282409603651670423947251286016,6369051672525773/4503599627370496,53166692910461591153103775185303/20282409603651670423947251286016,34483405944844083/18014398509481984,48966068342742174691401917774231/20282409603651670423947251286016,6369051672525773/4503599627370496,53166692910461591153103775185303/20282409603651670423947251286016,34483405944844083/18014398509481984,47221509289895627/18014398509481984,34483405944844083/18014398509481984,6369051672525773/4503599627370496,21745302599792537/9007199254740992,47221509289895627/18014398509481984,12738103345051545/4503599627370496,24483034171371087805752809077143/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,24483034171371087805752809077143/20282409603651670423947251286016,24483034171371087805752809077143/20282409603651670423947251286016,24483034171371087805752809077143/20282409603651670423947251286016,24483034171371087805752809077143/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,12738103345051545/9007199254740992,12738103345051545/9007199254740992,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,12738103345051545/9007199254740992,6369051672525773/9007199254740992,12738103345051545/9007199254740992,12738103345051545/9007199254740992,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,38824863540916338353528385288599/20282409603651670423947251286016,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,24483034171371086679852902234519/10141204801825835211973625643008,38824863540916338353528385288599/20282409603651670423947251286016,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,19107155017577317/9007199254740992,19107155017577317/9007199254740992,19107155017577317/9007199254740992,53166692910461588901303961500055/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,19107155017577317/9007199254740992,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,38824863540916338353528385288599/20282409603651670423947251286016,38824863540916338353528385288599/20282409603651670423947251286016,24483034171371086679852902234519/10141204801825835211973625643008,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,44765443775022757818107647438021/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,12738103345051545/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,12738103345051545/9007199254740992,6369051672525773/9007199254740992,6369051672525773/9007199254740992,12738103345051545/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,24483034171371087805752809077143/20282409603651670423947251286016,24483034171371087805752809077143/20282409603651670423947251286016,24483034171371087805752809077143/20282409603651670423947251286016,12738103345051545/9007199254740992,6369051672525773/9007199254740992,6369051672525773/9007199254740992,12738103345051545/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,24483034171371087805752809077143/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,24483034171371087805752809077143/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,12738103345051545/9007199254740992,12738103345051545/9007199254740992,12738103345051545/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,12738103345051545/9007199254740992,6369051672525773/9007199254740992,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,12738103345051545/9007199254740992},{21745302599792537/18014398509481984,21745302599792537/18014398509481984,12738103345051545/9007199254740992,12738103345051545/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,1592262918131443/2251799813685248,1592262918131443/2251799813685248,12738103345051545/9007199254740992,1592262918131443/2251799813685248,12738103345051545/9007199254740992,1592262918131443/2251799813685248,12738103345051545/9007199254740992,1592262918131443/2251799813685248,3730904090310553/18014398509481984,3730904090310553/18014398509481984,21745302599792537/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,21745302599792537/18014398509481984,21745302599792537/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,1592262918131443/2251799813685248,12738103345051545/9007199254740992,12738103345051545/9007199254740992,12738103345051545/9007199254740992,1592262918131443/2251799813685248,1592262918131443/2251799813685248,1592262918131443/2251799813685248,1592262918131443/2251799813685248,12738103345051545/9007199254740992,12738103345051545/9007199254740992,6369051672525773/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,6369051672525773/9007199254740992,6369051672525773/9007199254740992,6369051672525773/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,6369051672525773/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,34483405944844083/18014398509481984,6369051672525773/9007199254740992,6369051672525773/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,6369051672525773/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,21745302599792537/9007199254740992,34483405944844083/18014398509481984,6369051672525773/9007199254740992,39759701109274521/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,19107155017577317/9007199254740992,34483405944844081/18014398509481984,19107155017577317/9007199254740992,34483405944844081/18014398509481984,1592262918131443/2251799813685248,1592262918131443/2251799813685248,1592262918131443/2251799813685248,21745302599792537/9007199254740992,9553577508788659/4503599627370496,47221509289895627/18014398509481984,1592262918131443/2251799813685248,9553577508788659/4503599627370496,34483405944844081/18014398509481984,12738103345051545/9007199254740992,19107155017577317/9007199254740992,47221509289895627/18014398509481984,34483405944844083/18014398509481984,34483405944844081/18014398509481984,21745302599792537/9007199254740992,12738103345051545/9007199254740992,19107155017577317/9007199254740992,38824863540916338353528385288599/20282409603651670423947251286016,38824863540916338353528385288599/20282409603651670423947251286016,48966068342742174691401917774231/20282409603651670423947251286016,34483405944844083/18014398509481984,12738103345051545/9007199254740992,12738103345051545/9007199254740992,9553577508788659/4503599627370496,47221509289895627/18014398509481984,48966068342742172439602104088983/20282409603651670423947251286016,38824863540916338353528385288599/20282409603651670423947251286016,12738103345051545/4503599627370496,53166692910461591153103775185303/20282409603651670423947251286016,38824863540916338353528385288599/20282409603651670423947251286016,53166692910461588901303961500055/20282409603651670423947251286016,9553577508788659/4503599627370496,24483034171371086679852902234519/10141204801825835211973625643008,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,12738103345051545/9007199254740992,6369051672525773/9007199254740992,21745302599792537/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,6369051672525773/9007199254740992,3730904090310553/18014398509481984,6369051672525773/9007199254740992,21745302599792537/18014398509481984,12738103345051545/9007199254740992,21745302599792537/18014398509481984,12738103345051545/9007199254740992,6369051672525773/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,6369051672525773/9007199254740992,12738103345051545/9007199254740992,21745302599792537/18014398509481984,12738103345051545/9007199254740992,21745302599792537/18014398509481984,3730904090310553/18014398509481984},{1592262918131443/2251799813685248,1592262918131443/2251799813685248,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,1592262918131443/2251799813685248,1592262918131443/2251799813685248,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,12738103345051545/9007199254740992,12738103345051545/9007199254740992,1592262918131443/2251799813685248,1592262918131443/2251799813685248,12738103345051545/9007199254740992,12738103345051545/9007199254740992,1592262918131443/2251799813685248,1592262918131443/2251799813685248,12738103345051545/9007199254740992,1592262918131443/2251799813685248,1592262918131443/2251799813685248,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,24483034171371085553952995391895/20282409603651670423947251286016,12738103345051545/9007199254740992,1592262918131443/2251799813685248,1592262918131443/2251799813685248,12738103345051545/9007199254740992,24483034171371085553952995391895/20282409603651670423947251286016,24483034171371085553952995391895/20282409603651670423947251286016,24483034171371085553952995391895/20282409603651670423947251286016,12738103345051545/9007199254740992,1592262918131443/2251799813685248,12738103345051545/9007199254740992,1592262918131443/2251799813685248,1592262918131443/2251799813685248,12738103345051545/9007199254740992,24483034171371085553952995391895/20282409603651670423947251286016,9553577508788659/4503599627370496,9553577508788659/4503599627370496,53166692910461591153103775185303/20282409603651670423947251286016,1592262918131443/2251799813685248,38824863540916338353528385288599/20282409603651670423947251286016,24483034171371086679852902234519/10141204801825835211973625643008,1592262918131443/2251799813685248,38824863540916338353528385288599/20282409603651670423947251286016,24483034171371086679852902234519/10141204801825835211973625643008,44765443775022755566307833752773/20282409603651670423947251286016,1592262918131443/2251799813685248,1592262918131443/2251799813685248,38824863540916338353528385288599/20282409603651670423947251286016,1592262918131443/2251799813685248,1592262918131443/2251799813685248,1592262918131443/2251799813685248,38824863540916338353528385288599/20282409603651670423947251286016,1592262918131443/2251799813685248,9553577508788659/4503599627370496,9553577508788659/4503599627370496,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,24483034171371085553952995391895/20282409603651670423947251286016,24483034171371085553952995391895/20282409603651670423947251286016,12738103345051545/9007199254740992,1592262918131443/2251799813685248,1592262918131443/2251799813685248,12738103345051545/9007199254740992,1592262918131443/2251799813685248,12738103345051545/9007199254740992,24483034171371085553952995391895/20282409603651670423947251286016,24483034171371085553952995391895/20282409603651670423947251286016,12738103345051545/9007199254740992,1592262918131443/2251799813685248,1592262918131443/2251799813685248,12738103345051545/9007199254740992,24483034171371085553952995391895/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,4200624567719415541598157031017/20282409603651670423947251286016,48966068342742172439602104088983/20282409603651670423947251286016,34483405944844081/18014398509481984,34483405944844081/18014398509481984,34483405944844081/18014398509481984,48966068342742172439602104088983/20282409603651670423947251286016,34483405944844081/18014398509481984,47221509289895627/18014398509481984,47221509289895627/18014398509481984,12738103345051545/4503599627370496,53166692910461588901303961500055/20282409603651670423947251286016,53166692910461588901303961500055/20282409603651670423947251286016,24483034171371085553952995391895/10141204801825835211973625643008,1592262918131443/1125899906842624,1592262918131443/1125899906842624,1592262918131443/1125899906842624,1592262918131443/1125899906842624,38824863540916336101728571603351/20282409603651670423947251286016,19107155017577317/9007199254740992,19107155017577317/9007199254740992,38824863540916336101728571603351/20282409603651670423947251286016,38824863540916336101728571603351/20282409603651670423947251286016,19107155017577317/9007199254740992,19107155017577317/9007199254740992,38824863540916336101728571603351/20282409603651670423947251286016,21745302599792537/9007199254740992},{3730904090310553/18014398509481984,3730904090310553/18014398509481984,1592262918131443/2251799813685248,1592262918131443/2251799813685248,21745302599792537/18014398509481984,21745302599792537/18014398509481984,12738103345051545/9007199254740992,12738103345051545/9007199254740992,1592262918131443/2251799813685248,12738103345051545/9007199254740992,1592262918131443/2251799813685248,12738103345051545/9007199254740992,1592262918131443/2251799813685248,12738103345051545/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,21745302599792537/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,21745302599792537/18014398509481984,21745302599792537/18014398509481984,9553577508788659/4503599627370496,53166692910461588901303961500055/20282409603651670423947251286016,24483034171371086679852902234519/10141204801825835211973625643008,38824863540916338353528385288599/20282409603651670423947251286016,53166692910461591153103775185303/20282409603651670423947251286016,12738103345051545/4503599627370496,47221509289895627/18014398509481984,9553577508788659/4503599627370496,38824863540916338353528385288599/20282409603651670423947251286016,48966068342742172439602104088983/20282409603651670423947251286016,12738103345051545/9007199254740992,12738103345051545/9007199254740992,34483405944844083/18014398509481984,48966068342742174691401917774231/20282409603651670423947251286016,38824863540916338353528385288599/20282409603651670423947251286016,38824863540916338353528385288599/20282409603651670423947251286016,19107155017577317/9007199254740992,12738103345051545/9007199254740992,12738103345051545/9007199254740992,21745302599792537/9007199254740992,34483405944844081/18014398509481984,34483405944844083/18014398509481984,47221509289895627/18014398509481984,19107155017577317/9007199254740992,34483405944844081/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,34483405944844083/18014398509481984,6369051672525773/9007199254740992,6369051672525773/9007199254740992,6369051672525773/9007199254740992,39759701109274521/18014398509481984,21745302599792537/9007199254740992,6369051672525773/9007199254740992,34483405944844083/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,19107155017577317/9007199254740992,19107155017577317/9007199254740992,34483405944844081/18014398509481984,34483405944844081/18014398509481984,21745302599792537/9007199254740992,47221509289895627/18014398509481984,9553577508788659/4503599627370496,1592262918131443/2251799813685248,1592262918131443/2251799813685248,1592262918131443/2251799813685248,9553577508788659/4503599627370496,1592262918131443/2251799813685248,6369051672525773/9007199254740992,6369051672525773/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,6369051672525773/9007199254740992,6369051672525773/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,6369051672525773/9007199254740992,1592262918131443/2251799813685248,1592262918131443/2251799813685248,12738103345051545/9007199254740992,12738103345051545/9007199254740992,1592262918131443/2251799813685248,1592262918131443/2251799813685248,12738103345051545/9007199254740992,12738103345051545/9007199254740992,1592262918131443/2251799813685248,12738103345051545/9007199254740992,6369051672525773/9007199254740992,12738103345051545/9007199254740992,21745302599792537/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,6369051672525773/9007199254740992,6369051672525773/9007199254740992,21745302599792537/18014398509481984,21745302599792537/18014398509481984,12738103345051545/9007199254740992,12738103345051545/9007199254740992,12738103345051545/9007199254740992,21745302599792537/18014398509481984,12738103345051545/9007199254740992,21745302599792537/18014398509481984,6369051672525773/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,6369051672525773/9007199254740992,3730904090310553/18014398509481984},{9553577508788659/4503599627370496,9553577508788659/4503599627370496,38824863540916340605328198973847/20282409603651670423947251286016,38824863540916340605328198973847/20282409603651670423947251286016,9553577508788659/4503599627370496,38824863540916340605328198973847/20282409603651670423947251286016,9553577508788659/4503599627370496,38824863540916340605328198973847/20282409603651670423947251286016,24483034171371087805752809077143/10141204801825835211973625643008,6369051672525773/4503599627370496,48966068342742174691401917774231/20282409603651670423947251286016,34483405944844083/18014398509481984,53166692910461591153103775185303/20282409603651670423947251286016,6369051672525773/4503599627370496,48966068342742174691401917774231/20282409603651670423947251286016,34483405944844083/18014398509481984,53166692910461591153103775185303/20282409603651670423947251286016,6369051672525773/4503599627370496,47221509289895627/18014398509481984,34483405944844083/18014398509481984,47221509289895627/18014398509481984,12738103345051545/4503599627370496,21745302599792537/9007199254740992,34483405944844083/18014398509481984,6369051672525773/4503599627370496,24483034171371087805752809077143/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,24483034171371087805752809077143/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,12738103345051545/9007199254740992,12738103345051545/9007199254740992,6369051672525773/9007199254740992,24483034171371087805752809077143/20282409603651670423947251286016,24483034171371087805752809077143/20282409603651670423947251286016,24483034171371087805752809077143/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,12738103345051545/9007199254740992,12738103345051545/9007199254740992,6369051672525773/9007199254740992,6369051672525773/9007199254740992,12738103345051545/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,38824863540916338353528385288599/20282409603651670423947251286016,6369051672525773/9007199254740992,6369051672525773/9007199254740992,24483034171371086679852902234519/10141204801825835211973625643008,38824863540916338353528385288599/20282409603651670423947251286016,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,19107155017577317/9007199254740992,53166692910461588901303961500055/20282409603651670423947251286016,19107155017577317/9007199254740992,19107155017577317/9007199254740992,6369051672525773/9007199254740992,19107155017577317/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,6369051672525773/9007199254740992,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,38824863540916338353528385288599/20282409603651670423947251286016,24483034171371086679852902234519/10141204801825835211973625643008,38824863540916338353528385288599/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,44765443775022757818107647438021/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,12738103345051545/9007199254740992,12738103345051545/9007199254740992,6369051672525773/9007199254740992,12738103345051545/9007199254740992,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,12738103345051545/9007199254740992,12738103345051545/9007199254740992,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,24483034171371087805752809077143/20282409603651670423947251286016,24483034171371087805752809077143/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,24483034171371087805752809077143/20282409603651670423947251286016,24483034171371087805752809077143/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,24483034171371087805752809077143/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,12738103345051545/9007199254740992,12738103345051545/9007199254740992,12738103345051545/9007199254740992,6369051672525773/9007199254740992,12738103345051545/9007199254740992,6369051672525773/9007199254740992,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,6369051672525773/9007199254740992,6369051672525773/9007199254740992,4200624567719417793397970716265/20282409603651670423947251286016,12738103345051545/9007199254740992},{21745302599792537/18014398509481984,12738103345051545/9007199254740992,21745302599792537/18014398509481984,12738103345051545/9007199254740992,3730904090310553/18014398509481984,1592262918131443/2251799813685248,3730904090310553/18014398509481984,1592262918131443/2251799813685248,1592262918131443/2251799813685248,12738103345051545/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,21745302599792537/18014398509481984,1592262918131443/2251799813685248,12738103345051545/9007199254740992,1592262918131443/2251799813685248,12738103345051545/9007199254740992,3730904090310553/18014398509481984,21745302599792537/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,21745302599792537/18014398509481984,1592262918131443/2251799813685248,6369051672525773/9007199254740992,12738103345051545/9007199254740992,1592262918131443/2251799813685248,12738103345051545/9007199254740992,6369051672525773/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,1592262918131443/2251799813685248,1592262918131443/2251799813685248,1592262918131443/2251799813685248,12738103345051545/9007199254740992,12738103345051545/9007199254740992,12738103345051545/9007199254740992,6369051672525773/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,6369051672525773/9007199254740992,6369051672525773/9007199254740992,3730904090310553/18014398509481984,1592262918131443/2251799813685248,21745302599792537/9007199254740992,34483405944844081/18014398509481984,1592262918131443/2251799813685248,1592262918131443/2251799813685248,34483405944844081/18014398509481984,9553577508788659/4503599627370496,47221509289895627/18014398509481984,19107155017577317/9007199254740992,19107155017577317/9007199254740992,3730904090310553/18014398509481984,6369051672525773/9007199254740992,21745302599792537/9007199254740992,34483405944844083/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,39759701109274521/18014398509481984,3730904090310553/18014398509481984,6369051672525773/9007199254740992,6369051672525773/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,6369051672525773/9007199254740992,34483405944844083/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,9553577508788659/4503599627370496,1592262918131443/2251799813685248,19107155017577317/9007199254740992,47221509289895627/18014398509481984,34483405944844081/18014398509481984,12738103345051545/9007199254740992,34483405944844083/18014398509481984,34483405944844081/18014398509481984,12738103345051545/9007199254740992,21745302599792537/9007199254740992,19107155017577317/9007199254740992,53166692910461588901303961500055/20282409603651670423947251286016,48966068342742172439602104088983/20282409603651670423947251286016,38824863540916338353528385288599/20282409603651670423947251286016,9553577508788659/4503599627370496,47221509289895627/18014398509481984,12738103345051545/4503599627370496,34483405944844083/18014398509481984,12738103345051545/9007199254740992,38824863540916338353528385288599/20282409603651670423947251286016,48966068342742174691401917774231/20282409603651670423947251286016,12738103345051545/9007199254740992,38824863540916338353528385288599/20282409603651670423947251286016,53166692910461591153103775185303/20282409603651670423947251286016,38824863540916338353528385288599/20282409603651670423947251286016,9553577508788659/4503599627370496,24483034171371086679852902234519/10141204801825835211973625643008,12738103345051545/9007199254740992,6369051672525773/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,3730904090310553/18014398509481984,21745302599792537/18014398509481984,3730904090310553/18014398509481984,21745302599792537/18014398509481984,21745302599792537/18014398509481984,12738103345051545/9007199254740992,12738103345051545/9007199254740992,3730904090310553/18014398509481984,3730904090310553/18014398509481984,6369051672525773/9007199254740992,6369051672525773/9007199254740992,6369051672525773/9007199254740992,3730904090310553/18014398509481984,6369051672525773/9007199254740992,3730904090310553/18014398509481984,12738103345051545/9007199254740992,21745302599792537/18014398509481984,21745302599792537/18014398509481984,12738103345051545/9007199254740992,3730904090310553/18014398509481984}};
raysP = map(QQ^8, QQ^0, 0);
linealityP = map(QQ^8, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {132,330,300,120,20,1};
computed = fVector P;
assert(desired == computed)
///

-- Test flag_vector/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {32,80,80,40,10,1};
computed = fVector P;
assert(desired == computed)
///

-- Test flag_vector/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,0,0,0,0,0},{0,0,1,0,0,0,0,0},{0,0,0,1,0,0,0,0},{0,0,0,0,1,0,0,0},{0,0,0,0,0,1,0,0},{0,0,0,0,0,0,1,0},{0,0,0,0,0,0,0,1}};
raysP = map(QQ^7, QQ^0, 0);
linealityP = map(QQ^7, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,28,56,70,56,28,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test beneath_beyond/6.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,2}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {2,1};
computed = fVector P;
assert(desired == computed)
///

-- Test beneath_beyond/8.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,1,0,1,0},{0,0,1,1,0},{0,0,0,0,1},{0,0,0,0,0},{0,0,0,0,0}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {5,8,5,1};
computed = fVector P;
assert(desired == computed)
///

-- Test beneath_beyond/1r.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,-1,0,0,0,0,0,0},{0,0,1,-1,0,0,0,0},{0,0,0,0,1,-1,0,0},{0,0,0,0,0,0,1,-1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,24,32,16,1};
computed = fVector P;
assert(desired == computed)
///

-- Test beneath_beyond/1.poly
-- Checking fVector
TEST ///
verticesP = matrix {{1,-1,0,0,0,0,0,0},{0,0,1,-1,0,0,0,0},{0,0,0,0,1,-1,0,0},{0,0,0,0,0,0,1,-1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {8,24,32,16,1};
computed = fVector P;
assert(desired == computed)
///

-- Test beneath_beyond/4.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {64,192,240,160,60,12,1};
computed = fVector P;
assert(desired == computed)
///

-- Test beneath_beyond/9.poly
-- Checking fVector
TEST ///
verticesP = matrix {{55},{1250},{2800}};
raysP = matrix {{-1,0,1,0},{0,-1,0,1},{-80,0,140,5000}};
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {5,8,5,1};
computed = fVector P;
assert(desired == computed)
///

-- Test beneath_beyond/2r.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1},{-5/2,-1,-1,1/2,-2,-1/2,-1/2,1,-1,1/2,1/2,2,-1/2,1,1,5/2},{-5/4,1/4,1/4,7/4,-5/4,1/4,1/4,7/4,-1/4,5/4,5/4,11/4,-1/4,5/4,5/4,11/4},{-1,-1,1/2,1/2,-1/2,-1/2,1,1,-1/2,-1/2,1,1,0,0,3/2,3/2}};
raysP = map(QQ^7, QQ^0, 0);
linealityP = map(QQ^7, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {16,32,24,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test beneath_beyond/ineq7.poly
-- Checking fVector
TEST ///
verticesP = matrix {{0,0,1,1,1,1,0,0,-1,-1,-1,-1},{-1,1,-1,1,0,0,1,-1,0,1,0,-1},{-1,-1,0,0,1,-1,1,1,1,0,-1,0},{0,0,0,0,0,0,0,0,0,0,0,0}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = matrix {{-1},{-1},{-1},{1}};
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {12,24,14,1};
computed = fVector P;
assert(desired == computed)
///

-- Test beneath_beyond/2.poly
-- Checking fVector
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
desired = {16,32,24,8,1};
computed = fVector P;
assert(desired == computed)
///

-- Test beneath_beyond/3.poly
-- Checking fVector
TEST ///
verticesP = matrix {{6730902641522717314961998953235/104776506286137515566857289990144,-390280997608381835232697072991/1864397174641101581339368357888,-28661454104902942433939732686875/599213099946195397458372881022976,-15406128800245753276724909349157/30099827352902650724266352312320,-1348940276325668500074957593851/2697085334304636491828689895424,-75141446950690149587759747394937/165616557448512116164343317397504,-4127179246096640907403890265885/41904101308994666459112740487168,2163445037703968508482316572317/8272174879337969639020747030528,-450191691851794455064904918875/9911153521006773458158718287872,2279981434851121779512083749439/17676604320812387688659128680448,-27824252277803128204504832190717/58693645000177121121239226646528,432519296535220584799240474927507/1077260866563456139894848957186048,3765428054130167415452303705125/222125502036585245242532474912768,-10521689461745455177822871802093/85753133669755553209037369638912,-1615106097963036509945373835895/10989631586614384549803648352256,-153859104600774931051039606447/20406035371046271441470712971264,4095357961966490213093023541807/10600428188431546152758393438208,-37556954633350929381541664662061/120997085920833975034182430097408,296286671065291413809687610328321/1053538385342145802784220758671360,-10037572016285418472076457697283/34633413130182945354295273324544,997853003413383634343654437239/28057515132355876009082573815808,1582119868326493885513507218639/8407088162979660786601711108096,-81569316783071114122199918965583/121058810132428827263998000889856,1120789335983075619818326397155/4134944818141064063042084405248,7914014844818656873551204740075/22266590606652141677541010178048,25236149475136229335454327319069/79666744713521896378976557858816,34436819126181817978168993557249/82379817115275568750484537737216,1075237021510564600709996702909/17773044687234827937178204504064,-23175127494363607504616048277949/210988502108587902922675355385856,-262546879473029603707919740754503/1622551957453061927871757233946624,-19280515772323983437962092252821/35728275856999840664389622431744,-218131885003462022022715316659/469355616747114736836851793920,-32956926220711635100663405748243/449227965645187879958530102394880,-995867185257037157829295479027/13278017017308155570887822671872,-34890800420277220429041632861991/74325346191872223061463382622208,4881578075620478161802803758307/43604575931687752643748567187456,-20580810229325881370831876823683/230674452417758719947424114147328,-18559593442819017269644306113629/108009397080680772287604176453632,88458460007120463175689674713969/220978085925343540969004637093888,-37800542459003165953439084708129/195948843722016143991203635920896,758858976223275895833211232655/2363840771607767955197225074688,66614867809015005947115586555/1881417544855809094232601264128,9691421936173397280519865426369/48606240382070450486122702176256,-234129344117733669655521557354443/488479183229913383369921964015616,572772271277166369007899321289/4213292614681816007099019165696,-17725253852189936798796711715609/73625179114097004834733902790656,-2467413306791997788510344280955/8988565927180644761690706018304,-85229655145351274060232228811983/280192885126197765903285898706944,-628404180535349928731333609701/3709936664406618205468571467776,-642280552501046512786290404317/6459758645495573539114734059520,-327315657767793243998783766913/3134175500792059494205132636160,3323722793616802310017784382157/8990617413053070246706828804096,1407830498052662806322035048261/2881077377630623370955825610752,35711980245977968636464561209/10093496863672162882410132799488,-1803712978445146309490912595779/48179111502188368715192056414208,-4272000456244429145286834506875/6149444231630612191866909622272,-1899152396436366206979095160857/14913336393539997612264159969280,-79885633539449837090851846233/323468803175407180235067621376,-167630647380197786990945515144791/657217633371141849077965278674944,-749163246137013580167389662235/1658390490567448669885812441088,-5658485928621560047787578053135/20374235221048913858078802182144,21899128469814271135939613863441/45568617888980620247557090574336,8995083771771060735599094535787/24917620553257582720908074680320,-4659018797769907867302859829157/45788183250861295204600509366272,6182761421693475070225869587551/60832267043382222623616446496768,-95181007750031517664768387896297/141956724170309861478622472175616,-2532845396902464879676112717701/9542449889035840280145808064512,-3617307262047392943548475040639/70268006390052649658190697332736,-74121539530598398873606680775621/192839276433825988720292456824832,-49879226967660407713626026523023/584973119586817781864237061111808,-49522201761274099182019521066591/78952935354379874649010377588736,-14421542476081339814983748532481/53054540356761081813775547367424,667312808131112830515958873725/1695014187846971091419750465536,-551749439197412304463264245357805/2671042971747034365646309863456768},{7965064963571047990661974269879/13097063285767189445857161248768,2229088198582871639444364821867/29830354794257625301429893726208,-1151744064633648628088591931673/2340676171664825771321769066496,2145096958654462396472974670829/6019965470580530144853270462464,-10525723290363199551782593673797/86306730697748367738518076653568,-31881212010323691645997977395683/82808278724256058082171658698752,20882301837660656381405878009049/83808202617989332918225480974336,-3247489166253543566073686098855/6204131159503477229265560272896,-48189951258451377090286032216733/2537255301377734005288631881695232,193422173033721230934552429571343/2262605353063985624148368471097344,1664452692166777604995330538197/3668352812511070070077451665408,-349178708316418480086063324531239/1077260866563456139894848957186048,-4137667214039165408457690077963/13882843877286577827658279682048,36628341422630448695173972988305/171506267339511106418074739277824,-104270031989255405946445474888353/703336421543320611187433494544384,-4287408942114899427190338576911/81624141484185085765882851885056,1044531660056480550698210323335/10158743680580231729726793711616,122870349026601083185477506377683/241994171841667950068364860194816,-3064238193395914605517733535229/13169229816776822534802759483392,7121989971065341008516212447887/25975059847637209015721454993408,1607187210814660852458271891653/28057515132355876009082573815808,5974129575653224600489116374775/33628352651918643146406844432384,-27576546548265815473564379749289/242117620264857654527996001779712,10895075389827495465482092520049/33079558545128512504336675241984,29490743568262358155021760113843/66799771819956425032623030534144,22882169707515382444261727501521/79666744713521896378976557858816,-1027137756604630684155139079641/20594954278818892187621134434304,-33787938972684823589097355832555/284368714995757246994851272065024,709700154888250295073392607591/52747125527146975730668838846464,-228753373493680613220504777671/1584523395950255788937262923776,-11609384448259671752805283511119/44660344821249800830487028039680,4951449166715010318244791285653/9011627841544602947267554443264,-5749845985147907118554383737101/14038373926412121248704065699840,-607520463481970650216388406289/3319504254327038892721955667968,1583274583421674849338627977345/5308953299419444504390241615872,-31391449607091359924774996275781/54505719914609690804685708984320,-11555679008077558368969397222637/57668613104439679986856028536832,-2608701412841068154625078007003/6750587317542548267975261028352,1481586222944295658850823100921/55244521481335885242251159273472,-3007045330251727214901715433149/48987210930504035997800908980224,-108240639898642410653474213424931/378214523457242872831556011950080,2075036137626593180757888594613/7525670179423236376930405056512,-5061705390633347433157125601825/48606240382070450486122702176256,-4515290081362007265787106032939/15264974475934793230310061375488,248186171944610231946489491591/1296397727594404925261236666368,382747169834834998861276615505/18406294778524251208683475697664,-479593785294655215245696025383417/2301072877358245058992820740685824,5135289724553075263003159276727/70048221281549441475821474676736,-13279245757276656800060650422041/35244398311862872951951428943872,30325921334999442226877079934151/103356138327929176625835744952320,-107839038677592866986291244713069/401174464101383615258256977428480,8878203957537922206357026747511/35962469652212280986827315216384,674059909643007544673679137861/5762154755261246741911651221504,6669803676586111336491149098487/40373987454688651529640531197952,-13773573518448295337824773687363/192716446008753474860768225656832,-231463636747097253361586597425/12298888463261224383733819244544,52000938440254358608783646009/5592501147577499104599059988480,-23562677087222900245816100187885/66877175056515434513600230719488,-21660451933343641272278053192615/82152204171392731134745659834368,94148550224331818453108899944373/424547965585266859490767984918528,79665672352385144871400198763971/325987763536782621729260834914304,-4046169211321087859962148351051/22784308944490310123778545287168,-4718857893936659579984888877001/24917620553257582720908074680320,-40123600378327465056037029412677/366305466006890361636804074930176,3966006532539263535186966481861/15208066760845555655904111624192,-61744958048349796754460953041/8872295260644366342413904510976,1462184541536367936666149952673/4771224944517920140072904032256,-517523027417861842895135851573499/2248576204481684789062102314647552,29230891278611954387812151423/48209819108456497180073114206208,-5879415085666183789715223121129/9140204993544027841628704079872,-820259240608481307307693487755/19738233838594968662252594397184,3928915770255929627217575343551/53054540356761081813775547367424,-191861865212527600876268409985853/1735694528355298397613824476708864,534768793649473304007534010009/869480134032237749233824825344},{-3342652359699167139022539790471/13097063285767189445857161248768,-1534665979565667194785895009701/3728794349282203162678736715776,8333638905581478487649145193153/37450818746637212341148305063936,227099350507606258171259080649/1881239209556415670266647019520,-2549189918856456235279214270633/21576682674437091934629519163392,-3391055148694122261161421520801/165616557448512116164343317397504,4126594342041522852890080789749/13968033769664888819704246829056,-25553532659770934830940661851423/99266098552055635668248964366336,5611796467550617950615919653593/13214871361342364610878291050496,9117296580989591680997009490247/17676604320812387688659128680448,-1436234466833322011675678050943/7336705625022140140154903330816,6702309251531395125465372072179/16832201040054002185857014956032,-1191375422275600760421122131231/6941421938643288913829139841024,-3714838494133717175410872105647/10719141708719444151129671204864,3601528570793678479734188322871/10989631586614384549803648352256,4124308810249996578507623462813/5101508842761567860367678242816,-8276048895725174667756567107057/15238115520870347594590190567424,7835751045453304933163940987397/15124635740104246879272803762176,8779360110100023196809004789615/26338459633553645069605518966784,-271312425918379852558854083675/811720620238662781741295468544,-654489203874942421078944987163/7014378783088969002270643453952,173707664538688098341627228479/4203544081489830393300855554048,40011078672628179149395582213523/121058810132428827263998000889856,6476870628947590114352801910269/16539779272564256252168337620992,16348225413394055150040310271585/66799771819956425032623030534144,1412423539468898929430913210631/19916686178380474094744139464704,-15560074540843728627741726644121/41189908557637784375242268868608,11413196293167785450985319588221/35546089374469655874356409008128,-311185536419451160233993844703/1198798307435158539333382701056,-183154853190352416769378505685/3169046791900511577874525847552,9527964545234817714684521427921/89320689642499601660974056079360,1579767994401995619705563030509/120155037887261372630234059243520,-58983201425072442429570396645023/112306991411296969989632525598720,7546869778526812900451591245475/53112068069232622283551290687488,-10854321791226946298691499424689/74325346191872223061463382622208,-627983529694430921828919399057/6813214989326211350585713623040,-3601608239468940029746861235859/7208576638054959998357003567104,53825733865033525595133408767513/108009397080680772287604176453632,-363638222449790908339937182019/1649090193472712992306004754432,-2772896998307541263493490388279/12246802732626008999450227245056,78704501027233941700112017267317/236384077160776795519722507468800,-4441638733411078661282566836787/7525670179423236376930405056512,-1753983517695115260870924084057/4418749125642768226011154743296,-1983015490753760259194503607515/30529948951869586460620122750976,29795778558961735941298899667/2808861743121210671399346110464,-821730522190183876889418301579/1150393423657765700542717231104,43622254775655200789644696661961/71908527417445158093525648146432,-2229190949124792244296451655047/35024110640774720737910737338368,75585774919127201135954373455/1958022128436826275108412719104,-99741781664673517904615660281/1291951729099114707822946811904,6483423107591433836871080431083/12536702003168237976820530544640,-108826859649811573355633862181/2247654353263267561676707201024,-3708115436847593006902357490845/23048619021044986967646604886016,3870532237408438547643378079637/20186993727344325764820265598976,23458458620045304127588238297139/48179111502188368715192056414208,-88963458112695015782697589019/878492033090087455980987088896,-3871980035763002169953081979227/11185002295154998209198119976960,-1112963538455023576784955779837/66877175056515434513600230719488,12936122378001387600986836735115/20538051042848182783686414958592,-13388686370365297202034685008483/106136991396316714872691996229632,687461546063083194339580772613/1273389701315557116129925136384,-25805768785268132047843931364717/182274471555922480990228362297344,3035961123884614377973000809937/33223494071010110294544099573760,16833355465486492538105681170713/183152733003445180818402037465088,-1964970727894378894803910954259/5069355586948518551968037208064,-711541546661849495453097131897/8872295260644366342413904510976,2753469344034912002988429924169/14313674833553760420218712096768,163965830150859070435754838739267/281072025560210598632762789330944,-851477847073082923708235893731/24104909554228248590036557103104,8631081558121231969359032165547/73121639948352222733029632638976,12855502019727594208123394296261/78952935354379874649010377588736,46112584465630507570974453164301/212218161427044327255102189469696,12901935126394094592232124130935/27120227005551537462716007448576,-3082081120485313123433067687901/10433761608386852990805897904128},{-12308668566877594927456986443287/52388253143068757783428644995072,12092521140967957947599219546835/29830354794257625301429893726208,-14083213712596794813343921883225/37450818746637212341148305063936,92003761259524493894298939875499/481597237646442411588261636997120,7679610560756775980194537637027/43153365348874183869259038326784,340123136112772266489599412873/20702069681064014520542914674688,-579058088656906051659917785793/2619006331812166653694546280448,1252597485851790063258510952783/3102065579751738614632780136448,1341426339432260279014904680505/19822307042013546916317436575744,1659762896389895632197456817775/70706417283249550754636514721792,4478680683340412049888900924293/117387290000354242242478453293056,-65818921124676081288586434901577/718173911042304093263232638124032,-17504752861222139751849206810273/55531375509146311310633118728192,14649609555556855783947125895503/85753133669755553209037369638912,5568244772200243418893459174713/21979263173228769099607296704512,4200029788309206216566429766083/163248282968370171531765703770112,958594560982315267324351401251/15238115520870347594590190567424,5866249943065863810320309015959/120997085920833975034182430097408,-184317331904959610807944722893841/2107076770684291605568441517342720,30493121432348587108320401197039/207800478781097672125771639947264,1916735419076947913376569417777/3507189391544484501135321726976,3027698756661366163018180830937/8407088162979660786601711108096,-7011527632143303094731617535407/60529405066214413631999000444928,-10482756091357278639441333963869/33079558545128512504336675241984,-965159233335537536425991905875/5566647651663035419385252544512,13376309246470951690545806114677/39833372356760948189488278929408,4564343773297586122039171452141/10297477139409446093810567217152,-15793527673123056144393926185453/71092178748939311748712818016256,-8221354054328067582757245587123/13186781381786743932667209711616,-2437813634252807701634925328085/12676187167602046311498103390208,-5140406134677040923539796454051/22330172410624900415243514019840,2458042923071639438442846292825/12015503788726137263023405924352,-3789094791399207327344993018767/14038373926412121248704065699840,6257144472287756017976805089781/13278017017308155570887822671872,-25783464512443489029188959979481/297301384767488892245853530488832,-3577775900968243460824476776259/13626429978652422701171427246080,-3225170165795349114600139599791/14417153276109919996714007134208,-6432237948330146725059274949787/27002349270170193071901044113408,-21980387199943016908202571578563/55244521481335885242251159273472,-1312598781402932334774240231143/24493605465252017998900454490112,771757724628389191007802759489/2954800964509709943996531343360,9252093020603125071725395118413/30102680717692945507721620226048,-3421517553588920851685218338275/6075780047758806310765337772032,-8036692178797023757605686441179/15264974475934793230310061375488,-1408547819020415528245761608033/8426585229363632014198038331392,-2888163320153565192792228189619/12270863185682834139122317131776,23666511371301369207549433700949/71908527417445158093525648146432,-17078200532190162108467175815925/70048221281549441475821474676736,-241158834857542647841603800665/8811099577965718237987857235968,1756547047062910353758907256195/5167806916396458831291787247616,3779762897251443412558726104807/25073404006336475953641061089280,2643594180021758600993361882637/4495308706526535123353414402048,7610427779577871392135493157/53353284770937469832515289088,-235684367626
