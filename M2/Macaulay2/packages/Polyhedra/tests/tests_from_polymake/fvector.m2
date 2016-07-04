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
desired = {4,6,4,1};
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

