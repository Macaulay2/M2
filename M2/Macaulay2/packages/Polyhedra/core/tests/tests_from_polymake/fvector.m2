TEST ///
-- Test cell_from_subdivision/2.poly
-- Checking fVector
verticesP0 = matrix {{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1},{0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1,1},{0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,1,1,1,0,0},{0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,0,0,0,1,1,1,0,0,0,0,0},{1,0,1,1,0,1,1,0,0,1,0,1,1,0,0,1,0,0,1,0,1,1,0,0,1,0,0,1,0,1},{1,1,0,1,1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,1,0,1,0,1,0,0,1,0,1,0},{1,1,1,0,1,1,0,1,0,0,1,1,0,1,0,0,1,0,0,1,1,0,1,0,0,1,0,0,0,0}};
raysP0 = map(QQ^7, QQ^0, 0);
linealityP0 = map(QQ^7, QQ^0, 0);
verticesP0 = promote(verticesP0, QQ);
raysP0 = promote(raysP0, QQ);
linealityP0 = promote(linealityP0, QQ);
P0 = convexHull(verticesP0,raysP0,linealityP0);
desired0 = {30,166,299,239,92,16,1};
computed0 = fVector P0;
assert(desired0 == computed0)

-- Test gc_closure/2-0.poly
-- Checking fVector
verticesP1 = matrix {{0,1,1/2},{0,0,11/2}};
raysP1 = map(QQ^2, QQ^0, 0);
linealityP1 = map(QQ^2, QQ^0, 0);
verticesP1 = promote(verticesP1, QQ);
raysP1 = promote(raysP1, QQ);
linealityP1 = promote(linealityP1, QQ);
P1 = convexHull(verticesP1,raysP1,linealityP1);
desired1 = {3,3,1};
computed1 = fVector P1;
assert(desired1 == computed1)

-- Test delpezzo/1.poly
-- Checking fVector
verticesP2 = matrix {{1,0,0,0,-1,0,0,0,1,-1},{0,1,0,0,0,-1,0,0,1,-1},{0,0,1,0,0,0,-1,0,1,-1},{0,0,0,1,0,0,0,-1,1,-1}};
raysP2 = map(QQ^4, QQ^0, 0);
linealityP2 = map(QQ^4, QQ^0, 0);
verticesP2 = promote(verticesP2, QQ);
raysP2 = promote(raysP2, QQ);
linealityP2 = promote(linealityP2, QQ);
P2 = convexHull(verticesP2,raysP2,linealityP2);
desired2 = {10,40,60,30,1};
computed2 = fVector P2;
assert(desired2 == computed2)

-- Test cell_from_subdivision/1.poly
-- Checking fVector
verticesP3 = matrix {{0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1},{0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1},{0,0,1,0,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0},{0,1,0,0,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0},{1,1,1,0,1,1,0,1,0,1,0,1,1,0,1,0,1,0,1},{1,1,1,1,0,1,1,0,1,0,1,0,1,1,0,1,0,1,0},{1,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0}};
raysP3 = map(QQ^7, QQ^0, 0);
linealityP3 = map(QQ^7, QQ^0, 0);
verticesP3 = promote(verticesP3, QQ);
raysP3 = promote(raysP3, QQ);
linealityP3 = promote(linealityP3, QQ);
P3 = convexHull(verticesP3,raysP3,linealityP3);
desired3 = {19,75,123,103,47,11,1};
computed3 = fVector P3;
assert(desired3 == computed3)

-- Test cayley_polytope/1.poly
-- Checking fVector
verticesP4 = matrix {{0,1,0,1},{0,0,1,1}};
raysP4 = map(QQ^2, QQ^0, 0);
linealityP4 = map(QQ^2, QQ^0, 0);
verticesP4 = promote(verticesP4, QQ);
raysP4 = promote(raysP4, QQ);
linealityP4 = promote(linealityP4, QQ);
P4 = convexHull(verticesP4,raysP4,linealityP4);
desired4 = {4,4,1};
computed4 = fVector P4;
assert(desired4 == computed4)

-- Test cayley_polytope/c2.poly
-- Checking fVector
verticesP5 = matrix {{0,1,0,1,0,1,0,1,0,1,0},{0,0,1,1,0,0,1,1,0,0,1},{1,1,1,1,0,0,0,0,0,0,0},{0,0,0,0,1,1,1,1,0,0,0}};
raysP5 = map(QQ^4, QQ^0, 0);
linealityP5 = map(QQ^4, QQ^0, 0);
verticesP5 = promote(verticesP5, QQ);
raysP5 = promote(raysP5, QQ);
linealityP5 = promote(linealityP5, QQ);
P5 = convexHull(verticesP5,raysP5,linealityP5);
desired5 = {11,25,22,8,1};
computed5 = fVector P5;
assert(desired5 == computed5)

-- Test cayley_polytope/c1.poly
-- Checking fVector
verticesP6 = matrix {{0,1,0,1,0,1,0,1,0,1,0},{0,0,1,1,0,0,1,1,0,0,1},{1,1,1,1,0,0,0,0,0,0,0},{0,0,0,0,1,1,1,1,0,0,0},{0,0,0,0,0,0,0,0,1,1,1}};
raysP6 = map(QQ^5, QQ^0, 0);
linealityP6 = map(QQ^5, QQ^0, 0);
verticesP6 = promote(verticesP6, QQ);
raysP6 = promote(raysP6, QQ);
linealityP6 = promote(linealityP6, QQ);
P6 = convexHull(verticesP6,raysP6,linealityP6);
desired6 = {11,25,22,8,1};
computed6 = fVector P6;
assert(desired6 == computed6)

-- Test cayley_polytope/c4.poly
-- Checking fVector
verticesP7 = matrix {{0,1,0,1,1,1,3},{0,0,1,1,0,0,4},{1,1,1,1,0,0,0},{0,0,0,0,1,0,0},{0,0,0,0,0,1,0}};
raysP7 = map(QQ^5, QQ^0, 0);
linealityP7 = map(QQ^5, QQ^0, 0);
verticesP7 = promote(verticesP7, QQ);
raysP7 = promote(raysP7, QQ);
linealityP7 = promote(linealityP7, QQ);
P7 = convexHull(verticesP7,raysP7,linealityP7);
desired7 = {7,19,26,19,7,1};
computed7 = fVector P7;
assert(desired7 == computed7)

-- Test cayley_polytope/2.poly
-- Checking fVector
verticesP8 = matrix {{0,1,0},{0,0,1}};
raysP8 = map(QQ^2, QQ^0, 0);
linealityP8 = map(QQ^2, QQ^0, 0);
verticesP8 = promote(verticesP8, QQ);
raysP8 = promote(raysP8, QQ);
linealityP8 = promote(linealityP8, QQ);
P8 = convexHull(verticesP8,raysP8,linealityP8);
desired8 = {3,3,1};
computed8 = fVector P8;
assert(desired8 == computed8)

-- Test cayley_polytope/c3.poly
-- Checking fVector
verticesP9 = matrix {{0,1,0,1,1,3},{0,0,1,1,0,4},{1,1,1,1,0,0},{0,0,0,0,1,0},{0,0,0,0,0,1}};
raysP9 = map(QQ^5, QQ^0, 0);
linealityP9 = map(QQ^5, QQ^0, 0);
verticesP9 = promote(verticesP9, QQ);
raysP9 = promote(raysP9, QQ);
linealityP9 = promote(linealityP9, QQ);
P9 = convexHull(verticesP9,raysP9,linealityP9);
desired9 = {6,13,13,6,1};
computed9 = fVector P9;
assert(desired9 == computed9)

///

TEST ///
-- Test print_constraints/1.poly
-- Checking fVector
verticesP10 = matrix {{1,1,1,0,0,0},{1,0,0,1,1,0},{0,1,0,1,0,1},{0,0,1,0,1,1}};
raysP10 = map(QQ^4, QQ^0, 0);
linealityP10 = map(QQ^4, QQ^0, 0);
verticesP10 = promote(verticesP10, QQ);
raysP10 = promote(raysP10, QQ);
linealityP10 = promote(linealityP10, QQ);
P10 = convexHull(verticesP10,raysP10,linealityP10);
desired10 = {6,12,8,1};
computed10 = fVector P10;
assert(desired10 == computed10)

-- Test delpezzo/2.poly
-- Checking fVector
verticesP11 = matrix {{1,0,0,0,0,-1,0,0,0,0,1},{0,1,0,0,0,0,-1,0,0,0,1},{0,0,1,0,0,0,0,-1,0,0,1},{0,0,0,1,0,0,0,0,-1,0,1},{0,0,0,0,1,0,0,0,0,-1,1}};
raysP11 = map(QQ^5, QQ^0, 0);
linealityP11 = map(QQ^5, QQ^0, 0);
verticesP11 = promote(verticesP11, QQ);
raysP11 = promote(raysP11, QQ);
linealityP11 = promote(linealityP11, QQ);
P11 = convexHull(verticesP11,raysP11,linealityP11);
desired11 = {11,50,100,85,26,1};
computed11 = fVector P11;
assert(desired11 == computed11)

-- Test delpezzo/3.poly
-- Checking fVector
verticesP12 = matrix {{1,0,0,0,0,0,-1,0,0,0,0,0,1},{0,1,0,0,0,0,0,-1,0,0,0,0,1},{0,0,1,0,0,0,0,0,-1,0,0,0,1},{0,0,0,1,0,0,0,0,0,-1,0,0,1},{0,0,0,0,1,0,0,0,0,0,-1,0,1},{0,0,0,0,0,1,0,0,0,0,0,-1,1}};
raysP12 = map(QQ^6, QQ^0, 0);
linealityP12 = map(QQ^6, QQ^0, 0);
verticesP12 = promote(verticesP12, QQ);
raysP12 = promote(raysP12, QQ);
linealityP12 = promote(linealityP12, QQ);
P12 = convexHull(verticesP12,raysP12,linealityP12);
desired12 = {13,72,220,365,306,102,1};
computed12 = fVector P12;
assert(desired12 == computed12)

-- Test delpezzo/0.poly
-- Checking fVector
verticesP13 = matrix {{1,0,0,-1,0,0,1,-1},{0,1,0,0,-1,0,1,-1},{0,0,1,0,0,-1,1,-1}};
raysP13 = map(QQ^3, QQ^0, 0);
linealityP13 = map(QQ^3, QQ^0, 0);
verticesP13 = promote(verticesP13, QQ);
raysP13 = promote(raysP13, QQ);
linealityP13 = promote(linealityP13, QQ);
P13 = convexHull(verticesP13,raysP13,linealityP13);
desired13 = {8,12,6,1};
computed13 = fVector P13;
assert(desired13 == computed13)

-- Test cell_from_subdivision/1-in.poly
-- Checking fVector
verticesP14 = matrix {{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},{0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1},{0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1},{0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,0,0,0,1,1,1,0,0,0,1,0,0,0,1,0},{1,0,1,1,0,1,1,0,0,1,0,1,1,0,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,0,0,1,0,0},{1,1,0,1,1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,0,1,0,1,0,1,0,0,1,0,0,0,1,0,0,0},{1,1,1,0,1,1,0,1,0,0,1,1,0,1,0,0,1,0,0,0,1,1,0,1,0,0,1,0,0,0,1,0,0,0,0}};
raysP14 = map(QQ^7, QQ^0, 0);
linealityP14 = map(QQ^7, QQ^0, 0);
verticesP14 = promote(verticesP14, QQ);
raysP14 = promote(raysP14, QQ);
linealityP14 = promote(linealityP14, QQ);
P14 = convexHull(verticesP14,raysP14,linealityP14);
desired14 = {35,210,350,245,84,14,1};
computed14 = fVector P14;
assert(desired14 == computed14)

-- Test gc_closure/3-0.poly
-- Checking fVector
verticesP15 = matrix {{3/2,-1/2,1/2,1/2,1/2,1/2,1/2,1/2},{1/2,1/2,3/2,-1/2,1/2,1/2,1/2,1/2},{1/2,1/2,1/2,1/2,3/2,-1/2,1/2,1/2},{1/2,1/2,1/2,1/2,1/2,1/2,3/2,-1/2}};
raysP15 = map(QQ^4, QQ^0, 0);
linealityP15 = map(QQ^4, QQ^0, 0);
verticesP15 = promote(verticesP15, QQ);
raysP15 = promote(raysP15, QQ);
linealityP15 = promote(linealityP15, QQ);
P15 = convexHull(verticesP15,raysP15,linealityP15);
desired15 = {8,24,32,16,1};
computed15 = fVector P15;
assert(desired15 == computed15)

-- Test gc_closure/1.poly
-- Checking fVector
verticesP16 = matrix {{1,0,0,1,0,1,0,1},{1,1,0,0,0,0,1,1},{0,0,0,0,1,1,1,1}};
raysP16 = map(QQ^3, QQ^0, 0);
linealityP16 = map(QQ^3, QQ^0, 0);
verticesP16 = promote(verticesP16, QQ);
raysP16 = promote(raysP16, QQ);
linealityP16 = promote(linealityP16, QQ);
P16 = convexHull(verticesP16,raysP16,linealityP16);
desired16 = {8,12,6,1};
computed16 = fVector P16;
assert(desired16 == computed16)

-- Test gc_closure/4.poly
-- Checking fVector
verticesP17 = matrix {{1,0,0,1},{0,0,1,1}};
raysP17 = map(QQ^2, QQ^0, 0);
linealityP17 = map(QQ^2, QQ^0, 0);
verticesP17 = promote(verticesP17, QQ);
raysP17 = promote(raysP17, QQ);
linealityP17 = promote(linealityP17, QQ);
P17 = convexHull(verticesP17,raysP17,linealityP17);
desired17 = {4,4,1};
computed17 = fVector P17;
assert(desired17 == computed17)

-- Test gc_closure/2.poly
-- Checking fVector
verticesP18 = matrix {{0,1/2,1},{0,5,0}};
raysP18 = map(QQ^2, QQ^0, 0);
linealityP18 = map(QQ^2, QQ^0, 0);
verticesP18 = promote(verticesP18, QQ);
raysP18 = promote(raysP18, QQ);
linealityP18 = promote(linealityP18, QQ);
P18 = convexHull(verticesP18,raysP18,linealityP18);
desired18 = {3,3,1};
computed18 = fVector P18;
assert(desired18 == computed18)

-- Test gc_closure/3.poly
-- Checking fVector
verticesP19 = matrix {{2/3,2/3,0,1/3,1/2,1/2,2/3,2/3,1/3,2/3,1/3,1/3,1/3,2/3,1/2,1/3,1/3,2/3,1,1/3,2/3,1/2,1/2,1/2},{2/3,2/3,1/2,1/3,0,1/2,1/3,1/3,2/3,1/3,1/3,1/3,1/3,2/3,1/2,2/3,2/3,1/3,1/2,2/3,2/3,1/2,1/2,1},{1/3,2/3,1/2,2/3,1/2,0,1/3,2/3,1/3,1/3,1/3,2/3,1/3,1/3,1/2,2/3,1/3,2/3,1/2,2/3,2/3,1/2,1,1/2},{2/3,1/3,1/2,2/3,1/2,1/2,2/3,1/3,1/3,1/3,1/3,1/3,2/3,1/3,0,1/3,2/3,2/3,1/2,2/3,2/3,1,1/2,1/2}};
raysP19 = map(QQ^4, QQ^0, 0);
linealityP19 = map(QQ^4, QQ^0, 0);
verticesP19 = promote(verticesP19, QQ);
raysP19 = promote(raysP19, QQ);
linealityP19 = promote(linealityP19, QQ);
P19 = convexHull(verticesP19,raysP19,linealityP19);
desired19 = {24,88,96,32,1};
computed19 = fVector P19;
assert(desired19 == computed19)

///

TEST ///
-- Test gc_closure/4-0.poly
-- Checking fVector
verticesP20 = matrix {{3/2,-1/2,1/2,1/2},{1/2,1/2,3/2,-1/2}};
raysP20 = map(QQ^2, QQ^0, 0);
linealityP20 = map(QQ^2, QQ^0, 0);
verticesP20 = promote(verticesP20, QQ);
raysP20 = promote(raysP20, QQ);
linealityP20 = promote(linealityP20, QQ);
P20 = convexHull(verticesP20,raysP20,linealityP20);
desired20 = {4,4,1};
computed20 = fVector P20;
assert(desired20 == computed20)

-- Test gc_closure/1-0.poly
-- Checking fVector
verticesP21 = matrix {{0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1}};
raysP21 = map(QQ^3, QQ^0, 0);
linealityP21 = map(QQ^3, QQ^0, 0);
verticesP21 = promote(verticesP21, QQ);
raysP21 = promote(raysP21, QQ);
linealityP21 = promote(linealityP21, QQ);
P21 = convexHull(verticesP21,raysP21,linealityP21);
desired21 = {8,12,6,1};
computed21 = fVector P21;
assert(desired21 == computed21)

-- Test SCHLEGEL_VERTEX_COLORS/1.poly
-- Checking fVector
verticesP22 = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysP22 = map(QQ^4, QQ^0, 0);
linealityP22 = map(QQ^4, QQ^0, 0);
verticesP22 = promote(verticesP22, QQ);
raysP22 = promote(raysP22, QQ);
linealityP22 = promote(linealityP22, QQ);
P22 = convexHull(verticesP22,raysP22,linealityP22);
desired22 = {16,32,24,8,1};
computed22 = fVector P22;
assert(desired22 == computed22)

-- Test VISUAL_DUAL_FACE_LATTICE/1.poly
-- Checking fVector
verticesP23 = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP23 = map(QQ^3, QQ^0, 0);
linealityP23 = map(QQ^3, QQ^0, 0);
verticesP23 = promote(verticesP23, QQ);
raysP23 = promote(raysP23, QQ);
linealityP23 = promote(linealityP23, QQ);
P23 = convexHull(verticesP23,raysP23,linealityP23);
desired23 = {8,12,6,1};
computed23 = fVector P23;
assert(desired23 == computed23)

-- Test compress_incidence/1.poly
-- Checking fVector
verticesP24 = matrix {{0,0,1,3/2,5/2,5/2},{0,1/2,5/2,5/2,0,1/2}};
raysP24 = map(QQ^2, QQ^0, 0);
linealityP24 = map(QQ^2, QQ^0, 0);
verticesP24 = promote(verticesP24, QQ);
raysP24 = promote(raysP24, QQ);
linealityP24 = promote(linealityP24, QQ);
P24 = convexHull(verticesP24,raysP24,linealityP24);
desired24 = {6,6,1};
computed24 = fVector P24;
assert(desired24 == computed24)

-- Test compress_incidence/4.poly
-- Checking fVector
verticesP25 = matrix {{5/3,-1/3,-1,1,1/3},{1/3,-5/3,1,-1,5/3}};
raysP25 = map(QQ^2, QQ^0, 0);
linealityP25 = map(QQ^2, QQ^0, 0);
verticesP25 = promote(verticesP25, QQ);
raysP25 = promote(raysP25, QQ);
linealityP25 = promote(linealityP25, QQ);
P25 = convexHull(verticesP25,raysP25,linealityP25);
desired25 = {5,5,1};
computed25 = fVector P25;
assert(desired25 == computed25)

-- Test compress_incidence/5.poly
-- Checking fVector
verticesP26 = matrix {{0,15}};
raysP26 = map(QQ^1, QQ^0, 0);
linealityP26 = map(QQ^1, QQ^0, 0);
verticesP26 = promote(verticesP26, QQ);
raysP26 = promote(raysP26, QQ);
linealityP26 = promote(linealityP26, QQ);
P26 = convexHull(verticesP26,raysP26,linealityP26);
desired26 = {2,1};
computed26 = fVector P26;
assert(desired26 == computed26)

-- Test lawrence/2.poly
-- Checking fVector
verticesP27 = matrix {{1,1,2,2,3,3,0,0,0,0,0,0},{2,3,1,3,1,2,0,0,0,0,0,0},{3,2,3,1,2,1,0,0,0,0,0,0},{1,0,0,0,0,0,1,0,0,0,0,0},{0,1,0,0,0,0,0,1,0,0,0,0},{0,0,1,0,0,0,0,0,1,0,0,0},{0,0,0,1,0,0,0,0,0,1,0,0},{0,0,0,0,1,0,0,0,0,0,1,0},{0,0,0,0,0,1,0,0,0,0,0,1}};
raysP27 = map(QQ^9, QQ^0, 0);
linealityP27 = map(QQ^9, QQ^0, 0);
verticesP27 = promote(verticesP27, QQ);
raysP27 = promote(raysP27, QQ);
linealityP27 = promote(linealityP27, QQ);
P27 = convexHull(verticesP27,raysP27,linealityP27);
desired27 = {12,66,220,465,612,472,195,36,1};
computed27 = fVector P27;
assert(desired27 == computed27)

-- Test rand_sphere/1.poly
-- Checking fVector
verticesP28 = matrix {{3330883453830711/4503599627370496,853624150927223/2251799813685248,-2356553346050111/18014398509481984,8539723551892617/9007199254740992,-851543628576541/1125899906842624,5316837853882195/9007199254740992,-6538305985494021/9007199254740992,-1749267935005635/4503599627370496,7424579411304029/9007199254740992,-6887392800778413/18014398509481984},{-2298030817950255/18014398509481984,7698990389472477/9007199254740992,5054138723166979/9007199254740992,-4611252879029533/72057594037927936,7431314160210763/36028797018963968,3306083656494553/4503599627370496,-5119504026856145/9007199254740992,3819013085252451/4503599627370496,4143898238679627/9007199254740992,-5765394822723937/9007199254740992},{-5952332624486941/9007199254740992,-1596589986555531/4503599627370496,-7361860108925987/9007199254740992,-1402721951416725/4503599627370496,-1397989901669917/2251799813685248,3023262654016433/9007199254740992,-6977355308041419/18014398509481984,3248144300909891/9007199254740992,-1486048987021867/4503599627370496,-6002567455236229/9007199254740992}};
raysP28 = map(QQ^3, QQ^0, 0);
linealityP28 = map(QQ^3, QQ^0, 0);
verticesP28 = promote(verticesP28, QQ);
raysP28 = promote(raysP28, QQ);
linealityP28 = promote(linealityP28, QQ);
P28 = convexHull(verticesP28,raysP28,linealityP28);
desired28 = {10,24,16,1};
computed28 = fVector P28;
assert(desired28 == computed28)

-- Test rand_sphere/2.poly
-- Checking fVector
verticesP29 = matrix {{3/32,7/64,3/4,-3/16,-5/32,-3/4,-1/4,1/64,-7/8,-1,5/8,5/32,-3/4,3/64,-7/32,-3/8,3/8,-1,-1/32},{5/8,5/8,3/8,-1/2,1,1/2,-3/32,-1/4,7/256,-1/4,-3/4,-7/128,5/8,-3/8,-7/128,-7/8,5/8,3/16,1},{-3/4,-3/4,-5/8,-3/4,1/4,1/4,1,1,-1/2,-3/16,3/8,-1,7/256,7/8,1,-3/8,3/4,5/64,5/16}};
raysP29 = map(QQ^3, QQ^0, 0);
linealityP29 = map(QQ^3, QQ^0, 0);
verticesP29 = promote(verticesP29, QQ);
raysP29 = promote(raysP29, QQ);
linealityP29 = promote(linealityP29, QQ);
P29 = convexHull(verticesP29,raysP29,linealityP29);
desired29 = {19,51,34,1};
computed29 = fVector P29;
assert(desired29 == computed29)

///

TEST ///
-- Test zonotope/6.poly
-- Checking fVector
verticesP30 = matrix {{1,0,0,0,0,1,1,1,1,0,0,1,1,0,0,1},{0,0,2,0,2,2,0,0,2,2,2,2,2,0,0,0},{0,4,4,0,0,4,4,0,0,4,0,4,0,4,0,4},{0,0,8,8,0,8,0,8,0,0,8,0,8,8,0,8}};
raysP30 = map(QQ^4, QQ^0, 0);
linealityP30 = map(QQ^4, QQ^0, 0);
verticesP30 = promote(verticesP30, QQ);
raysP30 = promote(raysP30, QQ);
linealityP30 = promote(linealityP30, QQ);
P30 = convexHull(verticesP30,raysP30,linealityP30);
desired30 = {16,32,24,8,1};
computed30 = fVector P30;
assert(desired30 == computed30)

-- Test zonotope/8.poly
-- Checking fVector
verticesP31 = matrix {{2,4,5,-1,1,4,3,2,5,0,-5,0,-2,1,-3,4,-7,-3,-2,-4,5,-9,-4,2,5,-11,2,-1,-10,-12,2,-7,0,3,-2,-4,0,0,-9,-12,0,-12,-3,-6,2,-1,-7,-6,-7,-9,-4,-4,-1,-11,-9,-12,-9,-11,-3,-5,-8,-6,-7,-10,-5,-3,-8,-4,-7,-3,-6,-9},{-2,0,1,-4,-1,3,4,-3,-1,6,-3,-3,-3,1,-4,2,-3,7,-1,-4,2,-1,7,-2,0,0,5,7,-1,1,4,-1,-2,0,6,-1,0,4,6,4,2,2,-3,-4,4,6,6,7,1,5,6,1,3,3,-2,3,5,1,-2,6,4,-3,5,3,4,4,2,5,3,2,0,-1},{0,-1,0,0,-3,0,0,2,2,1,-1,4,-3,-5,2,-3,1,0,-5,2,3,-2,0,5,5,2,-3,-2,2,-1,4,-4,7,7,3,-6,9,6,0,0,8,2,6,4,-6,-5,-2,2,-6,2,-4,-8,-8,3,5,-3,-3,5,9,5,5,7,-5,-5,7,8,7,-7,-7,10,10,8}};
raysP31 = map(QQ^3, QQ^0, 0);
linealityP31 = map(QQ^3, QQ^0, 0);
verticesP31 = promote(verticesP31, QQ);
raysP31 = promote(raysP31, QQ);
linealityP31 = promote(linealityP31, QQ);
P31 = convexHull(verticesP31,raysP31,linealityP31);
desired31 = {72,138,68,1};
computed31 = fVector P31;
assert(desired31 == computed31)

-- Test zonotope/1.poly
-- Checking fVector
verticesP32 = matrix {{1/2,-1/2,-1/2,1/2,-1/2,1/2,-1/2,1/2,-1/2,1/2,-1/2,1/2,-1/2,1/2,-1/2,1/2},{-1,-1,1,1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1},{-2,-2,-2,-2,2,2,2,2,2,-2,-2,-2,-2,2,2,2},{-4,-4,-4,-4,-4,4,4,4,4,4,4,4,4,-4,-4,-4}};
raysP32 = map(QQ^4, QQ^0, 0);
linealityP32 = map(QQ^4, QQ^0, 0);
verticesP32 = promote(verticesP32, QQ);
raysP32 = promote(raysP32, QQ);
linealityP32 = promote(linealityP32, QQ);
P32 = convexHull(verticesP32,raysP32,linealityP32);
desired32 = {16,32,24,8,1};
computed32 = fVector P32;
assert(desired32 == computed32)

-- Test zonotope/4.poly
-- Checking fVector
verticesP33 = matrix {{-1,1,1,-1,0,0},{1,-1,0,0,1,-1},{0,0,-1,1,-1,1}};
raysP33 = map(QQ^3, QQ^0, 0);
linealityP33 = map(QQ^3, QQ^0, 0);
verticesP33 = promote(verticesP33, QQ);
raysP33 = promote(raysP33, QQ);
linealityP33 = promote(linealityP33, QQ);
P33 = convexHull(verticesP33,raysP33,linealityP33);
desired33 = {6,6,1};
computed33 = fVector P33;
assert(desired33 == computed33)

-- Test zonotope/9.poly
-- Checking fVector
verticesP34 = matrix {{0,2,0,2,0,2,0,2,0,2,0,2},{0,0,2,2,4,4,0,0,2,2,4,4},{0,0,2,2,0,0,-2,-2,-4,-4,-2,-2}};
raysP34 = map(QQ^3, QQ^0, 0);
linealityP34 = map(QQ^3, QQ^0, 0);
verticesP34 = promote(verticesP34, QQ);
raysP34 = promote(raysP34, QQ);
linealityP34 = promote(linealityP34, QQ);
P34 = convexHull(verticesP34,raysP34,linealityP34);
desired34 = {12,18,8,1};
computed34 = fVector P34;
assert(desired34 == computed34)

-- Test zonotope/2.poly
-- Checking fVector
verticesP35 = matrix {{-7/2,7/2,-3/2,3/2},{-5,5,-2,2}};
raysP35 = map(QQ^2, QQ^0, 0);
linealityP35 = map(QQ^2, QQ^0, 0);
verticesP35 = promote(verticesP35, QQ);
raysP35 = promote(raysP35, QQ);
linealityP35 = promote(linealityP35, QQ);
P35 = convexHull(verticesP35,raysP35,linealityP35);
desired35 = {4,4,1};
computed35 = fVector P35;
assert(desired35 == computed35)

-- Test zonotope/3.poly
-- Checking fVector
verticesP36 = matrix {{0,0,-1,1},{-3/2,3/2,-7/2,7/2},{-2,2,-5,5}};
raysP36 = map(QQ^3, QQ^0, 0);
linealityP36 = map(QQ^3, QQ^0, 0);
verticesP36 = promote(verticesP36, QQ);
raysP36 = promote(raysP36, QQ);
linealityP36 = promote(linealityP36, QQ);
P36 = convexHull(verticesP36,raysP36,linealityP36);
desired36 = {4,4,1};
computed36 = fVector P36;
assert(desired36 == computed36)

-- Test zonotope/7.poly
-- Checking fVector
verticesP37 = matrix {{1,2,2,0,1,0},{-1,-1,0,1,1,0},{0,-1,-2,-1,-2,0}};
raysP37 = map(QQ^3, QQ^0, 0);
linealityP37 = map(QQ^3, QQ^0, 0);
verticesP37 = promote(verticesP37, QQ);
raysP37 = promote(raysP37, QQ);
linealityP37 = promote(linealityP37, QQ);
P37 = convexHull(verticesP37,raysP37,linealityP37);
desired37 = {6,6,1};
computed37 = fVector P37;
assert(desired37 == computed37)

-- Test representations/1.poly
-- Checking fVector
verticesP38 = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP38 = map(QQ^3, QQ^0, 0);
linealityP38 = map(QQ^3, QQ^0, 0);
verticesP38 = promote(verticesP38, QQ);
raysP38 = promote(raysP38, QQ);
linealityP38 = promote(linealityP38, QQ);
P38 = convexHull(verticesP38,raysP38,linealityP38);
desired38 = {8,12,6,1};
computed38 = fVector P38;
assert(desired38 == computed38)

-- Test SIMPLICITY/1.poly
-- Checking fVector
verticesP39 = matrix {{0,1,0,0,0},{1,0,0,0,0},{0,0,1,0,2}};
raysP39 = map(QQ^3, QQ^0, 0);
linealityP39 = map(QQ^3, QQ^0, 0);
verticesP39 = promote(verticesP39, QQ);
raysP39 = promote(raysP39, QQ);
linealityP39 = promote(linealityP39, QQ);
P39 = convexHull(verticesP39,raysP39,linealityP39);
desired39 = {4,6,4,1};
computed39 = fVector P39;
assert(desired39 == computed39)

///

TEST ///
-- Test SIMPLICITY/2.poly
-- Checking fVector
verticesP40 = matrix {{0,1,0,0},{1,0,0,0},{0,0,1,0}};
raysP40 = map(QQ^3, QQ^0, 0);
linealityP40 = map(QQ^3, QQ^0, 0);
verticesP40 = promote(verticesP40, QQ);
raysP40 = promote(raysP40, QQ);
linealityP40 = promote(linealityP40, QQ);
P40 = convexHull(verticesP40,raysP40,linealityP40);
desired40 = {4,6,4,1};
computed40 = fVector P40;
assert(desired40 == computed40)

-- Test VERTEX_LABELS/Q4.poly
-- Checking fVector
verticesP41 = matrix {{-4/29,-1/2,0,1/19,1/12,19/30,-7/12,1/2,2/3,1/6,4/29,3/19,2/3,2/29,0,0,1/2,7/12,-1/19,-2/29,-19/30,-1/12,-1/2,-2/3,-1/6,-2/3,-3/19},{2/29,-1/2,0,3/19,1/6,2/3,2/3,1/2,-19/30,-1/12,-2/29,-1/19,7/12,4/29,0,0,-1/2,-2/3,-3/19,-4/29,-2/3,-1/6,1/2,19/30,1/12,-7/12,1/19},{-5/29,2,1,3/19,1/4,19/10,-7/4,2,-19/10,-1/4,-5/29,-3/19,7/4,5/29,0,-1,-2,-7/4,3/19,5/29,19/10,1/4,-2,-19/10,-1/4,7/4,-3/19},{8/29,-1/2,0,5/19,1/4,-1/2,-1/2,-1/2,-1/2,1/4,8/29,5/19,-1/2,8/29,1/3,0,-1/2,-1/2,5/19,8/29,-1/2,1/4,-1/2,-1/2,1/4,-1/2,5/19}};
raysP41 = map(QQ^4, QQ^0, 0);
linealityP41 = map(QQ^4, QQ^0, 0);
verticesP41 = promote(verticesP41, QQ);
raysP41 = promote(raysP41, QQ);
linealityP41 = promote(linealityP41, QQ);
P41 = convexHull(verticesP41,raysP41,linealityP41);
desired41 = {27,54,36,9,1};
computed41 = fVector P41;
assert(desired41 == computed41)

-- Test VERTEX_LABELS/2.poly
-- Checking fVector
verticesP42 = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP42 = map(QQ^3, QQ^0, 0);
linealityP42 = map(QQ^3, QQ^0, 0);
verticesP42 = promote(verticesP42, QQ);
raysP42 = promote(raysP42, QQ);
linealityP42 = promote(linealityP42, QQ);
P42 = convexHull(verticesP42,raysP42,linealityP42);
desired42 = {8,12,6,1};
computed42 = fVector P42;
assert(desired42 == computed42)

-- Test 4ti2/birkhoff2.poly
-- Checking fVector
verticesP43 = matrix {{1,0},{0,1},{0,1},{1,0}};
raysP43 = map(QQ^4, QQ^0, 0);
linealityP43 = map(QQ^4, QQ^0, 0);
verticesP43 = promote(verticesP43, QQ);
raysP43 = promote(raysP43, QQ);
linealityP43 = promote(linealityP43, QQ);
P43 = convexHull(verticesP43,raysP43,linealityP43);
desired43 = {2,1};
computed43 = fVector P43;
assert(desired43 == computed43)

-- Test 4ti2/zsolve.poly
-- Checking fVector
verticesP44 = matrix {{1,1,1,1,-1,-1,-1,-1},{-1,-1,1,1,1,1,-1,-1},{-1,1,1,-1,1,-1,1,-1}};
raysP44 = map(QQ^3, QQ^0, 0);
linealityP44 = map(QQ^3, QQ^0, 0);
verticesP44 = promote(verticesP44, QQ);
raysP44 = promote(raysP44, QQ);
linealityP44 = promote(linealityP44, QQ);
P44 = convexHull(verticesP44,raysP44,linealityP44);
desired44 = {8,12,6,1};
computed44 = fVector P44;
assert(desired44 == computed44)

-- Test 4ti2/hilb2.poly
-- Checking fVector
verticesP45 = matrix {{0,2,2,1,0},{0,0,2,2,1}};
raysP45 = map(QQ^2, QQ^0, 0);
linealityP45 = map(QQ^2, QQ^0, 0);
verticesP45 = promote(verticesP45, QQ);
raysP45 = promote(raysP45, QQ);
linealityP45 = promote(linealityP45, QQ);
P45 = convexHull(verticesP45,raysP45,linealityP45);
desired45 = {5,5,1};
computed45 = fVector P45;
assert(desired45 == computed45)

-- Test 4ti2/hilb1.poly
-- Checking fVector
verticesP46 = matrix {{0,1,0,1,2,1},{0,0,1,1,1,2},{0,0,0,3,3,3}};
raysP46 = map(QQ^3, QQ^0, 0);
linealityP46 = map(QQ^3, QQ^0, 0);
verticesP46 = promote(verticesP46, QQ);
raysP46 = promote(raysP46, QQ);
linealityP46 = promote(linealityP46, QQ);
P46 = convexHull(verticesP46,raysP46,linealityP46);
desired46 = {6,9,5,1};
computed46 = fVector P46;
assert(desired46 == computed46)

-- Test 4ti2/simplex.poly
-- Checking fVector
verticesP47 = matrix {{0,5/2,0},{0,0,5/2}};
raysP47 = map(QQ^2, QQ^0, 0);
linealityP47 = map(QQ^2, QQ^0, 0);
verticesP47 = promote(verticesP47, QQ);
raysP47 = promote(raysP47, QQ);
linealityP47 = promote(linealityP47, QQ);
P47 = convexHull(verticesP47,raysP47,linealityP47);
desired47 = {3,3,1};
computed47 = fVector P47;
assert(desired47 == computed47)

-- Test 4ti2/groeb2.poly
-- Checking fVector
verticesP48 = matrix {{1,0,0,1,0,0},{0,1,0,0,1,0},{0,0,1,0,0,1},{0,1,1,0,0,0},{1,0,0,0,0,1},{0,0,0,1,1,0},{0,0,0,0,1,1},{0,0,1,1,0,0},{1,1,0,0,0,0}};
raysP48 = map(QQ^9, QQ^0, 0);
linealityP48 = map(QQ^9, QQ^0, 0);
verticesP48 = promote(verticesP48, QQ);
raysP48 = promote(raysP48, QQ);
linealityP48 = promote(linealityP48, QQ);
P48 = convexHull(verticesP48,raysP48,linealityP48);
desired48 = {6,15,18,9,1};
computed48 = fVector P48;
assert(desired48 == computed48)

-- Test 4ti2/ratsimplex.poly
-- Checking fVector
verticesP49 = matrix {{0,5/2,0},{0,0,5/2}};
raysP49 = map(QQ^2, QQ^0, 0);
linealityP49 = map(QQ^2, QQ^0, 0);
verticesP49 = promote(verticesP49, QQ);
raysP49 = promote(raysP49, QQ);
linealityP49 = promote(linealityP49, QQ);
P49 = convexHull(verticesP49,raysP49,linealityP49);
desired49 = {3,3,1};
computed49 = fVector P49;
assert(desired49 == computed49)

///

TEST ///
-- Test 4ti2/ratcube.poly
-- Checking fVector
verticesP50 = matrix {{-1/2,1/2,-1/2,1/2},{-1/2,-1/2,1/2,1/2}};
raysP50 = map(QQ^2, QQ^0, 0);
linealityP50 = map(QQ^2, QQ^0, 0);
verticesP50 = promote(verticesP50, QQ);
raysP50 = promote(raysP50, QQ);
linealityP50 = promote(linealityP50, QQ);
P50 = convexHull(verticesP50,raysP50,linealityP50);
desired50 = {4,4,1};
computed50 = fVector P50;
assert(desired50 == computed50)

-- Test 4ti2/groeb-drl.poly
-- Checking fVector
verticesP51 = matrix {{1,1,0,-1,-1},{0,1,1,-1,0}};
raysP51 = map(QQ^2, QQ^0, 0);
linealityP51 = map(QQ^2, QQ^0, 0);
verticesP51 = promote(verticesP51, QQ);
raysP51 = promote(raysP51, QQ);
linealityP51 = promote(linealityP51, QQ);
P51 = convexHull(verticesP51,raysP51,linealityP51);
desired51 = {5,5,1};
computed51 = fVector P51;
assert(desired51 == computed51)

-- Test 4ti2/zsolve1.poly
-- Checking fVector
verticesP52 = matrix {{0,3},{2,-1}};
raysP52 = map(QQ^2, QQ^0, 0);
linealityP52 = map(QQ^2, QQ^0, 0);
verticesP52 = promote(verticesP52, QQ);
raysP52 = promote(raysP52, QQ);
linealityP52 = promote(linealityP52, QQ);
P52 = convexHull(verticesP52,raysP52,linealityP52);
desired52 = {2,1};
computed52 = fVector P52;
assert(desired52 == computed52)

-- Test 4ti2/groeb-dl.poly
-- Checking fVector
verticesP53 = matrix {{1,1,0,-1,-1},{0,1,1,-1,0}};
raysP53 = map(QQ^2, QQ^0, 0);
linealityP53 = map(QQ^2, QQ^0, 0);
verticesP53 = promote(verticesP53, QQ);
raysP53 = promote(raysP53, QQ);
linealityP53 = promote(linealityP53, QQ);
P53 = convexHull(verticesP53,raysP53,linealityP53);
desired53 = {5,5,1};
computed53 = fVector P53;
assert(desired53 == computed53)

-- Test 4ti2/groeb-l.poly
-- Checking fVector
verticesP54 = matrix {{1,1,0,-1},{0,1,1,-1}};
raysP54 = map(QQ^2, QQ^0, 0);
linealityP54 = map(QQ^2, QQ^0, 0);
verticesP54 = promote(verticesP54, QQ);
raysP54 = promote(raysP54, QQ);
linealityP54 = promote(linealityP54, QQ);
P54 = convexHull(verticesP54,raysP54,linealityP54);
desired54 = {4,4,1};
computed54 = fVector P54;
assert(desired54 == computed54)

-- Test 4ti2/groeb1.poly
-- Checking fVector
verticesP55 = matrix {{1,4}};
raysP55 = map(QQ^1, QQ^0, 0);
linealityP55 = map(QQ^1, QQ^0, 0);
verticesP55 = promote(verticesP55, QQ);
raysP55 = promote(raysP55, QQ);
linealityP55 = promote(linealityP55, QQ);
P55 = convexHull(verticesP55,raysP55,linealityP55);
desired55 = {2,1};
computed55 = fVector P55;
assert(desired55 == computed55)

-- Test 4ti2/sparse.poly
-- Checking fVector
verticesP56 = matrix {{1,0,0,0,0,0},{0,1,0,0,0,0},{0,0,1,0,0,0},{0,0,0,1,0,0},{0,0,0,0,1,0}};
raysP56 = map(QQ^5, QQ^0, 0);
linealityP56 = map(QQ^5, QQ^0, 0);
verticesP56 = promote(verticesP56, QQ);
raysP56 = promote(raysP56, QQ);
linealityP56 = promote(linealityP56, QQ);
P56 = convexHull(verticesP56,raysP56,linealityP56);
desired56 = {6,15,20,15,6,1};
computed56 = fVector P56;
assert(desired56 == computed56)

-- Test POSITIVE/1.poly
-- Checking fVector
verticesP57 = matrix {{0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1},{0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1}};
raysP57 = map(QQ^4, QQ^0, 0);
linealityP57 = map(QQ^4, QQ^0, 0);
verticesP57 = promote(verticesP57, QQ);
raysP57 = promote(raysP57, QQ);
linealityP57 = promote(linealityP57, QQ);
P57 = convexHull(verticesP57,raysP57,linealityP57);
desired57 = {16,32,24,8,1};
computed57 = fVector P57;
assert(desired57 == computed57)

-- Test POSITIVE/2.poly
-- Checking fVector
verticesP58 = matrix {{1,-1,0,0,0,0,0,0},{0,0,1,-1,0,0,0,0},{0,0,0,0,1,-1,0,0},{0,0,0,0,0,0,1,-1}};
raysP58 = map(QQ^4, QQ^0, 0);
linealityP58 = map(QQ^4, QQ^0, 0);
verticesP58 = promote(verticesP58, QQ);
raysP58 = promote(raysP58, QQ);
linealityP58 = promote(linealityP58, QQ);
P58 = convexHull(verticesP58,raysP58,linealityP58);
desired58 = {8,24,32,16,1};
computed58 = fVector P58;
assert(desired58 == computed58)

-- Test split_compatibility_graph/1.poly
-- Checking fVector
verticesP59 = matrix {{-1,1,-1,1},{-1,-1,1,1}};
raysP59 = map(QQ^2, QQ^0, 0);
linealityP59 = map(QQ^2, QQ^0, 0);
verticesP59 = promote(verticesP59, QQ);
raysP59 = promote(raysP59, QQ);
linealityP59 = promote(linealityP59, QQ);
P59 = convexHull(verticesP59,raysP59,linealityP59);
desired59 = {4,4,1};
computed59 = fVector P59;
assert(desired59 == computed59)

///

TEST ///
-- Test split_compatibility_graph/2.poly
-- Checking fVector
verticesP60 = matrix {{0,1,2,1,0},{0,0,1,3,1}};
raysP60 = map(QQ^2, QQ^0, 0);
linealityP60 = map(QQ^2, QQ^0, 0);
verticesP60 = promote(verticesP60, QQ);
raysP60 = promote(raysP60, QQ);
linealityP60 = promote(linealityP60, QQ);
P60 = convexHull(verticesP60,raysP60,linealityP60);
desired60 = {5,5,1};
computed60 = fVector P60;
assert(desired60 == computed60)

-- Test split_compatibility_graph/3.poly
-- Checking fVector
verticesP61 = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP61 = map(QQ^3, QQ^0, 0);
linealityP61 = map(QQ^3, QQ^0, 0);
verticesP61 = promote(verticesP61, QQ);
raysP61 = promote(raysP61, QQ);
linealityP61 = promote(linealityP61, QQ);
P61 = convexHull(verticesP61,raysP61,linealityP61);
desired61 = {8,12,6,1};
computed61 = fVector P61;
assert(desired61 == computed61)

-- Test porta/1.poly
-- Checking fVector
verticesP62 = matrix {{3,5/3,1},{3,1,5/2}};
raysP62 = map(QQ^2, QQ^0, 0);
linealityP62 = map(QQ^2, QQ^0, 0);
verticesP62 = promote(verticesP62, QQ);
raysP62 = promote(raysP62, QQ);
linealityP62 = promote(linealityP62, QQ);
P62 = convexHull(verticesP62,raysP62,linealityP62);
desired62 = {3,3,1};
computed62 = fVector P62;
assert(desired62 == computed62)

-- Test VISUAL_MIN_MAX_FACE/in1.poly
-- Checking fVector
verticesP63 = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP63 = map(QQ^3, QQ^0, 0);
linealityP63 = map(QQ^3, QQ^0, 0);
verticesP63 = promote(verticesP63, QQ);
raysP63 = promote(raysP63, QQ);
linealityP63 = promote(linealityP63, QQ);
P63 = convexHull(verticesP63,raysP63,linealityP63);
desired63 = {8,12,6,1};
computed63 = fVector P63;
assert(desired63 == computed63)

-- Test VISUAL_MIN_MAX_FACE/1.poly
-- Checking fVector
verticesP64 = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP64 = map(QQ^3, QQ^0, 0);
linealityP64 = map(QQ^3, QQ^0, 0);
verticesP64 = promote(verticesP64, QQ);
raysP64 = promote(raysP64, QQ);
linealityP64 = promote(linealityP64, QQ);
P64 = convexHull(verticesP64,raysP64,linealityP64);
desired64 = {8,12,6,1};
computed64 = fVector P64;
assert(desired64 == computed64)

-- Test VISUAL_MIN_MAX_FACE/in2.poly
-- Checking fVector
verticesP65 = matrix {{0,0,910872158600853/1125899906842624,-910872158600853/1125899906842624,1/2,-1/2,1/2,-1/2,910872158600853/1125899906842624,-910872158600853/1125899906842624,0,0},{910872158600853/1125899906842624,910872158600853/1125899906842624,1/2,1/2,0,0,0,0,-1/2,-1/2,-910872158600853/1125899906842624,-910872158600853/1125899906842624},{1/2,-1/2,0,0,910872158600853/1125899906842624,910872158600853/1125899906842624,-910872158600853/1125899906842624,-910872158600853/1125899906842624,0,0,1/2,-1/2}};
raysP65 = map(QQ^3, QQ^0, 0);
linealityP65 = map(QQ^3, QQ^0, 0);
verticesP65 = promote(verticesP65, QQ);
raysP65 = promote(raysP65, QQ);
linealityP65 = promote(linealityP65, QQ);
P65 = convexHull(verticesP65,raysP65,linealityP65);
desired65 = {12,30,20,1};
computed65 = fVector P65;
assert(desired65 == computed65)

///
