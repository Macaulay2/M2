TEST ///
-- Test 2 1 - 2 1
-- Checking minkowskiSum
verticesP0 = matrix {{1,-1}};
raysP0 = map(QQ^1, QQ^0, 0);
linealityP0 = map(QQ^1, QQ^0, 0);
verticesP0 = promote(verticesP0, QQ);
raysP0 = promote(raysP0, QQ);
linealityP0 = promote(linealityP0, QQ);
P0 = convexHull(verticesP0,raysP0,linealityP0);
verticesQ0 = matrix {{-1,1}};
raysQ0 = map(QQ^1, QQ^0, 0);
linealityQ0 = map(QQ^1, QQ^0, 0);
verticesQ0 = promote(verticesQ0, QQ);
raysQ0 = promote(raysQ0, QQ);
linealityQ0 = promote(linealityQ0, QQ);
Q0 = convexHull(verticesQ0,raysQ0,linealityQ0);
verticesdesired0 = matrix {{2,-2}};
raysdesired0 = map(QQ^1, QQ^0, 0);
linealitydesired0 = map(QQ^1, QQ^0, 0);
verticesdesired0 = promote(verticesdesired0, QQ);
raysdesired0 = promote(raysdesired0, QQ);
linealitydesired0 = promote(linealitydesired0, QQ);
desired0 = convexHull(verticesdesired0,raysdesired0,linealitydesired0);
computed0 = P0+Q0;
assert(computed0 == desired0)

-- Test 2 1 - 1 1
-- Checking minkowskiSum
verticesP1 = matrix {{-1,1}};
raysP1 = map(QQ^1, QQ^0, 0);
linealityP1 = map(QQ^1, QQ^0, 0);
verticesP1 = promote(verticesP1, QQ);
raysP1 = promote(raysP1, QQ);
linealityP1 = promote(linealityP1, QQ);
P1 = convexHull(verticesP1,raysP1,linealityP1);
verticesQ1 = matrix {{1}};
raysQ1 = map(QQ^1, QQ^0, 0);
linealityQ1 = map(QQ^1, QQ^0, 0);
verticesQ1 = promote(verticesQ1, QQ);
raysQ1 = promote(raysQ1, QQ);
linealityQ1 = promote(linealityQ1, QQ);
Q1 = convexHull(verticesQ1,raysQ1,linealityQ1);
verticesdesired1 = matrix {{0,2}};
raysdesired1 = map(QQ^1, QQ^0, 0);
linealitydesired1 = map(QQ^1, QQ^0, 0);
verticesdesired1 = promote(verticesdesired1, QQ);
raysdesired1 = promote(raysdesired1, QQ);
linealitydesired1 = promote(linealitydesired1, QQ);
desired1 = convexHull(verticesdesired1,raysdesired1,linealitydesired1);
computed1 = P1+Q1;
assert(computed1 == desired1)

-- Test 0 1 - 2 1
-- Checking minkowskiSum
verticesP2 = map(QQ^1, QQ^0, 0);
raysP2 = map(QQ^1, QQ^0, 0);
linealityP2 = map(QQ^1, QQ^0, 0);
verticesP2 = promote(verticesP2, QQ);
raysP2 = promote(raysP2, QQ);
linealityP2 = promote(linealityP2, QQ);
P2 = convexHull(verticesP2,raysP2,linealityP2);
verticesQ2 = matrix {{0,2}};
raysQ2 = map(QQ^1, QQ^0, 0);
linealityQ2 = map(QQ^1, QQ^0, 0);
verticesQ2 = promote(verticesQ2, QQ);
raysQ2 = promote(raysQ2, QQ);
linealityQ2 = promote(linealityQ2, QQ);
Q2 = convexHull(verticesQ2,raysQ2,linealityQ2);
verticesdesired2 = map(QQ^1, QQ^0, 0);
raysdesired2 = map(QQ^1, QQ^0, 0);
linealitydesired2 = map(QQ^1, QQ^0, 0);
verticesdesired2 = promote(verticesdesired2, QQ);
raysdesired2 = promote(raysdesired2, QQ);
linealitydesired2 = promote(linealitydesired2, QQ);
desired2 = convexHull(verticesdesired2,raysdesired2,linealitydesired2);
computed2 = P2+Q2;
assert(computed2 == desired2)

-- Test 2 0 - 2 0
-- Checking minkowskiSum
verticesP3 = matrix {{1}};
raysP3 = matrix {{1}};
linealityP3 = map(QQ^1, QQ^0, 0);
verticesP3 = promote(verticesP3, QQ);
raysP3 = promote(raysP3, QQ);
linealityP3 = promote(linealityP3, QQ);
P3 = convexHull(verticesP3,raysP3,linealityP3);
verticesQ3 = matrix {{1}};
raysQ3 = matrix {{1}};
linealityQ3 = map(QQ^1, QQ^0, 0);
verticesQ3 = promote(verticesQ3, QQ);
raysQ3 = promote(raysQ3, QQ);
linealityQ3 = promote(linealityQ3, QQ);
Q3 = convexHull(verticesQ3,raysQ3,linealityQ3);
verticesdesired3 = matrix {{2}};
raysdesired3 = matrix {{1}};
linealitydesired3 = map(QQ^1, QQ^0, 0);
verticesdesired3 = promote(verticesdesired3, QQ);
raysdesired3 = promote(raysdesired3, QQ);
linealitydesired3 = promote(linealitydesired3, QQ);
desired3 = convexHull(verticesdesired3,raysdesired3,linealitydesired3);
computed3 = P3+Q3;
assert(computed3 == desired3)

-- Test 2 1 - 0 1
-- Checking minkowskiSum
verticesP4 = matrix {{0,1}};
raysP4 = map(QQ^1, QQ^0, 0);
linealityP4 = map(QQ^1, QQ^0, 0);
verticesP4 = promote(verticesP4, QQ);
raysP4 = promote(raysP4, QQ);
linealityP4 = promote(linealityP4, QQ);
P4 = convexHull(verticesP4,raysP4,linealityP4);
verticesQ4 = map(QQ^1, QQ^0, 0);
raysQ4 = map(QQ^1, QQ^0, 0);
linealityQ4 = map(QQ^1, QQ^0, 0);
verticesQ4 = promote(verticesQ4, QQ);
raysQ4 = promote(raysQ4, QQ);
linealityQ4 = promote(linealityQ4, QQ);
Q4 = convexHull(verticesQ4,raysQ4,linealityQ4);
verticesdesired4 = map(QQ^1, QQ^0, 0);
raysdesired4 = map(QQ^1, QQ^0, 0);
linealitydesired4 = map(QQ^1, QQ^0, 0);
verticesdesired4 = promote(verticesdesired4, QQ);
raysdesired4 = promote(raysdesired4, QQ);
linealitydesired4 = promote(linealitydesired4, QQ);
desired4 = convexHull(verticesdesired4,raysdesired4,linealitydesired4);
computed4 = P4+Q4;
assert(computed4 == desired4)

-- Test 2 0 - 2 0
-- Checking minkowskiSum
verticesP5 = matrix {{1}};
raysP5 = matrix {{1}};
linealityP5 = map(QQ^1, QQ^0, 0);
verticesP5 = promote(verticesP5, QQ);
raysP5 = promote(raysP5, QQ);
linealityP5 = promote(linealityP5, QQ);
P5 = convexHull(verticesP5,raysP5,linealityP5);
verticesQ5 = matrix {{0}};
raysQ5 = matrix {{1}};
linealityQ5 = map(QQ^1, QQ^0, 0);
verticesQ5 = promote(verticesQ5, QQ);
raysQ5 = promote(raysQ5, QQ);
linealityQ5 = promote(linealityQ5, QQ);
Q5 = convexHull(verticesQ5,raysQ5,linealityQ5);
verticesdesired5 = matrix {{1}};
raysdesired5 = matrix {{1}};
linealitydesired5 = map(QQ^1, QQ^0, 0);
verticesdesired5 = promote(verticesdesired5, QQ);
raysdesired5 = promote(raysdesired5, QQ);
linealitydesired5 = promote(linealitydesired5, QQ);
desired5 = convexHull(verticesdesired5,raysdesired5,linealitydesired5);
computed5 = P5+Q5;
assert(computed5 == desired5)

-- Test 2 1 - 2 0
-- Checking minkowskiSum
verticesP6 = matrix {{-1,1}};
raysP6 = map(QQ^1, QQ^0, 0);
linealityP6 = map(QQ^1, QQ^0, 0);
verticesP6 = promote(verticesP6, QQ);
raysP6 = promote(raysP6, QQ);
linealityP6 = promote(linealityP6, QQ);
P6 = convexHull(verticesP6,raysP6,linealityP6);
verticesQ6 = matrix {{1}};
raysQ6 = matrix {{1}};
linealityQ6 = map(QQ^1, QQ^0, 0);
verticesQ6 = promote(verticesQ6, QQ);
raysQ6 = promote(raysQ6, QQ);
linealityQ6 = promote(linealityQ6, QQ);
Q6 = convexHull(verticesQ6,raysQ6,linealityQ6);
verticesdesired6 = matrix {{0}};
raysdesired6 = matrix {{1}};
linealitydesired6 = map(QQ^1, QQ^0, 0);
verticesdesired6 = promote(verticesdesired6, QQ);
raysdesired6 = promote(raysdesired6, QQ);
linealitydesired6 = promote(linealitydesired6, QQ);
desired6 = convexHull(verticesdesired6,raysdesired6,linealitydesired6);
computed6 = P6+Q6;
assert(computed6 == desired6)

-- Test 2 0 - 2 1
-- Checking minkowskiSum
verticesP7 = matrix {{0}};
raysP7 = matrix {{1}};
linealityP7 = map(QQ^1, QQ^0, 0);
verticesP7 = promote(verticesP7, QQ);
raysP7 = promote(raysP7, QQ);
linealityP7 = promote(linealityP7, QQ);
P7 = convexHull(verticesP7,raysP7,linealityP7);
verticesQ7 = matrix {{0,1}};
raysQ7 = map(QQ^1, QQ^0, 0);
linealityQ7 = map(QQ^1, QQ^0, 0);
verticesQ7 = promote(verticesQ7, QQ);
raysQ7 = promote(raysQ7, QQ);
linealityQ7 = promote(linealityQ7, QQ);
Q7 = convexHull(verticesQ7,raysQ7,linealityQ7);
verticesdesired7 = matrix {{0}};
raysdesired7 = matrix {{1}};
linealitydesired7 = map(QQ^1, QQ^0, 0);
verticesdesired7 = promote(verticesdesired7, QQ);
raysdesired7 = promote(raysdesired7, QQ);
linealitydesired7 = promote(linealitydesired7, QQ);
desired7 = convexHull(verticesdesired7,raysdesired7,linealitydesired7);
computed7 = P7+Q7;
assert(computed7 == desired7)

-- Test 2 1 - 0 1
-- Checking minkowskiSum
verticesP8 = matrix {{0,1}};
raysP8 = map(QQ^1, QQ^0, 0);
linealityP8 = map(QQ^1, QQ^0, 0);
verticesP8 = promote(verticesP8, QQ);
raysP8 = promote(raysP8, QQ);
linealityP8 = promote(linealityP8, QQ);
P8 = convexHull(verticesP8,raysP8,linealityP8);
verticesQ8 = map(QQ^1, QQ^0, 0);
raysQ8 = map(QQ^1, QQ^0, 0);
linealityQ8 = map(QQ^1, QQ^0, 0);
verticesQ8 = promote(verticesQ8, QQ);
raysQ8 = promote(raysQ8, QQ);
linealityQ8 = promote(linealityQ8, QQ);
Q8 = convexHull(verticesQ8,raysQ8,linealityQ8);
verticesdesired8 = map(QQ^1, QQ^0, 0);
raysdesired8 = map(QQ^1, QQ^0, 0);
linealitydesired8 = map(QQ^1, QQ^0, 0);
verticesdesired8 = promote(verticesdesired8, QQ);
raysdesired8 = promote(raysdesired8, QQ);
linealitydesired8 = promote(linealitydesired8, QQ);
desired8 = convexHull(verticesdesired8,raysdesired8,linealitydesired8);
computed8 = P8+Q8;
assert(computed8 == desired8)

-- Test 2 1 - 1 1
-- Checking minkowskiSum
verticesP9 = matrix {{-1,1}};
raysP9 = map(QQ^1, QQ^0, 0);
linealityP9 = map(QQ^1, QQ^0, 0);
verticesP9 = promote(verticesP9, QQ);
raysP9 = promote(raysP9, QQ);
linealityP9 = promote(linealityP9, QQ);
P9 = convexHull(verticesP9,raysP9,linealityP9);
verticesQ9 = matrix {{0}};
raysQ9 = map(QQ^1, QQ^0, 0);
linealityQ9 = map(QQ^1, QQ^0, 0);
verticesQ9 = promote(verticesQ9, QQ);
raysQ9 = promote(raysQ9, QQ);
linealityQ9 = promote(linealityQ9, QQ);
Q9 = convexHull(verticesQ9,raysQ9,linealityQ9);
verticesdesired9 = matrix {{-1,1}};
raysdesired9 = map(QQ^1, QQ^0, 0);
linealitydesired9 = map(QQ^1, QQ^0, 0);
verticesdesired9 = promote(verticesdesired9, QQ);
raysdesired9 = promote(raysdesired9, QQ);
linealitydesired9 = promote(linealitydesired9, QQ);
desired9 = convexHull(verticesdesired9,raysdesired9,linealitydesired9);
computed9 = P9+Q9;
assert(computed9 == desired9)

///

TEST ///
-- Test 3 0 - 4 1
-- Checking minkowskiSum
verticesP10 = matrix {{2},{2}};
raysP10 = matrix {{1,1},{-1,1}};
linealityP10 = map(QQ^2, QQ^0, 0);
verticesP10 = promote(verticesP10, QQ);
raysP10 = promote(raysP10, QQ);
linealityP10 = promote(linealityP10, QQ);
P10 = convexHull(verticesP10,raysP10,linealityP10);
verticesQ10 = matrix {{1,-1,1,-1},{3/2,3/2,-1/2,-1/2}};
raysQ10 = map(QQ^2, QQ^0, 0);
linealityQ10 = map(QQ^2, QQ^0, 0);
verticesQ10 = promote(verticesQ10, QQ);
raysQ10 = promote(raysQ10, QQ);
linealityQ10 = promote(linealityQ10, QQ);
Q10 = convexHull(verticesQ10,raysQ10,linealityQ10);
verticesdesired10 = matrix {{1,1},{7/2,3/2}};
raysdesired10 = matrix {{1,1},{-1,1}};
linealitydesired10 = map(QQ^2, QQ^0, 0);
verticesdesired10 = promote(verticesdesired10, QQ);
raysdesired10 = promote(raysdesired10, QQ);
linealitydesired10 = promote(linealitydesired10, QQ);
desired10 = convexHull(verticesdesired10,raysdesired10,linealitydesired10);
computed10 = P10+Q10;
assert(computed10 == desired10)

-- Test 12 1 - 4 1
-- Checking minkowskiSum
verticesP11 = matrix {{1,7800463371553963/9007199254740992,4503599627370497/9007199254740992,0,-4503599627370497/9007199254740992,-7800463371553963/9007199254740992,-1,-7800463371553963/9007199254740992,-4503599627370497/9007199254740992,0,4503599627370497/9007199254740992,7800463371553963/9007199254740992},{0,9007199254740991/18014398509481984,3900231685776981/4503599627370496,1,3900231685776981/4503599627370496,9007199254740991/18014398509481984,0,-9007199254740991/18014398509481984,-3900231685776981/4503599627370496,-1,-3900231685776981/4503599627370496,-9007199254740991/18014398509481984}};
raysP11 = map(QQ^2, QQ^0, 0);
linealityP11 = map(QQ^2, QQ^0, 0);
verticesP11 = promote(verticesP11, QQ);
raysP11 = promote(raysP11, QQ);
linealityP11 = promote(linealityP11, QQ);
P11 = convexHull(verticesP11,raysP11,linealityP11);
verticesQ11 = matrix {{0,0,1,1},{0,1,0,1}};
raysQ11 = map(QQ^2, QQ^0, 0);
linealityQ11 = map(QQ^2, QQ^0, 0);
verticesQ11 = promote(verticesQ11, QQ);
raysQ11 = promote(raysQ11, QQ);
linealityQ11 = promote(linealityQ11, QQ);
Q11 = convexHull(verticesQ11,raysQ11,linealityQ11);
verticesdesired11 = matrix {{2,2,16807662626294955/9007199254740992,13510798882111489/9007199254740992,0,1,-4503599627370497/9007199254740992,-7800463371553963/9007199254740992,-1,-1,-7800463371553963/9007199254740992,-4503599627370497/9007199254740992,0,1,13510798882111489/9007199254740992,16807662626294955/9007199254740992},{0,1,27021597764222975/18014398509481984,8403831313147477/4503599627370496,2,2,8403831313147477/4503599627370496,27021597764222975/18014398509481984,0,1,-9007199254740991/18014398509481984,-3900231685776981/4503599627370496,-1,-1,-3900231685776981/4503599627370496,-9007199254740991/18014398509481984}};
raysdesired11 = map(QQ^2, QQ^0, 0);
linealitydesired11 = map(QQ^2, QQ^0, 0);
verticesdesired11 = promote(verticesdesired11, QQ);
raysdesired11 = promote(raysdesired11, QQ);
linealitydesired11 = promote(linealitydesired11, QQ);
desired11 = convexHull(verticesdesired11,raysdesired11,linealitydesired11);
computed11 = P11+Q11;
assert(computed11 == desired11)

-- Test 5 1 - 4 1
-- Checking minkowskiSum
verticesP12 = matrix {{-1,1,1,0,-1},{-1,-1,1,1,0}};
raysP12 = map(QQ^2, QQ^0, 0);
linealityP12 = map(QQ^2, QQ^0, 0);
verticesP12 = promote(verticesP12, QQ);
raysP12 = promote(raysP12, QQ);
linealityP12 = promote(linealityP12, QQ);
P12 = convexHull(verticesP12,raysP12,linealityP12);
verticesQ12 = matrix {{-1/2,1/2,-1/2,1/2},{-1/2,-1/2,1/2,1/2}};
raysQ12 = map(QQ^2, QQ^0, 0);
linealityQ12 = map(QQ^2, QQ^0, 0);
verticesQ12 = promote(verticesQ12, QQ);
raysQ12 = promote(raysQ12, QQ);
linealityQ12 = promote(linealityQ12, QQ);
Q12 = convexHull(verticesQ12,raysQ12,linealityQ12);
verticesdesired12 = matrix {{-3/2,3/2,3/2,-1/2,-3/2},{-3/2,-3/2,3/2,3/2,1/2}};
raysdesired12 = map(QQ^2, QQ^0, 0);
linealitydesired12 = map(QQ^2, QQ^0, 0);
verticesdesired12 = promote(verticesdesired12, QQ);
raysdesired12 = promote(raysdesired12, QQ);
linealitydesired12 = promote(linealitydesired12, QQ);
desired12 = convexHull(verticesdesired12,raysdesired12,linealitydesired12);
computed12 = P12+Q12;
assert(computed12 == desired12)

-- Test 3 1 - 4 1
-- Checking minkowskiSum
verticesP13 = matrix {{1,2,3},{2,1,4}};
raysP13 = map(QQ^2, QQ^0, 0);
linealityP13 = map(QQ^2, QQ^0, 0);
verticesP13 = promote(verticesP13, QQ);
raysP13 = promote(raysP13, QQ);
linealityP13 = promote(linealityP13, QQ);
P13 = convexHull(verticesP13,raysP13,linealityP13);
verticesQ13 = matrix {{-1,1,-1,1},{-1,-1,1,1}};
raysQ13 = map(QQ^2, QQ^0, 0);
linealityQ13 = map(QQ^2, QQ^0, 0);
verticesQ13 = promote(verticesQ13, QQ);
raysQ13 = promote(raysQ13, QQ);
linealityQ13 = promote(linealityQ13, QQ);
Q13 = convexHull(verticesQ13,raysQ13,linealityQ13);
verticesdesired13 = matrix {{0,0,1,3,4,2,4},{1,3,0,0,3,5,5}};
raysdesired13 = map(QQ^2, QQ^0, 0);
linealitydesired13 = map(QQ^2, QQ^0, 0);
verticesdesired13 = promote(verticesdesired13, QQ);
raysdesired13 = promote(raysdesired13, QQ);
linealitydesired13 = promote(linealitydesired13, QQ);
desired13 = convexHull(verticesdesired13,raysdesired13,linealitydesired13);
computed13 = P13+Q13;
assert(computed13 == desired13)

-- Test 4 1 - 3 0
-- Checking minkowskiSum
verticesP14 = matrix {{-1,1,-1,1},{-1,-1,1,1}};
raysP14 = map(QQ^2, QQ^0, 0);
linealityP14 = map(QQ^2, QQ^0, 0);
verticesP14 = promote(verticesP14, QQ);
raysP14 = promote(raysP14, QQ);
linealityP14 = promote(linealityP14, QQ);
P14 = convexHull(verticesP14,raysP14,linealityP14);
verticesQ14 = matrix {{1},{1}};
raysQ14 = matrix {{-1,0},{0,-1}};
linealityQ14 = map(QQ^2, QQ^0, 0);
verticesQ14 = promote(verticesQ14, QQ);
raysQ14 = promote(raysQ14, QQ);
linealityQ14 = promote(linealityQ14, QQ);
Q14 = convexHull(verticesQ14,raysQ14,linealityQ14);
verticesdesired14 = matrix {{2},{2}};
raysdesired14 = matrix {{-1,0},{0,-1}};
linealitydesired14 = map(QQ^2, QQ^0, 0);
verticesdesired14 = promote(verticesdesired14, QQ);
raysdesired14 = promote(raysdesired14, QQ);
linealitydesired14 = promote(linealitydesired14, QQ);
desired14 = convexHull(verticesdesired14,raysdesired14,linealitydesired14);
computed14 = P14+Q14;
assert(computed14 == desired14)

-- Test 1 1 - 2 0
-- Checking minkowskiSum
verticesP15 = matrix {{1/2},{1/2}};
raysP15 = map(QQ^2, QQ^0, 0);
linealityP15 = map(QQ^2, QQ^0, 0);
verticesP15 = promote(verticesP15, QQ);
raysP15 = promote(raysP15, QQ);
linealityP15 = promote(linealityP15, QQ);
P15 = convexHull(verticesP15,raysP15,linealityP15);
verticesQ15 = matrix {{-1},{0}};
raysQ15 = matrix {{1},{0}};
linealityQ15 = matrix {{0},{1}};
verticesQ15 = promote(verticesQ15, QQ);
raysQ15 = promote(raysQ15, QQ);
linealityQ15 = promote(linealityQ15, QQ);
Q15 = convexHull(verticesQ15,raysQ15,linealityQ15);
verticesdesired15 = matrix {{-1/2},{1/2}};
raysdesired15 = matrix {{1},{0}};
linealitydesired15 = matrix {{0},{1}};
verticesdesired15 = promote(verticesdesired15, QQ);
raysdesired15 = promote(raysdesired15, QQ);
linealitydesired15 = promote(linealitydesired15, QQ);
desired15 = convexHull(verticesdesired15,raysdesired15,linealitydesired15);
computed15 = P15+Q15;
assert(computed15 == desired15)

-- Test 4 1 - 3 1
-- Checking minkowskiSum
verticesP16 = matrix {{1,-1,1,-1},{3/2,3/2,-1/2,-1/2}};
raysP16 = map(QQ^2, QQ^0, 0);
linealityP16 = map(QQ^2, QQ^0, 0);
verticesP16 = promote(verticesP16, QQ);
raysP16 = promote(raysP16, QQ);
linealityP16 = promote(linealityP16, QQ);
P16 = convexHull(verticesP16,raysP16,linealityP16);
verticesQ16 = matrix {{0,-2,-1},{0,0,-1}};
raysQ16 = map(QQ^2, QQ^0, 0);
linealityQ16 = map(QQ^2, QQ^0, 0);
verticesQ16 = promote(verticesQ16, QQ);
raysQ16 = promote(raysQ16, QQ);
linealityQ16 = promote(linealityQ16, QQ);
Q16 = convexHull(verticesQ16,raysQ16,linealityQ16);
verticesdesired16 = matrix {{1,-3,1,0,-3,-2},{3/2,3/2,-1/2,-3/2,-1/2,-3/2}};
raysdesired16 = map(QQ^2, QQ^0, 0);
linealitydesired16 = map(QQ^2, QQ^0, 0);
verticesdesired16 = promote(verticesdesired16, QQ);
raysdesired16 = promote(raysdesired16, QQ);
linealitydesired16 = promote(linealitydesired16, QQ);
desired16 = convexHull(verticesdesired16,raysdesired16,linealitydesired16);
computed16 = P16+Q16;
assert(computed16 == desired16)

-- Test 5 1 - 2 0
-- Checking minkowskiSum
verticesP17 = matrix {{2,0,0,1,2},{0,0,1,2,2}};
raysP17 = map(QQ^2, QQ^0, 0);
linealityP17 = map(QQ^2, QQ^0, 0);
verticesP17 = promote(verticesP17, QQ);
raysP17 = promote(raysP17, QQ);
linealityP17 = promote(linealityP17, QQ);
P17 = convexHull(verticesP17,raysP17,linealityP17);
verticesQ17 = matrix {{-1},{0}};
raysQ17 = matrix {{1},{0}};
linealityQ17 = matrix {{0},{1}};
verticesQ17 = promote(verticesQ17, QQ);
raysQ17 = promote(raysQ17, QQ);
linealityQ17 = promote(linealityQ17, QQ);
Q17 = convexHull(verticesQ17,raysQ17,linealityQ17);
verticesdesired17 = matrix {{-1},{0}};
raysdesired17 = matrix {{1},{0}};
linealitydesired17 = matrix {{0},{1}};
verticesdesired17 = promote(verticesdesired17, QQ);
raysdesired17 = promote(raysdesired17, QQ);
linealitydesired17 = promote(linealitydesired17, QQ);
desired17 = convexHull(verticesdesired17,raysdesired17,linealitydesired17);
computed17 = P17+Q17;
assert(computed17 == desired17)

-- Test 4 1 - 4 1
-- Checking minkowskiSum
verticesP18 = matrix {{1,-1,1,-1},{0,0,2,2}};
raysP18 = map(QQ^2, QQ^0, 0);
linealityP18 = map(QQ^2, QQ^0, 0);
verticesP18 = promote(verticesP18, QQ);
raysP18 = promote(raysP18, QQ);
linealityP18 = promote(linealityP18, QQ);
P18 = convexHull(verticesP18,raysP18,linealityP18);
verticesQ18 = matrix {{0,1/2,0,1/2},{0,0,1/2,1/2}};
raysQ18 = map(QQ^2, QQ^0, 0);
linealityQ18 = map(QQ^2, QQ^0, 0);
verticesQ18 = promote(verticesQ18, QQ);
raysQ18 = promote(raysQ18, QQ);
linealityQ18 = promote(linealityQ18, QQ);
Q18 = convexHull(verticesQ18,raysQ18,linealityQ18);
verticesdesired18 = matrix {{3/2,-1,3/2,-1},{0,0,5/2,5/2}};
raysdesired18 = map(QQ^2, QQ^0, 0);
linealitydesired18 = map(QQ^2, QQ^0, 0);
verticesdesired18 = promote(verticesdesired18, QQ);
raysdesired18 = promote(raysdesired18, QQ);
linealitydesired18 = promote(linealitydesired18, QQ);
desired18 = convexHull(verticesdesired18,raysdesired18,linealitydesired18);
computed18 = P18+Q18;
assert(computed18 == desired18)

-- Test 5 1 - 3 1
-- Checking minkowskiSum
verticesP19 = matrix {{0,2,2,1,0},{0,0,2,2,1}};
raysP19 = map(QQ^2, QQ^0, 0);
linealityP19 = map(QQ^2, QQ^0, 0);
verticesP19 = promote(verticesP19, QQ);
raysP19 = promote(raysP19, QQ);
linealityP19 = promote(linealityP19, QQ);
P19 = convexHull(verticesP19,raysP19,linealityP19);
verticesQ19 = matrix {{0,5/2,0},{0,0,5/2}};
raysQ19 = map(QQ^2, QQ^0, 0);
linealityQ19 = map(QQ^2, QQ^0, 0);
verticesQ19 = promote(verticesQ19, QQ);
raysQ19 = promote(raysQ19, QQ);
linealityQ19 = promote(linealityQ19, QQ);
Q19 = convexHull(verticesQ19,raysQ19,linealityQ19);
verticesdesired19 = matrix {{0,9/2,9/2,2,1,0},{0,0,2,9/2,9/2,7/2}};
raysdesired19 = map(QQ^2, QQ^0, 0);
linealitydesired19 = map(QQ^2, QQ^0, 0);
verticesdesired19 = promote(verticesdesired19, QQ);
raysdesired19 = promote(raysdesired19, QQ);
linealitydesired19 = promote(linealitydesired19, QQ);
desired19 = convexHull(verticesdesired19,raysdesired19,linealitydesired19);
computed19 = P19+Q19;
assert(computed19 == desired19)

///

TEST ///
-- Test 10 1 - 4 1
-- Checking minkowskiSum
verticesP20 = matrix {{-1,1,-1,1,-1,1,-1,1,-3/2,3/2},{-1,-1,1,1,-1,-1,1,1,0,0},{-1,-1,-1,-1,1,1,1,1,0,-3/2}};
raysP20 = map(QQ^3, QQ^0, 0);
linealityP20 = map(QQ^3, QQ^0, 0);
verticesP20 = promote(verticesP20, QQ);
raysP20 = promote(raysP20, QQ);
linealityP20 = promote(linealityP20, QQ);
P20 = convexHull(verticesP20,raysP20,linealityP20);
verticesQ20 = matrix {{0,13/4,0,0},{0,0,13/4,0},{0,0,0,13/4}};
raysQ20 = map(QQ^3, QQ^0, 0);
linealityQ20 = map(QQ^3, QQ^0, 0);
verticesQ20 = promote(verticesQ20, QQ);
raysQ20 = promote(raysQ20, QQ);
linealityQ20 = promote(linealityQ20, QQ);
Q20 = convexHull(verticesQ20,raysQ20,linealityQ20);
verticesdesired20 = matrix {{-1,17/4,-1,17/4,1,-1,17/4,1,-1,-1,17/4,1,1,-3/2,-3/2,-3/2,3/2,19/4,3/2},{-1,-1,17/4,1,17/4,-1,-1,-1,17/4,1,1,17/4,1,0,13/4,0,0,0,13/4},{-1,-1,-1,-1,-1,17/4,1,17/4,1,17/4,1,1,17/4,0,0,13/4,-3/2,-3/2,-3/2}};
raysdesired20 = map(QQ^3, QQ^0, 0);
linealitydesired20 = map(QQ^3, QQ^0, 0);
verticesdesired20 = promote(verticesdesired20, QQ);
raysdesired20 = promote(raysdesired20, QQ);
linealitydesired20 = promote(linealitydesired20, QQ);
desired20 = convexHull(verticesdesired20,raysdesired20,linealitydesired20);
computed20 = P20+Q20;
assert(computed20 == desired20)

-- Test 8 1 - 6 1
-- Checking minkowskiSum
verticesP21 = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP21 = map(QQ^3, QQ^0, 0);
linealityP21 = map(QQ^3, QQ^0, 0);
verticesP21 = promote(verticesP21, QQ);
raysP21 = promote(raysP21, QQ);
linealityP21 = promote(linealityP21, QQ);
P21 = convexHull(verticesP21,raysP21,linealityP21);
verticesQ21 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
raysQ21 = map(QQ^3, QQ^0, 0);
linealityQ21 = map(QQ^3, QQ^0, 0);
verticesQ21 = promote(verticesQ21, QQ);
raysQ21 = promote(raysQ21, QQ);
linealityQ21 = promote(linealityQ21, QQ);
Q21 = convexHull(verticesQ21,raysQ21,linealityQ21);
verticesdesired21 = matrix {{-2,-1,-1,2,1,1,-2,-1,-1,2,1,1,-2,-1,-1,2,1,1,-2,-1,-1,2,1,1},{-1,-2,-1,-1,-2,-1,1,2,1,1,2,1,-1,-2,-1,-1,-2,-1,1,2,1,1,2,1},{-1,-1,-2,-1,-1,-2,-1,-1,-2,-1,-1,-2,1,1,2,1,1,2,1,1,2,1,1,2}};
raysdesired21 = map(QQ^3, QQ^0, 0);
linealitydesired21 = map(QQ^3, QQ^0, 0);
verticesdesired21 = promote(verticesdesired21, QQ);
raysdesired21 = promote(raysdesired21, QQ);
linealitydesired21 = promote(linealitydesired21, QQ);
desired21 = convexHull(verticesdesired21,raysdesired21,linealitydesired21);
computed21 = P21+Q21;
assert(computed21 == desired21)

-- Test 8 1 - 44 1
-- Checking minkowskiSum
verticesP22 = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP22 = map(QQ^3, QQ^0, 0);
linealityP22 = map(QQ^3, QQ^0, 0);
verticesP22 = promote(verticesP22, QQ);
raysP22 = promote(raysP22, QQ);
linealityP22 = promote(linealityP22, QQ);
P22 = convexHull(verticesP22,raysP22,linealityP22);
verticesQ22 = matrix {{10,10,9,8,7,6,5,4,3,2,1,0,-1,-2,-3,-4,-5,-6,-7,-8,-9,0,1,-1,2,-2,3,-3,4,-4,5,-5,6,-6,7,-7,8,-8,9,-9,-10,-10,0,0},{1,1,81/100,16/25,49/100,9/25,1/4,4/25,9/100,1/25,1/100,0,1/100,1/25,9/100,4/25,1/4,9/25,49/100,16/25,81/100,0,1/100,1/100,1/25,1/25,9/100,9/100,4/25,4/25,1/4,1/4,9/25,9/25,49/100,49/100,16/25,16/25,81/100,81/100,1,1,30,30},{-10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,10,-10,10,-10}};
raysQ22 = map(QQ^3, QQ^0, 0);
linealityQ22 = map(QQ^3, QQ^0, 0);
verticesQ22 = promote(verticesQ22, QQ);
raysQ22 = promote(raysQ22, QQ);
linealityQ22 = promote(linealityQ22, QQ);
Q22 = convexHull(verticesQ22,raysQ22,linealityQ22);
verticesdesired22 = matrix {{-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,11,1,2,3,4,5,6,7,8,9,10,-11,-1,11,1,-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,11,10,9,8,7,6,5,4,3,2,1,-11,-1,11,1},{-1,-99/100,-24/25,-91/100,-21/25,-3/4,-16/25,-51/100,-9/25,-19/100,0,0,-1,-99/100,-24/25,-91/100,-21/25,-3/4,-16/25,-51/100,-9/25,-19/100,2,31,2,31,-1,-99/100,-24/25,-91/100,-21/25,-3/4,-16/25,-51/100,-9/25,-19/100,0,0,-19/100,-9/25,-51/100,-16/25,-3/4,-21/25,-91/100,-24/25,-99/100,-1,2,31,2,31},{-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}};
raysdesired22 = map(QQ^3, QQ^0, 0);
linealitydesired22 = map(QQ^3, QQ^0, 0);
verticesdesired22 = promote(verticesdesired22, QQ);
raysdesired22 = promote(raysdesired22, QQ);
linealitydesired22 = promote(linealitydesired22, QQ);
desired22 = convexHull(verticesdesired22,raysdesired22,linealitydesired22);
computed22 = P22+Q22;
assert(computed22 == desired22)

-- Test 6 1 - 8 1
-- Checking minkowskiSum
verticesP23 = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
raysP23 = map(QQ^3, QQ^0, 0);
linealityP23 = map(QQ^3, QQ^0, 0);
verticesP23 = promote(verticesP23, QQ);
raysP23 = promote(raysP23, QQ);
linealityP23 = promote(linealityP23, QQ);
P23 = convexHull(verticesP23,raysP23,linealityP23);
verticesQ23 = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysQ23 = map(QQ^3, QQ^0, 0);
linealityQ23 = map(QQ^3, QQ^0, 0);
verticesQ23 = promote(verticesQ23, QQ);
raysQ23 = promote(raysQ23, QQ);
linealityQ23 = promote(linealityQ23, QQ);
Q23 = convexHull(verticesQ23,raysQ23,linealityQ23);
verticesdesired23 = matrix {{2,2,2,2,-2,-2,-2,-2,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,1,-1,1,-1,1,-1,1,2,2,2,2,-2,-2,-2,-2,-1,-1,1,1,-1,-1,1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,2,2,2,2,-2,-2,-2,-2}};
raysdesired23 = map(QQ^3, QQ^0, 0);
linealitydesired23 = map(QQ^3, QQ^0, 0);
verticesdesired23 = promote(verticesdesired23, QQ);
raysdesired23 = promote(raysdesired23, QQ);
linealitydesired23 = promote(linealitydesired23, QQ);
desired23 = convexHull(verticesdesired23,raysdesired23,linealitydesired23);
computed23 = P23+Q23;
assert(computed23 == desired23)

-- Test 3 1 - 5 1
-- Checking minkowskiSum
verticesP24 = matrix {{0,0,1},{0,1,0},{1,0,0}};
raysP24 = map(QQ^3, QQ^0, 0);
linealityP24 = map(QQ^3, QQ^0, 0);
verticesP24 = promote(verticesP24, QQ);
raysP24 = promote(raysP24, QQ);
linealityP24 = promote(linealityP24, QQ);
P24 = convexHull(verticesP24,raysP24,linealityP24);
verticesQ24 = matrix {{2/3,-1/3,-1/3,2/3,-2/3},{-1/3,2/3,-1/3,2/3,-2/3},{-1/3,-1/3,2/3,2/3,-2/3}};
raysQ24 = map(QQ^3, QQ^0, 0);
linealityQ24 = map(QQ^3, QQ^0, 0);
verticesQ24 = promote(verticesQ24, QQ);
raysQ24 = promote(raysQ24, QQ);
linealityQ24 = promote(linealityQ24, QQ);
Q24 = convexHull(verticesQ24,raysQ24,linealityQ24);
verticesdesired24 = matrix {{-1/3,2/3,-2/3,-1/3,2/3,-2/3,5/3,5/3,1/3},{-1/3,2/3,-2/3,5/3,5/3,1/3,-1/3,2/3,-2/3},{5/3,5/3,1/3,-1/3,2/3,-2/3,-1/3,2/3,-2/3}};
raysdesired24 = map(QQ^3, QQ^0, 0);
linealitydesired24 = map(QQ^3, QQ^0, 0);
verticesdesired24 = promote(verticesdesired24, QQ);
raysdesired24 = promote(raysdesired24, QQ);
linealitydesired24 = promote(linealitydesired24, QQ);
desired24 = convexHull(verticesdesired24,raysdesired24,linealitydesired24);
computed24 = P24+Q24;
assert(computed24 == desired24)

-- Test 20 1 - 5 1
-- Checking minkowskiSum
verticesP25 = matrix {{0,46902659965/127621645493,55212343031/127621645493,52142788415/127621645493,28082239676/127621645493,1,56089631591/127621645493,0,53313461839/127621645493,35409120842/127621645493,56352088147/127621645493,37952788301/127621645493,41907724853/127621645493,51390022393/127621645493,49169522745/127621645493,66663198749/127621645493,50856828539/127621645493,31742509442/127621645493,505114043/1615463867,23485010074/127621645493},{0,51528106929/127621645493,47292349665/127621645493,45660645400/127621645493,76730540963/127621645493,0,50593622751/127621645493,1,59985094384/127621645493,71630997559/127621645493,42397780428/127621645493,57673994582/127621645493,66158074841/127621645493,37079987522/127621645493,55538907830/127621645493,39558139125/127621645493,50005002456/127621645493,80425270648/127621645493,800127896/1615463867,59821233712/127621645493},{0,15659519127/127621645493,22904411535/127621645493,19461046432/127621645493,5553989752/127621645493,96901221234/127621645493,24874171773/127621645493,0,25146776292/127621645493,10666020352/127621645493,23070938488/127621645493,9285296728/127621645493,15134164963/127621645493,17265023116/127621645493,18870911160/127621645493,33325625211/127621645493,19155908376/127621645493,10451043184/127621645493,12119300122/127621645493,0}};
raysP25 = map(QQ^3, QQ^0, 0);
linealityP25 = map(QQ^3, QQ^0, 0);
verticesP25 = promote(verticesP25, QQ);
raysP25 = promote(raysP25, QQ);
linealityP25 = promote(linealityP25, QQ);
P25 = convexHull(verticesP25,raysP25,linealityP25);
verticesQ25 = matrix {{0,1,0,1,0},{1,0,0,1,0},{0,0,1,1,2}};
raysQ25 = map(QQ^3, QQ^0, 0);
linealityQ25 = map(QQ^3, QQ^0, 0);
verticesQ25 = promote(verticesQ25, QQ);
raysQ25 = promote(raysQ25, QQ);
linealityQ25 = promote(linealityQ25, QQ);
Q25 = convexHull(verticesQ25,raysQ25,linealityQ25);
verticesdesired25 = matrix {{0,1,0,0,174524305458/127621645493,182833988524/127621645493,179764433908/127621645493,155703885169/127621645493,2,2,1,183711277084/127621645493,0,1,1,0,180935107332/127621645493,180935107332/127621645493,163030766335/127621645493,183973733640/127621645493,165574433794/127621645493,169529370346/127621645493,179011667886/127621645493,176791168238/127621645493,194284844242/127621645493,178478474032/127621645493,159364154935/127621645493,2120577910/1615463867,151106655567/127621645493},{1,0,0,0,51528106929/127621645493,47292349665/127621645493,45660645400/127621645493,76730540963/127621645493,0,1,0,50593622751/127621645493,2,1,2,1,59985094384/127621645493,187606739877/127621645493,71630997559/127621645493,42397780428/127621645493,57673994582/127621645493,66158074841/127621645493,37079987522/127621645493,55538907830/127621645493,39558139125/127621645493,50005002456/127621645493,80425270648/127621645493,800127896/1615463867,59821233712/127621645493},{0,0,1,2,15659519127/127621645493,22904411535/127621645493,19461046432/127621645493,5553989752/127621645493,96901221234/127621645493,224522866727/127621645493,352144512220/127621645493,24874171773/127621645493,0,0,1,2,25146776292/127621645493,152768421785/127621645493,10666020352/127621645493,23070938488/127621645493,9285296728/127621645493,15134164963/127621645493,17265023116/127621645493,18870911160/127621645493,33325625211/127621645493,19155908376/127621645493,10451043184/127621645493,12119300122/127621645493,0}};
raysdesired25 = map(QQ^3, QQ^0, 0);
linealitydesired25 = map(QQ^3, QQ^0, 0);
verticesdesired25 = promote(verticesdesired25, QQ);
raysdesired25 = promote(raysdesired25, QQ);
linealitydesired25 = promote(linealitydesired25, QQ);
desired25 = convexHull(verticesdesired25,raysdesired25,linealitydesired25);
computed25 = P25+Q25;
assert(computed25 == desired25)

-- Test 8 1 - 8 1
-- Checking minkowskiSum
verticesP26 = matrix {{1,1,1,1,-1,-1,-1,-1},{-1,-1,1,1,1,1,-1,-1},{-1,1,1,-1,1,-1,1,-1}};
raysP26 = map(QQ^3, QQ^0, 0);
linealityP26 = map(QQ^3, QQ^0, 0);
verticesP26 = promote(verticesP26, QQ);
raysP26 = promote(raysP26, QQ);
linealityP26 = promote(linealityP26, QQ);
P26 = convexHull(verticesP26,raysP26,linealityP26);
verticesQ26 = matrix {{10/27,0,1/3,10/27,2/7,0,0,2/7},{5/9,0,0,1/3,5/7,1,0,3/7},{0,0,1/2,2/9,0,0,1,2/7}};
raysQ26 = map(QQ^3, QQ^0, 0);
linealityQ26 = map(QQ^3, QQ^0, 0);
verticesQ26 = promote(verticesQ26, QQ);
raysQ26 = promote(raysQ26, QQ);
linealityQ26 = promote(linealityQ26, QQ);
Q26 = convexHull(verticesQ26,raysQ26,linealityQ26);
verticesdesired26 = matrix {{37/27,1,4/3,37/27,4/3,37/27,1,37/27,4/3,37/27,9/7,1,1,9/7,37/27,9/7,1,-1,-1,-1,-1,-1},{-4/9,-1,-1,-2/3,-1,-2/3,-1,14/9,1,4/3,12/7,2,1,10/7,14/9,12/7,2,2,1,2,-1,-1},{-1,-1,-1/2,-7/9,3/2,11/9,2,1,3/2,11/9,1,1,2,9/7,-1,-1,-1,1,2,-1,2,-1}};
raysdesired26 = map(QQ^3, QQ^0, 0);
linealitydesired26 = map(QQ^3, QQ^0, 0);
verticesdesired26 = promote(verticesdesired26, QQ);
raysdesired26 = promote(raysdesired26, QQ);
linealitydesired26 = promote(linealitydesired26, QQ);
desired26 = convexHull(verticesdesired26,raysdesired26,linealitydesired26);
computed26 = P26+Q26;
assert(computed26 == desired26)

-- Test 8 1 - 8 1
-- Checking minkowskiSum
verticesP27 = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP27 = map(QQ^3, QQ^0, 0);
linealityP27 = map(QQ^3, QQ^0, 0);
verticesP27 = promote(verticesP27, QQ);
raysP27 = promote(raysP27, QQ);
linealityP27 = promote(linealityP27, QQ);
P27 = convexHull(verticesP27,raysP27,linealityP27);
verticesQ27 = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysQ27 = map(QQ^3, QQ^0, 0);
linealityQ27 = map(QQ^3, QQ^0, 0);
verticesQ27 = promote(verticesQ27, QQ);
raysQ27 = promote(raysQ27, QQ);
linealityQ27 = promote(linealityQ27, QQ);
Q27 = convexHull(verticesQ27,raysQ27,linealityQ27);
verticesdesired27 = matrix {{-2,2,-2,2,-2,2,-2,2},{-2,-2,2,2,-2,-2,2,2},{-2,-2,-2,-2,2,2,2,2}};
raysdesired27 = map(QQ^3, QQ^0, 0);
linealitydesired27 = map(QQ^3, QQ^0, 0);
verticesdesired27 = promote(verticesdesired27, QQ);
raysdesired27 = promote(raysdesired27, QQ);
linealitydesired27 = promote(linealitydesired27, QQ);
desired27 = convexHull(verticesdesired27,raysdesired27,linealitydesired27);
computed27 = P27+Q27;
assert(computed27 == desired27)

///
