-- Test 2 1 - 2 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1,-1}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{-1,1}};
raysQ = map(QQ^1, QQ^0, 0);
linealityQ = map(QQ^1, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{2,-2}};
raysdesired = map(QQ^1, QQ^0, 0);
linealitydesired = map(QQ^1, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 2 1 - 1 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1}};
raysQ = map(QQ^1, QQ^0, 0);
linealityQ = map(QQ^1, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{0,2}};
raysdesired = map(QQ^1, QQ^0, 0);
linealitydesired = map(QQ^1, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 0 1 - 2 1
-- Checking minkowskiSum
TEST ///
verticesP = map(QQ^1, QQ^0, 0);
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,2}};
raysQ = map(QQ^1, QQ^0, 0);
linealityQ = map(QQ^1, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = map(QQ^1, QQ^0, 0);
raysdesired = map(QQ^1, QQ^0, 0);
linealitydesired = map(QQ^1, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 2 0 - 2 0
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1}};
raysP = matrix {{1}};
linealityP = map(QQ^1, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1}};
raysQ = matrix {{1}};
linealityQ = map(QQ^1, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{2}};
raysdesired = matrix {{1}};
linealitydesired = map(QQ^1, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 2 1 - 0 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0,1}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = map(QQ^1, QQ^0, 0);
raysQ = map(QQ^1, QQ^0, 0);
linealityQ = map(QQ^1, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = map(QQ^1, QQ^0, 0);
raysdesired = map(QQ^1, QQ^0, 0);
linealitydesired = map(QQ^1, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 2 0 - 2 0
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1}};
raysP = matrix {{1}};
linealityP = map(QQ^1, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0}};
raysQ = matrix {{1}};
linealityQ = map(QQ^1, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{1}};
raysdesired = matrix {{1}};
linealitydesired = map(QQ^1, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 2 1 - 2 0
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1}};
raysQ = matrix {{1}};
linealityQ = map(QQ^1, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{0}};
raysdesired = matrix {{1}};
linealitydesired = map(QQ^1, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 2 0 - 2 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0}};
raysP = matrix {{1}};
linealityP = map(QQ^1, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,1}};
raysQ = map(QQ^1, QQ^0, 0);
linealityQ = map(QQ^1, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{0}};
raysdesired = matrix {{1}};
linealitydesired = map(QQ^1, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 2 1 - 0 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0,1}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = map(QQ^1, QQ^0, 0);
raysQ = map(QQ^1, QQ^0, 0);
linealityQ = map(QQ^1, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = map(QQ^1, QQ^0, 0);
raysdesired = map(QQ^1, QQ^0, 0);
linealitydesired = map(QQ^1, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 2 1 - 1 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0}};
raysQ = map(QQ^1, QQ^0, 0);
linealityQ = map(QQ^1, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-1,1}};
raysdesired = map(QQ^1, QQ^0, 0);
linealitydesired = map(QQ^1, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 3 0 - 4 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{2},{2}};
raysP = matrix {{1,1},{-1,1}};
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1,-1,1,-1},{3/2,3/2,-1/2,-1/2}};
raysQ = map(QQ^2, QQ^0, 0);
linealityQ = map(QQ^2, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{1,1},{7/2,3/2}};
raysdesired = matrix {{1,1},{-1,1}};
linealitydesired = map(QQ^2, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 12 1 - 4 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1,7800463371553963/9007199254740992,4503599627370497/9007199254740992,0,-4503599627370497/9007199254740992,-7800463371553963/9007199254740992,-1,-7800463371553963/9007199254740992,-4503599627370497/9007199254740992,0,4503599627370497/9007199254740992,7800463371553963/9007199254740992},{0,9007199254740991/18014398509481984,3900231685776981/4503599627370496,1,3900231685776981/4503599627370496,9007199254740991/18014398509481984,0,-9007199254740991/18014398509481984,-3900231685776981/4503599627370496,-1,-3900231685776981/4503599627370496,-9007199254740991/18014398509481984}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,0,1,1},{0,1,0,1}};
raysQ = map(QQ^2, QQ^0, 0);
linealityQ = map(QQ^2, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{2,2,16807662626294955/9007199254740992,13510798882111489/9007199254740992,0,1,-4503599627370497/9007199254740992,-7800463371553963/9007199254740992,-1,-1,-7800463371553963/9007199254740992,-4503599627370497/9007199254740992,0,1,13510798882111489/9007199254740992,16807662626294955/9007199254740992},{0,1,27021597764222975/18014398509481984,8403831313147477/4503599627370496,2,2,8403831313147477/4503599627370496,27021597764222975/18014398509481984,0,1,-9007199254740991/18014398509481984,-3900231685776981/4503599627370496,-1,-1,-3900231685776981/4503599627370496,-9007199254740991/18014398509481984}};
raysdesired = map(QQ^2, QQ^0, 0);
linealitydesired = map(QQ^2, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 5 1 - 4 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1,1,0,-1},{-1,-1,1,1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{-1/2,1/2,-1/2,1/2},{-1/2,-1/2,1/2,1/2}};
raysQ = map(QQ^2, QQ^0, 0);
linealityQ = map(QQ^2, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-3/2,3/2,3/2,-1/2,-3/2},{-3/2,-3/2,3/2,3/2,1/2}};
raysdesired = map(QQ^2, QQ^0, 0);
linealitydesired = map(QQ^2, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 3 1 - 4 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1,2,3},{2,1,4}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{-1,1,-1,1},{-1,-1,1,1}};
raysQ = map(QQ^2, QQ^0, 0);
linealityQ = map(QQ^2, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{0,0,1,3,4,2,4},{1,3,0,0,3,5,5}};
raysdesired = map(QQ^2, QQ^0, 0);
linealitydesired = map(QQ^2, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 4 1 - 3 0
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1,-1,1},{-1,-1,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1},{1}};
raysQ = matrix {{-1,0},{0,-1}};
linealityQ = map(QQ^2, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{2},{2}};
raysdesired = matrix {{-1,0},{0,-1}};
linealitydesired = map(QQ^2, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 1 1 - 2 0
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1/2},{1/2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{-1},{0}};
raysQ = matrix {{1},{0}};
linealityQ = matrix {{0},{1}};
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-1/2},{1/2}};
raysdesired = matrix {{1},{0}};
linealitydesired = matrix {{0},{1}};
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 4 1 - 3 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1,-1,1,-1},{3/2,3/2,-1/2,-1/2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,-2,-1},{0,0,-1}};
raysQ = map(QQ^2, QQ^0, 0);
linealityQ = map(QQ^2, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{1,-3,1,0,-3,-2},{3/2,3/2,-1/2,-3/2,-1/2,-3/2}};
raysdesired = map(QQ^2, QQ^0, 0);
linealitydesired = map(QQ^2, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 5 1 - 2 0
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{2,0,0,1,2},{0,0,1,2,2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{-1},{0}};
raysQ = matrix {{1},{0}};
linealityQ = matrix {{0},{1}};
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-1},{0}};
raysdesired = matrix {{1},{0}};
linealitydesired = matrix {{0},{1}};
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 4 1 - 4 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1,-1,1,-1},{0,0,2,2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,1/2,0,1/2},{0,0,1/2,1/2}};
raysQ = map(QQ^2, QQ^0, 0);
linealityQ = map(QQ^2, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{3/2,-1,3/2,-1},{0,0,5/2,5/2}};
raysdesired = map(QQ^2, QQ^0, 0);
linealitydesired = map(QQ^2, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 5 1 - 3 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0,2,2,1,0},{0,0,2,2,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,5/2,0},{0,0,5/2}};
raysQ = map(QQ^2, QQ^0, 0);
linealityQ = map(QQ^2, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{0,9/2,9/2,2,1,0},{0,0,2,9/2,9/2,7/2}};
raysdesired = map(QQ^2, QQ^0, 0);
linealitydesired = map(QQ^2, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 10 1 - 4 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-3/2,3/2},{-1,-1,1,1,-1,-1,1,1,0,0},{-1,-1,-1,-1,1,1,1,1,0,-3/2}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,13/4,0,0},{0,0,13/4,0},{0,0,0,13/4}};
raysQ = map(QQ^3, QQ^0, 0);
linealityQ = map(QQ^3, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-1,17/4,-1,17/4,1,-1,17/4,1,-1,-1,17/4,1,1,-3/2,-3/2,-3/2,3/2,19/4,3/2},{-1,-1,17/4,1,17/4,-1,-1,-1,17/4,1,1,17/4,1,0,13/4,0,0,0,13/4},{-1,-1,-1,-1,-1,17/4,1,17/4,1,17/4,1,1,17/4,0,0,13/4,-3/2,-3/2,-3/2}};
raysdesired = map(QQ^3, QQ^0, 0);
linealitydesired = map(QQ^3, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 8 1 - 6 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
raysQ = map(QQ^3, QQ^0, 0);
linealityQ = map(QQ^3, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-2,-1,-1,2,1,1,-2,-1,-1,2,1,1,-2,-1,-1,2,1,1,-2,-1,-1,2,1,1},{-1,-2,-1,-1,-2,-1,1,2,1,1,2,1,-1,-2,-1,-1,-2,-1,1,2,1,1,2,1},{-1,-1,-2,-1,-1,-2,-1,-1,-2,-1,-1,-2,1,1,2,1,1,2,1,1,2,1,1,2}};
raysdesired = map(QQ^3, QQ^0, 0);
linealitydesired = map(QQ^3, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 8 1 - 44 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{10,10,9,8,7,6,5,4,3,2,1,0,-1,-2,-3,-4,-5,-6,-7,-8,-9,0,1,-1,2,-2,3,-3,4,-4,5,-5,6,-6,7,-7,8,-8,9,-9,-10,-10,0,0},{1,1,81/100,16/25,49/100,9/25,1/4,4/25,9/100,1/25,1/100,0,1/100,1/25,9/100,4/25,1/4,9/25,49/100,16/25,81/100,0,1/100,1/100,1/25,1/25,9/100,9/100,4/25,4/25,1/4,1/4,9/25,9/25,49/100,49/100,16/25,16/25,81/100,81/100,1,1,30,30},{-10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,-10,10,-10,10,-10}};
raysQ = map(QQ^3, QQ^0, 0);
linealityQ = map(QQ^3, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,11,1,2,3,4,5,6,7,8,9,10,-11,-1,11,1,-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,11,10,9,8,7,6,5,4,3,2,1,-11,-1,11,1},{-1,-99/100,-24/25,-91/100,-21/25,-3/4,-16/25,-51/100,-9/25,-19/100,0,0,-1,-99/100,-24/25,-91/100,-21/25,-3/4,-16/25,-51/100,-9/25,-19/100,2,31,2,31,-1,-99/100,-24/25,-91/100,-21/25,-3/4,-16/25,-51/100,-9/25,-19/100,0,0,-19/100,-9/25,-51/100,-16/25,-3/4,-21/25,-91/100,-24/25,-99/100,-1,2,31,2,31},{-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,-11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}};
raysdesired = map(QQ^3, QQ^0, 0);
linealitydesired = map(QQ^3, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 6 1 - 8 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysQ = map(QQ^3, QQ^0, 0);
linealityQ = map(QQ^3, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{2,2,2,2,-2,-2,-2,-2,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,1,-1,1,-1,1,-1,1,2,2,2,2,-2,-2,-2,-2,-1,-1,1,1,-1,-1,1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,2,2,2,2,-2,-2,-2,-2}};
raysdesired = map(QQ^3, QQ^0, 0);
linealitydesired = map(QQ^3, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 3 1 - 5 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0,0,1},{0,1,0},{1,0,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{2/3,-1/3,-1/3,2/3,-2/3},{-1/3,2/3,-1/3,2/3,-2/3},{-1/3,-1/3,2/3,2/3,-2/3}};
raysQ = map(QQ^3, QQ^0, 0);
linealityQ = map(QQ^3, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-1/3,2/3,-2/3,-1/3,2/3,-2/3,5/3,5/3,1/3},{-1/3,2/3,-2/3,5/3,5/3,1/3,-1/3,2/3,-2/3},{5/3,5/3,1/3,-1/3,2/3,-2/3,-1/3,2/3,-2/3}};
raysdesired = map(QQ^3, QQ^0, 0);
linealitydesired = map(QQ^3, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 20 1 - 5 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0,46902659965/127621645493,55212343031/127621645493,52142788415/127621645493,28082239676/127621645493,1,56089631591/127621645493,0,53313461839/127621645493,35409120842/127621645493,56352088147/127621645493,37952788301/127621645493,41907724853/127621645493,51390022393/127621645493,49169522745/127621645493,66663198749/127621645493,50856828539/127621645493,31742509442/127621645493,505114043/1615463867,23485010074/127621645493},{0,51528106929/127621645493,47292349665/127621645493,45660645400/127621645493,76730540963/127621645493,0,50593622751/127621645493,1,59985094384/127621645493,71630997559/127621645493,42397780428/127621645493,57673994582/127621645493,66158074841/127621645493,37079987522/127621645493,55538907830/127621645493,39558139125/127621645493,50005002456/127621645493,80425270648/127621645493,800127896/1615463867,59821233712/127621645493},{0,15659519127/127621645493,22904411535/127621645493,19461046432/127621645493,5553989752/127621645493,96901221234/127621645493,24874171773/127621645493,0,25146776292/127621645493,10666020352/127621645493,23070938488/127621645493,9285296728/127621645493,15134164963/127621645493,17265023116/127621645493,18870911160/127621645493,33325625211/127621645493,19155908376/127621645493,10451043184/127621645493,12119300122/127621645493,0}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,1,0,1,0},{1,0,0,1,0},{0,0,1,1,2}};
raysQ = map(QQ^3, QQ^0, 0);
linealityQ = map(QQ^3, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{0,1,0,0,174524305458/127621645493,182833988524/127621645493,179764433908/127621645493,155703885169/127621645493,2,2,1,183711277084/127621645493,0,1,1,0,180935107332/127621645493,180935107332/127621645493,163030766335/127621645493,183973733640/127621645493,165574433794/127621645493,169529370346/127621645493,179011667886/127621645493,176791168238/127621645493,194284844242/127621645493,178478474032/127621645493,159364154935/127621645493,2120577910/1615463867,151106655567/127621645493},{1,0,0,0,51528106929/127621645493,47292349665/127621645493,45660645400/127621645493,76730540963/127621645493,0,1,0,50593622751/127621645493,2,1,2,1,59985094384/127621645493,187606739877/127621645493,71630997559/127621645493,42397780428/127621645493,57673994582/127621645493,66158074841/127621645493,37079987522/127621645493,55538907830/127621645493,39558139125/127621645493,50005002456/127621645493,80425270648/127621645493,800127896/1615463867,59821233712/127621645493},{0,0,1,2,15659519127/127621645493,22904411535/127621645493,19461046432/127621645493,5553989752/127621645493,96901221234/127621645493,224522866727/127621645493,352144512220/127621645493,24874171773/127621645493,0,0,1,2,25146776292/127621645493,152768421785/127621645493,10666020352/127621645493,23070938488/127621645493,9285296728/127621645493,15134164963/127621645493,17265023116/127621645493,18870911160/127621645493,33325625211/127621645493,19155908376/127621645493,10451043184/127621645493,12119300122/127621645493,0}};
raysdesired = map(QQ^3, QQ^0, 0);
linealitydesired = map(QQ^3, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 8 1 - 8 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1,1,1,1,-1,-1,-1,-1},{-1,-1,1,1,1,1,-1,-1},{-1,1,1,-1,1,-1,1,-1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{10/27,0,1/3,10/27,2/7,0,0,2/7},{5/9,0,0,1/3,5/7,1,0,3/7},{0,0,1/2,2/9,0,0,1,2/7}};
raysQ = map(QQ^3, QQ^0, 0);
linealityQ = map(QQ^3, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{37/27,1,4/3,37/27,4/3,37/27,1,37/27,4/3,37/27,9/7,1,1,9/7,37/27,9/7,1,-1,-1,-1,-1,-1},{-4/9,-1,-1,-2/3,-1,-2/3,-1,14/9,1,4/3,12/7,2,1,10/7,14/9,12/7,2,2,1,2,-1,-1},{-1,-1,-1/2,-7/9,3/2,11/9,2,1,3/2,11/9,1,1,2,9/7,-1,-1,-1,1,2,-1,2,-1}};
raysdesired = map(QQ^3, QQ^0, 0);
linealitydesired = map(QQ^3, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 8 1 - 8 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysQ = map(QQ^3, QQ^0, 0);
linealityQ = map(QQ^3, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-2,2,-2,2,-2,2,-2,2},{-2,-2,2,2,-2,-2,2,2},{-2,-2,-2,-2,2,2,2,2}};
raysdesired = map(QQ^3, QQ^0, 0);
linealitydesired = map(QQ^3, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 34 0 - 4 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-593934448380038187629003561336244223315615416320/581295835759584558808312033228316509435714702879,-779973149963902010794106307206482951439877406720/1148181310580490847889443245285958332842937431117,-834911336348711971508916753479457864880196943872/1033279699092212080961238485377359834950104619125,-451351638156153469675449537298017167288402182144/661074666395213201954148779083788761647422101087,-11875211446039377815154684977628597209263677374464/28146830865538587708960983240939498061618358464803,-1066404108435779557262026998475926048840047656960/2663035629934843887354436756168725949136781115679,-286667771360306766448084578481195066846882037760/1070022436834922326869526480917927343294110940591,-101857867966402892552331775300708148248320147456/313078221326677069773728886384385920072137617303,-247647508815704899421824604945096989996604194816/4715350869261984712862393959738846535578964133343,-231354719769776038518069933565989679675543126016/448263694500983649316778464851002479369071878087,1173995576268867024101222385236247095587712794624/1063363920455119185533336766028459859555986812289,10919588709302286410864254853031789456928047890432/201086830318867823857028347393140421252269033251,7737382246988530702401719335728566856035340386304/302903591957417203012304489947529736347874762775,-208965238551936641108002108112096334628087922688/969015032110259826352173485095262980905288420413,-859290202182082173684458899037542304891078180864/5007207875503957243795218944333844953080912307477,-10197621225091634689632545083717720162533638144/34796618359701895270882545993358473009022808147,-164860432541172988012500288848677826630810337280/369527502856320347152283004429443120261713839807,-4788459856255095265270714418435572229295055044608/17794019300086801999024109977398818324480077290739,-121768151542727502417327058247524448316084453376/214725024102500251121085398955994353664526185507,-4695507374188243411505217745040521455703542464512/8239742805158896400827962926559115961918687751871,-85837091223178700317543387027914465534606835712/134535282588166625496077801426867389035530698279,-580435442581073966343298317053478125353981444096/601721064087771031599502474452759076861582344741,-4399086184007617290603389456832361923188004749312/5587846899827315228018162025780624481889822155377,-8946398221587728817626161242544128840325987827712/11324914804645713522973305338138304500811523904485,-2380243102799262291488880640371567860996900913152/2500718746193551593444075481086648309079292861499,-2101645623657309528239208136369819998420526432256/2575644869312600520837378718018929479143740845201,-1478893849222614588217813826634995473583454552064/1537865163739176575553792502864197330470575328727,-1650966813993801114766861815173619324152071585792/1477194555638248894623263976557986753178854006673},{-22818873475658756052383747193627849323869896704/581295835759584558808312033228316509435714702879,976998916289555033383810311596041748929302233088/1148181310580490847889443245285958332842937431117,7586383608487890572942933955429067934453511225344/9299517291829908728651146368396238514550941572125,1714126851978983796318065960745923606019303276544/1983223999185639605862446337251366284942266303261,9175789268262439599570487075487982327846387318784/28146830865538587708960983240939498061618358464803,2118822138322137696317075292329089245204883439616/2663035629934843887354436756168725949136781115679,-1035135536219048502230860487599639901808833331200/1070022436834922326869526480917927343294110940591,-2151310265872916459076542960878097013057267957760/2191547549286739488416102204690701440504963321121,-4629211660552619529874925514650714422269913858048/4715350869261984712862393959738846535578964133343,-30783432736003875979750701542967690016264814592/64037670642997664188111209264428925624153125441,-293529744116052320987137638985152910776380424192/1063363920455119185533336766028459859555986812289,1818696859734868465859909813538812606321256300544/201086830318867823857028347393140421252269033251,-8797965056266683176252475579552797508633440026624/2726132327616754827110740409527767627130872864975,950884402381782865481050691257735479637701558272/969015032110259826352173485095262980905288420413,4910313066152228029763916645661983662521781846016/5007207875503957243795218944333844953080912307477,200489647855657583570731698405731449071592275968/521949275395528429063238189900377095135342122205,-264064968691283956425736981033314113190331678720/369527502856320347152283004429443120261713839807,3237718166968952441032952974617210505523697287168/17794019300086801999024109977398818324480077290739,-690405230055092879748857330835757135958038806528/1503075168717501757847597792691960475651683298549,-3750862582827464504403021450241614596972344246272/8239742805158896400827962926559115961918687751871,-103079032796038331763922255286326302856768913408/134535282588166625496077801426867389035530698279,-399722854162767122647551157340747551288997183488/6618931704965481347594527218980349845477405792151,-4191403943173474617574437652497846669758179573760/5587846899827315228018162025780624481889822155377,-8544545583914319080093878921340161755887043608576/11324914804645713522973305338138304500811523904485,-1570702742277753177347654238016256171368196866048/2500718746193551593444075481086648309079292861499,3067136313122161735618404786018771451266004418560/7726934607937801562512136154056788437431222535603,8017251242771651453532827175785619515969990819840/13840786473652589179984132525777775974235177958543,-210009175232972847106184266082692189666278375424/1477194555638248894623263976557986753178854006673},{292015572736985309319079896593271243862274211840/581295835759584558808312033228316509435714702879,406448713537773738248476345065189171331470458880/1148181310580490847889443245285958332842937431117,-2419017046828958004959018541626998047413981151232/9299517291829908728651146368396238514550941572125,605841361880050239986422753184209142956366495744/1983223999185639605862446337251366284942266303261,26376580871057029152502082943918053696927447384064/28146830865538587708960983240939498061618358464803,1829625818811552520540966823773129831638487793664/2663035629934843887354436756168725949136781115679,355357895910219463637798838165319385714427691008/1070022436834922326869526480917927343294110940591,-400465054874798630522403667777820192393459138560/2191547549286739488416102204690701440504963321121,2282559012109495953069810963444182641650457116672/4715350869261984712862393959738846535578964133343,361986275197950306925085973513804365362903384064/448263694500983649316778464851002479369071878087,4294441423989885021499182554914770621993064595456/3190091761365357556600010298085379578667960436867,-122506588092093177848048518135056155612152856576/201086830318867823857028347393140421252269033251,470849944371399163319599630506760597200342876160/109045293104670193084429616381110705085234914599,127886593915226235025779439534246475699425640448/969015032110259826352173485095262980905288420413,-1505216801932142153130468954670392960833913094144/5007207875503957243795218944333844953080912307477,-613844172024470845003066640846993936060801613824/521949275395528429063238189900377095135342122205,-254819809966568204935607306891143779378352095232/369527502856320347152283004429443120261713839807,127281167770811709473005771620679312072431894528/124433701399208405587581188653138589681678862173,1197126379723621998890849251275759964883179798528/1503075168717501757847597792691960475651683298549,6562719208971136758614765103246540865130763124736/8239742805158896400827962926559115961918687751871,42417100986611552576579593815379552102260932608/134535282588166625496077801426867389035530698279,3814345438454256446950114483520119135859390808064/6618931704965481347594527218980349845477405792151,-2796281770744980650596513001246509959046460604416/5587846899827315228018162025780624481889822155377,-1110906015150491879853805652350360710054221447168/2264982960929142704594661067627660900162304780897,-162999463660340098562820645803440974937427279872/2500718746193551593444075481086648309079292861499,-7039471309045169588769443884563022662595116007424/7726934607937801562512136154056788437431222535603,-7720896340977146716264447239622474994632773074944/13840786473652589179984132525777775974235177958543,319771745064582453583006035182977561718765387776/1477194555638248894623263976557986753178854006673}};
raysP = matrix {{1,1,1,1,1,1},{251112891175366054364274580161127/1742729142036330334599966324909420,-34025299559200432416283693893587/344714124034586031771472840661500,-166072625277962181545693392506793/1856806596362728607273125232693112,128406741685128773958560033805701/869050463315085459389094472964712,6841838086192401798777624145777/204686637756790519442275767776536,2131537604361729521355148099871/57850743173374257358441466703168},{-28909925018099237201050958950312/118822441502477068268179522152915,-11929325663257510348818223910588/86178531008646507942868210165375,24333199372073205901955950761380/232100824545341075909140654086639,-7193367473035672524891855261596/325893923743157047270910427361767,-1842628407698070068212035118957/4498607423226165262467599291792,-47544614283236920077152578537201/115701486346748514716882933406336}};
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,1,0,0},{0,0,1,0},{0,0,0,1}};
raysQ = map(QQ^3, QQ^0, 0);
linealityQ = map(QQ^3, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-593934448380038187629003561336244223315615416320/581295835759584558808312033228316509435714702879,-593934448380038187629003561336244223315615416320/581295835759584558808312033228316509435714702879,-779973149963902010794106307206482951439877406720/1148181310580490847889443245285958332842937431117,-779973149963902010794106307206482951439877406720/1148181310580490847889443245285958332842937431117,-834911336348711971508916753479457864880196943872/1033279699092212080961238485377359834950104619125,-451351638156153469675449537298017167288402182144/661074666395213201954148779083788761647422101087,-11875211446039377815154684977628597209263677374464/28146830865538587708960983240939498061618358464803,-1066404108435779557262026998475926048840047656960/2663035629934843887354436756168725949136781115679,-1066404108435779557262026998475926048840047656960/2663035629934843887354436756168725949136781115679,-286667771360306766448084578481195066846882037760/1070022436834922326869526480917927343294110940591,-101857867966402892552331775300708148248320147456/313078221326677069773728886384385920072137617303,-101857867966402892552331775300708148248320147456/313078221326677069773728886384385920072137617303,-247647508815704899421824604945096989996604194816/4715350869261984712862393959738846535578964133343,-231354719769776038518069933565989679675543126016/448263694500983649316778464851002479369071878087,1173995576268867024101222385236247095587712794624/1063363920455119185533336766028459859555986812289,10919588709302286410864254853031789456928047890432/201086830318867823857028347393140421252269033251,10919588709302286410864254853031789456928047890432/201086830318867823857028347393140421252269033251,7737382246988530702401719335728566856035340386304/302903591957417203012304489947529736347874762775,-208965238551936641108002108112096334628087922688/969015032110259826352173485095262980905288420413,-859290202182082173684458899037542304891078180864/5007207875503957243795218944333844953080912307477,-10197621225091634689632545083717720162533638144/34796618359701895270882545993358473009022808147,-10197621225091634689632545083717720162533638144/34796618359701895270882545993358473009022808147,-164860432541172988012500288848677826630810337280/369527502856320347152283004429443120261713839807,-4788459856255095265270714418435572229295055044608/17794019300086801999024109977398818324480077290739,-121768151542727502417327058247524448316084453376/214725024102500251121085398955994353664526185507,-4695507374188243411505217745040521455703542464512/8239742805158896400827962926559115961918687751871,-85837091223178700317543387027914465534606835712/134535282588166625496077801426867389035530698279,-580435442581073966343298317053478125353981444096/601721064087771031599502474452759076861582344741,-4399086184007617290603389456832361923188004749312/5587846899827315228018162025780624481889822155377,-8946398221587728817626161242544128840325987827712/11324914804645713522973305338138304500811523904485,-8946398221587728817626161242544128840325987827712/11324914804645713522973305338138304500811523904485,-2380243102799262291488880640371567860996900913152/2500718746193551593444075481086648309079292861499,-2380243102799262291488880640371567860996900913152/2500718746193551593444075481086648309079292861499,-2101645623657309528239208136369819998420526432256/2575644869312600520837378718018929479143740845201,-2101645623657309528239208136369819998420526432256/2575644869312600520837378718018929479143740845201,-1478893849222614588217813826634995473583454552064/1537865163739176575553792502864197330470575328727,-1478893849222614588217813826634995473583454552064/1537865163739176575553792502864197330470575328727,-1650966813993801114766861815173619324152071585792/1477194555638248894623263976557986753178854006673,-1650966813993801114766861815173619324152071585792/1477194555638248894623263976557986753178854006673,-1650966813993801114766861815173619324152071585792/1477194555638248894623263976557986753178854006673},{558476962283925802755928286034688660111844806175/581295835759584558808312033228316509435714702879,-22818873475658756052383747193627849323869896704/581295835759584558808312033228316509435714702879,2125180226870045881273253556882000081772239664205/1148181310580490847889443245285958332842937431117,976998916289555033383810311596041748929302233088/1148181310580490847889443245285958332842937431117,16885900900317799301594080323825306449004452797469/9299517291829908728651146368396238514550941572125,3697350851164623402180512297997289890961569579805/1983223999185639605862446337251366284942266303261,9175789268262439599570487075487982327846387318784/28146830865538587708960983240939498061618358464803,4781857768256981583671512048497815194341664555295/2663035629934843887354436756168725949136781115679,2118822138322137696317075292329089245204883439616/2663035629934843887354436756168725949136781115679,-1035135536219048502230860487599639901808833331200/1070022436834922326869526480917927343294110940591,-2151310265872916459076542960878097013057267957760/2191547549286739488416102204690701440504963321121,-2151310265872916459076542960878097013057267957760/2191547549286739488416102204690701440504963321121,-4629211660552619529874925514650714422269913858048/4715350869261984712862393959738846535578964133343,-30783432736003875979750701542967690016264814592/64037670642997664188111209264428925624153125441,-293529744116052320987137638985152910776380424192/1063363920455119185533336766028459859555986812289,2019783690053736289716938160931953027573525333795/201086830318867823857028347393140421252269033251,1818696859734868465859909813538812606321256300544/201086830318867823857028347393140421252269033251,-8797965056266683176252475579552797508633440026624/2726132327616754827110740409527767627130872864975,1919899434492042691833224176352998460542989978685/969015032110259826352173485095262980905288420413,9917520941656185273559135589995828615602694153493/5007207875503957243795218944333844953080912307477,200489647855657583570731698405731449071592275968/521949275395528429063238189900377095135342122205,722438923251186012633969888306108544206934398173/521949275395528429063238189900377095135342122205,-264064968691283956425736981033314113190331678720/369527502856320347152283004429443120261713839807,3237718166968952441032952974617210505523697287168/17794019300086801999024109977398818324480077290739,-690405230055092879748857330835757135958038806528/1503075168717501757847597792691960475651683298549,-3750862582827464504403021450241614596972344246272/8239742805158896400827962926559115961918687751871,-103079032796038331763922255286326302856768913408/134535282588166625496077801426867389035530698279,-399722854162767122647551157340747551288997183488/6618931704965481347594527218980349845477405792151,-4191403943173474617574437652497846669758179573760/5587846899827315228018162025780624481889822155377,-8544545583914319080093878921340161755887043608576/11324914804645713522973305338138304500811523904485,-8544545583914319080093878921340161755887043608576/11324914804645713522973305338138304500811523904485,-1570702742277753177347654238016256171368196866048/2500718746193551593444075481086648309079292861499,-1570702742277753177347654238016256171368196866048/2500718746193551593444075481086648309079292861499,3067136313122161735618404786018771451266004418560/7726934607937801562512136154056788437431222535603,10794070921059963298130540940075559888697226954163/7726934607937801562512136154056788437431222535603,8017251242771651453532827175785619515969990819840/13840786473652589179984132525777775974235177958543,21858037716424240633516959701563395490205168778383/13840786473652589179984132525777775974235177958543,-210009175232972847106184266082692189666278375424/1477194555638248894623263976557986753178854006673,1267185380405276047517079710475294563512575631249/1477194555638248894623263976557986753178854006673,-210009175232972847106184266082692189666278375424/1477194555638248894623263976557986753178854006673},{292015572736985309319079896593271243862274211840/581295835759584558808312033228316509435714702879,873311408496569868127391929821587753297988914719/581295835759584558808312033228316509435714702879,406448713537773738248476345065189171331470458880/1148181310580490847889443245285958332842937431117,1554630024118264586137919590351147504174407889997/1148181310580490847889443245285958332842937431117,-2419017046828958004959018541626998047413981151232/9299517291829908728651146368396238514550941572125,605841361880050239986422753184209142956366495744/1983223999185639605862446337251366284942266303261,54523411736595616861463066184857551758545805848867/28146830865538587708960983240939498061618358464803,1829625818811552520540966823773129831638487793664/2663035629934843887354436756168725949136781115679,4492661448746396407895403579941855780775268909343/2663035629934843887354436756168725949136781115679,1425380332745141790507325319083246729008538631599/1070022436834922326869526480917927343294110940591,-400465054874798630522403667777820192393459138560/2191547549286739488416102204690701440504963321121,1791082494411940857893698536912881248111504182561/2191547549286739488416102204690701440504963321121,6997909881371480665932204923183029177229421250015/4715350869261984712862393959738846535578964133343,810249969698933956241864438364806844731975262151/448263694500983649316778464851002479369071878087,7484533185355242578099192853000150200661025032323/3190091761365357556600010298085379578667960436867,-122506588092093177848048518135056155612152856576/201086830318867823857028347393140421252269033251,78580242226774646008979829258084265640116176675/201086830318867823857028347393140421252269033251,579895237476069356404029246887871302285577790759/109045293104670193084429616381110705085234914599,127886593915226235025779439534246475699425640448/969015032110259826352173485095262980905288420413,-1505216801932142153130468954670392960833913094144/5007207875503957243795218944333844953080912307477,-613844172024470845003066640846993936060801613824/521949275395528429063238189900377095135342122205,-613844172024470845003066640846993936060801613824/521949275395528429063238189900377095135342122205,-254819809966568204935607306891143779378352095232/369527502856320347152283004429443120261713839807,251714869170020115060586960273817901754110756701/124433701399208405587581188653138589681678862173,2700201548441123756738447043967720440534863097077/1503075168717501757847597792691960475651683298549,14802462014130033159442728029805656827049450876607/8239742805158896400827962926559115961918687751871,176952383574778178072657395242246941137791630887/134535282588166625496077801426867389035530698279,10433277143419737794544641702500468981336796600215/6618931704965481347594527218980349845477405792151,-2796281770744980650596513001246509959046460604416/5587846899827315228018162025780624481889822155377,-1110906015150491879853805652350360710054221447168/2264982960929142704594661067627660900162304780897,1154076945778650824740855415277300190108083333729/2264982960929142704594661067627660900162304780897,-162999463660340098562820645803440974937427279872/2500718746193551593444075481086648309079292861499,2337719282533211494881254835283207334141865581627/2500718746193551593444075481086648309079292861499,-7039471309045169588769443884563022662595116007424/7726934607937801562512136154056788437431222535603,-7039471309045169588769443884563022662595116007424/7726934607937801562512136154056788437431222535603,-7720896340977146716264447239622474994632773074944/13840786473652589179984132525777775974235177958543,-7720896340977146716264447239622474994632773074944/13840786473652589179984132525777775974235177958543,319771745064582453583006035182977561718765387776/1477194555638248894623263976557986753178854006673,319771745064582453583006035182977561718765387776/1477194555638248894623263976557986753178854006673,1796966300702831348206270011740964314897619394449/1477194555638248894623263976557986753178854006673}};
raysdesired = matrix {{1,1,1,1,1,1},{251112891175366054364274580161127/1742729142036330334599966324909420,-34025299559200432416283693893587/344714124034586031771472840661500,-166072625277962181545693392506793/1856806596362728607273125232693112,128406741685128773958560033805701/869050463315085459389094472964712,6841838086192401798777624145777/204686637756790519442275767776536,2131537604361729521355148099871/57850743173374257358441466703168},{-28909925018099237201050958950312/118822441502477068268179522152915,-11929325663257510348818223910588/86178531008646507942868210165375,24333199372073205901955950761380/232100824545341075909140654086639,-7193367473035672524891855261596/325893923743157047270910427361767,-1842628407698070068212035118957/4498607423226165262467599291792,-47544614283236920077152578537201/115701486346748514716882933406336}};
linealitydesired = map(QQ^3, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 8 1 - 3 0
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0},{0},{-3}};
raysQ = matrix {{1,0},{0,1},{0,0}};
linealityQ = map(QQ^3, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-1,-1},{-1,-1},{-4,-2}};
raysdesired = matrix {{1,0},{0,1},{0,0}};
linealitydesired = map(QQ^3, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 16 1 - 14 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{9,7,8,3,3,7,2,5,2,5,4,5,9,5},{0,7,5,6,4,0,3,3,5,0,6,2,5,1},{9,8,1,3,2,3,2,9,8,0,2,7,9,7},{0,7,2,3,6,5,3,1,2,8,6,9,6,0}};
raysQ = map(QQ^4, QQ^0, 0);
linealityQ = map(QQ^4, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{8,7,6,1,1,4,4,10,9,8,6,6,6,7,2,1,1,4,3,4,10,8,9,4,6,5,10,6,8,1,4,1,4,4,10,8,6,2,4,1,8,4,10,8,9,4,6,3,10,6,2,1,4,4,10,9,8,6,6,10,6,7,2,2,1,1,4,3,4,8,9,6,5,6,10,8,2,1,4,1,4,4,8,4,10,8,6,6,10,6,2,2,1,4,1,3,4,8,8,6,10},{-1,4,-1,2,4,-1,0,-1,4,-1,-1,0,8,6,7,4,6,1,7,2,1,8,6,7,1,7,6,2,-1,2,2,4,-1,0,-1,1,8,7,4,6,6,2,1,8,6,7,4,6,6,2,3,2,-1,1,-1,4,-1,-1,1,4,8,6,7,5,4,6,1,7,3,8,6,1,7,3,6,-1,3,2,2,4,-1,1,4,0,-1,-1,-1,1,4,8,7,5,4,4,6,7,3,6,8,3,6},{8,0,2,1,7,-1,6,8,0,2,-1,6,7,0,2,1,7,-1,1,6,8,7,0,2,-1,1,8,6,10,3,10,9,1,8,10,10,9,4,10,9,10,8,10,9,2,4,10,9,10,8,1,1,-1,6,8,0,2,-1,6,8,7,0,2,1,1,7,-1,1,6,7,0,-1,1,6,8,10,3,3,10,9,1,8,10,8,10,4,1,8,10,9,4,3,3,10,9,3,8,10,9,8,10},{-1,1,4,2,1,7,-1,-1,1,4,7,-1,6,1,2,2,1,7,5,-1,-1,6,1,2,7,5,5,-1,-1,2,0,1,7,-1,-1,-1,6,2,0,1,5,-1,-1,6,1,2,0,1,5,-1,7,4,9,10,1,3,6,9,10,7,8,3,4,7,4,3,9,7,10,8,3,9,7,10,7,1,7,4,2,3,9,10,7,1,1,6,9,10,7,8,4,7,4,2,3,7,10,7,8,10,7}};
raysdesired = map(QQ^4, QQ^0, 0);
linealitydesired = map(QQ^4, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 16 1 - 10 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1,1216652631687587/2251799813685248,-7496634952020485/18014398509481984,-4458529838789353/4503599627370496,-5887498334708929/9007199254740992,5110003651005145/18014398509481984,8648445090341621/9007199254740992,6790547823468567/9007199254740992,-1310547796085733/9007199254740992,-8206731815819627/9007199254740992},{0,3789648413623927/4503599627370496,4095111552621091/4503599627370496,5084384125703515/36028797018963968,-3408335435861847/4503599627370496,-8637222012098867/9007199254740992,-78648470848139/281474976710656,739701150294337/1125899906842624,8911346861657979/9007199254740992,7424066626269073/18014398509481984},{1,-7496634952020485/18014398509481984,-5887498334708929/9007199254740992,8648445090341621/9007199254740992,-1310547796085733/9007199254740992,-7557684451371807/9007199254740992,7600760748205539/9007199254740992,4926477479747821/36028797018963968,-2156457439366109/2251799813685248,371725260149319/562949953421312},{0,4095111552621091/4503599627370496,-3408335435861847/4503599627370496,-78648470848139/281474976710656,8911346861657979/9007199254740992,-2450053272283049/4503599627370496,-4833019187127717/9007199254740992,8922597835955781/9007199254740992,-648300634825759/2251799813685248,-3382145884720915/4503599627370496}};
raysQ = map(QQ^4, QQ^0, 0);
linealityQ = map(QQ^4, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-25511033461502469/18014398509481984,-8962129466159849/4503599627370496,-14894697589449921/9007199254740992,-12904394858476839/18014398509481984,-10317747050826725/9007199254740992,-17213931070560619/9007199254740992,2,3468452445372835/2251799813685248,10517763557461499/18014398509481984,23124402160487129/18014398509481984,17655644345082613/9007199254740992,15797747078209559/9007199254740992,7696651458655259/9007199254740992,800467438921365/9007199254740992,-25511033461502469/18014398509481984,-8962129466159849/4503599627370496,-14894697589449921/9007199254740992,-12904394858476839/18014398509481984,-10317747050826725/9007199254740992,-17213931070560619/9007199254740992,2,3468452445372835/2251799813685248,10517763557461499/18014398509481984,23124402160487129/18014398509481984,17655644345082613/9007199254740992,15797747078209559/9007199254740992,7696651458655259/9007199254740992,0,-25511033461502469/18014398509481984,-8962129466159849/4503599627370496,-14894697589449921/9007199254740992,-12904394858476839/18014398509481984,-358754164399371/9007199254740992,-17213931070560619/9007199254740992,2,10517763557461499/18014398509481984,45069788581143/4503599627370496,3119700920032063/9007199254740992,23124402160487129/18014398509481984,17655644345082613/9007199254740992,800467438921365/9007199254740992,0,-1035147181997661/2251799813685248,-25511033461502469/18014398509481984,-8962129466159849/4503599627370496,-358754164399371/9007199254740992,-2216651431272425/9007199254740992,-10317747050826725/9007199254740992,-17213931070560619/9007199254740992,2,3468452445372835/2251799813685248,10517763557461499/18014398509481984,45069788581143/4503599627370496,17655644345082613/9007199254740992,15797747078209559/9007199254740992,7696651458655259/9007199254740992,800467438921365/9007199254740992,-1035147181997661/2251799813685248,-25511033461502469/18014398509481984,-8962129466159849/4503599627370496,-14894697589449921/9007199254740992,-12904394858476839/18014398509481984,-2216651431272425/9007199254740992,-10317747050826725/9007199254740992,-17213931070560619/9007199254740992,2,3468452445372835/2251799813685248,3119700920032063/9007199254740992,23124402160487129/18014398509481984,17655644345082613/9007199254740992,15797747078209559/9007199254740992,7696651458655259/9007199254740992,-1035147181997661/2251799813685248,-25511033461502469/18014398509481984,-8962129466159849/4503599627370496,-14894697589449921/9007199254740992,-2216651431272425/9007199254740992,-10317747050826725/9007199254740992,-17213931070560619/9007199254740992,2,3468452445372835/2251799813685248,3119700920032063/9007199254740992,23124402160487129/18014398509481984,17655644345082613/9007199254740992,15797747078209559/9007199254740992,7696651458655259/9007199254740992,0,-8962129466159849/4503599627370496,-14894697589449921/9007199254740992,-12904394858476839/18014398509481984,-358754164399371/9007199254740992,-2216651431272425/9007199254740992,2,3119700920032063/9007199254740992,23124402160487129/18014398509481984,17655644345082613/9007199254740992,15797747078209559/9007199254740992,0,-1035147181997661/2251799813685248,-25511033461502469/18014398509481984,-8962129466159849/4503599627370496,-14894697589449921/9007199254740992,-2216651431272425/9007199254740992,-10317747050826725/9007199254740992,-17213931070560619/9007199254740992,2,3468452445372835/2251799813685248,10517763557461499/18014398509481984,45069788581143/4503599627370496,15797747078209559/9007199254740992,7696651458655259/9007199254740992,800467438921365/9007199254740992},{-408488074749405/4503599627370496,-30944412893260453/36028797018963968,-7911935063232343/4503599627370496,-17644421266839859/9007199254740992,-95852393083013/9007199254740992,-10590331883212911/18014398509481984,-1,-713951213746569/4503599627370496,-408488074749405/4503599627370496,-17644421266839859/9007199254740992,-360123447558795/281474976710656,-386198756548287/1125899906842624,-95852393083013/9007199254740992,-10590331883212911/18014398509481984,8598711179991587/4503599627370496,41113181144667483/36028797018963968,1095264191508649/4503599627370496,369977242642125/9007199254740992,17918546116398971/9007199254740992,25438465135751057/18014398509481984,1,8293248040994423/4503599627370496,8598711179991587/4503599627370496,369977242642125/9007199254740992,202826505862517/281474976710656,1865601057136961/1125899906842624,17918546116398971/9007199254740992,-1,-408488074749405/4503599627370496,-30944412893260453/36028797018963968,-7911935063232343/4503599627370496,-17644421266839859/9007199254740992,-360123447558795/281474976710656,-10590331883212911/18014398509481984,-1,-408488074749405/4503599627370496,-30944412893260453/36028797018963968,-7911935063232343/4503599627370496,-17644421266839859/9007199254740992,-360123447558795/281474976710656,-10590331883212911/18014398509481984,1,8293248040994423/4503599627370496,8598711179991587/4503599627370496,41113181144667483/36028797018963968,202826505862517/281474976710656,1865601057136961/1125899906842624,17918546116398971/9007199254740992,25438465135751057/18014398509481984,1,8293248040994423/4503599627370496,8598711179991587/4503599627370496,41113181144667483/36028797018963968,202826505862517/281474976710656,1865601057136961/1125899906842624,17918546116398971/9007199254740992,25438465135751057/18014398509481984,-713951213746569/4503599627370496,-408488074749405/4503599627370496,-30944412893260453/36028797018963968,-7911935063232343/4503599627370496,-17644421266839859/9007199254740992,-386198756548287/1125899906842624,-95852393083013/9007199254740992,-10590331883212911/18014398509481984,-1,-713951213746569/4503599627370496,-7911935063232343/4503599627370496,-17644421266839859/9007199254740992,-360123447558795/281474976710656,-386198756548287/1125899906842624,-95852393083013/9007199254740992,8293248040994423/4503599627370496,8598711179991587/4503599627370496,41113181144667483/36028797018963968,1095264191508649/4503599627370496,1865601057136961/1125899906842624,17918546116398971/9007199254740992,25438465135751057/18014398509481984,1,8293248040994423/4503599627370496,1095264191508649/4503599627370496,369977242642125/9007199254740992,202826505862517/281474976710656,1865601057136961/1125899906842624,17918546116398971/9007199254740992,-1,-30944412893260453/36028797018963968,-7911935063232343/4503599627370496,-17644421266839859/9007199254740992,-360123447558795/281474976710656,-386198756548287/1125899906842624,-1,-7911935063232343/4503599627370496,-17644421266839859/9007199254740992,-360123447558795/281474976710656,-386198756548287/1125899906842624,1,8293248040994423/4503599627370496,8598711179991587/4503599627370496,41113181144667483/36028797018963968,1095264191508649/4503599627370496,1865601057136961/1125899906842624,17918546116398971/9007199254740992,25438465135751057/18014398509481984,1,8293248040994423/4503599627370496,8598711179991587/4503599627370496,41113181144667483/36028797018963968,1865601057136961/1125899906842624,17918546116398971/9007199254740992,25438465135751057/18014398509481984},{-14894697589449921/9007199254740992,-358754164399371/9007199254740992,-10317747050826725/9007199254740992,-16564883706112799/9007199254740992,-4408257253051357/2251799813685248,-191224693271993/562949953421312,0,-25511033461502469/18014398509481984,-14894697589449921/9007199254740992,-16564883706112799/9007199254740992,-1406438506535453/9007199254740992,-31102319539216147/36028797018963968,-4408257253051357/2251799813685248,-191224693271993/562949953421312,-14894697589449921/9007199254740992,-358754164399371/9007199254740992,-10317747050826725/9007199254740992,-16564883706112799/9007199254740992,-4408257253051357/2251799813685248,-191224693271993/562949953421312,0,-25511033461502469/18014398509481984,-14894697589449921/9007199254740992,-16564883706112799/9007199254740992,-1406438506535453/9007199254740992,-31102319539216147/36028797018963968,-4408257253051357/2251799813685248,2,3119700920032063/9007199254740992,17655644345082613/9007199254740992,7696651458655259/9007199254740992,1449514803369185/9007199254740992,16607960002946531/9007199254740992,934675213570631/562949953421312,2,3119700920032063/9007199254740992,17655644345082613/9007199254740992,7696651458655259/9007199254740992,1449514803369185/9007199254740992,16607960002946531/9007199254740992,934675213570631/562949953421312,2,10517763557461499/18014398509481984,3119700920032063/9007199254740992,17655644345082613/9007199254740992,16607960002946531/9007199254740992,40955274498711789/36028797018963968,95342374319139/2251799813685248,934675213570631/562949953421312,2,10517763557461499/18014398509481984,3119700920032063/9007199254740992,17655644345082613/9007199254740992,16607960002946531/9007199254740992,40955274498711789/36028797018963968,95342374319139/2251799813685248,934675213570631/562949953421312,-25511033461502469/18014398509481984,-14894697589449921/9007199254740992,-358754164399371/9007199254740992,-10317747050826725/9007199254740992,-16564883706112799/9007199254740992,-31102319539216147/36028797018963968,-4408257253051357/2251799813685248,-191224693271993/562949953421312,0,-25511033461502469/18014398509481984,-10317747050826725/9007199254740992,-16564883706112799/9007199254740992,-1406438506535453/9007199254740992,-31102319539216147/36028797018963968,-4408257253051357/2251799813685248,-25511033461502469/18014398509481984,-14894697589449921/9007199254740992,-358754164399371/9007199254740992,-10317747050826725/9007199254740992,-31102319539216147/36028797018963968,-4408257253051357/2251799813685248,-191224693271993/562949953421312,0,-25511033461502469/18014398509481984,-10317747050826725/9007199254740992,-16564883706112799/9007199254740992,-1406438506535453/9007199254740992,-31102319539216147/36028797018963968,-4408257253051357/2251799813685248,2,17655644345082613/9007199254740992,7696651458655259/9007199254740992,1449514803369185/9007199254740992,16607960002946531/9007199254740992,40955274498711789/36028797018963968,2,7696651458655259/9007199254740992,1449514803369185/9007199254740992,16607960002946531/9007199254740992,40955274498711789/36028797018963968,2,10517763557461499/18014398509481984,3119700920032063/9007199254740992,17655644345082613/9007199254740992,7696651458655259/9007199254740992,40955274498711789/36028797018963968,95342374319139/2251799813685248,934675213570631/562949953421312,2,10517763557461499/18014398509481984,3119700920032063/9007199254740992,17655644345082613/9007199254740992,40955274498711789/36028797018963968,95342374319139/2251799813685248,934675213570631/562949953421312},{-7911935063232343/4503599627370496,-360123447558795/281474976710656,-95852393083013/9007199254740992,-6953652899653545/4503599627370496,-2900100448511007/2251799813685248,-7885745512091411/4503599627370496,-1,-408488074749405/4503599627370496,-7911935063232343/4503599627370496,-6953652899653545/4503599627370496,-13840218441868709/9007199254740992,-84601418785211/9007199254740992,-2900100448511007/2251799813685248,-7885745512091411/4503599627370496,-7911935063232343/4503599627370496,-360123447558795/281474976710656,-95852393083013/9007199254740992,-6953652899653545/4503599627370496,-2900100448511007/2251799813685248,-7885745512091411/4503599627370496,-1,-408488074749405/4503599627370496,-7911935063232343/4503599627370496,-6953652899653545/4503599627370496,-13840218441868709/9007199254740992,-84601418785211/9007199254740992,-2900100448511007/2251799813685248,-1,-7911935063232343/4503599627370496,-360123447558795/281474976710656,-95852393083013/9007199254740992,-6953652899653545/4503599627370496,-13840218441868709/9007199254740992,-7885745512091411/4503599627370496,-1,-7911935063232343/4503599627370496,-360123447558795/281474976710656,-95852393083013/9007199254740992,-6953652899653545/4503599627370496,-13840218441868709/9007199254740992,-7885745512091411/4503599627370496,-1,-408488074749405/4503599627370496,-7911935063232343/4503599627370496,-360123447558795/281474976710656,-13840218441868709/9007199254740992,-84601418785211/9007199254740992,-2900100448511007/2251799813685248,-7885745512091411/4503599627370496,-1,-408488074749405/4503599627370496,-7911935063232343/4503599627370496,-360123447558795/281474976710656,-13840218441868709/9007199254740992,-84601418785211/9007199254740992,-2900100448511007/2251799813685248,-7885745512091411/4503599627370496,8598711179991587/4503599627370496,1095264191508649/4503599627370496,202826505862517/281474976710656,17918546116398971/9007199254740992,2053546355087447/4503599627370496,17929797090696773/9007199254740992,1603499178859489/2251799813685248,1121453742649581/4503599627370496,1,8598711179991587/4503599627370496,17918546116398971/9007199254740992,2053546355087447/4503599627370496,4174180067613275/9007199254740992,17929797090696773/9007199254740992,1603499178859489/2251799813685248,8598711179991587/4503599627370496,1095264191508649/4503599627370496,202826505862517/281474976710656,17918546116398971/9007199254740992,17929797090696773/9007199254740992,1603499178859489/2251799813685248,1121453742649581/4503599627370496,1,8598711179991587/4503599627370496,17918546116398971/9007199254740992,2053546355087447/4503599627370496,4174180067613275/9007199254740992,17929797090696773/9007199254740992,1603499178859489/2251799813685248,1,202826505862517/281474976710656,17918546116398971/9007199254740992,2053546355087447/4503599627370496,4174180067613275/9007199254740992,17929797090696773/9007199254740992,1,17918546116398971/9007199254740992,2053546355087447/4503599627370496,4174180067613275/9007199254740992,17929797090696773/9007199254740992,1,8598711179991587/4503599627370496,1095264191508649/4503599627370496,202826505862517/281474976710656,17918546116398971/9007199254740992,17929797090696773/9007199254740992,1603499178859489/2251799813685248,1121453742649581/4503599627370496,1,8598711179991587/4503599627370496,1095264191508649/4503599627370496,202826505862517/281474976710656,17929797090696773/9007199254740992,1603499178859489/2251799813685248,1121453742649581/4503599627370496}};
raysdesired = map(QQ^4, QQ^0, 0);
linealitydesired = map(QQ^4, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 8 1 - 22 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1,-1,0,0,0,0,0,0},{0,0,1,-1,0,0,0,0},{0,0,0,0,1,-1,0,0},{0,0,0,0,0,0,1,-1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{28/3,20/3,4,20/3,20/3,4,4,4/3,20/3,20/3,4,4/3,4/3,4,4/3,4/3,4,4,4,4/3,4/3,4},{4,4/3,4/3,4/3,4,4,20/3,4/3,20/3,4,4/3,4,4/3,4,20/3,4,20/3,4,28/3,20/3,4/3,4},{4,4/3,4,4,4/3,20/3,4,4/3,20/3,4,20/3,20/3,20/3,28/3,4,4,4/3,4/3,4,4/3,4/3,4},{4/3,4,20/3,4,4,4/3,4/3,20/3,4/3,4/3,4,4,4,4/3,4,20/3,4,20/3,4/3,4,28/3,20/3}};
raysQ = map(QQ^4, QQ^0, 0);
linealityQ = map(QQ^4, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{31/3,23/3,23/3,23/3,23/3,1/3,1/3,1/3,3,1/3,1/3,3,1/3,1/3,20/3,4/3,4,4,4/3,28/3,20/3,4,20/3,4/3,4,4/3,4,4/3,20/3,4,4/3,4/3,4,28/3,20/3,20/3,4/3,4,4,4,4/3,4/3,4,20/3,4/3,4,4/3,4,28/3,20/3,4,4,4/3,20/3,20/3,4/3,4,4,4/3},{4,4/3,4/3,4,20/3,4/3,4,4/3,4,20/3,4,28/3,20/3,4/3,23/3,23/3,23/3,31/3,23/3,3,1/3,1/3,1/3,1/3,1/3,1/3,3,1/3,20/3,4/3,4,4/3,4,4,4/3,4,4/3,20/3,4,28/3,20/3,4/3,4/3,20/3,4,4,4/3,4,4,4/3,4,20/3,4/3,20/3,4,4/3,4,28/3,20/3},{4,4/3,4,4/3,20/3,4/3,20/3,20/3,28/3,4,4,4,4/3,4/3,20/3,4,4/3,4,4/3,4,4/3,4,4,4/3,20/3,20/3,28/3,4/3,23/3,23/3,23/3,23/3,31/3,3,1/3,1/3,1/3,1/3,1/3,3,1/3,1/3,4,20/3,4,4/3,4/3,4,4,4/3,20/3,4,4/3,20/3,4,20/3,28/3,4,4/3},{4/3,4,4,4,4/3,20/3,4,4,4/3,4,20/3,4/3,4,28/3,4/3,4,4,4/3,4,4/3,4,20/3,4,20/3,4,4,4/3,28/3,4/3,4,4,4,4/3,4/3,4,4,20/3,4,20/3,4/3,4,28/3,23/3,7/3,23/3,23/3,31/3,23/3,1/3,3,1/3,1/3,17/3,1/3,1/3,3,1/3,1/3,3}};
raysdesired = map(QQ^4, QQ^0, 0);
linealitydesired = map(QQ^4, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 7 1 - 9 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0,0,1,0,0,1,0},{1,0,0,0,0,0,1},{0,0,0,1,0,0,1},{0,0,0,0,1,1,0}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,1,0,0,0,0,1,1,1/2},{0,0,1,0,0,1,0,1,1/2},{0,0,0,1,0,1,1,0,1/2},{0,0,0,0,1,1,1,1,3/2}};
raysQ = map(QQ^4, QQ^0, 0);
linealityQ = map(QQ^4, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{0,0,1,0,2,2,2,0,0,1,0,0,1,1,1/2,2,1,2,2,3/2,1,0,0,0,1,1},{2,2,2,0,0,0,1,0,1,0,0,1,0,1,1/2,0,0,0,1,1/2,1,2,1,2,1,2},{0,1,0,0,0,1,0,2,2,2,0,1,1,0,1/2,0,0,1,0,1/2,1,1,2,2,2,1},{0,1,1,0,0,1,1,0,1,1,2,2,2,2,5/2,1,2,2,2,5/2,0,0,0,1,1,1}};
raysdesired = map(QQ^4, QQ^0, 0);
linealitydesired = map(QQ^4, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 16 1 - 12 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1,0,1,0,1,0,0,1,0,1,0,1,0,1,0,1},{1,1,0,0,0,0,1,1,0,0,1,1,0,0,1,1},{1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1},{0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1,0,1,0,1,1,-1,-1,-1,-1,-1,-1},{-1,-1,-1,-1,1,1,1,1,0,0,-1,-1},{-1,-1,1/2,1,-1/2,-1,1/2,-1,-1,1,1,-1/2},{1/2,1,-1,-1,-1,-1/2,-1,1/2,1,-1,-1/2,1}};
raysQ = map(QQ^4, QQ^0, 0);
linealityQ = map(QQ^4, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{2,1,2,0,0,-1,-1,2,1,0,-1,-1,2,2,2,2,1,0,1,0,1,1,-1,-1,-1,-1,-1,-1,1,1,-1,-1,2,2,0,-1,-1,2,1,-1,-1,2,1,2,0,0,-1,-1,2,1,2,1,0,0,-1,-1,-1,-1,-1,-1,2,1,2,1,2,2,0,0,0,0,0,0},{0,0,2,2,1,2,1,-1,-1,-1,0,-1,-1,-1,1,1,-1,-1,-1,-1,1,1,1,1,0,0,-1,-1,2,2,2,2,2,2,-1,0,-1,-1,-1,2,1,0,0,2,2,1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,1,1,0,0,0,0,0,0,2,2,2,2,1,1,0,0},{3/2,2,1/2,3/2,2,3/2,2,3/2,2,2,2,2,-1,1/2,-1/2,-1,-1,-1,1/2,1,-1/2,-1,1/2,-1,-1,1,1,-1/2,-1/2,-1,1/2,-1,-1/2,-1,-1,-1,-1/2,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,1/2,0,0,3/2,2,2,1/2,3/2,0,0,2,2,1/2,0,0,3/2,2,1/2,0,3/2,0,0,2,2,1/2},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/2,1/2,-1,-1,-1/2,1/2,1,-1,-1,-1,-1/2,-1,1/2,1,-1,-1/2,1,-1,-1/2,-1,1/2,-1,-1/2,2,2,2,3/2,2,3/2,2,3/2,2,1/2,3/2,2,1/2,2,3/2,2,0,0,1/2,2,0,3/2,2,0,1/2,2,3/2,2,0,0,0,1/2,0,3/2,2,0,1/2,2}};
raysdesired = map(QQ^4, QQ^0, 0);
linealitydesired = map(QQ^4, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 0 0 - 16 1
-- Checking minkowskiSum
TEST ///
verticesP = map(QQ^4, QQ^0, 0);
raysP = map(QQ^4, QQ^0, 0);
linealityP = matrix {{-1},{-1},{1},{1}};
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysQ = map(QQ^4, QQ^0, 0);
linealityQ = map(QQ^4, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = map(QQ^4, QQ^0, 0);
raysdesired = map(QQ^4, QQ^0, 0);
linealitydesired = map(QQ^4, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 11 0 - 16 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0,1,0,0,0,1,0,0},{0,0,1,0,0,0,1,0},{0,0,0,1,0,0,0,1},{1,1,1,1,-1,-1,-1,-1}};
raysP = matrix {{1,1,1},{1/2,2,1},{1/2,1,2},{0,0,0}};
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysQ = map(QQ^4, QQ^0, 0);
linealityQ = map(QQ^4, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-1,2,2,2,-1,1,-1,-1,1,-1,-1,2,2,2,-1,1,-1,-1,1,-1},{-1,-1,1,-1,2,2,2,-1,-1,1,-1,-1,1,-1,2,2,2,-1,-1,1},{-1,-1,-1,1,-1,-1,1,2,2,2,-1,-1,-1,1,-1,-1,1,2,2,2},{2,2,2,2,2,2,2,2,2,2,-2,-2,-2,-2,-2,-2,-2,-2,-2,-2}};
raysdesired = matrix {{1,1,1},{1/2,2,1},{1/2,1,2},{0,0,0}};
linealitydesired = map(QQ^4, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 16 1 - 24 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4},{2,2,3,3,4,4,1,1,3,3,4,4,1,1,2,2,4,4,1,1,2,2,3,3},{3,4,2,4,2,3,3,4,1,4,1,3,2,4,1,4,1,2,2,3,1,3,1,2},{4,3,4,2,3,2,4,3,4,1,3,1,4,2,4,1,2,1,3,2,3,1,2,1}};
raysQ = map(QQ^4, QQ^0, 0);
linealityQ = map(QQ^4, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{0,0,0,0,0,0,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,5,5,5,5,5,5,0,0,1,1,2,2,4,4,5,5,0,0,1,1,2,2,4,4,5,5,0,0,1,1,3,3,4,4,5,5,0,0,1,1,2,2,4,4,5,5,0,0,1,1,3,3,4,4,5,5,0,0,1,1,3,3,4,4,5,5,0,0,0,0,0,0,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5},{1,1,2,2,3,3,0,0,2,2,3,3,0,0,1,1,3,3,0,0,1,1,2,2,0,0,1,1,2,2,5,5,5,5,5,5,5,5,4,4,1,2,0,2,0,1,0,1,0,1,4,5,4,5,4,5,3,5,3,4,1,2,0,2,0,1,0,1,0,1,4,5,4,5,4,5,3,5,3,4,1,1,0,0,0,0,0,0,0,0,3,3,4,4,5,5,3,3,4,4,5,5,2,2,4,4,5,5,2,2,3,3,5,5,2,2,3,3,4,4},{2,3,1,3,1,2,2,3,0,3,0,2,1,3,0,3,0,1,1,2,0,2,0,1,1,2,0,2,0,1,1,2,0,2,0,1,0,1,0,1,5,5,5,5,5,5,5,5,4,4,5,4,5,4,5,4,5,3,4,3,2,1,2,0,1,0,1,0,1,0,1,1,0,0,0,0,0,0,0,0,4,5,4,5,4,5,3,5,3,4,4,5,3,5,3,4,4,5,3,5,3,4,4,5,2,5,2,4,3,5,2,5,2,3,3,4,2,4,2,3},{3,2,3,1,2,1,3,2,3,0,2,0,3,1,3,0,1,0,2,1,2,0,1,0,2,1,2,0,1,0,2,1,2,0,1,0,1,0,1,0,2,1,2,0,1,0,1,0,1,0,1,1,0,0,0,0,0,0,0,0,5,5,5,5,5,5,5,5,4,4,5,4,5,4,5,4,5,3,4,3,5,4,5,4,5,4,5,3,4,3,5,4,5,3,4,3,5,4,5,3,4,3,5,4,5,2,4,2,5,3,5,2,3,2,4,3,4,2,3,2}};
raysdesired = map(QQ^4, QQ^0, 0);
linealitydesired = map(QQ^4, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 16 1 - 16 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1,0,1,0,1,0,0,1,1,0,1,0,0,1,0,1},{3/4,1,1/4,0,1/4,0,1,3/4,3/4,1,1/4,0,0,1/4,1,3/4},{11/64,1/4,3/64,0,3/64,0,1/4,11/64,53/64,3/4,61/64,1,1,61/64,3/4,53/64},{31/32,61/64,127/128,1,1/128,0,3/64,1/32,25/128,11/64,15/64,1/4,3/4,49/64,53/64,103/128}};
raysQ = map(QQ^4, QQ^0, 0);
linealityQ = map(QQ^4, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-1,2,1,0,-1,-1,0,2,1,1,2,0,-1,-1,0,0,-1,0,-1,2,1,1,2,2,1,2,1,0,-1,-1,0,0,-1,0,-1,2,1,1,2,2,1,2,1,-1,2,1,0,-1,0,-1,2,1,2,1,0,-1,0,-1,-1,0,-1,0,2,1,2,1,1,2,1,2,0,-1,0,-1,-1,0,-1,0,2,1,2,1,1,2,1,2},{-1,-3/4,-1,5/4,1,2,7/4,5/4,1,2,7/4,-3/4,-1,0,-1/4,-1/4,0,-3/4,-1,-3/4,-1,0,-1/4,-1/4,0,-3/4,-1,5/4,1,2,7/4,7/4,2,5/4,1,5/4,1,2,7/4,7/4,2,5/4,1,-1,-3/4,-1,7/4,2,5/4,1,7/4,2,5/4,1,-1/4,0,-3/4,-1,-1,-3/4,0,-1/4,-1/4,0,-3/4,-1,-1,-3/4,0,-1/4,7/4,2,5/4,1,1,5/4,2,7/4,7/4,2,5/4,1,1,5/4,2,7/4},{-1,-61/64,-1,-61/64,-1,-3/4,-53/64,-61/64,-1,-3/4,-53/64,67/64,1,5/4,75/64,117/64,7/4,125/64,2,67/64,1,5/4,75/64,117/64,7/4,125/64,2,67/64,1,5/4,75/64,117/64,7/4,125/64,2,67/64,1,5/4,75/64,117/64,7/4,125/64,2,-1,-61/64,-1,-53/64,-3/4,-61/64,-1,-53/64,-3/4,-61/64,-1,75/64,5/4,67/64,1,2,125/64,7/4,117/64,75/64,5/4,67/64,1,2,125/64,7/4,117/64,75/64,5/4,67/64,1,2,125/64,7/4,117/64,75/64,5/4,67/64,1,2,125/64,7/4,117/64},{-1,-127/128,-1,-127/128,-1,-61/64,-31/32,-127/128,-1,-61/64,-31/32,-127/128,-1,-61/64,-31/32,-103/128,-53/64,-49/64,-3/4,-127/128,-1,-61/64,-31/32,-103/128,-53/64,-49/64,-3/4,-127/128,-1,-61/64,-31/32,-103/128,-53/64,-49/64,-3/4,-127/128,-1,-61/64,-31/32,-103/128,-53/64,-49/64,-3/4,2,255/128,2,63/32,125/64,255/128,2,63/32,125/64,255/128,2,63/32,125/64,255/128,2,7/4,113/64,117/64,231/128,63/32,125/64,255/128,2,7/4,113/64,117/64,231/128,63/32,125/64,255/128,2,7/4,113/64,117/64,231/128,63/32,125/64,255/128,2,7/4,113/64,117/64,231/128}};
raysdesired = map(QQ^4, QQ^0, 0);
linealitydesired = map(QQ^4, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 16 1 - 8 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{3/2,-1/2,1/2,1/2,1/2,1/2,1/2,1/2},{1/2,1/2,3/2,-1/2,1/2,1/2,1/2,1/2},{1/2,1/2,1/2,1/2,3/2,-1/2,1/2,1/2},{1/2,1/2,1/2,1/2,1/2,1/2,3/2,-1/2}};
raysQ = map(QQ^4, QQ^0, 0);
linealityQ = map(QQ^4, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-3/2,-1/2,-1/2,-1/2,5/2,3/2,3/2,3/2,-3/2,-1/2,-1/2,-1/2,5/2,3/2,3/2,3/2,-3/2,-1/2,-1/2,-1/2,5/2,3/2,3/2,3/2,-3/2,-1/2,-1/2,-1/2,5/2,3/2,3/2,3/2,-3/2,-1/2,-1/2,-1/2,5/2,3/2,3/2,3/2,-3/2,-1/2,-1/2,-1/2,5/2,3/2,3/2,3/2,-3/2,-1/2,-1/2,-1/2,5/2,3/2,3/2,3/2,-3/2,-1/2,-1/2,-1/2,5/2,3/2,3/2,3/2},{-1/2,-3/2,-1/2,-1/2,-1/2,-3/2,-1/2,-1/2,3/2,5/2,3/2,3/2,3/2,5/2,3/2,3/2,-1/2,-3/2,-1/2,-1/2,-1/2,-3/2,-1/2,-1/2,3/2,5/2,3/2,3/2,3/2,5/2,3/2,3/2,-1/2,-3/2,-1/2,-1/2,-1/2,-3/2,-1/2,-1/2,3/2,5/2,3/2,3/2,3/2,5/2,3/2,3/2,-1/2,-3/2,-1/2,-1/2,-1/2,-3/2,-1/2,-1/2,3/2,5/2,3/2,3/2,3/2,5/2,3/2,3/2},{-1/2,-1/2,-3/2,-1/2,-1/2,-1/2,-3/2,-1/2,-1/2,-1/2,-3/2,-1/2,-1/2,-1/2,-3/2,-1/2,3/2,3/2,5/2,3/2,3/2,3/2,5/2,3/2,3/2,3/2,5/2,3/2,3/2,3/2,5/2,3/2,-1/2,-1/2,-3/2,-1/2,-1/2,-1/2,-3/2,-1/2,-1/2,-1/2,-3/2,-1/2,-1/2,-1/2,-3/2,-1/2,3/2,3/2,5/2,3/2,3/2,3/2,5/2,3/2,3/2,3/2,5/2,3/2,3/2,3/2,5/2,3/2},{-1/2,-1/2,-1/2,-3/2,-1/2,-1/2,-1/2,-3/2,-1/2,-1/2,-1/2,-3/2,-1/2,-1/2,-1/2,-3/2,-1/2,-1/2,-1/2,-3/2,-1/2,-1/2,-1/2,-3/2,-1/2,-1/2,-1/2,-3/2,-1/2,-1/2,-1/2,-3/2,3/2,3/2,3/2,5/2,3/2,3/2,3/2,5/2,3/2,3/2,3/2,5/2,3/2,3/2,3/2,5/2,3/2,3/2,3/2,5/2,3/2,3/2,3/2,5/2,3/2,3/2,3/2,5/2,3/2,3/2,3/2,5/2}};
raysdesired = map(QQ^4, QQ^0, 0);
linealitydesired = map(QQ^4, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 32 1 - 17 0
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1},{-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1},{-1,-1,1,1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1},{-1,1,-1,1,-1,1,1,-1,1,-1,1,-1,1,-1,1,-1},{1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}};
raysQ = matrix {{0},{0},{0},{0},{-1}};
linealityQ = map(QQ^5, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-2,2,-2,2,-2,2,-2,2,-2,2,-2,2,-2,2,-2,2},{-2,-2,2,2,-2,-2,2,2,-2,-2,2,2,-2,-2,2,2},{-2,-2,-2,-2,2,2,2,2,-2,-2,-2,-2,2,2,2,2},{-2,-2,-2,-2,-2,-2,-2,-2,2,2,2,2,2,2,2,2},{2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}};
raysdesired = matrix {{0},{0},{0},{0},{-1}};
linealitydesired = map(QQ^5, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 10 1 - 32 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1,0,1,0,1,1,1,0,1,1},{1,0,0,0,1,1,0,0,1,1},{0,1,0,0,0,0,0,1,0,1},{1,1,0,1,0,1,0,1,0,1},{0,0,0,1,0,1,1,1,1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}};
raysQ = map(QQ^5, QQ^0, 0);
linealityQ = map(QQ^5, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{0,2,0,2,2,0,2,-1,-1,-1,1,-1,1,-1,1,-1,-1,1,-1,1,0,2,0,2,0,2,2,-1,-1,-1,1,-1,-1,-1,-1,1,-1,0,2,0,2,2,0,2,0,2,0,2,2,2,-1,1,-1,-1,1,-1,0,2,0,2,2,0,2,2,0,2,2,0,2,2,0,2},{0,0,2,2,0,2,2,-1,1,-1,-1,1,1,-1,-1,1,-1,-1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,-1,-1,1,-1,1,-1,-1,1,2,2,2,2,0,2,2,-1,-1,-1,-1,-1,-1,-1,-1,1,-1,-1,1,2,2,2,2,0,2,2,0,2,2,0,2,2,0,2,2},{-1,-1,-1,-1,1,1,1,0,0,2,2,2,2,0,0,0,2,2,2,2,-1,-1,1,1,-1,-1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,-1,-1,-1,-1,-1,1,1,-1,1,2,2,2,2,2,2,-1,-1,1,1,2,2,2,2,2,2,2,2,2,2,2,2},{2,2,2,2,2,2,2,0,0,0,0,0,0,2,2,2,2,2,2,2,-1,-1,-1,-1,1,1,1,0,0,2,2,2,0,0,2,2,2,-1,-1,-1,-1,2,2,2,-1,-1,-1,-1,1,1,0,0,0,2,2,2,-1,-1,-1,-1,0,0,0,2,2,2,0,0,0,2,2,2},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,2,2,2,2,2,-1,-1,-1,-1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,0,0,0,0,0,2,2,2,2,2,2}};
raysdesired = map(QQ^5, QQ^0, 0);
linealitydesired = map(QQ^5, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 10 1 - 11 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0,1,2,3,4,5,6,7,8,9},{0,1,4,9,16,25,36,49,64,81},{0,1,8,27,64,125,216,343,512,729},{0,1,16,81,256,625,1296,2401,4096,6561},{0,1,32,243,1024,3125,7776,16807,32768,59049}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1,0,0,0,0,-1,0,0,0,0,1},{0,1,0,0,0,0,-1,0,0,0,1},{0,0,1,0,0,0,0,-1,0,0,1},{0,0,0,1,0,0,0,0,-1,0,1},{0,0,0,0,1,0,0,0,0,-1,1}};
raysQ = map(QQ^5, QQ^0, 0);
linealityQ = map(QQ^5, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{1,0,0,0,-1,0,0,0,0,2,1,1,1,0,1,1,1,1,2,3,2,2,2,1,2,2,3,4,3,3,3,2,3,3,4,5,4,4,4,3,4,4,5,6,5,5,5,4,5,5,6,7,6,6,6,5,6,6,7,8,7,7,7,6,7,7,8,9,8,8,8,7,8,8,9,10,9,9,9,9,8,9,9,9,10},{0,1,0,0,0,-1,0,0,0,1,2,1,1,1,0,1,1,1,2,4,5,4,4,4,3,4,5,9,10,9,9,9,8,9,10,16,17,16,16,16,15,16,17,25,26,25,25,25,24,25,26,36,37,36,36,36,35,36,37,49,50,49,49,49,48,49,50,64,65,64,64,64,63,64,65,81,82,81,81,81,81,80,81,81,82},{0,0,1,0,0,0,-1,0,0,1,1,2,1,1,1,0,1,1,2,8,8,9,8,8,8,7,9,27,27,28,27,27,27,26,28,64,64,65,64,64,64,63,65,125,125,126,125,125,125,124,126,216,216,217,216,216,216,215,217,343,343,344,343,343,343,342,344,512,512,513,512,512,512,511,513,729,729,730,729,729,729,729,728,729,730},{0,0,0,1,0,0,0,-1,0,1,1,1,2,1,1,1,0,1,2,16,16,16,17,16,16,16,17,81,81,81,82,81,81,81,82,256,256,256,257,256,256,256,257,625,625,625,626,625,625,625,626,1296,1296,1296,1297,1296,1296,1296,1297,2401,2401,2401,2402,2401,2401,2401,2402,4096,4096,4096,4097,4096,4096,4096,4097,6561,6561,6561,6562,6561,6561,6561,6561,6560,6562},{0,0,0,0,0,0,0,0,-1,1,1,1,1,1,1,1,1,0,2,32,32,32,32,32,32,32,33,243,243,243,243,243,243,243,244,1024,1024,1024,1024,1024,1024,1024,1025,3125,3125,3125,3125,3125,3125,3125,3126,7776,7776,7776,7776,7776,7776,7776,7777,16807,16807,16807,16807,16807,16807,16807,16808,32768,32768,32768,32768,32768,32768,32768,32769,59049,59049,59049,59049,59050,59049,59049,59049,59049,59050}};
raysdesired = map(QQ^5, QQ^0, 0);
linealitydesired = map(QQ^5, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 10 1 - 17 0
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1,0,1,0,1,1,1,0,1,1},{1,0,0,0,1,1,0,0,1,1},{0,1,0,0,0,0,0,1,0,1},{1,1,0,1,0,1,0,1,0,1},{0,0,0,1,0,1,1,1,1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1},{-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1},{-1,-1,1,1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1},{-1,1,-1,1,-1,1,1,-1,1,-1,1,-1,1,-1,1,-1},{1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}};
raysQ = matrix {{0},{0},{0},{0},{-1}};
linealityQ = map(QQ^5, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{1,-1,-1,-1,-1,2,2,0,2,2,2,2,0,0,1,1,-1,-1,-1,-1,2,2,0,0,2,2,2,2,0,0},{-1,1,1,-1,-1,0,2,2,-1,-1,-1,-1,-1,-1,-1,-1,1,1,-1,-1,2,2,2,2,0,0,2,2,2,2},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,-1,2,2,2,2,2,2,1,-1,1,-1,2,2,2,2,2,2},{2,2,0,2,0,2,2,2,-1,1,-1,1,-1,-1,0,2,2,0,2,0,-1,-1,-1,-1,0,2,0,2,2,0},{2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}};
raysdesired = matrix {{0},{0},{0},{0},{-1}};
linealitydesired = map(QQ^5, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 10 1 - 4 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0,1,0,0,0,0,1,1,1/2,7/18},{0,0,1,0,0,1,0,1,1/2,7/18},{0,0,0,1,0,1,1,0,1/2,7/18},{0,0,0,0,1,1,1,1,3/2,11/18},{0,0,0,0,0,0,0,0,0,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,1,0,1},{0,0,1,1},{0,0,0,0},{0,0,0,0},{0,0,0,0}};
raysQ = map(QQ^5, QQ^0, 0);
linealityQ = map(QQ^5, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{0,2,2,0,1,0,1,0,1,0,1,0,0,0,1,1,2,2,2,1,2,1/2,3/2,1/2,3/2,7/18,25/18,7/18,25/18},{0,0,1,2,2,0,0,1,1,0,0,1,1,2,2,0,0,1,1,2,2,1/2,1/2,3/2,3/2,7/18,7/18,25/18,25/18},{0,0,0,0,0,1,1,1,1,0,0,0,1,1,1,1,1,1,0,0,0,1/2,1/2,1/2,1/2,7/18,7/18,7/18,7/18},{0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,3/2,3/2,3/2,3/2,11/18,11/18,11/18,11/18},{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1}};
raysdesired = map(QQ^5, QQ^0, 0);
linealitydesired = map(QQ^5, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 10 1 - 32 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0,1,0,0,0,0,1,1,1/2,7/18},{0,0,1,0,0,1,0,1,1/2,7/18},{0,0,0,1,0,1,1,0,1/2,7/18},{0,0,0,0,1,1,1,1,3/2,11/18},{0,0,0,0,0,0,0,0,0,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}};
raysQ = map(QQ^5, QQ^0, 0);
linealityQ = map(QQ^5, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-1,-1,2,2,2,2,2,2,2,2,2,2,-1,1,-1,1,-1,-1,1,-1,1,-1,-1,1,-1,1,-1,-1,1,-1,1,-1,-1,1,-1,-1,-1,1,-1,-1,-1,1,-1,-1,-1,1,-1,1,-1,-1,-1,1,2,2,2,0,2,2,2,2,2,0,2,2,2,2,2,0,2,2,2,2,2,0,2,2,-1/2,3/2,-1/2,3/2,-1/2,3/2,-1/2,3/2,-1/2,3/2,-1/2,3/2,-1/2,3/2,-1/2,3/2,-11/18,25/18,-11/18,25/18,-11/18,25/18,-11/18,25/18,-11/18,25/18,-11/18,25/18,-11/18,25/18,-11/18,25/18},{-1,-1,-1,1,-1,1,-1,-1,1,-1,1,-1,2,2,2,2,2,2,2,2,2,2,-1,-1,1,1,-1,-1,-1,1,1,-1,-1,-1,1,-1,-1,-1,1,-1,2,2,2,0,2,2,2,2,2,0,2,2,-1,1,-1,-1,-1,1,-1,1,-1,-1,-1,1,2,2,0,2,2,2,2,2,0,2,2,2,-1/2,-1/2,3/2,3/2,-1/2,-1/2,3/2,3/2,-1/2,-1/2,3/2,3/2,-1/2,-1/2,3/2,3/2,-11/18,-11/18,25/18,25/18,-11/18,-11/18,25/18,25/18,-11/18,-11/18,25/18,25/18,-11/18,-11/18,25/18,25/18},{-1,-1,-1,-1,1,1,-1,-1,-1,1,1,-1,-1,-1,1,1,-1,-1,-1,1,1,-1,2,2,2,2,2,2,2,2,2,2,-1,-1,-1,1,-1,-1,-1,1,2,2,0,2,2,2,2,2,0,2,2,2,2,2,0,2,2,2,2,2,0,2,2,2,-1,1,-1,-1,-1,1,-1,1,-1,-1,-1,1,-1/2,-1/2,-1/2,-1/2,3/2,3/2,3/2,3/2,-1/2,-1/2,-1/2,-1/2,3/2,3/2,3/2,3/2,-11/18,-11/18,-11/18,-11/18,25/18,25/18,25/18,25/18,-11/18,-11/18,-11/18,-11/18,25/18,25/18,25/18,25/18},{-1,-1,-1,-1,-1,-1,1,-1,-1,-1,-1,1,-1,-1,-1,-1,1,-1,-1,-1,-1,1,-1,-1,-1,-1,1,-1,-1,-1,-1,1,2,2,2,2,2,2,2,2,0,0,2,2,2,2,0,0,2,2,2,2,0,0,2,2,2,2,0,0,2,2,2,2,0,0,2,2,2,2,0,0,2,2,2,2,5/2,5/2,5/2,5/2,5/2,5/2,5/2,5/2,5/2,5/2,5/2,5/2,5/2,5/2,5/2,5/2,-7/18,-7/18,-7/18,-7/18,-7/18,-7/18,-7/18,-7/18,29/18,29/18,29/18,29/18,29/18,29/18,29/18,29/18},{-1,1,-1,-1,-1,-1,-1,1,1,1,1,1,-1,-1,-1,-1,-1,1,1,1,1,1,-1,-1,-1,-1,-1,1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}};
raysdesired = map(QQ^5, QQ^0, 0);
linealitydesired = map(QQ^5, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 10 1 - 4 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1,-1,0,0,0,0,0,0,0,0},{0,0,1,-1,0,0,0,0,0,0},{0,0,0,0,1,-1,0,0,0,0},{0,0,0,0,0,0,1,-1,0,0},{0,0,0,0,0,0,0,0,1,-1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,1,0,1},{0,0,1,1},{0,0,0,0},{0,0,0,0},{0,0,0,0}};
raysQ = map(QQ^5, QQ^0, 0);
linealityQ = map(QQ^5, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{2,2,-1,-1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1},{0,1,0,1,2,2,-1,-1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1},{0,0,0,0,0,0,0,0,1,1,1,1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,-1,-1,-1,-1,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,-1,-1,-1,-1}};
raysdesired = map(QQ^5, QQ^0, 0);
linealitydesired = map(QQ^5, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 7 1 - 32 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0,1,0,1,1,1,3},{0,0,1,1,0,0,4},{1,1,1,1,0,0,0},{0,0,0,0,1,0,0},{0,0,0,0,0,1,0}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}};
raysQ = map(QQ^5, QQ^0, 0);
linealityQ = map(QQ^5, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,2,2,-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,2,0,2,0,0,2,0,2,0,2,0,2,0,2,0,2,0,2,0,2,0,2,0,0,2,0,2,0,2,0,2,0,2,0,2,0,2,0,2,2,4,2,4,4,2,4,4,2,4,4,2,4,4,2,4,4,2,4,4,2,4,4,2,4},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,2,2,2,2,2,2,2,2,2,-1,-1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,3,3,5,5,3,5,5,3,5,5,3,5,5,3,5,5,3,5,5,3,5,5,3,5,5},{0,2,0,2,0,2,0,2,0,2,2,2,2,0,2,0,2,0,2,0,2,2,2,2,2,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,-1,-1,-1,1,1,1,-1,-1,-1,1,1,1,-1,-1,-1,1,1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,-1,1,-1,-1,1,1,-1,-1,1,1,-1,1,-1,1,0,0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,0,0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1}};
raysdesired = map(QQ^5, QQ^0, 0);
linealitydesired = map(QQ^5, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 10 1 - 4 0
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1,0,1,0,1,1,1,0,1,1},{1,0,0,0,1,1,0,0,1,1},{0,1,0,0,0,0,0,1,0,1},{1,1,0,1,0,1,0,1,0,1},{0,0,0,1,0,1,1,1,1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{3,1,5/3},{3,5/2,1},{0,0,0},{2,4,1},{3,5,2}};
raysQ = matrix {{0},{0},{1},{0},{0}};
linealityQ = map(QQ^5, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{4,2,8/3,3,1,5/3,4,2,8/3,1,5/3,4,2,8/3,4,2,4,2,8/3,4,2,8/3},{4,7/2,2,3,5/2,1,3,5/2,1,5/2,1,4,7/2,2,4,7/2,3,5/2,1,4,7/2,2},{0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{3,5,2,3,5,2,2,4,1,5,2,2,4,1,3,5,2,4,1,2,4,1},{3,5,2,3,5,2,3,5,2,6,3,3,5,2,4,6,4,6,3,4,6,3}};
raysdesired = matrix {{0},{0},{1},{0},{0}};
linealitydesired = map(QQ^5, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 8 1 - 6 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1,-1,1,0,0,0,0},{-1,-1,1,1,0,0,0,0},{-1,-1,-1,-1,1,1,1,1},{0,0,0,0,-1,1,-1,1},{0,0,0,0,-1,-1,1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1,0,0,0,0,0},{0,1,0,0,0,0},{0,0,1,0,0,0},{0,0,0,1,0,0},{0,0,0,0,1,0}};
raysQ = map(QQ^5, QQ^0, 0);
linealityQ = map(QQ^5, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-1,-1,-1,-1,2,1,1,1,-1,-1,-1,-1,2,1,1,1,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0},{-1,-1,-1,-1,-1,-1,-1,-1,2,1,1,1,1,2,1,1,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0},{0,-1,-1,-1,-1,0,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1},{0,1,0,0,0,0,1,0,0,0,1,0,0,0,1,0,-1,-1,-1,-1,1,1,1,2,-1,-1,-1,-1,1,1,1,2,1},{0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,2,1,1,1,1,2}};
raysdesired = map(QQ^5, QQ^0, 0);
linealitydesired = map(QQ^5, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 15 1 - 4 0
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1,1,1,1,1,1,1,1,1,1,0,0,0,0,0},{1,1,1,1,1,1,0,0,0,0,1,1,1,1,0},{1,1,1,0,0,0,1,1,1,0,1,1,1,0,1},{1,0,0,1,1,0,1,1,0,1,1,1,0,1,1},{0,1,0,1,0,1,1,0,1,1,1,0,1,1,1},{0,0,1,0,1,1,0,1,1,1,0,1,1,1,1}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0},{1},{0},{5},{0},{1}};
raysQ = matrix {{1,0,0},{1,0,0},{0,1,0},{0,1,0},{0,0,1},{0,0,1}};
linealityQ = map(QQ^6, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{1,1,1,1,1,1,1,1,1,1,0,0,0,0,0},{2,2,2,2,2,2,1,1,1,1,2,2,2,2,1},{1,1,1,0,0,0,1,1,1,0,1,1,1,0,1},{6,5,5,6,6,5,6,6,5,6,6,6,5,6,6},{0,1,0,1,0,1,1,0,1,1,1,0,1,1,1},{1,1,2,1,2,2,1,2,2,2,1,2,2,2,2}};
raysdesired = matrix {{1,0,0},{1,0,0},{0,1,0},{0,1,0},{0,0,1},{0,0,1}};
linealitydesired = map(QQ^6, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 14 1 - 6 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0,0,1/2,0,1,1/2,0,0,0,0,0,1,0,0},{0,1,1/2,0,0,0,1/2,0,0,0,0,0,1,0},{1,0,1/2,0,0,0,0,1/2,0,0,0,0,0,1},{0,0,0,0,0,1/2,1/2,0,0,1,0,0,0,1},{0,0,0,0,0,1/2,0,1/2,1,0,0,0,1,0},{0,0,0,0,0,0,1/2,1/2,0,0,1,1,0,0}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{3,3,1,2,0,0},{0,0,2,1,3,3},{0,4,6,0,6,2},{6,2,0,6,0,4},{4,0,0,5,1,5},{1,5,5,0,4,0}};
raysQ = map(QQ^6, QQ^0, 0);
linealityQ = map(QQ^6, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{3,3,1,2,0,0,3,3,1,2,0,0,7/2,7/2,3/2,5/2,1/2,1/2,3,3,1,2,0,0,4,4,2,3,1,1,7/2,5/2,3,3,1,2,0,0,3,3,1,2,0,0,3,3,1,2,0,0,3,3,1,2,0,0,3,3,1,2,0,0,4,4,2,2,0,0,3,3,1,2,0,0},{0,0,2,1,3,3,1,1,3,2,4,4,1/2,1/2,5/2,3/2,7/2,7/2,0,0,2,1,3,3,0,0,2,1,3,3,0,1,1/2,1/2,5/2,3/2,7/2,7/2,0,0,2,1,3,3,0,0,2,1,3,3,0,0,2,1,3,3,0,0,2,1,3,3,0,0,2,2,4,4,0,0,2,1,3,3},{1,5,7,1,7,3,0,4,6,0,6,2,1/2,9/2,13/2,1/2,13/2,5/2,0,4,6,0,6,2,0,4,6,0,6,2,0,0,0,4,6,0,6,2,1/2,9/2,13/2,1/2,13/2,5/2,0,4,6,0,6,2,0,4,6,0,6,2,0,4,6,0,6,2,0,4,6,0,6,2,1,5,7,1,7,3},{6,2,0,6,0,4,6,2,0,6,0,4,6,2,0,6,0,4,6,2,0,6,0,4,6,2,0,6,0,4,13/2,13/2,13/2,5/2,1/2,13/2,1/2,9/2,6,2,0,6,0,4,6,2,0,6,0,4,7,3,1,7,1,5,6,2,0,6,0,4,6,2,0,6,0,4,7,3,1,7,1,5},{4,0,0,5,1,5,4,0,0,5,1,5,4,0,0,5,1,5,4,0,0,5,1,5,4,0,0,5,1,5,9/2,11/2,4,0,0,5,1,5,9/2,1/2,1/2,11/2,3/2,11/2,5,1,1,6,2,6,4,0,0,5,1,5,4,0,0,5,1,5,4,0,0,6,2,6,4,0,0,5,1,5},{1,5,5,0,4,0,1,5,5,0,4,0,1,5,5,0,4,0,1,5,5,0,4,0,1,5,5,0,4,0,1,0,3/2,11/2,11/2,1/2,9/2,1/2,3/2,11/2,11/2,1/2,9/2,1/2,1,5,5,0,4,0,1,5,5,0,4,0,2,6,6,1,5,1,2,6,6,0,4,0,1,5,5,0,4,0}};
raysdesired = map(QQ^6, QQ^0, 0);
linealitydesired = map(QQ^6, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 8 1 - 7 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1,-1,1,0,0,0,0},{-1,-1,1,1,0,0,0,0},{-1,-1,-1,-1,1,1,1,1},{0,0,0,0,0,1,0,0},{0,0,0,0,0,0,1,0},{0,0,0,0,0,0,0,1}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1,0,0,0,0,0,-1},{0,1,0,0,0,0,-1},{0,0,1,0,0,0,-1},{0,0,0,1,0,0,-1},{0,0,0,0,1,0,-1},{0,0,0,0,0,1,-1}};
raysQ = map(QQ^6, QQ^0, 0);
linealityQ = map(QQ^6, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-1,-1,-1,-1,-2,2,1,1,1,1,0,-1,-1,-1,-1,-1,-2,2,1,1,1,1,0,1,0,0,-1,1,0,0,0,-1,1,0,0,0,-1,1,0,0,0,-1},{-1,-1,-1,-1,-2,-1,-1,-1,-1,-1,-2,2,1,1,1,1,0,1,2,1,1,1,0,0,1,0,-1,0,1,0,0,-1,0,1,0,0,-1,0,1,0,0,-1},{0,-1,-1,-1,-2,-1,0,-1,-1,-1,-2,-1,0,-1,-1,-1,-2,-1,-1,-1,-1,-1,-2,1,1,2,0,1,1,2,1,0,1,1,2,1,0,1,1,2,1,0},{0,1,0,0,-1,0,0,1,0,0,-1,0,0,1,0,0,-1,0,0,1,0,0,-1,0,0,0,-1,1,1,1,2,0,0,0,0,0,-1,0,0,0,0,-1},{0,0,1,0,-1,0,0,0,1,0,-1,0,0,0,1,0,-1,0,0,0,1,0,-1,0,0,0,-1,0,0,0,0,-1,1,1,1,2,0,0,0,0,0,-1},{0,0,0,1,-1,0,0,0,0,1,-1,0,0,0,0,1,-1,0,0,0,0,1,-1,0,0,0,-1,0,0,0,0,-1,0,0,0,0,-1,1,1,1,2,0}};
raysdesired = map(QQ^6, QQ^0, 0);
linealitydesired = map(QQ^6, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 15 1 - 42 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1,1,1,1,1,0,0,0,0,0,0,0,0,0,0},{1,0,0,0,0,1,1,1,1,0,0,0,0,0,0},{0,1,0,0,0,1,0,0,0,1,1,1,0,0,0},{0,0,1,0,0,0,1,0,0,1,0,0,1,1,0},{0,0,0,1,0,0,0,1,0,0,1,0,1,0,1},{0,0,0,0,1,0,0,0,1,0,0,1,0,1,1}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{20,10,4,10,4,34,35,19,4,31,4,34,31,4,25,25,31,16,25,19,9,9,33,25,34,25,31,16,1,1,1,1,1,1,1,1,1,1,1,1,1,1},{1,1,1,1,1,1,1,1,1,1,1,1,1,1,16,19,9,9,16,4,4,4,4,19,4,20,10,10,31,19,31,34,10,34,10,35,20,4,4,4,4,4},{4,4,10,4,10,4,4,10,20,20,35,10,19,34,10,4,4,4,9,1,1,1,1,1,1,1,1,1,9,4,10,4,1,1,1,1,1,9,9,19,34,33},{9,19,16,34,31,19,9,1,1,1,1,1,4,4,1,1,1,1,4,10,20,35,20,10,10,4,4,4,4,1,1,1,19,10,34,4,4,16,31,1,1,4},{31,25,25,1,1,1,16,34,31,4,4,19,1,1,4,10,20,35,1,31,25,1,1,1,16,9,19,34,1,34,4,10,25,1,1,9,33,25,1,31,4,1},{15,15,15,25,25,31,25,15,15,33,30,25,34,31,34,31,25,15,35,15,15,25,31,34,25,31,25,15,34,15,33,30,15,33,25,30,15,15,25,15,30,31}};
raysQ = map(QQ^6, QQ^0, 0);
linealityQ = map(QQ^6, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{21,11,5,11,5,35,36,20,5,32,5,35,32,5,26,26,32,17,26,20,10,10,34,26,35,26,32,17,2,2,2,2,2,2,2,2,2,2,2,2,2,2,21,11,5,11,5,35,36,20,5,32,5,35,32,5,2,2,2,2,2,21,11,5,11,5,35,36,20,10,10,34,35,2,2,2,2,21,11,5,36,20,5,35,32,17,20,10,35,32,17,2,2,2,2,2,21,11,5,11,5,35,36,20,5,32,5,35,32,5,26,26,32,17,26,20,10,10,34,26,35,26,32,17,2,2,2,2,2,2,2,2,2,2,2,2,2,2,20,10,4,10,4,34,35,19,4,31,4,34,31,4,25,25,31,16,25,19,9,9,33,25,34,25,31,16,1,1,1,1,1,1,1,1,1,1,1,1,1,1,19,9,9,33,25,34,25,31,16,1,1,1,1,1,1,1,25,31,16,25,31,16,1,1,1,1,1,1,1,25,25,25,25,25,1,1,1,1,1,1,1,1,1,1,1,1,1,1,20,10,4,10,4,34,35,19,4,31,4,34,31,4,25,25,31,16,25,19,9,9,33,25,34,25,31,16,1,1,1,1,1,1,1,1,1,1,1,1,1,1,4,19,4,31,4,34,25,25,31,16,1,1,1,1,1,1,4,4,4,31,4,31,4,25,25,1,1,1,1,1,1,1,20,10,4,10,4,34,35,19,4,31,4,34,31,4,25,25,31,16,25,19,9,9,33,25,34,25,31,16,1,1,1,1,1,1,1,1,1,1,1,1,1,1,10,4,10,4,34,31,4,25,9,9,33,25,1,1,1,1,1,1,1,20,10,4,10,4,34,35,19,4,31,4,34,31,4,25,25,31,16,25,19,9,9,33,25,34,25,31,16,1,1,1,1,1,1,1,1,1,1,1,1,1,1},{2,2,2,2,2,2,2,2,2,2,2,2,2,2,17,20,10,10,17,5,5,5,5,20,5,21,11,11,32,20,32,35,11,35,11,36,21,5,5,5,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,4,4,4,4,4,1,1,1,1,1,1,1,4,4,4,4,4,10,10,4,4,1,1,1,1,1,1,1,9,9,4,4,4,10,10,19,10,20,4,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,16,19,9,9,16,4,4,4,4,19,4,20,10,10,31,19,31,34,10,34,10,35,20,4,4,4,4,4,2,2,2,2,2,2,2,2,2,2,2,2,2,2,17,20,10,10,17,5,5,5,5,20,5,21,11,11,32,20,32,35,11,35,11,36,21,5,5,5,5,5,5,5,5,5,20,5,21,11,11,11,35,11,36,21,5,5,20,10,10,21,11,11,20,35,11,36,21,5,5,17,20,17,20,21,32,20,32,35,11,35,11,36,21,5,5,5,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,16,19,9,9,16,4,4,4,4,19,4,20,10,10,31,19,31,34,10,34,10,35,20,4,4,4,4,4,1,1,1,1,1,1,16,19,9,9,19,31,34,4,4,4,1,1,1,1,1,1,1,16,16,31,31,4,4,4,4,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,16,19,9,9,16,4,4,4,4,19,4,20,10,10,31,19,31,34,10,34,10,35,20,4,4,4,4,4,1,1,1,1,1,1,1,16,4,4,4,19,31,10,34,10,4,4,4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,16,19,9,9,16,4,4,4,4,19,4,20,10,10,31,19,31,34,10,34,10,35,20,4,4,4,4,4},{4,4,10,4,10,4,4,10,20,20,35,10,19,34,10,4,4,4,9,1,1,1,1,1,1,1,1,1,9,4,10,4,1,1,1,1,1,9,9,19,34,33,5,5,11,5,11,5,5,11,21,21,36,11,20,35,10,10,20,35,34,4,4,10,4,10,4,4,1,1,1,1,1,1,1,9,9,4,4,10,4,10,20,10,4,4,1,1,1,1,1,4,1,1,9,19,4,4,10,4,10,4,4,10,20,20,35,10,19,34,10,4,4,4,9,1,1,1,1,1,1,1,1,1,9,4,10,4,1,1,1,1,1,9,9,19,34,33,5,5,11,5,11,5,5,11,21,21,36,11,20,35,11,5,5,5,10,2,2,2,2,2,2,2,2,2,10,5,11,5,2,2,2,2,2,10,10,20,35,34,1,1,1,1,1,1,1,1,1,1,1,1,1,1,9,9,4,4,4,1,1,1,4,4,1,1,1,9,19,10,4,9,1,1,9,4,10,4,1,1,1,1,1,9,9,19,34,33,5,5,11,5,11,5,5,11,21,21,36,11,20,35,11,5,5,5,10,2,2,2,2,2,2,2,2,2,10,5,11,5,2,2,2,2,2,10,10,20,35,34,11,11,21,21,36,11,11,5,5,5,5,11,5,10,20,35,11,11,21,21,36,20,35,11,10,10,11,10,10,20,35,34,4,4,10,4,10,4,4,10,20,20,35,10,19,34,10,4,4,4,9,1,1,1,1,1,1,1,1,1,9,4,10,4,1,1,1,1,1,9,9,19,34,33,4,10,4,10,4,19,34,9,1,1,1,1,9,1,1,1,9,9,33,4,4,10,4,10,4,4,10,20,20,35,10,19,34,10,4,4,4,9,1,1,1,1,1,1,1,1,1,9,4,10,4,1,1,1,1,1,9,9,19,34,33},{9,19,16,34,31,19,9,1,1,1,1,1,4,4,1,1,1,1,4,10,20,35,20,10,10,4,4,4,4,1,1,1,19,10,34,4,4,16,31,1,1,4,9,19,16,34,31,19,9,1,1,1,1,1,4,4,16,31,1,1,4,10,20,17,35,32,20,10,11,21,36,21,11,20,35,17,32,9,19,16,9,1,1,1,1,1,10,20,10,4,4,1,19,4,16,1,9,19,16,34,31,19,9,1,1,1,1,1,4,4,1,1,1,1,4,10,20,35,20,10,10,4,4,4,4,1,1,1,19,10,34,4,4,16,31,1,1,4,9,19,16,34,31,19,9,1,1,1,1,1,4,4,1,1,1,1,4,10,20,35,20,10,10,4,4,4,4,1,1,1,19,10,34,4,4,16,31,1,1,4,11,21,36,21,11,11,5,5,5,20,11,35,5,5,17,32,1,1,1,4,4,4,1,1,19,4,4,16,1,1,1,4,10,4,4,1,1,1,19,10,34,4,4,16,31,1,1,4,10,20,17,35,32,20,10,2,2,2,2,2,5,5,2,2,2,2,5,11,21,36,21,11,11,5,5,5,5,2,2,2,20,11,35,5,5,17,32,2,2,5,16,1,1,1,1,1,1,1,1,1,1,1,1,16,1,1,16,31,1,1,1,4,4,1,4,4,1,16,31,1,1,4,10,20,17,35,32,20,10,2,2,2,2,2,5,5,2,2,2,2,5,11,21,36,21,11,11,5,5,5,5,2,2,2,20,11,35,5,5,17,32,2,2,5,20,17,35,32,20,5,5,5,21,36,21,11,5,20,11,35,17,32,5,9,19,16,34,31,19,9,1,1,1,1,1,4,4,1,1,1,1,4,10,20,35,20,10,10,4,4,4,4,1,1,1,19,10,34,4,4,16,31,1,1,4},{31,25,25,1,1,1,16,34,31,4,4,19,1,1,4,10,20,35,1,31,25,1,1,1,16,9,19,34,1,34,4,10,25,1,1,9,33,25,1,31,4,1,31,25,25,1,1,1,16,34,31,4,4,19,1,1,25,1,31,4,1,31,25,25,1,1,1,16,31,25,1,1,16,25,1,25,1,32,26,26,17,35,32,20,21,36,32,26,17,20,35,35,26,34,26,32,31,25,25,1,1,1,16,34,31,4,4,19,1,1,4,10,20,35,1,31,25,1,1,1,16,9,19,34,1,34,4,10,25,1,1,9,33,25,1,31,4,1,31,25,25,1,1,1,16,34,31,4,4,19,1,1,4,10,20,35,1,31,25,1,1,1,16,9,19,34,1,34,4,10,25,1,1,9,33,25,1,31,4,1,31,25,1,1,1,16,9,19,34,25,1,1,9,33,25,1,11,21,36,10,20,35,35,11,26,10,34,26,32,4,10,1,1,9,1,34,4,10,25,1,1,9,33,25,1,31,4,1,31,25,25,1,1,1,16,34,31,4,4,19,1,1,4,10,20,35,1,31,25,1,1,1,16,9,19,34,1,34,4,10,25,1,1,9,33,25,1,31,4,1,26,35,32,5,5,20,5,11,21,36,35,5,11,26,32,5,25,1,31,4,4,1,1,4,1,1,4,25,1,31,4,1,32,26,26,2,2,2,17,35,32,5,5,20,2,2,5,11,21,36,2,32,26,2,2,2,17,10,20,35,2,35,5,11,26,2,2,10,34,26,2,32,5,2,25,25,1,1,1,1,1,1,25,1,1,1,1,25,1,1,25,1,1,32,26,26,2,2,2,17,35,32,5,5,20,2,2,5,11,21,36,2,32,26,2,2,2,17,10,20,35,2,35,5,11,26,2,2,10,34,26,2,32,5,2},{15,15,15,25,25,31,25,15,15,33,30,25,34,31,34,31,25,15,35,15,15,25,31,34,25,31,25,15,34,15,33,30,15,33,25,30,15,15,25,15,30,31,15,15,15,25,25,31,25,15,15,33,30,25,34,31,15,25,15,30,31,15,15,15,25,25,31,25,15,15,25,31,25,15,25,15,25,15,15,15,25,15,15,25,25,15,15,15,25,25,15,15,15,15,15,15,16,16,16,26,26,32,26,16,16,34,31,26,35,32,35,32,26,16,36,16,16,26,32,35,26,32,26,16,35,16,34,31,16,34,26,31,16,16,26,16,31,32,15,15,15,25,25,31,25,15,15,33,30,25,34,31,34,31,25,15,35,15,15,25,31,34,25,31,25,15,34,15,33,30,15,33,25,30,15,15,25,15,30,31,15,15,25,31,34,25,31,25,15,15,33,25,30,15,15,25,31,25,15,31,25,15,15,30,15,30,15,15,15,35,32,36,35,32,35,16,34,31,16,34,26,31,16,16,26,16,31,32,15,15,15,25,25,31,25,15,15,33,30,25,34,31,34,31,25,15,35,15,15,25,31,34,25,31,25,15,34,15,33,30,15,33,25,30,15,15,25,15,30,31,15,15,15,33,30,25,34,31,25,15,15,33,30,15,15,30,16,26,16,34,31,35,32,35,36,35,34,16,26,16,31,32,15,15,15,25,25,31,25,15,15,33,30,25,34,31,34,31,25,15,35,15,15,25,31,34,25,31,25,15,34,15,33,30,15,33,25,30,15,15,25,15,30,31,16,16,26,26,32,35,32,36,16,26,32,35,35,16,34,26,16,26,32,16,16,16,26,26,32,26,16,16,34,31,26,35,32,35,32,26,16,36,16,16,26,32,35,26,32,26,16,35,16,34,31,16,34,26,31,16,16,26,16,31,32}};
raysdesired = map(QQ^6, QQ^0, 0);
linealitydesired = map(QQ^6, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 18  - 6 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,0},{-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,0},{-1,-1,1,1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,0},{-1,1,-1,1,-1,1,1,-1,1,-1,1,-1,1,-1,1,-1,0},{1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0},{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5}};
raysP = matrix {{0},{0},{0},{0},{-1},{0}};
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{-5,0,6,0,5,-5},{6,-5,0,5,-5,0},{0,6,5,-5,0,-5},{5,0,-5,0,-5,6},{-5,5,0,-5,6,0},{0,-5,-5,6,0,5}};
raysQ = map(QQ^6, QQ^0, 0);
linealityQ = map(QQ^6, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{1,7,1,6,-4,-4,1,7,1,6,-4,1,7,6,-4,1,7,1,6,-4,-4,1,7,1,6,-4,-4,1,7,1,6,-4,-4,1,7,1,6,-4,-4,1,7,1,6,-4,-6,-1,5,-1,4,-6,-6,-1,5,-1,4,-6,-6,-1,5,-1,4,-6,-6,-1,5,-1,4,-6,-6,-1,4,-6,-6,-1,5,-1,4,-6,-6,-1,5,-1,4,-6,-6,-1,5,-1,4,-6,-5,0,6,0,5,-5},{-6,-1,4,-6,-1,5,-6,-1,4,-6,-1,-6,-1,-6,5,-6,-1,4,-6,-1,7,-4,1,6,-4,1,7,-4,1,6,-4,1,7,-4,1,6,-4,1,7,-4,1,6,-4,1,7,-4,1,6,-4,1,7,-4,1,6,-4,1,7,-4,1,6,-4,1,7,-4,1,6,-4,1,5,-6,-6,-1,5,-6,-1,4,-6,-1,5,-6,-1,4,-6,-1,5,-6,-1,4,-6,-1,6,-5,0,5,-5,0},{5,4,-6,-1,-6,-1,5,4,-6,-1,-6,7,6,1,1,7,6,-4,1,-4,1,7,6,-4,1,-4,1,7,6,-4,1,-4,-1,5,4,-6,-1,-6,-1,5,4,-6,-1,-6,1,7,6,-4,1,-4,1,7,6,-4,1,-4,-1,5,4,-6,-1,-6,-1,5,4,-6,-1,-6,1,7,1,-4,1,7,6,-4,1,-4,-1,5,4,-6,-1,-6,-1,5,4,-6,-1,-6,0,6,5,-5,0,-5},{-1,-6,-1,-6,5,6,1,-4,1,-4,7,-1,-6,-6,6,1,-4,1,-4,7,4,-1,-6,-1,-6,5,6,1,-4,1,-4,7,6,1,-4,1,-4,7,4,-1,-6,-1,-6,5,6,1,-4,1,-4,7,4,-1,-6,-1,-6,5,6,1,-4,1,-4,7,4,-1,-6,-1,-6,5,6,1,-4,7,4,-1,-6,-1,-6,5,6,1,-4,1,-4,7,4,-1,-6,-1,-6,5,5,0,-5,0,-5,6},{6,1,-4,7,1,-4,6,1,-4,7,1,6,1,7,-4,6,1,-4,7,1,-4,6,1,-4,7,1,-4,6,1,-4,7,1,-4,6,1,-4,7,1,-4,6,1,-4,7,1,-4,6,1,-4,7,1,-4,6,1,-4,7,1,-4,6,1,-4,7,1,-4,6,1,-4,7,1,-4,6,7,1,-4,6,1,-4,7,1,-4,6,1,-4,7,1,-4,6,1,-4,7,1,-5,5,0,-5,6,0},{-5,-5,6,0,5,0,-5,-5,6,0,5,-5,-5,0,0,-5,-5,6,0,5,0,-5,-5,6,0,5,0,-5,-5,6,0,5,0,-5,-5,6,0,5,0,-5,-5,6,0,5,0,-5,-5,6,0,5,0,-5,-5,6,0,5,0,-5,-5,6,0,5,0,-5,-5,6,0,5,0,-5,0,5,0,-5,-5,6,0,5,0,-5,-5,6,0,5,0,-5,-5,6,0,5,5,0,0,11,5,10}};
raysdesired = matrix {{0},{0},{0},{0},{-1},{0}};
linealitydesired = map(QQ^6, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 64 1 - 8 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{-1,1,-1,1,0,0,0,0},{-1,-1,1,1,0,0,0,0},{-1,-1,-1,-1,1,1,1,1},{0,0,0,0,0,1,0,0},{0,0,0,0,0,0,1,0},{0,0,0,0,0,0,0,1}};
raysQ = map(QQ^6, QQ^0, 0);
linealityQ = map(QQ^6, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-2,2,-2,2,-2,-1,2,1,-2,-1,2,1,-2,-1,2,1,-2,-1,2,1,-2,-1,2,1,-2,-1,2,1,-2,-1,2,1,-2,-1,2,1,-2,-1,2,1,-2,-1,2,1,-2,-1,-1,2,1,1,-2,-1,-1,2,1,1,-2,-1,-1,2,1,1,-2,-1,-1,2,1,1,-2,-1,2,1,-2,-1,2,1,-2,-1,2,1,-2,-1,2,1,-2,-1,-1,2,1,1,-2,-1,-1,2,1,1,-2,-1,-1,2,1,1,-2,-1,-1,2,1,1,-2,-1,-1,2,1,1,-2,-1,-1,2,1,1,-2,-1,-1,2,1,1,-2,-1,-1,2,1,1,-2,-1,-1,-1,2,1,1,1,-2,-1,-1,-1,2,1,1,1,-2,-1,-1,-1,2,1,1,1,-2,-1,-1,-1,2,1,1,1},{-2,-2,2,2,-2,-1,-2,-1,2,1,2,1,-2,-1,-2,-1,2,1,2,1,-2,-1,-2,-1,2,1,2,1,-2,-1,-2,-1,2,1,2,1,-2,-1,-2,-1,2,1,2,1,-2,-1,-1,-2,-1,-1,2,1,1,2,1,1,-2,-1,-1,-2,-1,-1,2,1,1,2,1,1,-2,-1,-2,-1,2,1,2,1,-2,-1,-2,-1,2,1,2,1,-2,-1,-1,-2,-1,-1,2,1,1,2,1,1,-2,-1,-1,-2,-1,-1,2,1,1,2,1,1,-2,-1,-1,-2,-1,-1,2,1,1,2,1,1,-2,-1,-1,-2,-1,-1,2,1,1,2,1,1,-2,-1,-1,-1,-2,-1,-1,-1,2,1,1,1,2,1,1,1,-2,-1,-1,-1,-2,-1,-1,-1,2,1,1,1,2,1,1,1},{-2,-2,-2,-2,0,2,0,2,0,2,0,2,-2,0,-2,0,-2,0,-2,0,0,2,0,2,0,2,0,2,-2,0,-2,0,-2,0,-2,0,0,2,0,2,0,2,0,2,-2,0,0,-2,0,0,-2,0,0,-2,0,0,0,2,2,0,2,2,0,2,2,0,2,2,-2,0,-2,0,-2,0,-2,0,0,2,0,2,0,2,0,2,-2,0,0,-2,0,0,-2,0,0,-2,0,0,0,2,2,0,2,2,0,2,2,0,2,2,-2,0,0,-2,0,0,-2,0,0,-2,0,0,0,2,2,0,2,2,0,2,2,0,2,2,-2,0,0,0,-2,0,0,0,-2,0,0,0,-2,0,0,0,0,2,2,2,0,2,2,2,0,2,2,2,0,2,2,2},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2}};
raysdesired = map(QQ^6, QQ^0, 0);
linealitydesired = map(QQ^6, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 6 1 - 18 
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-5,0,6,0,5,-5},{6,-5,0,5,-5,0},{0,6,5,-5,0,-5},{5,0,-5,0,-5,6},{-5,5,0,-5,6,0},{0,-5,-5,6,0,5}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,0},{-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,0},{-1,-1,1,1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,0},{-1,1,-1,1,-1,1,1,-1,1,-1,1,-1,1,-1,1,-1,0},{1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0},{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5}};
raysQ = matrix {{0},{0},{0},{0},{-1},{0}};
linealityQ = map(QQ^6, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-4,-4,-4,-4,-4,-4,-6,-6,-6,-6,-6,-6,-6,-6,-5,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,0,7,7,7,7,7,7,7,7,5,5,5,5,5,5,5,6,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,0,6,6,6,6,6,6,6,6,4,4,4,4,4,4,4,4,5,-4,-4,-4,-4,-4,-4,-4,-6,-6,-6,-6,-6,-6,-6,-6,-5},{5,5,7,7,7,7,7,7,7,7,5,5,5,5,6,-6,-6,-6,-6,-4,-4,-4,-4,-4,-4,-4,-4,-6,-6,-6,-6,-5,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,0,4,4,4,6,6,6,6,6,6,6,6,4,4,4,5,-6,-6,-6,-6,-4,-4,-4,-4,-4,-4,-4,-4,-6,-6,-6,-6,-5,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,0},{-1,1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,0,5,5,7,7,7,7,5,5,7,7,5,5,7,7,5,5,6,4,4,6,6,6,6,4,4,6,6,4,4,6,4,4,5,-6,-6,-4,-4,-4,-6,-6,-4,-4,-6,-6,-4,-6,-6,-5,-1,-1,1,1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,0,-6,-6,-4,-4,-4,-6,-6,-4,-4,-6,-6,-4,-4,-6,-6,-5},{6,6,4,6,6,4,6,4,6,4,6,4,6,4,5,-1,1,-1,1,-1,1,1,-1,1,-1,1,-1,1,-1,1,-1,0,-6,-4,-6,-4,-6,-4,-4,-6,-4,-6,-4,-6,-6,-4,-6,-5,-1,1,1,-1,1,1,-1,1,-1,1,-1,-1,1,-1,0,-6,-4,-6,-4,-6,-4,-4,-6,-4,-6,-4,-6,-4,-6,-4,-6,-5,5,7,7,5,7,7,5,7,5,7,5,7,5,7,5,6},{-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-5,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,6,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0},{0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,-5,-5,-5,-5,-5,-5,-5,-5,-5,-5,-5,-5,-5,-5,-5,-5,0,-5,-5,-5,-5,-5,-5,-5,-5,-5,-5,-5,-5,-5,-5,-5,0,6,6,6,6,6,6,6,6,6,6,6,6,6,6,11,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,10}};
raysdesired = matrix {{0},{0},{0},{0},{-1},{0}};
linealitydesired = map(QQ^6, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 18  - 18 
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,0},{-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,0},{-1,-1,1,1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,0},{-1,1,-1,1,-1,1,1,-1,1,-1,1,-1,1,-1,1,-1,0},{1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0},{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5}};
raysP = matrix {{0},{0},{0},{0},{-1},{0}};
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,0},{-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,0},{-1,-1,1,1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,0},{-1,1,-1,1,-1,1,1,-1,1,-1,1,-1,1,-1,1,-1,0},{1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0},{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5}};
raysQ = matrix {{0},{0},{0},{0},{-1},{0}};
linealityQ = map(QQ^6, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{2,2,2,2,2,2,2,2,-2,-2,-2,-2,-2,-2,-2,-2,0},{-2,-2,-2,-2,2,2,2,2,2,2,2,2,-2,-2,-2,-2,0},{-2,-2,2,2,2,2,-2,-2,2,2,-2,-2,2,2,-2,-2,0},{-2,2,-2,2,-2,2,2,-2,2,-2,2,-2,2,-2,2,-2,0},{2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0},{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10}};
raysdesired = matrix {{0},{0},{0},{0},{-1},{0}};
linealitydesired = map(QQ^6, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 64 1 - 6 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{-5,-5,0,0,5,6},{0,6,-5,5,-5,0},{-5,0,6,-5,0,5},{6,5,0,0,-5,-5},{0,-5,5,-5,6,0},{5,0,-5,6,0,-5}};
raysQ = map(QQ^6, QQ^0, 0);
linealityQ = map(QQ^6, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-6,-6,-1,-1,4,5,-4,-4,1,1,6,7,-6,-6,-1,-1,4,5,-4,-4,1,6,7,-6,-6,-1,-1,4,5,-4,1,1,6,7,-6,-1,-1,4,5,-4,1,1,7,-6,-6,-1,-1,4,5,-4,-4,1,1,6,7,-6,-6,-1,-1,5,-4,-4,1,1,6,7,-6,-6,-1,-1,5,-4,-4,1,1,6,7,-6,-6,-1,5,-4,-4,1,1,7,-6,-6,-1,-1,4,5,-4,1,1,6,7,-6,-6,-1,-1,4,5,-4,-4,1,1,6,7,-6,-6,-1,4,5,1,6,7,-6,-6,-1,-1,4,5,-4,1,1,6,7,-6,-6,-1,4,5,-4,-4,1,1,6,7,-6,-6,-1,-1,4,5,-4,-4,1,1,6,7,-6,-6,-1,4,-4,-4,1,6,7,-6,-6,-1,4,5,-4,-4,1,1,6,7,-6,-6,-1,-1,4,5,-4,1,1,6,7,-6,-6,-1,4,5,-4,1,6,7,-6,-6,-1,-1,4,5,-4,-4,1,1,6,7,-6,-6,-1,-1,4,5,-4,1,1,6,7,-6,-6,-1,-1,4,-4,-4,1,1,6,7,-6,-6,-1,-4,-4,1,6,7,-6,-6,-1,-1,4,5,-4,-4,1,1,6,7,-6,-6,-1,-1,5,-4,-4,1,1,6,7,-6,-6,-1,-1,4,-4,1,6,7,-6,-6,-1,-1,4,5,-4,-4,1,6,7,-6,-6,-1,-1,4,5,-4,1,1,6,7,-6,-6,-1,-1,4,5,-4,-4,1,1,6,7,-6,-1,-1,4,-4,1,1,6,7,-6,-6,-1,-1,4,-4,-4,1,1,6,7,-6,-6,-1,-1,4,-4,-4,1,1,6,7,-6,-6,-1,-1,4,5,-4,-4,1,1,6,7},{-1,5,-6,4,-6,-1,-1,5,-6,4,-6,-1,1,7,-4,6,-4,1,1,7,6,-4,1,-1,5,-6,4,-6,-1,5,-6,4,-6,-1,7,-4,6,-4,1,7,-4,6,1,-1,5,-6,4,-6,-1,-1,5,-6,4,-6,-1,1,7,-4,6,1,1,7,-4,6,-4,1,-1,5,-6,4,-1,-1,5,-6,4,-6,-1,1,7,-4,1,1,7,-4,6,1,-1,5,-6,4,-6,-1,-1,-6,4,-6,-1,1,7,-4,6,-4,1,1,7,-4,6,-4,1,-1,5,-6,-6,-1,-6,-6,-1,1,7,-4,6,-4,1,7,-4,6,-4,1,-1,5,-6,-6,-1,-1,5,-6,4,-6,-1,1,7,-4,6,-4,1,1,7,-4,6,-4,1,-1,5,-6,-6,-1,5,-6,-6,-1,1,7,-4,-4,1,1,7,-4,6,-4,1,-1,5,-6,4,-6,-1,-1,-6,4,-6,-1,1,7,6,-4,1,7,6,-4,1,-1,5,-6,4,-6,-1,-1,5,-6,4,-6,-1,1,7,-4,6,-4,1,7,-4,6,-4,1,-1,5,-6,4,-6,-1,5,-6,4,-6,-1,1,7,6,1,7,6,-4,1,-1,5,-6,4,-6,-1,-1,5,-6,4,-6,-1,1,7,-4,6,1,1,7,-4,6,-4,1,-1,5,-6,4,-6,-1,4,-6,-1,1,7,-4,6,-4,1,1,7,6,-4,1,-1,5,-6,4,-6,-1,-1,-6,4,-6,-1,1,7,-4,6,-4,1,1,7,-4,6,-4,1,-1,-6,4,-6,-1,-6,4,-6,-1,1,7,-4,6,-4,1,7,-4,6,-4,1,-1,5,-6,4,-6,-1,5,-6,4,-6,-1,1,7,-4,6,-4,1,1,7,-4,6,-4,1},{-6,-1,5,-6,-1,4,-6,-1,5,-6,-1,4,-6,-1,5,-6,-1,4,-6,-1,-6,-1,4,-4,1,7,-4,1,6,1,7,-4,1,6,1,7,-4,1,6,1,7,-4,6,-6,-1,5,-6,-1,4,-6,-1,5,-6,-1,4,-6,-1,5,-6,4,-6,-1,5,-6,-1,4,-4,1,7,-4,6,-4,1,7,-4,1,6,-4,1,7,6,-4,1,7,-4,6,-6,-1,5,-6,-1,4,-6,5,-6,-1,4,-6,-1,5,-6,-1,4,-6,-1,5,-6,-1,4,-4,1,7,1,6,7,1,6,-4,1,7,-4,1,6,1,7,-4,1,6,-6,-1,5,-1,4,-6,-1,5,-6,-1,4,-6,-1,5,-6,-1,4,-6,-1,5,-6,-1,4,-4,1,7,1,-4,1,7,1,6,-4,1,7,1,6,-4,1,7,-4,1,6,-6,-1,5,-6,-1,4,-6,5,-6,-1,4,-6,-1,-6,-1,4,-1,-6,-1,4,-4,1,7,-4,1,6,-4,1,7,-4,1,6,-4,1,7,-4,1,6,1,7,-4,1,6,-6,-1,5,-6,-1,-6,-1,5,-6,-1,4,-6,-1,-6,-6,-1,-6,-1,4,-4,1,7,-4,1,6,-4,1,7,-4,1,6,-4,1,7,-4,6,-4,1,7,-4,1,6,-6,-1,5,-6,-1,-6,-6,-1,4,-6,-1,5,-6,-1,4,-6,-1,-6,-1,4,-4,1,7,-4,1,6,-4,7,-4,1,6,-4,1,7,-4,1,6,-4,1,7,-4,1,6,-6,5,-6,-1,-6,5,-6,-1,4,-6,-1,5,-6,-1,-6,-1,5,-6,-1,4,-4,1,7,-4,1,-4,1,7,-4,1,6,-4,1,7,-4,1,6,-4,1,7,-4,1,6},{5,4,-1,-1,-6,-6,5,4,-1,-1,-6,-6,5,4,-1,-1,-6,-6,5,4,-1,-6,-6,5,4,-1,-1,-6,-6,4,-1,-1,-6,-6,4,-1,-1,-6,-6,4,-1,-1,-6,7,6,1,1,-4,-4,7,6,1,1,-4,-4,7,6,1,1,-4,7,6,1,1,-4,-4,7,6,1,1,-4,7,6,1,1,-4,-4,7,6,1,-4,7,6,1,1,-4,5,4,-1,-1,-6,-6,5,-1,-1,-6,-6,5,4,-1,-1,-6,-6,5,4,-1,-1,-6,-6,5,4,-1,-6,-6,-1,-6,-6,5,4,-1,-1,-6,-6,4,-1,-1,-6,-6,7,6,1,-4,-4,7,6,1,1,-4,-4,7,6,1,1,-4,-4,7,6,1,1,-4,-4,7,6,1,-4,7,6,1,-4,-4,7,6,1,-4,-4,7,6,1,1,-4,-4,5,4,-1,-1,-6,-6,5,-1,-1,-6,-6,5,4,-1,-6,-6,4,-1,-6,-6,5,4,-1,-1,-6,-6,5,4,-1,-1,-6,-6,5,4,-1,-1,-6,-6,4,-1,-1,-6,-6,7,6,1,1,-4,7,6,1,1,-4,-4,7,6,1,7,6,1,-4,-4,7,6,1,1,-4,-4,7,6,1,1,-4,-4,7,6,1,1,-4,7,6,1,1,-4,-4,5,4,-1,-1,-6,5,-1,-6,-6,5,4,-1,-1,-6,-6,5,4,-1,-6,-6,5,4,-1,-1,-6,-6,5,-1,-1,-6,-6,5,4,-1,-1,-6,-6,5,4,-1,-1,-6,-6,7,1,1,-4,7,1,1,-4,-4,7,6,1,1,-4,7,6,1,1,-4,-4,7,6,1,1,-4,7,6,1,1,-4,-4,7,6,1,1,-4,-4,7,6,1,1,-4,-4},{-1,-6,4,-6,5,-1,-1,-6,4,-6,5,-1,-1,-6,4,-6,5,-1,-1,-6,-6,5,-1,-1,-6,4,-6,5,-1,-6,4,-6,5,-1,-6,4,-6,5,-1,-6,4,-6,-1,-1,-6,4,-6,5,-1,-1,-6,4,-6,5,-1,-1,-6,4,-6,-1,-1,-6,4,-6,5,-1,-1,-6,4,-6,-1,-1,-6,4,-6,5,-1,-1,-6,4,-1,-1,-6,4,-6,-1,1,-4,6,-4,7,1,1,6,-4,7,1,1,-4,6,-4,7,1,1,-4,6,-4,7,1,1,-4,6,7,1,6,7,1,1,-4,6,-4,7,1,-4,6,-4,7,1,1,-4,6,7,1,1,-4,6,-4,7,1,1,-4,6,-4,7,1,1,-4,6,-4,7,1,1,-4,6,7,1,-4,6,7,1,1,-4,6,7,1,1,-4,6,-4,7,1,-1,-6,4,-6,5,-1,-1,4,-6,5,-1,-1,-6,-6,5,-1,-6,-6,5,-1,-1,-6,4,-6,5,-1,-1,-6,4,-6,5,-1,-1,-6,4,-6,5,-1,-6,4,-6,5,-1,-1,-6,4,-6,5,-1,-6,4,-6,5,-1,-1,-6,-6,-1,-6,-6,5,-1,-1,-6,4,-6,5,-1,-1,-6,4,-6,5,-1,-1,-6,4,-6,-1,-1,-6,4,-6,5,-1,1,-4,6,-4,7,1,-4,7,1,1,-4,6,-4,7,1,1,-4,-4,7,1,1,-4,6,-4,7,1,1,6,-4,7,1,1,-4,6,-4,7,1,1,-4,6,-4,7,1,1,6,-4,7,1,6,-4,7,1,1,-4,6,-4,7,1,-4,6,-4,7,1,1,-4,6,-4,7,1,-4,6,-4,7,1,1,-4,6,-4,7,1,1,-4,6,-4,7,1},{4,-1,-6,5,-1,-6,4,-1,-6,5,-1,-6,4,-1,-6,5,-1,-6,4,-1,5,-1,-6,4,-1,-6,5,-1,-6,-1,-6,5,-1,-6,-1,-6,5,-1,-6,-1,-6,5,-6,4,-1,-6,5,-1,-6,4,-1,-6,5,-1,-6,4,-1,-6,5,-6,4,-1,-6,5,-1,-6,4,-1,-6,5,-6,4,-1,-6,5,-1,-6,4,-1,-6,-6,4,-1,-6,5,-6,4,-1,-6,5,-1,-6,4,-6,5,-1,-6,4,-1,-6,5,-1,-6,4,-1,-6,5,-1,-6,4,-1,-6,-1,-6,-6,-1,-6,4,-1,-6,5,-1,-6,-1,-6,5,-1,-6,4,-1,-6,-1,-6,4,-1,-6,5,-1,-6,4,-1,-6,5,-1,-6,4,-1,-6,5,-1,-6,4,-1,-6,-1,4,-1,-6,-1,-6,4,-1,-6,-1,-6,4,-1,-6,5,-1,-6,6,1,-4,7,1,-4,6,-4,7,1,-4,6,1,7,1,-4,1,7,1,-4,6,1,-4,7,1,-4,6,1,-4,7,1,-4,6,1,-4,7,1,-4,1,-4,7,1,-4,6,1,-4,7,1,6,1,-4,7,1,-4,6,1,7,6,1,7,1,-4,6,1,-4,7,1,-4,6,1,-4,7,1,-4,6,1,-4,7,-4,6,1,-4,7,1,-4,6,1,-4,7,1,6,7,1,-4,6,1,-4,7,1,-4,6,1,7,1,-4,6,1,-4,7,1,-4,6,-4,7,1,-4,6,1,-4,7,1,-4,6,1,-4,7,1,-4,6,-4,7,1,6,-4,7,1,-4,6,1,-4,7,1,6,1,-4,7,1,-4,6,1,-4,7,1,6,1,-4,7,1,-4,6,1,-4,7,1,-4,6,1,-4,7,1,-4}};
raysdesired = map(QQ^6, QQ^0, 0);
linealitydesired = map(QQ^6, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 42 1 - 15 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{9,9,7,1,1,3,3,7,9,9,1,1,1,9,3,7,9,5,1,9,9,7,1,1,1,7,9,5,1,5,9,3,9,1,1,9,5,3,9,1,1,9},{16,14,12,12,16,4,4,8,10,10,16,4,4,16,4,8,10,6,4,16,14,12,12,8,16,10,12,8,4,6,10,4,16,8,16,12,8,4,10,16,4,16},{19,17,15,15,19,15,21,15,17,21,21,15,21,21,9,11,13,9,9,17,15,13,13,9,17,11,13,9,9,9,13,9,17,9,17,13,9,21,21,21,21,21},{20,18,16,16,20,16,22,16,18,22,22,16,22,22,16,16,18,16,16,20,18,16,16,16,20,16,18,16,24,24,24,24,24,24,24,24,24,24,24,24,24,24},{25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25}};
raysP = map(QQ^6, QQ^0, 0);
linealityP = map(QQ^6, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,1,2,3,4,5,6,7,8,9,10,11,12,13,14},{0,1,4,9,16,25,36,49,64,81,100,121,144,169,196},{0,1,8,27,64,125,216,343,512,729,1000,1331,1728,2197,2744},{0,1,16,81,256,625,1296,2401,4096,6561,10000,14641,20736,28561,38416},{0,1,32,243,1024,3125,7776,16807,32768,59049,100000,161051,248832,371293,537824},{0,1,64,729,4096,15625,46656,117649,262144,531441,1000000,1771561,2985984,4826809,7529536}};
raysQ = map(QQ^6, QQ^0, 0);
linealityQ = map(QQ^6, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14},{9,10,13,18,25,34,45,58,73,90,109,130,153,178,205,9,10,13,18,25,34,45,58,73,90,109,130,153,178,205,7,8,11,16,23,32,43,56,71,88,107,128,151,176,203,1,2,5,10,17,26,37,50,65,82,101,122,145,170,197,1,2,5,10,17,26,37,50,65,82,101,122,145,170,197,3,4,7,12,19,28,39,52,67,84,103,124,147,172,199,3,4,7,12,19,28,39,52,67,84,103,124,147,172,199,7,8,11,16,23,32,43,56,71,88,107,128,151,176,203,9,10,13,18,25,34,45,58,73,90,109,130,153,178,205,9,10,13,18,25,34,45,58,73,90,109,130,153,178,205,1,2,5,10,17,26,37,50,65,82,101,122,145,170,197,1,2,5,10,17,26,37,50,65,82,101,122,145,170,197,1,2,5,10,17,26,37,50,65,82,101,122,145,170,197,9,10,13,18,25,34,45,58,73,90,109,130,153,178,205,3,4,7,12,19,28,39,52,67,84,103,124,147,172,199,7,8,11,16,23,32,43,56,71,88,107,128,151,176,203,9,10,13,18,25,34,45,58,73,90,109,130,153,178,205,5,6,9,14,21,30,41,54,69,86,105,126,149,174,201,1,2,5,10,17,26,37,50,65,82,101,122,145,170,197,9,10,13,18,25,34,45,58,73,90,109,130,153,178,205,9,10,13,18,25,34,45,58,73,90,109,130,153,178,205,7,8,11,16,23,32,43,56,71,88,107,128,151,176,203,1,2,5,10,17,26,37,50,65,82,101,122,145,170,197,1,2,5,10,17,26,37,50,65,82,101,122,145,170,197,1,2,5,10,17,26,37,50,65,82,101,122,145,170,197,7,8,11,16,23,32,43,56,71,88,107,128,151,176,203,9,10,13,18,25,34,45,58,73,90,109,130,153,178,205,5,6,9,14,21,30,41,54,69,86,105,126,149,174,201,1,2,5,10,17,26,37,50,65,82,101,122,145,170,197,5,6,9,14,21,30,41,54,69,86,105,126,149,174,201,9,10,13,18,25,34,45,58,73,90,109,130,153,178,205,3,4,7,12,19,28,39,52,67,84,103,124,147,172,199,9,10,13,18,25,34,45,58,73,90,109,130,153,178,205,1,2,5,10,17,26,37,50,65,82,101,122,145,170,197,1,2,5,10,17,26,37,50,65,82,101,122,145,170,197,9,10,13,18,25,34,45,58,73,90,109,130,153,178,205,5,6,9,14,21,30,41,54,69,86,105,126,149,174,201,3,4,7,12,19,28,39,52,67,84,103,124,147,172,199,9,10,13,18,25,34,45,58,73,90,109,130,153,178,205,1,2,5,10,17,26,37,50,65,82,101,122,145,170,197,1,2,5,10,17,26,37,50,65,82,101,122,145,170,197,9,10,13,18,25,34,45,58,73,90,109,130,153,178,205},{16,17,24,43,80,141,232,359,528,745,1016,1347,1744,2213,2760,14,15,22,41,78,139,230,357,526,743,1014,1345,1742,2211,2758,12,13,20,39,76,137,228,355,524,741,1012,1343,1740,2209,2756,12,13,20,39,76,137,228,355,524,741,1012,1343,1740,2209,2756,16,17,24,43,80,141,232,359,528,745,1016,1347,1744,2213,2760,4,5,12,31,68,129,220,347,516,733,1004,1335,1732,2201,2748,4,5,12,31,68,129,220,347,516,733,1004,1335,1732,2201,2748,8,9,16,35,72,133,224,351,520,737,1008,1339,1736,2205,2752,10,11,18,37,74,135,226,353,522,739,1010,1341,1738,2207,2754,10,11,18,37,74,135,226,353,522,739,1010,1341,1738,2207,2754,16,17,24,43,80,141,232,359,528,745,1016,1347,1744,2213,2760,4,5,12,31,68,129,220,347,516,733,1004,1335,1732,2201,2748,4,5,12,31,68,129,220,347,516,733,1004,1335,1732,2201,2748,16,17,24,43,80,141,232,359,528,745,1016,1347,1744,2213,2760,4,5,12,31,68,129,220,347,516,733,1004,1335,1732,2201,2748,8,9,16,35,72,133,224,351,520,737,1008,1339,1736,2205,2752,10,11,18,37,74,135,226,353,522,739,1010,1341,1738,2207,2754,6,7,14,33,70,131,222,349,518,735,1006,1337,1734,2203,2750,4,5,12,31,68,129,220,347,516,733,1004,1335,1732,2201,2748,16,17,24,43,80,141,232,359,528,745,1016,1347,1744,2213,2760,14,15,22,41,78,139,230,357,526,743,1014,1345,1742,2211,2758,12,13,20,39,76,137,228,355,524,741,1012,1343,1740,2209,2756,12,13,20,39,76,137,228,355,524,741,1012,1343,1740,2209,2756,8,9,16,35,72,133,224,351,520,737,1008,1339,1736,2205,2752,16,17,24,43,80,141,232,359,528,745,1016,1347,1744,2213,2760,10,11,18,37,74,135,226,353,522,739,1010,1341,1738,2207,2754,12,13,20,39,76,137,228,355,524,741,1012,1343,1740,2209,2756,8,9,16,35,72,133,224,351,520,737,1008,1339,1736,2205,2752,4,5,12,31,68,129,220,347,516,733,1004,1335,1732,2201,2748,6,7,14,33,70,131,222,349,518,735,1006,1337,1734,2203,2750,10,11,18,37,74,135,226,353,522,739,1010,1341,1738,2207,2754,4,5,12,31,68,129,220,347,516,733,1004,1335,1732,2201,2748,16,17,24,43,80,141,232,359,528,745,1016,1347,1744,2213,2760,8,9,16,35,72,133,224,351,520,737,1008,1339,1736,2205,2752,16,17,24,43,80,141,232,359,528,745,1016,1347,1744,2213,2760,12,13,20,39,76,137,228,355,524,741,1012,1343,1740,2209,2756,8,9,16,35,72,133,224,351,520,737,1008,1339,1736,2205,2752,4,5,12,31,68,129,220,347,516,733,1004,1335,1732,2201,2748,10,11,18,37,74,135,226,353,522,739,1010,1341,1738,2207,2754,16,17,24,43,80,141,232,359,528,745,1016,1347,1744,2213,2760,4,5,12,31,68,129,220,347,516,733,1004,1335,1732,2201,2748,16,17,24,43,80,141,232,359,528,745,1016,1347,1744,2213,2760},{19,20,35,100,275,644,1315,2420,4115,6580,10019,14660,20755,28580,38435,17,18,33,98,273,642,1313,2418,4113,6578,10017,14658,20753,28578,38433,15,16,31,96,271,640,1311,2416,4111,6576,10015,14656,20751,28576,38431,15,16,31,96,271,640,1311,2416,4111,6576,10015,14656,20751,28576,38431,19,20,35,100,275,644,1315,2420,4115,6580,10019,14660,20755,28580,38435,15,16,31,96,271,640,1311,2416,4111,6576,10015,14656,20751,28576,38431,21,22,37,102,277,646,1317,2422,4117,6582,10021,14662,20757,28582,38437,15,16,31,96,271,640,1311,2416,4111,6576,10015,14656,20751,28576,38431,17,18,33,98,273,642,1313,2418,4113,6578,10017,14658,20753,28578,38433,21,22,37,102,277,646,1317,2422,4117,6582,10021,14662,20757,28582,38437,21,22,37,102,277,646,1317,2422,4117,6582,10021,14662,20757,28582,38437,15,16,31,96,271,640,1311,2416,4111,6576,10015,14656,20751,28576,38431,21,22,37,102,277,646,1317,2422,4117,6582,10021,14662,20757,28582,38437,21,22,37,102,277,646,1317,2422,4117,6582,10021,14662,20757,28582,38437,9,10,25,90,265,634,1305,2410,4105,6570,10009,14650,20745,28570,38425,11,12,27,92,267,636,1307,2412,4107,6572,10011,14652,20747,28572,38427,13,14,29,94,269,638,1309,2414,4109,6574,10013,14654,20749,28574,38429,9,10,25,90,265,634,1305,2410,4105,6570,10009,14650,20745,28570,38425,9,10,25,90,265,634,1305,2410,4105,6570,10009,14650,20745,28570,38425,17,18,33,98,273,642,1313,2418,4113,6578,10017,14658,20753,28578,38433,15,16,31,96,271,640,1311,2416,4111,6576,10015,14656,20751,28576,38431,13,14,29,94,269,638,1309,2414,4109,6574,10013,14654,20749,28574,38429,13,14,29,94,269,638,1309,2414,4109,6574,10013,14654,20749,28574,38429,9,10,25,90,265,634,1305,2410,4105,6570,10009,14650,20745,28570,38425,17,18,33,98,273,642,1313,2418,4113,6578,10017,14658,20753,28578,38433,11,12,27,92,267,636,1307,2412,4107,6572,10011,14652,20747,28572,38427,13,14,29,94,269,638,1309,2414,4109,6574,10013,14654,20749,28574,38429,9,10,25,90,265,634,1305,2410,4105,6570,10009,14650,20745,28570,38425,9,10,25,90,265,634,1305,2410,4105,6570,10009,14650,20745,28570,38425,9,10,25,90,265,634,1305,2410,4105,6570,10009,14650,20745,28570,38425,13,14,29,94,269,638,1309,2414,4109,6574,10013,14654,20749,28574,38429,9,10,25,90,265,634,1305,2410,4105,6570,10009,14650,20745,28570,38425,17,18,33,98,273,642,1313,2418,4113,6578,10017,14658,20753,28578,38433,9,10,25,90,265,634,1305,2410,4105,6570,10009,14650,20745,28570,38425,17,18,33,98,273,642,1313,2418,4113,6578,10017,14658,20753,28578,38433,13,14,29,94,269,638,1309,2414,4109,6574,10013,14654,20749,28574,38429,9,10,25,90,265,634,1305,2410,4105,6570,10009,14650,20745,28570,38425,21,22,37,102,277,646,1317,2422,4117,6582,10021,14662,20757,28582,38437,21,22,37,102,277,646,1317,2422,4117,6582,10021,14662,20757,28582,38437,21,22,37,102,277,646,1317,2422,4117,6582,10021,14662,20757,28582,38437,21,22,37,102,277,646,1317,2422,4117,6582,10021,14662,20757,28582,38437,21,22,37,102,277,646,1317,2422,4117,6582,10021,14662,20757,28582,38437},{20,21,52,263,1044,3145,7796,16827,32788,59069,100020,161071,248852,371313,537844,18,19,50,261,1042,3143,7794,16825,32786,59067,100018,161069,248850,371311,537842,16,17,48,259,1040,3141,7792,16823,32784,59065,100016,161067,248848,371309,537840,16,17,48,259,1040,3141,7792,16823,32784,59065,100016,161067,248848,371309,537840,20,21,52,263,1044,3145,7796,16827,32788,59069,100020,161071,248852,371313,537844,16,17,48,259,1040,3141,7792,16823,32784,59065,100016,161067,248848,371309,537840,22,23,54,265,1046,3147,7798,16829,32790,59071,100022,161073,248854,371315,537846,16,17,48,259,1040,3141,7792,16823,32784,59065,100016,161067,248848,371309,537840,18,19,50,261,1042,3143,7794,16825,32786,59067,100018,161069,248850,371311,537842,22,23,54,265,1046,3147,7798,16829,32790,59071,100022,161073,248854,371315,537846,22,23,54,265,1046,3147,7798,16829,32790,59071,100022,161073,248854,371315,537846,16,17,48,259,1040,3141,7792,16823,32784,59065,100016,161067,248848,371309,537840,22,23,54,265,1046,3147,7798,16829,32790,59071,100022,161073,248854,371315,537846,22,23,54,265,1046,3147,7798,16829,32790,59071,100022,161073,248854,371315,537846,16,17,48,259,1040,3141,7792,16823,32784,59065,100016,161067,248848,371309,537840,16,17,48,259,1040,3141,7792,16823,32784,59065,100016,161067,248848,371309,537840,18,19,50,261,1042,3143,7794,16825,32786,59067,100018,161069,248850,371311,537842,16,17,48,259,1040,3141,7792,16823,32784,59065,100016,161067,248848,371309,537840,16,17,48,259,1040,3141,7792,16823,32784,59065,100016,161067,248848,371309,537840,20,21,52,263,1044,3145,7796,16827,32788,59069,100020,161071,248852,371313,537844,18,19,50,261,1042,3143,7794,16825,32786,59067,100018,161069,248850,371311,537842,16,17,48,259,1040,3141,7792,16823,32784,59065,100016,161067,248848,371309,537840,16,17,48,259,1040,3141,7792,16823,32784,59065,100016,161067,248848,371309,537840,16,17,48,259,1040,3141,7792,16823,32784,59065,100016,161067,248848,371309,537840,20,21,52,263,1044,3145,7796,16827,32788,59069,100020,161071,248852,371313,537844,16,17,48,259,1040,3141,7792,16823,32784,59065,100016,161067,248848,371309,537840,18,19,50,261,1042,3143,7794,16825,32786,59067,100018,161069,248850,371311,537842,16,17,48,259,1040,3141,7792,16823,32784,59065,100016,161067,248848,371309,537840,24,25,56,267,1048,3149,7800,16831,32792,59073,100024,161075,248856,371317,537848,24,25,56,267,1048,3149,7800,16831,32792,59073,100024,161075,248856,371317,537848,24,25,56,267,1048,3149,7800,16831,32792,59073,100024,161075,248856,371317,537848,24,25,56,267,1048,3149,7800,16831,32792,59073,100024,161075,248856,371317,537848,24,25,56,267,1048,3149,7800,16831,32792,59073,100024,161075,248856,371317,537848,24,25,56,267,1048,3149,7800,16831,32792,59073,100024,161075,248856,371317,537848,24,25,56,267,1048,3149,7800,16831,32792,59073,100024,161075,248856,371317,537848,24,25,56,267,1048,3149,7800,16831,32792,59073,100024,161075,248856,371317,537848,24,25,56,267,1048,3149,7800,16831,32792,59073,100024,161075,248856,371317,537848,24,25,56,267,1048,3149,7800,16831,32792,59073,100024,161075,248856,371317,537848,24,25,56,267,1048,3149,7800,16831,32792,59073,100024,161075,248856,371317,537848,24,25,56,267,1048,3149,7800,16831,32792,59073,100024,161075,248856,371317,537848,24,25,56,267,1048,3149,7800,16831,32792,59073,100024,161075,248856,371317,537848,24,25,56,267,1048,3149,7800,16831,32792,59073,100024,161075,248856,371317,537848},{25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561,25,26,89,754,4121,15650,46681,117674,262169,531466,1000025,1771586,2986009,4826834,7529561}};
raysdesired = map(QQ^6, QQ^0, 0);
linealitydesired = map(QQ^6, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 4 0 - 10 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1},{0},{0},{4},{43/2},{0},{3}};
raysP = matrix {{1,1,0},{1,0,0},{0,1,0},{2,1,0},{3,27/2,1},{0,0,1},{-2,1,0}};
linealityP = map(QQ^7, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,1,2,3,4,5,6,7,8,9},{0,1,4,9,16,25,36,49,64,81},{0,1,8,27,64,125,216,343,512,729},{0,1,16,81,256,625,1296,2401,4096,6561},{0,1,32,243,1024,3125,7776,16807,32768,59049},{0,1,64,729,4096,15625,46656,117649,262144,531441},{0,1,128,2187,16384,78125,279936,823543,2097152,4782969}};
raysQ = map(QQ^7, QQ^0, 0);
linealityQ = map(QQ^7, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{1,2,3,4,5,6,7,8,9,10},{0,1,4,9,16,25,36,49,64,81},{0,1,8,27,64,125,216,343,512,729},{4,5,20,85,260,629,1300,2405,4100,6565},{43/2,45/2,107/2,529/2,2091/2,6293/2,15595/2,33657/2,65579/2,118141/2},{0,1,64,729,4096,15625,46656,117649,262144,531441},{3,4,131,2190,16387,78128,279939,823546,2097155,4782972}};
raysdesired = matrix {{1,1,0},{1,0,0},{0,1,0},{2,1,0},{3,27/2,1},{0,0,1},{-2,1,0}};
linealitydesired = map(QQ^7, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 40  - 8 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-2147/1260,-4189/2520,-3979/2520,-3349/2520,-2987/1260,-5869/2520,-5659/2520,-631/252,-1241/504,-461/180},{1499/1260,34039/30240,127/126,1063/1440,7951/3780,2253/1120,6949/3780,67/28,13883/6048,91/36},{-20089/45360,-24433/60480,-30809/90720,-5767/25920,-14083/15120,-5773/6720,-22219/30240,-10469/9072,-12949/12096,-8239/6480},{2/21,1259/15120,149/2268,17/432,1721/7560,45/224,485/3024,697/2268,59/216,77/216},{-269/22680,-299/30240,-37/5040,-53/12960,-59/1890,-11/420,-37/1890,-23/504,-233/6048,-91/1620},{1/1260,19/30240,1/2268,1/4320,17/7560,1/560,19/15120,2/567,17/6048,1/216},{-1/45360,-1/60480,-1/90720,-1/181440,-1/15120,-1/20160,-1/30240,-1/9072,-1/12096,-1/6480}};
raysP = matrix {{-1,-1,-1,1,-1,-1,1,-1,-1,1,1,-1,1,-1,1,1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1},{2357/1260,4609/2520,4399/2520,-2509/2520,505/252,989/504,-569/504,142/63,1115/504,-695/504,-1031/504,371/180,-427/360,104/45,-517/360,-757/360,431/180,-547/360,-167/72,-787/360,-341/280,-411/280,-1793/840,-1303/840,-395/168,-621/280,-223/140,-49/20,-67/28,-949/420},{-8831/7560,-11059/10080,-14599/15120,12287/30240,-701/504,-7949/6048,1033/2016,-461/252,-5269/3024,41/56,2161/1512,-539/360,91/160,-39/20,289/360,1661/1080,-2299/1080,785/864,1697/864,2447/1440,6077/10080,2129/2520,269/168,3203/3360,1367/672,1977/1120,319/315,203/90,134/63,389/210},{1313/3780,313/1008,127/504,-39/448,260/567,625/1512,-4355/36288,395/567,40/63,-3505/18144,-905/2016,847/1620,-3661/25920,127/162,-583/2592,-445/864,11/12,-2369/8640,-455/576,-355/576,-209/1344,-2477/10080,-1871/3360,-2003/6720,-1135/1344,-297/448,-37/112,-49/48,-925/1008,-1219/1680},{-17/315,-23/504,-13/378,125/12096,-59/756,-67/1008,557/36288,-151/1134,-173/1512,491/18144,61/864,-77/810,497/25920,-13/81,29/864,25/288,-11/54,127/2880,277/1728,65/576,89/4032,43/1120,331/3360,337/6720,241/1344,57/448,59/1008,35/144,23/112,247/1680},{4/945,17/5040,1/420,-13/20160,5/756,1/189,-37/36288,1/81,5/504,-5/2592,-11/2016,7/810,-7/5184,13/810,-11/4320,-31/4320,1/45,-31/8640,-1/64,-29/2880,-11/6720,-31/10080,-29/3360,-29/6720,-25/1344,-27/2240,-3/560,-7/240,-23/1008,-5/336},{-1/7560,-1/10080,-1/15120,1/60480,-1/4536,-1/6048,1/36288,-1/2268,-1/3024,1/18144,1/6048,-1/3240,1/25920,-1/1620,1/12960,1/4320,-1/1080,1/8640,1/1728,1/2880,1/20160,1/10080,1/3360,1/6720,1/1344,1/2240,1/5040,1/720,1/1008,1/1680}};
linealityP = map(QQ^7, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,1,0,0,0,0,0,0},{0,0,1,0,0,0,0,0},{0,0,0,1,0,0,0,0},{0,0,0,0,1,0,0,0},{0,0,0,0,0,1,0,0},{0,0,0,0,0,0,1,0},{0,0,0,0,0,0,0,1}};
raysQ = map(QQ^7, QQ^0, 0);
linealityQ = map(QQ^7, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-2147/1260,-4189/2520,-3979/2520,-3349/2520,-2987/1260,-5869/2520,-5659/2520,-631/252,-1241/504,-461/180},{1499/1260,34039/30240,127/126,1063/1440,7951/3780,2253/1120,6949/3780,67/28,13883/6048,91/36},{-20089/45360,-24433/60480,-30809/90720,-5767/25920,-14083/15120,-5773/6720,-22219/30240,-10469/9072,-12949/12096,-8239/6480},{2/21,1259/15120,149/2268,17/432,1721/7560,45/224,485/3024,697/2268,59/216,77/216},{-269/22680,-299/30240,-37/5040,-53/12960,-59/1890,-11/420,-37/1890,-23/504,-233/6048,-91/1620},{1/1260,19/30240,1/2268,1/4320,17/7560,1/560,19/15120,2/567,17/6048,1/216},{-1/45360,-1/60480,-1/90720,-1/181440,-1/15120,-1/20160,-1/30240,-1/9072,-1/12096,-1/6480}};
raysdesired = matrix {{-1,-1,-1,1,-1,-1,1,-1,-1,1,1,-1,1,-1,1,1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1},{2357/1260,4609/2520,4399/2520,-2509/2520,505/252,989/504,-569/504,142/63,1115/504,-695/504,-1031/504,371/180,-427/360,104/45,-517/360,-757/360,431/180,-547/360,-167/72,-787/360,-341/280,-411/280,-1793/840,-1303/840,-395/168,-621/280,-223/140,-49/20,-67/28,-949/420},{-8831/7560,-11059/10080,-14599/15120,12287/30240,-701/504,-7949/6048,1033/2016,-461/252,-5269/3024,41/56,2161/1512,-539/360,91/160,-39/20,289/360,1661/1080,-2299/1080,785/864,1697/864,2447/1440,6077/10080,2129/2520,269/168,3203/3360,1367/672,1977/1120,319/315,203/90,134/63,389/210},{1313/3780,313/1008,127/504,-39/448,260/567,625/1512,-4355/36288,395/567,40/63,-3505/18144,-905/2016,847/1620,-3661/25920,127/162,-583/2592,-445/864,11/12,-2369/8640,-455/576,-355/576,-209/1344,-2477/10080,-1871/3360,-2003/6720,-1135/1344,-297/448,-37/112,-49/48,-925/1008,-1219/1680},{-17/315,-23/504,-13/378,125/12096,-59/756,-67/1008,557/36288,-151/1134,-173/1512,491/18144,61/864,-77/810,497/25920,-13/81,29/864,25/288,-11/54,127/2880,277/1728,65/576,89/4032,43/1120,331/3360,337/6720,241/1344,57/448,59/1008,35/144,23/112,247/1680},{4/945,17/5040,1/420,-13/20160,5/756,1/189,-37/36288,1/81,5/504,-5/2592,-11/2016,7/810,-7/5184,13/810,-11/4320,-31/4320,1/45,-31/8640,-1/64,-29/2880,-11/6720,-31/10080,-29/3360,-29/6720,-25/1344,-27/2240,-3/560,-7/240,-23/1008,-5/336},{-1/7560,-1/10080,-1/15120,1/60480,-1/4536,-1/6048,1/36288,-1/2268,-1/3024,1/18144,1/6048,-1/3240,1/25920,-1/1620,1/12960,1/4320,-1/1080,1/8640,1/1728,1/2880,1/20160,1/10080,1/3360,1/6720,1/1344,1/2240,1/5040,1/720,1/1008,1/1680}};
linealitydesired = map(QQ^7, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 7 0 - 8 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{79/48,1,37/6,3},{65/48,2,43/6,0},{31/24,0,0,4},{0,0,31/3,0},{0,0,0,65/2},{0,31/2,0,0},{-53/12,-7,-52/3,1}};
raysP = matrix {{1,1,0},{1,0,0},{0,1,0},{2,1,0},{3,27/2,1},{0,0,1},{-2,1,0}};
linealityP = map(QQ^7, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,1,0,0,0,0,0,0},{0,0,1,0,0,0,0,0},{0,0,0,1,0,0,0,0},{0,0,0,0,1,0,0,0},{0,0,0,0,0,1,0,0},{0,0,0,0,0,0,1,0},{0,0,0,0,0,0,0,1}};
raysQ = map(QQ^7, QQ^0, 0);
linealityQ = map(QQ^7, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{79/48,127/48,79/48,79/48,79/48,79/48,79/48,79/48,1,2,1,1,1,1,1,1,37/6,43/6,37/6,37/6,37/6,37/6,37/6,37/6,3,4,3,3,3,3,3,3},{65/48,65/48,113/48,65/48,65/48,65/48,65/48,65/48,2,2,3,2,2,2,2,2,43/6,43/6,49/6,43/6,43/6,43/6,43/6,43/6,0,0,1,0,0,0,0,0},{31/24,31/24,31/24,55/24,31/24,31/24,31/24,31/24,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,4,4,4,5,4,4,4,4},{0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,31/3,31/3,31/3,31/3,34/3,31/3,31/3,31/3,0,0,0,0,1,0,0,0},{0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,65/2,65/2,65/2,65/2,65/2,67/2,65/2,65/2},{0,0,0,0,0,0,1,0,31/2,31/2,31/2,31/2,31/2,31/2,33/2,31/2,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0},{-53/12,-53/12,-53/12,-53/12,-53/12,-53/12,-53/12,-41/12,-7,-7,-7,-7,-7,-7,-7,-6,-52/3,-52/3,-52/3,-52/3,-52/3,-52/3,-52/3,-49/3,1,1,1,1,1,1,1,2}};
raysdesired = matrix {{1,1,0},{1,0,0},{0,1,0},{2,1,0},{3,27/2,1},{0,0,1},{-2,1,0}};
linealitydesired = map(QQ^7, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 128 1 - 35 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1},{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1}};
raysP = map(QQ^7, QQ^0, 0);
linealityP = map(QQ^7, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},{0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1},{0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1},{0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,0,0,0,1,1,1,0,0,0,1,0,0,0,1,0},{1,0,1,1,0,1,1,0,0,1,0,1,1,0,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,0,0,1,0,0},{1,1,0,1,1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,0,1,0,1,0,1,0,0,1,0,0,0,1,0,0,0},{1,1,1,0,1,1,0,1,0,0,1,1,0,1,0,0,1,0,0,0,1,1,0,1,0,0,1,0,0,0,1,0,0,0,0}};
raysQ = map(QQ^7, QQ^0, 0);
linealityQ = map(QQ^7, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,1,2,2,2,1,2,2,2,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,1,2,2,2,1,2,2,2,1,1,1,1,2,2,2,2,2,2,2,1,2,2,2,1,2,2,2,1,1,1,1,2,2,2,2,2,2,1,2,2,2,1,1,1,1,2,2,2,2,2,2,1,1,1,1,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,1,2,2,2,1,1,1,1,2,2,2,2,2,2,1,2,2,2,1,1,1,1,2,2,2,2,2,2,1,1,1,1,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,1,1,1,1,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,1,1,1,1,2,2,2,2,2,2,1,1,1,1,2,2,2,2,2,2,1,2,2,2,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,1,1,1,1,2,2,2,2,2,2,1,1,1,1,2,2,2,2,2,2,1,2,2,2,1,1,1,1,2,2,2,2,2,2,1,2,2,2,1,2,2,2,2,1,1,1,1,2,2,2,2,2,2,1,2,2,2,1,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,-1,-1,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,1,2,2,2,1,2,2,1,2,2,2,1,1,1,2,2,2,2,1,2,2,1,2,2,2,1,1,1,2,2,2,1,2,2,2,1,1,1,2,2,2,1,1,1,1,2,2,2,2,2,2,1,1,1,1,1,1,2,2,2,2,1,2,2,2,1,1,1,2,2,2,1,1,1,1,2,2,2,2,2,2,1,1,1,1,1,1,2,2,2,2,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,1,1,1,1,2,2,2,2,2,2,1,1,1,1,1,1,2,2,2,2,1,1,1,1,2,2,2,2,2,2,1,1,1,1,1,1,2,2,2,2,1,2,2,2,1,1,1,2,2,2,1,2,2,2,1,1,1,2,2,2,2,1,2,2,1,1,1,1,2,2,2,2,2,2,1,1,1,1,1,1,2,2,2,2,1,2,2,2,1,1,1,2,2,2,1,2,2,2,1,1,1,2,2,2,2,1,2,2,1,2,2,2,1,1,1,2,2,2,2,1,2,2,2,1,2,2,2,1,2,2,2,1,1,1,2,2,2,2,1,2,2,2,1,2,2,2,2,1,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,1,1,1,1,2,2,2,2,2,2,1,1,1,1,2,2,2,2,2,2,1,2,2,2,1,1,1,1,2,2,2,2,2,2,1,2,2,2,1,2,2,2,2,1,1,1,1,2,2,2,2,2,2,1,2,2,2,1,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,1,1,1,1,2,2,2,2,2,2,1,2,2,2,1,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,-1,-1,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,-1,-1,-1,-1,0,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,0,0,0,0,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0},{-1,-1,-1,-1,-1,-1,0,0,0,0,-1,-1,-1,-1,0,-1,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,2,2,2,2,2,1,2,2,2,2,1,2,2,2,1,2,2,1,2,2,2,1,1,1,2,2,2,2,2,1,2,2,2,1,2,2,1,2,2,2,1,1,1,2,2,2,2,1,2,2,1,2,2,2,1,1,1,2,2,2,1,2,2,2,1,1,1,2,2,2,1,1,1,1,2,2,2,2,2,2,1,1,1,1,1,1,2,2,2,2,2,2,2,1,2,2,2,1,2,2,1,2,2,1,2,2,1,1,2,2,2,1,2,2,1,2,2,1,2,2,1,1,2,2,1,2,2,1,2,2,1,1,2,1,2,2,2,1,1,1,2,2,2,1,1,1,2,2,2,1,1,1,2,2,1,2,2,1,2,2,1,1,2,1,2,2,2,1,1,1,2,2,2,1,1,1,2,2,2,1,1,1,2,1,1,1,1,2,2,2,2,2,2,1,1,1,1,1,1,2,2,2,2,1,1,1,1,1,1,2,2,2,2,1,1,1,1,2,1,2,2,2,1,1,1,2,2,2,1,1,1,2,2,2,1,1,1,2,1,2,2,2,1,1,1,2,2,2,1,1,1,2,2,2,1,1,1,2,2,1,2,2,1,2,2,1,1,2,2,1,2,2,1,2,2,1,1,2,2,2,1,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,1,1,1,1,2,2,2,2,2,2,1,1,1,1,1,1,2,2,2,2,1,2,2,2,1,1,1,2,2,2,1,2,2,2,1,1,1,2,2,2,2,1,2,2,1,2,2,2,1,1,1,2,2,2,2,1,2,2,2,1,2,2,2,1,2,2,2,1,1,1,2,2,2,2,1,2,2,2,1,2,2,2,2,1,2,2,2,2,2,2,2,2,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,-1,-1,-1,-1,0,1,1,1,1,2,2,2,2,2,2,1,2,2,2,1,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,0,0,0,-1,-1,-1,0,-1,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,0,0,0,-1,-1,-1,0,-1,-1,-1,0,-1,-1,-1,0,-1,-1,-1,-1,-1,0,0,0,-1,-1,-1,0,-1,-1,-1,0,-1,-1,-1,-1,0,0,0,-1,-1,-1,0,-1,-1,-1,0,-1,-1,-1,-1,-1,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,0,0,0,0,-1,-1,-1,-1,-1,-1,0,0,0,0,-1,-1,-1,-1,0},{-1,-1,-1,0,0,0,-1,-1,-1,0,-1,-1,-1,0,-1,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,2,2,2,2,2,1,2,2,2,2,1,2,2,2,1,2,2,1,2,2,2,1,1,1,2,2,2,-1,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,1,2,2,2,1,2,2,1,2,2,1,2,2,1,1,2,2,2,1,2,2,1,2,2,1,2,2,1,1,2,2,1,2,2,1,2,2,1,1,2,1,2,2,2,1,1,1,2,2,2,1,1,1,2,2,2,1,1,1,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,1,2,2,1,2,1,2,1,2,1,2,2,1,2,2,1,1,2,1,2,2,1,1,2,1,1,2,1,1,2,2,2,1,1,1,2,2,2,1,1,1,2,2,2,1,1,1,2,1,1,1,2,2,2,1,1,1,2,1,1,1,2,1,2,1,2,2,1,2,2,1,1,2,1,2,2,1,1,2,1,1,2,1,2,1,2,2,1,2,2,1,1,2,1,2,2,1,1,2,1,1,2,1,2,2,1,2,2,1,2,1,2,1,2,2,1,2,2,1,2,1,2,1,2,2,2,1,1,2,2,2,1,1,1,2,2,2,1,1,1,2,2,2,1,1,1,2,2,1,2,2,1,2,2,1,1,2,2,1,2,2,1,2,2,1,1,2,2,2,1,2,2,1,2,2,1,2,2,1,1,2,2,2,1,2,2,2,1,2,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,1,2,2,2,1,1,1,2,2,2,1,1,1,2,2,2,1,1,1,2,2,1,2,2,1,2,2,1,1,2,2,1,2,2,1,2,2,1,1,2,2,2,1,2,2,1,2,2,1,2,2,1,1,2,2,2,1,2,2,2,1,2,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,1,2,2,2,1,1,1,2,2,2,2,1,2,2,2,1,2,2,2,2,1,2,2,2,2,2,2,2,2,2,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,0,0,0,-1,-1,-1,0,-1,-1,-1,0,-1,1,2,2,2,1,1,1,2,2,2,2,1,2,2,2,1,2,2,2,2,1,2,2,2,2,2,2,2,2,2,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,-1,0,0,0,-1,-1,-1,0,-1,-1,-1,0,-1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,0,0,-1,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,-1,0,-1,-1,-1,-1,0,0,-1,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,-1,0,0,-1,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,-1,0,0,0,-1,-1,-1,0,0,0,-1,-1,-1,0,0,0,-1,-1,-1,0,-1,-1,-1,0,0,0,-1,-1,-1,0,-1,-1,-1,0,-1},{-1,0,0,-1,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,2,2,2,2,2,2,2,2,1,2,2,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,1,2,2,2,1,2,2,1,2,2,1,2,2,1,1,2,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,1,2,2,2,1,2,2,1,2,2,1,2,2,1,1,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,2,1,2,1,2,1,2,2,1,2,2,1,2,1,2,1,2,1,2,2,1,2,2,1,1,2,1,2,2,1,1,2,1,1,2,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,2,1,2,1,2,1,2,2,1,2,2,1,2,1,2,1,2,1,2,2,1,2,2,1,1,2,1,2,2,1,1,2,1,1,2,1,2,2,2,1,2,2,1,2,1,1,2,2,1,2,2,1,2,1,2,1,2,1,2,1,2,1,1,2,1,1,2,1,2,2,1,2,2,1,1,2,1,2,2,1,1,2,1,1,2,1,1,2,2,1,1,2,1,1,2,1,1,1,2,1,1,2,2,1,2,2,1,2,1,2,1,2,1,2,1,2,1,1,2,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,1,2,2,1,2,2,1,1,2,1,2,2,1,1,2,1,1,2,1,2,2,1,2,2,1,2,1,2,1,2,2,1,2,2,1,2,1,2,1,2,2,2,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,1,2,2,1,2,2,1,1,2,2,2,1,2,2,2,1,2,2,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,2,1,2,2,1,2,2,1,1,2,1,2,2,1,1,2,1,1,2,1,2,2,1,2,2,1,2,1,2,1,2,2,1,2,2,1,2,1,2,1,2,2,2,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,1,2,2,1,2,2,1,1,2,2,2,1,2,2,2,1,2,2,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,2,1,2,2,1,2,2,1,1,2,2,2,1,2,2,2,1,2,2,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,2,1,2,2,2,2,2,2,2,2,2,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,0,0,-1,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,2,1,2,2,1,2,2,1,1,2,2,2,1,2,2,2,1,2,2,-1,-1,-1,-1,-1,-1,-1,-1,0,-1,-1,2,1,2,2,2,2,2,2,2,2,2,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,0,0,-1,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,2,1,2,2,2,2,2,2,2,2,2,-1,-1,0,-1,-1,-1,-1,0,-1,-1,-1,-1,0,0,-1,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,-1,-1,-1,-1,0,-1,0,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,-1,0,-1,0,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,-1,0,-1,0,0,-1,0,0,-1,-1,0,-1,0,0,-1,-1,0,-1,-1,0,-1,-1,0,0,-1,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1},{0,-1,0,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,-1,0,-1,-1,-1,-1,2,2,2,2,2,2,-1,0,-1,-1,-1,-1,2,2,2,1,2,-1,0,-1,-1,-1,-1,2,2,2,1,2,-1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,2,1,2,1,2,1,-1,0,-1,-1,-1,-1,2,2,2,1,2,-1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,2,1,2,1,2,1,-1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,2,1,2,1,2,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,1,1,2,2,1,2,2,1,2,1,2,1,2,1,2,1,2,1,1,2,1,1,-1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,2,1,2,1,2,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,1,1,2,2,1,2,2,1,2,1,2,1,2,1,2,1,2,1,1,2,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,1,2,2,1,2,1,2,1,2,1,2,1,2,1,1,2,1,1,2,1,2,1,2,1,1,2,1,1,1,2,1,1,1,2,2,2,1,2,2,1,2,1,1,2,2,1,2,1,1,2,1,1,1,2,2,1,2,2,1,2,1,2,1,2,1,2,1,2,1,1,2,1,1,2,2,2,1,2,2,1,2,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,1,2,2,1,2,1,2,1,2,1,2,1,2,1,1,2,1,1,2,2,2,1,2,2,1,2,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,1,2,2,1,2,1,2,1,2,2,2,1,-1,-1,-1,-1,-1,2,2,1,2,2,1,2,1,2,1,2,2,2,1,-1,-1,-1,-1,-1,2,2,1,2,2,-1,-1,0,-1,-1,-1,2,2,1,2,2,1,2,1,2,1,2,1,2,1,2,1,1,2,1,1,2,2,2,1,2,2,1,2,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,1,2,2,1,2,1,2,1,2,2,2,1,-1,-1,-1,-1,-1,2,2,1,2,2,1,2,1,2,1,2,2,2,1,-1,-1,-1,-1,-1,2,2,1,2,2,-1,-1,0,-1,-1,-1,2,2,1,2,2,1,2,1,2,1,2,2,2,1,-1,-1,-1,-1,-1,2,2,1,2,2,-1,-1,0,-1,-1,-1,2,2,1,2,2,-1,-1,0,-1,-1,-1,2,2,2,2,2,2,0,-1,-1,-1,-1,0,-1,0,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,-1,2,2,1,2,2,1,2,1,2,1,2,2,2,1,-1,-1,-1,-1,-1,2,2,1,2,2,-1,-1,0,-1,-1,-1,2,2,1,2,2,-1,-1,0,-1,-1,-1,2,2,2,2,2,2,0,-1,-1,-1,-1,0,-1,0,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,-1,2,2,1,2,2,-1,-1,0,-1,-1,-1,2,2,2,2,2,2,0,-1,-1,-1,-1,0,-1,0,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,-1,2,2,2,2,2,2,0,-1,-1,-1,-1,0,-1,0,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,-1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,0,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,-1,-1,0,0,-1,0,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,-1,0,-1,-1,0,-1,0,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,-1},{0,0,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,-1,-1,2,2,2,2,2,0,-1,-1,-1,-1,2,0,-1,-1,-1,-1,2,-1,2,2,2,1,0,-1,-1,-1,-1,2,-1,2,2,2,1,-1,2,2,2,1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,1,1,0,-1,-1,-1,-1,2,-1,2,2,2,1,-1,2,2,2,1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,1,1,-1,2,2,2,1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,1,1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,1,1,2,2,1,2,1,1,2,1,1,1,-1,2,2,2,1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,1,1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,1,1,2,2,1,2,1,1,2,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,1,1,2,2,1,2,1,1,2,1,1,1,2,2,2,1,2,2,1,2,1,1,2,2,1,2,1,1,2,1,1,1,2,2,1,2,1,1,2,1,1,1,2,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,1,1,2,2,1,2,1,1,2,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,1,1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,1,1,2,2,1,2,1,1,2,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,1,1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,1,1,-1,-1,-1,-1,2,2,2,1,-1,2,2,2,1,2,2,1,2,1,1,-1,-1,-1,-1,2,2,2,1,-1,2,2,2,1,-1,2,0,-1,-1,-1,-1,2,2,2,1,2,2,1,2,1,1,2,2,1,2,1,1,2,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,1,1,-1,-1,-1,-1,2,2,2,1,2,2,1,2,1,1,-1,-1,-1,-1,2,2,2,1,-1,2,2,2,1,2,2,1,2,1,1,-1,-1,-1,-1,2,2,2,1,-1,2,2,2,1,-1,2,0,-1,-1,-1,-1,2,2,2,1,2,2,1,2,1,1,-1,-1,-1,-1,2,2,2,1,-1,2,2,2,1,-1,2,0,-1,-1,-1,-1,2,2,2,1,-1,2,0,-1,-1,-1,-1,2,0,-1,-1,-1,-1,2,2,2,2,2,0,0,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,-1,-1,2,2,2,1,2,2,1,2,1,1,-1,-1,-1,-1,2,2,2,1,-1,2,2,2,1,-1,2,0,-1,-1,-1,-1,2,2,2,1,-1,2,0,-1,-1,-1,-1,2,0,-1,-1,-1,-1,2,2,2,2,2,0,0,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,-1,-1,2,2,2,1,-1,2,0,-1,-1,-1,-1,2,0,-1,-1,-1,-1,2,2,2,2,2,0,0,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,-1,-1,2,0,-1,-1,-1,-1,2,2,2,2,2,0,0,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,-1,-1,2,2,2,2,2,0,0,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,-1,-1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,0,0,-1,0,0,-1,0,-1,-1,0,0,-1,0,-1,-1,0,-1,-1,-1,0,0,-1,0,-1,-1,0,-1,-1,-1,0,-1,-1,-1,-1}};
raysdesired = map(QQ^7, QQ^0, 0);
linealitydesired = map(QQ^7, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 8 1 - 20 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,0,-1,-1,-1,-1,-1,-1},{-1,-1,0,-1,-1,-1,-1,-1},{-1,-1,-1,0,-1,-1,-1,-1},{-1,-1,-1,-1,0,-1,-1,-1},{-1,-1,-1,-1,-1,0,-1,-1},{-1,-1,-1,-1,-1,-1,0,-1},{-1,-1,-1,-1,-1,-1,-1,0}};
raysP = map(QQ^7, QQ^0, 0);
linealityP = map(QQ^7, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19},{0,1,4,9,16,25,36,49,64,81,100,121,144,169,196,225,256,289,324,361},{0,1,8,27,64,125,216,343,512,729,1000,1331,1728,2197,2744,3375,4096,4913,5832,6859},{0,1,16,81,256,625,1296,2401,4096,6561,10000,14641,20736,28561,38416,50625,65536,83521,104976,130321},{0,1,32,243,1024,3125,7776,16807,32768,59049,100000,161051,248832,371293,537824,759375,1048576,1419857,1889568,2476099},{0,1,64,729,4096,15625,46656,117649,262144,531441,1000000,1771561,2985984,4826809,7529536,11390625,16777216,24137569,34012224,47045881},{0,1,128,2187,16384,78125,279936,823543,2097152,4782969,10000000,19487171,35831808,62748517,105413504,170859375,268435456,410338673,612220032,893871739}};
raysQ = map(QQ^7, QQ^0, 0);
linealityQ = map(QQ^7, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,-1,18},{-1,-1,0,3,8,15,24,35,48,63,80,99,120,143,168,195,224,255,288,323,360,0,1,4,9,16,25,36,49,64,81,100,121,144,169,196,225,256,289,324,361,-1,0,3,8,15,24,35,48,63,80,99,120,143,168,195,224,255,288,323,360,-1,0,3,8,15,24,35,48,63,80,99,120,143,168,195,224,255,288,323,360,-1,0,3,8,15,24,35,48,63,80,99,120,143,168,195,224,255,288,323,360,-1,0,3,8,15,24,35,48,63,80,99,120,143,168,195,224,255,288,323,360,-1,360},{-1,-1,0,7,26,63,124,215,342,511,728,999,1330,1727,2196,2743,3374,4095,4912,5831,6858,-1,0,7,26,63,124,215,342,511,728,999,1330,1727,2196,2743,3374,4095,4912,5831,6858,0,1,8,27,64,125,216,343,512,729,1000,1331,1728,2197,2744,3375,4096,4913,5832,6859,-1,0,7,26,63,124,215,342,511,728,999,1330,1727,2196,2743,3374,4095,4912,5831,6858,-1,0,7,26,63,124,215,342,511,728,999,1330,1727,2196,2743,3374,4095,4912,5831,6858,-1,0,7,26,63,124,215,342,511,728,999,1330,1727,2196,2743,3374,4095,4912,5831,6858,-1,6858},{-1,-1,0,15,80,255,624,1295,2400,4095,6560,9999,14640,20735,28560,38415,50624,65535,83520,104975,130320,-1,0,15,80,255,624,1295,2400,4095,6560,9999,14640,20735,28560,38415,50624,65535,83520,104975,130320,-1,0,15,80,255,624,1295,2400,4095,6560,9999,14640,20735,28560,38415,50624,65535,83520,104975,130320,0,1,16,81,256,625,1296,2401,4096,6561,10000,14641,20736,28561,38416,50625,65536,83521,104976,130321,-1,0,15,80,255,624,1295,2400,4095,6560,9999,14640,20735,28560,38415,50624,65535,83520,104975,130320,-1,0,15,80,255,624,1295,2400,4095,6560,9999,14640,20735,28560,38415,50624,65535,83520,104975,130320,-1,130320},{-1,-1,0,31,242,1023,3124,7775,16806,32767,59048,99999,161050,248831,371292,537823,759374,1048575,1419856,1889567,2476098,-1,0,31,242,1023,3124,7775,16806,32767,59048,99999,161050,248831,371292,537823,759374,1048575,1419856,1889567,2476098,-1,0,31,242,1023,3124,7775,16806,32767,59048,99999,161050,248831,371292,537823,759374,1048575,1419856,1889567,2476098,-1,0,31,242,1023,3124,7775,16806,32767,59048,99999,161050,248831,371292,537823,759374,1048575,1419856,1889567,2476098,0,1,32,243,1024,3125,7776,16807,32768,59049,100000,161051,248832,371293,537824,759375,1048576,1419857,1889568,2476099,-1,0,31,242,1023,3124,7775,16806,32767,59048,99999,161050,248831,371292,537823,759374,1048575,1419856,1889567,2476098,-1,2476098},{-1,-1,0,63,728,4095,15624,46655,117648,262143,531440,999999,1771560,2985983,4826808,7529535,11390624,16777215,24137568,34012223,47045880,-1,0,63,728,4095,15624,46655,117648,262143,531440,999999,1771560,2985983,4826808,7529535,11390624,16777215,24137568,34012223,47045880,-1,0,63,728,4095,15624,46655,117648,262143,531440,999999,1771560,2985983,4826808,7529535,11390624,16777215,24137568,34012223,47045880,-1,0,63,728,4095,15624,46655,117648,262143,531440,999999,1771560,2985983,4826808,7529535,11390624,16777215,24137568,34012223,47045880,-1,0,63,728,4095,15624,46655,117648,262143,531440,999999,1771560,2985983,4826808,7529535,11390624,16777215,24137568,34012223,47045880,0,1,64,729,4096,15625,46656,117649,262144,531441,1000000,1771561,2985984,4826809,7529536,11390625,16777216,24137569,34012224,47045881,-1,47045880},{-1,-1,0,127,2186,16383,78124,279935,823542,2097151,4782968,9999999,19487170,35831807,62748516,105413503,170859374,268435455,410338672,612220031,893871738,-1,0,127,2186,16383,78124,279935,823542,2097151,4782968,9999999,19487170,35831807,62748516,105413503,170859374,268435455,410338672,612220031,893871738,-1,0,127,2186,16383,78124,279935,823542,2097151,4782968,9999999,19487170,35831807,62748516,105413503,170859374,268435455,410338672,612220031,893871738,-1,0,127,2186,16383,78124,279935,823542,2097151,4782968,9999999,19487170,35831807,62748516,105413503,170859374,268435455,410338672,612220031,893871738,-1,0,127,2186,16383,78124,279935,823542,2097151,4782968,9999999,19487170,35831807,62748516,105413503,170859374,268435455,410338672,612220031,893871738,-1,0,127,2186,16383,78124,279935,823542,2097151,4782968,9999999,19487170,35831807,62748516,105413503,170859374,268435455,410338672,612220031,893871738,0,893871739}};
raysdesired = map(QQ^7, QQ^0, 0);
linealitydesired = map(QQ^7, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 21 0 - 21 
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1/3,1/3,0,2/3,1/3,-1/3,-2/3},{-1/3,1/3,0,2/3,1/3,-1/3,-2/3},{2/3,-2/3,0,-1/3,1/3,-1/3,1/3},{2/3,-2/3,0,-1/3,1/3,-1/3,1/3},{2/3,1/3,0,2/3,1/3,2/3,1/3},{-1/3,1/3,0,-1/3,-2/3,2/3,1/3},{-1/3,1/3,0,-1/3,-2/3,2/3,1/3}};
raysP = matrix {{0,1,1,0,1,1,1,0,1,0,0,0,1,-1},{0,1,1,1,0,1,1,0,1,0,0,0,-2,1/2},{1,1,1,0,0,1,1,0,-2,0,0,0,1,1/2},{0,-2,1,0,0,1,1,1,1,0,0,0,1,1/2},{0,1,1,0,0,1,-2,0,1,1,0,0,1,1/2},{0,1,-2,0,0,1,1,0,1,0,1,0,1,1/2},{0,1,1,0,0,-2,1,0,1,0,0,1,1,1/2}};
linealityP = map(QQ^7, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,-2/3,1/3,-1/3,1/3,-1/3,2/3},{0,-2/3,1/3,-1/3,1/3,-1/3,2/3},{0,1/3,-2/3,-1/3,1/3,2/3,-1/3},{0,1/3,-2/3,-1/3,1/3,2/3,-1/3},{0,1/3,1/3,2/3,1/3,2/3,2/3},{0,1/3,1/3,2/3,-2/3,-1/3,-1/3},{0,1/3,1/3,2/3,-2/3,-1/3,-1/3}};
raysQ = matrix {{1,1,1,0,0,1,1,0,-1,0,0,0,1,1},{0,-2,1,0,0,1,1,1,1/2,0,0,0,1,1},{0,1,1,0,0,1,-2,0,1/2,1,0,0,1,1},{0,1,-2,0,0,1,1,0,1/2,0,1,0,1,1},{0,1,1,0,0,-2,1,0,1/2,0,0,1,1,1},{0,1,1,0,1,1,1,0,1/2,0,0,0,1,-2},{0,1,1,1,0,1,1,0,1/2,0,0,0,-2,1}};
linealityQ = map(QQ^7, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-2/3,2/3,0,4/3,2/3,-2/3,-4/3},{-2/3,2/3,0,4/3,2/3,-2/3,-4/3},{4/3,-4/3,0,-2/3,2/3,-2/3,2/3},{4/3,-4/3,0,-2/3,2/3,-2/3,2/3},{4/3,2/3,0,4/3,2/3,4/3,2/3},{-2/3,2/3,0,-2/3,-4/3,4/3,2/3},{-2/3,2/3,0,-2/3,-4/3,4/3,2/3}};
raysdesired = matrix {{0,1,0,1,1,1,1,0,0,1,-1,0,0,1},{0,1,1,1,1,0,-2,0,0,1,1/2,0,0,1},{1,1,0,1,-2,0,1,0,0,1,1/2,0,0,1},{0,-2,0,1,1,0,1,0,0,1,1/2,1,0,1},{0,1,0,1,1,0,1,0,0,-2,1/2,0,1,1},{0,1,0,1,1,0,1,0,1,1,1/2,0,0,-2},{0,1,0,-2,1,0,1,1,0,1,1/2,0,0,1}};
linealitydesired = map(QQ^7, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 16  - 128 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1/3,-1/3,-2/3},{-1/3,-1/3,-2/3},{2/3,-1/3,1/3},{2/3,-1/3,1/3},{2/3,2/3,1/3},{-1/3,2/3,1/3},{-1/3,2/3,1/3}};
raysP = matrix {{0,1,1,0,1,1,0,1,0,0,0,1,-1},{0,1,1,1,1,1,0,1,0,0,0,-2,1/2},{1,1,1,0,1,1,0,-2,0,0,0,1,1/2},{0,-2,1,0,1,1,1,1,0,0,0,1,1/2},{0,1,1,0,1,-2,0,1,1,0,0,1,1/2},{0,1,-2,0,1,1,0,1,0,1,0,1,1/2},{0,1,1,0,-2,1,0,1,0,0,1,1,1/2}};
linealityP = map(QQ^7, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1},{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1}};
raysQ = map(QQ^7, QQ^0, 0);
linealityQ = map(QQ^7, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{2/3,-4/3,2/3,-4/3,1/3,-5/3},{-4/3,-4/3,-4/3,-4/3,-5/3,-5/3},{-1/3,-1/3,-4/3,-4/3,-2/3,-2/3},{-1/3,-1/3,-4/3,-4/3,-2/3,-2/3},{-1/3,-1/3,-1/3,-1/3,-2/3,-2/3},{-4/3,-4/3,-1/3,-1/3,-2/3,-2/3},{-4/3,-4/3,-1/3,-1/3,-2/3,-2/3}};
raysdesired = matrix {{0,1,1,0,1,1,0,1,0,0,0,1,-1},{0,1,1,1,1,1,0,1,0,0,0,-2,1/2},{1,1,1,0,1,1,0,-2,0,0,0,1,1/2},{0,-2,1,0,1,1,1,1,0,0,0,1,1/2},{0,1,1,0,1,-2,0,1,1,0,0,1,1/2},{0,1,-2,0,1,1,0,1,0,1,0,1,1/2},{0,1,1,0,-2,1,0,1,0,0,1,1,1/2}};
linealitydesired = map(QQ^7, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

