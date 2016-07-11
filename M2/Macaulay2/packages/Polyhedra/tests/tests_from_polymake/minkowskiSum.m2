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

