-- Test 1 1 - 2 0
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0}};
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
verticesP = matrix {{0,1}};
raysP = map(QQ^1, QQ^0, 0);
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
verticesP = matrix {{1}};
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

-- Test 1 1 - 2 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,15}};
raysQ = map(QQ^1, QQ^0, 0);
linealityQ = map(QQ^1, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{1,16}};
raysdesired = map(QQ^1, QQ^0, 0);
linealitydesired = map(QQ^1, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 2 1 - 2 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0,15}};
raysP = map(QQ^1, QQ^0, 0);
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
verticesdesired = matrix {{0,16}};
raysdesired = map(QQ^1, QQ^0, 0);
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
verticesP = matrix {{0,2}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = map(QQ^0, QQ^0, 0);
raysQ = map(QQ^0, QQ^0, 0);
linealityQ = map(QQ^1, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = map(QQ^0, QQ^0, 0);
raysdesired = map(QQ^0, QQ^0, 0);
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
verticesP = matrix {{-1,1}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = map(QQ^0, QQ^0, 0);
raysQ = map(QQ^0, QQ^0, 0);
linealityQ = map(QQ^1, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = map(QQ^0, QQ^0, 0);
raysdesired = map(QQ^0, QQ^0, 0);
linealitydesired = map(QQ^1, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 2 1 - 2 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0,1}};
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
verticesdesired = matrix {{-1,2}};
raysdesired = map(QQ^1, QQ^0, 0);
linealitydesired = map(QQ^1, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

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
verticesQ = matrix {{1,-1}};
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

-- Test 2 1 - 2 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1}};
raysP = map(QQ^1, QQ^0, 0);
linealityP = map(QQ^1, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1,4}};
raysQ = map(QQ^1, QQ^0, 0);
linealityQ = map(QQ^1, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{0,5}};
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
verticesP = matrix {{0,1},{0,0}};
raysP = matrix {{0},{1}};
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{-1,-1,1,1},{-1,1,1,-1}};
raysQ = map(QQ^2, QQ^0, 0);
linealityQ = map(QQ^2, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-1,2},{-1,-1}};
raysdesired = matrix {{0},{1}};
linealitydesired = map(QQ^2, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 3 0 - 3 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0},{0}};
raysP = matrix {{1,0},{0,1}};
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{10,20,30},{10,30,40}};
raysQ = map(QQ^2, QQ^0, 0);
linealityQ = map(QQ^2, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{10},{10}};
raysdesired = matrix {{1,0},{0,1}};
linealitydesired = map(QQ^2, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 6 1 - 3 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0,1,1,0,-1,-1},{1,1,0,-1,-1,0}};
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
verticesdesired = matrix {{0,7/2,1,7/2,5/2,-1,-1},{7/2,1,7/2,0,-1,-1,5/2}};
raysdesired = map(QQ^2, QQ^0, 0);
linealitydesired = map(QQ^2, QQ^0, 0);
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
verticesP = matrix {{-1,1,-1,1},{-1,-1,1,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1,-1,1,-1},{0,0,2,2}};
raysQ = map(QQ^2, QQ^0, 0);
linealityQ = map(QQ^2, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-2,2,-2,2},{-1,-1,3,3}};
raysdesired = map(QQ^2, QQ^0, 0);
linealitydesired = map(QQ^2, QQ^0, 0);
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
verticesP = matrix {{0},{0}};
raysP = matrix {{1,0},{0,1}};
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{3/2,-1/2,1/2,1/2},{1/2,1/2,3/2,-1/2}};
raysQ = map(QQ^2, QQ^0, 0);
linealityQ = map(QQ^2, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-1/2,1/2},{1/2,-1/2}};
raysdesired = matrix {{1,0},{0,1}};
linealitydesired = map(QQ^2, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 2 1 - 3 0
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0,1},{1,0}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{2},{2}};
raysQ = matrix {{1,1},{-1,1}};
linealityQ = map(QQ^2, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{2},{3}};
raysdesired = matrix {{1,1},{-1,1}};
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
verticesP = matrix {{0,1,2,1,0},{0,0,1,3,1}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,1,1,2},{0,0,1,1}};
raysQ = map(QQ^2, QQ^0, 0);
linealityQ = map(QQ^2, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{0,2,4,1,2,3,0},{0,0,2,3,4,4,1}};
raysdesired = map(QQ^2, QQ^0, 0);
linealitydesired = map(QQ^2, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 4 1 - 1 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1,-1,1,-1},{3/2,3/2,-1/2,-1/2}};
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{3},{4}};
raysQ = map(QQ^2, QQ^0, 0);
linealityQ = map(QQ^2, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{4,2,4,2},{11/2,11/2,7/2,7/2}};
raysdesired = map(QQ^2, QQ^0, 0);
linealitydesired = map(QQ^2, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 2 0 - 5 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1},{0}};
raysP = matrix {{1},{0}};
linealityP = matrix {{0},{1}};
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1,0,0,1,-1},{0,1,-1,-1,1}};
raysQ = map(QQ^2, QQ^0, 0);
linealityQ = map(QQ^2, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-2},{1}};
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
verticesP = matrix {{0,1,0,1},{0,0,1,1}};
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
verticesdesired = matrix {{-1,2,-1,2},{-1,-1,2,2}};
raysdesired = map(QQ^2, QQ^0, 0);
linealitydesired = map(QQ^2, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 6 1 - 4 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{3/2,-3/2,0,0,0,0},{0,0,3/2,-3/2,0,0},{0,0,0,0,3/2,-3/2}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,0,1,0},{0,1,0,0},{1,0,0,0}};
raysQ = map(QQ^3, QQ^0, 0);
linealityQ = map(QQ^3, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{5/2,-3/2,-3/2,-3/2,0,0,1,0,0,0,1,0},{0,0,1,0,5/2,-3/2,-3/2,-3/2,0,1,0,0},{0,1,0,0,0,1,0,0,5/2,-3/2,-3/2,-3/2}};
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

-- Test 5 1 - 8 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1/2,-1/2,3/4,2,100},{2/3,2/3,2/3,3,200},{3/4,-3/4,1/2,4,300}};
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
verticesdesired = matrix {{-1/2,3/2,-3/2,1/2,-3/2,1/2,-3/2,-3/2,7/4,7/4,7/4,3,3,3,101,99,101,99,101,99,101},{-1/3,-1/3,-1/3,-1/3,5/3,5/3,-1/3,5/3,-1/3,5/3,-1/3,2,4,2,199,201,201,199,199,201,201},{7/4,7/4,-7/4,-7/4,-7/4,-7/4,1/4,1/4,-1/2,-1/2,3/2,3,3,5,299,299,299,301,301,301,301}};
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
verticesQ = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysQ = map(QQ^3, QQ^0, 0);
linealityQ = map(QQ^3, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{2,2,2,2,-2,-2,-2,-2},{-2,-2,2,2,2,2,-2,-2},{-2,2,2,-2,2,-2,2,-2}};
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
verticesP = matrix {{0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{3/2,-3/2,0,0,0,0},{0,0,3/2,-3/2,0,0},{0,0,0,0,3/2,-3/2}};
raysQ = map(QQ^3, QQ^0, 0);
linealityQ = map(QQ^3, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-3/2,0,0,5/2,1,1,-3/2,0,0,5/2,1,1,-3/2,0,0,5/2,1,1,-3/2,0,0,5/2,1,1},{0,-3/2,0,0,-3/2,0,1,5/2,1,1,5/2,1,0,-3/2,0,0,-3/2,0,1,5/2,1,1,5/2,1},{0,0,-3/2,0,0,-3/2,0,0,-3/2,0,0,-3/2,1,1,5/2,1,1,5/2,1,1,5/2,1,1,5/2}};
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
verticesP = matrix {{-1/2,1/2,-1/2,1/2,-1/2,1/2,-1/2,1/2},{-1/2,-1/2,1/2,1/2,-1/2,-1/2,1/2,1/2},{-1/2,-1/2,-1/2,-1/2,1/2,1/2,1/2,1/2}};
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
verticesdesired = matrix {{-3/2,3/2,-3/2,3/2,-3/2,3/2,-3/2,3/2},{-3/2,-3/2,3/2,3/2,-3/2,-3/2,3/2,3/2},{-3/2,-3/2,-3/2,-3/2,3/2,3/2,3/2,3/2}};
raysdesired = map(QQ^3, QQ^0, 0);
linealitydesired = map(QQ^3, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 4 1 - 8 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,0,-1,-1},{-1,-1,0,-1},{-1,-1,-1,0}};
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
verticesdesired = matrix {{-2,1,1,1,1,-2,0,-2,0,-2,0,-2,0},{-2,-2,0,-2,0,1,1,1,1,-2,-2,0,0},{-2,-2,-2,0,0,-2,-2,0,0,1,1,1,1}};
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
verticesP = matrix {{1,1,0,1,1,0,0,-4},{1,0,1,-2,1,1,-3,1},{0,0,-1,1,1,1,1,-3}};
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
verticesdesired = matrix {{2,2,1,1,2,2,2,-1,-1,1,-1,1,-5,-3,-5,-3,-5,-5},{2,-1,0,2,-3,-3,2,2,-4,-4,-4,-4,0,0,2,2,0,2},{-1,-1,-2,-2,0,2,2,2,0,0,2,2,-4,-4,-4,-4,-2,-2}};
raysdesired = map(QQ^3, QQ^0, 0);
linealitydesired = map(QQ^3, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 6 1 - 6 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0,1,0,0,0,-1},{0,0,1,0,-1,0},{1,0,0,-1,0,0}};
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
verticesdesired = matrix {{0,2,0,0,0,-2},{0,0,2,0,-2,0},{2,0,0,-2,0,0}};
raysdesired = map(QQ^3, QQ^0, 0);
linealitydesired = map(QQ^3, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 8 1 - 4 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
raysP = map(QQ^3, QQ^0, 0);
linealityP = map(QQ^3, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,1,0,1},{0,1,1,0},{0,0,1,1}};
raysQ = map(QQ^3, QQ^0, 0);
linealityQ = map(QQ^3, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-1,1,2,2,-1,0,-1,2,-1,-1,0,2,-1,2,1,2},{-1,-1,0,-1,1,2,2,2,-1,0,-1,-1,2,2,2,1},{-1,-1,-1,0,-1,-1,0,-1,1,2,2,2,2,1,2,2}};
raysdesired = map(QQ^3, QQ^0, 0);
linealitydesired = map(QQ^3, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///


-- Test 10 1 - 16 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0,1/2,1,0,1/2,1/2,0,0,0,1/2},{1,1/2,0,0,1/2,0,1/2,0,0,1/2},{0,0,0,0,1/2,1/2,1/2,1,0,1/2},{0,1/2,0,0,0,1/2,1/2,0,1,1/2}};
raysP = map(QQ^4, QQ^0, 0);
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
verticesdesired = matrix {{-1,1,-1,1,-1,1,-1,1,3/2,2,2,2,2,2,2,2,2,-1,3/2,3/2,-1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,3/2},{2,2,2,2,2,2,2,2,3/2,-1,1,-1,1,-1,1,-1,1,-1,3/2,-1,3/2,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,3/2},{-1,-1,1,1,-1,-1,1,1,-1,-1,-1,1,1,-1,-1,1,1,-1,3/2,3/2,3/2,2,2,2,2,2,2,2,2,-1,-1,-1,-1,1,1,1,1,3/2},{-1,-1,-1,-1,1,1,1,1,3/2,-1,-1,-1,-1,1,1,1,1,-1,-1,3/2,3/2,-1,-1,-1,-1,1,1,1,1,2,2,2,2,2,2,2,2,3/2}};
raysdesired = map(QQ^4, QQ^0, 0);
linealitydesired = map(QQ^4, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///


-- Test 6 1 - 24 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1,1,1,0,0,0},{1,0,0,1,1,0},{0,1,0,1,0,1},{0,0,1,0,1,1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1,0,0,0,0,1,-1,1,0,1,0,1,0,-1,-1,0,-1,1,-1,0,0,0,0,-1},{0,1,0,0,-1,1,0,0,1,0,1,-1,-1,1,0,-1,0,0,-1,1,0,0,-1,0},{0,0,1,-1,0,0,0,1,1,-1,-1,0,1,0,1,-1,-1,0,0,0,1,-1,0,0},{-1,-1,-1,-1,-1,0,-1,0,0,0,0,0,0,0,0,0,0,1,0,1,1,1,1,1}};
raysQ = map(QQ^4, QQ^0, 0);
linealityQ = map(QQ^4, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{2,1,1,2,2,1,2,1,1,2,2,1,2,2,1,2,1,1,0,0,-1,0,-1,-1,0,-1,-1,0,0,-1,0,-1,-1,0,0,-1},{1,2,1,2,1,2,0,0,-1,0,-1,-1,0,-1,-1,0,0,-1,2,1,1,2,2,1,2,2,1,2,1,1,-1,0,-1,0,-1,0},{0,0,-1,0,-1,-1,1,2,1,2,1,2,-1,0,-1,0,-1,0,1,2,1,2,1,2,-1,0,-1,0,-1,0,2,2,1,2,1,1},{-1,-1,-1,0,0,0,-1,-1,-1,0,0,0,1,1,1,2,2,2,-1,-1,-1,0,0,0,1,1,1,2,2,2,1,1,1,2,2,2}};
raysdesired = map(QQ^4, QQ^0, 0);
linealitydesired = map(QQ^4, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///


-- Test 10 1 - 16 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,1},{-1,-1,-1,-1,1,1,1,1,-1,1},{0,0,0,0,0,0,0,0,2,-2}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1},{0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1},{0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1},{0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1}};
raysQ = map(QQ^4, QQ^0, 0);
linealityQ = map(QQ^4, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-1,2,2,-1,-1,2,2,-1,-1,2,2,-1,-1,2,-1,0,-1,0,-1,0,-1,0,1,2,1,2,1,2,1,2},{-1,-1,-1,2,2,2,2,-1,-1,-1,-1,2,2,2,-1,-1,0,0,-1,-1,0,0,1,1,2,2,1,1,2,2},{-1,-1,-1,-1,-1,-1,-1,2,2,2,2,2,2,2,-1,-1,-1,-1,0,0,0,0,1,1,1,1,2,2,2,2},{0,0,1,0,1,0,1,0,1,0,1,0,1,1,3,3,3,3,3,3,3,3,-2,-2,-2,-2,-2,-2,-2,-2}};
raysdesired = map(QQ^4, QQ^0, 0);
linealitydesired = map(QQ^4, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 8 1 - 9 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{3/2,-1/2,1/2,1/2,1/2,1/2,1/2,1/2},{1/2,1/2,3/2,-1/2,1/2,1/2,1/2,1/2},{1/2,1/2,1/2,1/2,3/2,-1/2,1/2,1/2},{1/2,1/2,1/2,1/2,1/2,1/2,3/2,-1/2}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{2,-2,0,0,0,0,2,0,0},{0,0,2,-2,0,0,0,-2,0},{0,0,0,0,2,-2,0,0,2},{0,0,0,0,0,0,2,2,2}};
raysQ = map(QQ^4, QQ^0, 0);
linealityQ = map(QQ^4, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{7/2,7/2,-5/2,-1/2,-1/2,1/2,5/2,1/2,1/2,1/2,1/2,1/2,1/2,5/2,1/2,-3/2,1/2,1/2,5/2,1/2,1/2,5/2,-3/2,1/2,1/2,1/2,1/2},{1/2,1/2,1/2,-3/2,1/2,7/2,3/2,3/2,-5/2,-5/2,1/2,1/2,1/2,1/2,-3/2,1/2,5/2,1/2,1/2,-3/2,1/2,1/2,1/2,5/2,-3/2,1/2,1/2},{1/2,1/2,1/2,1/2,5/2,1/2,1/2,5/2,1/2,1/2,7/2,7/2,-5/2,-1/2,-1/2,1/2,1/2,-3/2,1/2,1/2,5/2,1/2,1/2,1/2,1/2,5/2,-3/2},{1/2,5/2,1/2,5/2,5/2,1/2,5/2,5/2,1/2,5/2,1/2,5/2,1/2,5/2,5/2,3/2,3/2,3/2,7/2,7/2,7/2,-1/2,-1/2,-1/2,-1/2,-1/2,-1/2}};
raysdesired = map(QQ^4, QQ^0, 0);
linealitydesired = map(QQ^4, QQ^0, 0);
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
verticesP = matrix {{1,-1,0,0,0,0,0,0},{0,0,1,-1,0,0,0,0},{0,0,0,0,1,-1,0,0},{0,0,0,0,0,0,1,-1}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{1,-1,0,0,0,0,0,0},{0,0,1,-1,0,0,0,0},{0,0,0,0,1,-1,0,0},{0,0,0,0,0,0,1,-1}};
raysQ = map(QQ^4, QQ^0, 0);
linealityQ = map(QQ^4, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{2,-2,0,0,0,0,0,0},{0,0,2,-2,0,0,0,0},{0,0,0,0,2,-2,0,0},{0,0,0,0,0,0,2,-2}};
raysdesired = map(QQ^4, QQ^0, 0);
linealitydesired = map(QQ^4, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///


