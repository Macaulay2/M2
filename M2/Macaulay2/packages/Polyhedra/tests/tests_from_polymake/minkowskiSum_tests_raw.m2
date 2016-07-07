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

-- Test 48 1 - 16 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{3/4,3/4,3/4,3/4,3/4,3/4,-3/4,-3/4,-3/4,-3/4,-3/4,-3/4,1/4,-1/4,0,0,0,0,1/4,-1/4,0,0,0,0,1/4,-1/4,0,0,0,0,1/4,-1/4,0,0,0,0,1/4,-1/4,0,0,0,0,1/4,-1/4,0,0,0,0},{1/4,-1/4,0,0,0,0,1/4,-1/4,0,0,0,0,3/4,3/4,3/4,3/4,3/4,3/4,-3/4,-3/4,-3/4,-3/4,-3/4,-3/4,0,0,1/4,-1/4,0,0,0,0,1/4,-1/4,0,0,0,0,1/4,-1/4,0,0,0,0,1/4,-1/4,0,0},{0,0,1/4,-1/4,0,0,0,0,1/4,-1/4,0,0,0,0,1/4,-1/4,0,0,0,0,1/4,-1/4,0,0,3/4,3/4,3/4,3/4,3/4,3/4,-3/4,-3/4,-3/4,-3/4,-3/4,-3/4,0,0,0,0,1/4,-1/4,0,0,0,0,1/4,-1/4},{0,0,0,0,1/4,-1/4,0,0,0,0,1/4,-1/4,0,0,0,0,1/4,-1/4,0,0,0,0,1/4,-1/4,0,0,0,0,1/4,-1/4,0,0,0,0,1/4,-1/4,3/4,3/4,3/4,3/4,3/4,3/4,-3/4,-3/4,-3/4,-3/4,-3/4,-3/4}};
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
verticesdesired = matrix {{7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-1,1,-1,1,-1,1,-1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,-1,1,1,-1,-1,1,1,-1,1,-1,1,-1,1,-1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,-1,1,1,-1,-1,1,1,-1,1,-1,1,-1,1,-1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,-1,1,1,-1,-1,1,1,-1,1,-1,1,-1,1,-1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,-1,1,1,-1,-1,1,1},{-1,1,-1,1,-1,1,-1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,-1,1,1,-1,-1,1,1,-1,1,-1,1,-1,1,-1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,-1,1,1,-1,-1,1,1,-1,1,-1,1,-1,1,-1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,-1,1,1,-1,-1,1,1,-1,1,-1,1,-1,1,-1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,-1,1,1,-1,-1,1,1,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,5/4,5/4,5/4,5/4,-5/4,-5/4,-5/4,-5/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4,-7/4}};
raysdesired = map(QQ^4, QQ^0, 0);
linealitydesired = map(QQ^4, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 42 1 - 16 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{33,34,19,9,9,1,1,1,1,1,4,10,4,9,1,1,1,1,1,1,1,1,1,9,4,4,4,10,34,19,10,35,20,20,10,4,4,10,4,10,4,4},{4,1,1,31,16,4,4,34,10,19,1,1,1,4,4,4,4,10,10,20,35,20,10,4,1,1,1,1,4,4,1,1,1,1,1,9,19,31,34,16,19,9},{1,4,31,1,25,33,9,1,1,25,10,4,34,1,34,19,9,16,1,1,1,25,31,1,35,20,10,4,1,1,19,4,4,31,34,16,1,1,1,25,25,31},{31,30,15,25,15,15,30,25,33,15,30,33,15,34,15,25,31,25,34,31,25,15,15,35,15,25,31,34,31,34,25,30,33,15,15,25,31,25,25,15,15,15}};
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
verticesdesired = matrix {{32,33,18,8,8,0,0,0,0,0,3,9,3,8,0,0,2,0,0,2,0,0,2,0,0,0,2,0,2,0,0,2,0,0,2,0,2,0,2,0,2,0,2,0,2,0,2,0,2,0,2,0,2,0,2,0,2,8,10,8,10,8,10,8,10,3,5,3,5,3,5,3,5,3,5,3,5,3,3,5,3,5,9,11,9,11,9,11,35,35,35,35,35,35,35,20,20,20,20,11,11,36,36,36,36,36,36,36,36,21,21,21,21,21,21,21,21,21,11,11,11,11,5,5,5,11,11,11,11,11,5,5,5,5,11,11,11,11,5,5,5,5,5},{3,0,0,30,15,3,3,33,9,18,0,0,0,3,3,5,5,3,5,5,3,5,5,3,3,5,5,11,11,9,11,11,9,11,11,21,21,21,21,36,36,36,36,36,36,36,36,21,21,21,21,21,21,11,11,11,11,3,3,5,5,3,3,5,5,0,0,2,2,0,0,2,2,0,0,2,2,0,0,0,2,2,0,0,0,0,2,2,3,5,5,3,5,3,5,3,5,3,5,0,2,0,2,0,2,0,2,0,2,0,0,2,0,2,0,2,0,2,0,2,0,2,10,20,20,30,32,32,32,32,35,35,35,35,15,17,17,17,20,20,20,10,10},{0,3,30,0,24,32,8,0,0,24,9,3,33,0,35,35,35,35,35,35,20,20,20,8,10,10,10,17,17,0,0,0,2,2,2,0,0,2,2,0,0,2,2,0,0,2,2,24,24,26,26,26,26,32,32,32,32,0,0,0,0,2,2,2,2,36,36,36,36,36,36,36,36,21,21,21,21,9,11,11,11,11,3,3,5,5,5,5,0,0,2,0,0,2,2,0,0,2,2,20,20,3,3,5,5,3,3,5,5,3,5,5,30,30,32,32,32,32,35,35,35,35,17,0,2,0,0,2,0,2,0,2,0,2,24,24,26,26,24,26,26,32,32},{30,29,14,24,14,14,29,24,32,14,29,32,14,33,14,14,14,16,16,16,26,26,26,32,32,32,32,26,26,35,35,35,35,35,35,32,32,32,32,24,24,24,24,26,26,26,26,14,14,14,14,16,16,14,14,16,16,36,36,36,36,36,36,36,36,14,14,14,14,16,16,16,16,26,26,26,26,32,32,32,32,32,35,35,35,35,35,35,30,30,30,32,32,32,32,35,35,35,35,26,26,29,29,29,29,31,31,31,31,34,34,34,14,14,14,14,16,16,14,14,16,16,26,32,32,24,24,24,26,26,24,24,26,26,14,14,14,16,14,14,16,14,16}};
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

-- Test 7 1 - 10 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0,0,1,0,0,1,0},{1,0,0,0,0,0,1},{0,0,0,1,0,0,1},{0,0,0,0,1,1,0}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{5,6,7,8,9,10,11,12,13,14},{25,36,49,64,81,100,121,144,169,196},{125,216,343,512,729,1000,1331,1728,2197,2744},{625,1296,2401,4096,6561,10000,14641,20736,28561,38416}};
raysQ = map(QQ^4, QQ^0, 0);
linealityQ = map(QQ^4, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{5,6,7,8,9,10,11,12,13,14,5,6,7,8,9,10,11,12,13,14,15,5,6,7,8,9,10,11,12,13,14,5,14,6,7,8,9,10,11,12,13,14,15,5,6,7,8,9,10,11,12,13,14},{26,37,50,65,82,101,122,145,170,197,25,25,36,49,64,81,100,121,144,169,196,25,36,49,64,81,100,121,144,169,196,25,196,25,36,49,64,81,100,121,144,169,196,26,37,50,65,82,101,122,145,170,197},{125,216,343,512,729,1000,1331,1728,2197,2744,125,125,216,343,512,729,1000,1331,1728,2197,2744,126,217,344,513,730,1001,1332,1729,2198,2745,125,2744,125,216,343,512,729,1000,1331,1728,2197,2744,126,217,344,513,730,1001,1332,1729,2198,2745},{625,1296,2401,4096,6561,10000,14641,20736,28561,38416,625,625,1296,2401,4096,6561,10000,14641,20736,28561,38416,625,1296,2401,4096,6561,10000,14641,20736,28561,38416,626,38417,626,1297,2402,4097,6562,10001,14642,20737,28562,38417,625,1296,2401,4096,6561,10000,14641,20736,28561,38416}};
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

-- Test 4 1 - 24 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{2,-2,5,0},{3,3,4,2},{4,-4,-3,0},{5,5,-2,-4}};
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
verticesdesired = matrix {{3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,-1,-1,-1,-1,-1,-1,0,0,0,0,1,1,1,2,2,2,6,6,7,7,7,7,8,8,8,8,9,9,9,9,9,9,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4},{5,5,6,6,7,7,4,4,6,6,7,7,4,4,5,5,7,7,4,4,5,5,6,6,5,5,6,6,7,7,4,4,6,7,4,5,7,4,5,6,8,8,7,7,8,8,6,6,8,8,5,5,6,6,7,7,4,4,5,5,6,6,3,3,5,5,6,6,3,3,4,4,6,6,3,3,4,4,5,5},{7,8,6,8,6,7,7,8,5,8,5,7,6,8,5,8,5,6,6,7,5,7,5,6,-1,0,-2,0,-2,-1,-1,0,-3,-3,-2,-3,-3,-2,-3,-3,-1,0,-2,1,-2,0,-2,1,-2,-1,-1,0,-2,0,-2,-1,3,4,2,4,2,3,3,4,1,4,1,3,2,4,1,4,1,2,2,3,1,3,1,2},{9,8,9,7,8,7,9,8,9,6,8,6,9,7,9,6,7,6,8,7,8,6,7,6,9,8,9,7,8,7,9,8,9,8,9,9,7,8,8,7,1,0,2,-1,1,-1,2,-1,0,-1,1,0,1,-1,0,-1,0,-1,0,-2,-1,-2,0,-1,0,-3,-1,-3,0,-2,0,-3,-2,-3,-1,-2,-1,-3,-2,-3}};
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

-- Test 16 1 - 6 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{1,0,1,0,1,0,0,1,1,0,1,0,0,1,0,1},{2/3,1,1/3,0,1/3,0,1,2/3,2/3,1,1/3,0,0,1/3,1,2/3},{7/36,1/3,1/12,0,1/12,0,1/3,7/36,29/36,2/3,11/12,1,1,11/12,2/3,29/36},{103/108,11/12,53/54,1,1/54,0,1/12,5/108,1/4,7/36,8/27,1/3,2/3,19/27,29/36,3/4}};
raysP = map(QQ^4, QQ^0, 0);
linealityP = map(QQ^4, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{0,1,0,0,0,15/56},{0,0,1,0,0,15/56},{0,0,0,1,0,15/56},{0,0,0,0,1,-1/14}};
raysQ = map(QQ^4, QQ^0, 0);
linealityQ = map(QQ^4, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{2,1,1,0,0,2,1,1,1,0,0,2,1,1,71/56,0,1,0,0,15/56,0,0,15/56,2,1,1,71/56,2,1,1,71/56,0,0,2,1,1,0,1,0,0,2,1,1,0,0,0,2,1,1,1},{2/3,5/3,2/3,2,1,1/3,4/3,1/3,0,1,0,1/3,4/3,1/3,101/168,0,0,1,0,15/56,2,1,71/56,2/3,5/3,2/3,157/168,2/3,5/3,2/3,157/168,2,1,1/3,1/3,0,0,0,0,0,1/3,1/3,1/3,2,1,1,2/3,5/3,2/3,2/3},{7/36,7/36,7/36,1/3,1/3,1/12,1/12,1/12,0,0,0,1/12,1/12,13/12,59/168,0,0,0,1,15/56,1/3,4/3,101/168,7/36,7/36,43/36,233/504,29/36,29/36,65/36,541/504,2/3,5/3,11/12,23/12,1,2,1,2,1,11/12,23/12,11/12,2/3,5/3,2/3,29/36,29/36,65/36,29/36},{103/108,103/108,211/108,11/12,23/12,53/54,53/54,107/54,1,1,2,1/54,1/54,1/54,-10/189,0,0,0,0,-1/14,1/12,1/12,1/84,5/108,5/108,5/108,-19/756,1/4,1/4,1/4,5/28,7/36,7/36,8/27,8/27,1/3,1/3,2/3,2/3,5/3,19/27,19/27,46/27,29/36,29/36,65/36,3/4,3/4,3/4,7/4}};
raysdesired = map(QQ^4, QQ^0, 0);
linealitydesired = map(QQ^4, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 4 1 - 32 1
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{0,1,0,1},{0,0,1,1},{0,0,0,0},{0,0,0,0},{0,0,0,0}};
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
verticesdesired = matrix {{-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,2,2,2,2,2,-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,2,2,2,2,2},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2},{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1}};
raysdesired = map(QQ^5, QQ^0, 0);
linealitydesired = map(QQ^5, QQ^0, 0);
verticesdesired = promote(verticesdesired, QQ);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
desired = convexHull(verticesdesired,raysdesired,linealitydesired);
computed = P+Q;
assert(computed == desired)
///

-- Test 32 1 - 32 0
-- Checking minkowskiSum
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}};
raysP = map(QQ^5, QQ^0, 0);
linealityP = map(QQ^5, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
verticesQ = matrix {{10242/1573,-3474/1573,-3474/1573,-3474/1573,-3474/1573,-7830/1573,-7830/1573,-7830/1573,-7830/1573,-7830/1573,-7830/1573,-7830/1573,-7830/1573,-7830/1573,-7830/1573,-7830/1573,-7830/1573,-3474/1573,-3474/1573,882/1573,-3474/1573,-3474/1573,882/1573,10242/1573},{277149/22022,-1245/182,-1245/182,699/182,1245/182,27/14,-15/14,57/14,15/14,15/14,57/14,-15/14,-57/14,-27/14,15/14,-15/14,-57/14,1245/182,1245/182,1749/182,-1245/182,-699/182,-1749/182,-277149/22022},{277149/22022,-699/182,-1245/182,1245/182,1245/182,-15/14,-57/14,15/14,-27/14,15/14,57/14,-15/14,-57/14,15/14,57/14,27/14,-15/14,1245/182,699/182,1749/182,-1245/182,-1245/182,-1749/182,-277149/22022},{91566/11011,3/7,-18/7,486/91,486/91,-18/7,-18/7,-3/7,-3/7,18/7,18/7,-18/7,-18/7,18/7,18/7,3/7,3/7,18/7,-3/7,486/91,-486/91,-486/91,-486/91,-91566/11011},{91566/11011,-486/91,-486/91,-3/7,18/7,3/7,3/7,18/7,18/7,18/7,18/7,-18/7,-18/7,-3/7,-3/7,-18/7,-18/7,486/91,486/91,486/91,-18/7,3/7,-486/91,-91566/11011}};
raysQ = matrix {{1,1,1,1,1,1,1,1},{0,0,1,0,-1,0,0,0},{0,1,0,0,0,-1,0,0},{0,0,0,1,0,0,-1,0},{1,0,0,0,0,0,0,-1}};
linealityQ = map(QQ^5, QQ^0, 0);
verticesQ = promote(verticesQ, QQ);
raysQ = promote(raysQ, QQ);
linealityQ = promote(linealityQ, QQ);
Q = convexHull(verticesQ,raysQ,linealityQ);
verticesdesired = matrix {{-5047/1573,-9403/1573,-5047/1573,-691/1573,8669/1573,-5047/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-5047/1573,-5047/1573,-691/1573,8669/1573,-5047/1573,-5047/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-5047/1573,-691/1573,8669/1573,-5047/1573,-5047/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-5047/1573,-5047/1573,-691/1573,-5047/1573,-5047/1573,-9403/1573,-9403/1573,-9403/1573,-691/1573,8669/1573,-5047/1573,-5047/1573,-5047/1573,-5047/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-5047/1573,-5047/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,8669/1573,-5047/1573,-5047/1573,-9403/1573,-9403/1573,-9403/1573,-691/1573,-9403/1573,-9403/1573,-9403/1573,-5047/1573,-5047/1573,-691/1573,8669/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-5047/1573,-5047/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-5047/1573,-5047/1573,-5047/1573,-5047/1573,8669/1573,-9403/1573,-9403/1573,-9403/1573,-5047/1573,-5047/1573,-691/1573,-5047/1573,-5047/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-5047/1573,-5047/1573,-691/1573,8669/1573,-5047/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-5047/1573,-5047/1573,-691/1573,8669/1573,-5047/1573,-5047/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-9403/1573,-5047/1573,-691/1573,8669/1573,-5047/1573,-9403/1573,-5047/1573,-691/1573},{-1427/182,-71/14,-1427/182,-1931/182,-299171/22022,-1063/182,41/14,-1/14,71/14,-1/14,-43/14,-1063/182,-517/182,-1567/182,-255127/22022,-1427/182,-1427/182,-29/14,-71/14,1/14,-29/14,-71/14,-1427/182,-1931/182,-299171/22022,881/182,1427/182,41/14,71/14,71/14,-1/14,29/14,-1/14,1427/182,1427/182,1931/182,-1427/182,-1427/182,-71/14,-41/14,-71/14,-1931/182,-299171/22022,-1063/182,-1063/182,881/182,1427/182,41/14,-1/14,71/14,29/14,29/14,71/14,-1/14,-43/14,-13/14,29/14,-1/14,-43/14,-1427/182,517/182,-41/14,1/14,-29/14,-71/14,299171/22022,881/182,1427/182,71/14,29/14,-1/14,1931/182,-29/14,1/14,-71/14,-1427/182,-881/182,-1931/182,-299171/22022,41/14,-1/14,71/14,29/14,1427/182,-517/182,13/14,-29/14,43/14,1/14,1/14,43/14,-29/14,-71/14,-41/14,1/14,-29/14,-71/14,1063/182,1063/182,-1427/182,-881/182,299171/22022,41/14,71/14,71/14,1427/182,1427/182,1931/182,-1427/182,-1427/182,-29/14,1/14,1/14,-71/14,-41/14,-71/14,-1427/182,-881/182,-1931/182,299171/22022,1427/182,-1/14,71/14,29/14,29/14,71/14,1427/182,1427/182,1931/182,255127/22022,517/182,1063/182,1/14,43/14,-41/14,1/14,-71/14,1063/182,1567/182,299171/22022,1427/182,71/14,1427/182,1931/182},{-1427/182,-71/14,-1427/182,-1931/182,-299171/22022,-1427/182,-29/14,-71/14,1/14,-29/14,-71/14,-1427/182,-1427/182,-1931/182,-299171/22022,-517/182,-1063/182,-1/14,-43/14,71/14,41/14,-1/14,-1063/182,-1567/182,-255127/22022,1427/182,1427/182,-1/14,29/14,71/14,-1/14,71/14,41/14,1427/182,881/182,1931/182,-881/182,-1427/182,-71/14,1/14,-29/14,-1931/182,-299171/22022,-881/182,-1427/182,1063/182,1063/182,-29/14,-71/14,1/14,-41/14,1/14,43/14,-29/14,-71/14,1/14,43/14,13/14,-29/14,-517/182,1427/182,29/14,71/14,41/14,-1/14,299171/22022,1427/182,1427/182,71/14,71/14,41/14,1931/182,-71/14,-41/14,-71/14,-1427/182,-1427/182,-1931/182,-299171/22022,-29/14,-71/14,1/14,-41/14,517/182,-1427/182,-1/14,-43/14,29/14,-13/14,29/14,71/14,-1/14,-43/14,29/14,71/14,41/14,-1/14,1427/182,881/182,-1063/182,-1063/182,299171/22022,-1/14,29/14,71/14,1427/182,881/182,1931/182,-881/182,-1427/182,-71/14,-41/14,1/14,-71/14,1/14,-29/14,-1427/182,-1427/182,-1931/182,255127/22022,1063/182,-71/14,1/14,-41/14,1/14,43/14,1063/182,517/182,1567/182,299171/22022,1427/182,1427/182,29/14,71/14,29/14,71/14,-1/14,1427/182,1931/182,299171/22022,1427/182,71/14,1427/182,1931/182},{-25/7,-25/7,-577/91,-577/91,-102577/11011,-25/7,-25/7,-25/7,-10/7,-25/7,-25/7,-577/91,-577/91,-577/91,-102577/11011,-4/7,-25/7,-25/7,-25/7,11/7,-4/7,-4/7,-577/91,-577/91,-102577/11011,395/91,395/91,-25/7,-10/7,11/7,-25/7,11/7,-4/7,11/7,-10/7,395/91,10/7,-11/7,-11/7,25/7,10/7,-395/91,-80555/11011,10/7,-11/7,577/91,577/91,-11/7,-11/7,4/7,4/7,25/7,25/7,-11/7,-11/7,25/7,25/7,10/7,10/7,10/7,577/91,25/7,25/7,10/7,10/7,102577/11011,577/91,577/91,25/7,25/7,10/7,577/91,-25/7,-10/7,-25/7,-57
