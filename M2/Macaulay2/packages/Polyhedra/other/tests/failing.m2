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

-- Test wreath/4.poly
-- Checking latticePoints
TEST ///
verticesP = matrix {{8350133076306891/9007199254740992,3,8350133076306891/9007199254740992,-21860931958418379/9007199254740992,-21860931958418379/9007199254740992,0,0,0,0,0,0,0,0,0,0},{-6424769926212153/2251799813685248,0,6424769926212153/2251799813685248,7941444920921901/4503599627370496,-7941444920921901/4503599627370496,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,0,8350133076306891/9007199254740992,3,8350133076306891/9007199254740992,-21860931958418379/9007199254740992,-21860931958418379/9007199254740992,0,0,0,0,0},{0,0,0,0,0,6424769926212153/2251799813685248,0,-6424769926212153/2251799813685248,7941444920921901/4503599627370496,-7941444920921901/4503599627370496,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,8350133076306891/9007199254740992,8350133076306891/9007199254740992,3,-21860931958418379/9007199254740992,-21860931958418379/9007199254740992},{0,0,0,0,0,0,0,0,0,0,-6424769926212153/2251799813685248,6424769926212153/2251799813685248,0,7941444920921901/4503599627370496,-7941444920921901/4503599627370496},{-1,-1,-1,-1,-1,0,0,0,0,0,1,1,1,1,1},{-1,-1,-1,-1,-1,1,1,1,1,1,0,0,0,0,0}};
desiredLP = matrix {{-2,-2,-2,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,2,2,2,3},{-1,0,1,-2,-1,0,1,2,-2,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,-2,-1,0,0,0,0,0,1,2,-1,0,1,0},{0,0,0,0,0,0,0,0,0,0,-2,-2,-2,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,2,3,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,-1,0,1,-2,-1,0,1,2,-2,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,-2,-1,0,0,0,1,2,-1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-2,-2,-2,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,1,1,1,1,1,1,2,2,2,3,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,0,1,-2,-1,0,1,2,-2,-1,0,0,0,0,1,2,-2,-1,0,0,1,2,-1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,-1,0,0,1,1,1,1,1,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,0,0,0,0,-1,-1,-1,-1,-1,-1},{-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,-1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,1,1,1,1,1,1,-1,-1,-1,-1,-1,0,0,0,0,-1,-1,-1,-1,-1,-1}};
desiredLP = sort desiredLP;
P = convexHull(verticesP)
computedLP = sort matrix {latticePoints P};
assert(desiredLP == computedLP);
///

-- Test ambDim: 2, dim: -1, nvert: 0
-- Checking normal_fan + several booleans
TEST ///
verticesP = map(QQ^2, QQ^0, 0);
raysP = map(QQ^2, QQ^0, 0);
linealityP = map(QQ^2, QQ^0, 0);
verticesP = promote(verticesP, QQ);
raysP = promote(raysP, QQ);
linealityP = promote(linealityP, QQ);
P = convexHull(verticesP,raysP,linealityP);
raysdesired = map(QQ^2, QQ^0, 0);
linealitydesired = map(QQ^2, QQ^0, 0);
raysdesired = promote(raysdesired, QQ);
linealitydesired = promote(linealitydesired, QQ);
maxConesdesired = {};
desired = fan (raysdesired ,linealitydesired ,maxConesdesired);
computed = normalFan P;
assert(computed == desired)
assert(isPolytopal computed)
assert(isSmooth computed)
assert(isPure computed)
assert(isSimplicial computed)
assert(isComplete computed)
///
