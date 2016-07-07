-- Test 30
-- Checking fan and Fan basics
TEST ///
M = matrix {{1,0,0},{0,1,0},{0,0,1}};
L = {{0,1,2}}
F = fan(M,L);
assert(maxCones F == {{0,1,2}})
assert(ambDim(F) == 3)
assert(dim F == 3)
assert(#(maxCones F) == 1)
assert not isComplete F
///

-- Test 33
-- Checking isSmooth and smoothSubfan
TEST ///
C1 = posHull matrix {{1,2},{2,1}};
C2 = posHull matrix {{1,0},{2,1}};
C3 = posHull matrix {{1,2},{0,1}};
M = transpose matrix {{1,2},{2,1},{1,0},{0,1}}
L = {{0,1},{1,2},{0,3}};
F = fan(M, L);
assert not isSmooth F
F1 = fan {C2,C3};
assert(smoothSubfan F == F1)
///

-- Test 34
-- Checking normalFan
TEST ///
P = convexHull matrix {{1,0,0},{0,1,0}};
F = normalFan P;
M = transpose matrix{{1,0},{0,1},{-1,-1}};
L = {{0,1},{1,2},{0,2}}
assert(F == fan(M,L))
///

-- Test 34a
-- Checking normalFan
TEST ///
P =  convexHull (matrix {{1,0,0},{0,1,0}},matrix {{1},{1}});
F = normalFan P;
M = transpose matrix {{1,0},{0,1},{1,-1},{-1,1}}
L = {{0,1},{0,2},{1,3}}
assert(F == fan(M,L))
///

