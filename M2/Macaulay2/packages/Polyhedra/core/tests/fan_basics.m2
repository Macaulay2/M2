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
M = transpose matrix {{1,2},{2,1},{1,0},{0,1}}
L = {{0,1},{1,2},{0,3}};
L2 = {{0,3},{1,2}}
F = fan(M, L);
assert not isSmooth F
F1 = fan(M, L2);
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

-- Checking fanFromGfan constructor
TEST ///
R = matrix {{1,0},{0,1}};
L = matrix {{}};
MC = {{0},{1},{0,1}};
F = fan(R, MC)
FG = fanFromGfan {R, L, {{0,1}}, 2, 1,1, {1, 2, 1}}
assert(isWellDefined FG)
assert(numRows linealitySpace FG == 2)
assert(ambDim FG == 2)
assert(F == FG)
///

-- isWellDefined should fail for redundant rays and bad intersections
TEST ///
M = transpose matrix {{1,0},{1,1},{1,-1}}
assert(not isWellDefined fan(M, {{0,1,2}}))
M = transpose matrix {{1,0},{1,1},{1,-1},{0,1}}
assert(not isWellDefined fan(M, {{1,2},{0,3}}))
///
