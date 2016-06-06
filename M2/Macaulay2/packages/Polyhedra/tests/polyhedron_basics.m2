-- Test 0
-- Checking convexHull basics
TEST ///
P = convexHull matrix {{3,1,0,2},{0,2,2,1},{1,-1,2,0}};
assert(numColumns vertices P == 3)
assert(dim P == 2)
assert(ambDim P == 3)
assert(rays P == 0)
assert(linSpace P == 0)
M = matrix {{3,4,1}};
v = matrix {{10}};
assert(hyperplanes P == (M,v) or hyperplanes P == (-M,-v))
///

-- Test 2
-- Checking convexHull halfspaces
TEST ///
P = convexHull (matrix{{1},{1}},matrix{{1,0},{0,1}});
M1 = matrix {{-1,0},{0,-1}};
v = matrix {{-1},{-1}};
assert(halfspaces P == (M1,v))
///
