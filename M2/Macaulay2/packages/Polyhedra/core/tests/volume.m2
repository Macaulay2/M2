-- Test Volume Square 
TEST ///
P = hypercube 2
assert(latticeVolume P == 8)
assert(volume P == 4)
///

-- Test Volume Cube 
TEST ///
P = convexHull transpose matrix {{0,0,0},{1,0,0},{0,1,0},{0,0,1},{1,1,0},{1,0,1},{0,1,1},{1,1,1}}
assert(latticeVolume P == 6)
assert(volume P == 1)
///

-- Test Volume Empty
TEST /// 
P = convexHull matrix {{}}
assert(latticeVolume P == 0)
assert(volume P == 0)
///

-- Test Volume Zero Dimensional
TEST /// 
P = convexHull matrix {{0}}
assert(latticeVolume P == 1)
assert(volume P == 1)
///


-- Test Volume Not Full Dimensional 
TEST ///
V = vertices hypercube 2 
A = map(QQ^2,QQ^4,0)
Q = convexHull(V||A)
assert(latticeVolume Q == 8)
assert(volume Q == 4)
///


-- Test Volume Zero Ambdim
TEST ///
V = map(QQ^0,QQ^1,0)
P = convexHull V
assert(latticeVolume P == 1)
assert(volume P == 1)
///

