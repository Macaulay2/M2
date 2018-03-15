-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{0,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 1, nrays: 2, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0},{0,1},{0,0}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0},{1}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==1)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,-1,0},{0,1,0,-1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{1,2},{2,3},{0,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==4)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 4, 4})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 5, n_max_cones: 3
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0,-1,0},{0,1,0,0,-1},{0,0,1,0,0}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2},{1,3},{4}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==3)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 5, 4, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
TEST ///
raysF = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{2},{3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==3)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 3, n_max_cones: 1
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0},{0,1,0},{0,0,1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==1)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 3, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 7, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,0,-1,0,1,1,1},{-1,1,-2,0,1,0,-1},{-1,0,-1,1,-1,0,1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0},{1,3,4,5},{2,6},{3,5,6}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==4)
assert(not isSmooth F)
assert(not isPure F)
assert(not isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 7, 7, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 4, dim: 4, nrays: 12, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,1,1,1,1,1,1,1,1,1,1,1},{0,1,1,0,0,1,1,0,-1,-1,1/2,-2},{0,0,1,1,0,0,1,1,0,-1,1/2,0},{0,0,0,0,1,1,1,1,0,0,-1/4,0}};
linealityF = map(QQ^4, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2,3,4,5,6,7},{0,3,8,9},{0,1,2,3,10},{8,11}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==4)
assert(ambDim F ==4)
assert(#(maxCones F) ==4)
assert(not isSmooth F)
assert(not isPure F)
assert(not isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 12, 20, 11, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 4, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0,0},{0,1,0,0},{0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2},{0,1,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 5, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{0,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 4, dim: 4, nrays: 12, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,1,1,1,1,1,1,1,1,1,1,1},{0,1,1,0,0,1,1,0,-1,-1,1/2,-2},{0,0,1,1,0,0,1,1,0,-1,1/2,0},{0,0,0,0,1,1,1,1,0,0,-1/4,0}};
linealityF = map(QQ^4, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2,3,4,5,6,7},{0,3,8,9},{0,1,2,3,10},{8,11}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==4)
assert(ambDim F ==4)
assert(#(maxCones F) ==4)
assert(not isSmooth F)
assert(not isPure F)
assert(not isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 12, 20, 11, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
TEST ///
raysF = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{2},{3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==3)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 7, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,0,-1,0,1,1,1},{-1,1,-2,0,1,0,-1},{-1,0,-1,1,-1,0,1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0},{1,3,4,5},{2,6},{3,5,6}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==4)
assert(not isSmooth F)
assert(not isPure F)
assert(not isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 7, 7, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 4, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0,0},{0,1,0,0},{0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2},{0,1,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 5, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 2, dim: 1, nrays: 1, n_max_cones: 1
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1},{0}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==1)
assert(ambDim F ==2)
assert(#(maxCones F) ==1)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 6, n_max_cones: 6
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,-1,-1,0,1},{1,1,0,-1,-1,0}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{1,2},{2,3},{3,4},{4,5},{0,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==6)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 6})
assert(isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,-1,0},{0,1,0,-1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{1,2},{2,3},{0,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==4)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 4, 4})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 5, n_max_cones: 5
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,-1,0,0},{0,1,0,-1,0},{1,2,1,2,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2,3},{0,1,4},{1,2,4},{2,3,4},{0,3,4}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==5)
assert(not isSmooth F)
assert(isPure F)
assert(not isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 5, 8, 5})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,1,0,-1,1,0},{-1,0,1,-1,0,1},{-1,-1,-1,1,1,1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2},{3,4,5},{0,1,4},{0,3,4},{1,2,5},{1,4,5},{0,2,3},{2,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(not isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,-1,0},{1,-1,1,1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{1,2},{2,3},{0,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==4)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 4, 4})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{0,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
TEST ///
raysF = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{2},{3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==3)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,1,0},{1,0,-1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{1,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,-1,0},{0,1,-1,-1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{2,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,1,0,0},{0,1,1,-1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{1,2},{0,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==3)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 3})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 2, n_max_cones: 1
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0},{0,1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==1)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 2, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 4, dim: 4, nrays: 6, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,1,0,0,0,0},{0,1,1,0,0,0},{0,0,0,1,1,0},{0,0,0,0,1,1}};
linealityF = map(QQ^4, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,3},{0,4,5},{1,2,3},{1,2,4,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==4)
assert(ambDim F ==4)
assert(#(maxCones F) ==4)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 6, 11, 6, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,1,0},{0,1,1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0},{1,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 1, dim: 1, nrays: 2, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,1}};
linealityF = map(QQ^1, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{1},{0}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==1)
assert(ambDim F ==1)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 2})
assert(isPolytopal F)
///

-- Test ambDim: 4, dim: 4, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,1,0,0,0,0},{0,0,-1,1,0,0},{0,0,-1,0,1,0},{0,0,-1,0,0,1}};
linealityF = map(QQ^4, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{1,3,4,5},{1,2,4,5},{1,2,3,5},{1,2,3,4},{0,3,4,5},{0,2,4,5},{0,2,3,5},{0,2,3,4}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==4)
assert(ambDim F ==4)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 14, 16, 8})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,1,0,0},{-1,0,1,0},{-1,0,0,1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{1,2,3},{0,2,3},{0,1,3},{0,1,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==4)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 4, 6, 4})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 4, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0,0},{0,1,0,0},{0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2},{0,1,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 5, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 4, dim: 4, nrays: 12, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,1,1,1,1,1,1,1,1,1,1,1},{0,1,1,0,0,1,1,0,-1,-1,1/2,-2},{0,0,1,1,0,0,1,1,0,-1,1/2,0},{0,0,0,0,1,1,1,1,0,0,-1/4,0}};
linealityF = map(QQ^4, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2,3,4,5,6,7},{0,3,8,9},{0,1,2,3,10},{8,11}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==4)
assert(ambDim F ==4)
assert(#(maxCones F) ==4)
assert(not isSmooth F)
assert(not isPure F)
assert(not isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 12, 20, 11, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{0,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
TEST ///
raysF = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{2},{3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==3)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{0,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
TEST ///
raysF = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{2},{3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==3)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 7, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,0,-1,0,1,1,1},{-1,1,-2,0,1,0,-1},{-1,0,-1,1,-1,0,1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0},{1,3,4,5},{2,6},{3,5,6}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==4)
assert(not isSmooth F)
assert(not isPure F)
assert(not isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 7, 7, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{1,3,4},{0,2,5},{1,2,5},{0,2,4},{1,2,4},{0,3,4},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,1,-1,1},{-1,-1,1,1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2},{1,3},{0,1},{2,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==4)
assert(not isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 4, 4})
assert(isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,1,-1,1},{-1,-1,1,1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2},{1,3},{0,1},{2,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==4)
assert(not isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 4, 4})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{0,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
TEST ///
raysF = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{2},{3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==3)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 7, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,0,-1,0,1,1,1},{-1,1,-2,0,1,0,-1},{-1,0,-1,1,-1,0,1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0},{1,3,4,5},{2,6},{3,5,6}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==4)
assert(not isSmooth F)
assert(not isPure F)
assert(not isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 7, 7, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 3, n_max_cones: 1
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0},{0,1,0},{0,0,1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==1)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 3, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{0,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
TEST ///
raysF = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{2},{3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==3)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 7, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,0,-1,0,1,1,1},{-1,1,-2,0,1,0,-1},{-1,0,-1,1,-1,0,1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0},{1,3,4,5},{2,6},{3,5,6}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==4)
assert(not isSmooth F)
assert(not isPure F)
assert(not isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 7, 7, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 8, n_max_cones: 6
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,2},{-1,-1,-1,-1,1,1,1,3}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4,6},{1,3,5,7},{0,1,4,5},{2,3,6,7},{0,1,2,3},{4,5,6,7}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==6)
assert(not isSmooth F)
assert(isPure F)
assert(not isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 8, 12, 6})
assert(not isPolytopal F)
///

-- Test ambDim: 4, dim: 4, nrays: 6, n_max_cones: 9
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0,0,1,-1},{0,1,0,0,0,-1},{0,0,1,0,-1,0},{0,0,0,1,-1,0}};
linealityF = map(QQ^4, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{1,2,3,5},{1,2,4,5},{1,3,4,5},{0,3,4,5},{0,1,3,4},{0,1,2,3},{0,2,3,5},{0,1,2,4},{0,2,4,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==4)
assert(ambDim F ==4)
assert(#(maxCones F) ==9)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 15, 18, 9})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,-1,0},{0,1,0,-1},{0,0,0,0}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{1,2},{2,3},{0,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==4)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 4})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,1,0,-1,1,0},{-1,0,1,-1,0,1},{-1,-1,-1,1,1,1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2},{3,4,5},{0,1,4},{0,3,4},{1,2,5},{1,4,5},{0,2,3},{2,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(not isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 4, dim: 4, nrays: 42, n_max_cones: 47
-- Checking misc tests for fan
TEST ///
raysF = matrix {{0,1,1,1,1,1,1,1,0,1,0,0,1,1,1,0,0,1,1,1,1,0,1,1,1,1,0,0,1,1,0,0,0,0,1,1,1,1,0,0,1,0},{1,1,1,3,0,1,1,1/3,0,0,1,1,0,1,0,1,1,0,1,0,1/3,1,3,1,3,1/3,0,0,0,0,1,1,1,1,0,0,1/3,3,0,0,0,1},{1,1,3,1,1,0,1,1/3,1,0,0,1,1,0,1,1,3,3,0,1/3,0,1/3,0,3,1,1/3,1,1,0,0,0,0,3,1/3,3,1/3,0,0,0,1,0,0},{1,3,1,1,1,1,0,1/3,1,1,1,0,0,0,3,3,1,1,3,1/3,1/3,1/3,1,0,0,0,3,1/3,3,1/3,3,1/3,0,0,0,0,0,0,1,0,0,0}};
linealityF = map(QQ^4, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2,3,4,5,6,7},{0,1,2,4,8},{1,4,5,7,9},{0,1,3,5,10},{0,2,3,6,11},{2,4,6,7,12},{3,5,6,7,13},{1,4,8,14},{0,1,8,15},{0,2,8,16},{2,4,8,17},{1,4,9,14},{1,5,9,18},{4,7,9,19},{5,7,9,20},{0,1,10,15},{1,5,10,18},{0,3,10,21},{3,5,10,22},{0,2,11,16},{0,3,11,21},{2,6,11,23},{3,6,11,24},{2,4,12,17},{4,7,12,19},{2,6,12,23},{6,7,12,25},{5,7,13,20},{3,5,13,22},{3,6,13,24},{6,7,13,25},{1,8,14,15,26},{2,8,16,17,27},{1,9,14,18,28},{7,9,19,20,29},{1,10,15,18,30},{3,10,21,22,31},{2,11,16,23,32},{3,11,21,24,33},{2,12,17,23,34},{7,12,19,25,35},{7,13,20,25,36},{3,13,22,24,37},{1,14,15,18,26,28,30,38},{2,16,17,23,27,32,34,39},{7,19,20,25,29,35,36,40},{3,21,22,24,31,33,37,41}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==4)
assert(ambDim F ==4)
assert(#(maxCones F) ==47)
assert(not isSmooth F)
assert(isPure F)
assert(not isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 42, 132, 138, 47})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,-1},{0,1,-1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{0,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
TEST ///
raysF = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{2},{3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==3)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 7, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,0,-1,0,1,1,1},{-1,1,-2,0,1,0,-1},{-1,0,-1,1,-1,0,1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0},{1,3,4,5},{2,6},{3,5,6}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==4)
assert(not isSmooth F)
assert(not isPure F)
assert(not isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 7, 7, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 6, n_max_cones: 9
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,1,1,-1,1,-1},{818794080519290712218637983454656/829031252525274642799173400372205,-1708395506221045828793844317065/2509884118711564596175832378997,-89861355972553928970973245336569/17042562087395757815312397098412,1049518079883784126812314321891552/450042039024864499809788074031143,-57294552649102853504594142035824/101936291723111614375366730069055,-665091906365730277342873147817696/824708073670045873632314373349197},{508502279829950177343385417632574/829031252525274642799173400372205,-1554287588558661024133351439231/5019768237423129192351664757994,-117560758327001996748584699487539/17042562087395757815312397098412,729269897957095196268750301204717/1800168156099457999239152296124572,32205519897594406205837799691473/33978763907703871458455576689685,575327871058798074206062443400391/183268460815565749696069860744266}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{1,2},{2,3},{0,3},{1,4},{0,4},{2,5},{4,5},{3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==9)
assert(not isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 6, 9})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 26, n_max_cones: 24
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,1,1,1,0,0,0,-1,-1,-1,-1,1,1,0,0,-1,-1,1,1,0,0,-1,-1,1,0,-1},{0,1,1,0,1,1,0,0,1,1,0,-1,-1,-1,-1,-1,-1,1,0,1,0,1,0,-1,-1,-1},{0,1,0,1,0,1,1,0,1,0,1,1,0,0,1,1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2,3},{1,2,4,5},{1,3,5,6},{7,8,9,10},{4,5,8,9},{5,6,8,10},{0,3,11,12},{11,12,13,14},{3,6,11,14},{7,10,15,16},{13,14,15,16},{6,10,14,15},{0,2,17,18},{2,4,17,19},{17,18,19,20},{7,9,21,22},{4,9,19,21},{19,20,21,22},{0,12,18,23},{12,13,23,24},{18,20,23,24},{7,16,22,25},{13,16,24,25},{20,22,24,25}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==24)
assert(not isSmooth F)
assert(isPure F)
assert(not isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 26, 48, 24})
assert(isPolytopal F)
///

-- Test ambDim: 2, dim: 1, nrays: 2, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1},{1,-1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0},{1}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==1)
assert(ambDim F ==2)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,-1,0},{0,1,0,-1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{1,2},{2,3},{0,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==4)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 4, 4})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 4, n_max_cones: 1
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,1,1,1},{2,2,-2,-2},{2,-2,2,-2}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==1)
assert(not isSmooth F)
assert(isPure F)
assert(not isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 4, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 6, n_max_cones: 9
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,-1,-1,-1,1,1},{-1205394900074469874373270529299145/1219924955620354784582205359200148,254259858377295999839568073773176/148046853252149346413819602783863,2243462179548695050242687861482460/1504544056605790353640250717076991,2611761947406237914344776369881788/836649136082077893811605221470929,-1464482936131543532935907488283297/2245754515867165641441598773070946,-7492543451517178896179028037619435/9685081344024379534412048185367294},{2067170872801727856163589081805493/2439849911240709569164410718400296,2714624376991390424158764699813095/1184374826017194771310556822270904,-4175583222893067296766283081716952/1504544056605790353640250717076991,253610316070904946732597996386840/278883045360692631270535073823643,-150681337723949333338936635687700/1122877257933582820720799386535473,-1216361637388816490414928053374456/4842540672012189767206024092683647}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{2,3},{0,2},{1,3},{4,5},{0,5},{1,4},{2,5},{3,4}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==9)
assert(not isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 6, 9})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0},{0,0,1,-1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2},{0,3},{1,2},{1,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==4)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 4, 4})
assert(isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 2, n_max_cones: 1
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1},{0,1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==1)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 2, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 4, n_max_cones: 1
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,1,1,1},{1,1,-1,-1},{1,-1,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==1)
assert(not isSmooth F)
assert(isPure F)
assert(not isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 4, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 1, nrays: 9, n_max_cones: 9
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,-1,-1,1,-1,1,1,1,1},{1293979728731322079588387147955424/1712438211560724056148889068914273,30462351624619330260226372681081792/10196478602115919324126891460274647,152068420200329849224235051568195/134531827657486826638733806293982,-100051411100103709949459821946216/150381214932541824351341188556277,-992743498110247554806868862142581/303910908676062961508377082662302,-114930824870322725859722800237178/715216571008431039965238822244085,-1371725916620064457370316118978907/1854472312296316275632005978817650,3937954451592994032648807182828/265228143643495962897125993788577,3128926505495789804278361302472768/92545684373883375414959080116119},{24363516342674736410105952776873329/13699505692485792449191112551314184,6181275973857700312240096535829328/10196478602115919324126891460274647,-604899197274149933741600370862463/269063655314973653277467612587964,-22092236501875462380317377220987/150381214932541824351341188556277,81963292302580260335198188354388/50651818112677160251396180443717,4927520449762444076188034690180639/5721732568067448319721910577952680,-365617381734550053009221157097704/927236156148158137816002989408825,1533472706659392667347660977368/20402164895653535607471230291429,988675603333370524929137049198952/92545684373883375414959080116119}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0},{1},{2},{3},{4},{5},{6},{7},{8}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==1)
assert(ambDim F ==3)
assert(#(maxCones F) ==9)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 9})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 8, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,-1,0,1,-1,-1,1},{0,1,0,-1,1,1,-1,-1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{1,4},{0,4},{1,5},{2,5},{2,6},{3,6},{3,7},{0,7}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 8, 8})
assert(isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,-1,1},{1,1,-1,-1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{1,2},{2,3},{0,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==4)
assert(not isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 4, 4})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 8, n_max_cones: 6
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4,6},{1,3,5,7},{0,1,4,5},{2,3,6,7},{0,1,2,3},{4,5,6,7}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==6)
assert(not isSmooth F)
assert(isPure F)
assert(not isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 8, 12, 6})
assert(isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 8, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,-1,0,1,-1,-1,1},{0,1,0,-1,1,1,-1,-1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{1,4},{1,5},{2,5},{2,6},{3,6},{3,7},{0,4},{0,7}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 8, 8})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 4, n_max_cones: 1
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,1,1,1},{1,1,-1,-1},{1,-1,-1,1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==1)
assert(not isSmooth F)
assert(isPure F)
assert(not isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 4, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{0,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 1, nrays: 2, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0},{0,1},{0,0}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0},{1}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==1)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 1, dim: 1, nrays: 2, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1}};
linealityF = map(QQ^1, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0},{1}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==1)
assert(ambDim F ==1)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 2})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0},{0,1,0},{0,0,1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{0,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 4, dim: 1, nrays: 1, n_max_cones: 1
-- Checking misc tests for fan
TEST ///
raysF = matrix {{0},{0},{0},{0}};
linealityF = map(QQ^4, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==0)
assert(ambDim F ==4)
assert(#(maxCones F) ==1)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{0,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
TEST ///
raysF = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{2},{3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==3)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 7, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,0,-1,0,1,1,1},{-1,1,-2,0,1,0,-1},{-1,0,-1,1,-1,0,1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0},{1,3,4,5},{2,6},{3,5,6}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==4)
assert(not isSmooth F)
assert(not isPure F)
assert(not isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 7, 7, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2},{0,1}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{0,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0},{0,-1,1},{0,0,0}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2},{0,1}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{0,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 4, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0,0},{0,1,0,0},{0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2},{0,1,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 5, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{0,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
TEST ///
raysF = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{2},{3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==3)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 8, n_max_cones: 6
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,2},{-1,-1,-1,-1,1,1,1,3}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4,6},{1,3,5,7},{0,1,4,5},{2,3,6,7},{0,1,2,3},{4,5,6,7}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==6)
assert(not isSmooth F)
assert(isPure F)
assert(not isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 8, 12, 6})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,1},{0,-1,1,6}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{1,2},{2,3},{0,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==4)
assert(not isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 4, 4})
assert(isPolytopal F)
///

-- Test ambDim: 4, dim: 4, nrays: 6, n_max_cones: 9
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0,0,1,-1},{0,1,0,0,0,-1},{0,0,1,0,-1,0},{0,0,0,1,-1,0}};
linealityF = map(QQ^4, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{1,2,3,5},{1,2,4,5},{1,3,4,5},{0,3,4,5},{0,1,3,4},{0,1,2,3},{0,2,3,5},{0,1,2,4},{0,2,4,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==4)
assert(ambDim F ==4)
assert(#(maxCones F) ==9)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 15, 18, 9})
assert(isPolytopal F)
///

-- Test ambDim: 4, dim: 4, nrays: 43, n_max_cones: 107
-- Checking misc tests for fan
TEST ///
raysF = matrix {{0,1,1,1,1,1,1,1,0,1,0,0,1,1,1,0,0,1,1,1,1,0,1,1,1,1,0,0,1,1,0,0,0,0,1,1,1,1,0,0,1,0,-1},{1,1,1,3,0,1,1,1/3,0,0,1,1,0,1,0,1,1,0,1,0,1/3,1,3,1,3,1/3,0,0,0,0,1,1,1,1,0,0,1/3,3,0,0,0,1,-1},{1,1,3,1,1,0,1,1/3,1,0,0,1,1,0,1,1,3,3,0,1/3,0,1/3,0,3,1,1/3,1,1,0,0,0,0,3,1/3,3,1/3,0,0,0,1,0,0,-1},{1,3,1,1,1,1,0,1/3,1,1,1,0,0,0,3,3,1,1,3,1/3,1/3,1/3,1,0,0,0,3,1/3,3,1/3,3,1/3,0,0,0,0,0,0,1,0,0,0,-1}};
linealityF = map(QQ^4, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2,3,4,5,6,7},{0,1,2,4,8},{1,4,5,7,9},{0,1,3,5,10},{0,2,3,6,11},{2,4,6,7,12},{3,5,6,7,13},{1,4,8,14},{0,1,8,15},{0,2,8,16},{2,4,8,17},{1,4,9,14},{1,5,9,18},{4,7,9,19},{5,7,9,20},{0,1,10,15},{1,5,10,18},{0,3,10,21},{3,5,10,22},{0,2,11,16},{0,3,11,21},{2,6,11,23},{3,6,11,24},{2,4,12,17},{4,7,12,19},{2,6,12,23},{6,7,12,25},{5,7,13,20},{3,5,13,22},{3,6,13,24},{6,7,13,25},{1,8,14,15,26},{2,8,16,17,27},{1,9,14,18,28},{7,9,19,20,29},{1,10,15,18,30},{3,10,21,22,31},{2,11,16,23,32},{3,11,21,24,33},{2,12,17,23,34},{7,12,19,25,35},{7,13,20,25,36},{3,13,22,24,37},{1,14,15,18,26,28,30,38},{2,16,17,23,27,32,34,39},{7,19,20,25,29,35,36,40},{3,21,22,24,31,33,37,41},{4,8,14,42},{0,8,15,42},{0,8,16,42},{4,8,17,42},{4,9,14,42},{5,9,18,42},{4,9,19,42},{5,9,20,42},{0,10,15,42},{5,10,18,42},{0,10,21,42},{5,10,22,42},{0,11,16,42},{0,11,21,42},{6,11,23,42},{6,11,24,42},{4,12,17,42},{4,12,19,42},{6,12,23,42},{6,12,25,42},{5,13,20,42},{5,13,22,42},{6,13,24,42},{6,13,25,42},{8,15,26,42},{8,14,26,42},{8,17,27,42},{8,16,27,42},{9,14,28,42},{9,18,28,42},{9,19,29,42},{9,20,29,42},{10,15,30,42},{10,18,30,42},{10,21,31,42},{10,22,31,42},{11,16,32,42},{11,23,32,42},{11,21,33,42},{11,24,33,42},{12,17,34,42},{12,23,34,42},{12,19,35,42},{12,25,35,42},{13,20,36,42},{13,25,36,42},{13,22,37,42},{13,24,37,42},{15,26,30,38,42},{14,26,28,38,42},{18,28,30,38,42},{17,27,34,39,42},{16,27,32,39,42},{23,32,34,39,42},{19,29,35,40,42},{20,29,36,40,42},{25,35,36,40,42},{21,31,33,41,42},{22,31,37,41,42},{24,33,37,41,42}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==4)
assert(ambDim F ==4)
assert(#(maxCones F) ==107)
assert(not isSmooth F)
assert(isPure F)
assert(not isSimplicial F)
assert(fVector F == {1, 43, 170, 234, 107})
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,1,0,-1,1,0},{-1,0,1,-1,0,1},{-1,-1,-1,1,1,1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2},{3,4,5},{0,1,4},{0,3,4},{1,2,5},{1,4,5},{0,2,3},{2,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(not isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,-1,0},{0,1,0,-1},{0,0,0,0}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{1,2},{2,3},{0,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==4)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 4})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{0,1,0,-1},{1,0,-1,0}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{1,2},{2,3},{0,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==4)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 4, 4})
assert(isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0},{0,1,-1},{0,0,0}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{0,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 4, n_max_cones: 3
-- Checking misc tests for fan
TEST ///
raysF = matrix {{0,1,1,-1},{1,0,-1,0}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{2},{3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==3)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 4, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 7, n_max_cones: 4
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,0,-1,0,1,1,1},{-1,1,-2,0,1,0,-1},{-1,0,-1,1,-1,0,1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0},{1,3,4,5},{2,6},{3,5,6}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==4)
assert(not isSmooth F)
assert(not isPure F)
assert(not isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 7, 7, 2})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 5, dim: 3, nrays: 20, n_max_cones: 170
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},{0,0,0,0,0,0,0,-1,0,0,0,1,0,0,0,0,0,0,0,0},{0,-1,1,-1,-1,1,1,0,-1,1,1,0,-1,1,1,-1,1,-1,-1,0},{0,-2498297281740881198678291544546863/4292233922170347411817814193674192,-7012538792226086112924358907263123/8715740054670143779591681945787888,-853168194540718366519552314380549/1633604436597579867756918855037712,-2376368850200680272846378297742423/1188965054868984553175757399859904,3959858915640440808161367802075969/1756385359147711885281230275771760,-15370518860189209722653530257365101/4032359592672019241882820197740480,0,-33005235043899437497208274745288/105868656415338140456586840412833,723365086110191345493168434272/387998850010162462078593566713487,25362665923488389096629268456676/511676852640953683525248935581739,0,284734156325347081245652018033768/249884178116974368294619025288309,247742622372680406219772156972539/651910696131929186067030561586961,-11920975463394952023387172305872/5901745461923206181439973021473,-108239975558002546890967166653714/261036258360191737714509799678319,539426970431570236277770871109463/130987853236973852203880901389345,25170421131309517652087703934203/15926414258505777594363147697561,-20303616827570155889020353601744/9102424464896945257519978842191,0},{0,3878447178730676951910694486166429/8584467844340694823635628387348384,12509787701488839738937439262537009/17431480109340287559183363891575776,3124204339441645711919396014383823/6534417746390319471027675420150848,477892342042613973646209057148781/594482527434492276587878699929952,26099595939955105780012082333606671/7025541436590847541124921103087040,983540656839097004575785510609369/1008089898168004810470705049435120,0,-330103335103499526366998142504871/423474625661352561826347361651332,-838442066190720239385936283948709/387998850010162462078593566713487,-930713632552731931854669041770003/438580159406531728735927659070062,0,-2662382051315823227432821454096327/1999073424935794946356952202306472,-853819649472531556941987776395057/1303821392263858372134061123173922,-16616654466592771089357379907323/5901745461923206181439973021473,-177602914497116110289244987475037/261036258360191737714509799678319,843580423007891323234545346409517/523951412947895408815523605557380,-20356059935560866960509607801117/63705657034023110377452590790244,-24375837395791716729326666520827/9102424464896945257519978842191,0}};
linealityF = map(QQ^5, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,3},{0,1,4},{0,1,7},{0,1,11},{0,1,15},{0,2,5},{0,2,6},{0,2,7},{0,2,11},{0,2,13},{0,3,5},{0,3,7},{0,3,11},{0,3,17},{0,4,6},{0,4,7},{0,4,11},{0,4,18},{0,5,7},{0,5,11},{0,5,16},{0,6,7},{0,6,11},{0,6,14},{0,7,8},{0,7,9},{0,7,10},{0,7,12},{0,7,13},{0,7,14},{0,7,15},{0,7,16},{0,7,17},{0,7,18},{0,8,9},{0,8,11},{0,8,12},{0,8,15},{0,9,10},{0,9,11},{0,9,14},{0,10,11},{0,10,12},{0,10,13},{0,11,12},{0,11,13},{0,11,14},{0,11,15},{0,11,16},{0,11,17},{0,11,18},{0,12,17},{0,13,16},{0,14,18},{0,15,18},{0,16,17},{1,2,3,4,5,6},{1,3,7},{1,3,8,12,15,17},{1,3,11},{1,3,19},{1,4,7},{1,4,11},{1,4,15,18},{1,4,19},{1,7,15},{1,7,19},{1,11,15},{1,11,19},{1,15,19},{2,5,7},{2,5,11},{2,5,13,16},{2,5,19},{2,6,7},{2,6,9,10,13,14},{2,6,11},{2,6,19},{2,7,13},{2,7,19},{2,11,13},{2,11,19},{2,13,19},{3,5,7},{3,5,11},{3,5,16,17},{3,5,19},{3,7,17},{3,7,19},{3,11,17},{3,11,19},{3,17,19},{4,6,7},{4,6,11},{4,6,14,18},{4,6,19},{4,7,18},{4,7,19},{4,11,18},{4,11,19},{4,18,19},{5,7,16},{5,7,19},{5,11,16},{5,11,19},{5,16,19},{6,7,14},{6,7,19},{6,11,14},{6,11,19},{6,14,19},{7,8,9},{7,8,12},{7,8,15},{7,8,19},{7,9,10},{7,9,14},{7,9,19},{7,10,12},{7,10,13},{7,10,19},{7,12,17},{7,12,19},{7,13,16},{7,13,19},{7,14,18},{7,14,19},{7,15,18},{7,15,19},{7,16,17},{7,16,19},{7,17,19},{7,18,19},{8,9,10,12},{8,9,11},{8,9,14,15,18},{8,9,19},{8,11,12},{8,11,15},{8,11,19},{8,12,19},{8,15,19},{9,10,11},{9,10,19},{9,11,14},{9,11,19},{9,14,19},{10,11,12},{10,11,13},{10,11,19},{10,12,13,16,17},{10,12,19},{10,13,19},{11,12,17},{11,12,19},{11,13,16},{11,13,19},{11,14,18},{11,14,19},{11,15,18},{11,15,19},{11,16,17},{11,16,19},{11,17,19},{11,18,19},{12,17,19},{13,16,19},{14,18,19},{15,18,19},{16,17,19}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==5)
assert(#(maxCones F) ==170)
assert(not isSmooth F)
assert(isPure F)
assert(not isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 20, 92, 170})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 5, n_max_cones: 5
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0,-1,-1},{0,1,0,-1,-2},{0,0,1,-1,-3}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{4},{0,3},{1,3},{2,3},{0,1,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==5)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 5, 6, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 5, n_max_cones: 5
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0,-1,-1},{0,1,0,-1,-2},{0,0,1,-1,-3}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2},{0,3},{1,3},{2,3},{4}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==5)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 5, 6, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 5, dim: 4, nrays: 20, n_max_cones: 136
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},{0,0,0,0,0,0,0,-1,0,0,0,1,0,0,0,0,0,0,0,0},{0,-1,1,-1,-1,1,1,0,-1,1,1,0,-1,1,1,-1,1,-1,-1,0},{0,-2498297281740881198678291544546863/4292233922170347411817814193674192,-7012538792226086112924358907263123/8715740054670143779591681945787888,-853168194540718366519552314380549/1633604436597579867756918855037712,-2376368850200680272846378297742423/1188965054868984553175757399859904,3959858915640440808161367802075969/1756385359147711885281230275771760,-15370518860189209722653530257365101/4032359592672019241882820197740480,0,-33005235043899437497208274745288/105868656415338140456586840412833,723365086110191345493168434272/387998850010162462078593566713487,25362665923488389096629268456676/511676852640953683525248935581739,0,284734156325347081245652018033768/249884178116974368294619025288309,247742622372680406219772156972539/651910696131929186067030561586961,-11920975463394952023387172305872/5901745461923206181439973021473,-108239975558002546890967166653714/261036258360191737714509799678319,539426970431570236277770871109463/130987853236973852203880901389345,25170421131309517652087703934203/15926414258505777594363147697561,-20303616827570155889020353601744/9102424464896945257519978842191,0},{0,3878447178730676951910694486166429/8584467844340694823635628387348384,12509787701488839738937439262537009/17431480109340287559183363891575776,3124204339441645711919396014383823/6534417746390319471027675420150848,477892342042613973646209057148781/594482527434492276587878699929952,26099595939955105780012082333606671/7025541436590847541124921103087040,983540656839097004575785510609369/1008089898168004810470705049435120,0,-330103335103499526366998142504871/423474625661352561826347361651332,-838442066190720239385936283948709/387998850010162462078593566713487,-930713632552731931854669041770003/438580159406531728735927659070062,0,-2662382051315823227432821454096327/1999073424935794946356952202306472,-853819649472531556941987776395057/1303821392263858372134061123173922,-16616654466592771089357379907323/5901745461923206181439973021473,-177602914497116110289244987475037/261036258360191737714509799678319,843580423007891323234545346409517/523951412947895408815523605557380,-20356059935560866960509607801117/63705657034023110377452590790244,-24375837395791716729326666520827/9102424464896945257519978842191,0}};
linealityF = map(QQ^5, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1,2,3,4,5,6},{0,1,3,7},{0,1,3,8,12,15,17},{0,1,3,11},{0,1,4,7},{0,1,4,11},{0,1,4,15,18},{0,1,7,15},{0,1,11,15},{0,2,5,7},{0,2,5,11},{0,2,5,13,16},{0,2,6,7},{0,2,6,9,10,13,14},{0,2,6,11},{0,2,7,13},{0,2,11,13},{0,3,5,7},{0,3,5,11},{0,3,5,16,17},{0,3,7,17},{0,3,11,17},{0,4,6,7},{0,4,6,11},{0,4,6,14,18},{0,4,7,18},{0,4,11,18},{0,5,7,16},{0,5,11,16},{0,6,7,14},{0,6,11,14},{0,7,8,9},{0,7,8,12},{0,7,8,15},{0,7,9,10},{0,7,9,14},{0,7,10,12},{0,7,10,13},{0,7,12,17},{0,7,13,16},{0,7,14,18},{0,7,15,18},{0,7,16,17},{0,8,9,10,12},{0,8,9,11},{0,8,9,14,15,18},{0,8,11,12},{0,8,11,15},{0,9,10,11},{0,9,11,14},{0,10,11,12},{0,10,11,13},{0,10,12,13,16,17},{0,11,12,17},{0,11,13,16},{0,11,14,18},{0,11,15,18},{0,11,16,17},{1,2,3,4,5,6,7},{1,2,3,4,5,6,11},{1,2,3,4,5,6,19},{1,3,7,8,12,15,17},{1,3,7,19},{1,3,8,11,12,15,17},{1,3,8,12,15,17,19},{1,3,11,19},{1,4,7,15,18},{1,4,7,19},{1,4,11,15,18},{1,4,11,19},{1,4,15,18,19},{1,7,15,19},{1,11,15,19},{2,5,7,13,16},{2,5,7,19},{2,5,11,13,16},{2,5,11,19},{2,5,13,16,19},{2,6,7,9,10,13,14},{2,6,7,19},{2,6,9,10,11,13,14},{2,6,9,10,13,14,19},{2,6,11,19},{2,7,13,19},{2,11,13,19},{3,5,7,16,17},{3,5,7,19},{3,5,11,16,17},{3,5,11,19},{3,5,16,17,19},{3,7,17,19},{3,11,17,19},{4,6,7,14,18},{4,6,7,19},{4,6,11,14,18},{4,6,11,19},{4,6,14,18,19},{4,7,18,19},{4,11,18,19},{5,7,16,19},{5,11,16,19},{6,7,14,19},{6,11,14,19},{7,8,9,10,12},{7,8,9,14,15,18},{7,8,9,19},{7,8,12,19},{7,8,15,19},{7,9,10,19},{7,9,14,19},{7,10,12,13,16,17},{7,10,12,19},{7,10,13,19},{7,12,17,19},{7,13,16,19},{7,14,18,19},{7,15,18,19},{7,16,17,19},{8,9,10,11,12},{8,9,10,12,19},{8,9,11,14,15,18},{8,9,11,19},{8,9,14,15,18,19},{8,11,12,19},{8,11,15,19},{9,10,11,19},{9,11,14,19},{10,11,12,13,16,17},{10,11,12,19},{10,11,13,19},{10,12,13,16,17,19},{11,12,17,19},{11,13,16,19},{11,14,18,19},{11,15,18,19},{11,16,17,19}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==4)
assert(ambDim F ==5)
assert(#(maxCones F) ==136)
assert(not isSmooth F)
assert(isPure F)
assert(not isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 20, 92, 170, 136})
assert(not isPolytopal F)
///

-- Test ambDim: 5, dim: 5, nrays: 20, n_max_cones: 40
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},{0,0,0,0,0,0,0,-1,0,0,0,1,0,0,0,0,0,0,0,0},{0,-1,1,-1,-1,1,1,0,-1,1,1,0,-1,1,1,-1,1,-1,-1,0},{0,-2498297281740881198678291544546863/4292233922170347411817814193674192,-7012538792226086112924358907263123/8715740054670143779591681945787888,-853168194540718366519552314380549/1633604436597579867756918855037712,-2376368850200680272846378297742423/1188965054868984553175757399859904,3959858915640440808161367802075969/1756385359147711885281230275771760,-15370518860189209722653530257365101/4032359592672019241882820197740480,0,-33005235043899437497208274745288/105868656415338140456586840412833,723365086110191345493168434272/387998850010162462078593566713487,25362665923488389096629268456676/511676852640953683525248935581739,0,284734156325347081245652018033768/249884178116974368294619025288309,247742622372680406219772156972539/651910696131929186067030561586961,-11920975463394952023387172305872/5901745461923206181439973021473,-108239975558002546890967166653714/261036258360191737714509799678319,539426970431570236277770871109463/130987853236973852203880901389345,25170421131309517652087703934203/15926414258505777594363147697561,-20303616827570155889020353601744/9102424464896945257519978842191,0},{0,3878447178730676951910694486166429/8584467844340694823635628387348384,12509787701488839738937439262537009/17431480109340287559183363891575776,3124204339441645711919396014383823/6534417746390319471027675420150848,477892342042613973646209057148781/594482527434492276587878699929952,26099595939955105780012082333606671/7025541436590847541124921103087040,983540656839097004575785510609369/1008089898168004810470705049435120,0,-330103335103499526366998142504871/423474625661352561826347361651332,-838442066190720239385936283948709/387998850010162462078593566713487,-930713632552731931854669041770003/438580159406531728735927659070062,0,-2662382051315823227432821454096327/1999073424935794946356952202306472,-853819649472531556941987776395057/1303821392263858372134061123173922,-16616654466592771089357379907323/5901745461923206181439973021473,-177602914497116110289244987475037/261036258360191737714509799678319,843580423007891323234545346409517/523951412947895408815523605557380,-20356059935560866960509607801117/63705657034023110377452590790244,-24375837395791716729326666520827/9102424464896945257519978842191,0}};
linealityF = map(QQ^5, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{1,4,11,15,18,19},{8,9,11,14,15,18,19},{10,11,12,13,16,17,19},{4,6,11,14,18,19},{1,3,8,11,12,15,17,19},{8,9,10,11,12,19},{1,2,3,4,5,6,11,19},{2,6,9,10,11,13,14,19},{2,5,11,13,16,19},{3,5,11,16,17,19},{1,4,7,15,18,19},{7,8,9,14,15,18,19},{7,10,12,13,16,17,19},{4,6,7,14,18,19},{1,3,7,8,12,15,17,19},{7,8,9,10,12,19},{1,2,3,4,5,6,7,19},{2,6,7,9,10,13,14,19},{2,5,7,13,16,19},{3,5,7,16,17,19},{0,1,4,11,15,18},{0,8,9,11,14,15,18},{0,10,11,12,13,16,17},{0,4,6,11,14,18},{0,1,3,8,11,12,15,17},{0,8,9,10,11,12},{0,1,2,3,4,5,6,11},{0,2,6,9,10,11,13,14},{0,2,5,11,13,16},{0,3,5,11,16,17},{0,1,4,7,15,18},{0,7,8,9,14,15,18},{0,7,10,12,13,16,17},{0,4,6,7,14,18},{0,1,3,7,8,12,15,17},{0,7,8,9,10,12},{0,1,2,3,4,5,6,7},{0,2,6,7,9,10,13,14},{0,2,5,7,13,16},{0,3,5,7,16,17}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==5)
assert(ambDim F ==5)
assert(#(maxCones F) ==40)
assert(not isSmooth F)
assert(isPure F)
assert(not isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 20, 92, 170, 136, 40})
///

-- Test ambDim: 3, dim: 3, nrays: 5, n_max_cones: 5
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0,-1,-1},{0,1,0,-1,-2},{0,0,1,-1,-3}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{4},{0,3},{1,3},{2,3},{0,1,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==5)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 5, 6, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 1, nrays: 3, n_max_cones: 3
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,-1},{0,1,-1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0},{1},{2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==1)
assert(ambDim F ==2)
assert(#(maxCones F) ==3)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,-1},{0,1,-1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,1},{2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 1, nrays: 3, n_max_cones: 3
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,-1},{0,1,-1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0},{1},{2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==1)
assert(ambDim F ==2)
assert(#(maxCones F) ==3)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 6, n_max_cones: 12
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2},{0,3},{0,4},{0,5},{1,2},{1,3},{1,4},{1,5},{2,4},{2,5},{3,4},{3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==12)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 6, 12})
assert(not isPolytopal F)
///

-- Test ambDim: 5, dim: 1, nrays: 20, n_max_cones: 20
-- Checking misc tests for fan
TEST ///
raysF = matrix {{-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},{0,0,0,0,0,0,0,-1,0,0,0,1,0,0,0,0,0,0,0,0},{0,-1,1,-1,-1,1,1,0,-1,1,1,0,-1,1,1,-1,1,-1,-1,0},{0,-2498297281740881198678291544546863/4292233922170347411817814193674192,-7012538792226086112924358907263123/8715740054670143779591681945787888,-853168194540718366519552314380549/1633604436597579867756918855037712,-2376368850200680272846378297742423/1188965054868984553175757399859904,3959858915640440808161367802075969/1756385359147711885281230275771760,-15370518860189209722653530257365101/4032359592672019241882820197740480,0,-33005235043899437497208274745288/105868656415338140456586840412833,723365086110191345493168434272/387998850010162462078593566713487,25362665923488389096629268456676/511676852640953683525248935581739,0,284734156325347081245652018033768/249884178116974368294619025288309,247742622372680406219772156972539/651910696131929186067030561586961,-11920975463394952023387172305872/5901745461923206181439973021473,-108239975558002546890967166653714/261036258360191737714509799678319,539426970431570236277770871109463/130987853236973852203880901389345,25170421131309517652087703934203/15926414258505777594363147697561,-20303616827570155889020353601744/9102424464896945257519978842191,0},{0,3878447178730676951910694486166429/8584467844340694823635628387348384,12509787701488839738937439262537009/17431480109340287559183363891575776,3124204339441645711919396014383823/6534417746390319471027675420150848,477892342042613973646209057148781/594482527434492276587878699929952,26099595939955105780012082333606671/7025541436590847541124921103087040,983540656839097004575785510609369/1008089898168004810470705049435120,0,-330103335103499526366998142504871/423474625661352561826347361651332,-838442066190720239385936283948709/387998850010162462078593566713487,-930713632552731931854669041770003/438580159406531728735927659070062,0,-2662382051315823227432821454096327/1999073424935794946356952202306472,-853819649472531556941987776395057/1303821392263858372134061123173922,-16616654466592771089357379907323/5901745461923206181439973021473,-177602914497116110289244987475037/261036258360191737714509799678319,843580423007891323234545346409517/523951412947895408815523605557380,-20356059935560866960509607801117/63705657034023110377452590790244,-24375837395791716729326666520827/9102424464896945257519978842191,0}};
linealityF = map(QQ^5, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0},{1},{2},{3},{4},{5},{6},{7},{8},{9},{10},{11},{12},{13},{14},{15},{16},{17},{18},{19}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==1)
assert(ambDim F ==5)
assert(#(maxCones F) ==20)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 20})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 6, n_max_cones: 12
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2},{0,3},{0,4},{0,5},{1,2},{1,3},{1,4},{1,5},{2,4},{2,5},{3,4},{3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==12)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 6, 12})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 1, nrays: 5, n_max_cones: 5
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0,-1,-1},{0,1,0,-1,-2},{0,0,1,-1,-3}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0},{1},{2},{3},{4}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==1)
assert(ambDim F ==3)
assert(#(maxCones F) ==5)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 5})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 5, n_max_cones: 7
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0,-1,-1},{0,1,0,-1,-2},{0,0,1,-1,-3}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{4},{0,1},{0,2},{0,3},{1,2},{1,3},{2,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==7)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 5, 6})
assert(not isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,-1},{0,1,-1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{2},{0,1}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 6, n_max_cones: 8
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,-1,0,0,0,0},{0,0,1,-1,0,0},{0,0,0,0,1,-1}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0,2,4},{1,2,4},{0,3,4},{1,3,4},{0,2,5},{1,2,5},{0,3,5},{1,3,5}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==8)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(isComplete F)
assert(fVector F == {1, 6, 12, 8})
assert(isPolytopal F)
///

-- Test ambDim: 2, dim: 2, nrays: 3, n_max_cones: 2
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,-1},{0,1,-1}};
linealityF = map(QQ^2, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{2},{0,1}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==2)
assert(#(maxCones F) ==2)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 3, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 2, nrays: 5, n_max_cones: 7
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0,-1,-1},{0,1,0,-1,-2},{0,0,1,-1,-3}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{4},{0,1},{0,2},{0,3},{1,2},{1,3},{2,3}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==2)
assert(ambDim F ==3)
assert(#(maxCones F) ==7)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 5, 6})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 3, nrays: 5, n_max_cones: 5
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0,-1,-1},{0,1,0,-1,-2},{0,0,1,-1,-3}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{4},{0,3},{1,3},{2,3},{0,1,2}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==3)
assert(ambDim F ==3)
assert(#(maxCones F) ==5)
assert(isSmooth F)
assert(not isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 5, 6, 1})
assert(not isPolytopal F)
///

-- Test ambDim: 3, dim: 1, nrays: 5, n_max_cones: 5
-- Checking misc tests for fan
TEST ///
raysF = matrix {{1,0,0,-1,-1},{0,1,0,-1,-2},{0,0,1,-1,-3}};
linealityF = map(QQ^3, QQ^0, 0);
raysF = promote(raysF, QQ);
linealityF = promote(linealityF, QQ);
maxConesF = {{0},{1},{2},{3},{4}};
F = fan (raysF ,linealityF ,maxConesF);
assert(dim F ==1)
assert(ambDim F ==3)
assert(#(maxCones F) ==5)
assert(isSmooth F)
assert(isPure F)
assert(isSimplicial F)
assert(not isComplete F)
assert(fVector F == {1, 5})
assert(not isPolytopal F)
///

