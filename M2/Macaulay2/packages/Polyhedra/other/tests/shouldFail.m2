-- coneFromVData fails with wrong number of rows
TEST /// 
R = matrix {{1,0},{0,1}};
L = matrix {{}};
assert((try coneFromVData(R, L) else oops) === oops)
assert((try coneFromVData(L, R) else oops) === oops)
///

-- convexHull fails with wrong number of rows
TEST /// 
R = matrix {{1,0},{0,1}};
L = matrix {{}};
assert((try convexHull(R, L) else oops) === oops)
assert((try convexHull(L, R) else oops) === oops)
///

-- fan constructor fails with wrong number of rows
TEST /// 
R = matrix {{1,0},{0,1}};
L = matrix {{}};
MC = {{}};
assert((try fan(R, L, MC) else oops) === oops)
assert((try fan(L, R, MC) else oops) === oops)
///

-- intersection fails with wrong number of columns
TEST /// 
R = matrix {{1,0},{0,1}};
L = matrix {{}};
assert((try intersection(R, L) else oops) === oops)
assert((try intersection(L, R) else oops) === oops)
///

-- fanFromGfan constructor
TEST ///
R = matrix {{1,0},{0,1}};
L = matrix {{}};
assert((try fanFromGfan {R, L, {0,1}, 2, 1,1} else oops) === oops)
assert((try fanFromGfan {L, L, {0,1}, 2, 1,1, {1, 2, 1}} else oops) === oops)
///

-- regularSubdivision should fail for non-compact
TEST ///
P = convexHull(matrix{{0}}, matrix{{1}})
assert(not isCompact P)
assert((try regularSubdivision(P, matrix{{1}}) else oops) === oops)
///

-- PolyhedralComplex constructor should fail if there are more indices than vertices
TEST ///
M=matrix {{1,0,0},{1,1,0},{1,0,1},{1,1,1},{1,-1,-1}}
L={{0, 1, 2, 3}, {0, 4}}
assert((try P=polyhedralComplex(M,L) else oops) === oops)
///
