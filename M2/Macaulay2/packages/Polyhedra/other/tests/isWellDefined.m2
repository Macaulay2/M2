-- Polyhedral complex constructor
TEST ///
P1 = convexHull transpose matrix {{0,0},{2,2}};
P2 = convexHull transpose matrix {{-1,-1},{1,1}};
PC = polyhedralComplex {P1, P2};
assert(not isWellDefined PC)
///

-- Fan constructor
TEST ///
R = transpose matrix {{1,0},{1,1},{0,1}};
L = {{0,2},{1,2}};
F = fan(R,L);
assert(not isWellDefined F)
///
