-- Test /home/lars/polymake-source/apps/polytope/testsuite/face/c.poly
-- Checking latticePoints
TEST ///
verticesP = matrix {{-1,1,-1,1,-1,1,-1,1},{-1,-1,1,1,-1,-1,1,1},{-1,-1,-1,-1,1,1,1,1}};
desiredLP = matrix {{-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1},{-1,-1,-1,0,0,0,1,1,1,-1,-1,-1,0,0,0,1,1,1,-1,-1,-1,0,0,0,1,1,1},{-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1}};
P = convexHull(verticesP)
computedLP = latticePoints P;
assert(numColumns(desiredLP) == #computedLP);
///

