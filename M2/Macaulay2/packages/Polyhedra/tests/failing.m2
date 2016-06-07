-- Test where isSmooth fails for cone with lineality space.
-- Checking isSmooth
TEST ///
C = posHull matrix {{1,1,-1,-1},{1,2,1,-1},{1,3,0,-1}};
assert isSmooth C
C = posHull {C,matrix{{1},{0},{1}}};
assert isSmooth C
///

