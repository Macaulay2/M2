-- mixedVolume of cube 2
TEST ///
C = hypercube 2;
assert(8 == mixedVolume({C, C}))
///


-- mixedVolume of two lines in 2dim space
TEST ///
P = convexHull transpose matrix {{0,0},{1,0}};
Q = convexHull transpose matrix {{0,0},{0,1}};
assert(1 == mixedVolume({P,Q}))
///
