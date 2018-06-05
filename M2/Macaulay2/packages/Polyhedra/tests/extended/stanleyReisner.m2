-- Test minimalNonFaces on the normal fan of a square 
TEST ///
Phi = normalFan hypercube 2;
S = minimalNonFaces Phi; 
assert(S#0 == {0,1})
assert(S#1 == {2,3})
///


-- Test minimalNonFaces on the toric fan of the projective plane 
TEST ///
C0 = coneFromVData matrix{ {1,0},{0,1} }; 
C1 = coneFromVData matrix{ {0,-1},{1,-1} }; 
C2 = coneFromVData matrix{ {-1,1}, {-1,0} }; 
assert(minimalNonFaces fan C0 == {}) 
assert(minimalNonFaces addCone({C1,C2}, fan C0) == {{0,1,2}})
///


---- Test stanleyReisnerRing on the normal fan of a square
--TEST ///
--Phi = normalFan hypercube 2;
--S = stanleyReisnerRing Phi;
--A = QQ[]
--assert(S == A)
--///
