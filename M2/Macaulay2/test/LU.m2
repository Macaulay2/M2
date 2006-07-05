permutationMatrix = (p) -> (
     M := mutableZero(ZZ, #p, #p, Dense=>false);
     for i from 0 to #p-1 do M_(i,p#i) = 1;
     M)

M = matrix {{1.0, 3.0, 4.0, 5.0},{2.0, 3.0, 0.0, 1.0}}
M = mutableMatrix(M, Dense=>true)
(P,L,U) = LU M
P = permutationMatrix P
assert(0 == P * L * U - M)				    -- crashes
