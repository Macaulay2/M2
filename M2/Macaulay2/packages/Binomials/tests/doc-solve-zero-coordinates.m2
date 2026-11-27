-- doc-solve-zero-coordinates.m2
-- binomialSolve on a zero-dimensional unital binomial ideal with nilpotent
-- variables: R = QQ[x,y,z]; I = ideal(x^2-x*y, y^3, z-1)  (dim I == 0,
-- V(I) = {(0,0,1)}).
-- (The same ideal with z instead of z-1 is in doc-solve-origin-shape.m2.)
-- Expected: every returned point is {0,0,1}.

R = QQ[x,y,z];
I = ideal(x^2-x*y, y^3, z-1);
assert(dim I == 0 and isBinomial I and isUnital I);
sols = try binomialSolve I else "error";
assert(instance(sols, List));
assert(all(sols, p -> p#0 == 0 and p#1 == 0 and p#2 == 1));
