-- doc-FINDING-solve-crash.m2
-- FINDING: binomialSolve crashes on a zero-dimensional unital binomial ideal.
-- Reproduction: R = QQ[x,y,z]; I = ideal(x^2-x*y, y^3, z-1)  (dim I == 0,
-- V(I) = {(0,0,1)}).
-- Observed: error: no method for binary operator * applied to objects:
--            1 (of class ZZ)  *  null (of class Nothing)
-- (The same ideal with z instead of z-1 does not crash but triggers
-- doc-FINDING-solve-origin-shape.m2.)
-- Expected: the single point {0,0,1}.

R = QQ[x,y,z];
I = ideal(x^2-x*y, y^3, z-1);
assert(dim I == 0 and isBinomial I and isUnital I);
sols = try binomialSolve I else "error";
assert(instance(sols, List));
assert(all(sols, p -> p#0 == 0 and p#1 == 0 and p#2 == 1));
