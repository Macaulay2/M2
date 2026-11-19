-- binomialSolve lists every point as often as its multiplicity, the length of
-- the local ring at the point, so the list has deg I entries ([AISM]
-- Example 3; decided by Thomas on 2026-11-02).  The examples are those of the
-- independent tester's finding doc-FINDING-solve-multiplicity, which read the
-- documentation the other way (each point once).
R = QQ[x,y,z];
-- one point (0,0,1) of multiplicity 2
S1 = binomialSolve ideal(x^2, y, z - 1);
assert(#S1 == 2 and #unique S1 == 1 and toString first S1 == "{0, 0, 1}");
-- two points (0,0,1), (0,0,-1), each with local ring k[x,y]/(x^3,xy,y^2) of length 4
I2 = ideal(x^3, x*y, y^2, z^2 - 1);
S2 = binomialSolve I2;
assert(#S2 == degree I2 and #S2 == 8);
assert(set apply(unique S2, toString) === set {"{0, 0, 1}", "{0, 0, -1}"});
assert(all(unique S2, pt -> #select(S2, q -> q == pt) == 4));
-- three points (x a cube root of unity, y = x^2, z = 0), each of multiplicity 2
I3 = ideal(x*y - 1, x^2 - y, z^2);
S3 = binomialSolve I3;
assert(#S3 == degree I3 and #S3 == 6 and #unique S3 == 3);
assert(all(unique S3, pt -> #select(S3, q -> q == pt) == 2));
