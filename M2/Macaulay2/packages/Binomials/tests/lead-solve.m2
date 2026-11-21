-- binomialSolve: every point of the variety, as often as its multiplicity.
load (Binomials#"auxiliary files" | "tests/helpers/oracles.m2");
R = QQ[x,y,z];
scan({
        ideal(x^2 - 1, y - 1, z - 1),
        ideal(x^3 - 1, y - x, z - 1),                 -- roots of unity of order 3
        ideal(x^4 - 1, y^3 - 1, z - x*y),             -- order 12
        ideal(x^2, x*y, y^2, z - 1),                  -- one point of multiplicity 3; version 1.2.1 returned 4 copies
        ideal(x^2, x*z, z^2, y - 1),
        ideal(y^2 - 1, x^2, x*y - x, z),              -- two points of multiplicities 2 and 1
        ideal(x^2 - x*z, x^3, z^2, y - 1),            -- version 1.2.1 stopped with an error
        ideal(x^2 - x*y, y^3, z - 1),
        ideal(x*y - x, x^2, y^3 - 1, z - 1),          -- version 1.2.1 returned x = 1
        ideal(x^2, y^3, z),                           -- only the origin, six times
        ideal(x, y, z),
        ideal(x^3 - x, y^2 - y, z^2 - z),             -- not cellular: 3 * 2 * 2 points
        ideal(x^2 - y^2, x*y - 1, z^2),               -- x^4 = 1, each point twice
        ideal(x^3 - x^2, y - x, z - x),               -- the origin twice and the point (1,1,1)
        ideal(x^2*y - x, x^3 - x, y^2 - 1, z^2 - z)
        }, I -> assert orcCheckSolve(I, binomialSolve I));
-- nilpotent coordinates are zero
s = binomialSolve ideal(y^2 - 1, x^2, x*y - x, z);
assert(all(s, p -> p#0 == 0 and p#2 == 0));
-- the documented example
S = QQ[x,y,z,w];
I = ideal(x - y, y - z, z*w - w, w^2 - x);
s = binomialSolve I;
assert(#s == 3 and orcCheckSolve(I, s));
-- the unit ideal has no points
assert(binomialSolve ideal(x - 1, x - 2, y, z, w) == {});
-- errors
assert(try (binomialSolve ideal(x - 2*y, y^2 - 1, z, w); false) else true);    -- not unital
assert(try (binomialSolve ideal(x - y, z, w); false) else true);               -- not zero-dimensional
