-- doc-FINDING-idealFromCharacter-variables.m2
-- FINDING: idealFromCharacter(R, rho) ignores which variables rho#"J" names and
-- builds the lattice ideal in the FIRST #J variables of R.
-- Documentation: "the lattice ideal corresponding to rho"; Caveat: "The
-- variables occurring in rho#"J" must be variables of R" (they are, here).
-- Expected: idealFromCharacter(R, partialCharacter I) == I meet k[J]; e.g. for
--   I = ideal(x - z^2, y^4) (cellular, cell variables {x,z}) this is (x - z^2).
-- Observed on the original package: ideal(-y^2+x)  (z replaced by y).
-- Hand-made characters: J = {y}, L = (1), c = {1} gives ideal(x-1,x-1)
-- instead of (y-1); J = {z,x}, L = (1,-1)^T, c = {2} gives ideal(x-2*y)
-- instead of (z-2*x).

R = QQ[x,y,z];
I = ideal(x - z^2, y^4);
pc = partialCharacter I;
assert(set pc#"J" === set {x,z});
assert(idealFromCharacter(R, pc) == ideal(x - z^2));
pc = new PartialCharacter from {"J" => {y}, "L" => matrix{{1}}, "c" => {1}};
assert(idealFromCharacter(R, pc) == ideal(y - 1));
pc = new PartialCharacter from {"J" => {z,x}, "L" => matrix{{1},{-1}}, "c" => {2}};
assert(idealFromCharacter(R, pc) == ideal(z - 2*x));
