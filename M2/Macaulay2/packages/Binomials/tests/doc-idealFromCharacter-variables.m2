-- doc-idealFromCharacter-variables.m2
-- idealFromCharacter(R, rho) builds the lattice ideal in the variables that
-- rho#"J" names, not in the first #J variables of R.
-- Documented: the variables in rho#"J" are matched with the variables of R
-- by name.
-- Expected: idealFromCharacter(R, partialCharacter I) == I meet k[J]; e.g. for
--   I = ideal(x - z^2, y^4) (cellular, cell variables {x,z}) this is (x - z^2).
-- Hand-made characters: J = {y}, L = (1), c = {1} gives (y-1); J = {z,x},
-- L = (1,-1)^T, c = {2} gives (z-2*x).

R = QQ[x,y,z];
I = ideal(x - z^2, y^4);
pc = partialCharacter I;
assert(set pc#"J" === set {x,z});
assert(idealFromCharacter(R, pc) == ideal(x - z^2));
pc = new PartialCharacter from {"J" => {y}, "L" => matrix{{1}}, "c" => {1}};
assert(idealFromCharacter(R, pc) == ideal(y - 1));
pc = new PartialCharacter from {"J" => {z,x}, "L" => matrix{{1},{-1}}, "c" => {2}};
assert(idealFromCharacter(R, pc) == ideal(z - 2*x));
