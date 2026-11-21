-- BUGS.md 6: Number * SOSPoly does not promote the scalar, value then fails.
-- Correct: either an error at the multiplication (as Core does for 2.5*(x+1)
-- in QQ[x]) or a promotion to the coefficient field; the choice is open.
R = QQ[x];
s = sosPoly(R, {x+1}, {2});
r = try (2.5 * s) else null;
assert(r === null or (try (value r == 5/2 * value s) else false))
