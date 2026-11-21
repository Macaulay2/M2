-- BUGS.md 4: RoundTol => 33 raises "expected true or false".  Correct: a
-- result (rounded at 33 bits, or the real one).
R = QQ[x,y];
sol = solveSOS(x^2 + y^2, RoundTol => 33);
assert(sol#GramMatrix =!= null)
(b, sol) = lowerBound(x^2 + 1, RoundTol => 33);
assert(b =!= null)
setRandomSeed 0;
(sol, mult) = sosInIdeal(matrix{{x + 1}}, 2, RoundTol => 33);
assert(sol#GramMatrix =!= null)
