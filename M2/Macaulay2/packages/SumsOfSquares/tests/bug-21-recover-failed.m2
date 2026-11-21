-- BUGS.md 21: recoverSolution of a failed result raises "no method found".
-- Correct: {} (as the code intends).
R = QQ[x,y];
assert(recoverSolution solveSOS(x^2*y^2 - 1) == {})
assert(recoverSolution solveSOS(x^3) == {})
