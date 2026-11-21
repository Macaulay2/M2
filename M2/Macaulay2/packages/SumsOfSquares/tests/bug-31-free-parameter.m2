-- BUGS.md 31 (found in step 3): a parameter that does not change the
-- polynomial (it does not occur, or a combination of the multiples is 0)
-- makes the solver fail ("opening input file failed").  Correct: the
-- parameter is free; with an objective on it the problem is unbounded.
R = QQ[x]; S = R[t];
f = sub(x^2 + 1, S);                       -- t does not occur
sol = solveSOS f;
assert(sol#Status == "SDP solved, primal-dual feasible" and sol#GramMatrix =!= null)
assert(value sosPoly sol == x^2 + 1)
sol = solveSOS(f, matrix{{1_R}, {x_R}});
assert(sol#GramMatrix =!= null)
-- an objective on the free parameter is unbounded: no Gram matrix
sol = solveSOS(f, -t);
assert(sol#GramMatrix === null and sol#Parameters === null)
-- two parameters, one free
T = QQ[x][s,t];
g = x^2 + 1 + s*x^2;
sol = solveSOS g;
assert(sol#GramMatrix =!= null)
p = sol#Parameters;
assert(p =!= null and numRows p == 2)
assert(sub(g, {s => p_(0,0), t => p_(1,0)}) == (transpose sol#Monomials * sol#GramMatrix * sol#Monomials)_(0,0))
-- dependent constraints in lowerBound: the multipliers of h1, h2 = 2 h1 are not unique
U = QQ[x];
(b, sol, mult) = lowerBound(x, matrix{{x^2 - 1, 2*x^2 - 2}}, 2);
assert(b == -1)
assert(x - b + (matrix{{x^2 - 1, 2*x^2 - 2}} * mult)_(0,0) == value sosPoly sol)
