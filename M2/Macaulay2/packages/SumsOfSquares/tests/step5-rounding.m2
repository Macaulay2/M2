-- LOG.md 5.1 decisions 3 and 4 (step 5): the rounding in image coordinates
-- gives exact certificates where the forced parameters are not dyadic
-- (BUGS.md 1); for optimisation problems the continued fraction candidates
-- find simple rational optima, and the bound is monotone in RoundTol
-- (BUGS.md 13(g)).  The original gives 69/8, 5/16, real results, and bounds
-- below the optimum at fine RoundTol.
-- forced parameter t = 1/3 (BUGS.md 1)
R = QQ[x][t];
f = x^2 + 1 - 1/3*x^3 + t*x^3;
sol = solveSOS f;
assert(sol#Parameters == matrix(QQ, {{1/3}}));
assert(ring sol#GramMatrix === QQ);
-- sosInIdeal with multipliers involving non-dyadic values (BUGS.md 1)
R = QQ[x];
h = matrix{{x^3 + 3*x, x^2 - 7*x}};
(sol, mult) = sosInIdeal(h, 4);
assert(ring sol#GramMatrix === QQ and (h*mult)_(0,0) == value sosPoly sol);
-- simple rational optima, exactly
assert(first lowerBound((x^4 + 3)/(3*x^4 + 3)) == 1/3);
assert(first lowerBound((x^4 + 3)/(3*x^4 + 3), 2) == 1/3);
R = QQ[x,y];
assert(first lowerBound(x^2 + x*y + y^2 + x + 9) == 26/3);    -- Macaulay2 issue #4107
assert(first lowerBound(x^2 + x*y + y^2 + x + 2) == 5/3);
R = QQ[x];
assert(first lowerBound(x, matrix{{3*x - 1}}, 2) == 1/3);
-- monotone in RoundTol: the exact optimum at every fine precision (BUGS.md 13(g))
R = QQ[x][t];
for d in {3, 12, 26, 27, 30, 32, 40} do
    assert((solveSOS(x^2 - 3*x - t, -t, RoundTol => d))#Parameters == matrix(QQ, {{-9/4}}));
R = QQ[x,z];
bs = for d in {12, 24, 27, 30, 32} list first lowerBound(x^4 + x^2 + z^6 - 3*x^2*z^2, RoundTol => d);
assert(all(bs, b -> b == -729/4096));
