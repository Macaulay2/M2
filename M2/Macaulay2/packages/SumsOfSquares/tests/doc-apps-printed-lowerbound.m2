-- Output fidelity (independent tester, applications): printed outputs of the
-- documentation nodes lowerBound, RoundTol, recoverSolution and of [JSAG]
-- sections 4 and 5, as computed by the Macaulay2 1.26.06 build
-- (share/doc/Macaulay2/SumsOfSquares/example-output/*.out).
-- These pin printed values, not mathematical properties: a renewed version
-- may legitimately change some of them (e.g. a better rounding); then the
-- decision is the maintainers'.  Wrong printed values are not pinned (the
-- recovered point .382683 of the rational example is not the minimizer,
-- see tests/open/doc-FINDING-recover-rational.m2).

-- lowerBound node, o4: (8, {x => -.999994}) and o5: true
R = QQ[x];
f = (x-1)^2 + (x+3)^2;
(bound, sol) = lowerBound f;
assert(bound === 8_QQ or (instance(bound, QQ) and bound == 8));
pt = recoverSolution sol;
-- the printed value depends on the solver and the machine (-.999816 on an
-- Apple M5, CSDP 6.2.0); the minimizer is x = -1
assert(#pt == 1 and abs(pt#0#1 + 1) < 1e-3);
assert(f - bound == sosPoly sol);
-- o9: -1 (QQ) and o10: true
R = QQ[x,y]/ideal(x^2 - x, y^2 - y);
f = x - y;
(bound, sol) = lowerBound(f, 2);
assert(instance(bound, QQ) and bound == -1);
assert(f - bound == sosPoly sol);
-- o15: -1 (QQ) and o16: true
R = QQ[x,y];
f = x - y;
h = matrix{{x^2 - x, y^2 - y}};
(bound, sol, mult) = lowerBound(f, h, 2);
assert(instance(bound, QQ) and bound == -1);
assert(f - bound + h*mult == sosPoly sol);
-- o20: (-1/4, ...)
R = QQ[x];
f = (x^2 - x)/(x^2 + 1);
(bound, sol) = lowerBound f;
assert(instance(bound, QQ) and bound == -1/4);

-- RoundTol node: o4 = -3/16, o6 = -729/4096, o8 = -.1779785174727856 (RR)
R = QQ[x,z];
f = x^4 + x^2 + z^6 - 3*x^2*z^2;
(bound, sol) = lowerBound(f, RoundTol => 4);
-- (lead, step 5: the rounding in image coordinates gives -23/128 here, a
-- better certified bound than the documented -3/16, LOG.md 5.1 decision 3;
-- pinned now: exact, at least -3/16, at most the optimum -729/4096)
assert(instance(bound, QQ) and bound >= -3/16 and bound <= -729/4096);
(bound, sol) = lowerBound(f, RoundTol => 12);
assert(instance(bound, QQ) and bound == -729/4096);
(bound, sol) = lowerBound(f, RoundTol => infinity);
-- (lead, step 5: as in doc-core-printed-roundtol.m2, LOG.md 5.2)
assert(instance(bound, RR) and abs(bound + 729/4096) < 1e-7);

-- [JSAG] section 4: t = -729/4096; sol#Parameters = | -729/4096 |
R = QQ[x,z];
f = library("Motzkin", {x, 1_R, z});
(t, sol) = lowerBound(f, RoundTol => 12);
assert(t == -729/4096);
Rt = QQ[x,z][tt];
solt = solveSOS(library("Motzkin", {x, 1_Rt, z}) - tt, -tt, RoundTol => 12);
assert(solt#Parameters == matrix{{-729/4096}});

-- recoverSolution node: o4 = {x => 0, y => 1}
R = RR[x,y];
mon = matrix {{1},{x},{y}};
X = matrix(RR, {{1,0,1},{0,0,0},{1,0,1}});
sol = recoverSolution(mon, X);
assert(#sol == 2 and toString sol#0#0 == "x" and toString sol#1#0 == "y");
assert(abs(sol#0#1) < 1e-12 and abs(sol#1#1 - 1) < 1e-12);
