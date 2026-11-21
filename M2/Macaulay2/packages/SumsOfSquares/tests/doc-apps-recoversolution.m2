-- Tests written from the documentation alone (independent tester, applications).
-- Topic: recoverSolution(mon,X) and recoverSolution(sdpsol) (doc nodes
-- recoverSolution and lowerBound; [JSAG] section 5).
-- "checking if the moment matrix is rank one.  In this case it factorizes
-- the matrix and returns the defining vector."  Tested on rank one moment
-- matrices X = v v^T with v = (1, monomials evaluated at a point), and on
-- minimizers found by lowerBound where the optimum is unique.
-- Cases with several minimizers and rational functions: see tests/open/.

rsVal = (pt, name) -> (
    sel := select(pt, o -> toString o#0 == name);
    assert(#sel == 1);
    sel#0#1);
rsMoment = (mon, a) -> (
    -- the rank one moment matrix of the point a (a list of options var => value)
    v := sub(mon, apply(a, o -> o#0 => o#1 * 1.0));
    v = lift(v, RR);
    v * transpose v);

-- doc example -------------------------------------------------------------
R = RR[x,y];
mon = matrix {{1},{x},{y}};
X = matrix(RR, {{1,0,1},{0,0,0},{1,0,1}});
sol = recoverSolution(mon, X);
assert(instance(sol, List) and #sol == 2);
assert(all(sol, o -> instance(o, Option)));
assert(abs(rsVal(sol, "x") - 0) < 1e-9 and abs(rsVal(sol, "y") - 1) < 1e-9);
-- the keys are the variables of the ring of mon
assert(set apply(sol, o -> toString o#0) === set {"x", "y"});

-- more rank one moment matrices: the point is recovered (the origin itself:
-- see tests/open/doc-FINDING-recover-origin.m2)
for a in {{x => 2, y => -3}, {x => -1/2, y => 1/3}, {x => 0, y => 1}, {x => 10, y => 0.25}, {x => 1e-3, y => 0}} do (
    print("rank one moment matrix at " | toString a);
    s := recoverSolution(mon, rsMoment(mon, a));
    assert(abs(rsVal(s, "x") - a#0#1) < 1e-6 and abs(rsVal(s, "y") - a#1#1) < 1e-6));
-- monomials of degree two
mon2 = matrix {{1},{x},{y},{x^2},{x*y},{y^2}};
s2 = recoverSolution(mon2, rsMoment(mon2, {x => 2, y => -1}));
assert(abs(rsVal(s2, "x") - 2) < 1e-6 and abs(rsVal(s2, "y") + 1) < 1e-6);
-- three variables
R3 = RR[a,b,c];
mon3 = matrix {{1},{a},{b},{c}};
s3 = recoverSolution(mon3, rsMoment(mon3, {a => 1.5, b => -2, c => 0.5}));
assert(abs(rsVal(s3, "a") - 1.5) < 1e-6 and abs(rsVal(s3, "b") + 2) < 1e-6 and abs(rsVal(s3, "c") - 0.5) < 1e-6);
-- a moment matrix with rational entries
R = QQ[x,y];
monq = matrix {{1},{x},{y}};
s4 = recoverSolution(monq, matrix(QQ, {{1, 2, -3}, {2, 4, -6}, {-3, -6, 9}}));
assert(abs(rsVal(s4, "x") - 2) < 1e-6 and abs(rsVal(s4, "y") + 3) < 1e-6);

-- recoverSolution(sdpsol) agrees with recoverSolution(mon, X) on the fields
R = QQ[x];
f = (x-1)^2 + (x+3)^2;
(bound, sol) = lowerBound f;
p1 = recoverSolution sol;
p2 = recoverSolution(sol#Monomials, sol#MomentMatrix);
assert(abs(rsVal(p1, "x") - rsVal(p2, "x")) < 1e-9);
assert(abs(rsVal(p1, "x") + 1) < 1e-3);                     -- unique minimizer x = -1

-- minimizers from lowerBound (unique optimum) -------------------------------
-- (x-a)^2 + (y-b)^2 + 1 with a rational center
R = QQ[x,y];
g = (x - 3/2)^2 + (y + 2/3)^2 + 1;
(bg, solg) = lowerBound g;
pg = recoverSolution solg;
assert(abs(rsVal(pg, "x") - 3/2) < 1e-3 and abs(rsVal(pg, "y") + 2/3) < 1e-3);
-- the recovered point attains the bound
assert(abs(sub(g, apply(pg, o -> o#0 => o#1)) - bg) < 1e-3);
-- quartic with unique minimizer: x^4 + y^4 + xy + x (minimizer about (-0.7318, 0.5677))
g2 = x^4 + y^4 + x*y + x;
(b2, sol2) = lowerBound g2;
p2 = recoverSolution sol2;
assert(abs(rsVal(p2, "x") + 0.731813) < 1e-3 and abs(rsVal(p2, "y") - 0.567693) < 1e-3);
assert(abs(sub(g2, apply(p2, o -> o#0 => o#1)) - b2) < 1e-2);
-- [BPT13] Exercise 3.54, unique minimizer about -3.17467
R = QQ[x];
(b3, sol3) = lowerBound(x^4 - 20*x^2 + x);
assert(abs(rsVal(recoverSolution sol3, "x") + 3.17467) < 1e-3);

-- constrained problems, unique minimizer ------------------------------------
-- [JSAG] section 5: f = x - y on {0,1}^2 with equations: {x => 0, y => 1}
R = QQ[x,y];
(bh, solh, multh) = lowerBound(x - y, matrix{{x^2 - x, y^2 - y}}, 2);
ph = recoverSolution solh;
assert(abs(rsVal(ph, "x")) < 1e-3 and abs(rsVal(ph, "y") - 1) < 1e-3);
-- the same in the quotient ring
S = QQ[x,y]/ideal(x^2 - x, y^2 - y);
(bq, solq) = lowerBound(x - y, 2);
pq = recoverSolution solq;
assert(abs(rsVal(pq, "x")) < 1e-3 and abs(rsVal(pq, "y") - 1) < 1e-3);
-- x + 2y on {-1,1}^2: unique minimizer (-1,-1)
S = QQ[x,y]/ideal(x^2 - 1, y^2 - 1);
(bc, solc) = lowerBound(x + 2*y, 2);
assert(bc <= -3 and bc >= -3 - 1e-2);
pc = recoverSolution solc;
assert(abs(rsVal(pc, "x") + 1) < 1e-3 and abs(rsVal(pc, "y") + 1) < 1e-3);
