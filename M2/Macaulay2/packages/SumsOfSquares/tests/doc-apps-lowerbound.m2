-- Tests written from the documentation alone (independent tester, applications).
-- Topic: lowerBound(f) and lowerBound(f,D) for polynomials (no constraints).
-- Sources: doc node lowerBound; [JSAG] sections 4 and 5; [BPT13] section 3.2
-- (Exercises 3.54, 3.57); [P05] Example 6 (dehomogenized Robinson polynomial).
-- Checked properties: shapes (bound, SDPResult); exact certificates
-- f - bound == sosPoly sol with positive weights; bound <= true minimum
-- (exact where possible); closeness to the known minimum / SOS optimum;
-- consistency between lowerBound(f) and lowerBound(f,D); scale and shift
-- covariance; RR input; inputs without a lower bound.

-- helpers -----------------------------------------------------------------
lbCertified = (f, b, sol) -> (
    instance(b, QQ) and sol#GramMatrix =!= null and (
        s := sosPoly sol;
        f - b == s and all(coefficients s, c -> c > 0)));
lbRealRoots = g -> apply(select(roots g, r -> abs imaginaryPart r < 1e-8), realPart);
lbAt = (f, pt) -> sub(f, apply(pt, o -> o#0 => o#1));
lbCoeffNorm = g -> max(apply(terms g, t -> abs leadCoefficient t) | {0});
lbNoBound = f -> (
    r := try lowerBound f else null;
    r === null or not instance(r#0, Number));

-- doc example: f = (x-1)^2 + (x+3)^2 = 2(x+1)^2 + 8, minimum 8 at x = -1 ---
R = QQ[x];
f = (x-1)^2 + (x+3)^2;
r = lowerBound f;
assert(instance(r, Sequence) and #r == 2);
(bound, sol) = r;
assert(instance(sol, SDPResult));
assert(instance(bound, QQ));
assert(f - bound == sosPoly sol);                        -- documented identity
assert(lbCertified(f, bound, sol));
assert(bound <= sub(f, x => -1));                        -- exact: bound <= min = 8
assert(bound == 8);                                      -- tight (univariate: psos = min)
pt = recoverSolution sol;
assert(instance(pt, List) and #pt == 1);
assert(abs(pt#0#1 - (-1)) < 1e-3);                       -- minimizer x = -1
assert(abs(lbAt(f, pt) - bound) < 1e-4);

-- lowerBound(f,D) with large D agrees with lowerBound(f)
for D in {2, 4, 6} do (
    (bD, solD) = lowerBound(f, D);
    print("doc example with D = " | toString D);
    assert(lbCertified(f, bD, solD));
    -- 8 up to the rounding grid of the default RoundTol (the original snaps
    -- onto 8 exactly; lead, after the test audit)
    assert(bD <= 8 and bD >= 8 - 2^-3));

-- [BPT13] Exercise 3.54: p = x^4 - 20x^2 + x (univariate, so psos = min) ---
p = x^4 - 20*x^2 + x;
crit = lbRealRoots diff(x, p);
pmin = min apply(crit, c -> sub(p, x => c));             -- about -103.169 at x = -3.1747
(bp, solp) = lowerBound p;
assert(lbCertified(p, bp, solp));
assert(bp <= sub(p, x => -317467/100000));               -- exact upper bound on the minimum
assert(bp <= pmin + 1e-9);
assert(bp >= pmin - 1e-2 * abs pmin);
ptp = recoverSolution solp;
assert(abs(ptp#0#1 - (-3.1747)) < 1e-3);
assert(abs(lbAt(p, ptp) - pmin) < 1e-3);
(bpR, solpR) = lowerBound(p, RoundTol => infinity);
assert(instance(bpR, RR));
assert(abs(bpR - pmin) < 1e-4 * abs pmin);

-- multiple minimizers: (x^2-1)^2, minimum 0 at x = 1 and x = -1 ------------
q = (x^2 - 1)^2;
(bq, solq) = lowerBound q;
assert(lbCertified(q, bq, solq));
assert(bq <= 0 and bq >= -1e-2);

-- constants and the zero polynomial
(b5, sol5) = lowerBound(5_R);
assert(instance(b5, QQ) and b5 <= 5 and b5 >= 5 - 1e-2);
assert(5_R - b5 == sosPoly sol5);
(b0, sol0) = lowerBound(0_R);
assert(instance(b0, QQ) and b0 <= 0 and b0 >= -1e-2);

-- inputs without a lower bound: no number may be returned as a lower bound
R = QQ[x,y];
for g in {x^3, x*y, x, x^3 + y^2, -x^2 - y^2} do (
    print("unbounded input " | toString g);
    assert(lbNoBound g));

-- infimum not attained (minimum at infinity): x^2 + (1-xy)^2 has inf 0 -----
g = x^2 + (1 - x*y)^2;
(bg, solg) = lowerBound g;
assert(lbCertified(g, bg, solg));
assert(bg <= 0);                                         -- g(1/n, n) = 1/n^2 -> 0
assert(bg >= -1e-2);

-- bivariate quartic g2 = x^4 + y^4 + xy + x: nonnegative bivariate quartics
-- are SOS (Hilbert), so psos = min.  Critical points: x = -4y^3 and
-- -256y^9 + y + 1 = 0.
g2 = x^4 + y^4 + x*y + x;
S1 = QQ[t];
ys = lbRealRoots(-256*t^9 + t + 1);
g2min = min apply(ys, c -> (xx := -4*c^3; xx^4 + c^4 + xx*c + xx));   -- about -0.756583
(b2, sol2) = lowerBound g2;
assert(lbCertified(g2, b2, sol2));
assert(b2 <= sub(g2, {x => -731813/1000000, y => 567693/1000000}));  -- exact
assert(b2 >= g2min - 1e-2);
pt2 = recoverSolution sol2;
assert(abs(lbAt(g2, pt2) - g2min) < 1e-3);
-- lowerBound(f,D) for D >= deg f agrees with lowerBound(f)
for D in {4, 6} do (
    (bD, solD) = lowerBound(g2, D);
    print("quartic with D = " | toString D);
    assert(lbCertified(g2, bD, solD));
    assert(abs(bD - b2) < 1e-2));
-- scale and shift covariance (numerical bounds)
(bR, solR) = lowerBound(g2, RoundTol => infinity);
assert(abs(bR - g2min) < 1e-4);
(b3R, sol3R) = lowerBound(3*g2, RoundTol => infinity);
assert(abs(b3R - 3*bR) < 1e-4);
(b5R, sol5R) = lowerBound(g2 + 5, RoundTol => infinity);
assert(abs(b5R - (bR + 5)) < 1e-4);
(b7R, sol7R) = lowerBound(g2/7, RoundTol => infinity);
assert(abs(b7R - bR/7) < 1e-4);
-- and the rounded bounds of the scaled problems are certified lower bounds
(b3, sol3) = lowerBound(3*g2);
assert(lbCertified(3*g2, b3, sol3) and b3 <= 3*g2min + 1e-9 and b3 >= 3*g2min - 3e-2);
(b5s, sol5s) = lowerBound(g2 + 5);
assert(lbCertified(g2 + 5, b5s, sol5s) and b5s <= g2min + 5 + 1e-9 and b5s >= g2min + 5 - 1e-2);

-- RR coefficients: a real bound and a real decomposition of f - bound ------
RRx = RR[x,y];
g2R = sub(g2, RRx);
(bRR, solRR) = lowerBound g2R;
assert(instance(bRR, RR));
assert(abs(bRR - g2min) < 1e-4);
sRR = sosPoly solRR;
assert(all(coefficients sRR, c -> c > 0));
assert(lbCoeffNorm(sub(value sRR, RRx) - (g2R - bRR)) < 1e-6);

-- [BPT13] Exercise 3.57: x^4+y^4+z^4-4xyz+2x+3y+4z; psos equals the minimum
R = QQ[x,y,z];
p3 = x^4 + y^4 + z^4 - 4*x*y*z + 2*x + 3*y + 4*z;
(b3d, sol3d) = lowerBound p3;
assert(lbCertified(p3, b3d, sol3d));
pt3 = recoverSolution sol3d;
assert(#pt3 == 3);
v3 = lbAt(p3, pt3);                                      -- about -7.18493
assert(b3d <= v3 + 1e-9);
assert(v3 - b3d < 1e-2 * abs v3);                        -- relaxation is tight here
pt3q = apply(pt3, o -> o#0 => round(10^6 * o#1) / 10^6);
assert(b3d <= sub(p3, pt3q));                            -- exact
grad = apply(gens R, v -> lbAt(diff(v, p3), pt3));
assert(all(grad, c -> abs c < 1e-3));                    -- recovered point is critical

-- [P05] Example 6: the dehomogenized Robinson polynomial r.  r >= 0 with
-- r(1,1) = 0, but r is not SOS; r + 3825/4096 is SOS ([P05], up to the sign
-- of the constant), so psos >= -3825/4096; numerically psos is about
-- -3825/4096 (the solver's value is 5.8e-6 below it; lead, after the test
-- audit: equality is not proved here).
R = QQ[x,y];
r6 = x^6 + y^6 - x^4*y^2 - y^4*x^2 - x^4 - y^4 - x^2 - y^2 + 3*x^2*y^2 + 1;
assert(r6 == library("Robinson", {x, y, 1_R}));
assert(sub(r6, {x => 1, y => 1}) == 0);
assert(r6 + 3825/4096 == 1/4*(x^2 + y^2 - 89/32)^2 + (x^3 - y^2*x - 5/8*x)^2 + (y^3 - x^2*y - 5/8*y)^2);
(b6, sol6) = lowerBound r6;
assert(lbCertified(r6, b6, sol6));
assert(b6 <= 0);
assert(b6 <= -3825/4096 + 1e-4);                         -- the SOS gap shows
assert(b6 >= -3825/4096 - 2e-2);
(b6R, sol6R) = lowerBound(r6, RoundTol => infinity);
assert(abs(b6R + 3825/4096) < 1e-4);
