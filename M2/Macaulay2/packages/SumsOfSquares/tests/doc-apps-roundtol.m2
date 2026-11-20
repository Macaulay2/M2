-- Tests written from the documentation alone (independent tester, applications).
-- Topic: the option RoundTol of lowerBound, sosInIdeal and sosdecTernary
-- (doc node RoundTol: "Higher values of RoundTol lead to better solutions",
-- "skip the rounding by setting RoundTol => infinity"; [JSAG] section 4 and
-- section 6 "Rounding tolerance"; [PP08]).
-- Checked: exact certificates for finite RoundTol, real results for
-- infinity, all bounds below the true minimum, the trade-off (bounds do not
-- get worse when RoundTol grows, up to the accuracy of the numerical
-- solution), agreement with the parametric formulation of [JSAG] section 4.

rtCert = (f, b, sol) -> (
    instance(b, QQ) and sol#GramMatrix =!= null and (
        s := sosPoly sol;
        f - b == s and all(coefficients s, c -> c > 0)));
rtNorm = g -> max(apply(terms g, t -> abs leadCoefficient t) | {0});

-- doc node example: f = x^4 + x^2 + z^6 - 3x^2z^2, the Motzkin polynomial
-- with y = 1; its minimum is 0 (at x = z = 0 and at x = +-1, z = +-1) -----
R = QQ[x,z];
f = x^4 + x^2 + z^6 - 3*x^2*z^2;
assert(f == library("Motzkin", {x, 1_R, z}));
assert(sub(f, {x => 1, z => 1}) == 0);
(b4, sol4) = lowerBound(f, RoundTol => 4);
assert(rtCert(f, b4, sol4) and b4 <= 0);
(b12, sol12) = lowerBound(f, RoundTol => 12);
assert(rtCert(f, b12, sol12) and b12 <= 0);
(binf, solinf) = lowerBound(f, RoundTol => infinity);
assert(instance(binf, RR));                                -- no rounding: a real number
assert(binf <= 1e-6);
-- (the comparisons allow the rounding grid 2^-12: the original's -729/4096
-- lies on it, BUGS.md 13(g); lead, after the test audit)
assert(b12 >= b4 - 2^-12);                                 -- higher RoundTol, better bound
assert(abs(binf - b12) < 2^-12 + 1e-6);                    -- RoundTol 12 is near the numerical optimum
assert(b12 >= -729/4096 - 2^-12 - 1e-6);                   -- as good as documented, up to the grid
-- the real decomposition of f - binf is numerically correct
sinf = sosPoly solinf;
assert(all(coefficients sinf, c -> c > 0));
Rr = ring value sinf;
assert(rtNorm(value sinf - (sub(f, Rr) - binf)) < 1e-6);
-- a sweep of RoundTol values: every bound certified, the sequence improves
-- (up to the accuracy 1e-6 of the numerical solution)
rtsM = {1, 2, 3, 4, 6, 8, 10, 12, 16, 20};
bs = apply(rtsM, rt -> (
    (b, s) := lowerBound(f, RoundTol => rt);
    print("Motzkin, RoundTol " | toString rt);
    assert(rtCert(f, b, s) and b <= 0);
    b));
-- monotone up to the grid of the finer precision (lead, after the test audit)
assert(all(toList(0 .. #bs - 2), i -> bs#(i+1) >= bs#i - 2^(-rtsM#(i+1)) - 1e-6));
assert(bs#(#bs - 1) >= binf - 1e-6);

-- [JSAG] section 4: lowerBound "internally declares a new parameter t and
-- optimizes f(x) - t": the parametric solveSOS gives the same value
Rt = QQ[x,z][t];
ft = library("Motzkin", {x, 1_Rt, z});
solt = solveSOS(ft - t, -t, RoundTol => 12);
assert(instance(solt#Parameters, Matrix));
assert(abs((solt#Parameters)_(0,0) - b12) < 2^-12 + 1e-6);

-- the rational function of the lowerBound doc node: bounds below the
-- minimum (1 - sqrt 2)/2 for every RoundTol, improving towards it --------
R = QQ[x];
g = (x^2 - x)/(x^2 + 1);
rts = {1, 3, 6, 12, 20};
rtgb = apply(rts, rt -> first lowerBound(g, RoundTol => rt));
assert(all(rtgb, b -> instance(b, QQ) and 1 - 2*b > 0 and (1 - 2*b)^2 >= 2));
assert(all(toList(0 .. #rtgb - 2), i -> rtgb#(i+1) >= rtgb#i - 2^(-rts#(i+1)) - 1e-6));
assert(abs(last rtgb - (1 - sqrt 2)/2) < 1e-5);
(rtgbinf, gsinf) = lowerBound(g, RoundTol => infinity);
assert(instance(rtgbinf, RR) and abs(rtgbinf - (1 - sqrt 2)/2) < 1e-6);

-- RoundTol for sosInIdeal: the documented identity h*mult == sosPoly sol
-- holds exactly for finite RoundTol and numerically for infinity -----------
R = QQ[x,y,z];
h = matrix {{x^2 - 4*x + 2*y^2, 2*z^2 - y^2 + 2}};
for rt in {3, 12} do (
    setRandomSeed 0;
    (sol, mult) = sosInIdeal(h, 2, RoundTol => rt);
    print("sosInIdeal, RoundTol " | toString rt);
    s = sosPoly sol;
    assert(h*mult == s and value s != 0 and all(coefficients s, c -> c > 0)));
setRandomSeed 0;
(solI, multI) = sosInIdeal(h, 2, RoundTol => infinity);
sI = sosPoly solI;
RI = ring value sI;
assert(all(coefficients sI, c -> c > 0));
dI = value sI - sub((sub(h, ring multI)*multI)_(0,0), RI);
assert(rtNorm(dI) < 1e-6 * rtNorm(value sI));
assert(rtNorm(value sI) > 1e-3);                           -- not the zero polynomial

-- RoundTol for sosdecTernary: f * prod q == prod p, exactly when all
-- factors are rational, else numerically ----------------------------------
R = QQ[x,y,z];
m = library("Motzkin", R);
RRR = RR[x,y,z];
for rt in {3, 12, infinity} do (
    setRandomSeed 0;
    (p, q) = sosdecTernary(m, RoundTol => rt);
    print("sosdecTernary, RoundTol " | toString rt);
    assert(instance(p, List) and instance(q, List) and #p > 0);
    assert(all(p | q, s -> instance(s, SOSPoly) and all(coefficients s, c -> c > 0)));
    if all(p | q, s -> ring s === R) then
        assert(m * product(q, s -> value s) == product(p, s -> value s))
    else (
        lhs = sub(m, RRR) * product(q, s -> sub(value s, RRR));
        rhs = product(p, s -> sub(value s, RRR));
        assert(rtNorm(lhs - rhs) < 1e-6 * rtNorm lhs)));
