-- Tests written from the documentation alone (independent tester, applications).
-- Topic: sosdecTernary (doc node sosdecTernary; [JSAG] section 3; [dKP04];
-- library forms from [Rez00]).
-- "Given a nonnegative ternary form f, this method uses Hilbert's algorithm
-- to compute a decomposition of f as f = prod p_i / prod q_i where each
-- factor p_i and q_i is a sum of squares.  The method returns null if f is
-- not nonnegative."  Caveat: some factors may be real, some rational.
-- Checked: f * prod value q == prod value p (exactly when all factors are
-- rational, else relative error 1e-6 in RR[x,y,z]), positive weights, the
-- types, degrees, a denominator of positive degree for forms that are not
-- SOS, a null result for forms that are negative somewhere.
-- setRandomSeed before each call.

RRR = RR[x,y,z];
tnNorm = g -> max(apply(terms g, t -> abs leadCoefficient t) | {0});
tnCheck = (f, name) -> (
    -- runs sosdecTernary and checks the documented decomposition
    setRandomSeed 0;
    r := sosdecTernary f;
    print("sosdecTernary: " | name);
    assert(instance(r, Sequence) and #r == 2);
    (p, q) := r;
    assert(instance(p, List) and instance(q, List));
    assert(#p >= 1);
    assert(all(p | q, s -> instance(s, SOSPoly)));
    assert(all(p | q, s -> all(coefficients s, c -> c > 0)));
    assert(all(p | q, s -> value s != 0));
    assert(all(p | q, s -> isHomogeneous value s));
    -- degrees add up
    assert((first degree f) + sum(q, s -> first degree value s) == sum(p, s -> first degree value s));
    if coefficientRing ring f === QQ and all(p | q, s -> ring value s === ring f) then
        assert(f * product(q, s -> value s) == product(p, s -> value s))
    else (
        lhs := sub(f, RRR) * product(q, s -> sub(value s, RRR));
        rhs := product(p, s -> sub(value s, RRR));
        assert(tnNorm(lhs - rhs) < 1e-6 * tnNorm lhs));
    (p, q));
tnIsNull = f -> (
    setRandomSeed 0;
    r := sosdecTernary f;
    r === null or (instance(r, Sequence) and all(r, e -> e === null)));

-- doc example: the Motzkin form over RR -------------------------------------
R = RR[x,y,z];
f = library("Motzkin", {x,y,z});
(p, q) = tnCheck(f, "Motzkin over RR");
assert(clean(1e-4, f * value q#0 - value p#0) == 0);       -- the documented check
-- Motzkin is not SOS, so a denominator of positive degree is needed
assert(any(q, s -> first degree value s > 0));

-- [JSAG] section 3: the Motzkin form over QQ, f * value(den) == value(num) --
R = QQ[x,y,z];
f = library("Motzkin", {x,y,z});
(p, q) = tnCheck(f, "Motzkin over QQ");
assert(any(q, s -> first degree value s > 0));
-- (compared only when both factors are rational: tnCheck covers the real
-- case; lead, after the test audit)
if #p == 1 and #q == 1 and ring value q#0 === R and ring value p#0 === R then
    assert(f * value(q#0) == value(p#0));

-- other nonnegative ternary sextics that are not SOS -------------------------
for name in {"Robinson", "Schmuedgen"} do (
    (p, q) = tnCheck(library(name, R), name);
    assert(any(q, s -> first degree value s > 0)));
-- the Choi-Lam form S(x,y,z) = x^4y^2 + y^4z^2 + z^4x^2 - 3x^2y^2z^2 [Rez00]
(p, q) = tnCheck(x^4*y^2 + y^4*z^2 + z^4*x^2 - 3*x^2*y^2*z^2, "Choi-Lam S");
assert(any(q, s -> first degree value s > 0));
-- the Robinson form over RR
(p, q) = tnCheck(library("Robinson", {RRR_0, RRR_1, RRR_2}), "Robinson over RR");

-- forms that are SOS: degree 2 and 4 (every nonnegative ternary quartic is
-- SOS), the Scheiderer quartic (SOS over RR but not over QQ), degree 6, 8
(p, q) = tnCheck(x^2 + y^2 + z^2, "x^2+y^2+z^2");
(p, q) = tnCheck(x^2 + 2*x*y + 3*y^2 + z^2, "positive definite quadric");
(p, q) = tnCheck((x^2 + y^2 + z^2)^2, "(x^2+y^2+z^2)^2");
(p, q) = tnCheck(x^4 + y^4 + z^4, "x^4+y^4+z^4");
(p, q) = tnCheck((x^2 - y^2)^2 + (y*z)^2, "(x^2-y^2)^2+(yz)^2");
(p, q) = tnCheck(library("Scheiderer", R), "Scheiderer");
(p, q) = tnCheck(x^6 + y^6 + z^6, "x^6+y^6+z^6");
(p, q) = tnCheck(library("Motzkin", R) * (x^2 + y^2 + z^2), "Motzkin*(x^2+y^2+z^2), degree 8");
(p, q) = tnCheck(library("Motzkin", R) + (x^2 + y^2 + z^2)^3/100, "Motzkin + small positive, degree 6");

-- forms that are negative somewhere: null ----------------------------------
assert(tnIsNull(x^2 - y^2));
assert(tnIsNull(x^2 + y^2 - z^2));
assert(tnIsNull(library("Motzkin", R) - z^6/2));             -- value -1/2 at (1,1,1)
assert(tnIsNull(x^4 + y^4 + z^4 - 4*x^2*y^2));               -- value -2 at (1,1,0)
assert(tnIsNull(-(x^2 + y^2 + z^2)));

-- the seed does not affect the documented properties
for sd in {1, 99} do (
    setRandomSeed sd;
    (p, q) = sosdecTernary library("Motzkin", R);
    print("Motzkin, seed " | toString sd);
    assert(instance(p, List) and instance(q, List));
    if all(p | q, s -> ring value s === R) then
        assert(library("Motzkin", R) * product(q, s -> value s) == product(p, s -> value s)));
