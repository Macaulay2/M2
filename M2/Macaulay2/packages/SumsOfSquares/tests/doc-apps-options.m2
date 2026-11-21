-- Tests written from the documentation alone (independent tester, applications).
-- Topic: the options Verbosity and Solver of lowerBound, sosInIdeal and
-- sosdecTernary (doc nodes Verbosity: levels 0, 1, 2 control only what is
-- printed; Solver: "change the semidefinite programming solver used in the
-- background", CSDP the default; caveat of sosInIdeal and sosdecTernary:
-- only "CSDP" and "MOSEK" work).
-- Checked: every documented level is accepted and the mathematical result
-- does not depend on it; Solver => "CSDP" gives the same results as the
-- default; with Solver => "M2" any certificate that lowerBound returns is
-- valid (the "M2" solver is documented as rudimentary, so no result is
-- required from it).

opCert = (f, b, sol) -> (
    instance(b, QQ) and sol#GramMatrix =!= null and f - b == sosPoly sol
    and all(coefficients sosPoly sol, c -> c > 0));

-- lowerBound(f) -------------------------------------------------------------
R = QQ[x];
f = (x-1)^2 + (x+3)^2;
lbs = apply({0, 1, 2}, v -> lowerBound(f, Verbosity => v));
assert(all(lbs, r -> opCert(f, r#0, r#1) and r#0 == 8));
(bC, solC) = lowerBound(f, Solver => "CSDP");
assert(opCert(f, bC, solC) and bC == 8);
-- rational function
g = (x^2 - x)/(x^2 + 1);
gbs = apply({0, 1, 2}, v -> first lowerBound(g, Verbosity => v));
assert(all(gbs, b -> b == gbs#0));
assert(first lowerBound(g, Solver => "CSDP") == first lowerBound g);
-- with the M2 solver: if a bound and a Gram matrix are returned, they certify
(bM, solM) = lowerBound(f, Solver => "M2");
assert(bM === null or solM#GramMatrix === null or opCert(f, bM, solM));

-- lowerBound(f,D) in a quotient ring ------------------------------------------
S = QQ[x,y]/ideal(x^2 - x, y^2 - y);
f = x - y;
qbs = apply({0, 1, 2}, v -> lowerBound(f, 2, Verbosity => v));
assert(all(qbs, r -> opCert(f, r#0, r#1) and r#0 == -1));
(bq, solq) = lowerBound(f, 2, Solver => "CSDP");
assert(opCert(f, bq, solq) and bq == -1);
(bqM, solqM) = lowerBound(f, 2, Solver => "M2");
assert(bqM === null or solqM#GramMatrix === null or opCert(f, bqM, solqM));

-- lowerBound(f,h,D) -------------------------------------------------------------
R = QQ[x,y];
f = x - y;
h = matrix{{x^2 - x, y^2 - y}};
for v in {0, 1, 2} do (
    (b, sol, mult) = lowerBound(f, h, 2, Verbosity => v);
    print("lowerBound(f,h,D), Verbosity " | toString v);
    assert(b == -1 and f - b + h*mult == sosPoly sol));
(b, sol, mult) = lowerBound(f, h, 2, Solver => "CSDP");
assert(b == -1 and f - b + h*mult == sosPoly sol);
(b, sol, mult) = lowerBound(f, h, 2, Solver => "M2");
assert(b === null or sol#GramMatrix === null or (instance(b, QQ) and f - b + h*mult == sosPoly sol));

-- sosInIdeal(h,D) and sosInIdeal(S,D) -------------------------------------------
R = QQ[x,y,z];
h = matrix {{x^2 - 4*x + 2*y^2, 2*z^2 - y^2 + 2}};
w = (x/2 - 1)^2 + z^2;
for v in {0, 1, 2} do (
    setRandomSeed 0;
    (sol, mult) = sosInIdeal(h, 2, Verbosity => v);
    print("sosInIdeal(h,D), Verbosity " | toString v);
    s := sosPoly sol;
    assert(h*mult == s and value s == (leadCoefficient(value s)/leadCoefficient w) * w));
setRandomSeed 0;
(sol, mult) = sosInIdeal(h, 2, Solver => "CSDP");
assert(h*mult == sosPoly sol and value sosPoly sol != 0);
S = R/ideal h;
for v in {0, 1, 2} do (
    setRandomSeed 0;
    solS := sosInIdeal(S, 2, Verbosity => v);
    print("sosInIdeal(S,D), Verbosity " | toString v);
    assert(value sosPoly solS == 0 and any(gens sosPoly solS, g -> g != 0)));
setRandomSeed 0;
solS = sosInIdeal(S, 2, Solver => "CSDP");
assert(value sosPoly solS == 0 and any(gens sosPoly solS, g -> g != 0));

-- sosdecTernary --------------------------------------------------------------------
R = QQ[x,y,z];
m = library("Motzkin", R);
RRR = RR[x,y,z];
opNorm = g -> max(apply(terms g, t -> abs leadCoefficient t) | {0});
opTernaryOK = (f, r) -> (
    (p, q) := r;
    instance(p, List) and instance(q, List) and #p > 0 and (
        lhs := sub(f, RRR) * product(q, s -> sub(value s, RRR));
        rhs := product(p, s -> sub(value s, RRR));
        opNorm(lhs - rhs) < 1e-6 * opNorm lhs));
for v in {0, 1, 2} do (
    setRandomSeed 0;
    r := sosdecTernary(m, Verbosity => v);
    print("sosdecTernary, Verbosity " | toString v);
    assert(opTernaryOK(m, r)));
setRandomSeed 0;
assert(opTernaryOK(m, sosdecTernary(m, Solver => "CSDP")));
