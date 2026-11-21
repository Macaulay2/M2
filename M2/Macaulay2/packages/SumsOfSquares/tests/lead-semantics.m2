-- The conventions of notes/conventions.md as behaviour (A1-A5, B2, C1,
-- D2-D7): operations on SOSPoly, the coefficient field, the monomial choice,
-- quotient rings, objectives, RoundTol, and the input checks of every
-- function.  Only behaviour that is documented, stated in [JSAG], or decided
-- with Thomas (LOG.md 1.7) is pinned; an error is pinned as "an error", not
-- by its message.
fails = f -> try (f(); false) else true

-- A1. SOSPoly arithmetic
R = QQ[x,y];
s = sosPoly(R, {x+1, y}, {2, 3});
s2 = sosPoly(R, {x*y, x-y}, {1/2, 5});
assert(value s == 2*(x+1)^2 + 3*y^2)
assert(value(s + s2) == value s + value s2)
assert(value(s * s2) == value s * value s2 and length(s * s2) == 4)
assert(value(3 * s) == 3 * value s and value(1/7 * s) == 1/7 * value s)
assert(value(s^2) == (value s)^2 and value(s^4) == (value s)^4)
assert(s == value s and value s == s and s == s and s == sosPoly(R, {y, x+1}, {3, 2}))
assert(s == matrix{{value s}} and matrix{{value s}} == s)
assert(s != s2 and not (s == value s + 1))
assert fails(() -> (-1) * s)            -- a negative weight is not allowed
-- (s^1, s^3, s^0 raise errors in the original: BUGS.md 28, needs discussion,
-- so they are not pinned here)
assert fails(() -> s == matrix{{x, y}})
S = QQ[x,y,z];
t = sosPoly(S, {x+1, y}, {2, 3});
assert fails(() -> s + t)               -- different rings
assert fails(() -> s * t)
assert fails(() -> s == value t)
assert(sub(s, S) == t and ring sub(s, S) === S)
use R;
assert(gens s == {x+1, y} and coefficients s == {2, 3} and ring s === R and length s == 2)
assert(sosPoly({x+1, y}, {2, 3}) == s)
assert(toString expression s == toString expression sosPoly(R, {x+1, y}, {2, 3}))
-- clean: removes small weights over RR, does nothing over QQ (documented)
assert(clean(0.001, sosPoly(R, {x+1, y}, {2, 1/100000})) == sosPoly(R, {x+1, y}, {2, 1/100000}))
assert(length clean(0.001, sosPoly(R, {x+1, y}, {2, 1/100000})) == 2)
RR2 = RR[x,y];
c = clean(0.001, sosPoly(RR2, {x^2 + .0001*x + 1, y}, {2, .0001}));
assert(length c == 1 and c == sosPoly(RR2, {x^2 + 1}, {2}))

-- sosPoly(mon, Q): the decomposition of a PSD Gram matrix (rank = number of squares)
mon = matrix{{1}, {x}, {y}};
Q = matrix(QQ, {{1,1,1},{1,1,1},{1,1,1}});
p = sosPoly(mon, Q);
assert(length p == 1 and value p == (1 + x + y)^2)
Q = matrix(QQ, {{2,1,0},{1,2,0},{0,0,0}});
p = sosPoly(mon, Q);
assert(length p == 2 and value p == (transpose mon * Q * mon)_(0,0))
assert(sosPoly(mon, matrix(QQ, {{1,2,0},{2,1,0},{0,0,1}})) === null)   -- not PSD

-- A2. the coefficient field: QQ input gives a rational result when the
-- rounding succeeds, a real result in a new ring over RR with the same
-- variable names when it fails (Scheiderer: no rational decomposition exists)
f = library("Scheiderer", QQ[x,y,z]);
sol = solveSOS f;
Rr = ring sol#Monomials;
assert(instance(coefficientRing Rr, RealField) and Rr =!= ring f)
assert(toString gens Rr == toString gens ring f)
-- RR input gives a real result in the same ring
R = RR[x,y];
sol = solveSOS(x^4 + y^4 + 1);
assert(ring sol#Monomials === R and instance(ring sol#GramMatrix, RealField))

-- A4. monomial choice by the Newton polytope; an odd vertex means "not SOS".
-- The monomials lie in half the Newton polytope ([PP08] Theorem 3); a renewal
-- may drop more of them (e.g. by diagonal consistency), so only the
-- inclusion is pinned, with the certificate.
R = QQ[x,y];
sol = solveSOS(x^4*y + 1);
assert(sol#GramMatrix === null and sosPoly sol === null)
assert((solveSOS(x^3 + 1))#GramMatrix === null)
assert((solveSOS(-x^2))#GramMatrix === null)
sol = solveSOS(x^4 + y^4);
assert(isSubset(set flatten entries sol#Monomials, set {x^2, x*y, y^2}) and value sosPoly sol == x^4 + y^4)
sol = solveSOS(x^2*y^2 + 1);     -- Newton polytope: segment from 1 to x^2y^2
assert(isSubset(set flatten entries sol#Monomials, set {1_R, x*y}) and value sosPoly sol == x^2*y^2 + 1)
-- a degree bound D: all monomials of degree at most D/2 in the variables of f
sol = solveSOS(x^2 + 1, 2);
assert(set flatten entries sol#Monomials === set {1_R, x})
sol = solveSOS(x^4 + y^2 + 1, 4);
assert(set flatten entries sol#Monomials === set {1_R, x, y, x^2, x*y, y^2})
assert fails(() -> solveSOS(x^2 + 1, 3))      -- odd degree bound
assert fails(() -> solveSOS(x^2 + 1, 0))
assert fails(() -> solveSOS(x^2 + 1, -2))
-- the monomial vector must be a column of monomials
assert fails(() -> solveSOS(x^2 + 1, matrix{{1_R, x}}))
assert fails(() -> solveSOS(x^2 + 1, matrix{{1_R}, {x + 1}}))
-- solveSOS of a constant ring element
assert((solveSOS(5_R))#GramMatrix =!= null)

-- A5. quotient rings need a monomial vector or a degree bound
S = QQ[x,y]/ideal(x^2 + y^2 - 1);
assert fails(() -> solveSOS(10 - x^2 - y))
assert fails(() -> lowerBound(x - y))
assert((solveSOS(10 - x^2 - y, 2))#GramMatrix =!= null)

-- B2. objectives: only with parameters, and linear in them; the polynomial
-- must be affine in the parameters
R = QQ[x];
assert fails(() -> solveSOS(x^2 + 1, x))
R = QQ[x][symbol t];
assert fails(() -> solveSOS(x^2 + t*x + 1, t^2))
assert fails(() -> solveSOS(x^2 + t*x + 1, x*t))
assert fails(() -> solveSOS(x^2 + t^2*x + 1))
-- minimizing a parameter: x^2 + t x + 1 is SOS iff |t| <= 2
f = x^2 + t*x + 1;
p1 = (solveSOS(f, t, RoundTol => 12))#Parameters;
p2 = (solveSOS(f, -t, RoundTol => 12))#Parameters;
assert(abs(p1_(0,0) + 2) < 1e-2 and abs(p2_(0,0) - 2) < 1e-2)
-- several parameters
R = QQ[x][symbol s, symbol t];
sol = solveSOS(x^4 + s*x^2 + t, s + t, RoundTol => 12);
assert(sol#Parameters =!= null and numRows sol#Parameters == 2)
-- TraceObj replaces an objective of the parameters (decided, LOG.md 1.7
-- item 5: documented, with a warning, behaviour unchanged)
R = QQ[x][symbol t];
f = x^2 + t*x + 1;
assert((solveSOS(f, t, RoundTol => 12, TraceObj => true))#Parameters
    == (solveSOS(f, RoundTol => 12, TraceObj => true))#Parameters)

-- C1. RoundTol: infinity skips the rounding (real result); 1 and 32 round.
-- (Values above 33 also skip it in the original: BUGS.md 26, needs
-- discussion, so they are not pinned here.)
R = QQ[x,y];
f = 2*x^4 + 2*x^3*y - 2*x^2*y^2 + 5*y^4;
for d in {infinity} do (
    sol := solveSOS(f, RoundTol => d);
    assert(instance(ring sol#GramMatrix, RealField) and instance(coefficientRing ring sol#Monomials, RealField)))
for d in {1, 32} do assert(ring (solveSOS(f, RoundTol => d))#GramMatrix === QQ)
-- the same for lowerBound and sosInIdeal
R = QQ[x];
assert(instance(first lowerBound(x^2 + 1, RoundTol => infinity), RR))
assert(ring first lowerBound(x^2 + 1) === QQ)
setRandomSeed 0;
assert(instance(ring (first sosInIdeal(matrix{{x+1}}, 2, RoundTol => infinity))#GramMatrix, RealField))

-- D2. lowerBound: input checks
R = QQ[x,y];
assert fails(() -> lowerBound(x^2 + 1, 3))
assert fails(() -> lowerBound(x, matrix{{x^2 - 1}}, 3))
assert fails(() -> lowerBound(x, matrix{{x^2 - 1}, {y}}, 2))
assert fails(() -> lowerBound(x, matrix{{x^2 - 1}}, -1))
-- lowerBound(f) for f with the minimum 0 and the minimizer recovered
(b, sol) = lowerBound((x - 2)^2 + (y + 1)^2, RoundTol => infinity);
assert(abs b < 1e-6)
pt = recoverSolution sol;
assert(#pt == 2 and abs(sub(x, pt) - 2) < 1e-4 and abs(sub(y, pt) + 1) < 1e-4)

-- D3. sosInIdeal: input checks
assert fails(() -> sosInIdeal(matrix{{x - y}}, 3))
assert fails(() -> sosInIdeal(matrix{{x - y}, {x}}, 2))
assert fails(() -> sosInIdeal(QQ[x,y]/ideal(x^2 + y^2), 3))

-- D4. sosdecTernary: only ternary forms
assert fails(() -> sosdecTernary(x^2 + y^2))
assert fails(() -> sosdecTernary((library("Motzkin", QQ[x,y,z])) + 1))
setRandomSeed 0;
R = QQ[x,y,z];
(p, q) = sosdecTernary(x^4 + y^4 + z^4);     -- degree 4: one SOS, no denominator
assert(#p == 1 and #q == 0 and value p#0 == x^4 + y^4 + z^4)

-- D5. recoverSolution: errors for a matrix that is not PSD; nothing to recover
-- if the monomials contain no variable
R = RR[x,y];
-- (an error in the original; the fixes of BUGS.md 24, 25 may return {} instead)
r = try recoverSolution(matrix{{x},{y}}, matrix(RR, {{1,2},{2,1}})) else "error";
assert(r === "error" or r === {})
assert(recoverSolution(matrix{{1_R}}, matrix(RR, {{1}})) == {})
pt = recoverSolution(matrix{{1},{x},{y}}, matrix(RR, {{1,2,3},{2,4,6},{3,6,9}}));
assert(abs(sub(x, pt) - 2) < 1e-8 and abs(sub(y, pt) - 3) < 1e-8)

-- D6. library: unknown names and too few variables are errors
R = QQ[x,y,z];
assert fails(() -> library("Hilbert", R))
assert fails(() -> library("Lax-Lax", R))
assert fails(() -> library("Choi-Lam", {x,y,z}))
assert fails(() -> library("Motzkin", QQ[x,y]))
-- the forms use the first three or four variables of the list
R = QQ[symbol a, symbol b, symbol c, symbol d, symbol e];
assert(library("Motzkin", {e,d,c}) == sub(library("Motzkin", R), {a => e, b => d, c => c}))
assert(support library("Robinson", R) == {a,b,c})

-- D7. checkSolver: all internal tests pass with CSDP (documentation); a
-- function name or the function itself; an unknown name is an error
for fn in {"solveSOS", "lowerBound", "sosInIdeal", "sosdecTernary"} do (
    setRandomSeed 0;
    assert all(checkSolver("CSDP", fn), identity))
setRandomSeed 0;
assert all(checkSolver("CSDP", solveSOS), identity)
assert fails(() -> checkSolver("CSDP", "foo"))

-- Determinism (decided, LOG.md 1.7 item 1: output is deterministic for a
-- given machine and CSDP version): the same call twice gives the same result.
-- (Independence of the random seed in sosInIdeal is BUGS.md 18, tests/open/.)
R = QQ[x,y];
f = 2*x^4 + 2*x^3*y - 2*x^2*y^2 + 5*y^4;
assert((solveSOS f)#GramMatrix == (solveSOS f)#GramMatrix)
(b1, sol1) = lowerBound(x^4 - x + 1); (b2, sol2) = lowerBound(x^4 - x + 1);
assert(b1 == b2 and sol1#GramMatrix == sol2#GramMatrix)
S = QQ[x,y]/ideal(x^2 + y^2 - 1);
assert((solveSOS(10 - x^2 - y, 2))#GramMatrix == (solveSOS(10 - x^2 - y, 2))#GramMatrix)
R = QQ[x,y,z];
h = matrix{{x^2 - 4*x + 2*y^2, 2*z^2 - y^2 + 2}};
grams = for i to 1 list (setRandomSeed 0; (first sosInIdeal(h, 2))#GramMatrix);
assert(grams#0 == grams#1)
-- sosdecTernary of the Motzkin form: the same factors for every seed (the
-- decided fix of BUGS.md 18 makes sosInIdeal, and so sosdecTernary,
-- independent of the seed; on this input the original already is)
M = library("Motzkin", R);
dec = for seed in {0, 1, 2} list (setRandomSeed seed; (p, q) := sosdecTernary M; apply(p | q, value));
assert(#unique dec == 1)
