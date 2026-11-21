-- doc-core-solvesos-mon.m2 -- independent tests written from the documentation
-- (node solveSOS(RingElement,Matrix) etc.: the methods with a monomial vector
-- or a degree bound, with and without objective, here in polynomial rings;
-- quotient rings are in doc-core-quotient.m2).
-- Self-contained; assumes SumsOfSquares is loaded.
setRandomSeed 0;  -- fixed seed for anything random in the solver path

isPSDQQ = Q -> (
    n := numrows Q;
    if n != numcols Q or Q != transpose Q then return false;
    A := mutableMatrix Q;
    for k from 0 to n-1 do (
        p := A_(k,k);
        if p < 0 then return false;
        if p == 0 then (
            for j from k+1 to n-1 do if A_(k,j) != 0 then return false;
            ) else (
            for i from k+1 to n-1 do for j from k+1 to n-1 do
                A_(i,j) = A_(i,j) - A_(i,k)*A_(k,j)/p;
            );
        );
    true)
okStatus = "SDP solved, primal-dual feasible";
exactCert = (f, sol) -> (
    Q := sol#GramMatrix; mon := sol#Monomials;
    Q =!= null and mon =!= null and ring Q === QQ and isPSDQQ Q
      and (transpose(mon)*Q*mon - f == 0)
      and (s := sosPoly sol; value s == f and all(coefficients s, c -> c > 0)));
-- the monomials of a vector, as a set
monSet = mon -> set flatten entries mon;

-------------------------------------------------------------------------
-- solveSOS(f, mon) with a given monomial vector
R = QQ[x,y];
f = 2*x^4+5*y^4-2*x^2*y^2+2*x^3*y;
monF = matrix{{x^2},{x*y},{y^2}};
sol = solveSOS(f, monF);
assert(sol#Status == okStatus);
assert exactCert(f, sol);
-- the Gram matrix refers to the monomials returned in the result
assert(numrows sol#GramMatrix == numrows sol#Monomials);
-- another order of the same monomials
sol = solveSOS(f, matrix{{y^2},{x^2},{x*y}});
assert exactCert(f, sol);
-- a larger monomial vector (all monomials of degree <= 3)
monBig = matrix{{1},{x},{y},{x^2},{x*y},{y^2},{x^3},{x^2*y},{x*y^2},{y^3}};
sol = solveSOS(f, monBig);
assert exactCert(f, sol);
-- consistency with solveSOS f: the monomials found there work as input
sol0 = solveSOS f;
sol1 = solveSOS(f, sol0#Monomials);
assert exactCert(f, sol1);
-- inhomogeneous polynomial with the full monomial vector of degree <= 2
g = (x^2 - 2*x + 3)*(y^2 + y + 1);
sol = solveSOS(g, matrix{{1},{x},{y},{x^2},{x*y},{y^2}});
assert exactCert(g, sol);
-- univariate with monomials 1, x, x^2
h = x^4 - 2*x^3 + 3*x^2 - 2*x + 1;  -- = (x^2 - x + 1)^2
sol = solveSOS(h, matrix{{1},{x},{x^2}});
assert(sol#GramMatrix =!= null);
s = sosPoly sol;
assert(if ring sol#GramMatrix === QQ then value s == h else
       clean(1e-6, value s - sub(h, ring s)) == 0);
-- a monomial vector that cannot represent f: no certificate may be claimed
sol = solveSOS(f, matrix{{x},{y}});
assert(not (sol#GramMatrix =!= null and ring sol#GramMatrix === QQ and
            isPSDQQ sol#GramMatrix and transpose(sol#Monomials)*sol#GramMatrix*sol#Monomials - f == 0));
assert(sol#Status =!= okStatus);

-------------------------------------------------------------------------
-- solveSOS(f, D): monomials of degree at most D/2
-- inhomogeneous inputs: D = deg f and D > deg f
for D in {4, 6} do (
    p := (x^2 + 1)*(y^2 + 2) + x*y;
    sl := solveSOS(p, D);
    assert(sl#Status == okStatus);
    assert exactCert(p, sl);
    -- only monomials of degree at most D/2 are used
    assert all(flatten entries sl#Monomials, m -> first degree m <= D//2);
    );
for D in {2, 4, 6, 8} do (
    sl := solveSOS(x^2 + 1, D);
    assert exactCert(x^2 + 1, sl);
    assert all(flatten entries sl#Monomials, m -> first degree m <= D//2);
    );
-- homogeneous input with D = deg f
sol = solveSOS(f, 4);
assert exactCert(f, sol);
assert all(flatten entries sol#Monomials, m -> first degree m <= 2);
sol = solveSOS(x^2 + y^2, 2);
assert exactCert(x^2 + y^2, sol);
-- consistency: solveSOS(f, D) and solveSOS(f, mon) with the monomials of
-- degree <= D/2 both certify f
q = x^4 + x^2*y^2 + y^4 + x^2 + 1;
mon4 = matrix{{1},{x},{y},{x^2},{x*y},{y^2}};
sA = solveSOS(q, 4);
sB = solveSOS(q, mon4);
assert exactCert(q, sA);
assert exactCert(q, sB);
assert isSubset(monSet sA#Monomials, monSet mon4);
-- a degree bound that is too small: no certificate may be claimed
sol = solveSOS(x^4 + 1, 2);
assert(not (sol#GramMatrix =!= null and ring sol#GramMatrix === QQ and
            transpose(sol#Monomials)*sol#GramMatrix*sol#Monomials - (x^4 + 1) == 0));

-------------------------------------------------------------------------
-- objective with monomial vector or degree bound (polynomial ring)
-- min x^4 + y^4 - x*y = -1/8 (attained at x = y = +-1/2); bivariate quartics
-- that are nonnegative are SOS, so the SOS bound is exactly -1/8
S = QQ[x,y][t];
A = coefficientRing S;
F = x^4 + y^4 - x*y - t;
-- tightness up to the rounding grid 2^-RoundTol (the original snaps onto the
-- dyadic -1/8 exactly; lead, after the test audit)
checkBound = (sl, target, slack) -> (
    tv := (sl#Parameters)_(0,0);
    fs := lift(sub(F, {t => tv}), A);
    -- exact certificate => tv is a rigorous lower bound
    ring tv === QQ and exactCert(fs, sl) and tv <= target and tv >= target - slack);
assert checkBound(solveSOS(F, -t), -1/8, 2^-3);
assert checkBound(solveSOS(F, -t, 4), -1/8, 2^-3);
assert checkBound(solveSOS(F, -t, matrix{{1},{x},{y},{x^2},{x*y},{y^2}}), -1/8, 2^-3);
assert checkBound(solveSOS(F, -t, 4, RoundTol=>12), -1/8, 2^-12);
assert checkBound(solveSOS(F, -t, matrix{{1},{x},{y},{x^2},{x*y},{y^2}}, RoundTol=>12), -1/8, 2^-12);
