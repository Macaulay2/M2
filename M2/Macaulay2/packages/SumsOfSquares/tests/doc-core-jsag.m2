-- doc-core-jsag.m2 -- independent tests of the examples in the paper
-- [JSAG] D. Cifuentes, T. Kahle, P. Parrilo, Sums of squares in Macaulay2,
-- JSAG 10 (2020), sections 1, 4, 6 and the Appendix (rational rounding).
-- Mathematical properties only; the printed values are pinned in
-- doc-core-printed-solvesos.m2.  Self-contained; assumes SumsOfSquares is loaded.
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
      and (s := sosPoly sol; instance(s, SOSPoly) and value s == f
           and all(coefficients s, c -> c > 0)));
substParams = (F, sol) -> (
    S := ring F; P := sol#Parameters;
    lift(sub(F, apply(numgens S, i -> S_i => P_(i,0))), coefficientRing S));

-------------------------------------------------------------------------
-- Section 1
R = QQ[x,y];
f = 2*x^4+5*y^4-2*x^2*y^2+2*x^3*y;
sol = solveSOS f;
-- "The Status line indicates that a Gram matrix was found"
assert(sol#Status == okStatus);
(Q,v) = ( sol#GramMatrix, sol#Monomials );
-- f = v' Q v with Q in S^N_+ (exact, rational)
assert(ring Q === QQ);
assert isPSDQQ Q;
assert(transpose(v)*Q*v - f == 0);
assert(numrows Q == numrows v);
-- sosPoly: the LDL factorization gives three squares (at most three: the
-- number of squares may change, LOG.md 1.7 decision 1; lead, after the test audit)
s = sosPoly sol;
assert(length s <= 3);
-- f = sum_i lambda_i f_i^2 with positive scalars lambda_i
lam = coefficients s;
fi = gens s;
assert(#lam == #fi and #lam <= 3);
assert all(lam, c -> c > 0);
assert(sum(#lam, i -> lam#i * (fi#i)^2) == f);
-- the quotient ring example with d = 1 and TraceObj
Rq = QQ[x,y]/ideal(x^2 + y^2 - 1);
fq = 10-x^2-y;     d = 1;
sq = sosPoly solveSOS (fq, 2*d, TraceObj=>true);
assert(value sq == fq);
assert(length sq == 2);
assert all(coefficients sq, c -> c > 0);
-- Hilbert: nonnegative = SOS for n = 1, for d = 2, and for n = 2, d = 4
U = QQ[symbol u];
assert exactCert(u^6 - u^4 - u^2 + 2, solveSOS(u^6 - u^4 - u^2 + 2));
Q5 = QQ[symbol a_1 .. symbol a_5];
qf = sum(5, i -> a_(i+1)^2) - sum(4, i -> a_(i+1)*a_(i+2));
assert exactCert(qf, solveSOS qf);
B2 = QQ[symbol x, symbol y];
bq = x^4 + y^4 - x*y + 1/4;  -- min over R^2 is 1/8 > 0
assert exactCert(bq, solveSOS bq);

-------------------------------------------------------------------------
-- Section 4: parametric SOS problems
R4 = QQ[symbol x, symbol y, symbol z][symbol s, symbol t];
g = library("Robinson", {x,y,z}) + s*x^6 + t*y^6;
sol4 = solveSOS g;
assert(sol4#Status == okStatus);
assert(ring sol4#Parameters === QQ);
assert exactCert(substParams(g, sol4), sol4);
-- the dehomogenized Motzkin polynomial: largest t with f - t SOS
R5 = QQ[symbol x, symbol z][symbol t];
fm = library ("Motzkin", {x,1,z});
sol5 = solveSOS (fm-t, -t, RoundTol=>12);
assert(sol5#Status == okStatus);
tv = (sol5#Parameters)_(0,0);
assert(ring tv === QQ);
assert exactCert(substParams(fm - t, sol5), sol5);
-- a certified lower bound: below min f = 0, close to the SOS bound -0.17798
assert(tv < 0);
assert(abs(tv - (-0.1779785)) < 2^-12 + 1e-5);   -- up to the rounding grid (lead, after the test audit)
-- lowerBound declares the parameter internally and gives the same bound
R6 = QQ[symbol x, symbol z];
fm6 = library ("Motzkin", {x,1,z});
(t6,sol6) = lowerBound (fm6, RoundTol=>12);
assert(abs(t6 - tv) < 1e-6);
assert(t6 <= 0);

-------------------------------------------------------------------------
-- Section 6: optional arguments
-- the default solver is CSDP: the explicit option gives a certificate too
use R;
assert exactCert(f, solveSOS(f, Solver=>"CSDP"));
-- rounding may be skipped by setting RoundTol to infinity
solInf = solveSOS(f, RoundTol=>infinity);
assert(instance(ring solInf#GramMatrix, RealField));
-- smaller RoundTol: rational matrices with smaller denominators (compared
-- between precisions 2 and 24, which differ a lot)
maxDen = Q -> max flatten apply(entries Q, r -> apply(r, e -> denominator e));
Q2 = (solveSOS(f, RoundTol=>2))#GramMatrix;
Q24 = (solveSOS(f, RoundTol=>24))#GramMatrix;
assert(ring Q2 === QQ and ring Q24 === QQ);
assert(maxDen Q2 < maxDen Q24);
-- the trace heuristic reduces the number of squares in the example above
use Rq;
assert(length sosPoly solveSOS (fq, 2*d, TraceObj=>true) <= length sosPoly solveSOS (fq, 2*d));   -- (lead, after the test audit: < held only for the original's three squares)

-------------------------------------------------------------------------
-- Appendix: the rounded matrix Q_r lies on L = {Q : v'Qv = f}, is rational,
-- and is PSD when the numerical matrix is in the interior of the cone.
-- Interior examples: positive definite Gram matrices; the result is close to
-- the numerical solution for high precision.
use R;
for p in {f, (x^2+1)*(y^2+1) + x*y, x^4 + x^2 + 1, (x - y)^2 + (x*y - 1)^2 + 1/3} do
    for rt in {12, 24} do (
        sl := solveSOS(p, RoundTol=>rt);
        Qr := sl#GramMatrix;
        assert(ring Qr === QQ);
        assert(transpose(sl#Monomials)*Qr*sl#Monomials - p == 0);
        assert isPSDQQ Qr;
        );
