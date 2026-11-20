-- doc-core-options.m2 -- independent tests written from the documentation
-- (nodes TraceObj, [solveSOS,Solver], [solveSOS,Verbosity]; [JSAG] sec. 6;
-- [P05] Example 5; SemidefiniteProgramming documentation of Solver).
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
      and (s := sosPoly sol; instance(s, SOSPoly) and value s == f
           and all(coefficients s, c -> c > 0)));
-- a valid certificate, exact or numerical
anyCert = (f, sol) -> (
    Q := sol#GramMatrix;
    Q =!= null and (if ring Q === QQ then exactCert(f, sol) else (
        s := sosPoly sol;
        min toList eigenvalues(Q, Hermitian => true) > -1e-6
          and clean(1e-6, value s - sub(f, ring s)) == 0)));
substParams = (F, sol) -> (
    S := ring F; P := sol#Parameters;
    lift(sub(F, apply(numgens S, i -> S_i => P_(i,0))), coefficientRing S));

-------------------------------------------------------------------------
-- TraceObj: [P05] Example 5 with two squares (doc of TraceObj, [JSAG] sec. 1)
R = QQ[x,y]/ideal(x^2 + y^2 - 1);
f = 10-x^2-y;
s3 = sosPoly solveSOS (f, 2);
sT = sosPoly solveSOS (f, 2, TraceObj=>true);
assert(value s3 == f);
assert(value sT == f);
assert(length sT == 2);
-- (length sT < length s3 held only because the original gives three squares;
-- removed by the lead after the test audit, LOG.md 1.7 decision 1)
assert all(coefficients sT, c -> c > 0);
-- with the monomial vector instead of the degree bound
sT2 = sosPoly solveSOS (f, matrix{{1},{x},{y}}, TraceObj=>true);
assert(value sT2 == f);
assert(length sT2 == 2);
-- the trace objective is a heuristic: the result is still a certificate
P = QQ[x,y];
g = 2*x^4+5*y^4-2*x^2*y^2+2*x^3*y;
assert exactCert(g, solveSOS(g, TraceObj=>true));
-- a polynomial that is a sum of two squares of linear forms
h = (x+y)^2 + (x-y+1)^2;
solh = solveSOS(h, TraceObj=>true);
assert exactCert(h, solh);
-- the Gram matrix of h on 1,x,y is unique (products of 1,x,y are distinct
-- monomials), of rank 2
assert(rank solh#GramMatrix == 2);
assert(length sosPoly solh == 2);
-- univariate: x^4 + 1 with the minimum trace
U = QQ[symbol u];
assert exactCert(u^4 + 1, solveSOS(u^4 + 1, TraceObj=>true));
assert exactCert(u^4 + 2*u^2 + 1, solveSOS(u^4 + 2*u^2 + 1, TraceObj=>true));

-------------------------------------------------------------------------
-- Solver: CSDP is the default ([JSAG] sec. 6); the result with the explicit
-- option agrees in its mathematical content
use P;
solD = solveSOS g;
solC = solveSOS(g, Solver=>"CSDP");
assert exactCert(g, solD);
assert exactCert(g, solC);
assert(solC#Status == okStatus);
solCq = solveSOS(f, 2, Solver=>"CSDP");
assert(value sosPoly solCq == f);
-- the rudimentary solver "M2" (may fail when the problem is not strictly
-- feasible; here the problem is strictly feasible)
-- (the rudimentary solver may fail on a reformulated SDP: no result is
-- accepted, a result must be a certificate; lead, after the test audit)
solM = solveSOS(g, Solver=>"M2");
assert(solM#GramMatrix === null or anyCert(g, solM));
solM2 = solveSOS(x^2 + y^2 + 1, Solver=>"M2");
assert(solM2#GramMatrix === null or anyCert(x^2 + y^2 + 1, solM2));
-- M2 solver on a non-SOS input: no certificate may be claimed
solMn = solveSOS(-x^2 - 1, Solver=>"M2");
assert(not anyCert(-x^2 - 1, solMn));
-- M2 solver with an objective: min x^2 - 3x = -9/4
Pt = QQ[x][t];
Fo = x^2 - 3*x - t;
solMo = solveSOS(Fo, -t, Solver=>"M2");
if solMo#Parameters =!= null then (
    tv = (solMo#Parameters)_(0,0);
    assert(abs(tv + 9/4) < 1e-4);
    if ring tv === QQ then (
        assert exactCert(substParams(Fo, solMo), solMo);
        assert(tv <= -9/4)));

-------------------------------------------------------------------------
-- Verbosity: only the printed information changes, not the result
use P;
for v in {0, 1, 2} do (
    sl := solveSOS(g, Verbosity=>v);
    assert exactCert(g, sl);
    assert(sl#Status == okStatus);
    );
for v in {0, 1} do (
    sl := solveSOS(Fo, -Pt_0, RoundTol=>12, Verbosity=>v);
    assert((sl#Parameters)_(0,0) <= -9/4);
    assert exactCert(substParams(Fo, sl), sl);
    );
