-- doc-core-roundtol.m2 -- independent tests written from the documentation
-- (node RoundTol, [solveSOS,RoundTol], "coefficient field"; [JSAG] sec. 4,
-- sec. 6 "Rounding tolerance" and Appendix; [PP08]).
-- RoundTol is the minimal rounding precision in binary digits; infinity skips
-- the rounding; rounding is guaranteed to work when the space of Gram
-- matrices is full dimensional; higher values lead to better solutions
-- (here only checked where the original agrees, see the tester report).
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
-- a numerical certificate: real PSD Gram matrix (up to tol) and small residual
realCert = (f, sol, tol) -> (
    Q := sol#GramMatrix; mon := sol#Monomials;
    Q =!= null and mon =!= null and instance(ring Q, RealField)
      and min toList eigenvalues(Q, Hermitian => true) > -tol
      and (s := sosPoly sol;
           clean(tol, value s - sub(f, ring s)) == 0));
substParams = (F, sol) -> (
    S := ring F; P := sol#Parameters;
    lift(sub(F, apply(numgens S, i -> S_i => P_(i,0))), coefficientRing S));

-------------------------------------------------------------------------
-- RoundTol => infinity skips the rounding: a real Gram matrix is returned
R = QQ[x,y];
f = 2*x^4+5*y^4-2*x^2*y^2+2*x^3*y;
solInf = solveSOS(f, RoundTol=>infinity);
assert(solInf#Status == okStatus);
assert(ring solInf#GramMatrix =!= QQ);
assert realCert(f, solInf, 1e-6);
solInf2 = solveSOS(x^2 + y^2 + 1, RoundTol=>infinity);
assert(ring solInf2#GramMatrix =!= QQ);
assert realCert(x^2 + y^2 + 1, solInf2, 1e-6);

-- the default and moderate precisions on strictly feasible problems give
-- exact rational certificates (rounding guaranteed, [PP08], [JSAG] App.)
strict = {f, x^2 + y^2 + 1, x^4 + x^2 + 1, (x^2+1)*(y^2+1) + x*y,
          (x - 1/3)^2 + (y + 2/7)^2 + 1/5};
for g in strict do (
    assert exactCert(g, solveSOS g);
    for rt in {8, 12, 16, 20, 24, 30, 32} do
        assert exactCert(g, solveSOS(g, RoundTol=>rt));
    );
-- coarse precisions: rounding may fail, but a returned rational Gram
-- matrix must still be an exact certificate
for g in strict do for rt in {0, 1, 2, 3, 4} do (
    sl := solveSOS(g, RoundTol=>rt);
    assert(sl#GramMatrix =!= null);
    if ring sl#GramMatrix === QQ then assert exactCert(g, sl)
    else assert realCert(g, sl, 1e-6);
    );

-------------------------------------------------------------------------
-- doc example of RoundTol / [JSAG] sec. 4: lower bound for the
-- dehomogenized Motzkin polynomial f = x^4+x^2+z^6-3x^2z^2 (min f = 0 at the
-- origin, f is not SOS; the SOS bound is about -0.1779785)
M = QQ[x,z][t];
fm = x^4+x^2+z^6-3*x^2*z^2;
assert(fm == library("Motzkin", {x, 1_M, z}));
bounds = new MutableHashTable;
for rt in {4, 12} do (
    sl := solveSOS(fm - t, -t, RoundTol=>rt);
    assert(sl#Status == okStatus);
    assert(ring sl#Parameters === QQ);
    assert exactCert(substParams(fm - t, sl), sl);
    bounds#rt = (sl#Parameters)_(0,0);
    -- a certified lower bound is below min f = 0 and near the SOS bound
    assert(bounds#rt <= 0);
    -- near the SOS bound -0.1779785, up to twice the rounding grid (lead,
    -- after the test audit: the original's -3/16 at 4 bits is a grid snap)
    assert(bounds#rt >= -0.1779785 - 2^(1-rt));
    );
-- higher RoundTol, better bound (doc: -3/16 for 4, -729/4096 for 12)
assert(bounds#4 <= bounds#12 + 2^-12);   -- BUGS.md 13(g): monotone up to the grid
solMinf = solveSOS(fm - t, -t, RoundTol=>infinity);
tInf = (solMinf#Parameters)_(0,0);
assert(instance(ring solMinf#GramMatrix, RealField));
assert(abs(tInf - (-0.1779785)) < 1e-5);
-- the rounded bound with 12 digits is within the grid 2^-12 of the numerical
-- one (the original's -729/4096 lies on that grid; lead, after the test audit)
assert(abs(bounds#12 - tInf) < 2^-12 + 1e-6);

-- doc example of solveSOS: x^2 - 3x - t, optimum -9/4
P1 = QQ[x][t];
prev = -infinity;
for rt in {0, 4, 12} do (
    sl := solveSOS(x^2 - 3*x - t, -t, RoundTol=>rt);
    tv := (sl#Parameters)_(0,0);
    assert exactCert(substParams(x^2 - 3*x - t, sl), sl);
    assert(tv <= -9/4);
    assert(tv >= prev - 2^-rt);   -- monotone up to the grid (BUGS.md 13(g); lead, after the test audit)
    prev = tv;
    );
assert(prev > -9/4 - 1/1000);
-- without rounding the optimum is found numerically
sl = solveSOS(x^2 - 3*x - t, -t, RoundTol=>infinity);
assert(abs((sl#Parameters)_(0,0) + 9/4) < 1e-6);
