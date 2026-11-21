-- doc-core-coefficient-field.m2 -- independent tests written from the
-- documentation (node "coefficient field", clean(RR,SOSPoly), solveSOS for
-- real polynomials; [Sch16] Thm. 2.1).
-- The package works over QQ and RR; when the rounding fails a real result is
-- returned; Scheiderer's polynomial is SOS but has no rational decomposition,
-- so the package must return a real solution.
-- Self-contained; assumes SumsOfSquares is loaded.
setRandomSeed 0;  -- fixed seed for anything random in the solver path

okStatus = "SDP solved, primal-dual feasible";
-- numerical certificate of f: PSD real Gram matrix (up to tol), small residual
realCert = (f, sol, tol) -> (
    Q := sol#GramMatrix; mon := sol#Monomials;
    Q =!= null and mon =!= null and instance(ring Q, RealField)
      and min toList eigenvalues(Q, Hermitian => true) > -tol
      and (s := sosPoly sol;
           all(coefficients s, c -> c > 0) and
           clean(tol, value s - sub(f, ring s)) == 0));

-------------------------------------------------------------------------
-- Scheiderer's example: SOS over RR, not over QQ
f = library("Scheiderer", QQ[x,y,z]);
sol = solveSOS f;
-- SOS over the reals: the solver succeeds ...
assert(sol#Status == okStatus);
-- ... and the result must be real (no rational Gram matrix exists)
assert(sol#GramMatrix =!= null);
assert(ring sol#GramMatrix =!= QQ);
assert(instance(ring sol#GramMatrix, RealField));
assert realCert(f, sol, 1e-6);
s = sosPoly sol;
assert instance(s, SOSPoly);
assert(instance(coefficientRing ring s, RealField));
-- clean removes the squares with small coefficients; what remains is still
-- a decomposition of f up to the tolerance
c = clean(0.001, s);
assert instance(c, SOSPoly);
assert(length c <= length s);
assert all(coefficients c, d -> d >= 0.001);
assert(clean(1e-4, value c - sub(f, ring c)) == 0);
-- f is a sum of two squares over RR [Sch16]: the Gram matrix has numerical
-- rank at least 2 (a single square would be a square of a quadratic form)
ev = sort toList eigenvalues(sol#GramMatrix, Hermitian => true);
assert(#select(ev, e -> e > 1e-3) >= 2);
-- without rounding the result is real as well
solInf = solveSOS(f, RoundTol=>infinity);
assert(instance(ring solInf#GramMatrix, RealField));
assert realCert(f, solInf, 1e-6);
-- also with high and low rounding precisions no rational result can appear
for rt in {2, 12, 24} do (
    sl := solveSOS(f, RoundTol=>rt);
    assert(sl#GramMatrix =!= null);
    assert(ring sl#GramMatrix =!= QQ);
    );

-------------------------------------------------------------------------
-- real input polynomials give real Gram matrices
R = RR[x,y];
g = 2*x^4+5*y^4-2*x^2*y^2+2*x^3*y;
solR = solveSOS g;
assert(solR#Status == okStatus);
assert(instance(ring solR#GramMatrix, RealField));
assert realCert(g, solR, 1e-6);
sR = sosPoly solR;
assert(clean(1e-6, value sR - g) == 0);
-- a polynomial with genuinely real coefficients
h = (x - sqrt 2)^2 + (y + pi*x)^2 + 0.5;
solH = solveSOS h;
assert realCert(h, solH, 1e-6);
-- not SOS over the reals
solN = solveSOS(x^2 - y^2);
assert(solN#Status =!= okStatus);
-- parametrized real problem: min x^2 - 3x is -2.25
Rp = RR[x][t];
solP = solveSOS(x^2 - 3*x - t, -t);
assert(solP#Status == okStatus);
assert(abs((solP#Parameters)_(0,0) + 2.25) < 1e-6);

-------------------------------------------------------------------------
-- clean (doc example and variations)
T = RR[x,y];
s1 = sosPoly(T, {x^2+.0001*x+1, y}, {2, .0001});
c1 = clean(.001, s1);
assert(length c1 == 1);
assert(clean(.001, value c1 - 2*(x^2+1)^2) == 0);
-- clean of an RR polynomial removes small coefficients (core function used
-- in the sosdecTernary documentation); for SOSPoly it removes small squares
c2 = clean(1e-10, s1);
assert(length c2 == 2);
assert(clean(1e-12, value c2 - value s1) == 0);
-- clean does nothing on rational coefficients
Rq = QQ[x,y];
sq = sosPoly(Rq, {x, y, x + y}, {1, 1/1000000, 1/10^12});
cq = clean(0.001, sq);
assert(length cq == 3);
assert(value cq == value sq);

-------------------------------------------------------------------------
-- substitute keeps the coefficient field (doc of SOSPoly)
Rq2 = QQ[x,y,z];
ss = sub(sq, Rq2);
assert(ring ss === Rq2);
assert(value ss == sub(value sq, Rq2));
assert(coefficients ss == coefficients sq);
