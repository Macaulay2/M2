-- doc-core-sdpresult.m2 -- independent tests written from the documentation
-- (node SDPResult: keys, status, net, peek, # access; [JSAG] sec. 1).
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
maxAbs = M -> max apply(flatten entries M, e -> abs e);
okStatus = "SDP solved, primal-dual feasible";

-------------------------------------------------------------------------
-- the documented example: R = QQ[x][t], minimize -t s.t. x^2-3x-t is SOS
R = QQ[x][t];
f = x^2 - 3*x - t;
sol = solveSOS (f, -t, RoundTol=>12);
assert instance(sol, SDPResult);
assert instance(sol, HashTable);
-- the fields shown by peek
for k in {GramMatrix, MomentMatrix, Monomials, Parameters, Status} do
    assert(sol#?k);
-- (status, SDPResult) and the Status field
assert(status sol === sol#Status);
assert(sol#Status == okStatus);
-- GramMatrix: a symmetric PSD rational matrix with f = mon' Q mon (t substituted)
Q = sol#GramMatrix;
mon = sol#Monomials;
P = sol#Parameters;
assert(ring Q === QQ);
assert(numrows Q == 2 and numcols Q == 2);
assert(numrows mon == 2 and numcols mon == 1);
assert(numrows P == 1 and numcols P == 1);
assert(ring P === QQ);
assert isPSDQQ Q;
A = coefficientRing R;
fs = lift(sub(f, {t => P_(0,0)}), A);
assert((transpose(mon)*Q*mon)_(0,0) == fs);
-- the optimum: max t with x^2-3x-t SOS is min(x^2-3x) = -9/4; an exact
-- certificate proves t <= -9/4
assert(P_(0,0) <= -9/4);
assert(P_(0,0) > -9/4 - 1/1000);
-- the moment matrix is the moment matrix of the minimizer x = 3/2:
-- [[1, x], [x, x^2]] = [[1, 1.5], [1.5, 2.25]] (doc output)
MM = sol#MomentMatrix;
assert(numrows MM == 2 and numcols MM == 2);
-- 1e-3: the accuracy of the solver depends on the machine (1.50012 on an
-- Apple M5, CSDP 6.2.0)
assert(maxAbs(MM - matrix(RR, {{1, 1.5}, {1.5, 2.25}})) < 1e-3);
-- if the Gram matrix is not null, sosPoly recovers the SOS polynomial
s = sosPoly sol;
assert instance(s, SOSPoly);
assert(value s == fs);
assert all(coefficients s, c -> c > 0);
-- net: a Net mentioning the fields
n = toString net sol;
assert instance(net sol, Net);
for k in {"GramMatrix", "Monomials", "Status", "Parameters"} do
    assert(match(k, n));
assert instance(peek sol, Net);

-------------------------------------------------------------------------
-- a non-parametric problem: no parameters
use QQ[symbol x, symbol y];
g = 2*x^4+5*y^4-2*x^2*y^2+2*x^3*y;
sol2 = solveSOS g;
assert instance(sol2, SDPResult);
assert(sol2#Status == okStatus);
assert(status sol2 == okStatus);
P2 = sol2#Parameters;
assert(P2 === null or numrows P2 == 0);
Q2 = sol2#GramMatrix;
mon2 = sol2#Monomials;
assert(ring Q2 === QQ);
assert(Q2 == transpose Q2);
assert isPSDQQ Q2;
assert(transpose(mon2)*Q2*mon2 - g == 0);
assert(numrows Q2 == numrows mon2);
-- [JSAG] sec. 1: the Status line indicates that a Gram matrix was found
assert(match("GramMatrix", toString net sol2));
assert(match("Status", toString net sol2));

-------------------------------------------------------------------------
-- failures: polynomials that are not sums of squares.  No valid certificate
-- may be claimed, and the success status must not be reported ([JSAG] sec. 1:
-- the Status line "SDP solved, primal-dual feasible" indicates that a Gram
-- matrix was found, so f is a sum of squares).
notSOS = {-x^2 - 1, x^2 - y^2, -1 + 0*x, x^3, x, x^4 - x^2*y^2 - 1, x*y};
for h in notSOS do (
    r := solveSOS h;
    assert instance(r, SDPResult);
    Qh := r#GramMatrix;
    monh := r#Monomials;
    valid := Qh =!= null and monh =!= null and ring Qh === QQ and isPSDQQ Qh
        and (transpose(monh)*Qh*monh - h == 0);
    assert(not valid);
    assert(r#Status =!= okStatus);
    -- sosPoly can only be applied when the Gram matrix is not null (doc)
    if Qh =!= null then (
        sh := sosPoly r;
        assert(not (instance(sh, SOSPoly) and value sh == h and all(coefficients sh, c -> c > 0)));
        );
    );
