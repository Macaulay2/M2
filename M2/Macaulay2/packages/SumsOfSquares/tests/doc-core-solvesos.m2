-- doc-core-solvesos.m2 -- independent tests written from the documentation
-- (node solveSOS(RingElement); [JSAG] sec. 1 and Appendix; [PP08] Ex. 1, 3).
-- solveSOS f finds a PSD Q and monomials mon with f = mon' Q mon; over QQ
-- the result should be an exact certificate (rounding guaranteed when the
-- space of Gram matrices is full dimensional, doc of RoundTol / [JSAG] App.).
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
-- an exact rational certificate for f
exactCert = (f, sol) -> (
    Q := sol#GramMatrix; mon := sol#Monomials;
    Q =!= null and mon =!= null and ring Q === QQ and isPSDQQ Q
      and (transpose(mon)*Q*mon - f == 0)
      and (s := sosPoly sol; instance(s, SOSPoly) and value s == f
           and all(coefficients s, c -> c > 0)));
okStatus = "SDP solved, primal-dual feasible";
-- f has an exact certificate from solveSOS f
certified = f -> (sol := solveSOS f; sol#Status == okStatus and exactCert(f, sol));
-- f is not SOS: no valid certificate may be claimed
refuted = f -> (
    sol := solveSOS f; Q := sol#GramMatrix; mon := sol#Monomials;
    sol#Status =!= okStatus and
      not (Q =!= null and mon =!= null and ring Q === QQ and isPSDQQ Q
           and (transpose(mon)*Q*mon - f == 0)));

-------------------------------------------------------------------------
-- the documented example ([JSAG] sec. 1, [PP08] Ex. 1)
R = QQ[x,y];
f = 2*x^4+5*y^4-2*x^2*y^2+2*x^3*y;
sol = solveSOS f;
assert(sol#Status == okStatus);
Q = sol#GramMatrix;
mon = sol#Monomials;
assert(ring Q === QQ);
assert(transpose(mon)*Q*mon - f == 0);
assert exactCert(f, sol);
-- f is a homogeneous quartic: the Gram matrix of a form only involves
-- monomials of degree 2 (half the degree)
assert all(flatten entries mon, m -> first degree m == 2);
-- [JSAG] sec. 1: "we get three squares" (at most three: the number of
-- squares may change, LOG.md 1.7 decision 1; lead, after the test audit)
assert(length sosPoly sol <= 3);

-------------------------------------------------------------------------
-- simple SOS inputs, one or two variables
assert certified(x^2);
assert certified(x^2 + y^2);
assert certified(1 + x^2);
assert certified(x^2*y^2);
assert certified((x - y)^2);
assert certified(x^4 + 1);
assert certified((x^2 + 1)*(y^2 + 1) + x*y);
assert certified(x^2 - 2*x*y + 2*y^2);
assert certified(x^6 + y^6 + 1);
-- constants
assert certified(5_R);
assert certified(1/7 + 0*x);
-- inhomogeneous
assert certified((x^2 - 2*x + 3)*(y^2 + y + 1));
assert certified(x^4 - 4*x^3 + 6*x^2 - 4*x + 2);

-------------------------------------------------------------------------
-- univariate: nonnegative <=> SOS (Hilbert, [JSAG] sec. 1)
U = QQ[symbol u];
assert certified(u^6 - 3*u^4 + 3*u^2 + 1);
assert certified(u^8 + 1);
assert certified((u^2+1)*(u^2+2*u+3)*(u^2 - u + 1));
assert certified((u - 1)^2*(u + 1)^2 + 1);
assert certified(u^4 + u^3 + u^2 + u + 1);

-------------------------------------------------------------------------
-- not SOS: negative, odd degree, indefinite
use R;
assert refuted(-x^2 - 1);
assert refuted(-1 + 0*x);
assert refuted(x^3);
assert refuted(x);
assert refuted(x^2 - y^2);
assert refuted(x*y);
assert refuted(x^4 - 3*x^2*y^2 + y^4);
assert refuted(x^2 + y^2 - 1);

-------------------------------------------------------------------------
-- invariances: c*f for c > 0, sums, products, changes of variables
for c in {1/1000, 1/3, 7, 10^6, 10^9} do
    assert certified(c*f);
g = (x - y + 1)^2 + 2*(x*y - 1)^2 + 1/5;
assert certified g;
assert certified(f + g);
assert certified(f * (1 + x^2));
assert certified(g * (x^2 + y^2 + 1));
-- linear change of variables x -> x+y, y -> x-2y
phi = map(R, R, {x + y, x - 2*y});
assert certified(phi f);
assert certified(phi g);

-------------------------------------------------------------------------
-- rational coefficients with large denominators
assert certified((x - 1/1000003)^2 + (y + 7/999983)^2 + 1/1000);
assert certified(1/123456789*x^2 + 987654321/1000*y^2 + 1);
assert certified(x^2 + 1/1000000);

-------------------------------------------------------------------------
-- [PP08] Example 3: p has a rational SOS decomposition (six squares, with
-- a positive definite Gram matrix), so rounding is guaranteed to work
P4 = QQ[symbol x, symbol y, symbol z, symbol w];
pp = 2*x^4 + x^2*y^2 + y^4 - 4*x^2*z - 4*x*y*z - 2*y^2*w + y^2 - 2*y*z + 8*z^2 - 2*z*w + 2*w^2;
assert certified pp;

-------------------------------------------------------------------------
-- strictly feasible problems built from positive definite Gram matrices:
-- the rounding of [PP08] / [JSAG] Appendix is guaranteed to give a rational
-- certificate
T3 = QQ[symbol x, symbol y, symbol z];
mon10 = matrix{{1},{x},{y},{z},{x^2},{x*y},{y^2},{x*z},{y*z},{z^2}};
n10 = numrows mon10;
A1 = matrix(QQ, apply(n10, i -> apply(n10, j -> ((i*j + i + 2*j) % 7) - 3)));
Q1 = A1*transpose(A1) + id_(QQ^n10);
assert certified((transpose(mon10)*Q1*mon10)_(0,0));
A2 = matrix(QQ, apply(n10, i -> apply(n10, j -> ((3*i + 5*j + i*j) % 11) - 5)));
Q2 = A2*transpose(A2) + 1/10*id_(QQ^n10);
assert certified((transpose(mon10)*Q2*mon10)_(0,0));
-- a ternary form (homogeneous sextic) with PD Gram matrix on the cubics
mon3 = matrix{{x^3},{x^2*y},{x^2*z},{x*y^2},{x*y*z},{x*z^2},{y^3},{y^2*z},{y*z^2},{z^3}};
Q3 = A1*transpose(A1) + 2*id_(QQ^10);
f3 = (transpose(mon3)*Q3*mon3)_(0,0);
sol3 = solveSOS f3;
assert exactCert(f3, sol3);
assert all(flatten entries sol3#Monomials, m -> first degree m == 3);

-------------------------------------------------------------------------
-- many variables
M8 = QQ[symbol a_1 .. symbol a_8];
q8 = sum(8, i -> a_(i+1)^2) + sum(7, i -> a_(i+1)*a_(i+2));
assert certified q8;
assert certified(sum(8, i -> (a_(i+1) - i)^2));

-------------------------------------------------------------------------
-- higher degree univariate and sparse inputs
use U;
assert certified(product(1..5, k -> u^2 + k));
assert certified((u^2+1)^10);
assert certified(u^20 + 1);
assert certified(product(1..4, k -> (u - k)^2 + 1/k));
use T3;
assert certified(x^4*y^2 + x^2 + 1);
assert certified(x^8*y^2 + y^6*z^4 + 1 + x^2*y^2*z^2);
-- a sum of two squares whose Gram matrices may all be singular: the rounding
-- may fail ("coefficient field"), but the result must be a certificate,
-- exact or numerical
f2sq = (x^3 - y*z + 1)^2 + (x*y*z - 2)^2;
sol2sq = solveSOS f2sq;
assert(sol2sq#Status == okStatus);
assert(sol2sq#GramMatrix =!= null);
if ring sol2sq#GramMatrix === QQ then assert exactCert(f2sq, sol2sq) else (
    s2sq := sosPoly sol2sq;
    assert(min toList eigenvalues(sol2sq#GramMatrix, Hermitian => true) > -1e-6);
    assert(clean(1e-6, value s2sq - sub(f2sq, ring s2sq)) == 0);
    );
