-- Exactness of the certificates (conventions C3): every exact result must be
-- an exactly checked certificate.  For an SDPResult with a Gram matrix over QQ:
-- Q symmetric, Q positive semidefinite (checked exactly, independently of the
-- package and of SemidefiniteProgramming), f = mon' Q mon exactly, and
-- sosPoly gives positive weights with value s == f.  Inputs without
-- parameters, where the projection of the rounding is exact (BUGS.md 1 is
-- about parameters); the inputs of BUGS.md 1 are in tests/open/.

-- exact PSD test: a real symmetric matrix is PSD iff the coefficients of its
-- characteristic polynomial det(T - Q) alternate in sign (all roots are real)
isPSD = Q -> (
    n := numRows Q;
    if n == 0 then return true;
    T := local T;
    A := QQ[T];
    p := det(A_0 * id_(A^n) - sub(Q, A));
    all(0..n, k -> (-1)^(n-k) * coefficient(A_0^k, p) >= 0))
-- negative controls of the helper
assert isPSD matrix(QQ, {{1,1},{1,1}})
assert isPSD matrix(QQ, {{2,-1,0},{-1,2,-1},{0,-1,2}})
assert not isPSD matrix(QQ, {{1,2},{2,1}})
assert not isPSD matrix(QQ, {{0,1},{1,0}})
assert not isPSD matrix(QQ, {{1,0,0},{0,0,0},{0,0,-1/1000}})

-- the certificate of an exact result; f lives in the ring of the monomials
-- (for a quotient ring the identity holds in the quotient)
isCertificate = (f, sol) -> (
    (mon, Q) := (sol#Monomials, sol#GramMatrix);
    if Q === null or ring Q =!= QQ then return false;
    s := sosPoly sol;
    Q == transpose Q
      and isPSD Q
      and f == (transpose mon * Q * mon)_(0,0)
      and all(coefficients s, c -> c > 0)
      and value s == f)
-- negative controls: a wrong polynomial, a non-PSD matrix
R = QQ[x,y];
fakeSol = new SDPResult from {Monomials => matrix{{x},{y}}, GramMatrix => matrix(QQ,{{1,2},{2,1}}),
    MomentMatrix => null, Parameters => null, Status => null};
assert not isCertificate(x^2 + 4*x*y + y^2, fakeSol)
assert not isCertificate(x^2 + y^2 + 1, solveSOS(x^2 + y^2))

-- polynomials with a positive definite Gram matrix: the rounding must succeed
-- ([PP08] Proposition 8) and the result must be a certificate
R = QQ[x,y];
for f in {
    2*x^4 + 2*x^3*y - 2*x^2*y^2 + 5*y^4,       -- the documented example
    4*x^4 + y^4,
    (x^2 + y^2 + 1)^2,
    (x+2*y)^2 + (x-y)^4 + 1,
    x^4 + x^2*y^2 + y^4 + x + y + 1,
    1/3*x^2 + 7/5*y^2 + 1/11,
    x^2 + 1, x^4 + 1, x^6 - x + 1, 5_R} do (
    sol := solveSOS f;
    if not isCertificate(f, sol) then error("no exact certificate for " | toString f))
R = QQ[x,y,z];
for f in {
    x^4 + y^4 + z^4 - 4*x*y*z + x + y + z + 3,   -- [PP08] Example 3 (checkSolveSOS t2)
    x^2 + y^2 + z^2 + x*y + y*z,
    (x^2 + y^2 + z^2)^2 + x*y*z + 1} do (
    sol := solveSOS f;
    if not isCertificate(f, sol) then error("no exact certificate for " | toString f))
R = QQ[x,y,z,w];
f = 2*x^4 + x^2*y^2 + y^4 - 4*x^2*z - 4*x*y*z - 2*y^2*w + y^2 - 2*y*z + 8*z^2 - 2*z*w + 2*w^2;  -- [PP08] Example 4
assert isCertificate(f, solveSOS f)

-- the same with a monomial vector and with a degree bound
R = QQ[x,y];
f = 2*x^4 + 2*x^3*y - 2*x^2*y^2 + 5*y^4;
assert isCertificate(f, solveSOS(f, matrix{{x^2},{x*y},{y^2}}))
assert isCertificate(f, solveSOS(f, 4))
-- (solveSOS(f, 6) of this homogeneous quartic fails: BUGS.md 23, tests/open/)
-- a monomial vector too small for f: there is no Gram matrix at all (the
-- monomials of f outside mon*mon' cannot be matched)
R = QQ[x,y];
assert((solveSOS(x^4 + 1, matrix{{1_R}, {x}}))#GramMatrix === null)
assert((solveSOS(x^4 + y^2 + x^3*y, matrix{{1_R}, {x}, {y}, {x^2}}))#GramMatrix === null)
assert((solveSOS(x^2 + y^2 + 1, matrix{{x}, {y}}))#GramMatrix === null)
assert(first lowerBound(x^4 - x + 1, 2) === null)
assert(first lowerBound(x^4 + y^4 + x*y, matrix{{x - y}}, 2) === null)
f = 2*x^4 + 2*x^3*y - 2*x^2*y^2 + 5*y^4;
-- several RoundTol values: the certificate stays exact
for d in {1, 3, 8, 16, 32} do assert isCertificate(f, solveSOS(f, RoundTol => d))

-- quotient ring (the circle example of [P05]): the identity holds in the quotient
R = QQ[x,y]/ideal(x^2 + y^2 - 1);
f = 10 - x^2 - y;
assert isCertificate(f, solveSOS(f, matrix{{1},{x},{y}}))
assert isCertificate(f, solveSOS(f, 2))
assert isCertificate(f, solveSOS(f, 2, TraceObj => true))
assert isCertificate(f, solveSOS(f, 4))
R = QQ[x,y,z]/ideal(x^2 + y^2 + z^2 - 1);
f = 2 - x*y - z;
assert isCertificate(f, solveSOS(f, 2))

-- lowerBound: f - bound = sosPoly, with an exact bound
certifiedBound = (g, bound, sol) -> (
    bound =!= null and ring bound === QQ and isCertificate(g - bound, sol))
R = QQ[x];
f = (x-1)^2 + (x+3)^2;
(bound, sol) = lowerBound f;
assert(certifiedBound(f, bound, sol) and bound <= 8)
(bound, sol) = lowerBound(f, 4);
assert(certifiedBound(f, bound, sol) and bound <= 8)
R = QQ[x,y];
f = (x - y)^2 + (x + y - 2)^2 + 3;
(bound, sol) = lowerBound f;
assert(certifiedBound(f, bound, sol) and bound <= 3)
-- in a quotient ring (documented example): min of x - y on {0,1}^2 is -1
R = QQ[x,y]/ideal(x^2 - x, y^2 - y);
(bound, sol) = lowerBound(x - y, 2);
assert(certifiedBound(x - y, bound, sol) and bound <= -1)
-- with equations (documented example): f - bound + h*mult = sosPoly
R = QQ[x,y];
h = matrix{{x^2 - x, y^2 - y}};
(bound, sol, mult) = lowerBound(x - y, h, 2);
assert(bound <= -1 and isCertificate(x - y - bound + (h*mult)_(0,0), sol))
-- a polynomial f with equations: min z on the unit sphere is -1 (checkLowerBound t5)
R = QQ[x,y,z];
h = matrix{{x^2 + y^2 + z^2 - 1}};
(bound, sol, mult) = lowerBound(z, h, 4);
assert(bound <= -1 and isCertificate(z - bound + (h*mult)_(0,0), sol))

-- sosInIdeal (documented example): h * mult == sosPoly sol, exactly
R = QQ[x,y,z];
h = matrix{{x^2 - 4*x + 2*y^2, 2*z^2 - y^2 + 2}};
setRandomSeed 0;
(sol, mult) = sosInIdeal(h, 2);
assert(isCertificate((h*mult)_(0,0), sol) and (h*mult)_(0,0) != 0)
assert(h * mult == sosPoly sol)
-- in a quotient ring: a nonzero SOS that is 0 in the ring, so its squares
-- vanish on the real points
S = R/ideal h;
setRandomSeed 0;
sol = sosInIdeal(S, 2);
s = sosPoly sol;
assert(isPSD sol#GramMatrix and value s == 0 and #gens s > 0)

-- real input: a term that the monomials cannot represent, of size 1e-7, is
-- not ignored (x^2 + 1 + 1e-7 x^3 is not SOS; a "Gram matrix" of x^2 + 1 would
-- be a wrong certificate; added after the mutation analysis, LOG.md 2.6)
R = RR[x];
assert((solveSOS(x^2 + 1 + 1e-7*x^3, matrix{{1_R}, {x}}))#GramMatrix === null)
-- real input: the residual of the Gram identity is small and Q is PSD up to rounding
R = RR[x,y];
f = 2*x^4 + 2*x^3*y - 2*x^2*y^2 + 5*y^4;
sol = solveSOS f;
(mon, Q) = (sol#Monomials, sol#GramMatrix);
assert(instance(ring Q, RealField) and norm(f - (transpose mon * Q * mon)_(0,0)) < 1e-6)
assert(min eigenvalues(Q, Hermitian => true) > -1e-6)
assert(norm(f - value sosPoly sol) < 1e-6)
