-- Oracles from the references (notes/conventions.md E, review-step1-papers
-- sections 5 and 11, LOG.md 1.8): the library forms, their zeros and
-- nonnegativity; "not SOS" claims; the Scheiderer form has no rational
-- decomposition; the exact SOS bound -729/4096 with its primal and dual
-- certificates; the unique trace-minimal Gram matrix of [P05] Example 5;
-- [BPT13] Exercise 3.55; [JSAG] section 4 (Robinson with parameters);
-- [dKP04] Algorithm 1 on the Motzkin form.

isPSD = Q -> (
    n := numRows Q;
    if n == 0 then return true;
    T := local T;
    A := QQ[T];
    p := det(A_0 * id_(A^n) - sub(Q, A));
    all(0..n, k -> (-1)^(n-k) * coefficient(A_0^k, p) >= 0))
isRational = sol -> sol#GramMatrix =!= null and ring sol#GramMatrix === QQ
isCertificate = (f, sol) -> (
    (mon, Q) := (sol#Monomials, sol#GramMatrix);
    isRational sol and Q == transpose Q and isPSD Q and f == (transpose mon * Q * mon)_(0,0))

-- the library forms, written down from the references ([Rez00] pp. 7, 8, 11,
-- [BPT13] Ex. 3.69, [Sch16] Theorem 2.1, [Har99] Lemma 5.1(9) with u = 2, v = 0)
R = QQ[x,y,z,w];
lit = hashTable {
    "Motzkin" => x^4*y^2 + x^2*y^4 - 3*x^2*y^2*z^2 + z^6,
    "Robinson" => x^6 + y^6 + z^6 - (x^4*y^2 + x^2*y^4 + x^4*z^2 + x^2*z^4 + y^4*z^2 + y^2*z^4) + 3*x^2*y^2*z^2,
    "Schmuedgen" => 200*(x^3 - 4*x*z^2)^2 + 200*(y^3 - 4*y*z^2)^2 + (y^2 - x^2)*x*(x + 2*z)*(x^2 - 2*x*z + 2*y^2 - 8*z^2),
    "Choi-Lam" => x^2*y^2 + y^2*z^2 + z^2*x^2 + w^4 - 4*x*y*z*w,
    "Scheiderer" => x^4 + x*y^3 + y^4 - 3*x^2*y*z - 4*x*y^2*z + 2*x^2*z^2 + x*z^3 + y*z^3 + z^4,
    -- Reznick's Lax-Lax form: sum over i of prod_{j != i} (x_i - x_j), five
    -- variables, the fifth set to 0 (the form is translation invariant)
    "Lax-Lax" => (L := {x,y,z,w,0_R}; sum(5, i -> product(select(5, j -> j != i), j -> L#i - L#j)))};
for n in keys lit do assert(library(n, R) == lit#n)
-- Harris: R_{2,0} = 8 * (16, -36, 20, 57, -38) in the symmetric basis ([Har99]
-- (5.5), (5.6): f = 8, beta = -36, gamma = 160 give 128, -288, 160, 456, -304)
symm = e -> sum(unique permutations e, p -> x^(p#0) * y^(p#1) * z^(p#2))
harris = 16*symm{10,0,0} - 36*symm{8,2,0} + 20*symm{6,4,0} + 57*symm{6,2,2} - 38*symm{4,4,2}
assert(library("Harris", R) == harris)
assert(8 * harris == 128*symm{10,0,0} - 288*symm{8,2,0} + 160*symm{6,4,0} + 456*symm{6,2,2} - 304*symm{4,4,2})
assert(library("Harris", {x,y,z}) == harris and library("Motzkin", {y,z,x}) == sub(lit#"Motzkin", {x=>y, y=>z, z=>x}))

-- nonnegativity at random rational points
setRandomSeed 7;
for n in keys lit | {"Harris"} do for i to 200 do (
    p := matrix{apply(4, j -> random(-20,20) / random(1,7))};
    if sub(library(n, R), p) < 0 then error(n | " negative at " | toString p))

-- known real zeros ([Rez00]; [Har99] Lemma 6.8: the orbits of (0,1,1), (1,2,2), (1,1,sqrt 2))
zeroAt = (f, pt) -> sub(f, matrix{pt}) == 0
M = library("Motzkin", R);
assert all({{1,1,1,0},{1,-1,1,0},{-1,1,1,0},{1,1,-1,0},{1,0,0,0},{0,1,0,0}}, p -> zeroAt(M, p))
Rob = library("Robinson", R);
assert all({{1,1,1,0},{1,-1,1,0},{1,1,0,0},{1,-1,0,0},{1,0,1,0},{0,1,-1,0}}, p -> zeroAt(Rob, p))
assert zeroAt(library("Choi-Lam", R), {1,1,1,1})
H = library("Harris", R);
signs = p -> unique flatten apply(toList((#p:-1)..(#p:1)), s -> if member(0, s) then {} else {apply(p, s, times)})
orbit = p -> unique flatten apply(unique permutations p, signs)
assert(all(orbit{0,1,1} | orbit{1,2,2}, p -> zeroAt(H, p | {0})))
-- (1,1,sqrt 2) in QQ[r]/(r^2 - 2)
A = QQ[r]/ideal(r^2 - 2);
hA = sub(H, {x => 1_A, y => 1_A, z => r, w => 0_A});
assert(hA == 0)
-- the orbits give 30 projective points (up to the sign of the whole vector)
assert(#orbit{0,1,1} // 2 + #orbit{1,2,2} // 2 + 12 == 30)
-- the singular locus of the Harris curve is exactly these 30 points
-- (degree 30, radical; papers verifier section 5)
S = QQ[x,y,z];
hS = library("Harris", S);
J = saturate(ideal jacobian ideal hS, ideal vars S);
assert(dim J == 1 and degree J == 30)

-- "not a sum of squares" (documentation, library node): no exact result, and
-- no Gram matrix at all
S = QQ[x,y,z];
for n in {"Motzkin", "Robinson", "Schmuedgen", "Harris"} do (
    sol := solveSOS library(n, S);
    if sol#GramMatrix =!= null then error(n | ": a Gram matrix for a form that is not SOS"))
S = QQ[x,y,z,w];
for n in {"Lax-Lax", "Choi-Lam"} do (
    sol := solveSOS library(n, S);
    if sol#GramMatrix =!= null then error(n | ": a Gram matrix for a form that is not SOS"))

-- Scheiderer ([Sch16] Theorem 2.1): SOS over RR, not over QQ.  The package
-- must return a real result (documentation, "coefficient field"); a rational
-- Gram matrix would be a false certificate.
S = QQ[x,y,z];
f = library("Scheiderer", S);
for d in {3, 10, 20, 30} do (
    sol := solveSOS(f, RoundTol => d);
    assert(sol#GramMatrix =!= null and not isRational sol);
    (mon, Q) := (sol#Monomials, sol#GramMatrix);
    assert(norm(sub(f, ring mon) - (transpose mon * Q * mon)_(0,0)) < 1e-6);
    assert(min eigenvalues(Q, Hermitian => true) > -1e-6))

-- [BPT13] / review section 11: the SOS bound of x^4 + x^2 + z^6 - 3x^2z^2 is
-- exactly -729/4096.  Primal certificate (exact):
S = QQ[x,z];
f = x^4 + x^2 + z^6 - 3*x^2*z^2;
assert(f + 729/4096 == (x^2 - 3/2*z^2 + 27/64)^2 + (z^3 - 9/8*z)^2 + 5/32*x^2)
-- dual certificate: a PSD moment matrix on v = (1, x, x^2, z, z^2, z^3, x*z)
-- with L(f) = -729/4096, so no SOS bound can exceed it
v = {1_S, x, x^2, z, z^2, z^3, x*z};
mom = hashTable {1_S => 1, x^2 => 0, z^2 => 9/32, x^4 => 2187/4096, x^2*z^2 => 729/2048, z^4 => 81/256, z^6 => 729/2048};
Lfun = g -> sum(terms g, t -> (leadCoefficient t) * (if mom#?(leadMonomial t) then mom#(leadMonomial t) else 0))
-- odd moments are 0; all moments of degree <= 6 that occur are listed
assert all(flatten apply(v, a -> apply(v, b -> a*b)), m -> mom#?m or any(first exponents m, odd))
Mmat = matrix(QQ, apply(v, a -> apply(v, b -> Lfun(a*b))));
assert(isPSD Mmat and Lfun f == -729/4096)
-- the package: RoundTol => 12 gives the optimum up to the rounding grid
-- 2^-12 (the original returns exactly -729/4096, which lies on that grid;
-- the documented value is pinned in the printed files)
(bound, sol) = lowerBound(f, RoundTol => 12);
assert(bound <= -729/4096 and bound >= -729/4096 - 2^-12 and isCertificate(f - bound, sol))
-- every certified bound is at most the optimum; RoundTol => 4 is the documented -3/16
for d in {3, 4, 8, 16} do (
    (b, s) := lowerBound(f, RoundTol => d);
    assert(b =!= null and isCertificate(f - b, s) and b <= -729/4096))
(bound, sol) = lowerBound(f, RoundTol => infinity);
assert(abs(bound + 729/4096) < 1e-6)

-- [P05] Example 5 (LOG.md 1.8): the trace-minimal Gram matrix of 10 - x^2 - y
-- on the circle with the basis {1, x, y} is unique: rows/columns (1, x, y)
-- [[9, 0, -1/2], [0, 0, 0], [-1/2, 0, 1]], i.e. (3 - y/6)^2 + (35/36) y^2
S = QQ[x,y]/ideal(x^2 + y^2 - 1);
f = 10 - x^2 - y;
for sol in {solveSOS(f, 2, TraceObj => true), solveSOS(f, matrix{{1},{x},{y}}, TraceObj => true)} do (
    mon := flatten entries sol#Monomials;
    assert(set mon === set {1_S, x, y});
    want := hashTable {(1_S,1_S) => 9, (1_S,y) => -1/2, (y,1_S) => -1/2, (y,y) => 1};
    Qwant := matrix(QQ, apply(mon, a -> apply(mon, b -> if want#?(a,b) then want#(a,b) else 0)));
    assert(sol#GramMatrix == Qwant and isCertificate(f, sol));
    assert(length sosPoly sol == 2))
-- without TraceObj the documentation shows three squares; any exact result is a certificate
sol = solveSOS(f, 2);
assert isCertificate(f, sol)

-- [BPT13] Exercise 3.55 (numerical oracle, review section 12): the minimum
-- of (x^3 - 8x + 1)/(x^4 + x^2 + 12) is about -0.4433831 at x = 1.1710047;
-- the denominator is positive, so the SOS bound is the minimum
S = QQ[x];
num = x^3 - 8*x + 1; den = x^4 + x^2 + 12;
(bound, sol) = lowerBound(num/den, RoundTol => infinity);
assert(abs(bound + 0.4433831) < 1e-5)
(bound, sol) = lowerBound(num/den);
assert(ring bound === QQ and bound <= -0.4433831 and isCertificate(num - bound*den, sol))

-- [JSAG] 4: Robinson plus s x^6 + t y^6 is SOS for the returned s, t
-- ([JSAG] prints s = t = 34, the documentation 125/4: one point of many)
S = QQ[x,y,z][symbol s, symbol t];
g = library("Robinson", {x,y,z}) + s*x^6 + t*y^6;
sol = solveSOS g;
p = sol#Parameters;
assert(ring p === QQ and numRows p == 2)
assert isCertificate(sub(g, {s => p_(0,0), t => p_(1,0)}), sol)
-- s = t = 0 is not SOS, so the parameters are not both 0
assert(p != 0)

-- [dKP04] Algorithm 1 on the Motzkin form (sextic): one step, a numerator of
-- degree 2*6 - 4 = 8 and a denominator of degree 2; f * den == num
S = QQ[x,y,z];
f = library("Motzkin", S);
setRandomSeed 0;
(p, q) = sosdecTernary f;
assert(#p == 1 and #q == 1)
pv = value p#0; qv = value q#0;
assert(first degree pv == 8 and first degree qv == 2)
(if all(p | q, a -> coefficientRing ring a === QQ) then (
        assert(f * sub(qv, S) == sub(pv, S));
        assert all(p | q, a -> all(coefficients a, c -> c > 0)))
    else assert(norm(sub(f, ring pv) * sub(qv, ring pv) - pv) < 1e-4))
