-- Random inputs against oracles that do not use the package:
-- (1) f = sum of squares of random dense polynomials: f has a positive
--     definite Gram matrix, so the rounding must give an exact certificate
--     ([PP08] Proposition 8; conventions C2), checked exactly;
-- (2) random sparse SOS: any exact result must be a certificate;
-- (3) random polynomials that are negative somewhere: no exact result;
-- (4) univariate lowerBound against the true minimum (critical points from
--     the roots of f'; univariate nonnegative = SOS, so the relaxation is exact);
-- (5) lowerBound on {0,1}^n (quotient ring) against the minimum over the 2^n
--     points (exact for D = 2n, since f - min is a square of a polynomial of
--     degree at most n there);
-- (6) a multivariate certified bound is below f at random points.
setRandomSeed 20261024;

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
randomPoly = (R, d, density) -> (
    mons := flatten entries basis(0, d, R);
    sum(mons, m -> if random 1.0 < density then (random(-5,5)) * m else 0_R))
monsUpTo = (R, d) -> #flatten entries basis(0, d, R)

-- (1) dense random SOS: exact certificate expected
count1 = 0;
for n in {1, 2, 3} do for d in {1, 2} do for trial to (if n == 3 and d == 2 then 1 else 3) do (
    R := QQ[vars(0..n-1)];
    k := monsUpTo(R, d);
    f := sum(k + 1, i -> (random(1,3)) * (randomPoly(R, d, 1.0))^2);
    sol := solveSOS f;
    if not isCertificate(f, sol) then error("no exact certificate for the dense SOS " | toString f);
    count1 = count1 + 1)
assert(count1 == 22)

-- (2) sparse random SOS (Gram matrices may be singular): every exact result
-- is a certificate, every real result has a small residual
for n in {2, 3} do for trial to 5 do (
    R := QQ[vars(0..n-1)];
    f := sum(2, i -> (randomPoly(R, 2, 0.4))^2);
    if f == 0 then continue;
    sol := solveSOS f;
    if sol#GramMatrix === null then continue;
    if isRational sol then (
        if not isCertificate(f, sol) then error("wrong exact certificate for " | toString f))
    else (
        (mon, Q) := (sol#Monomials, sol#GramMatrix);
        if norm(sub(f, ring mon) - (transpose mon * Q * mon)_(0,0)) > 1e-5 then
            error("large residual for " | toString f)))

-- (3) negative somewhere (f(p) < 0 at an integer point p): no exact result
for n in {1, 2, 3} do for trial to 3 do (
    R := QQ[vars(0..n-1)];
    g := randomPoly(R, 2, 0.8);
    p := apply(n, i -> random(-2, 2));
    c := sub(g, matrix{p});
    f := g^2 - c^2 - 1;              -- f(p) = -1
    sol := solveSOS f;
    -- no exact result, and no real "Gram matrix" either (infeasible, BUGS.md 20)
    if sol#GramMatrix =!= null then error("a Gram matrix for a polynomial that is negative at " | toString p))

-- (4) univariate lowerBound against the minimum over the real critical points
evalRR = (f, r) -> sum(listForm f, (e, c) -> c * r^(e#0))
trueMin = f -> (
    crit := select(roots diff((ring f)_0, f), z -> abs imaginaryPart z < 1e-8);
    min apply(crit, z -> evalRR(f, realPart z)))
R = QQ[x];
for trial to 5 do (
    f := x^(2*random(2,3)) + randomPoly(R, 3, 1.0);
    m := trueMin f;
    (b, sol) := lowerBound(f, RoundTol => infinity);
    if abs(b - m) > 1e-5 * max(1, abs m) then error("real bound " | toString b | " vs min " | toString m | " for " | toString f);
    (b, sol) = lowerBound f;
    if b =!= null and ring b === QQ then (
        if not isCertificate(f - b, sol) then error("bound " | toString b | " not certified for " | toString f);
        if b > m + 1e-9 then error("certified bound above the minimum for " | toString f));
    -- at the optimum the Gram matrix is singular (double root), so a rational
    -- result is not guaranteed ([PP08] Prop. 8 needs a positive definite one):
    -- an exact result must be a certificate, any result must be tight
    (b, sol) = lowerBound(f, RoundTol => 16);
    assert(b =!= null);
    if ring b === QQ then assert(isCertificate(f - b, sol) and b <= m + 1e-9);
    if b < m - 1e-3 * max(1, abs m) then error("bound at RoundTol 16 not tight for " | toString f))

-- (5) quadratic functions on {0,1}^n
for n in {2, 3} do for trial to 2 do (
    A := QQ[vars(0..n-1)];
    S := A/ideal apply(n, i -> A_i^2 - A_i);
    f := sum(n, i -> random(-5,5) * S_i) + sum(n, i -> sum(i, j -> random(-5,5) * S_i * S_j));
    pts := toList((n:0)..(n:1));
    m := min apply(pts, p -> (map(QQ, A, toList p)) lift(f, A));
    (b, sol) := lowerBound(f, 2);
    if b =!= null and ring b === QQ then assert(isCertificate(f - b, sol) and b <= m);
    (b, sol) = lowerBound(f, 2*n, RoundTol => infinity);
    if abs(b - m) > 1e-5 then error("bound " | toString b | " vs min " | toString m | " on the cube for " | toString f);
    (b, sol) = lowerBound(f, 2*n);    -- singular at the optimum, as in (4)
    assert(b =!= null);
    if ring b === QQ then assert(isCertificate(f - b, sol) and b <= m))

-- (6) multivariate: a certified bound is below f at random rational points
R = QQ[x,y];
for trial to 4 do (
    f := (randomPoly(R, 2, 0.8))^2 + (randomPoly(R, 2, 0.8))^2 + randomPoly(R, 1, 1.0);
    (b, sol) := lowerBound f;
    if b === null or ring b =!= QQ then continue;
    assert isCertificate(f - b, sol);
    for i to 30 do (
        p := matrix{{random(-30,30)/random(1,10), random(-30,30)/random(1,10)}};
        if sub(f, p) < b then error("certified bound " | toString b | " above f at " | toString p)))
