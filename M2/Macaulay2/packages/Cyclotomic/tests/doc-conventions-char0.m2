-- doc-conventions-char0.m2 -- independent tests from the documentation alone.
-- Conventions of the top page in characteristic 0:
--   e(q) = exp(2 pi i q); the generator ww_n of cyclotomicField n is e(1/n);
--   rootOfUnityGenerator K = e(1/w) with w = 2 for QQ, w = n for n even,
--   w = 2n for n odd, and for n odd it is -ww_n^((n+1)/2);
--   cyclotomicRoot(K, q) = e(q), an error if K does not contain e(q);
--   cyclotomicRoot q lives in cyclotomicField(N, QQ), N the denominator of q.

-- multiplicative order by brute force (null if > bound)
bruteOrder = (x, bound) -> (
    if x == 0 then return null;
    y := x; k := 1;
    while y != 1 and k < bound do (y = y*x; k = k+1);
    if y == 1 then k else null);
-- the n-th cyclotomic polynomial, evaluated at z, from its definition
-- x^n - 1 = prod_{d | n} Phi_d, computed independently of the package
tRing = QQ[symbol t];
Phi = memoize(n -> (
    f := (tRing_0)^n - 1;
    for d from 1 to n-1 do if n % d == 0 then f = f // Phi d;
    f));
evalAt = (f, z) -> sum(listForm f, (e, c) -> c * z^(e#0));

-- QQ
assert(rootOfUnityGenerator QQ == -1);
assert(ring rootOfUnityGenerator QQ === QQ);
assert(cyclotomicRoot(QQ, 0) == 1 and ring cyclotomicRoot(QQ, 0) === QQ);
assert(cyclotomicRoot(QQ, 1/2) == -1 and cyclotomicRoot(QQ, -1/2) == -1);
assert(cyclotomicRoot(QQ, 3/2) == -1 and cyclotomicRoot(QQ, 1) == 1);
assert(cyclotomicRoot(QQ, 7) == 1 and ring cyclotomicRoot(QQ, 7) === QQ);
assert(cyclotomicRoot(QQ, -3) == 1);
assert(try (cyclotomicRoot(QQ, 1/3); false) else true);
assert(try (cyclotomicRoot(QQ, 1/4); false) else true);
assert(try (cyclotomicRoot(QQ, 2/5); false) else true);

for n from 1 to 24 do (
    F := cyclotomicField n;
    z := F_0;
    -- the generator is a root of Phi_n of exact order n
    assert(evalAt(Phi n, z) == 0);
    assert(bruteOrder(z, 100) == n);
    -- the group of roots of unity and its generator
    w := if even n then n else 2*n;
    g := rootOfUnityGenerator F;
    assert(ring g === F);
    assert(bruteOrder(g, 100) == w);
    assert(g == cyclotomicRoot(F, 1/w));
    if even n then assert(g == z) else assert(g == -z^((n+1)//2));
    -- e(k/w) = g^k, including negative and large k
    for k from -w to 2*w do assert(cyclotomicRoot(F, k/w) == g^k);
    -- the generator is e(1/n)
    for k from 0 to n-1 do assert(cyclotomicRoot(F, k/n) == z^k);
    assert(cyclotomicRoot(F, 1/n) == z);
    -- e(1/(2n)) for n odd
    if odd n then assert(cyclotomicRoot(F, 1/(2*n)) == -z^((n+1)//2));
    -- cyclotomicRoot(F, q) has exactly the order of the denominator of q
    for k from 0 to w-1 do (
        q := k/w;
        assert(bruteOrder(cyclotomicRoot(F, q), 100) == denominator q));
    -- integer exponents (the method for ZZ)
    assert(cyclotomicRoot(F, 0) == 1 and ring cyclotomicRoot(F, 0) === F);
    assert(cyclotomicRoot(F, 5) == 1 and cyclotomicRoot(F, -2) == 1);
    -- K does not contain e(q): an error
    for d in {3, 4, 5, 7, 8, 9, 16, 25} do
        if w % d != 0 then
            assert(try (cyclotomicRoot(F, 1/d); false) else true);
    -- the ring is made once
    assert(cyclotomicField n === F);
    );

-- odd n: cyclotomicField n and cyclotomicField(2n) are different rings,
-- with generators e(1/n) and e(1/(2n))
for n in {1, 3, 5, 7, 9, 15} do (
    F := cyclotomicField n; G := cyclotomicField(2*n);
    assert(F =!= G);
    assert(cyclotomicRoot(G, 1/(2*n)) == G_0);
    assert(cyclotomicRoot(F, 1/n) == F_0);
    assert(bruteOrder(G_0, 100) == 2*n);
    assert(rootOfUnityGenerator G == G_0);
    );

-- cyclotomicRoot q without a ring: it lives in cyclotomicField(N, QQ)
for N from 1 to 30 do
    for j from -N to 2*N do (
        q := j/N;
        x := cyclotomicRoot q;
        d := denominator q;
        assert(ring x === cyclotomicField(d, QQ));
        assert(bruteOrder(x, 100) == d);
        assert(x == cyclotomicRoot(ring x, q));
        );
assert(ring cyclotomicRoot(0/1) === QQ and cyclotomicRoot(0/1) == 1);
assert(ring cyclotomicRoot 0 === QQ and cyclotomicRoot 0 == 1);
assert(ring cyclotomicRoot 3 === QQ and cyclotomicRoot 3 == 1);
assert(ring cyclotomicRoot(-2) === QQ and cyclotomicRoot(-2) == 1);
assert(ring cyclotomicRoot(1/2) === QQ and cyclotomicRoot(1/2) == -1);
assert(ring cyclotomicRoot(-1/2) === QQ and cyclotomicRoot(-1/2) == -1);
assert(ring cyclotomicRoot(1/4) === cyclotomicField 4 and cyclotomicRoot(1/4) == (cyclotomicField 4)_0);
assert(ring cyclotomicRoot(1/6) === cyclotomicField 3);
assert(ring cyclotomicRoot(1/10) === cyclotomicField 5);
assert(cyclotomicRoot(1/10) == -(cyclotomicField 5)_0^3);
assert(ring cyclotomicRoot(1/12) === cyclotomicField 12 and cyclotomicRoot(1/12) == (cyclotomicField 12)_0);
assert(ring cyclotomicRoot(5/18) === cyclotomicField 9);
assert(cyclotomicRoot(-1/3) == (cyclotomicField 3)_0^2);
assert(cyclotomicRoot(4/3) == (cyclotomicField 3)_0);
-- e(q) e(r) = e(q + r) in a common field
F = cyclotomicField 60;
for i from 0 to 11 do for j from 0 to 9 do (
    q := i/12; r := j/10;
    assert(cyclotomicRoot(F, q) * cyclotomicRoot(F, r) == cyclotomicRoot(F, q + r)));

-- the generator of cyclotomicField(N, K) (char 0) is again e(1/w)
for p in {(4, 3), (5, 3), (3, 4), (8, 6), (5, 2)} do (
    L := cyclotomicField(p#0, cyclotomicField p#1);
    g := rootOfUnityGenerator L;
    w := bruteOrder(g, 200);
    assert(g == cyclotomicRoot(L, 1/w));
    assert(cyclotomicRoot(L, 1/(p#0)) != 0);
    assert(bruteOrder(cyclotomicRoot(L, 1/(p#0)), 200) == p#0);
    );
