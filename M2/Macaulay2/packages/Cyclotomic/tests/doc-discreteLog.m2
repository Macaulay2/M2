-- doc-discreteLog.m2 -- independent tests from the documentation alone.
-- discreteLog x, for a nonzero x in ZZ/p or a Galois field K, is the k with
-- 0 <= k < |K| - 1 and x = gamma^k, gamma = rootOfUnityGenerator K.
-- Checked against brute force on small fields, on samples in larger ones.

fieldSize = K -> if instance(K, GaloisField) then K.order else char K;
fieldDeg = K -> if instance(K, GaloisField) then K.degree else 1;
-- all elements of K, written in the basis 1, K_0, ..., K_0^(n-1)
allElements = K -> (
    p := char K; n := fieldDeg K;
    if not instance(K, GaloisField) then return apply(p, i -> i_K);
    vecs := {{}};
    for i from 1 to n do vecs = flatten apply(vecs, v -> apply(p, c -> append(v, c)));
    apply(vecs, v -> sum(n, i -> v#i * K_0^i)));

small = {ZZ/2, ZZ/3, ZZ/5, ZZ/7, ZZ/11, ZZ/13, ZZ/101, GF 4, GF 8, GF 9, GF 16,
    GF 25, GF 27, GF 32, GF 49, GF 121, GF 125, GF 128, GF 243,
    GF(ZZ/3[c]/(c^2+1)), GF(ZZ/2[c]/(c^3+c^2+1)), GF(ZZ/5[c]/(c^2-2)),
    GF(ZZ/2[c]/(c^4+c^3+c^2+c+1)), GF(ZZ/7[c]/(c^2+1))};
for K in small do (
    w := fieldSize K - 1;
    g := rootOfUnityGenerator K;
    -- table of all powers of gamma
    tab := new MutableHashTable;
    y := 1_K;
    for k from 0 to w-1 do (tab#y = k; y = y * g);
    assert(#keys tab == w);
    els := select(allElements K, x -> x != 0);
    assert(#els == w);
    for x in els do (
        k := discreteLog x;
        assert(instance(k, ZZ));
        assert(0 <= k and k < w);
        assert(k == tab#x);
        assert(g^k == x));
    assert(discreteLog(1_K) == 0);
    if w > 1 then assert(discreteLog g == 1);
    if odd fieldSize K then assert(discreteLog(-1_K) == w // 2);
    );

-- larger fields: random elements and random powers of gamma
large = {ZZ/32003, ZZ/65521, ZZ/1000003, ZZ/2147483647, GF(2^20), GF(3^10),
    GF(7^5), GF(2^31), GF(ZZ/32003[c]/(c^2-2)), GF(ZZ/101[c]/(c^2-3))};
for K in large do (
    w := fieldSize K - 1;
    g := rootOfUnityGenerator K;
    for k in {0, 1, 2, w-1, w-2, w//2} | apply(15, i -> random w) do
        assert(discreteLog(g^k) == k);
    for i from 1 to 15 do (
        x := random K;
        if x != 0 then (
            k := discreteLog x;
            assert(0 <= k and k < w and g^k == x)));
    );
-- the documented example
K = ZZ/32003;
assert(discreteLog 12345_K == 2702);
L = GF(2^20);
assert(discreteLog(L_0^1000 + 1) == 387868);
-- discreteLog is a homomorphism to ZZ/(|K|-1)
K = GF(3^7); w = K.order - 1;
for i from 1 to 20 do (
    x := random K; y := random K;
    if x != 0 and y != 0 then
        assert(discreteLog(x*y) == (discreteLog x + discreteLog y) % w));
-- a large field whose group order has only prime factors below 2^38
L = GF(2^71);  -- 2^71 - 1 = 228479 * 48544121 * 212885833
g = rootOfUnityGenerator L;
for i from 1 to 3 do (
    x := random L;
    if x != 0 then (k := discreteLog x; assert(0 <= k and k < 2^71 - 1 and g^k == x)));
-- caveat: a prime factor of |K| - 1 above 2^38 raises an error
L = GF(2^61);  -- 2^61 - 1 is prime
assert(try (discreteLog(L_0 + 1); false) else true);
L = GF(2^59);  -- 2^59 - 1 = 179951 * 3203431780337
assert(try (discreteLog(L_0 + 1); false) else true);
