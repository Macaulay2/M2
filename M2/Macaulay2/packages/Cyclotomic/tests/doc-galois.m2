-- doc-galois.m2 -- independent tests from the documentation alone.
-- galoisAutomorphism(K, k): for cyclotomicField n and k prime to n the
-- automorphism zeta_n |-> zeta_n^k; for a finite field of characteristic p
-- the k-th power of the Frobenius x |-> x^p.

fieldSize = K -> if instance(K, GaloisField) then K.order else char K;
fieldDeg = K -> if instance(K, GaloisField) then K.degree else 1;
someElements = K -> (
    z := K_0;
    {0_K, 1_K, z, z + 1, 2*z^2 - 3, z^3 + z - 1, (z^2 + 5)^3, rootOfUnityGenerator K});
-- multiplicative order of k modulo n
ordMod = (k, n) -> (j := 1; while (k^j - 1) % n != 0 do j = j+1; j);

-- cyclotomic fields
for n in {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 15, 16} do (
    F := cyclotomicField n; z := F_0;
    w := if even n then n else 2*n;
    units := select(toList(1 .. n), k -> gcd(k, n) == 1);
    for k in units | {-1, n + 1, 1 - n, 2*n + 1} do if gcd(k, n) == 1 then (
        sigma := galoisAutomorphism(F, k);
        assert(instance(sigma, RingMap));
        assert(source sigma === F and target sigma === F);
        -- zeta_n |-> zeta_n^k
        assert(sigma z == z^(k % n));
        -- a ring homomorphism
        els := someElements F;
        for a in els do for b in els do (
            assert(sigma(a + b) == sigma a + sigma b);
            assert(sigma(a * b) == sigma a * sigma b));
        assert(sigma 1_F == 1_F);
        -- e(j/n) |-> e(kj/n)
        for j from 0 to n-1 do assert(sigma cyclotomicRoot(F, j/n) == cyclotomicRoot(F, k*j/n));
        -- roots of unity go to roots of unity of the same order
        for j from 0 to w-1 do
            assert(rootOfUnityOrder sigma cyclotomicRoot(F, j/w) == rootOfUnityOrder cyclotomicRoot(F, j/w));
        -- the order of sigma is the order of k modulo n
        o := ordMod(k % n, n);
        y := z;
        for i from 1 to o do (
            y = sigma y;
            assert((y == z) == (i == o)));
        -- compositions: sigma_k sigma_l = sigma_(kl)
        for l in units do (
            tau := galoisAutomorphism(F, l);
            rho := galoisAutomorphism(F, k*l);
            assert(sigma tau z == rho z);
            assert((sigma * tau) (z + 2*z^2) == rho (z + 2*z^2)));
        ));
-- complex conjugation
F = cyclotomicField 5;
assert((galoisAutomorphism(F, -1)) F_0 == F_0^4);
assert((galoisAutomorphism(F, 4)) F_0 == F_0^4);
F = cyclotomicField 4;
assert((galoisAutomorphism(F, 3)) F_0 == -F_0);

-- finite fields: the k-th power of the Frobenius
fields = {ZZ/2, ZZ/7, GF 4, GF 8, GF 9, GF 16, GF 25, GF 27, GF 64,
    GF(ZZ/3[c]/(c^2+1)), GF(ZZ/2[c]/(c^3+c^2+1)), GF(ZZ/5[c]/(c^2-2)),
    cyclotomicField(5, ZZ/2), cyclotomicField(8, ZZ/5)};
for K in fields do (
    p := char K; n := fieldDeg K; q := fieldSize K;
    g := rootOfUnityGenerator K;
    els := apply(q - 1, i -> g^i) | {0_K};
    for k from -n to 2*n do (
        sigma := galoisAutomorphism(K, k);
        assert(instance(sigma, RingMap));
        assert(source sigma === K and target sigma === K);
        e := p^(k % n);
        for x in els do assert(sigma x == x^e);
        -- e(r) |-> e(p^k r)
        for j in {1, 2, 3, q - 2} do
            assert(sigma cyclotomicRoot(K, j/(q-1)) == cyclotomicRoot(K, e*j/(q-1)));
        -- the identity exactly for k = 0 mod n
        assert((sigma g == g) == (k % n == 0));
        );
    if instance(K, GaloisField) then
        assert((galoisAutomorphism(K, 1)) K_0 == K_0^p);
    );
-- the documented example
K = GF 25;
assert((galoisAutomorphism(K, 1)) K_0 == K_0^5);
assert((galoisAutomorphism(K, 2)) K_0 == K_0);
L = GF(ZZ/3[c]/(c^2+1));
assert((galoisAutomorphism(L, 1)) L_0 == -L_0);
