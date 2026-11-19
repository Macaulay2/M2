-- Roots of unity, exponents, orders, inclusions, automorphisms.
-- characteristic 0
scan({1, 2, 3, 4, 5, 6, 8, 9, 12, 15, 30}, N -> (
    scan(N, k -> (
        q := k / N;
        x := cyclotomicRoot q;
        assert(rootOfUnityExponent x == q);
        assert(rootOfUnityOrder x == denominator q);
        assert(ring x === (if denominator q > 2 then cyclotomicField(denominator q, QQ) else QQ))))));
assert(cyclotomicRoot(1/6) == (cyclotomicField 3)_0 + 1);
assert(ring cyclotomicRoot(1/2) === QQ and cyclotomicRoot(1/2) == -1);
assert(cyclotomicRoot(-1/3) == cyclotomicRoot(2/3) and cyclotomicRoot 5 == 1);
F12 = cyclotomicField 12;
assert(try (cyclotomicRoot(F12, 1/5); false) else true);
assert(rootOfUnityGenerator QQ == -1);
assert(rootOfUnityOrder rootOfUnityGenerator cyclotomicField 5 == 10);
assert(rootOfUnityOrder (2 * (cyclotomicField 5)_0) === null);
assert(rootOfUnityExponent (2 * (cyclotomicField 5)_0) === null);
assert(rootOfUnityOrder(1/2) === null and rootOfUnityOrder(-1) == 2);
-- a number field that is not presented by a cyclotomic polynomial
Sb = QQ[getSymbol "b"];
K = toField(Sb/(Sb_0^2 + Sb_0 + 1));
assert(rootOfUnityOrder K_0 == 3);
-- inclusions: compatible with e(q), and with each other
scan({(3, 12), (4, 12), (6, 12), (5, 15), (3, 6), (6, 3), (2, 4), (1, 5)}, mn -> (
    (m, n) := mn;
    K := cyclotomicField m; L := cyclotomicField n;
    phi := cyclotomicInclusion(K, L);
    scan(2 * m, k -> assert(phi cyclotomicRoot(K, k/(2*m) * (if m % 2 == 0 then 2 else 1)) == cyclotomicRoot(L, k/(2*m) * (if m % 2 == 0 then 2 else 1))))));
psi1 = cyclotomicInclusion(cyclotomicField 3, cyclotomicField 12);
psi2 = cyclotomicInclusion(cyclotomicField 12, cyclotomicField 24);
psi3 = cyclotomicInclusion(cyclotomicField 3, cyclotomicField 24);
assert(psi2 psi1 (cyclotomicField 3)_0 == psi3 (cyclotomicField 3)_0);
assert(try (cyclotomicInclusion(cyclotomicField 5, cyclotomicField 12); false) else true);
-- automorphisms of QQ(zeta_5): the group (ZZ/5)^*, of order 4
F5 = cyclotomicField 5;
s2 = galoisAutomorphism(F5, 2);
assert(s2 F5_0 == F5_0^2 and s2 s2 s2 s2 F5_0 == F5_0 and s2 s2 F5_0 != F5_0);
assert(try (galoisAutomorphism(F5, 5); false) else true);
-- characteristic p
Sc = ZZ/3[getSymbol "c"];
scan({ZZ/2, ZZ/3, ZZ/5, ZZ/7, ZZ/13, GF 4, GF 8, GF 9, GF 25, GF(Sc/(Sc_0^2+1))}, K -> (
    g := rootOfUnityGenerator K;
    w := (if instance(K, GaloisField) then K.order else char K) - 1;  -- the order of the multiplicative group
    assert(rootOfUnityOrder g == w);
    x := 1_K;
    for k from 0 to w - 1 do (
        assert(discreteLog x == k);
        assert(rootOfUnityExponent x == k / w);
        x = x * g);
    assert(rootOfUnityOrder 0_K === null and rootOfUnityExponent 0_K === null)));
-- a large prime field
K = ZZ/32003;
scan({2, 3, 12345, 31999, 32002}, a -> assert((rootOfUnityGenerator K)^(discreteLog a_K) == a_K));
-- fields with roots of unity over finite fields, Conway compatibility
F25 = cyclotomicField(3, ZZ/5);
assert(F25 === cyclotomicField(8, ZZ/5) and F25.order == 25);
assert(try (cyclotomicField(5, ZZ/5); false) else true);
F625 = cyclotomicField(13, ZZ/5);
phi = cyclotomicInclusion(F25, F625);
scan(24, k -> assert(phi cyclotomicRoot(F25, k/24) == cyclotomicRoot(F625, k/24)));
assert(phi rootOfUnityGenerator F25 == (rootOfUnityGenerator F625)^26);
-- a Galois field not given by its Conway polynomial, embedded into GF(81)
Kn = GF(Sc/(Sc_0^2+1));
L = cyclotomicField(5, Kn);
psi = cyclotomicInclusion(Kn, L);
assert((psi Kn_0)^2 == -1);
scan(8, k -> assert(psi cyclotomicRoot(Kn, k/8) == cyclotomicRoot(L, k/8)));
-- the Frobenius
s = galoisAutomorphism(F25, 1);
assert(s F25_0 == F25_0^5 and s s F25_0 == F25_0);
