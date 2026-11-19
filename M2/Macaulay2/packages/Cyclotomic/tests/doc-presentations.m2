-- doc-presentations.m2 -- independent tests from the documentation alone.
-- Several rings for the same field, used one after the other: different
-- polynomials for GF(9) and GF(16), several copies of one Galois field,
-- and rings that differ only in their monoid.  Every result must belong to
-- the ring it was asked for (the documentation promises e.g. "the k with
-- x = gamma^k for gamma = rootOfUnityGenerator K" for the K of x).
needsPackage "ConwayPolynomials";

primeDivs = m -> apply(toList factor m, f -> f#0);
hasOrder = (x, m) -> (x != 0 and x^m == 1 and all(primeDivs m, r -> x^(m // r) != 1));
evalAt = (f, z) -> sum(listForm f, (e, c) -> (lift(c, ZZ)) * z^(e#0));

-- GF(9): the Conway polynomial x^2 - x - 1 and the others x^2 + 1, x^2 + x - 1
G9 = {GF 9, GF(ZZ/3[c]/(c^2+1)), GF(ZZ/3[d]/(d^2+d-1)), GF 9, GF(ZZ/3[c]/(c^2+1))};
-- GF(16): Conway x^4 + x + 1, and x^4 + x^3 + 1, x^4 + x^3 + x^2 + x + 1
G16 = {GF 16, GF(ZZ/2[c]/(c^4+c^3+1)), GF(ZZ/2[d]/(d^4+d^3+d^2+d+1)), GF 16};
for round from 1 to 2 do for Ks in {G9, G16} do for K in Ks do (
    p := char K; n := K.degree; w := K.order - 1;
    g := rootOfUnityGenerator K;
    assert(ring g === K);
    assert(hasOrder(g, w));
    assert(evalAt(conwayPolynomial(p, n), g) == 0);
    k := discreteLog K_0;
    assert(g^k == K_0);
    for j from 0 to w-1 do assert(discreteLog(g^j) == j);
    x := cyclotomicRoot(K, 1/w);
    assert(ring x === K and x == g);
    assert(rootOfUnityExponent(K_0) == k/w);
    assert(rootOfUnityOrder(K_0) == w // gcd(k, w));
    sigma := galoisAutomorphism(K, 1);
    assert(source sigma === K and sigma K_0 == K_0^p);
    -- into the field of degree 2n made by cyclotomicField
    N := if p == 3 then 5 else 17;
    L := cyclotomicField(N, K);
    assert(L.order == p^(2*n));
    phi := cyclotomicInclusion(K, L);
    assert(source phi === K and target phi === L);
    assert(phi g == (rootOfUnityGenerator L)^((L.order - 1) // w));
    assert(phi K_0 == (phi g)^k);
    -- the defining polynomial of K vanishes at the image of K_0
    f := (ideal ambient K)_0;
    assert(evalAt(f, phi K_0) == 0);
    -- the same field L for every presentation of K
    assert(L === cyclotomicField(N, first Ks));
    );
-- elements of two copies of one presentation stay in their copy
A = GF(ZZ/3[c]/(c^2+1)); B = GF(ZZ/3[c]/(c^2+1));
assert(A =!= B);
gA = rootOfUnityGenerator A; gB = rootOfUnityGenerator B;
assert(ring gA === A and ring gB === B);
assert(ring cyclotomicRoot(B, 1/8) === B and ring cyclotomicRoot(A, 1/8) === A);
assert(discreteLog(A_0) == discreteLog(B_0));

-- polynomial rings that differ only in the monoid
F3 = cyclotomicField 3; F4 = cyclotomicField 4;
R1 = F3[x, y]; R2 = F3[x, y, MonomialOrder => Lex]; R3 = F3[x, y, Degrees => {1, 4}];
S = F4[x, y];
T1 = ring first joinCyclotomic {ideal(R1_0), ideal(S_0)};
T2 = ring first joinCyclotomic {ideal(R2_0), ideal(S_0)};
T3 = ring first joinCyclotomic {ideal(R3_0), ideal(S_0)};
assert((options T1).MonomialOrder === (options R1).MonomialOrder);
assert((options T2).MonomialOrder === (options R2).MonomialOrder);
assert(degrees T3 == {{1}, {4}});
assert(T1 =!= T2 and T1 =!= T3);
assert(ring first joinCyclotomic {ideal(R1_0), ideal(S_0)} === T1);
assert(ring first joinCyclotomic {ideal(R2_0), ideal(S_0)} === T2);
E1 = cyclotomicExtension(R1, 4); E2 = cyclotomicExtension(R2, 4); E3 = cyclotomicExtension(R3, 4);
assert((options E1).MonomialOrder === (options R1).MonomialOrder);
assert((options E2).MonomialOrder === (options R2).MonomialOrder);
assert(degrees E3 == {{1}, {4}});
assert(cyclotomicExtension(R1, 4) === E1 and cyclotomicExtension(R2, 4) === E2);
-- two copies of QQ[x, y]
P1 = QQ[x, y]; P2 = QQ[x, y];
Q1 = cyclotomicExtension(P1, 5); Q2 = cyclotomicExtension(P2, 5);
assert((cyclotomicInclusion(P1, Q1)) P1_0 == Q1_0);
assert((cyclotomicInclusion(P2, Q2)) P2_1 == Q2_1);
