-- doc-conventions-charp.m2
-- Conventions of the top page in characteristic p:
--   for K = ZZ/p or GF(p^n) the group of roots of unity has order
--   w = p^n - 1 and its generator gamma = rootOfUnityGenerator K is a root
--   of the Conway polynomial of degree n: the least primitive root mod p,
--   the variable of GF(p,n), or for a Galois field defined by another
--   polynomial a root of the Conway polynomial;  e(k/w) = gamma^k;
--   cyclotomicRoot(K, q) is an error if K does not contain e(q).
needsPackage "ConwayPolynomials";

primeDivs = m -> apply(toList factor m, f -> f#0);
-- x has multiplicative order exactly m
hasOrder = (x, m) -> (x != 0 and x^m == 1 and all(primeDivs m, r -> x^(m // r) != 1));
-- evaluate a polynomial over ZZ/p at an element of a finite field
evalAt = (f, z) -> sum(listForm f, (e, c) -> (lift(c, ZZ)) * z^(e#0));
leastPrimitiveRoot = p -> (
    if p == 2 then return 1;
    g := 2;
    while not all(primeDivs(p-1), r -> powermod(g, (p-1)//r, p) != 1) do g = g+1;
    g);
fieldSize = K -> if instance(K, GaloisField) then K.order else char K;
fieldDeg = K -> if instance(K, GaloisField) then K.degree else 1;

-- prime fields: gamma is the least primitive root
for p in {2, 3, 5, 7, 11, 13, 17, 23, 41, 101, 32003, 65521, 1000003, 2147483647} do (
    K := ZZ/p;
    g := rootOfUnityGenerator K;
    assert(ring g === K);
    assert(g == leastPrimitiveRoot p);
    assert(hasOrder(g, p-1));
    f := conwayPolynomial(p, 1);
    if f =!= null then assert(evalAt(f, g) == 0);
    );

-- Galois fields as Macaulay2 makes them (Conway polynomials), and Galois
-- fields defined by other polynomials
nonConway = {
    GF(ZZ/3[c]/(c^2+1)),            -- c has order 4, not primitive
    GF(ZZ/2[c]/(c^3+c^2+1)),        -- the reciprocal of the Conway polynomial
    GF(ZZ/5[c]/(c^2-2)),            -- c has order 8
    GF(ZZ/2[c]/(c^4+c^3+c^2+c+1)),  -- c has order 5
    GF(ZZ/7[c]/(c^2+1)),            -- c has order 4
    GF(ZZ/3[c]/(c^3-c-1))};         -- primitive, but not the Conway polynomial
fields = {ZZ/2, ZZ/3, ZZ/5, ZZ/7, ZZ/32003, GF 4, GF 8, GF 9, GF 16, GF 25,
    GF 27, GF 49, GF 64, GF 81, GF 125, GF(2^10), GF(3^7)} | nonConway;

for K in fields do (
    q := fieldSize K; n := fieldDeg K; p := char K;
    w := q - 1;
    g := rootOfUnityGenerator K;
    assert(ring g === K);
    assert(hasOrder(g, w));
    assert(evalAt(conwayPolynomial(p, n), g) == 0);
    -- Macaulay2's GF uses the Conway polynomial; then gamma is the variable
    if instance(K, GaloisField) and evalAt(conwayPolynomial(p, n), K_0) == 0 then
        assert(g == K_0);
    -- e(k/w) = gamma^k: all k for small fields, a sample for large ones
    ks := if w <= 130 then toList(-w .. 2*w) else
        {0, 1, 2, 3, w-1, w, w+1, -1, -7} | apply(30, i -> random(-w, 2*w));
    for k in ks do assert(cyclotomicRoot(K, k/w) == g^k);
    -- cyclotomicRoot(K, q) has exactly the order of the denominator of q
    for d in select(toList(1 .. min(w, 200)), d -> w % d == 0) do
        for j in {1, d-1, d+1, -1} do (
            r := j/d;
            assert(hasOrder(cyclotomicRoot(K, r), denominator r)));
    -- integer exponents
    assert(cyclotomicRoot(K, 0) == 1 and ring cyclotomicRoot(K, 0) === K);
    assert(cyclotomicRoot(K, 1) == 1 and cyclotomicRoot(K, -4) == 1);
    -- missing roots of unity: an error (orders not dividing w, multiples of p)
    for d in {2, 3, 4, 5, 7, 8, 9, 11, 13, 16, 31, 32, 49} do
        if w % d != 0 then assert(try (cyclotomicRoot(K, 1/d); false) else true);
    );

-- the documented generators
assert(rootOfUnityGenerator(ZZ/7) == 3);
assert(rootOfUnityGenerator(ZZ/2) == 1);
assert(rootOfUnityGenerator(ZZ/3) == -1);
L = GF(ZZ/3[c]/(c^2+1));
assert(rootOfUnityGenerator L == -L_0 - 1);
-- char 2: there is no root of unity of order 2
assert(try (cyclotomicRoot(ZZ/2, 1/2); false) else true);
assert(try (cyclotomicRoot(GF 4, 1/2); false) else true);
K4 = GF 4; assert(cyclotomicRoot(K4, 1/3) == K4_0);
assert(cyclotomicRoot(ZZ/3, 1/2) == -1);
assert(cyclotomicRoot(ZZ/5, 1/4) == 2 and cyclotomicRoot(ZZ/5, 3/4) == 3);
