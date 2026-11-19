-- doc-inclusion.m2 -- independent tests from the documentation alone.
-- cyclotomicInclusion(A, B) is the inclusion A -> B compatible with the roots
-- of unity, e(q) |-> e(q); for polynomial rings the variables go to the
-- variables in order.  Conway polynomials make the choices in
-- characteristic p compatible with GF(p^n) in GF(p^m).  A Galois field not
-- defined by its Conway polynomial is embedded by sending its variable,
-- gamma^k with k = discreteLog, to the k-th power of the image of gamma.

primeDivs = m -> apply(toList factor m, f -> f#0);
hasOrder = (x, m) -> (x != 0 and x^m == 1 and all(primeDivs m, r -> x^(m // r) != 1));
fieldSize = K -> if instance(K, GaloisField) then K.order else char K;
fieldDeg = K -> if instance(K, GaloisField) then K.degree else 1;
-- the order w of the group of roots of unity, found by testing
wOf = K -> if char K > 0 then fieldSize K - 1 else (
    g := rootOfUnityGenerator K; k := 1; y := g;
    while y != 1 do (y = y*g; k = k+1); k);
-- some elements of a field, for the homomorphism checks
someElements = K -> (
    z := if K === QQ then 1_K else K_0;
    {0_K, 1_K, z, z + 1, 2*z^2 - 3, z^3 + z - 1, (z^2 + 5)^3, rootOfUnityGenerator K});

-- a ring map phi: A -> B of fields is the inclusion compatible with e(q)
checkInclusion = (A, B) -> (
    phi := cyclotomicInclusion(A, B);
    assert(instance(phi, RingMap));
    assert(source phi === A and target phi === B);
    -- well defined: the generator of A goes to a root of its defining relation
    if A =!= QQ and not (char A > 0 and not instance(A, GaloisField)) then (
        I := if instance(A, GaloisField) then ideal ambient A else ideal coefficientRing A;
        f := I_0;
        assert(sum(listForm f, (e, c) -> phi(promote(c, A)) * (phi A_0)^(e#0)) == 0));
    -- a ring homomorphism
    els := someElements A;
    for a in els do for b in els do (
        assert(phi(a + b) == phi a + phi b);
        assert(phi(a * b) == phi a * phi b));
    assert(phi 1_A == 1_B and phi 0_A == 0_B);
    -- injective on the roots of unity: e(q) |-> e(q)
    w := wOf A;
    ks := if w <= 60 then toList(0 .. w-1) else {0, 1, 2, w-1} | apply(10, i -> random w);
    for k in ks do assert(phi cyclotomicRoot(A, k/w) == cyclotomicRoot(B, k/w));
    assert(phi rootOfUnityGenerator A == cyclotomicRoot(B, 1/w));
    phi);

-- characteristic 0
pairs0 = {(1, 3), (1, 4), (2, 5), (3, 6), (6, 3), (3, 12), (4, 12), (6, 12),
    (5, 10), (10, 5), (5, 15), (3, 15), (6, 30), (8, 24), (9, 18), (18, 9),
    (2, 4), (4, 8), (7, 7), (12, 12), (4, 20), (15, 60)};
for p in pairs0 do checkInclusion(cyclotomicField p#0, cyclotomicField p#1);
for n in {1, 2, 3, 4, 5, 8} do checkInclusion(QQ, cyclotomicField n);
checkInclusion(QQ, QQ);
-- the inclusion of an odd order field into its double is an isomorphism
phi = cyclotomicInclusion(cyclotomicField 3, cyclotomicField 6);
psi = cyclotomicInclusion(cyclotomicField 6, cyclotomicField 3);
z3 = (cyclotomicField 3)_0; z6 = (cyclotomicField 6)_0;
assert(phi z3 == z6^2 and psi z6 == z3 + 1);
assert(psi phi z3 == z3 and phi psi z6 == z6);
-- compositions of inclusions agree
triples0 = {(3, 6, 12), (1, 3, 15), (4, 12, 24), (5, 10, 20), (3, 12, 60), (6, 3, 30)};
for t in triples0 do (
    A := cyclotomicField t#0; B := cyclotomicField t#1; C := cyclotomicField t#2;
    f := cyclotomicInclusion(A, B); g := cyclotomicInclusion(B, C);
    h := cyclotomicInclusion(A, C);
    for a in someElements A do assert(g f a == h a);
    assert((g * f) A_0 == h A_0));
-- a field not containing A: an inclusion would not be a ring map
assert(try (cyclotomicInclusion(cyclotomicField 4, cyclotomicField 3); false) else true);
assert(try (cyclotomicInclusion(cyclotomicField 5, cyclotomicField 12); false) else true);

-- characteristic p
K4 = GF 4; K8 = GF 8; K16 = GF 16; K64 = GF 64; K256 = GF 256;
K9 = GF 9; K27 = GF 27; K81 = GF 81; K729 = GF 729;
K25 = GF 25; K125 = GF 125; K625 = GF 625; K49 = GF 49;
N9 = GF(ZZ/3[c]/(c^2+1));
N25 = GF(ZZ/5[c]/(c^2-2));
N8 = GF(ZZ/2[c]/(c^3+c^2+1));
N16 = GF(ZZ/2[c]/(c^4+c^3+c^2+c+1));
pairsP = {(ZZ/2, K4), (ZZ/2, K8), (K4, K16), (K4, K64), (K8, K64), (K16, K256),
    (K4, K256), (ZZ/3, K9), (K9, K81), (K9, K729), (K27, K729), (ZZ/3, K27),
    (ZZ/5, K25), (ZZ/5, K125), (K25, K625), (ZZ/7, K49),
    (N9, K81), (N9, K729), (N25, K625), (N8, K64), (N16, K256), (ZZ/3, N9),
    (N9, K9), (N25, K25), (N8, N8), (K9, K9),
    -- (K9, N9), (K25, N25), (K4, N16): see doc-FINDING-inclusion-nonConway-target.m2
    (N9, cyclotomicField(5, N9)), (K9, cyclotomicField(5, K9)),
    (ZZ/5, cyclotomicField(8, ZZ/5)), (cyclotomicField(3, ZZ/5), K625),
    (cyclotomicField(3, ZZ/2), cyclotomicField(5, ZZ/2)), (K81, cyclotomicField(5, K9))};
for p in pairsP do (
    A := p#0; B := p#1;
    phi := checkInclusion(A, B);
    -- Conway compatibility: gamma_A |-> gamma_B^((|B|-1)/(|A|-1))
    gA := rootOfUnityGenerator A; gB := rootOfUnityGenerator B;
    assert(phi gA == gB^((fieldSize B - 1) // (fieldSize A - 1)));
    -- the variable of a Galois field A, gamma^k, goes to the k-th power of the image of gamma
    if instance(A, GaloisField) then
        assert(phi A_0 == (phi gA)^(discreteLog A_0));
    -- injective
    if instance(A, GaloisField) then assert(hasOrder(phi A_0, rootOfUnityOrder A_0));
    );
-- two Galois fields of the same order made from the Conway polynomial: the
-- variables correspond
phi = cyclotomicInclusion(K81, cyclotomicField(5, K9));
assert(phi K81_0 == (cyclotomicField(5, K9))_0);
-- compositions in characteristic p
triplesP = {(K4, K16, K256), (ZZ/2, K4, K16), (N9, K81, GF 6561), (ZZ/3, N9, K81),
    (N9, K9, K81), (ZZ/5, K25, K625), (N25, K25, K625), (N16, K16, K256)};
for t in triplesP do (
    A := t#0; B := t#1; C := t#2;
    f := cyclotomicInclusion(A, B); g := cyclotomicInclusion(B, C);
    h := cyclotomicInclusion(A, C);
    for a in someElements A do assert(g f a == h a));
assert(try (cyclotomicInclusion(K8, K16); false) else true);
assert(try (cyclotomicInclusion(K9, K27); false) else true);

-- polynomial rings: variables go to variables in order, coefficients by the
-- inclusion of the coefficient fields
F3 = cyclotomicField 3; F12 = cyclotomicField 12;
R = F3[x, y]; T = F12[u, v];
phi = cyclotomicInclusion(R, T);
assert(source phi === R and target phi === T);
assert(phi R_0 == T_0 and phi R_1 == T_1);
z = F3_0;
assert(phi(z * R_0 + R_1^2) == cyclotomicRoot(F12, 1/3) * T_0 + T_1^2);
f = (R_0 - z)^2 * (R_1 + z^2) - 1; g = R_0*R_1 + z;
assert(phi(f * g) == phi f * phi g and phi(f + g) == phi f + phi g);
R = QQ[x, y, z]; T = cyclotomicExtension(R, 5);
phi = cyclotomicInclusion(R, T);
assert(phi R_0 == T_0 and phi R_1 == T_1 and phi R_2 == T_2);
assert(phi(R_0^2 - 1/2 * R_1 * R_2) == T_0^2 - 1/2 * T_1 * T_2);
R = K9[x]; T = K81[t];
phi = cyclotomicInclusion(R, T);
assert(phi R_0 == T_0);
assert(phi(K9_0 * R_0) == (cyclotomicInclusion(K9, K81)) K9_0 * T_0);
R = N9[x, y]; T = cyclotomicExtension(R, 5);
phi = cyclotomicInclusion(R, T);
assert(phi(N9_0 * R_1) == (cyclotomicInclusion(N9, coefficientRing T)) N9_0 * T_1);
assert((phi(N9_0 * R_0))^2 == -T_0^2);
-- the same ring: the identity
R = F3[x, y];
phi = cyclotomicInclusion(R, R);
assert(phi(z * R_0 + R_1) == z * R_0 + R_1);
