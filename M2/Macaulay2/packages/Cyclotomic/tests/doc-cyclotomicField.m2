-- doc-cyclotomicField.m2 -- independent tests from the documentation alone.
-- cyclotomicField n is QQ[ww_n]/Phi_n, made once for every n.
-- cyclotomicField(N, K) chooses one ring for every field:
--   char 0: K if K contains the N-th roots of unity, otherwise
--           cyclotomicField M for the smallest possible M (M = 2 mod 4
--           replaced by M/2);
--   char p: K if K contains them, otherwise GF(p, m) made from its Conway
--           polynomial, with generator ww_(p^m-1), where m is the lcm of the
--           degree of K and the order of p modulo N.
needsPackage "ConwayPolynomials";

primeDivs = m -> apply(toList factor m, f -> f#0);
hasOrder = (x, m) -> (x != 0 and x^m == 1 and all(primeDivs m, r -> x^(m // r) != 1));
evalAt = (f, z) -> sum(listForm f, (e, c) -> (lift(c, ZZ)) * z^(e#0));
multOrder = (p, N) -> (k := 1; while (p^k - 1) % N != 0 do k = k+1; k);
-- order w of the group of roots of unity of QQ or cyclotomicField n
wOf = n -> if even n then n else 2*n;

-- cyclotomicField n
for n from 1 to 30 do (
    F := cyclotomicField n;
    assert(cyclotomicField n === F);
    if n >= 3 then assert(toString F_0 == ("ww_" | toString n));
    assert(isField F);
    -- it is a field: nonzero elements are invertible
    a := 1 + 2*F_0 - F_0^2/3;
    if a != 0 then assert(a * a^-1 == 1);
    -- F_0 = e(1/n) has order n and is a root of Phi_n
    t := (QQ[symbol t])_0;
    assert(evalAt(cyclotomicPoly(n, t), F_0) == 0);
    );
-- odd n: cyclotomicField n and cyclotomicField 2n are different rings
for n in {1, 3, 5, 7, 9, 11} do assert(cyclotomicField n =!= cyclotomicField(2*n));

-- char 0, cyclotomicField(N, K)
-- expected ring, from the documented rule
expected0 = (N, n) -> (
    -- K = QQ is passed as n = 1 with w = 2
    w := if n == 0 then 2 else wOf n;
    L := lcm(N, max(n, 1));
    if L % 4 == 2 then L = L // 2;
    if w % N == 0 then null else cyclotomicField L);
for N from 1 to 24 do (
    -- K = QQ
    e := expected0(N, 0);
    if e === null then assert(cyclotomicField(N, QQ) === QQ)
    else assert(cyclotomicField(N, QQ) === e);
    -- K = cyclotomicField n
    -- (K = cyclotomicField 1 or 2 with N = 1 or 2: see doc-FINDING-cyclotomicField-K-trivial.m2)
    for n from 1 to 12 do if not (n <= 2 and N <= 2) and lcm(N, n) <= 48 then (
        K := cyclotomicField n;
        e = expected0(N, n);
        if e === null then assert(cyclotomicField(N, K) === K)
        else assert(cyclotomicField(N, K) === e);
        ));
-- some explicit values
assert(cyclotomicField(1, QQ) === QQ);
assert(cyclotomicField(2, QQ) === QQ);
assert(cyclotomicField(3, QQ) === cyclotomicField 3);
assert(cyclotomicField(4, QQ) === cyclotomicField 4);
assert(cyclotomicField(10, QQ) === cyclotomicField 5);
assert(cyclotomicField(18, QQ) === cyclotomicField 9);
assert(cyclotomicField(12, QQ) === cyclotomicField 12);
assert(cyclotomicField(4, cyclotomicField 3) === cyclotomicField 12);
assert(cyclotomicField(3, cyclotomicField 4) === cyclotomicField 12);
assert(cyclotomicField(6, cyclotomicField 4) === cyclotomicField 12);
assert(cyclotomicField(12, cyclotomicField 6) === cyclotomicField 12);
assert(cyclotomicField(5, cyclotomicField 6) === cyclotomicField 15);
assert(cyclotomicField(10, cyclotomicField 3) === cyclotomicField 15);
assert(cyclotomicField(6, cyclotomicField 3) === cyclotomicField 3);
assert(cyclotomicField(2, cyclotomicField 5) === cyclotomicField 5);
assert(cyclotomicField(3, cyclotomicField 6) === cyclotomicField 6);
assert(cyclotomicField(1, cyclotomicField 7) === cyclotomicField 7);
-- the result contains the N-th roots of unity
for N in {3, 4, 5, 8, 9, 12, 20} do for n in {1, 3, 4, 6} do (
    L := cyclotomicField(N, cyclotomicField n);
    assert(hasOrder(cyclotomicRoot(L, 1/N), N)));

-- char p, cyclotomicField(N, K)
fieldSize = K -> if instance(K, GaloisField) then K.order else char K;
fieldDeg = K -> if instance(K, GaloisField) then K.degree else 1;
checkCharP = (N, K) -> (
    p := char K; d := fieldDeg K; q := fieldSize K;
    L := cyclotomicField(N, K);
    assert(cyclotomicField(N, K) === L);
    if (q - 1) % N == 0 then (assert(L === K); return L);
    m := lcm(d, multOrder(p, N));
    assert(instance(L, GaloisField));
    assert(char L == p and L.degree == m and L.order == p^m);
    assert(toString L_0 == ("ww_" | toString(p^m - 1)));
    -- made from the Conway polynomial; ww is the generator gamma
    assert(evalAt(conwayPolynomial(p, m), L_0) == 0);
    assert(hasOrder(L_0, p^m - 1));
    assert(rootOfUnityGenerator L == L_0);
    -- the N-th roots of unity are there
    assert(hasOrder(cyclotomicRoot(L, 1/N), N));
    L);
-- one ring for every field: remember the ring of each (p, m)
seen = new MutableHashTable;
remember = L -> if instance(L, GaloisField) and toString L_0 == ("ww_" | toString(L.order - 1)) then (
    key := (char L, L.degree);
    if seen#?key then assert(seen#key === L) else seen#key = L);
baseFields = {ZZ/2, ZZ/3, ZZ/5, ZZ/7, ZZ/11, GF 4, GF 8, GF 9, GF 25, GF 49,
    GF(ZZ/3[c]/(c^2+1)), GF(ZZ/5[c]/(c^2-2)), GF(ZZ/2[c]/(c^3+c^2+1))};
-- size of the field that cyclotomicField(N, K) should make
targetSize = (N, K) -> (char K)^(lcm(fieldDeg K, multOrder(char K, N)));
for K in baseFields do (
    p := char K;
    for N from 1 to 26 do if N % p != 0 and targetSize(N, K) <= 10^5 then (
        L := checkCharP(N, K);
        remember L;
        -- iterate once more: cyclotomicField(N', L) for a field L made here
        if L =!= K then for N' in {3, 5} do
            if N' % p != 0 and targetSize(N', L) <= 10^5 then remember checkCharP(N', L);
        ));
-- caveat: no Conway polynomial known for large m; an error, not a wrong field
-- (32003 has order 5 modulo 11, and no Conway polynomial of degree 5 is known)
assert(try (cyclotomicField(11, ZZ/32003); false) else true);
-- explicit values
assert(cyclotomicField(4, ZZ/5) === ZZ/5);
assert(cyclotomicField(1, ZZ/5) === ZZ/5);
assert(cyclotomicField(6, ZZ/7) === ZZ/7);
assert(cyclotomicField(3, ZZ/5) === cyclotomicField(8, ZZ/5));
assert(cyclotomicField(3, ZZ/5) === cyclotomicField(24, ZZ/5));
assert(cyclotomicField(13, ZZ/5) === cyclotomicField(13, cyclotomicField(3, ZZ/5)));
assert(cyclotomicField(13, ZZ/5) === cyclotomicField(16, ZZ/5));
K25 = GF 25;
assert(cyclotomicField(3, K25) === K25);
assert(cyclotomicField(13, K25) === cyclotomicField(13, ZZ/5));
assert(cyclotomicField(3, ZZ/2) === cyclotomicField(3, cyclotomicField(3, ZZ/2)));
assert(cyclotomicField(3, ZZ/2) =!= cyclotomicField(7, ZZ/2));
assert((cyclotomicField(7, ZZ/2)).order == 8);
assert((cyclotomicField(5, ZZ/2)).order == 16);
assert((cyclotomicField(3, cyclotomicField(7, ZZ/2))).order == 64);
assert((cyclotomicField(5, GF 9)).order == 81);
assert((cyclotomicField(13, cyclotomicField(3, ZZ/5))).order == 625);
assert(toString (cyclotomicField(8, ZZ/5))_0 == "ww_24");
