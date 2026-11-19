-- doc-exponent-order.m2 -- independent tests from the documentation alone.
-- rootOfUnityExponent x: the rational q with 0 <= q < 1 and x = e(q), or
--   null if x is not a root of unity (inputs in QQ, a cyclotomic field,
--   ZZ/p, a Galois field; also numbers).
-- rootOfUnityOrder x: the least n > 0 with x^n = 1, or null if there is
--   none (inputs in QQ, a number field QQ[a]/(f), ZZ/p, a Galois field;
--   also numbers).  The function always stops.

fracPart = q -> q - floor q;
bruteOrder = (x, bound) -> (
    if x == 0 then return null;
    y := x; k := 1;
    while y != 1 and k < bound do (y = y*x; k = k+1);
    if y == 1 then k else null);

-- numbers
assert(rootOfUnityExponent 1 == 0 and instance(rootOfUnityExponent 1, QQ));
assert(rootOfUnityExponent(-1) == 1/2 and instance(rootOfUnityExponent(-1), QQ));
assert(rootOfUnityExponent(1/1) == 0);
assert(rootOfUnityExponent(-1/1) == 1/2);
assert(rootOfUnityExponent 2 === null);
assert(rootOfUnityExponent 0 === null);
assert(rootOfUnityExponent(1/2) === null);
assert(rootOfUnityExponent(-3) === null);
assert(rootOfUnityOrder 1 == 1 and instance(rootOfUnityOrder 1, ZZ));
assert(rootOfUnityOrder(-1) == 2);
assert(rootOfUnityOrder(-1/1) == 2);
assert(rootOfUnityOrder 0 === null);
assert(rootOfUnityOrder 2 === null);
assert(rootOfUnityOrder(1/2) === null);
assert(rootOfUnityOrder(-2/3) === null);
-- elements of QQ as ring elements
assert(rootOfUnityExponent(1_QQ) == 0 and rootOfUnityExponent(-1_QQ) == 1/2);
assert(rootOfUnityOrder(-1_QQ) == 2 and rootOfUnityOrder(3_QQ) === null);

-- cyclotomic fields: every root of unity, and some elements that are not
for n from 1 to 20 do (
    F := cyclotomicField n;
    w := if even n then n else 2*n;
    for k from -2 to 2*w do (
        q := k/w;
        x := cyclotomicRoot(F, q);
        e := rootOfUnityExponent x;
        assert(instance(e, QQ));
        assert(e == fracPart q and 0 <= e and e < 1);
        assert(cyclotomicRoot(F, e) == x);
        o := rootOfUnityOrder x;
        assert(instance(o, ZZ) and o == denominator q);
        assert(bruteOrder(x, 100) == o);
        -- 2x and x + 3 are not roots of unity
        assert(rootOfUnityExponent(2*x) === null and rootOfUnityOrder(2*x) === null);
        assert(rootOfUnityOrder(x + 3) === null and rootOfUnityExponent(x + 3) === null);
        );
    assert(rootOfUnityExponent(0_F) === null and rootOfUnityOrder(0_F) === null);
    assert(rootOfUnityExponent(1_F) == 0 and rootOfUnityOrder(1_F) == 1);
    );
-- elements that are not roots of unity although all their conjugates have
-- absolute value 1, or that are close to roots of unity
F4 = cyclotomicField 4; i = F4_0;
assert(rootOfUnityOrder((3 + 4*i)/5) === null);
assert(rootOfUnityExponent((3 + 4*i)/5) === null);
assert(rootOfUnityOrder(1 + i) === null);
assert(rootOfUnityOrder(i) == 4 and rootOfUnityExponent(-i) == 3/4);
F3 = cyclotomicField 3; z = F3_0;
assert(rootOfUnityOrder(1 + z) == 6 and rootOfUnityExponent(1 + z) == 1/6);
F5 = cyclotomicField 5; z = F5_0;
assert(rootOfUnityOrder(1 + z) === null);
assert(rootOfUnityOrder(z + z^4) === null);
assert(rootOfUnityOrder(-z) == 10 and rootOfUnityExponent(-z) == 7/10);
F12 = cyclotomicField 12; z = F12_0;
assert(rootOfUnityOrder((z + z^5)) == 4);
assert(rootOfUnityOrder(z^2 - 1) == 3);
assert(rootOfUnityOrder(z^3 * (1 + z^4)) == 12 and rootOfUnityExponent(z^3 * (1 + z^4)) == 5/12);
assert(rootOfUnityOrder(z^3 + z^4) === null);

-- number fields QQ[a]/(f) made with toField, not by cyclotomicField
K = toField(QQ[a]/(a^2+1));
assert(rootOfUnityOrder a == 4 and rootOfUnityOrder(-a) == 4);
assert(rootOfUnityOrder(a^2) == 2 and rootOfUnityOrder(1_K) == 1);
assert(rootOfUnityOrder(1 + a) === null);
assert(rootOfUnityOrder((3 + 4*a)/5) === null);
assert(rootOfUnityOrder(0_K) === null);
K = toField(QQ[b]/(b^2+3));
assert(rootOfUnityOrder((1 + b)/2) == 6 and rootOfUnityOrder((-1 + b)/2) == 3);
assert(rootOfUnityOrder b === null);
K = toField(QQ[c]/(c^4+1));
assert(rootOfUnityOrder c == 8 and rootOfUnityOrder(c^2) == 4);
assert(rootOfUnityOrder(c + c^3) === null);
K = toField(QQ[d]/(d^2-2));
assert(rootOfUnityOrder d === null and rootOfUnityOrder(-1_K) == 2);
assert(rootOfUnityOrder(d - 1) === null and rootOfUnityOrder(d^2 - 1) == 1);
K = toField(QQ[e]/(e^3-2));
assert(rootOfUnityOrder e === null and rootOfUnityOrder(-1_K) == 2);
K = toField(QQ[f]/(f^4-f^2+1));
assert(rootOfUnityOrder f == 12 and rootOfUnityOrder(-f) == 12 and rootOfUnityOrder(f^3) == 4);
K = toField(QQ[g]/(g^6+g^3+1));
assert(rootOfUnityOrder g == 9 and rootOfUnityOrder(-g) == 18);
K = toField(QQ[h]/(h^4+h^3+h^2+h+1));
assert(rootOfUnityOrder h == 5 and rootOfUnityOrder(-h) == 10);
assert(rootOfUnityOrder(h + 1) === null);
-- the golden ratio is a unit but not a root of unity
K = toField(QQ[j]/(j^2-j-1));
assert(rootOfUnityOrder j === null and rootOfUnityOrder(-j) === null);
-- degree 8 field Q(zeta_24) given by another polynomial (zeta_24 = a root of Phi_24)
K = toField(QQ[m]/(m^8-m^4+1));
assert(rootOfUnityOrder m == 24 and rootOfUnityOrder(-m) == 24 and rootOfUnityOrder(m^8) == 3);

-- finite fields: every element, against brute force
fields = {ZZ/2, ZZ/3, ZZ/5, ZZ/7, ZZ/13, GF 4, GF 8, GF 9, GF 25, GF 27,
    GF(ZZ/3[c]/(c^2+1)), GF(ZZ/5[c]/(c^2-2))};
fieldSize = K -> if instance(K, GaloisField) then K.order else char K;
for K in fields do (
    w := fieldSize K - 1;
    g := rootOfUnityGenerator K;
    assert(rootOfUnityExponent(0_K) === null and rootOfUnityOrder(0_K) === null);
    for k from 0 to w-1 do (
        x := g^k;
        assert(rootOfUnityExponent x == k/w);
        assert(rootOfUnityOrder x == w // gcd(k, w));
        assert(rootOfUnityOrder x == bruteOrder(x, w + 1));
        ));
-- larger finite fields, a sample
for K in {ZZ/32003, GF(2^20), GF(3^10)} do (
    w := fieldSize K - 1;
    g := rootOfUnityGenerator K;
    for k in {0, 1, 2, w-1, w//2, w//3} | apply(20, i -> random w) do (
        x := g^k;
        assert(rootOfUnityExponent x == k/w);
        assert(rootOfUnityOrder x == w // gcd(k, w));
        assert(x^(rootOfUnityOrder x) == 1);
        ));
assert(rootOfUnityExponent(6_(ZZ/7)) == 1/2);
assert(rootOfUnityOrder(2_(ZZ/7)) == 3);
assert(rootOfUnityOrder(1_(ZZ/2)) == 1);
-- a field of degree 48
F = cyclotomicField 105; z = F_0;
for k in {0, 1, 2, 3, 5, 7, 15, 21, 35, 52, 104} do (
    assert(rootOfUnityOrder(z^k) == 105 // gcd(k, 105));
    assert(rootOfUnityOrder(-z^k) == 210 // gcd(105 + 2*k, 210));
    assert(rootOfUnityExponent(z^k) == k/105));
assert(rootOfUnityOrder(z + z^2) === null);
assert(rootOfUnityExponent(1 - z) === null);
