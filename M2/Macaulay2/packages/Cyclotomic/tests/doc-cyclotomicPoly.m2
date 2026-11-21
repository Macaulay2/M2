-- doc-cyclotomicPoly.m2 -- independent tests from the documentation alone.
-- cyclotomicPoly(n, v) is the n-th cyclotomic polynomial Phi_n(v), the
-- minimal polynomial of e(1/n), for a variable v of a polynomial ring.

totient = n -> #select(toList(1..n), k -> gcd(k, n) == 1);
divisorsOf = n -> select(toList(1..n), d -> n % d == 0);

R = QQ[w];
for n from 1 to 60 do (
    f := cyclotomicPoly(n, w);
    assert(ring f === R);
    assert(first degree f == totient n);
    assert(leadCoefficient f == 1);
    -- x^n - 1 = prod_{d | n} Phi_d
    assert(product(divisorsOf n, d -> cyclotomicPoly(d, w)) == w^n - 1);
    -- irreducible over QQ
    if n <= 30 then assert(#select(toList factor f, p -> first degree p#0 > 0) == 1);
    );
assert(cyclotomicPoly(1, w) == w - 1);
assert(cyclotomicPoly(2, w) == w + 1);
assert(cyclotomicPoly(3, w) == w^2 + w + 1);
assert(cyclotomicPoly(4, w) == w^2 + 1);
assert(cyclotomicPoly(6, w) == w^2 - w + 1);
assert(cyclotomicPoly(8, w) == w^4 + 1);
assert(cyclotomicPoly(12, w) == w^4 - w^2 + 1);
assert(cyclotomicPoly(7, w) == sum(7, i -> w^i));
assert(cyclotomicPoly(9, w) == w^6 + w^3 + 1);
assert(cyclotomicPoly(30, w) == w^8 + w^7 - w^5 - w^4 - w^3 + w + 1);

-- Phi_n is the minimal polynomial of e(1/n): it vanishes at the generator
-- of cyclotomicField n and at cyclotomicRoot(1/n)
evalAt = (f, z) -> sum(listForm f, (e, c) -> c * z^(e#0));
for n from 1 to 20 do (
    F := cyclotomicField n;
    assert(evalAt(cyclotomicPoly(n, w), F_0) == 0);
    assert(evalAt(cyclotomicPoly(n, w), cyclotomicRoot(1/n)) == 0);
    -- and at e(k/n) for k prime to n, but not at e(k/n) of smaller order
    for k from 0 to n-1 do (
        z := cyclotomicRoot(F, k/n);
        assert((evalAt(cyclotomicPoly(n, w), z) == 0) == (gcd(k, n) == 1)));
    );

-- other polynomial rings, and a variable that is not the first one
S = QQ[x, y, z];
assert(cyclotomicPoly(6, y) == y^2 - y + 1 and ring cyclotomicPoly(6, y) === S);
assert(cyclotomicPoly(5, z) == z^4 + z^3 + z^2 + z + 1);
A = ZZ[u];
assert(cyclotomicPoly(12, u) == u^4 - u^2 + 1 and ring cyclotomicPoly(12, u) === A);
-- other coefficient rings: see doc-FINDING-cyclotomicPoly-coefficients.m2
S2 = ZZ[a, b];
assert(cyclotomicPoly(8, b) == b^4 + 1 and ring cyclotomicPoly(8, b) === S2);
T = QQ[t, MonomialOrder => Lex];
assert(cyclotomicPoly(10, t) == t^4 - t^3 + t^2 - t + 1 and ring cyclotomicPoly(10, t) === T);
