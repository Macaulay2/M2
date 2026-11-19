-- Internal functions for coefficients in finite fields: discrete logarithms
-- (Pohlig-Hellman with baby-step giant-step) against brute force, the
-- Chinese remainder theorem, p-power roots by the inverse Frobenius, the
-- generator of a Galois field not given by its Conway polynomial, and the
-- inclusion of such a field into the Conway field.
needsPackage "ConwayPolynomials";
needsPackage "Cyclotomic";
debug Cyclotomic;   -- discreteLogData, chineseRemainder, isConwayField
debug Binomials;
needsPackage "ConwayPolynomials";

-- 1. discrete logarithms of every element of small fields, against the powers of the generator
scan({ZZ/2, ZZ/3, ZZ/5, ZZ/7, ZZ/13, ZZ/101, GF(2, 2), GF(3, 2), GF(5, 2), GF(2, 6), GF(3, 4), GF 7, GF(ZZ/3[c]/(c^2 + 1)), GF(ZZ/2[c]/(c^4 + c^3 + 1))}, F -> (
        (p, n, gamma) := finiteFieldData F;
        N := p^n - 1;
        x := 1_F;
        for k from 0 to N - 1 do (
            assert(discreteLog x == k);
            assert(rootExponent x == k / N);
            assert(rootValue(F, k / N) == x);
            x = x * gamma);
        assert(x == 1)));
-- the tables: one per prime factor of the group order, of size ceiling of the square root
data = discreteLogData(ZZ/101);                                           -- 100 = 2^2 5^2
assert(apply(data, part -> (part#0, part#1, part#4)) == {(2, 2, 2), (5, 2, 3)});
assert all(data, part -> #keys part#5 == part#4 and part#6 * (part#3)^(part#4) == 1 and (part#3)^(part#0) == 1 and (part#2)^((part#0)^(part#1)) == 1);
assert(apply(discreteLogData GF(2, 6), part -> (part#0, part#1, part#4)) == {(3, 2, 2), (7, 1, 3)});   -- 63 = 3^2 7
assert(#discreteLogData(ZZ/2) == 0);
-- 2. the Chinese remainder theorem
assert(chineseRemainder {(2, 3), (3, 5), (2, 7)} == 23);
assert(chineseRemainder {} == 0 and chineseRemainder {(5, 7)} == 5 and chineseRemainder {(0, 4), (0, 9)} == 0);
assert(chineseRemainder {(3, 4), (8, 9), (24, 25)} == 899);
-- 3. large fields: round trips for random elements (seeded)
scan({ZZ/32003, ZZ/1073741789, ZZ/(2^31 - 1), ZZ/(2^61 - 1), GF(2, 20), GF(32003, 2), GF(3, 12)}, F -> (
        (p, n, gamma) := finiteFieldData F;
        setRandomSeed 1;
        scan(20, i -> (c := random F; if c != 0 then assert(gamma^(discreteLog c) == c)))));
-- 0 has no logarithm, fields that are not finite have none, and a group order
-- with a prime factor beyond 2^38 is refused with an error
assert(try (discreteLog 0_(ZZ/5); false) else true);
assert(try (discreteLog t_(frac(ZZ/3[t])); false) else true);
q = nextPrime(2^39);
while not isPrime(2*q + 1) do q = nextPrime(q + 1);
assert(try (discreteLog 3_(ZZ/(2*q + 1)); false) else true);
-- 4. p-power roots by the inverse Frobenius
scan({ZZ/5, GF(3, 2), GF(2, 4), GF(ZZ/5[c]/(c^2 - 2))}, F -> (
        (p, n, gamma) := finiteFieldData F;
        scan({1, p, p^2, p^3}, pa -> (
                x := 1_F;
                scan(p^n - 1, i -> (
                        r := pPowerRoot(x, pa);
                        assert(ring r === F and r^pa == x);
                        x = x * gamma))))));
assert(pPowerRoot(2_(ZZ/5), 5) == 2 and pPowerRoot(-1_(ZZ/7), 49) == -1);
assert(try (pPowerRoot(t_(frac(ZZ/3[t])), 3); false) else true);
assert(pPowerRoot(1_(frac(ZZ/3[t])), 3) == 1);                            -- 1 and -1 need no field data
-- 5. Galois fields not given by their Conway polynomial: the generator is a
-- root of the Conway polynomial, and the generator of the field has a logarithm
Kn = GF(ZZ/3[c]/(c^2 + 1));
(p, n, g) = finiteFieldData Kn;
cp = conwayPolynomial(3, 2);
assert(ring g === Kn and sub(cp, {(ring cp)_0 => g}) == 0 and g^4 == -1 and g^2 != 1);
assert(not isConwayField Kn and g^(discreteLog Kn_0) == Kn_0 and member(discreteLog Kn_0, {2, 6}));   -- c has order 4
assert(finiteFieldData Kn === finiteFieldData Kn);                        -- cached: the same root every time
K16 = GF(ZZ/2[c]/(c^4 + c^3 + 1));
(p, n, g) = finiteFieldData K16;
cp = conwayPolynomial(2, 4);
assert(ring g === K16 and sub(cp, {(ring cp)_0 => g}) == 0 and g^5 != 1 and g^3 != 1);
K25 = GF(ZZ/5[c]/(c^2 - 2));
(p, n, g) = finiteFieldData K25;
assert(sub(conwayPolynomial(5, 2), {(ring conwayPolynomial(5, 2))_0 => g}) == 0);
-- a field made by GF(p, n) with more than 10000 elements keeps F_0 as its generator
F = GF(101, 2);
assert((finiteFieldData F)#2 == F_0 and isConwayField F);
F = GF(32003, 2);
assert((finiteFieldData F)#2 == F_0 and isConwayField F);
-- 6. the inclusion of a non-Conway field into the Conway field GF(p^m): a ring
-- map that sends the generator to a root of its minimal polynomial and the
-- generator gamma to the Conway-compatible power
Y = Kn[x,y];
(p, n, g) = finiteFieldData Kn;
(T, phi) = galoisRing(Y, 4);
F81 = coefficientRing T;
assert(F81.order == 81 and isConwayField F81 and toString F81_0 == "ww_80");
r = lift(phi promote(Kn_0, Y), F81);
assert(r^2 + 1 == 0);
assert(lift(phi promote(g, Y), F81) == F81_0^10);
assert(phi (Kn_0 * Y_0^2 + g * Y_1 - 1) == r * T_0^2 + F81_0^10 * T_1 - 1);
assert(galoisRing(Y, 4) === (T, phi) and galoisRing(Y, 2) === (Y, identity));
assert(first extensionRing(Y, 5) === T and extensionRing(Y, 8) === (Y, identity));
-- joinExtensions moves ideals of Y and of T into T
J = joinExtensions(Y, {ideal(Y_0 - Kn_0 * Y_1), ideal(T_0 - F81_0 * T_1)});
assert(all(J, I -> ring I === T) and J#0 == ideal(T_0 - r * T_1));
