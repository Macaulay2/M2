-- Internal functions for positive characteristic: finite fields and their
-- compatible generators, extension rings, roots of unity, the extensions of
-- partial characters to Sat'_p and Sat, and the p-saturation.
needsPackage "ConwayPolynomials";
needsPackage "Cyclotomic";
debug Cyclotomic;   -- leastPrimitiveRoot, isConwayField, multiplicativeOrder
debug Binomials;
needsPackage "ConwayPolynomials";

-- 1. least primitive roots and finite field data
assert(apply({2, 3, 5, 7, 11, 13, 17, 101}, leastPrimitiveRoot) == {1, 2, 2, 3, 2, 2, 3, 2});
assert(finiteFieldData QQ === null and finiteFieldData(frac(ZZ/3[t])) === null);
assert(finiteFieldData(ZZ/7) == (7, 1, 3_(ZZ/7)));
F9 = GF(3, 2);
(p, n, g) = finiteFieldData F9;
assert(p == 3 and n == 2 and ring g === F9 and g^8 == 1 and g^4 == -1 and g^2 != 1);
assert(isConwayField F9 and isConwayField GF 7 and isConwayField GF(2, 4));
assert(not isConwayField GF(ZZ/3[b]/(b^2 + 1)));
assert(apply({(2, 3), (2, 7), (3, 4), (5, 8), (3, 1)}, s -> multiplicativeOrder(s#0, s#1)) == {2, 3, 2, 2, 1});
assert(apply({(12, 2), (12, 3), (12, 5), (8, 2), (12, 0)}, s -> primeToPart(s#0, s#1)) == {3, 4, 12, 1, 12});

-- 2. roots of unity: e(q) has the right order, and e(1/2) = -1
scan({ZZ/5, ZZ/7, GF(3, 2), GF(2, 2), GF(5, 2)}, F -> (
        q := if instance(F, GaloisField) then F.order else char F;
        scan(select(toList(1..q-1), N -> (q - 1) % N == 0), N -> (
                w := rootValue(F, 1/N);
                assert(ring w === F and w^N == 1 and all(toList(1..N-1), k -> w^k != 1))));
        if char F != 2 then assert(rootValue(F, 1/2) == -1);
        assert(rootValue(F, 0) == 1)));
assert(try (rootValue(ZZ/5, 1/3); false) else true);
assert(rootExponent(1_(ZZ/5)) == 0 and rootExponent(-1_(ZZ/5)) == 1/2 and rootExponent(2_(ZZ/5)) == 1/4 and rootExponent(3_(ZZ/5)) == 3/4);
assert(rootExponent(-1_(ZZ/2)) == 0);
assert(rootExponent(1_(frac(ZZ/3[t]))) == 0 and rootExponent(t_(frac(ZZ/3[t]))) === null);

-- 3. extension rings: one ring per (R, degree), Conway compatible inclusion
R = ZZ/3[x,y];
(T, phi) = galoisRing(R, 2);
assert(galoisRing(R, 2) === (T, phi) and (coefficientRing T).order == 9);
assert(galoisRing(R, 1) === (R, identity));
assert(phi (R_0 - R_1) == T_0 - T_1);
assert((T4, phi4) := galoisRing(R, 4); (coefficientRing T4).order == 81);
-- the generators of GF(9) and GF(81) are compatible: gamma_81^10 is a root
-- of the Conway polynomial of GF(9)
g81 = (finiteFieldData coefficientRing T4)#2;
cp9 = conwayPolynomial(3, 2);
assert(sub(cp9, {(ring cp9)_0 => g81^10}) == 0);
-- the same for the prime field: gamma_9^4 is the least primitive root 2 = -1
g9 = (finiteFieldData coefficientRing T)#2;
assert(g9^4 == -1);
-- extensionRing by root order
assert(extensionRing(R, 1) === (R, identity) and extensionRing(R, 2) === (R, identity));
assert(first extensionRing(R, 4) === T and first extensionRing(R, 8) === T and first extensionRing(R, 5) === T4);
assert(first extensionRing(R, 16) === T4 and first extensionRing(R, 10) === T4);
assert(try (extensionRing(R, 3); false) else true);
-- over GF(9): the 4th roots are there, the 5th need GF(81)
S = (coefficientRing T)[x,y];
assert(extensionRing(S, 4) === (S, identity) and extensionRing(S, 8) === (S, identity));
(T', psi) = extensionRing(S, 5);
assert((coefficientRing T').order == 81 and psi (S_0 - (coefficientRing S)_0 * S_1) == T'_0 - ((coefficientRing T')_0)^10 * T'_1);
-- joinExtensions: ideals over GF(9) and GF(27) meet in GF(729)
R2 = ZZ/3[x];
J1 = first binomialMinimalPrimes ideal(x^4 - 1);
J2 = first binomialMinimalPrimes ideal(x^13 - 1);
assert((coefficientRing ring J1).order == 9 and (coefficientRing ring J2).order == 27);
joined = joinExtensions(R2, {J1, J2, ideal(x - 1)});
assert(all(joined, J -> (coefficientRing ring J).order == 3^6));
assert(all(joined, J -> J != 0 and J != 1));
-- in characteristic 2, rings over GF(4) and GF(8) meet in GF(64)
R4 = ZZ/2[x];
assert((coefficientRing ring first joinExtensions(R4, {first binomialMinimalPrimes ideal(x^3 - 1), first binomialMinimalPrimes ideal(x^7 - 1)})).order == 64);

-- 4. characterExtensions: g = prime-to-p part of the index extensions
countExtensions = (L, c) -> (
    (S, Sp, ext) := characterExtensions(L, c);
    p := char ring first c;
    d := latticeIndex L;
    g := primeToPart(d, p);
    assert(#ext == g and #unique apply(ext, last) == g and #unique apply(ext, first) == g);
    -- Sat'_p has index g over L, and Sat has index d
    assert(latticeIndex Sp == d // g);
    assert(numcols Sp == numcols L and isSubset(image L, image Sp) and isSubset(image Sp, image S));
    -- every extension restricts to c on L: check the values on S
    X := L // S;
    scan(ext, pair -> scan(numcols L, j -> (
                q := sum(#pair#1, i -> X_(i,j) * pair#1#i#1);
                assert(all(pair#1, v -> v#0 == 1));
                assert(q - rootExponent c#j == floor(q - rootExponent c#j)))));
    -- and the values on Sp are those of the extension to S, restricted
    Y := Sp // S;
    scan(ext, pair -> scan(numcols Sp, j -> (
                q := sum(#pair#1, i -> Y_(i,j) * pair#1#i#1);
                assert(q - pair#0#j#1 == floor(q - pair#0#j#1)))));
    #ext);
assert(countExtensions(matrix {{2}}, {1_(ZZ/2)}) == 1);
assert(countExtensions(matrix {{6}}, {1_(ZZ/2)}) == 3);
assert(countExtensions(matrix {{6}}, {1_(ZZ/3)}) == 2);
assert(countExtensions(matrix {{6}}, {-1_(ZZ/3)}) == 2);
assert(countExtensions(matrix {{6}}, {1_(ZZ/5)}) == 6);
assert(countExtensions(matrix {{12}, {-12}}, {-1_(ZZ/3)}) == 4);
assert(countExtensions(matrix {{2, 0}, {0, 3}}, {1_(ZZ/2), 1_(ZZ/2)}) == 3);
assert(countExtensions(matrix {{4, 2}, {0, 2}}, {1_(ZZ/2), -1_(ZZ/2)}) == 1);
assert(countExtensions(matrix {{4, 2}, {0, 2}}, {1_(ZZ/3), -1_(ZZ/3)}) == 8);
assert(countExtensions(matrix {{4, 2}, {0, 6}}, {1_(ZZ/3), -1_(ZZ/3)}) == 8);
assert(countExtensions(matrix {{1}, {-1}}, {1_(ZZ/2)}) == 1);
assert(countExtensions(matrix {{4}}, {2_(ZZ/5)}) == 4);                               -- 2 = e(1/4): roots in GF(5^4)
assert(countExtensions(matrix {{4}}, {(GF(3,2))_0}) == 4);                            -- gamma_9 = e(1/8): roots in GF(81)
assert(countExtensions(matrix {{15}}, {2_(ZZ/5)}) == 3);                              -- index 15 = 5 * 3
assert(try (characterExtensions(matrix {{4}}, {t_(frac(ZZ/3[t]))}); false) else true);   -- not a finite field
-- characterSaturations keeps its shape and agrees in characteristic zero
(S0, sats0) = characterSaturations(matrix {{2, 0}, {0, 3}}, {1_QQ, 1_QQ});
assert(#sats0 == 6 and all(sats0, v -> #v == 2 and all(v, pair -> #pair == 2)));
(S1, sats1) = characterSaturations(matrix {{2, 0}, {0, 3}}, {1_(ZZ/5), 1_(ZZ/5)});
assert(#sats1 == 6 and S1 == S0);

-- 5. pSaturation: Sat_p(L) has index the p-part of the index
checkP = (L, c) -> (
    (Lp, v) := pSaturation(L, c);
    p := char ring first c;
    d := latticeIndex L;
    assert(latticeIndex Lp == primeToPart(d, p));
    assert(isSubset(image L, image Lp));
    -- the character restricts to c: the values are 1 and -1, so compare powers
    X := L // Lp;
    scan(numcols L, j -> assert(product(numcols Lp, i -> (v#i)^(X_(i,j))) == c#j));
    Lp);
assert(image checkP(matrix {{2}}, {1_(ZZ/2)}) == image matrix {{1}});
assert(image checkP(matrix {{6}}, {1_(ZZ/2)}) == image matrix {{3}});
assert(image checkP(matrix {{6}}, {-1_(ZZ/3)}) == image matrix {{2}});
assert(image checkP(matrix {{12}, {-12}}, {-1_(ZZ/3)}) == image matrix {{4}, {-4}});
assert(image checkP(matrix {{4, 2}, {0, 2}}, {1_(ZZ/2), -1_(ZZ/2)}) == image matrix {{1, 0}, {0, 1}});
assert(image checkP(matrix {{4, 2}, {0, 6}}, {1_(ZZ/3), -1_(ZZ/3)}) == image matrix {{4, 2}, {0, 2}});
assert(image (pSaturation(matrix {{5}}, {1_(ZZ/2)}))#0 == image matrix {{5}});
assert(image checkP(matrix {{5}}, {2_(ZZ/5)}) == image matrix {{1}});              -- the 5th root of 2 is 2
assert((pSaturation(matrix {{5}}, {2_(ZZ/5)}))#1 == {2_(ZZ/5)});
assert(image checkP(matrix {{9}}, {F9_0}) == image matrix {{1}});                  -- the 9th root is the inverse Frobenius twice
assert(image checkP(matrix {{3}}, {F9_0}) == image matrix {{1}});
assert((pSaturation(matrix {{3}}, {F9_0}))#1 == {F9_0^3});
assert(image (pSaturation(matrix {{3}}, {2_(ZZ/5)}))#0 == image matrix {{3}});                  -- no p-part: nothing to take roots of
assert(try (pSaturation(matrix {{3}}, {t_(frac(ZZ/3[t]))}); false) else true);                 -- not perfect
