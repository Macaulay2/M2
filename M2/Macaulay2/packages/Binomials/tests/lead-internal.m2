-- Tests of internal functions of the renewed package.  The central one
-- checks the congruence classes of cellular ideals against the definition:
-- for every monomial m in the nilpotent variables that is not in I, the
-- partial character of (I : m) must be the character of the class of m.
-- Version 1.2.1 computed (I : m) for each m; the renewed package gets all
-- characters from one pass over a graph.
debug Binomials;
needsPackage "LLLBases";
-- 1. congruence classes against colon ideals
checkClasses = (I, cellIdx) -> (
    R := ring I;
    K := coefficientRing R;
    cv := R_* _ cellIdx;
    classes := congruenceClasses(I, cellIdx);
    -- the classes partition the monomials that are not in I
    monomials := flatten apply(classes, cl -> cl#2);
    assert(#monomials == #unique monomials);
    assert(all(monomials, m -> m % I != 0));
    assert(first classes#0#2 == 1_R);
    scan(classes, cl -> (
            (H, values, mons, count) := cl;
            scan(mons, m -> (
                    pc := partialCharacter(I : m, CellVariables => cv);
                    if numcols H == 0 then assert(pc#"L" == 0)
                    else (
                        assert(image pc#"L" == image H);
                        X := pc#"L" // H;
                        assert(apply(numcols pc#"L", j -> product(numcols H, i -> (values#i)^(X_(i,j)))) == apply(pc#"c", c -> promote(c, K))))))));
    -- multiplying a monomial that is not in I by a nilpotent variable gives
    -- a monomial in I or a monomial of some class
    nilIdx := otherIndices(R, cellIdx);
    assert(all(monomials, m -> all(nilIdx, i -> (m * R_i) % I == 0 or member(m * R_i, monomials)))));
R = QQ[x,y,z,w];
scan({
        (ideal(x^2, x*y, y^2, x*(z^3 - 1), y*(z^2 - 1)), {2, 3}),
        (ideal(x^6, y^6, x^2*y^3*(w - 1), x^3*(w^2 - 1), y^2*(z*w - 1)), {2, 3}),
        (ideal(x^3 - y^3, z^2, z*(x - y), w^3), {0, 1}),
        (ideal(x^2 - 4*y^2, z^2, z*(x - 2*y)), {0, 1, 3}),
        (saturate(ideal(x*y - z*w, x^2 - z^2), x*y*z*w), {0, 1, 2, 3}),
        (ideal(x^2, y^2, z^2, w^2), {}),
        (ideal(x - y, y^3, z*w - 1), {2, 3})
        }, p -> checkClasses p);
-- the cellular components of a decomposition
I = ideal(x^3*y^2*w - z^2, x^5*y^2 - w^7, w^3 - z^8);
scan(binomialCellularDecomposition(I, ReturnCellVars => true), p -> checkClasses(p#0, apply(p#1, index)));

-- 2. saturating by a variable: the homogeneous method agrees with repeated quotients
scan({ideal(x^2*y - y^2*z, x*z^3 - w^4, x*y*z - w^3), ideal(x^3 - x*y^2, y*z - w^2), ideal(x*y, x*z)}, J -> (
        assert isTotalDegreeHomogeneous J;
        scan(4, i -> (
                (k, S) := saturateByVariable(J, i);
                assert(S == saturate(J, R_i));
                assert(J : R_i^k == S and (k == 0 or J : R_i^(k-1) != S))))));
J = ideal(x^3*y - x, y^2*z - 1);                        -- not homogeneous
scan(4, i -> ((k, S) := saturateByVariable(J, i); assert(S == saturate(J, R_i) and J : R_i^k == S)));
assert(syzygyQuotient(ideal(x^2*y, x*z), x) == ideal(x*y, z));
-- saturating by a product of variables: homogeneous ideals (one variable
-- after the other) and others (repeated quotients) agree with saturate, and
-- an ideal that is saturated already comes back as the same object
scan({ideal(x^2*y, x*z - x*w), ideal(x^2*y - y^2*z, x*z^3 - w^4, x*y*z - w^3), ideal(x^3 - x*y^2, y*z - w^2),
        ideal(x^3*y - x, y^2*z - 1), ideal(x*y - x, y^2*z^2 - z^2, w^3), ideal(x^2*y*(z - 1), x*w)}, J -> (
        scan({{0}, {0, 1}, {1, 2}, {0, 1, 2, 3}, {}}, idx -> (
                S := saturateByCells(J, idx);
                assert(S == saturate(J, product(idx, i -> R_i)));
                if S == J then assert(S === J)))));
-- the cell variables are found with and without the shortcuts
scan({ideal(x^2, x*y - x, z^3 - z), ideal(x^5, x^2*y - x^2, z*w), ideal(x*y - z*w), ideal(1_R), ideal(x^2 - y^2, x^3)}, J ->
    assert(cellIndices J == select(4, i -> saturate(J, R_i) != 1)));
-- a pure power of high degree: no saturation is computed for the variable
J = ideal(x^3000*(y - 1), x^3001);
assert(first timing(isCellular J) < 2 and isCellular J);
assert(binomialMinimalPrimes J == {ideal x});
assert(binomialRadical J == ideal x);
assert(#binomialCellularDecomposition J == 1 and first binomialCellularDecomposition J == J);

-- 3. lattices
assert(latticeIndex matrix {{2}, {-2}} == 2);
assert(latticeIndex matrix {{2, 0}, {0, 3}} == 6);
assert(latticeIndex map(ZZ^3, ZZ^0, 0) == 1);
assert(latticeIndex hermiteBasis matrix {{1, 0}, {0, 1}, {5, 7}} == 1);
assert(latticeIndex hermiteBasis matrix {{2, 0}, {0, 1}, {0, 0}} == 2);
assert(image latticeSaturation matrix {{2, 4}, {-2, 2}} == image id_(ZZ^2));
-- the Hermite basis is a canonical form
A = matrix {{2, 0}, {0, 3}, {1, 1}};
B = A * matrix {{1, 1}, {0, 1}} | A * matrix {{1}, {-1}};
assert(image A == image B and hermiteBasis A == hermiteBasis B);

-- 4. saturations of characters: every saturation extends the character,
--    they are distinct, and their number is the index
countSaturations = (L, c) -> (
    (S, sats) := characterSaturations(L, c);
    assert(#sats == latticeIndex L and #unique sats == #sats);
    X := L // S;
    scan(sats, vals -> scan(numcols L, j -> (
                -- value of the saturation on the j-th column of L, as a pair (a, q)
                a := product(#vals, i -> (vals#i#0)^(X_(i,j)));
                q := sum(#vals, i -> X_(i,j) * vals#i#1);
                assert(a * (if q == floor q then 1 else if 2*q == floor(2*q) then -1 else 0) == c#j or
                    (rootExponent c#j =!= null and a == 1 and (q - rootExponent c#j) == floor(q - rootExponent c#j))))));
    #sats);
assert(countSaturations(matrix {{3}}, {1_QQ}) == 3);
assert(countSaturations(matrix {{2}, {-2}}, {-1_QQ}) == 2);
assert(countSaturations(matrix {{2, 0}, {0, 3}}, {1_QQ, 1_QQ}) == 6);
assert(countSaturations(matrix {{4, 2}, {0, 2}}, {1_QQ, -1_QQ}) == 8);
assert(countSaturations(matrix {{1}, {-1}}, {2_QQ}) == 1);
assert(first first last characterSaturations(matrix {{2}}, {4_QQ}) == (2_QQ, 0/1));
assert(try (characterSaturations(matrix {{2}}, {2_QQ}); false) else true);

-- 5. lattice ideals: twisting a Markov basis gives the lattice ideal of a character
S = QQ[a,b,c];
L = matrix {{2, 0}, {-1, 3}, {0, -1}};
assert(latticeIdeal(S, {0, 1, 2}, L, {1_QQ, 1_QQ}) == saturate(ideal(a^2 - b, b^3 - c), a*b*c));
assert(latticeIdeal(S, {0, 1, 2}, L, {2_QQ, -1_QQ}) == saturate(ideal(a^2 - 2*b, b^3 + c), a*b*c));
L = matrix {{2, 1}, {-2, 1}, {0, -2}};                    -- not saturated, needs more than a basis
assert(latticeIdeal(S, {0, 1, 2}, L, {1_QQ, 1_QQ}) == saturate(ideal(a^2 - b^2, a*b - c^2), a*b*c));
assert(latticeIdeal(S, {0, 1, 2}, L, {3_QQ, 5_QQ}) == saturate(ideal(a^2 - 3*b^2, a*b - 5*c^2), a*b*c));
assert(latticeIdeal(S, {0, 2}, matrix {{1}, {-1}}, {7_QQ}) == ideal(a - 7*c));

-- 6. roots of unity
needsPackage "Cyclotomic";
F = cyclotomicField 12;
assert(cyclotomicOrder F == 12 and cyclotomicOrder QQ == 1 and cyclotomicOrder(ZZ/7) === null);
assert(cyclotomicOrder toField(QQ[t]/(t^2 - 2)) === null);
assert(apply({1_F, -1_F, F_0, F_0^5, -F_0}, rootExponent) == {0, 1/2, 1/12, 5/12, 7/12});
assert(rootExponent 2_F === null and rootExponent 2_QQ === null);
assert(all({0, 1/2, 1/12, 7/12, 1/3, 5/6}, q -> rootExponent rootValue(F, q) == q));
-- the fields of order 3 and 6 are the same; the odd order is used (B3 of the review)
assert(orderForExponents {1/2, 2/3, 0} == 3 and orderForExponents {} == 1);
assert(orderForExponents {1/2} == 1 and orderForExponents {1/4} == 4 and orderForExponents {1/10, 1/4} == 20);
assert(rationalRoot(8/27, 3) == 2/3 and rationalRoot(2, 2) === null and rationalRoot(0, 5) == 0);
