-- doc-auxiliary.m2
-- randomBinomialIdeal and extractInclusionMinimalIdeals (option Verbose).
-- randomBinomialIdeal: an ideal of R, unital binomial (documented: each
-- binomial is x^(m+) - x^(m-)), reproducible under setRandomSeed.  The number
-- of generators (documented: at most n) is tested in
-- doc-randomBinomialIdeal-count.m2.
-- extractInclusionMinimalIdeals: the result is a sublist of the input (as
-- ideals), every input ideal contains some output ideal (so only redundant
-- ideals are removed and the intersection is unchanged), and no output ideal
-- strictly contains another one.

-- ---------------------------------------------------------------------------
-- Oracle helpers (independent of the package; only Macaulay2 core is used).
-- Results may live in K[x] with K = QQ[ww]/(Phi) a cyclotomic field.  Since
-- K[x] = QQ[x,ww]/(Phi), ideals of K[x] correspond to ideals of QQ[x,ww]
-- containing Phi, and primality, primariness, associated primes, minimal
-- primes, intersections and containments are preserved under this
-- correspondence.  liftData S returns (phi, Phi) with phi: S -> QQ[x,ww]
-- sending a normal form to a representative; liftIdeal adds Phi back.
liftData = S -> (
    g := generators(S, CoefficientRing => QQ);
    T := QQ[Variables => #g];
    phi := map(T, S, gens T);
    rel := ideal(0_T);
    R0 := S;
    while R0 =!= QQ do (
        if instance(R0, QuotientRing) then (
            A0 := ambient R0;
            gA := generators(A0, CoefficientRing => QQ);
            rel = rel + (map(T, A0, take(gens T, -#gA))) ideal R0;
            R0 = A0)
        else R0 = coefficientRing R0);
    (phi, rel))
liftIdeal = (J, D) -> (D#0) J + D#1
-- equality of two lists of ideals as sets
setEq = (A, B) -> all(A, a -> any(B, b -> a == b)) and all(B, b -> any(A, a -> a == b))
-- cellular variables / nilpotent variables by definition
nzdVars = I -> select(gens ring I, v -> saturate(I, v) == I)
nilVars = I -> (r := radical I; select(gens ring I, v -> isSubset(ideal v, r)))
isCellularOracle = I -> (#nzdVars I + #nilVars I == numgens ring I)
-- ---------------------------------------------------------------------------

R = QQ[a..x];
for args in {(6,2,4,true), (3,4,10,false), (1,1,2,true), (5,3,3,false), (10,1,24,true)} do (
    (n, d, w, h) := args;
    setRandomSeed 17;
    I := randomBinomialIdeal(R, n, d, w, h);
    assert(instance(I, Ideal) and ring I === R);
    -- generators are x^a - x^b (checked on the generators; a Groebner basis in 24 variables is too slow)
    assert(all(flatten entries gens I, f -> #terms f == 2 and sum(terms f, leadCoefficient) == 0));
    assert(all(flatten entries gens I, f -> all(terms f, t -> abs leadCoefficient t == 1)));
    setRandomSeed 17;
    J := randomBinomialIdeal(R, n, d, w, h);
    assert(I == J));
S = QQ[x,y,z];
setRandomSeed 3;
for i from 1 to 10 do (
    I := randomBinomialIdeal(S, 3, 2, 2, false);
    assert(ring I === S and isBinomial I and isUnital I));

-- extractInclusionMinimalIdeals
checkEIM = (L, E) -> (
    assert(instance(E, List));
    assert(all(E, e -> any(L, l -> l == e)));
    assert(all(L, l -> any(E, e -> isSubset(e, l))));
    assert(all(E, e -> all(E, f -> e == f or not isSubset(e, f))));
    if #L > 0 then assert(intersect E == intersect L));
R = QQ[a,b];
L = {ideal(a^4), ideal(a^3), ideal(a^5), ideal(b^2*a)};
E = extractInclusionMinimalIdeals L;
checkEIM(L, E);
assert(setEq(E, {ideal(a^5), ideal(a*b^2)}));
L = {ideal(a), ideal(a^2), ideal(a^2, b)};
checkEIM(L, extractInclusionMinimalIdeals L);
assert(setEq(extractInclusionMinimalIdeals L, {ideal(a^2)}));
L = {ideal(a), ideal(b)};
assert(setEq(extractInclusionMinimalIdeals L, L));
assert(setEq(extractInclusionMinimalIdeals(L, Verbose => false), L));
L = {ideal(a), ideal(a)};
E = extractInclusionMinimalIdeals L;
checkEIM(L, E);
assert(extractInclusionMinimalIdeals {} == {});
L = {ideal(a*b), ideal(a), ideal(b), ideal(a^2*b, a*b^2), ideal(a-b), ideal(a^2-b^2), ideal(a,b)};
E = extractInclusionMinimalIdeals L;
checkEIM(L, E);
assert(setEq(E, {ideal(a^2*b, a*b^2), ideal(a^2-b^2)}));
-- ideals in different presentation that are equal
L = {ideal(a^2, a*b), ideal(a*b, a^2, a^3), ideal(a)};
checkEIM(L, extractInclusionMinimalIdeals L);
