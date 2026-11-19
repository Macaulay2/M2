-- Oracles for testing Binomials in positive characteristic against
-- Macaulay2's general algorithms.
--
-- The input lives in R = K[x] with K = ZZ/p or a Galois field GF(p^n) made
-- by GF; Binomials returns ideals in T = GF(p^m)[x].  Macaulay2's
-- associatedPrimes, minimalPrimes, primaryDecomposition and radical work
-- over K, not over the algebraic closure, so the results are compared by
-- Galois descent: the primes of T lying over a prime q of K[x] are the
-- Frobenius orbit of one of them, and their intersection is q T.  The
-- intersection of a Frobenius orbit has coefficients in K, so it can be
-- moved to R term by term.
--
-- Absolute primality (over the algebraic closure) is checked independently
-- of the package: a binomial ideal P is absolutely prime if and only if it
-- is cellular, contains its nilpotent variables and the lattice of its
-- cell part is saturated ([ES96] Corollary 2.6); the lattice is saturated
-- if and only if the greatest common divisor of the maximal minors of a
-- generating matrix is 1.
--
-- All names start with pch.  Nothing here uses Binomials.

pchDegree = K -> if instance(K, GaloisField) then K.degree else 1;

-- The inclusion of the coefficient field of R into that of T.  For fields
-- made by GF(p, n) it is Macaulay2's map (the Conway rule).  A Galois field
-- given by another polynomial has no canonical inclusion: the image of its
-- generator must be registered in pchGeneratorImage under the key (K, F)
-- by the test (it is a root of the minimal polynomial of the generator; the
-- test checks that, and uses the root the package chose).
pchGeneratorImage = new MutableHashTable;
pchFieldMap = (R, T) -> (
    K := coefficientRing R;
    F := coefficientRing T;
    if not instance(K, GaloisField) then map(F, K, {})
    else if pchGeneratorImage#?(K, F) then (
        r := pchGeneratorImage#(K, F);
        f := (ideal ambient K)_0;
        assert(sub(f, {(ring f)_0 => r}) == 0);     -- a root of the minimal polynomial: a field embedding
        map(F, K, {r}))
    else map(F, K));

-- extension of an ideal of R to T (same variables, bigger field)
pchExtend = (I, T) -> (
    R := ring I;
    if R === T then return I;
    K := coefficientRing R;
    phi := if instance(K, GaloisField) then map(T, R, gens T | {(pchFieldMap(R, T)) K_0}) else map(T, R, gens T);
    phi I);

-- the intersection of the Frobenius orbit of J (over the coefficient field
-- of R), as an ideal of R: the contraction of J to R
pchContract = (J, R) -> (
    T := ring J;
    if T === R then return J;
    K := coefficientRing R;
    F := coefficientRing T;
    p := char K;
    n := pchDegree K;
    m := F.degree;
    frob := map(T, T, gens T | {F_0^(p^n)});
    Jd := J; Js := J;
    for i from 1 to m // n - 1 do (Js = frob Js; Jd = intersect(Jd, Js));
    toK := if instance(K, GaloisField) then (
        psi := pchFieldMap(R, T);
        -- all elements of K as polynomials in its generator (which need not
        -- be primitive if K was not made by GF(p, n))
        elements := {0_K};
        scan(n, i -> elements = flatten apply(elements, e -> apply(p, a -> e + a * K_0^i)));
        table := hashTable apply(elements, e -> psi e => e);
        c -> (if not table#?c then error "pchContract: a coefficient is not in the small field"; table#c))
    else (c -> lift(c, K));
    ideal apply(flatten entries gens gb Jd, f -> sum(listForm f, t -> (
                (e, c) := t;
                toK(c) * product(#e, i -> R_i^(e#i))))));

pchSameSets = (L1, L2) -> all(L1, I -> any(L2, J -> I == J)) and all(L2, J -> any(L1, I -> I == J));
pchDistinct = L -> all(#L, i -> all(i, j -> L#i != L#j));
pchCommonRing = L -> (S := ring first L; assert all(L, J -> ring J === S); S);

-- the lattice of exponent differences of the binomials generating J, as a
-- matrix with one column per binomial, restricted to the variables idx
pchLatticeOf = (J, idx) -> (
    cols := for g in flatten entries gens gb J list (
        lf := listForm g;
        if #lf != 2 then continue;
        (lf#0#0 - lf#1#0)_idx);
    if #cols == 0 then map(ZZ^(#idx), ZZ^0, 0) else transpose matrix cols);

-- (the gcd of the maximal minors is the product of the elementary divisors;
-- minors(r, M) itself is far too slow for 14 x 30 matrices)
pchIsSaturatedLattice = M -> (
    if numcols M == 0 then return true;
    D := first smithNormalForm M;
    all(min(numrows D, numcols D), i -> D_(i,i) == 0 or abs D_(i,i) == 1));

-- P is prime over the algebraic closure.  By [ES96] Corollaries 2.3 and
-- 2.6 this holds if and only if P = (variables in P) + Pe, where Pe is a
-- binomial ideal in the other variables without monomials, saturated with
-- respect to the product of these variables, and the lattice of its
-- exponent differences is saturated.  The reduced Groebner basis of such a
-- P consists of the variables in P and binomials in the other variables.
pchIsAbsolutelyPrime = P -> (
    T := ring P;
    if P == 1 then return false;
    G := flatten entries gens gb P;
    nil := select(numgens T, i -> member(T_i, G));
    cells := select(numgens T, i -> not member(i, nil));
    binomials := select(G, g -> not member(g, T_* _ nil));
    if not all(binomials, g -> #terms g == 2 and all(nil, i -> degree(T_i, g) == 0)) then return false;
    Pe := if #binomials == 0 then ideal 0_T else ideal binomials;
    -- saturated by the cell variables iff the quotient by each of them is
    -- Pe again (Macaulay2's saturate by the product of 12 variables took
    -- more than a minute on a prime of the birth-death ideal)
    if #binomials > 0 and not all(cells, i -> quotient(Pe, T_i) == Pe) then return false;
    pchIsSaturatedLattice pchLatticeOf(Pe, cells));

-- L is a list of absolutely prime ideals of one extension ring of R, without
-- repetitions, whose contractions are exactly the primes in ass (a list of
-- primes of R), and every Frobenius orbit is complete
pchCheckPrimes = (R, L, ass) -> (
    assert(#L > 0);
    T := pchCommonRing L;
    assert pchDistinct L;
    assert all(L, pchIsAbsolutelyPrime);
    below := apply(L, P -> pchContract(P, R));
    assert pchSameSets(below, ass);
    scan(ass, q -> assert(intersect L_(positions(below, b -> b == q)) == pchExtend(q, T)));
    true);

pchCheckAssociatedPrimes = (I, L) -> (
    if I == 1 then (assert(#L == 0); return true);
    pchCheckPrimes(ring I, L, associatedPrimes I));

pchCheckMinimalPrimes = (I, L) -> (
    if I == 1 then (assert(#L == 0); return true);
    assert all(#L, i -> all(#L, j -> i == j or not isSubset(L#i, L#j)));
    assert(intersect L == pchExtend(radical I, pchCommonRing L));
    pchCheckPrimes(ring I, L, minimalPrimes I));

-- L is a primary decomposition of I over the algebraic closure: the
-- components intersect to I, each is primary over T with an absolutely prime
-- radical, and the radicals descend to the associated primes of I over K
pchCheckPrimaryDecomposition = (I, L) -> (
    R := ring I;
    if I == 1 then (assert(#L == 0); return true);
    assert(#L > 0);
    T := pchCommonRing L;
    assert(intersect L == pchExtend(I, T));
    rads := apply(L, Q -> (
            ass := associatedPrimes Q;
            assert(#ass == 1);
            first ass));
    assert all(rads, pchIsAbsolutelyPrime);
    assert pchDistinct rads;
    pchCheckPrimes(R, rads, associatedPrimes I));

pchCheckRadical = (I, r) -> (
    assert(ring r === ring I);
    assert(r == radical I);
    true);

pchCheckUnmixedDecomposition = (I, L) -> (
    if I == 1 then (assert(#L == 0); return true);
    assert(pchCommonRing L === ring I);
    assert(intersect L == I);
    assert all(L, C -> #unique apply(associatedPrimes C, dim) == 1);
    true);

-- I is primary over the algebraic closure
pchIsAbsolutelyPrimary = I -> (
    if I == 1 then return false;
    ass := associatedPrimes I;
    #ass == 1 and pchIsAbsolutelyPrime first ass);

-- sols is the list of zeros of the zero-dimensional ideal I with multiplicity
pchCheckSolve = (I, sols) -> (
    R := ring I;
    assert(dim I == 0);
    assert(#sols == degree I);
    assert all(sols, s -> #s == numgens R);
    F := ring first first sols;
    assert all(sols, s -> all(s, c -> ring c === F));
    T := F(monoid R);
    G := (pchExtend(I, T))_*;
    assert all(sols, s -> all(G, g -> sub(g, apply(numgens T, i -> T_i => s#i)) == 0));
    assert(#unique apply(sols, s -> apply(s, toString)) == degree radical I);
    true);

-- all checks on one ideal; used by the random tests
pchCheckAll = I -> (
    assert pchCheckPrimaryDecomposition(I, binomialPrimaryDecomposition I);
    assert pchCheckAssociatedPrimes(I, binomialAssociatedPrimes I);
    assert pchCheckMinimalPrimes(I, binomialMinimalPrimes I);
    assert pchCheckRadical(I, binomialRadical I);
    assert pchCheckUnmixedDecomposition(I, binomialUnmixedDecomposition I);
    assert(binomialIsPrimary I === pchIsAbsolutelyPrimary I);
    assert(binomialIsPrime I === (I != 1 and radical I == I and pchIsAbsolutelyPrime I));
    if isCellular I then (
        assert pchCheckRadical(I, cellularBinomialRadical I);
        assert pchCheckAssociatedPrimes(I, cellularBinomialAssociatedPrimes I);
        assert pchCheckPrimaryDecomposition(I, cellularBinomialPrimaryDecomposition I));
    -- in positive characteristic binomialSolve takes every binomial ideal
    if dim I == 0 and (isUnital I or char ring I > 0) then assert pchCheckSolve(I, binomialSolve I);
    true);
