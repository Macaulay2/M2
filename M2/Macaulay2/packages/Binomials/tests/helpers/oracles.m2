-- Oracles for testing Binomials against Macaulay2's general algorithms
-- (radical, isPrime, isPrimary, associatedPrimes, minimalPrimes over QQ).
--
-- Binomials returns ideals in K[x], where K is QQ or a cyclotomic field.
-- The general algorithms want QQ, so we "descend": K[x]/J is the same ring
-- as QQ[w,x]/(J', Phi(w)), and J is prime (primary) iff (J', Phi) is.
-- Contracting to QQ[x] turns a prime over K into the prime over QQ below it.
--
-- All names start with orc.  Nothing here uses Binomials.

-- an ideal of a polynomial ring over QQ with the same quotient ring as J.
-- The ring over QQ is created once per ring of J, so that results for
-- several ideals of the same ring can be compared.
orcDescendCache = new MutableHashTable;
orcDescend = J -> (
    S := ring J;
    K := coefficientRing S;
    if K === QQ then return J;
    if not orcDescendCache#?S then (
        A := last K.baseRings;                 -- QQ[w]/Phi, before toField
        if not (coefficientRing A === QQ and numgens A == 1) then error "orcDescend: unexpected coefficient field";
        n := numgens S;
        T := QQ(monoid [Variables => n + 1]);  -- T_0 = w, T_1..T_n = x
        orcDescendCache#S = (
            map(T, S, (for i from 1 to n list T_i) | {T_0}),
            ideal sub((ideal A)_0, {(ambient A)_0 => T_0})));
    (phi, Phi) := orcDescendCache#S;
    phi J + Phi);

-- the contraction of J to the polynomial ring R over QQ (same number of variables)
orcContract = (J, R) -> (
    S := ring J;
    n := numgens R;
    if coefficientRing S === QQ then return (map(R, S, gens R)) J;
    J' := orcDescend J;
    T := ring J';
    (map(R, T, {0_R} | gens R)) eliminate({T_0}, J'));

-- extension of an ideal of R to the ring S (same variables, bigger field), by position
orcExtend = (I, S) -> if ring I === S then I else (map(S, ring I, gens S)) I;

orcIsPrime = J -> isPrime orcDescend J;
orcIsPrimary = J -> isPrimary orcDescend J;

-- equality of two lists of ideals of the same ring as sets
orcSameSets = (L1, L2) -> all(L1, I -> any(L2, J -> I == J)) and all(L2, J -> any(L1, I -> I == J));
orcDistinct = L -> all(#L, i -> all(i, j -> L#i != L#j));

-- cellular: proper, and every variable is a nonzerodivisor or nilpotent modulo C
orcIsCellular = C -> (
    one := ideal 1_(ring C);
    C != one and all(gens ring C, x -> (s := saturate(C, x); s == C or s == one)));
orcCellVars = C -> select(gens ring C, x -> saturate(C, x) == C);

-- all components live in one ring
orcCommonRing = L -> (S := ring first L; assert all(L, J -> ring J === S); S);

-- L is a cellular decomposition of I
orcCheckCellularDecomposition = (I, L) -> (
    if I == ideal 1_(ring I) then (assert(#L == 0); return true);
    assert(#L > 0);
    assert(orcCommonRing L === ring I);
    assert(intersect L == I);
    assert all(L, orcIsCellular);
    true);

-- L is a primary decomposition of I, possibly over a cyclotomic field, and
-- the radicals of its components are exactly the associated primes of I
orcCheckPrimaryDecomposition = (I, L) -> (
    R := ring I;
    if I == ideal 1_R then (assert(#L == 0); return true);
    assert(#L > 0);
    S := orcCommonRing L;
    assert(intersect L == orcExtend(I, S));
    assert all(L, orcIsPrimary);
    assert orcSameSets(apply(L, Q -> radical orcContract(Q, R)), associatedPrimes I);
    true);

-- L is the set of associated primes of I over a cyclotomic field
orcCheckAssociatedPrimes = (I, L) -> (
    R := ring I;
    if I == ideal 1_R then (assert(#L == 0); return true);
    assert(#L > 0);
    S := orcCommonRing L;
    assert all(L, orcIsPrime);
    assert orcDistinct L;
    ass := associatedPrimes I;
    below := apply(L, P -> orcContract(P, R));
    assert orcSameSets(below, ass);
    -- all conjugates are present: the primes over p intersect to the extension of p
    scan(ass, p -> assert(intersect L_(positions(below, q -> q == p)) == orcExtend(p, S)));
    true);

-- L is the set of minimal primes of I over a cyclotomic field
orcCheckMinimalPrimes = (I, L) -> (
    R := ring I;
    if I == ideal 1_R then (assert(#L == 0); return true);
    assert(#L > 0);
    S := orcCommonRing L;
    assert all(L, orcIsPrime);
    assert orcDistinct L;
    assert all(#L, i -> all(#L, j -> i == j or not isSubset(L#i, L#j)));
    assert(intersect L == orcExtend(radical I, S));
    assert orcSameSets(apply(L, P -> orcContract(P, R)), minimalPrimes I);
    true);

-- L is a decomposition of I into unmixed ideals (all associated primes of a
-- component have the same dimension)
orcCheckUnmixedDecomposition = (I, L) -> (
    if I == ideal 1_(ring I) then (assert(#L == 0); return true);
    assert(#L > 0);
    S := orcCommonRing L;
    assert(intersect L == orcExtend(I, S));
    assert all(L, C -> #unique apply(associatedPrimes orcDescend C, dim) == 1);
    true);

-- sols is the list of zeros of the zero-dimensional ideal I, with multiplicity
orcCheckSolve = (I, sols) -> (
    R := ring I;
    assert(dim I == 0);
    assert(#sols == degree I);
    assert all(sols, s -> #s == numgens R);
    K := if #sols == 0 then QQ else ring first first sols;
    assert all(sols, s -> all(s, c -> ring c === K));
    S := K(monoid R);
    G := (orcExtend(I, S))_*;
    assert all(sols, s -> all(G, g -> sub(g, apply(numgens S, i -> S_i => s#i)) == 0));
    assert(#unique apply(sols, s -> apply(s, toString)) == degree radical I);
    true);

-- A random binomial ideal of R with n generators.  Each monomial involves at
-- most w variables with exponents between 1 and d.  The second term of a
-- binomial is 1 with probability 1/8.  Coefficients are drawn from the list
-- cs; with probability pm/10 a generator is a monomial.
orcRandomBinomialIdeal = (R, n, d, w, cs, pm) -> (
    v := numgens R;
    randomMonomial := () -> (
        supp := take(shuffle toList(0..v-1), random(1, min(w, v)));
        product(supp, i -> R_i^(random(1, d))));
    ideal for i from 1 to n list (
        m1 := randomMonomial();
        if random 10 < pm then m1
        else m1 - cs#(random(#cs)) * (if random 8 == 0 then 1_R else randomMonomial())));
