-- doc-prime-primary-tests.m2
-- binomialIsPrime (option CellVariables), binomialIsPrimary,
-- cellularBinomialIsPrimary (options CellVariables, ReturnPrimes, ReturnPChars).
-- Oracles: isPrime / isPrimary from the core, on unital binomial ideals (for
-- these primality and primariness do not depend on extending QQ, so the QQ
-- answer is the oracle).  For ReturnPrimes: two DISTINCT associated primes
-- (checked against associatedPrimes after lifting from the cyclotomic field);
-- for ReturnPChars: two distinct partial characters whose lattice ideals plus
-- the non-cellular variables are associated primes.
-- Further cases of ReturnPrimes/ReturnPChars: doc-returnprimes-*.m2 and
-- doc-returnpchars-type.m2.

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

R = QQ[x,y,z];
ids = {ideal(x^2-y, y^2-x), ideal(x-y, z^3), ideal(x^3-y^2), ideal(x*y-z^2), ideal(x*y-z, x*z-y^2),
       ideal(x^2, y), ideal(x^2-y^2, x^3), ideal(x^6-y^6), ideal(x^6-y^5), ideal(x*y-z^2, x^2-y*z),
       ideal(x^2, y^2), ideal(x^2, y^2, z-1), ideal(x^3-y^3), ideal(x^2, x*(y-1)), ideal(x^2, x*y),
       ideal(x-y, y-z), ideal(x^2-1), ideal(x*y-1), ideal(x*y*z-1, x-y), ideal(x^3*y-z^4, x*z-y^2),
       ideal(x,y,z), ideal(0_R), ideal(2*x-3*y), ideal(x-y+z, z), ideal(x^2-y^2, x^2+y^2-2*z^2),
       ideal(x^4-1, y-x^2), ideal(x^3-z^2, y^2-z^3, x*y-z^2), ideal(y^2, y*(x^3-1)), ideal(x^2*y, z^2)};
for I in ids do (
    assert(binomialIsPrime I === isPrime I);
    assert(binomialIsPrimary I === isPrimary I);
    if isCellularOracle I then (
        cv := isCellular(I, ReturnCellVars => true);
        assert(binomialIsPrime(I, CellVariables => cv) === isPrime I);
        assert(cellularBinomialIsPrimary I === isPrimary I);
        assert(cellularBinomialIsPrimary(I, CellVariables => cv) === isPrimary I)));
-- documented examples
R = QQ[x,y];
assert(binomialIsPrime ideal(x^2-y, y^2-x) === false);
R = QQ[x,y,z];
assert(binomialIsPrimary ideal(x-y, z^3) === true);
-- primes needing nothing: toric ideals
assert(binomialIsPrime ideal(x*z-y^2) === true);
-- primary but not prime
assert(binomialIsPrime ideal(x^2, y-z) === false);
assert(binomialIsPrimary ideal(x^2, y-z) === true);

-- ReturnPrimes / ReturnPChars on non-primary cellular ideals whose variables are all cellular
pcData = p -> if instance(p, HashTable) then (p#"J", p#"L", p#"c") else (p#0, p#1, p#2);
-- ideal of the prime given by a partial character (+ the non-cellular variables), in K[x]
charPrime = (R, p) -> (
    (J, L, c) := pcData p;
    K := ring sum(c, v -> v);
    S := if K === ZZ or K === QQ then R else K(monoid R);
    jj := apply(J, v -> S_(index v));
    nc := select(gens S, v -> not member(index v, apply(J, index)));
    ideal(nc) + ideal apply(numcols L, i -> (
        e := flatten entries L_{i};
        product(#jj, k -> (jj#k)^(max(e#k,0))) - (sub(c#i, S)) * product(#jj, k -> (jj#k)^(max(-e#k,0))))));
isAssociated = (P, I) -> (
    S := ring P; D := liftData S;
    lP := liftIdeal(P, D);
    any(associatedPrimes liftIdeal(sub(I, S), D), Q -> Q == lP));
for I in {ideal(x^2-1), ideal(x^3-1), ideal(x^4-y^4), ideal(x^6-1, y-x^2), ideal(x^2-y^2, z^2-1), ideal(x^5-1, y^2-1)} do (
    assert(isCellularOracle I and not isPrimary I);
    assert(cellularBinomialIsPrimary I === false);
    for pr in {cellularBinomialIsPrimary(I, ReturnPrimes => true),
               cellularBinomialIsPrimary(I, ReturnPrimes => true, CellVariables => nzdVars I)} do (
        assert(instance(pr, List) and #pr == 2);
        assert(ring pr#0 === ring pr#1);
        assert(pr#0 != pr#1);
        assert(all(pr, P -> isAssociated(P, I))));
    pcs := cellularBinomialIsPrimary(I, ReturnPChars => true);
    assert(instance(pcs, List) and #pcs == 2);
    P0 := charPrime(R, pcs#0); P1 := charPrime(R, pcs#1);
    assert(isAssociated(P0, I) and isAssociated(P1, I));
    S := ring P1;
    assert(sub(P0, S) != P1);
    -- both options together: partial characters are returned
    both := cellularBinomialIsPrimary(I, ReturnPrimes => true, ReturnPChars => true);
    assert(#both == 2 and all(both, p -> not instance(p, Ideal))));
