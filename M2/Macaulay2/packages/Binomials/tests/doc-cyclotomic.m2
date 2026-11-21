-- doc-cyclotomic.m2
-- Ideals that need roots of unity of order 3, 4, 5, 6, 8, 12 (the package
-- extends QQ by a cyclotomic field via the package Cyclotomic).
-- For x^n - 1 (one variable) and x^n - y^n (two variables), over the
-- extension there are exactly n minimal primes, all linear binomial ideals,
-- and the ideal is radical, so the primary decomposition consists of exactly
-- those n primes.  Checked for binomialPrimaryDecomposition,
-- binomialMinimalPrimes, binomialAssociatedPrimes,
-- cellularBinomialPrimaryDecomposition, cellularBinomialAssociatedPrimes and
-- binomialSolve, with lifting oracles (intersection, primality via isPrime
-- over QQ[x,ww]/(Phi)).

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

checkLinearSplit = (L, I, n) -> (
    -- L: list of ideals over an extension; must be n distinct primes whose intersection is I
    assert(#L == n);
    S := ring first L;
    D := liftData S;
    lL := apply(L, l -> liftIdeal(l, D));
    assert(#unique lL == n);
    assert(all(lL, isPrime));
    assert(intersect lL == liftIdeal(sub(I, S), D));
    assert(all(L, isBinomial)));

for n in {3,4,5,6,8,12} do (
    R := QQ[x];
    I := ideal(R_0^n - 1);
    checkLinearSplit(binomialPrimaryDecomposition I, I, n);
    checkLinearSplit(binomialMinimalPrimes I, I, n);
    checkLinearSplit(binomialAssociatedPrimes I, I, n);
    checkLinearSplit(cellularBinomialPrimaryDecomposition I, I, n);
    checkLinearSplit(cellularBinomialAssociatedPrimes I, I, n);
    sols := binomialSolve I;
    assert(#unique sols == n);
    assert(all(sols, p -> (p#0)^n == 1));
    -- primitive root present: some solution has multiplicative order exactly n
    assert(any(sols, p -> all(toList(1..n-1), k -> (p#0)^k != 1)));
    );
for n in {3,4,5,6,8,12} do (
    R := QQ[x,y];
    I := ideal(R_0^n - R_1^n);
    checkLinearSplit(binomialPrimaryDecomposition I, I, n);
    checkLinearSplit(binomialMinimalPrimes I, I, n);
    );
-- mixed orders: x^4 = 1, y^6 = 1 gives 24 points, needs zeta_12
R = QQ[x,y];
I = ideal(x^4-1, y^6-1);
checkLinearSplit(binomialPrimaryDecomposition I, I, 24);
assert(#unique binomialSolve I == 24);
-- non-radical with roots of unity: (x^3-1)-torsion with a nilpotent, minimal primes still 3
I = ideal(x^3-1, y^2);
M = binomialMinimalPrimes I;
assert(#M == 3);
P = binomialPrimaryDecomposition I;
S = ring first P; D = liftData S; use R;
assert(intersect apply(P, p -> liftIdeal(p, D)) == liftIdeal(sub(I, S), D));
assert(all(P, p -> isPrimary liftIdeal(p, D)));
