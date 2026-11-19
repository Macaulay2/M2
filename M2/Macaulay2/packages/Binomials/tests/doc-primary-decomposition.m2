-- doc-primary-decomposition.m2
-- binomialPrimaryDecomposition, its synonym BPD, option Verbose.
-- Documented: "a list of binomial primary components of I"; the coefficient
-- field may be extended to a cyclotomic field; the decomposition need not be
-- irredundant (Caveat).
-- Oracles (computed over QQ via K[x] = QQ[x,ww]/(Phi), see helpers):
--   * intersection of the components equals I extended to K[x],
--   * every component is primary (isPrimary) and binomial,
--   * every associated prime of I (associatedPrimes) is the radical of some
--     component (true for any, even redundant, primary decomposition).
-- Inputs: unital binomial ideals of many shapes.  Known counterexamples (non
-- primary output) are in doc-FINDING-bpd-not-primary.m2.

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

checkBPD = I -> (
    P := binomialPrimaryDecomposition I;
    assert(instance(P, List) and all(P, p -> instance(p, Ideal)));
    if I == 1 then (assert(P == {}); return);
    S := ring first P;
    assert(all(P, p -> ring p === S));
    assert(set apply(gens S, toString) === set apply(gens ring I, toString));
    D := liftData S;
    lI := liftIdeal(sub(I, S), D);
    lP := apply(P, p -> liftIdeal(p, D));
    assert(intersect lP == lI);
    assert(all(lP, isPrimary));
    assert(all(P, isBinomial));
    rads := apply(lP, radical);
    assert(all(associatedPrimes lI, Q -> any(rads, r -> r == Q)));
    assert(#BPD I == #P);
    );

R = QQ[x,y,z];
scan({ideal(x*y-z, x*z-y^2), ideal(x^2, x*y), ideal(x^2-y, y^2-x), ideal(x^3-y^2),
      ideal(x*y-z^2), ideal(x^2, y^2, z^2), ideal(x*(y-1)), ideal(x*(y^2-1)), ideal(x^2*(y-1)),
      ideal(y^2, y*(x-1)), ideal(y^2, y*(x^2-1)), ideal(z^2, y^2, y*z*(x^2-1)),
      ideal(y^3, y^2*z^2-x^3, x*y^2*z, x^3*z-x*y),
      ideal(x-y+z, z), ideal(x^2-y^2, x^2+y^2-2*z^2), ideal(x^4-y^4), ideal(x^2-y^2, z^3),
      ideal(x*y, y*z, x*z), ideal(x^2*y - z^3, x*z - y^2), ideal(0_R), ideal(1_R),
      ideal(x,y,z), ideal(x^2-1, y^2-1, z^2-1, x*y*z-1), ideal(x^3-1, y-x)}, checkBPD);

-- documented example: the two components are the twisted-cubic-like toric prime and (y,z)
P = BPD ideal(x*y-z, x*z-y^2);
assert(setEq(apply(P, p -> sub(p, R)), {ideal(y^2-x*z, x*y-z, x^2-y), ideal(y,z)}));
-- a prime binomial ideal is its own primary decomposition
P = BPD ideal(x*y-z^2);
assert(#P == 1 and sub(first P, R) == ideal(x*y-z^2));
-- primary ideals
for I in {ideal(x-y, z^3), ideal(x^2, y^2, z-1), ideal(x^3-y^2, z^2)} do (
    assert(isPrimary I);
    P := BPD I;
    assert(intersect apply(P, p -> sub(p, R)) == I);
    assert(all(P, p -> radical sub(p, R) == radical I)));

-- Verbose option
P = binomialPrimaryDecomposition(ideal(x^2-y, y^2-x), Verbose => false);
assert(#P == 4);

