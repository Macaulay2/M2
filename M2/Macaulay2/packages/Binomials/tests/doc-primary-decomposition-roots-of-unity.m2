-- doc-primary-decomposition-roots-of-unity.m2
-- binomialPrimaryDecomposition returns primary components also when an
-- embedded component needs roots of unity of order >= 3.
-- Documented: a list of primary binomial ideals whose intersection is I.
-- QQ[x,y]: J = ideal(y^2, y*(x^3-1)).
--   Ass(J) = {(y), (y,x-1), (y,x^2+x+1)} (over QQ(zeta_3): 4 primes).
-- Further cases:
--   ideal(y-x^3*y, y^2-x*y^2)
--   ideal(x^2,x*y,y^2,x*(z^3-1),y*(z^2-1)) (the BUD documentation example)
--   ideal(-a*c^3+a^2*b, a^3*b^3*c-a^3*c)
-- Oracle: isPrimary over QQ[x,ww]/(Phi) (see helpers).

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

allPrimary = I -> (
    P := binomialPrimaryDecomposition I;
    S := ring first P; D := liftData S;
    lP := apply(P, p -> liftIdeal(p, D));
    assert(intersect lP == liftIdeal(sub(I, S), D));
    all(lP, isPrimary));
R = QQ[x,y];
assert(allPrimary ideal(y^2, y*(x^3-1)));
assert(allPrimary ideal(y-x^3*y, y^2-x*y^2));
R = QQ[x,y,z];
assert(allPrimary ideal(x^2, x*y, y^2, x*(z^3-1), y*(z^2-1)));
R = QQ[a,b,c];
assert(allPrimary ideal(-a*c^3+a^2*b, a^3*b^3*c-a^3*c));
