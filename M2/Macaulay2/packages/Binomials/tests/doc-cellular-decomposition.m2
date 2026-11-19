-- doc-cellular-decomposition.m2
-- binomialCellularDecomposition, its synonym BCD, the options ReturnCellVars
-- and Verbose.  Documented: a list of cellular ideals whose intersection is
-- I; with ReturnCellVars => true "a list of pairs consisting of an ideal and
-- the list of its cell variables".
-- Oracles: intersection equals I; every component is cellular by definition
-- (each variable a non-zerodivisor or nilpotent) and binomial; no component can
-- be dropped (minimality); the returned cell variables are exactly the
-- non-zerodivisor variables of the component.
-- Includes non-unital inputs (the function does not require unital input),
-- monomial, principal, prime, already-cellular ideals, embedded components,
-- non-binomial generators, other term orders and gradings.

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

checkBCD = I -> (
    C := binomialCellularDecomposition I;
    assert(instance(C, List));
    assert(all(C, c -> instance(c, Ideal) and ring c === ring I));
    if I == 1 then (assert(C == {}); return);
    assert(intersect C == I);
    assert(all(C, isCellularOracle));
    assert(all(C, isBinomial));
    if #C > 1 then assert(all(#C, i -> intersect drop(C, {i,i}) != I));
    CV := binomialCellularDecomposition(I, ReturnCellVars => true);
    assert(#CV == #C);
    assert(all(CV, p -> instance(p, List) and #p == 2 and instance(p#0, Ideal) and instance(p#1, List)));
    assert(setEq(apply(CV, first), C));
    assert(all(CV, p -> set p#1 === set nzdVars p#0));
    assert(setEq(BCD I, C));
    assert(setEq(binomialCellularDecomposition(I, Verbose => false), C));
    assert(setEq(apply(BCD(I, ReturnCellVars => true, Verbose => false), first), C));
    );

R = QQ[x,y,z,w];
scan({ideal(x*y-z, x*z-y^2), ideal(x^2, x*y, y^2, x*(z^3-1), y*(z^2-1)),
      ideal(y^3, y^2*z^2-x^3, x*y^2*z, x^3*z-x*y), ideal(x^2, x*y), ideal(x*y, z*w),
      ideal(x*(y-1)), ideal(x*(y^3-1)), ideal(x^3-y^2), ideal(x*y-z*w), ideal(x,y,z,w),
      ideal(0_R), ideal(1_R), ideal(x^2-2*y^2), ideal(2*x-3*y), ideal(x*(2*y-3*z)),
      ideal(x^2-2*y^2, x*z), ideal(x-y+z, z), ideal(x^2-y^2, x^2+y^2-2*z^2),
      ideal(x*y-1, x^2-y, x^3-1, x*y*z-z), ideal(x^2*y-z^2*w, x*w^3),
      ideal(x*y^2-z*w^2, x^2*z-y*w, z^3), ideal(x^4-y^4, x*z, y*w)}, checkBCD);

-- the documented example: exactly two cells
R = QQ[x,y,z];
C = BCD(ideal(x*y-z, x*z-y^2), ReturnCellVars => true);
assert(#C == 2);
-- a cellular ideal is its own cellular decomposition
for I in {ideal(x-z^2, y^4), ideal(x^3-y^2), ideal(x^2-y^2, z^3)} do (
    assert(isCellularOracle I);
    C := BCD I;
    assert(#C == 1 and first C == I));

-- other term orders and gradings
for R in {QQ[x,y,z, MonomialOrder=>Lex], QQ[x,y,z, Weights=>{1,2,3}], QQ[x,y,z, Degrees=>{1,2,3}],
          QQ[x,y,z, MonomialOrder=>{Weights=>{3,1,2}, GRevLex}], QQ[x,y,z, Degrees=>{{1,0},{0,1},{1,1}}]} do (
    x := R_0; y := R_1; z := R_2;
    scan({ideal(x^3*z - y*z, y^2*z^2 - z^2*x), ideal(x*y-z, x*z-y^2), ideal(x*(y^2-1), x^2*z)}, checkBCD));

-- many variables, few used
R = QQ[a..l];
checkBCD ideal(a*b - c^2, a*c - b^2, d*e);
checkBCD ideal(a^2 - l^2, a*l);
