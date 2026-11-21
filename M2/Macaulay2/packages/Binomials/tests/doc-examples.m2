-- doc-examples.m2
-- Every EXAMPLE block of the Binomials documentation, with the mathematical
-- content asserted (not the printed form).  Where the documentation shows a
-- decomposition, the intersection is checked and the promised property of each
-- component is checked against Macaulay2 core functions.
-- Not covered here (see FINDING files): unmixedness of the BUD /
-- cellularBinomialUnmixedDecomposition examples (doc-FINDING-unmixed.m2).

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

-- binomialPrimaryDecomposition / BPD
R = QQ[x,y,z];
I = ideal (x*y-z, x*z-y^2);
bpd = binomialPrimaryDecomposition I;
assert(instance(bpd, List) and all(bpd, j -> instance(j, Ideal)));
assert(intersect bpd == I);
assert(all(bpd, isPrimary));
assert(setEq(apply(bpd, radical), associatedPrimes I));
assert(setEq(BPD I, bpd));

-- binomialUnmixedDecomposition / BUD
I = ideal (x^2, x*y, y^2, x*(z^3-1), y*(z^2-1));
bud = binomialUnmixedDecomposition I;
assert(instance(bud, List) and all(bud, j -> instance(j, Ideal)));
assert(intersect bud == I);
assert(all(bud, isBinomial));
assert(intersect BUD I == I);

-- binomialCellularDecomposition / BCD / ReturnCellVars
I = ideal (x*y-z, x*z-y^2);
bcd = binomialCellularDecomposition I;
assert(intersect bcd == I);
assert(all(bcd, isCellularOracle));
assert(setEq(BCD I, bcd));
cv = binomialCellularDecomposition (I, ReturnCellVars=>true, Verbose=>false);
assert(instance(cv, List) and #cv == #bcd);
assert(all(cv, p -> instance(p, List) and #p == 2 and instance(p#0, Ideal) and instance(p#1, List)));
assert(setEq(apply(cv, first), bcd));
assert(all(cv, p -> set p#1 === set nzdVars p#0));
-- the two cells: the toric component (all variables are cell variables) and (y,z) with cell variable x
assert(setEq(apply(cv, first), {ideal(y^2-x*z, x*y-z, x^2-y), ideal(y,z)}));
assert(all(cv, p -> if p#0 == ideal(y,z) then set p#1 === set {x} else set p#1 === set {x,y,z}));

-- binomialRadical
R = QQ[x,y];
I = ideal (y^2, x*y-y, x^2-1);
r = binomialRadical I;
assert(r == sub(radical I, ring r));
assert(r == sub(ideal(y, x^2-1), ring r));

-- cellularBinomialRadical with CellVariables from isCellular
R = QQ[x,y,z];
I = ideal(y^3,y^2*z^2-x^3,x*y^2*z,x^3*z-x*y);
cv = isCellular (I,ReturnCellVars=>true);
assert(set cv === set {z});
assert(set cv === set nzdVars I);
r = cellularBinomialRadical (I,CellVariables=>cv);
assert(r == radical I);
assert(r == ideal(x,y));

-- binomialMinimalPrimes
mp = binomialMinimalPrimes I;
assert(#mp == 1);
assert(sub(first mp, R) == ideal(x,y));

-- binomialAssociatedPrimes: I = (x^2-y, y^2-x) has the four points (0,0) and (z,z^2), z^3 = 1
R = QQ[x,y];
I = ideal(x^2-y,y^2-x);
ap = binomialAssociatedPrimes I;
assert(#ap == 4);
D = liftData ring first ap;
assert(setEq(apply(ap, P -> liftIdeal(P, D)), associatedPrimes liftIdeal(sub(I, ring first ap), D)));
assert(all(ap, isBinomial));

-- binomialIsPrime
assert(binomialIsPrime I === false);
assert(isPrime I === false);

-- binomialIsPrimary
R = QQ[x,y,z];
I = ideal(x-y,z^3);
assert(binomialIsPrimary I === true);
assert(isPrimary I);

-- cellularBinomialIsPrimary with ReturnPrimes: two distinct associated primes
R = QQ[x,y];
I = ideal(x^2-1);
pr = cellularBinomialIsPrimary (I,ReturnPrimes=>true);
assert(instance(pr, List) and #pr == 2);
S = ring first pr; use R;
assert(ring last pr === S);
assert(first pr != last pr);
D = liftData S;
assP = associatedPrimes liftIdeal(sub(I, S), D);
assert(all(pr, P -> any(assP, Q -> liftIdeal(P, D) == Q)));

-- binomialSolve
R = QQ[x,y,z,w];
I = ideal (x-y,y-z,z*w-1*w,w^2-x);
assert(dim I == 0);
sols = binomialSolve I;
assert(instance(sols, List));
assert(all(sols, p -> instance(p, List) and #p == numgens R));
satisfies = (I, p) -> (K := ring first p; f := map(K, ring I, p); all(flatten entries gens I, g -> f g == 0));
assert(all(sols, p -> satisfies(I, p)));
assert(#unique sols == degree radical I);
expected = {{1,1,1,1},{1,1,1,-1},{0,0,0,0}};
assert(all(expected, e -> any(sols, p -> all(#e, i -> p#i == e#i))));
J = ideal (x^3-1,y-x,z-1,w-1);
sols = binomialSolve J;
assert(all(sols, p -> satisfies(J, p)));
assert(#unique sols == 3);
assert(#unique sols == degree radical J);

-- isCellular
R = QQ[x,y,z];
I = ideal (x-z^2,y^4);
assert(isCellular I === true);
assert(set isCellular (I, ReturnCellVars=>true) === set {x,z});
assert(isCellularOracle I);

-- isBinomial / isUnital
assert(isBinomial ideal(x^2) === true);
assert(isBinomial ideal(x-y+z,z) === true);
assert(isBinomial ideal(x^3-x) === true); -- x^3-x has two terms
assert(isUnital ideal (x-z,z-y) === true);
assert(isUnital ideal (x+z) === false);
assert(isUnital ideal (x^2) === true);

-- makeBinomial: exponent vector m = m+ - m-, binomial x^m+ - c x^m-
b = makeBinomial (R, [1,-1,-2], 10);
assert(ring b === R);
assert(b == x - 10*y*z^2);

-- latticeBasisIdeal: one binomial per column
L = matrix {{1,1},{-3,0},{0,1}};
assert(latticeBasisIdeal (R, L) == ideal(x - y^3, x*z - 1));

-- cellularBinomialAssociatedPrimes
R = QQ[x,y];
I = ideal(x^2-1,y-x);
cv = isCellular (I,ReturnCellVars=>true);
assert(set cv === set {x,y});
cap = cellularBinomialAssociatedPrimes (I,CellVariables=>cv);
assert(setEq(apply(cap, P -> sub(P, R)), {ideal(x-1,y-1), ideal(x+1,y+1)}));

-- cellularBinomialPrimaryDecomposition: (x^3-1, y-x) splits over QQ(zeta_3)
I = ideal(x^3-1,y-x);
cv = isCellular (I,ReturnCellVars=>true);
pd = cellularBinomialPrimaryDecomposition (I,CellVariables=>cv);
assert(#pd == 3);
S = ring first pd; use R; D = liftData S;
lpd = apply(pd, j -> liftIdeal(j, D));
assert(intersect lpd == liftIdeal(sub(I, S), D));
assert(all(lpd, isPrime));
assert(all(mingens \ pd, m -> numcols m == 2));

-- cellularBinomialUnmixedDecomposition
I = ideal(x*(y^3-1),x^2);
cv = isCellular (I,ReturnCellVars=>true);
assert(set cv === set {y});
ud = cellularBinomialUnmixedDecomposition (I,CellVariables=>cv);
assert(intersect ud == I);

-- partialCharacter / PartialCharacter / idealFromCharacter
I = ideal(x^3-1,y-x);
cv = isCellular (I,ReturnCellVars=>true);
pc = partialCharacter (I,CellVariables=>cv);
assert(instance(pc, PartialCharacter));
assert(instance(pc, HashTable));
assert(set keys pc === set {"J","L","c"});
assert(set pc#"J" === set {x,y});
assert(instance(pc#"L", Matrix));
assert(#(pc#"c") == numcols pc#"L");
-- the lattice is {(a,b) : a+b = 0 mod 3} = span{(1,-1),(0,3)}
Lexp = matrix{{1,0},{-1,3}};
Lpc = pc#"L";
-- rows of L correspond to pc#"J"; reorder to (x,y)
Lpc = matrix apply({x,y}, v -> (i := position(pc#"J", u -> u == v); first entries Lpc^{i}));
assert(image Lpc == image Lexp);
assert(idealFromCharacter (R,pc) == I);

-- randomBinomialIdeal
R = QQ[a..x];
rb = randomBinomialIdeal (R,6,2,4,true);
assert(instance(rb, Ideal) and ring rb === R);
assert(isBinomial rb and isUnital rb);
rb = randomBinomialIdeal (R,3,4,10,false);
assert(instance(rb, Ideal) and ring rb === R);
assert(isBinomial rb and isUnital rb);

-- extractInclusionMinimalIdeals
R = QQ[a,b];
L = {ideal(a^4),ideal(a^3),ideal(a^5),ideal(b^2*a) };
E = extractInclusionMinimalIdeals L;
assert(setEq(E, {ideal(a^5), ideal(a*b^2)}));

-- ReturnCellVars documentation example
R = QQ[x,y,z];
I = ideal (x*y-z, x*z-y^2);
bcd = binomialCellularDecomposition (I,ReturnCellVars=>true);
assert(intersect apply(bcd, first) == I);
assert(all(bcd, p -> set p#1 === set nzdVars p#0));

-- ReturnPrimes documentation example (three variables)
I = ideal (x^2-1);
pr = cellularBinomialIsPrimary (I,ReturnPrimes=>true);
assert(#pr == 2);
assert(setEq(apply(pr, P -> sub(P, R)), {ideal(x-1), ideal(x+1)}));

-- ReturnPChars documentation example: two partial characters of distinct associated primes
R = QQ[x];
I = ideal (x^2-1);
pcs = cellularBinomialIsPrimary (I,ReturnPChars=>true);
assert(instance(pcs, List) and #pcs == 2);
-- accept both the PartialCharacter and the {J,L,c} list representation here;
-- the type is checked in doc-FINDING-returnpchars-type.m2
pcData = p -> if instance(p, HashTable) then (p#"J", p#"L", p#"c") else (p#0, p#1, p#2);
charIdeal = (R, p) -> (
    (J, L, c) := pcData p;
    ideal apply(numcols L, i -> (
        e := flatten entries L_{i};
        product(#J, k -> (J#k)^(max(e#k,0))) - (c#i) * product(#J, k -> (J#k)^(max(-e#k,0))))));
assert(setEq(apply(pcs, p -> charIdeal(R, p)), {ideal(x-1), ideal(x+1)}));
