-- doc-partial-character.m2
-- partialCharacter (with and without CellVariables), the type
-- PartialCharacter (keys "J", "L", "c"), and idealFromCharacter.
-- Oracles (ES96, Cor. 2.6 / Thm. 2.1): for a cellular binomial ideal I with
-- cellular variables J, I meets k[J] in the lattice ideal of its partial
-- character, so
--   * pc#"J" are exactly the non-zerodivisor variables,
--   * idealFromCharacter(R, pc) == eliminate(other variables, I),
--   * every column l of pc#"L" with value c_l gives x^(l+) - c_l x^(l-) in I,
--   * rank L = numgens R - dim(I meet k[J]) (computed in R),
--   * #c == number of columns of L.
-- idealFromCharacter is also tested on hand-made PartialCharacters.

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

colBinomial = (R, J, l, c) -> product(#J, k -> (J#k)^(max(l#k,0))) - c * product(#J, k -> (J#k)^(max(-l#k,0)));

checkPC = I -> (
    R := ring I;
    assert(isCellularOracle I);
    cv := nzdVars I;
    pc := partialCharacter I;
    pc2 := partialCharacter(I, CellVariables => isCellular(I, ReturnCellVars=>true));
    for p in {pc, pc2} do (
        assert(instance(p, PartialCharacter));
        assert(set keys p === set {"J","L","c"});
        assert(set p#"J" === set cv);
        L := p#"L";
        assert(instance(L, Matrix));
        assert(numrows L == #(p#"J") or #(p#"J") == 0);
        assert(#(p#"c") == numcols L);
        nc := toList(set gens R - set cv);
        E := if #nc == 0 then I else eliminate(nc, I);
        assert(idealFromCharacter(R, p) == E);
        if #(p#"J") > 0 then (
            assert(rank L == numgens R - dim E);
            scan(numcols L, i -> assert(colBinomial(R, p#"J", flatten entries L_{i}, (p#"c")#i) % I == 0))));
    assert(idealFromCharacter(R, pc) == idealFromCharacter(R, pc2));
    );

R = QQ[x,y,z];
scan({ideal(x^3-1, y-x), ideal(y^3, y^2*z^2-x^3, x*y^2*z, x^3*z-x*y), ideal(x^2-y^2, z^3),
      ideal(x*y-z^2, x^2-y*z, y^2-x*z), ideal(x^2*y-2*z^3), ideal(x^4-1, y^6-1, x^2*y^3+1),
      ideal(x^2-y^2), ideal(x^2*y^2-z^4), ideal(z^2, x^2-y^2),
      ideal(2*x-3*y), ideal(x^2-2*y^2, z^2), ideal(x^6-y^4, z^3), ideal(0_R),
      ideal(x^2, x*(y-1)), ideal(x*(y^3-1), x^2), ideal(x,y,z)}, checkPC);
-- NOTE: cellular ideals whose cellular variables are not the first variables of R
-- with a non-trivial lattice, e.g. ideal(x-z^2, y^4), are tested in
-- doc-FINDING-idealFromCharacter-variables.m2

-- documented example: lattice span{(1,-1),(0,3)}, trivial character
R = QQ[x,y];
I = ideal(x^3-1, y-x);
pc = partialCharacter(I, CellVariables => isCellular(I, ReturnCellVars=>true));
assert(idealFromCharacter(R, pc) == I);
assert(all(pc#"c", v -> v == 1));

-- non-trivial character values: x - 2y, y - 3z, i.e. c = 2 on (1,-1,0), 3 on (0,1,-1)
R = QQ[x,y,z];
I = ideal(x-2*y, y-3*z);
pc = partialCharacter I;
assert(set pc#"J" === set {x,y,z});
assert(idealFromCharacter(R, pc) == I);
assert(rank pc#"L" == 2);

-- hand-made partial characters
pc = new PartialCharacter from {"J" => {x,y}, "L" => matrix{{2},{-2}}, "c" => {1}};
assert(idealFromCharacter(R, pc) == ideal(x^2-y^2));
pc = new PartialCharacter from {"J" => {x,y,z}, "L" => matrix{{1,0},{-1,1},{0,-1}}, "c" => {2, 3}};
assert(idealFromCharacter(R, pc) == ideal(x-2*y, y-3*z));
-- saturated lattice, trivial character: toric ideal of the twisted cubic
S = QQ[a,b,c,d];
pc = new PartialCharacter from {"J" => {a,b,c,d}, "L" => matrix{{1,0},{-2,1},{1,-2},{0,1}}, "c" => {1,1}};
F = idealFromCharacter(S, pc);
assert(F == ker map(QQ[s,t], S, {s^3, s^2*t, s*t^2, t^3}));
assert(isPrime F);
-- non-saturated lattice 2Z: x^2 - y^2 type lattice ideal is the lattice ideal, not the basis ideal
pc = new PartialCharacter from {"J" => {a,b}, "L" => matrix{{2},{-2}}, "c" => {-1}};
assert(idealFromCharacter(S, pc) == ideal(a^2+b^2));
-- lattice given by a non-basis generating set: lattice ideal (saturated at the variables)
pc = new PartialCharacter from {"J" => {a,b,c}, "L" => matrix{{1,1},{-1,0},{0,-1}}, "c" => {1,1}};
assert(idealFromCharacter(S, pc) == ideal(a-b, a-c));
