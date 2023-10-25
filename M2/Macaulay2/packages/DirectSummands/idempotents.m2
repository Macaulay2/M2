--needsPackage "RationalPoints2"

generalEndomorphism = method()
generalEndomorphism Module := M -> (
    R := ring M;
    EndM := Hom(M, M, degree 1_R);
    --EndM := End(M);
    B := smartBasis(0, EndM);
    --r := rank source B;
    --if r == 1 then return "warning: End(M)_0 is 1-dimensional";
    --rm=length select(for i from 0 to r-1 list( unique apply(flatten entries homomorphism B_{i},j->j% ideal gens R) == {0}) ,k->k==false);
    --if rm< 2 then return "warning: End(M)_0/maximal ideal is 1-dimensional";
    homomorphism(B * random(ZZ^(rank source B),ZZ^1)))
generalEndomorphism CoherentSheaf := M -> generalEndomorphism module M

-- TODO: this needs improvement to work over RR, QQ, GF, FractionField, etc.
-- e.g. given a tower such as K[x][y]/I, returns K
baseField = method()
baseField GaloisField := identity
baseField FractionField := identity -- FIXME: does this always work?
baseField Ring := R -> try ( coefficientRing first flattenRing R ) else R

-- e.g. given a field isomorphic to GF(p,e), returns e
fieldExponent = R -> (
    L := baseField R;
    (p, e) := (char L, 1);
    if p == 0 then return e;
    a := L_0; -- primitive element of L
    while a^(p^e) != a do (e = e + 1);
    e)

-- finds the characteristic polynomial of a matrix
char Matrix := A -> (
    if numRows A != numColumns A then error "expected a square matrix";
    T := (baseField ring A)(monoid[vars 0]);
    B := sub(cover A, T);
    I := id_(source B);
    det(B - T_0 * I))

protect Tries
findIdempotent = method(Options => { Tries => 50 })
findIdempotent Module      := opts ->  M     -> findIdempotent(M, fieldExponent ring M)
findIdempotent(Module, ZZ) := opts -> (M, e) -> (
    p := char ring M;
    K := quotient ideal gens ring M;
    l := if p == 0 then e else max(e, ceiling log_p numgens M);
    for i to opts.Tries - 1 do (
        f := generalEndomorphism M;
        eigen := flatten rationalPoints ideal char f;
        opers := flatten for i in eigen list if p == 0 then (f - i*id_M) else for j from 0 to e list (((f-i*id_M)^(p^l))^(p^(j+1)-1));
        idem := position(opers, g -> isIdempotent(K ** g) and g != id_M and K ** g != 0);
        if idem =!= null then (
	    if 1 < debugLevel then printerr("found idempotent after ", toString i, " attempts.");
	    return opers_idem));
    error("no idempotent found after ", toString opts.Tries, " attempts. Try extending the base field."))
findIdempotent CoherentSheaf := opts -> M -> findIdempotent module M
