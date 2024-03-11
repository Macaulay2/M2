--needsPackage "RationalPoints2"

generalEndomorphism = method()
generalEndomorphism Module := M -> (
    R := ring M;
    A := Hom(M, M,
	DegreeLimit       => zdeg := degree 0_M,
	MinimalGenerators => false);
    B := smartBasis(zdeg, A);
    homomorphism(B * random(source B, R^1)))
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
    b := symbol b;
    T := (baseField ring A)(monoid[b]);
    B := sub(cover A, T);
    I := id_(source B);
    det(B - T_0 * I))

largePower = (p,l,M) -> (
    if p^l < 2^30 then return M^(p^l);
    --should have this line check for monomial size of ambient ring also
    N := M;
    for i from 1 to l do N = N^p;
    N)

largePower' = (p,l,M) -> (
    if p^l < 2^30 then return M^(p^l-1);
    --should have this line check for monomial size of ambient ring also
    N := M;
    (largePower(p,l-1,M))^(p-1) * largePower'(p,l-1,M))

findIdempotent = method(Options => { Tries => 50 })
findIdempotent Module      := opts ->  M     -> findIdempotent(M, fieldExponent ring M,opts)
findIdempotent(Module, ZZ) := opts -> (M, e) -> (
    p := char ring M;
    K := quotient ideal gens ring M;
    l := if p == 0 then e else max(e, ceiling log_p numgens M);
    q := infinity;
    for i to opts.Tries - 1 do (
        f := generalEndomorphism M;
        q = min(q,(extField {chi:=char f}).order);
        eigen := flatten rationalPoints ideal chi;
        opers := flatten for i in eigen list if p == 0 then (f - i*id_M) else for j from 0 to e list largePower'(p,j+1, largePower(p,l,f-i*id_M));
        idem := position(opers, g -> isIdempotent(K ** g) and g != id_M and K ** g != 0);
        if idem =!= null then (
	    if 1 < debugLevel then printerr("found idempotent after ", toString i, " attempts.");
	    return opers_idem));
    error("no idempotent found after ", toString opts.Tries, " attempts. Try extending the base field to GF ",toString q))

findIdempotent CoherentSheaf := opts -> M -> findIdempotent(module M,opts)

potentialExtension = method()
potentialExtension Module := M -> extField {char generalEndomorphism M}
potentialExtension CoherentSheaf := M -> potentialExtension module M
