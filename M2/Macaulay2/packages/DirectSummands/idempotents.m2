--needsPackage "RationalPoints2"

-- give a random vector in a module over a local ring
localRandom = (M, opts) -> (
    R := ring M;
    -- TODO: which coefficient ring do we want?
    K := try coefficientRing R else R;
    v := random(cover M ** K, module K, opts);
    -- TODO: sub should be unnecessary, but see https://github.com/Macaulay2/M2/issues/3638
    vector inducedMap(M, , generators M * sub(v, R)))

random(ZZ,   Module) :=
random(List, Module) := Vector => o -> (d, M) -> vector map(M, , random(cover M, (ring M)^{-d}, o))
random       Module  := Vector => o ->     M  -> (
    if isHomogeneous M then random(degree 1_(ring M), M, o) else localRandom(M, o))

generalEndomorphism = method(Options => options random)
generalEndomorphism Module := Matrix => o -> M -> (
    zdeg := if isHomogeneous M then degree 0_M;
    A := Hom(M, M,
	DegreeLimit       => zdeg,
	MinimalGenerators => false);
    B := if isHomogeneous M then smartBasis(zdeg, A) else inducedMap(A, , gens A);
    homomorphism(B * random(source B, o)))
-- the sheaf needs to be pruned to prevent missing endomorphisms
generalEndomorphism CoherentSheaf := SheafMap => o -> F -> sheaf generalEndomorphism(module prune F, o)

-----NEW STUFF FOR INHOMOGENEOUS CASE-----

findSplitInclusion = method(Options => { Tries => 50 })
--tests if M is a split summand of N
findSplitInclusion(Module, Module) := opts -> (M, N) -> (
    h := for i to opts.Tries - 1 do (
        b := homomorphism random Hom(M, N, MinimalGenerators => false);
        c := homomorphism random Hom(N, M, MinimalGenerators => false);
        if isIsomorphism(c * b) then break b);
    if h === null then return "not known" else return h)

findIdem' = method(Options => { Tries=>500 })
findIdem' Module      := opts ->  M     -> findIdem'(M, fieldExponent ring M,opts)
findIdem'(Module,ZZ) := opts -> (M,e) -> (
    R := ring M;
    p := char R;
    K := quotient ideal gens R;
    l := if p == 0 then e else max(e, ceiling log_p numgens M);
    L := infinity;
    for c to opts.Tries - 1 do (
        f := generalEndomorphism M;
	eigen := eigenvalues' f;
	if #eigen <= 1 then continue;
        opers := flatten for y in eigen list (
	    if p == 0 then (f - y*id_M) else (
		for j from 0 to e list largePower'(p, j+1, largePower(p, l, f - y*id_M))));
        idem := position(opers, g -> findSplitInclusion(image g, source g) =!= null and g != id_M and K ** g != 0 and prune ker g != 0);
        if idem =!= null then (
	    if 1 < debugLevel then printerr("found idempotent after ", toString c, " attempts.");
	    return opers_idem));
)

--ONLY IF WE NEED THE FULL BASE FIELD:
--randomFieldElement = method()
--randomFieldElement(Ring) := K -> (
--    gensK := numgens first flattenRing K;
--    (random(K^1,K^1))_(0,0) + sum for i from 0 to gensK - 1 list (random(K^1,K^1))_(0,0) * K_i
--)

---------------------

-- e.g. given a tower such as K[x][y]/I, returns K
-- TODO: use in localRandom?
groundField = method()
groundField Ring := R -> ultimate(K -> if isField K then K else coefficientRing K, R)

-- e.g. given a field isomorphic to GF(p,e), returns e
fieldExponent = R -> (
    L := groundField R;
    (p, e) := (char L, 1);
    if p == 0 then return e;
    a := L_0; -- primitive element of L
    while a^(p^e) != a do (e = e + 1);
    e)

-- finds the characteristic polynomial of a matrix mod the maximal ideal
char Matrix := A -> A.cache.char ??= (
    if numRows A != numColumns A then error "expected a square matrix";
    b := symbol b;
    T := (groundField ring A)(monoid[b]);
    B := sub(cover A, T);
    I := id_(source B);
    det(B - T_0 * I))

eigenvalues' = A -> (
    Chi := char A;
    F := groundField ring A;
    if instance(F, InexactField) then roots Chi
    else flatten rationalPoints ideal Chi)

largePower = (p,l,M) -> (
    if p^l < 2^30 then return M^(p^l);
    --should have this line check for monomial size of ambient ring also
    N := M;
    for i from 1 to l do N = N^p;
    N)

-- TODO: use BinaryPowerMethod?
largePower' = (p,l,M) -> (
    if p^l < 2^30 then return M^(p^l-1);
    --should have this line check for monomial size of ambient ring also
    N := M;
    (largePower(p,l-1,M))^(p-1) * largePower'(p,l-1,M))

lift(CC, CC_*) := opts -> (r, C) -> numeric(precision C, r)

-- adjust as needed LOL
findErrorMargin = m -> ceiling(log_10 2^(precision ring m))

--TODO: findIdem right now will fail if K is not L[a]/f(a); in general, will need to find a primitive element first
findIdempotent = method(Options => { Tries => 50 })
findIdempotent Module      := opts ->  M     -> findIdempotent(M, fieldExponent ring M,opts)
findIdempotent(Module, ZZ) := opts -> (M, e) -> (
    R := ring M;
    p := char R;
    F := groundField R;
    K := quotient ideal gens R;
    V := K ** M;
    exactFlag := not instance(F, InexactField);
    l := if p == 0 then e else max(e, ceiling log_p numgens M);
    L := infinity;
    for c to opts.Tries - 1 do (
        f := generalEndomorphism M;
	fm := K ** f;
        Chi := char f;
	K' := if not exactFlag then F else try extField {Chi};
        --TODO: remember different L to extend to; right now the L you return at the end may not be large enough
        if p != 0 then L = min(L, K'.order) else L = K';
	-- TODO: this seems too messy, what's the precise requirement?
	-- maybe we should separate this in a different method
        --exactFlag := not( instance(F, InexactField) or isMember(coefficientRing ring Chi, {ZZ, QQ}));
	--needsPackage "RationalPoints2"
	-- TODO: replace with eigenvalues'?
        eigen := if not exactFlag then roots Chi else flatten rationalPoints ideal Chi;
	-- if at most one eigenvalue is found then the module is probably indecomposable
	if not exactFlag and #eigen <= 1 then continue;
	if exactFlag and p!= 0 and #eigen <= 1 and L == F.order then continue;
        --TODO: add check for when the field is QQ
	-- we only want eigen values in F
	eigen = nonnull apply(eigen, y -> try lift(y, F));
	if #eigen == 0 then continue;
        opers := flatten for y in eigen list (
	    if p == 0 then (1, f - y*id_M, fm - y*id_V) else (
		for j from 0 to e list (j, f - y*id_M, largePower'(p, j+1, largePower(p, l, fm - y*id_V)))));
	idem := position(opers, (j, g, g') -> isWeakIdempotent g' and not isSurjective g' and g' != 0);
        if idem =!= null then (
	    (j, g, g') := opers_idem;
	    if 1 < debugLevel then printerr("found idempotent after ", toString c, " attempts.");
	    idem = (if p != 0 then largePower'(p, j+1, largePower(p, l, g)) else g);
	    -- for inexact fields, we compose the idempotent until the determinant is zero
	    if instance(F, InexactField) then idem = idem ^ (findErrorMargin idem);
	    return idem));
    -- TODO: skip the "Try passing" line if the field is large enough, e.g. L === K
    error("no idempotent found after ", toString opts.Tries, " attempts. Try passing
	ExtendGroundField => ", if p != 0 then ("GF " | toString L) else toString L))

findIdempotent CoherentSheaf := opts -> M -> findIdempotent(module M,opts)

potentialExtension = method()
potentialExtension Module := M -> extField {char generalEndomorphism M}
potentialExtension CoherentSheaf := M -> potentialExtension module M
