--needsPackage "RationalPoints2"

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
findIdempotent = method(Options => DirectSummandsOptions)
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

summandsFromIdempotents = method(Options => DirectSummandsOptions)
summandsFromIdempotents Module := opts -> M -> (
    zdeg := degree 0_M;
    -- TODO: make "elapsedTime" contingent on verbosity
    if debugLevel > 1 then printerr "computing Hom module";
    A := Hom(M, M, -- most time consuming step
	DegreeLimit       => if opts.Strategy & 2 == 2 then zdeg,
	MinimalGenerators => if opts.Strategy & 4 == 4 then false);
    B := smartBasis(zdeg, A);
    summandsFromIdempotents(M, B, opts))

summandsFromIdempotents(Module, Matrix) := opts -> (M, B) -> (
    h := if numcols B == 1 then homomorphism B_{0}
    else try findIdempotent(M, opts) else return {M};
    -- TODO: add this when the maps M_[w] and M^[w] also work with subsets
    -- M.cache.components =
    -- TODO: restrict End M to each summand and pass it on
    -- TODO: could use 'compose' perhaps
    -- TODO: can we check if M has multiple copies of M0 or M1 quickly?
    M0 := prune image h;
    M1 := prune coker h;
    -- TODO: can we keep homomorphisms?
    --B0.cache.homomorphism = f -> map(M0, M0, adjoint'(p1 * f * inverse p0, M0, M0), Degree => first degrees source f + degree f);
    M0comps := directSummands(M0, opts);
    M1comps := directSummands(M1, opts);
    -- Projection maps to the summands
    c := -1;
    p0 := inverse M0.cache.pruningMap * inducedMap(image h, M, h);
    p1 := inverse M1.cache.pruningMap * inducedMap(coker h, M);
    if #M0comps > 1 then apply(#M0comps, i -> M.cache#(symbol ^, [c += 1]) = M0^[i] * p0) else M.cache#(symbol ^, [c += 1]) = p0;
    if #M1comps > 1 then apply(#M1comps, i -> M.cache#(symbol ^, [c += 1]) = M1^[i] * p1) else M.cache#(symbol ^, [c += 1]) = p1;
    -- Inclusion maps from the summands
    -- TODO: will this always work?
    scan(c + 1, i -> M.cache#(symbol _, [i]) = inverse M.cache#(symbol ^, [i]));
    -- return the lists
    -- TODO: why is this nonzero needed?
    -- TODO: sort these, along with the projections
    nonzero flatten join(M0comps, M1comps))
