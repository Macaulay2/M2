--needsPackage "RationalPoints2"

-----------------------------------------------------------------------------
-- helpers that should probably move to Core
-----------------------------------------------------------------------------

-- same as flatten(Matrix), but doesn't bother homogenizing the result
--flatten' = m -> map(R := ring m, rawReshape(m = raw m, raw R^1, raw R^(rawNumberOfColumns m * rawNumberOfRows m)))

leadCoefficient Number := x -> x
leadMonomial    Number := x -> 0

-- not strictly speaking the "lead" coefficient, but the first nonzero coefficient
leadCoefficient Matrix := RingElement => m -> if zero m then 0 else (
    for c to numcols m - 1 do for r to numrows m - 1 do (
	if not zero m_(r,c) then return leadCoefficient m_(r,c)))

-- not strictly speaking the "lead" monomial, but the first nonzero monomial
leadMonomial Matrix := RingElement => m -> if zero m then 0 else (
    for c to numcols m - 1 do for r to numrows m - 1 do (
	if not zero m_(r,c) then return leadMonomial m_(r,c)))

-- used to be called reduceScalar
reduceCoefficient = m -> if zero m then m else (
    map(target m, source m, cover m // leadCoefficient m))

reduceMonomial = m -> if zero m then m else (
    map(target m, source m, cover m // leadMonomial m))

-- hacky things for CC
-- TODO: move to Core, also add conjugate Matrix, realPart, imaginaryPart, etc.
conjugate RingElement := x -> sum(listForm x, (e, c) -> conjugate c * (ring x)_e)
magnitude = x -> x * conjugate x
isZero = x -> if not instance(F := ultimate(coefficientRing, ring x), InexactField) then x == 0 else (
    leadCoefficient magnitude x < 2^(-precision F))

-- borrowed from Varieties as hack to get around
-- https://github.com/Macaulay2/M2/issues/3407
flattenMorphism = f -> (
    g := presentation ring f;
    S := ring g;
    -- TODO: sometimes lifting to ring g is enough, how can we detect this?
    -- TODO: why doesn't lift(f, ring g) do this automatically?
    map(target f ** S, source f ** S, lift(cover f, S)) ** cokernel g)

-- reduceCoefficient is a kludge to handle the case when h^2 = ah
isIdempotent = h -> reduceCoefficient(h^2) == reduceCoefficient h
isWeakIdempotent = h -> all(flatten entries flattenMorphism(reduceCoefficient(h^2) - reduceCoefficient h), isZero)
--isWeakIdempotent = h -> isZero det cover flattenMorphism(reduceCoefficient(h^2) - reduceCoefficient h)

-----------------------------------------------------------------------------

-- e.g. given a tower such as K[x][y]/I, returns K
-- TODO: use in localRandom?
groundField = method()
groundField Ring := R -> ultimate(K -> if isField K then K else coefficientRing K, R)

potentialExtension = method()
potentialExtension Module := M -> extField {char generalEndomorphism M}
potentialExtension CoherentSheaf := M -> potentialExtension module M

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

-----------------------------------------------------------------------------
-- findIdempotents
-----------------------------------------------------------------------------

--TODO: findIdem right now will fail if K is not L[a]/f(a); in general, will need to find a primitive element first
findIdempotent = method(Options => DirectSummandsOptions ++ { "SplitSurjection" => null })
findIdempotent CoherentSheaf := opts -> M -> findIdempotent(module M, opts)
findIdempotent Module        := opts -> M -> (
    R := ring M;
    p := char R;
    F := groundField R;
    e := fieldExponent R;
    K := quotient ideal gens R;
    V := K ** M;
    exactFlag := not instance(F, InexactField);
    l := if p == 0 then e else max(e, ceiling log_p numgens M);
    L := infinity;
    -- this is used in generalEndomorphism
    -- to avoid recomputing the Hom module
    surj := opts#"SplitSurjection" ?? id_M;
    tries := opts.Tries ?? defaultNumTries p;
    for c to tries - 1 do (
        f := generalEndomorphism(M, surj);
	if f == 0 then continue;
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
    error("no idempotent found after ", tries, " attempts. Try passing
	ExtendGroundField => ", if p != 0 then ("GF " | toString L) else toString L))

protect Idempotents

-- only tries to find an idempotent among the generators of End_0(M)
-- which is in general unlikely to be successful, but it often works!
-- returns a pair: (idempotent or null, whether M is certified indecomposable)
findBasicIdempotent = M -> (
    M.cache.Idempotents ??= {};
    if 0 < #M.cache.Idempotents
    then return (first M.cache.Idempotents, false);
    R := ring M;
    B := gensEnd0 M;
    K := coker vars R;
    -- whether all non-identity endomorphisms are zero mod m
    -- if this remains true till the end, the module is
    -- certifiably indecomposable.
    certified := true;
    -- TODO: searching over 10k generators for F_*(OO_X)
    -- on Gr(2,4) even over ZZ/3 takes a very long time
    -- TODO: parallelized this and break on first success
    idemp := scan(numcols B, c -> (
	    h := homomorphism B_{c};
	    if zero h or h == id_M
	    or zero(hm := h ** K) then return;
	    certified = false;
	    if isWeakIdempotent hm then break h));
    if idemp =!= null then M.cache.Idempotents ??= { idemp };
    (idemp, certified))

summandsFromIdempotents = method(Options => options findIdempotent)
summandsFromIdempotents Module := opts -> M -> (
    M.cache.Idempotents ??= {};
    h := if 0 < #M.cache.Idempotents then M.cache.Idempotents
    else try findIdempotent(M, opts) else return {M};
    summandsFromIdempotents(M, h, opts))

-- FIXME: handle the case when multiple idempotents are given
summandsFromIdempotents(Module, List)   := opts -> (M, idems) -> summandsFromIdempotents(M, idems#0, opts)
summandsFromIdempotents(Module, Matrix) := opts -> (M, h) -> (
    -- TODO: add this when the maps M_[w] and M^[w] also work with subsets
    -- M.cache.components =
    -- this is a split surjection from a module whose
    -- degree zero endomorphisms have already been computed
    surj := opts#"SplitSurjection" ?? id_M;
    -- TODO: can we check if M has multiple copies of M0 or M1 quickly?
    M0 := prune image h;
    M1 := prune coker h;
    if M0 == 0 or M1 == 0 then return {M};
    if 0 < debugLevel then printerr("splitting 2 summands of ranks ",
	toString {rank M0, rank M1});
    p0 := inverse M0.cache.pruningMap * inducedMap(image h, M, h);
    p1 := inverse M1.cache.pruningMap * inducedMap(coker h, M);
    M0comps := summandsFromIdempotents(M0, opts, "SplitSurjection" => p0 * surj);
    M1comps := summandsFromIdempotents(M1, opts, "SplitSurjection" => p1 * surj);
    -- Projection maps to the summands
    c := -1;
    if #M0comps > 1 then apply(#M0comps, i -> M.cache#(symbol ^, [c += 1]) = M0^[i] * p0) else M.cache#(symbol ^, [c += 1]) = p0;
    if #M1comps > 1 then apply(#M1comps, i -> M.cache#(symbol ^, [c += 1]) = M1^[i] * p1) else M.cache#(symbol ^, [c += 1]) = p1;
    -- Inclusion maps are computed on-demand
    -- return the lists
    -- TODO: why is this nonzero needed?
    -- TODO: sort these, along with the projections
    flatten join(M0comps, M1comps))
