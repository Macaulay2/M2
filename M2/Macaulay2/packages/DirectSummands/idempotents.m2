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

-- TODO: findIdem right now will fail if K is not L[a]/f(a);
-- in general, will need to find a primitive element first
findIdempotent = method(Options => DirectSummandsOptions ++ { "Splitting" => null })
findIdempotent CoherentSheaf := opts -> M -> findIdempotent(module M, opts)
findIdempotent Module        := opts -> M -> (
    R := ring M;
    p := char R;
    F := groundField R;
    e := fieldExponent R;
    K := quotient ideal gens R;
    V := K ** M;
    inexactFlag := instance(F, InexactField);
    l := if p == 0 then e else max(e, ceiling log_p numgens M);
    L := null;
    -- this is used in generalEndomorphism
    -- to avoid recomputing the Hom module
    (pr, inc) := opts#"Splitting" ?? (id_M, id_M);
    tries := opts.Tries ?? defaultNumTries p;
    for c to tries - 1 do (
	f := generalEndomorphism(M, pr, inc);
	fm := sub(K ** cover f, F);
	if fm == 0 then continue;
	-- if at most one eigenvalue is found the module is probably indecomposable,
	-- unless the characteristic polynomial has odd degree, then one is enough.
	eigen := eigenvalues' fm;
	-- we only want eigenvalues in F
	eigen = nonnull apply(eigen, y -> try lift(y, F));
	if #eigen <= 1 then (
	    -- to be used as a suggestion in the error
	    -- TODO: expand for inexact fields
	    if L === null and not inexactFlag then L = try extField { char fm };
	    -- if char fm doesn't factor over F, or if it fully factors
	    -- but has only one eigenvalue, we can't find an idempotent
	    if #eigen == 1 and F === L
	    or #eigen == 0 then continue);
	-- try to find idempotens from eigenvalues
        opers := flatten for y in eigen list (
	    if p == 0 then (1, f - y*id_M, fm - y*id_V) else (
		for j from 0 to e list (j, f - y*id_M, largePower'(p, j+1, largePower(p, l, fm - y*id_V)))));
	idem := position(opers, (j, g, gm) -> isWeakIdempotent gm and not isSurjective gm and gm != 0);
        if idem =!= null then (
	    (j, g, gm) := opers_idem;
	    idem = if p == 0 then g else largePower'(p, j+1, largePower(p, l, g));
	    -- for inexact fields, we compose the idempotent until the determinant is zero
	    if inexactFlag then idem = idem ^ (findErrorMargin idem);
	    return idem));
    -- TODO: skip the "Try using" line if the field is large enough, e.g. L === K
    -- TODO: if L is still null, chane the error
    error("no idempotent found after ", tries, " attempts. ",
	"Try using changeBaseField with ", toString L))

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
	    or zero(hm := K ** cover h) then return;
	    certified = false;
	    if isWeakIdempotent hm then break h));
    if idemp =!= null then M.cache.Idempotents ??= { idemp };
    (idemp, certified))

-- this is essentially the Meat-Axe algorithm,
-- but the process for finding an idempotent for
-- a module over a polynomial ring makes it distict.
summandsFromIdempotents = method(Options => options findIdempotent)
summandsFromIdempotents Module := opts -> M -> (
    if rank cover M <= 1 then return {M};
    M.cache.Idempotents ??= {};
    idems := if 0 < #M.cache.Idempotents then M.cache.Idempotents
    else try findIdempotent(M, opts) else return {M};
    summandsFromIdempotents(M, idems, opts))

-- keep close to summandsFromProjectors
summandsFromIdempotents(Module, Matrix) := opts -> (M, h) -> summandsFromIdempotents(M, {h}, opts)
summandsFromIdempotents(Module, List)   := opts -> (M, ends) -> (
    checkRecursionDepth();
    -- maps from kernel summands and to cokernel summands
    injs  := apply(ends, h -> inducedMap(M, ker h));
    projs := apply(ends, h -> inducedMap(coker h, M));
    -- composition of all endomorphisms is the complement
    comp := product ends;
    injs  = append(injs,  inducedMap(M, image comp));
    projs = append(projs, inducedMap(image comp, M, comp));
    -- assert(0 == intersect apply(ends, ker));
    -- assert(0 == intersect apply(injs, image));
    -- assert first isIsomorphic(M, directSum apply(projs, target));
    -- this is the splitting (surjection, inclusion) of M to a module
    -- whose degree zero endomorphisms have already been computed.
    (pr0, inc0) := opts#"Splitting" ?? (id_M, id_M);
    if opts.Verbose then printerr("splitting summands of ranks ",
	toString apply(injs, i -> rank source i));
    c := -1;
    comps := flatten for n to #ends list (
	(pr, inc) := (projs#n, injs#n);
	(N0, K0) := (target pr, source inc);
	if (N := prune N0) == 0 then continue;
	-- TODO: can we check if M has multiple copies of N quickly?
	iso := isomorphism(K0, N0);
	p := inverse N.cache.pruningMap * pr;
	i := try inc * iso * N.cache.pruningMap;
	L := nonzero summandsFromIdempotents(N, opts,
	    "Splitting" => (p * pr0, try inc0 * i));
	-- Projection maps to the summands
	if #L > 1 then apply(#L, i ->
	    M.cache#(symbol ^, [c += 1]) = N^[i] * p)
	else M.cache#(symbol ^, [c += 1]) = p;
	-- Inclusion maps are computed on-demand
	L);
    --M.cache.Idempotents = apply(c, i -> M.cache#(symbol ^, [i]));
    -- TODO: sort these, along with the projections
    comps)
