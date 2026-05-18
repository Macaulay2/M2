needsPackage "RationalPoints2"

findProjectors = method(Options => DirectSummandsOptions)
findProjectors CoherentSheaf := opts -> M -> sheaf \ findProjectors(module M, opts)
findProjectors Module        := opts -> M -> (
    if not isHomogeneous M then error "expected a homogeneous module";
    R := ring M;
    p := char R;
    F := groundField R;
    K := quotient ideal gens R;
    n := numgens M;
    L := null;
    -- this is used in generalEndomorphism
    -- to avoid recomputing the Hom module
    (pr, inc) := opts#"Splitting" ?? (id_M, id_M);
    -- TODO: sort the degrees to make finding eigenvalues faster?
    -- degs := unique sort degrees M;
    tries := opts.Tries ?? defaultNumTries p;
    for c to tries - 1 do (
	f := generalEndomorphism(M, pr, inc); -- about 20% of computation
	-- eigenvalues of f are over an extension of the field,
	-- and f can be made in upper block triangular form over R
	-- (i.e. without passing to frac R), where the blocks
	-- are over the field (not its extension), hence we can
	-- compute the eigenvalues by going to the field
	f0 := sub(K ** cover f, F);
	if f0 == 1 or f0 == 0 then continue;
	-- finding eigenvalues would be faster if the matrix
	-- was put in Jordan form first, but this is easier...
	-- TODO: computing eigenvalues over coefficient field
	-- would significantly speed up this step
	eigen := eigenvalues'' f0; -- about 25% of computation
	-- if no eigenvalues are found or the min. poly. has been
	-- computed already, we get projectors from its factors
	projs := if #eigen < 1 or f0.cache.?minimalPolynomial
	then projectorsFromMinimalPolynomial(f, minimalPolynomial f0)
	else apply(eigen, y -> minimalProjectorFromEigenvalue(f - y, f0 - y));
	-- if the min. poly. was linear, the single projector is zero
	projs = select(projs, g -> not zero g and not isInjective g);
	if 0 < #projs then return projs;
	-- if the min. poly. is (t - y)^n, M is likely indecomposable over k
	-- and if it is (t - y), then it is probably indecomposable over \bar{k}
	-- TODO: can we store and use this info somewhere? e.g. isIndecomposable?
	if L === null and isField F and {1} < degree(mp := minimalPolynomial f0)
	then L = if instance(F, InexactField) then CC else splittingField mp;
	continue
    );
    if L =!= null and L =!= F
    then printerr("try using changeBaseField with ", toString L);
    error("no projectors found after ", tries, " attempts."))

-- TODO: can this be useful?
findBasicProjectors = M -> (
    R := ring M;
    F := groundField R;
    K := quotient ideal gens R;
    n := numgens M;
    B := gensEnd0 M;
    for c to numcols B - 1 do (
	f := homomorphism B_{c};
	if f === id_M then continue;
	f0 := sub(K ** cover f, F);
	eigen := eigenvalues'' f0;
	projs := if #eigen < 1 or f0.cache.?minimalPolynomial
	then projectorsFromMinimalPolynomial(f, minimalPolynomial f0)
	else apply(eigen, y -> minimalProjectorFromEigenvalue(f - y, f0 - y));
	-- if the min. poly. was linear, the single projector is zero
	projs = select(projs, g -> not zero g and not isInjective g);
	if 0 < #projs then return projs);
    {})

-- this algorithm does not depend on finding idempotents,
-- hence it is distinct from the Meat-Axe algorithm.
summandsFromProjectors = method(Options => options findProjectors)
summandsFromProjectors Module := opts -> M -> (
    if not isHomogeneous M then error "expected homogeneous module";
    if opts.Verbose then printerr "splitting summands using projectors";
    if rank cover M <= 1 or prune' M == 0 then return {M};
    projs := try M.cache.Idempotents else {};
    if 0 == #projs then projs = try findProjectors(M, opts) else
    if char ring M == 0 then findBasicProjectors M else return {M};
    summandsFromProjectors(M, projs, opts))

-- keep close to summandsFromIdempotents
-- this algorithm is more efficient as it has a significant
-- chance of splitting the module in a single iteration.
summandsFromProjectors(Module, Matrix) := opts -> (M, pr) -> summandsFromProjectors(M, {pr}, opts)
summandsFromProjectors(Module, List) := opts -> (M, ends) -> (
    if #ends == 0 then return {M};
    checkRecursionDepth();
    -- in some examples, we use barebones splitComponentsBasic
    if opts.Strategy & 4 == 4 or not isHomogeneous M
    then return splitComponentsBasic(M, ends, opts);
    -- maps from kernel summands and to cokernel summands
    injs  := apply(ends, h -> inducedMap(M, ker h));
    projs := apply(ends, h -> inducedMap(coker h, M));
    -- composition of all endomorphisms is the complement
    comp := product ends;
    injs  = append(injs,  inducedMap(M, image comp));
    projs = append(projs, inducedMap(image comp, M, comp));
    -- assert(0 == intersect apply(ends, ker));
    -- assert(0 == intersect apply(injs, image));
    -- assert isIsomorphic(M, directSum apply(projs, target));
    -- this is the splitting (surjection, inclusion) of M to a module
    -- whose degree zero endomorphisms have already been computed.
    (pr0, inc0) := opts#"Splitting" ?? (id_M, id_M);
    if opts.Verbose then printerr("splitting summands of ranks ",
	toString apply(injs, i -> rank source i));
    c := -1; -- component counter
    comps := for n to #ends list (
	(pr, inc) := (projs#n, injs#n);
	(N0, K0) := (target pr, source inc);
	if (N := prune' N0) == 0 then continue;
	-- TODO: can we check if M has multiple copies of N quickly?
	iso := try isomorphism(K0, N0);
	p := inverse N.cache.pruningMap * pr;
	i := try inc * iso * N.cache.pruningMap;
	M.cache#(symbol ^, [c += 1]) = p; -- temporary
	N.cache.components = summandsFromProjectors(N,
	    opts, "Splitting" => (p * pr0, try inc0 * i));
	N);
    --M.cache.Idempotents = apply(c, i -> M.cache#(symbol ^, [i]));
    -- Finally, call a helper to add splitting maps
    splitComponents(M, comps, components))
