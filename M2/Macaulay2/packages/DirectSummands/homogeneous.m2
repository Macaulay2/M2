needsPackage "RationalPoints2"

findProjectors = method(Options => DirectSummandsOptions)
findProjectors Module := opts -> M -> (
    R := ring M;
    p := char R;
    F := groundField R;
    K := quotient ideal gens R;
    n := numgens M;
    L := null;
    -- TODO: sort the degrees to make finding eigenvalues faster?
    -- degs := unique sort degrees M;
    tries := opts.Tries ?? ceiling(0.1 + 50 / log p);
    for c to tries - 1 do (
	f := generalEndomorphism M; -- about 20% of computation
	-- eigenvalues of f must be over the field,
	-- and we can prove that f can be diagonalized over R
	-- (i.e. without passing to frac R), hence we can
	-- compute the eigenvalues by going to the field
	f0 := sub(K ** f, F);
	-- finding eigenvalues would be faster if the matrix
	-- was put in Jordan form first, but this is easier...
	eigen := eigenvalues' f0; -- about 25% of computation
	if #eigen <= 1 then (
	    -- to be used as a suggestion in the error
	    -- TODO: expand for characteristic zero
	    -- TODO: is there any way to tell if the module is indecomposable here?
	    -- e.g. based on the characteristic polynomial factoring completely
	    -- but having a single root only?
	    L = extField { char f0 };
	    continue);
	return for y in eigen list (f - y * id_M)^n
    );
    -- TODO: skip the "Try passing" line if the field is large enough, e.g. L === K
    error("no projector found after ", toString opts.Tries, " attempts. Try passing
	ExtendGroundField => ", if p != 0 then ("GF " | toString L) else toString L))

-- TODO: can this be useful?
findBasicProjectors = M -> (
    R := ring M;
    F := groundField R;
    K := quotient ideal gens R;
    n := numgens M;
    B := gensEnd0 M;
    for c to numcols B - 1 do (
	f := homomorphism B_{c};
	if f == id_M then return;
	f0 := sub(K ** f, F);
	eigen := eigenvalues' f0;
	if #eigen > 1 then return for y in eigen list (f - y * id_M)^n);
    {})

summandsFromProjectors = method(Options => DirectSummandsOptions)
summandsFromProjectors Module := opts -> M -> (
    if degree M <= 1 then return {M};
    -- maps M -> M whose (co)kernel is a (usually indecomposable) summand
    projs := try findProjectors(M, opts) else return {M};
    summandsFromProjectors(M, projs, opts))

summandsFromProjectors(Module, List) := opts -> (M, projs) -> (
    -- assert(0 == intersect apply(projs, ker));
    -- maps M_i -> M from the kernel summands
    injs := apply(projs, pr -> inducedMap(M, ker pr));
    -- assert(0 == intersect apply(injs, image));
    -- the map \bigoplus M_i -> M, whose cokernel is the complement of M_i
    iota := matrix { injs };
    -- assert first isIsomorphic(M, coker iota ++ directSum(coker \ projs));
    if 0 < debugLevel then printerr("splitting ", toString(#projs+1),
	" summands of ranks ", toString apply(injs, i -> degree source i));
    c := -1;
    comps := flatten for pr in append(projs, iota) list (
	N := prune coker pr;
	L := nonzero summandsFromProjectors(N, opts);
	-- Projection maps to the summands
	p := inverse N.cache.pruningMap * inducedMap(coker pr, M);
	if #L > 1 then apply(#L, i ->
	    M.cache#(symbol ^, [c += 1]) = N^[i] * p)
	else M.cache#(symbol ^, [c += 1]) = p;
	L);
    -- Inclusion maps from the summands
    scan(c + 1, i -> M.cache#(symbol _, [i]) = inverse M.cache#(symbol ^, [i]));
    comps)
