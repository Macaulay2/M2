needsPackage "RationalPoints2"

findProjectors = method(Options => DirectSummandsOptions ++ { "SplitSurjection" => null })
findProjectors Module := opts -> M -> (
    R := ring M;
    p := char R;
    F := groundField R;
    K := quotient ideal gens R;
    n := numgens M;
    L := null;
    -- this is used in generalEndomorphism
    -- to avoid recomputing the Hom module
    surj := opts#"SplitSurjection" ?? id_M;
    -- TODO: sort the degrees to make finding eigenvalues faster?
    -- degs := unique sort degrees M;
    tries := opts.Tries ?? defaultNumTries p;
    for c to tries - 1 do (
	f := generalEndomorphism(M, surj); -- about 20% of computation
	if f == 0 then continue;
	-- eigenvalues of f are necessarily over the field,
	-- and we can prove that f can be diagonalized over R
	-- (i.e. without passing to frac R), hence we can
	-- compute the eigenvalues by going to the field
	f0 := sub(K ** cover f, F);
	-- finding eigenvalues would be faster if the matrix
	-- was put in Jordan form first, but this is easier...
	eigen := eigenvalues' f0; -- about 25% of computation
	if #eigen <= 1 then (
	    -- to be used as a suggestion in the error
	    -- TODO: is there any way to tell if the module is indecomposable here?
	    -- e.g. based on the characteristic polynomial factoring completely
	    -- but having a single root only? (= End_0(M) has only one generator?)
	    -- TODO: expand for inexact fields
	    if L === null and not instance(F, InexactField) then L = extField { char f0 };
	    continue);
	return for y in eigen list (f - y * id_M)^n
    );
    -- TODO: skip the "Try using" line if the field is large enough, e.g. L === K
    -- TODO: if L is still null, chane the error
    error("no idempotent found after ", tries, " attempts. ",
	"Try using changeBaseField with ", toString L))

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
	f0 := sub(K ** cover f, F);
	eigen := eigenvalues' f0;
	if #eigen > 1 then return for y in eigen list (f - y * id_M)^n);
    {})

-- this algorithm does not depend on finding idempotents,
-- hence it is distict from the Meat-Axe algorithm.
summandsFromProjectors = method(Options => options findProjectors)
summandsFromProjectors Module := opts -> M -> (
    if rank cover M <= 1 then return {M};
    -- TODO: if M.cache.Idempotents is nonempty, should we use it here?
    -- maps M -> M whose (co)kernel is a (usually indecomposable) summand
    projs := try findProjectors(M, opts) else return {M};
    summandsFromProjectors(M, projs, opts))

-- keep close to summandsFromIdempotents
-- this algorithm is more efficient as it has a significant
-- chance of splitting the module in a single iteration.
summandsFromProjectors(Module, Matrix) := opts -> (M, pr) -> summandsFromProjectors(M, {pr}, opts)
summandsFromProjectors(Module, List) := opts -> (M, projs) -> (
    -- assert(0 == intersect apply(projs, ker));
    -- maps M_i -> M from the kernel summands
    injs := apply(projs, pr -> inducedMap(M, ker pr));
    -- assert(0 == intersect apply(injs, image));
    -- the map \bigoplus M_i -> M, whose cokernel is the complement of M_i
    iota := matrix { injs };
    -- assert first isIsomorphic(M, coker iota ++ directSum(coker \ projs));
    -- this is a split surjection from a module whose
    -- degree zero endomorphisms have already been computed
    surj := opts#"SplitSurjection" ?? id_M;
    c := -1;
    if opts.Verbose then printerr("splitting summands of ranks ",
	toString prepend_(rank coker iota) apply(injs, i -> rank source i));
    comps := flatten for pr in append(projs, iota) list (
	N := prune coker pr;
	if N == 0 then continue;
	p := inverse N.cache.pruningMap * inducedMap(coker pr, M);
	L := nonzero summandsFromProjectors(N, opts,
	    "SplitSurjection" => p * surj);
	-- Projection maps to the summands
	if #L > 1 then apply(#L, i ->
	    M.cache#(symbol ^, [c += 1]) = N^[i] * p)
	else M.cache#(symbol ^, [c += 1]) = p;
	-- Inclusion maps are computed on-demand
	L);
    --M.cache.Idempotents = apply(c, i -> M.cache#(symbol ^, [i]));
    -- TODO: sort these, along with the projections
    comps)
