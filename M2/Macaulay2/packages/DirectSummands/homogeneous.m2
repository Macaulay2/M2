needsPackage "RationalPoints2"

findProjectors = method(Options => DirectSummandsOptions ++ { "Splitting" => null })
findProjectors Module := opts -> M -> (
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
summandsFromProjectors(Module, List) := opts -> (M, ends) -> (
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
    c := -1; -- component counter
    comps := for n to #ends list (
	(pr, inc) := (projs#n, injs#n);
	(N0, K0) := (target pr, source inc);
	if (N := prune N0) == 0 then continue;
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
