needsPackage "RationalPoints2"

findProjectors = method(Options => { Tries => 50 })
findProjectors Module := opts -> M -> (
    R := ring M;
    p := char R;
    F := groundField R;
    K := quotient ideal gens R;
    n := numgens M;
    L := null;
    -- TODO: sort the degrees to make finding eigenvalues faster?
    -- degs := unique sort degrees M;
    for c to opts.Tries - 1 do (
	f := generalEndomorphism M; -- about 20% of computation
	-- eigenvalues of f must be over the field,
	-- and we can prove that f can be diagonalized over R
	-- (i.e. without passing to frac R), hence we can
	-- compute the eigenvalues by going to the field
	f0 := sub(K ** f, F);
	-- to be used as a suggestion in the error
	-- TODO: expand for characteristic zero
	L = extField { char f0 }; -- about 25% of computation
	-- finding eigenvalues would be faster if the matrix
	-- was put in Jordan form first, but this is easier...
	eigen := eigenvalues' f0; -- about 25% of computation
	if #eigen <= 1 then continue;
	-- TODO: n is sufficient but usually too large
	-- we need the maximum sum of multiplicities of an eigenvalue
	-- TODO: worth using largePower?
	-*
	netList(projs = for y in eigen list (f - y * id_M)^n)
	(f - eigen#0 * id_M)^n
	(f - eigen#1 * id_M)^n
	netList(injs = apply(projs, pr -> inducedMap(M, ker pr)))
	projs#0
	projs#1
	image injs#0
	image injs#1
	*-
	return for y in eigen list (f - y * id_M)^n
    );
    -- TODO: skip the "Try passing" line if the field is large enough, e.g. L === K
    error("no projector found after ", toString opts.Tries, " attempts. Try passing
	ExtendGroundField => ", if p != 0 then ("GF " | toString L) else toString L))

-- TODO: this should take the same options as summands
summandsFromProjectors = method(Options => { Tries => 50 })
summandsFromProjectors Module := opts -> M -> (
    if degree M <= 1 then return {M};
    projs := try findProjectors(M, Tries => opts.Tries) else return {M};
    injs := apply(projs, pr -> inducedMap(M, ker pr));
    iota := matrix { injs };
    -- assert(0 == intersect apply(injs, image));
    -- assert same { numcols mingens M - numcols mingens coker iota,
    -- 	sum(projs, f -> numcols mingens kernel f),
    -- 	sum(injs,  f -> numcols mingens image  f) };
    L1 := flatten for pr in projs list (
	-- inducedMap(M, ker pr)
	N := prune ker pr;
	Ncomps := summandsFromProjectors(N, opts);
	-- TODO: Projection maps to the summands
	--c := -1;
	--p := inverse N.cache.pruningMap * inducedMap(ker pr, N, ??);
	--if #Ncomps > 1 then apply(#Ncomps, i -> M.cache#(symbol ^, [c += 1]) = N^[i] * p) else M.cache#(symbol ^, [c += 1]) = p;
	-- Inclusion maps from the summands
	-- TODO: will this always work?
	--scan(c + 1, i -> M.cache#(symbol _, [i]) = inverse M.cache#(symbol ^, [i]));
	Ncomps);
    N := prune coker iota;
    L2 := if N != 0 then summandsFromProjectors(N, opts) else {};
    join(L1, L2)
)

end--

restart
debug needsPackage "DirectSummands"
  -- ~2.2s
  R = ZZ/101[x,y,z]/(x^3, x^2*y, x*y^2, y^4, y^3*z)
  C = res(coker vars R, LengthLimit => 3)
  D = res(coker transpose C.dd_3, LengthLimit => 3)
  M = coker D.dd_3
  summands M
  elapsedTime profile summands M;
  profileSummary "DirectSum"
  elapsedTime assert(8 == #summands M)
  isIsomorphic(M, directSum summands M)
