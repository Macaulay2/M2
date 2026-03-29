-----------------------------------------------------------------------------
-- helpers for sheaf cohomology
-----------------------------------------------------------------------------

-- TODO: should this also check that the variety is finite type over the field?
checkVariety := (X, F) -> (
    if not X === variety F     then error "expected coherent sheaf over the same variety";
    if not isAffineRing ring X then error "expected a variety defined over a field";
    )

-- pushforward the complex to PP^n via S/I <-- S
flattenComplex = C -> C.cache#"flattenComplex" ??= (
    if instance(ring C, PolynomialRing) then return C;
    (lo, hi) := C.concentration;
    if lo === hi
    then complex(flattenModule C_lo, Base => lo)
    else complex applyValues(C.dd.map, flattenMorphism))

-- TODO: this is called twice
-- TODO: implement for multigraded ring
degreeList := M -> (
    -- gives the exponents of the numerator of reduced Hilbert series of M
    if dim M > 0 then error "expected module of finite length";
    R2 := ring M; -- This should be singly graded.
    deglist := flatten degrees R2;
    n := numgens R2;
    H := poincare M; -- The Hilbert series of M is (poincare M)/(product_i (1 - T^(a_i))),
    -- where a_0,...,a_(n-1) are the degrees of the generators of R2, as positive integers.
    T := (ring H)_0;
    H = H // fold(times, apply(n, i -> 1-T^(deglist#i)));
    exponents H / first)

-- quotienting by the local cohomology H_m^0(M) to "saturate" M
-- TODO: use irrelevant ideal here
killLocalH0 = M -> M.cache.TorsionFree ??= if (H0 := saturate(0*M)) == 0 then M else M / H0
-- We mainly remember M.cache.TorsionFree (as a quotient module of M) for use in SheafMaps.m2.

-- Given a CoherentSheaf F, defined by a graded module M over a positively graded algebra R2,
-- return the map from M to a possibly simpler R2-module N that represents the same sheaf F. We always simplify
-- at least to M/M_tors, and if an even "better" module has been cached, we return that.
-- We expect not to apply this function when F was defined as a twist (i.e., when F.cache.?Twist is true), to simplify caching.
-- The module N can be obtained by "target currentModuleMap F".
currentModuleMap = F -> (
    M := module F;
    R2 := ring M;
    if F.cache.?SaturationMap then F.cache.SaturationMap
    else
    if F.cache.?TorsionFreeMap then F.cache.TorsionFreeMap
    else (
	N0 := killLocalH0 M;
	N := minimalPresentation N0;
	F.cache.TorsionFreeBaseRing = if isPolynomialRing R2 then N else minimalPresentation flattenModule N;
	F.cache.TorsionFreeMap = (inverse N.cache.pruningMap) * inducedMap(N0, M)))
-- In this last step, if M is already torsion-free (but we have not checked that before),
-- we don't bother to keep literally the same module (in Macaulay2 terms). Indeed, all the cohomology functions
-- start with calling a "currentModule" program; so we will not have done significant earlier calculations with M.
-- And in some cases, our running minimalPresentation here may give a simpler description of the same module.

-- Given a CoherentSheaf F, defined by a graded module M over a positively graded algebra R2, a quotient
-- of a graded polynomial ring R1, return a possibly simpler R1-module N that represents the same sheaf F.
-- (It will always be the R1-module underlying an R2-module.) We always simplify at least to M/M_tors,
-- and if an even "better" module has been cached, we return that.
-- We expect not to apply this function when F was defined as a twist (i.e., when F.cache.?Twist is true), to simplify caching.
currentModuleBaseRing = F -> (
    M := module F;
    R2 := ring M;
    if F.cache.?SaturationBaseRing then F.cache.SaturationBaseRing
    else
    if F.cache.?TorsionFreeBaseRing then F.cache.TorsionFreeBaseRing
    else (
	N0 := killLocalH0 M;
	N := minimalPresentation N0;
	F.cache.TorsionFreeMap = (inverse N.cache.pruningMap) * inducedMap(N0, M);
	F.cache.TorsionFreeBaseRing = if isPolynomialRing R2 then N else minimalPresentation flattenModule N))
-- In this last step, if M is already torsion-free (but we have not checked that before),
-- we don't bother to keep literally the same module (in Macaulay2 terms). Indeed, all the cohomology functions
-- start with calling a "currentModule" program; so we will not have done significant earlier calculations with M.
-- And in some cases, our running minimalPresentation here may give a simpler description of the same module.

-- This function "twistedGlobalSectionsModule" returns a sequence (b0, b1, M0) as output,
-- meaning that M0 is a module that maps to H^0(X, F(*)),
-- the map is an isomorphism in degrees at least b0, and it is surjective in degrees at least b1.
-- (These bounds need not be optimal.) Here F can be a coherent sheaf on a closed subspace
-- of a weighted projective space.
--
-- TODO: add tests:
-- - global sections of sheafHom are Hom
-- TODO: implement for multigraded ring using emsbound
-- TODO: this can change F.module to the result!
-- NOTE: this may have elements in degrees below bound as well, is that a bug?
twistedGlobalSectionsModule = (F, bound) -> (
    -- compute global sections module Gamma_(d >= bound)(X, F(d)), for a CoherentSheaf F.
    -- But if H^0(X, F(*)) is bounded below, then return the complete answer, regardless of "bound".
    A := assertWeightedZZGraded ring variety F;
    -- FIXME: this line, as opposed to
    --  cokernel presentation module F
    -- breaks the test added in 975d780470.
    -- However, we need to keep the information
    -- cached in M, for instance if M is a Hom module.
    M := module F;
    quot := currentModuleMap F; -- The map from M to a simplified R2-module N (at least simplified to M/M_tors).
    N := target quot;
    N' := currentModuleBaseRing F; -- This is N as an R1-module.
    S := ring N'; -- This is a graded polynomial ring.
    degs := flatten degrees S; -- This is a list of the form {1,9,15,22}, say.
    n := #degs; -- So P = Proj S has dimension n-1.
    sumOfWeights := fold(plus, degs); -- This is sum_i |x_i|, where S = k[x_0,...,x_(n-1)].
    S.cache ??= new MutableHashTable;
    w := S.cache.Dualizing ??= S^{-sumOfWeights};
    -- We fix the dualizing module w, as a graded S-module. As a result, Macaulay2 automatically remembers Ext^j(M, w)
    -- (for a number j), or more precisely Ext^j_S(N', w), in case another function has computed that module earlier.
    --
    -- Note: bound=infinity may mean that HH^1_m(N) = 0, i.e., N is saturated,
    -- or just that the user does not want to search for any global sections besides those in N.
    -- TODO: what would pdim N' < n-1, hence E1 = 0, imply?
    complete := false;
    p := 0;
    if bound < infinity then (
	p = if pdim N' < n-1 then (
	    complete = true;
	    0)
	else (
	    E1 := Ext^(n-1)(N', w); -- This is the graded dual of the local cohomology HH^1_m(N').
	    -- For example, if N' = M/M_tors, then this is isomorphic to HH^1_m(M).
	    if dim E1 <= 0 then ( -- 0-module or 0-dim module (i.e., finite length))
		complete = true;
		degList := degreeList E1;
		1 + max degList - min degList
		)
	    else 1 - (first min exponents poincare E1) - bound
	    -- Note that "first min degrees E1" would give the smallest degree of a generator for E1,
	    -- even if that generator is killed by the relations. Instead, we use poincare E1 to find the smallest degree
	    -- in which E1 is not zero. For example, if poincare E1 = 5T^(-1)+T^2, then exponents poincare E_1 = {{-1},{2}},
	    -- hence the need for the "first".
	    )
	);
    -- this can only happen if bound=-infinity, that is, from calling H^0(F(*)) = H^0(F(>=(-infinity))
    if p === infinity then error "the global sections module is not finitely generated";
    -- caching these to be used later in prune SheafMap
    F.cache.TorsionFree = M.cache.TorsionFree;
    -- We mainly remember F.cache.TorsionFree (as a quotient module of M) for use in SheafMaps.m2.
    F.cache.GlobalSectionLimit = max(0, p);
    -- this is the module Gamma_* M, as a module over the original ring A.
    if p <= 0 then (
	F.cache.SaturationMap = quot; -- The map from M to the simplified A-module N. Since we keep the same module N
	-- in this case, Macaulay2 remembers any earlier calculations done for N, such as Ext calculations.
	F.cache.SaturationBaseRing = N'; -- This is N as an S-module.
	return if complete then (-infinity, -infinity, N)
	else (p + bound, p + bound, N));
    G := minimalPresentation target(
	-- TODO: substitute with appropriate irrelevant ideal here
	-- TODO: separate as a helper function
	M2gens := apply(n, i -> ((S_i)^-((-p)//degs#i)));
	-- That is, the list M2gens consists of each variable x_i to the power roundup(p/a_i).
	BpS := ideal M2gens; -- This is the ideal Ip of the form (x_0^(b_0),x_1^(b_1),...,x_(n-1)^(b_(n-1)))
	-- in the polynomial ring S, viewed as an S-module. In practice, considering this ideal
	-- in S seemed to make this function faster than considering the analogous ideal in the quotient ring A.
	incS := inducedMap(module S, module BpS);
	inc := incS ** A; -- This is the A-linear map  Ip tensor_S A -> A. (So A-linear maps from the first module
	-- to N (which may be M/M_tors) are equivalent to S-linear maps from Ip to N' = (N as an S-module).)
	iso := inducedMap(Hom(A, N), N);
	-- we compute the map N -> Gamma_* F as a limit by
	-- applying Hom(-, N) to the sequence above
	-- cf. the limit from minimalPresentation hook
	-- and emsbound in NormalToricVarieties/Sheaves.m2
	phi := Hom(inc, N, MinimalGenerators => true) * iso);
    -- Note that phi: N -> G is always injective, because we have arranged for N to be m-torsion-free.
    -- If phi is surjective, hence an isomorphism, then we will take the output to be the original module N rather than G,
    -- so that Macaulay2 remembers any earlier calculations done for N, such as Ext calculations.
    if isSurjective phi then (
	F.cache.SaturationMap = quot; -- The map from M to the simplified A-module N.
	F.cache.SaturationBaseRing = N';
	return if complete then (-infinity, -infinity, N)
	else (bound, bound, N));
    -- now we compute the center map in the sequence, where m is the maximal ideal of S:
    -- 0 -> HH^0_m(M) -> M -> Gamma_* M -> HH^1_m(M) -> 0
    iota := inverse G.cache.pruningMap; -- map from Gamma_* M to its minimal presentation
    -- quot is the map from M to N (which may be M/M_tors).
    F.cache.SaturationMap = if p <= 0 then iota * quot else iota * phi * quot;
    F.cache.SaturationBaseRing = minimalPresentation flattenModule G;
    if complete then (-infinity, -infinity, G)
    else (bound, bound, G))


-----------------------------------------------------------------------------
-- cohomology
-----------------------------------------------------------------------------
-- TODO: add hooks for X not finite type over k?

-- HH^p(X, OO_X)
cohomology(ZZ,          SheafOfRings) := Module => opts -> (p,    O) -> cohomology(p, variety O, O^1, opts)
cohomology(ZZ, Variety, SheafOfRings) := Module => opts -> (p, X, O) -> cohomology(p,         X, O^1, opts)

-- HH^p(X, F(>=b))
--
-- The function HH^p(F(>=b)) computes the cohomology of a coherent sheaf F on a closed subspace X
-- of a weighted projective space with all twists at least a given number,
-- as a module. If you just want the Hilbert series of the cohomology,
-- hh^p(F, b1, b2) or hh^p(F(*)) will probably be faster. The base ring of the output module
-- will be the positively graded ring corresponding to X, not necessarily a graded polynomial ring.
-- The function prints to say whether the output is complete or not, unless you use the "NonPrint" options below.
--
-- The base ring should be a field. Note that it is usually faster
-- to work over Z/p for a prime number p <= 32767, say ZZ/31991, rather than over Q.
--
-- The distinction between a WPS as a stack X and its associated coarse moduli space, f: X -> V, does not matter
-- for computing cohomology. Indeed, a finitely generated graded module M determines a coherent sheaf M~
-- on X, and it is a fact that H^i(X, M~) = H^i(V, f_*(M~)) for every i. Twists are interpreted by tensoring with
-- the line bundles O(c) on the stack, which ensures that H^i(X,M~(c)) = H^i(X,M(c)~).
--
-- In more detail, HH^p(F(>=b)) computes a graded module M0 that maps to H^p(X, F(*)) in all degrees.
-- The map will be an isomorphism in degrees at least b. But if H^p(X, F(*)) is bounded below,
-- then M0 will be isomorphic to H^p(X, F(*)) in all degrees, and Macaulay2 will say so.
-- In the case of H^0, there will be a map from M to the output module M0, and the map is stored
-- as F.cache.SaturationMap.
--
-- This function uses "localCohomology", which in turn uses local duality (keeping track of the module structure).
-- We only reuse an earlier cached calculation when the earlier calculation computed cohomology in all degrees.
-- Otherwise, the user might want to run the program again
-- with different inputs (e.g., HH^2(F(>=-3)) is in principle determined by HH^2(F(>=-10)),
-- but it would give a simpler module, which might be preferable). Also, we try to fix the modules involved,
-- so that Macaulay2 automatically remembers calculations such as Ext made with those modules.
--
-- The option HH^p(F(>=b), Degree => Direct) uses a direct method, rather than local duality.
-- The two algorithms are similar for H^0, but for H^i with i>0, there can be big differences in speed.
--
-- Some other options are "Degree => NonPrint" or "Degree => DirectNonPrint", which avoid printing to the screen
-- and instead return a sequence (b0, b1, M0) as output, meaning that M0 is a module that maps to H^i(X, F(*)),
-- the map is an isomorphism in degrees at least b0, and it is surjective in degrees at least b1. (These bounds
-- need not be optimal.) We recommend using those options when calling this function from another function.
--
-*
cohomology(ZZ,                    SumOfTwists) := Module => opts -> (p,    S) -> cohomology(p, variety S, S, opts)
cohomology(ZZ, ProjectiveVariety, SumOfTwists) := Module => opts -> (p, X, S) -> (
    checkVariety(X, S);
    (F, b) := (S#0, S#1#0);
    F.cache.HH        ??= new MutableHashTable;
    -- TODO: when p>0, HH^p(F(*)) gives a "not implemented yet" error
    F.cache.HH#(p, b) ??= if p == 0
    then twistedGlobalSectionsModule(F, b)
    else HH^(p+1)(module F, Degree => b))
*-
cohomology(ZZ, ProjectiveVariety, SumOfTwists) := Module => opts -> (p, X, S) -> (
    checkVariety(X, S);
    cohomology(p, S, opts))
cohomology(ZZ, SumOfTwists) := Module => opts -> (p, S) -> (
    (F, b) := (S#0, S#1#0);
    if F.cache.?Twist then ( -- Here F was defined as a twist of another sheaf, say F = E(shift). We reduce to the calculation for E.
	shift := first F.cache.Twist#0; -- Here F.cache.Twist#0 should be a degree in the form {3}, and then shift would be 3.
	if opts.Degree === Direct or opts.Degree === DirectNonPrint then
	originalseq := HH^p(F.cache.Twist#1(>= b + shift), Degree => DirectNonPrint)
	else originalseq = HH^p(F.cache.Twist#1(>= b + shift), Degree => NonPrint);
	b0 := -shift + originalseq#0; b1 := -shift + originalseq#1;
	if p == 0 then F.cache.GlobalSectionLimit = (F.cache.Twist#1).cache.GlobalSectionLimit;
	-- We record this number for use in SheafMaps.m2.
	output := (originalseq#2)(shift))
    else ( -- Now the sheaf F was not defined as a twist.
	F.cache.Hsum        ??= new MutableHashTable;
	-- TODO: when p>0, HH^p(F(*)) gives a "not implemented yet" error
	--
	-- We store as F.cache.Hsum#p a sequence (b0, b1, output), meaning that output is a module that maps to H^p(X,F(*)),
	-- isomorphically in degrees at least b0 and surjectively in degrees at least b1.
	-- Moreover, b0 is -infinity if and only if H^p(X,F(*)) is bounded below,
	-- and in that case, output is all of H^p(X,F(*)).
	if F.cache.Hsum#?p and ((b0 = F.cache.Hsum#p#0) === -infinity or b == b0) then (b0, b1, output) = F.cache.Hsum#p
	-- If we cached a complete answer, or if we are computing in exactly the same range as before, then we use the cached answer.
	else ( -- Otherwise, we compute the answer, even if b0 < b, meaning that we have done a more complete calculation earlier.
	    -- The point is that computing the answer in a smaller range is often faster than truncating the earlier answer,
	    -- and it typically gives a simpler module than just repeating the earlier answer.
	    if opts.Degree === Direct or opts.Degree === DirectNonPrint then (b0, b1, output) = cohomologyDirect(p, S, Print => false)
	    else if p == 0 then (b0, b1, output) = twistedGlobalSectionsModule(F, b)
	    else (b0, b1, output) = localCohomology(p+1, F, b);
	    -- If H^p(X,F(*)) is bounded below,
	    -- then the output is the whole cohomology, regardless of the given bound "twist".
	    --
	    -- Here we compute local cohomology over the base ring of the sheaf F, which need not be a polynomial ring.
	    -- So the function localCohomology (like Macaulay2's HH^* function)
	    -- lifts that module to one over a polynomial ring, rather than using our cached module. Nonetheless,
	    -- that function seems to be fast.
	    if (not F.cache.Hsum#?p) or b0 < F.cache.Hsum#p#0 then F.cache.Hsum#p = (b0, b1, output) -- If we did not have a cached answer,
	    -- or if we have computed in a bigger range now, we cache our new answer.
	    )
	); -- Now we report the answer, whether F was defined as a twist or not.
    if opts.Degree === NonPrint or opts.Degree === DirectNonPrint then return (b0, b1, output);
    if b0 === infinity then (<< "The following module is correct in sufficiently high degrees.")
    else if b0 === -infinity then (<< "The following module is correct in all degrees.")
    else if b0 == b1 then (<< "The following module is correct in degrees at least " << b0 << ".")
    else if b1 === -infinity then (<< "The following module maps isomorphically to the cohomology in degrees >= " << b0
	<< " and surjectively in all degrees.")
    else (<< "The following module maps isomorphically to the cohomology in degrees >= " << b0
	<< " and surjectively in degrees >= " << b1 << ".");
    output);

-- Alternate version 2 of HH^i(F(>=b)), "cohomologyDirect". This can be called by the usual cohomology function,
-- for example by "HH^i(F(>=b), Degree => Direct)". Eventually, "cohomology" should allow more options,
-- so that we can use a better notation such as "HH^i(F(>=b), Strategy => Direct)".
-- In any case, when i>0, this option uses a completely different algorithm,
-- based on Ext from an ideal of the form (x_0^(b_0),x_1^(b_1),...x_(n-1)^(b_(n-1))), rather than using local duality.
-- This can be much faster or much slower than the main function HH^i(F(>=b)); so both options may be worth a try.
-- One reason for making the other algorithm the main one is that it gives
-- the complete answer when the cohomology is bounded below. This function only returns a module
-- which is correct in at least the given degree b (and it prints that fact).
--
-- One choice for speed in this function is that it uses the complete intersection ideal
-- above, rather than the truncations R_(>=j) which have more complicated free resolutions. This function also uses
-- a bound based on the minimal free resolution of M, which is sharper than just using the regularity of M.
--
-- The distinction between a weighted projective space as a stack X and its associated coarse moduli space, f: X -> V,
-- does not matter for computing cohomology. Indeed, a finitely generated graded module M determines a coherent sheaf M~
-- on X, and it is a fact that H^i(X, M~) = H^i(V, f_*(M~)) for every i. Twists are interpreted by tensoring with
-- the line bundles O(c) on the stack, which ensures that H^i(X,M~(c)) = H^i(X,M(c)~).
--
-- This function takes more time if you ask for a larger range of twists, even if the cohomology is zero
-- in part of that range. It might be reasonable to first run hh^i(F*)), which computes
-- the Hilbert series of cohomology in all degrees, and then (if you want cohomology as a module)
-- apply this function starting in the lowest degree where the cohomology is not zero.
--
-- An option is "Print => false", which avoids printing to the screen
-- and instead returns a sequence (b0, b1, M0) as output, meaning that M0 is a module that maps to H^i(X, F(*)),
-- the map is an isomorphism in degrees at least b0, and it is surjective in degrees at least b1. (These bounds
-- need not be optimal.) That option is recommended when this function is called
-- by another function, rather than by the user.
--
cohomologyDirect = {Print => true} >> opts -> (cohodeg, S) -> (
    -- Here S should be a SumOfTwists, as in "cohomologyDirect(1,F(>=0))".
    (F, twist) := (S#0, S#1#0);
    R2 := ring module F;
    quot := currentModuleMap F; -- The map from the original R2-module M to a simplified module N (at least simplified to M/M_tors).
    N := target quot;
    N' := currentModuleBaseRing F;
    R1 := assertWeightedZZGraded ring N'; -- R1 is a graded polynomial ring, and N' is N as an R1-module.
    -- In particular, N' is m-torsion-free, where m is the maximal ideal of R1.
    output := 0;
    degs := flatten degrees R1; -- This is a list of the form {1,9,15,22}, say.
    n := #degs; -- So P = Proj R1 has dimension n-1.
    sumOfWeights := fold(plus, degs);
    -- Thus sumOfWeights = sum_i |x_i|, where R1 = k[x_0,...,x_(n-1)].
    bettitable := betti res(N', LengthLimit => n-cohodeg); -- The Betti numbers of the minimal resolution ... -> F_1 -> F_0 -> N' -> 0,
    -- correct out to F_{n-cohodeg}.
    nonzerokeys := select(keys bettitable, k -> bettitable#k != 0);
    relevantpart0 := select(nonzerokeys, (i,d,h) -> (i == n-cohodeg));
    relevantpart1 := select(nonzerokeys, (i,d,h) -> (i == n-cohodeg-1));
    maxdeg0 := max apply(relevantpart0, (i,d,h) -> h); -- The maximum degree of a generator of F_{n-cohodeg}.
    maxdeg1 := max apply(relevantpart1, (i,d,h) -> h); -- The maximum degree of a generator of F_{n-cohodeg-1}.
    maxdeg := max(maxdeg0, maxdeg1); -- The maximum degree of a generator of F_{n-cohodeg} or F_{n-cohodeg-1}.
    -- It is -infinity if those two free modules are zero.
    -- Then the map Ext^c_R1(M2,N')_a -> H^c(X, F(a)) is an isomorphism for all a >= twist, for any homogeneous ideal M2 in R1 that lives
    -- in degrees > maxdeg - sumOfWeights - twist and that contains a power of the irrelevant ideal. So, define j by:
    -- maxdeg+1-sumOfWeights-twist;
    -- Previous versions took j to be reg_{Symonds}(N') + 1 - cohodeg - twist = reg_{Macaulay2}(N') - sumOfWeights + n + 1 - cohodeg - twist,
    -- which gave a weaker result.
    -- Also, for this choice of j and hence M2, the map from Ext^c to H^c is surjective
    -- in degrees a >= twist - (maxdeg-maxdeg1). That adds information if maxdeg1 < maxdeg0.
    -- Finally, if cohodeg = 0, then (because N is m-torsion-free) the map from Ext^c to H^c is injective in all degrees.
    -- So, in this case, we can instead take j = maxdeg1+1-sumOfWeights-twist, and then the map is surjective (hence an isomorphism)
    -- in degrees >= twist.
    j := 0;
    if cohodeg == 0 then j = maxdeg1 + 1 - sumOfWeights - twist
    else j = maxdeg + 1 - sumOfWeights - twist;
    --
    -- We need to construct a homogeneous ideal M2 in the graded polynomial ring R1 that is concentrated
    -- in degrees at least j and that contains a power of the irrelevant ideal (x_0,...,x_(n-1)). The following
    -- seems like the best choice for efficiency.
    if j <= 0 then (
	-- In this case, we can take M2 = R1, which gives the following results.
	if cohodeg == 0 then output = N
	-- output = truncate(twist, N);
	-- One might prefer to truncate at twist. As it is, we output a module that is only known
	-- to be correct in degrees at least twist. But this seems OK, especially since we print an explanation.
	else output = R2^0  -- This is zero, as an R2-module.
	)
    else (
	-- Here j > 0.
	M2gens := apply(n, i -> ((R1_i)^-((-j)//degs#i)));
	-- That is, the list M2gens consists of each variable x_i to the power b_i := roundup(j/a_i).
	M2matrix := matrix {M2gens};
	if cohodeg == 0 then (
	    -- If cohodeg = 0, then we know that N < output < H^0(X, F(*)). Thus the output is always
	    -- at least as good an approximation to H^0(X, F(*)) as N was, and so we will cache it (unless it is equal to N).
	    M2 := image M2matrix; -- This is an ideal in R1, viewed as a module.
	    incR1 := inducedMap(module R1, M2);
	    inc := incR1 ** R2; -- This is the R2-linear map  M2 tensor_R1 R2 -> R2. (So R2-linear maps from the first module
	    -- to N are equivalent to R1-linear maps from M2 to N' (which is N as an R1-module).)
	    iso := inducedMap(Hom(R2, N), N);
	    -- we compute the map N -> Gamma_* F as a limit by
	    -- applying Hom(-, N) to the sequence above
	    phi := Hom(inc, N, MinimalGenerators => true) * iso; -- Thus phi: N -> G = Hom_R1(M2, N'), viewed as an R2-module.
	    -- Note that phi: N -> G is always injective, because we have arranged for N to be m-torsion-free.
	    -- If phi is surjective, hence an isomorphism, then we will take the output to be the original module N rather than G,
	    -- so that Macaulay2 remembers any earlier calculations done for N,\ such as Ext calculations.
	    if isSurjective phi then output = N
	    else (
		--output = minimalPresentation target phi; -- This is Hom_R1(M2, N'), viewed as an R2-module. --DELETE.
		output = target phi; -- This is Hom_R1(M2, N'), viewed as an R2-module. It may be given as a subquotient, which seems fine.
		-- output = truncate(twist, M3);
		-- One might prefer to truncate at twist. As it is, we output a module that is only known
		-- to be correct in degrees at least twist. But this seems OK, especially since we print an explanation.
		iota := inverse output.cache.pruningMap; -- the map from G to its minimal presentation, output.
		-- quot is the map from M to N (which may be M/M_tors).
		F.cache.SaturationMap = iota * phi * quot;
		F.cache.SaturationBaseRing = minimalPresentation flattenModule output)
	    )
	else (
	    -- Following the advice of the Macaulay2 documentation, for c = cohodeg > 0,
	    -- we compute the graded module Ext^{c+1}(R1/M2,N'), rather than Ext^c(M2,N') (which is isomorphic).
	    --
	    -- Also, since we take M2 to be the complete intersection ideal M2=(x_0^b_0,...,x_(n-1)^b_(n-1)), the minimal free resolution
	    -- of R1/M2 is a Koszul complex, which is self-dual; so Ext^{c+1}_{R1}(R1/M2, N') = Tor_{n-c-1}^{R1}(R1/M2, N')(sum |x_i^b_i|),
	    -- keeping track of the grading. That avoids dualizing and hence should be faster.
	    -- Furthermore, since we compute Tor by resolving R1/M2, it is manifestly computed by a complex of R2-modules
	    -- (using N in place of N'). That avoids having to tensor with R2 at the end of the calculation, which can be slow,
	    -- because Macaulay2 would have to convert the Ext module from a subquotient module to a quotient.
	    M2matrixR2 := M2matrix ** R2; -- This is the row matrix (x_0^b_0,...,x_(n-1)^b_(n-1)) over R2.
	    K := koszulComplex(M2matrixR2, Concentration => (n-cohodeg-2, n-cohodeg));
	    Torshift := fold(plus, apply(M2gens, i -> first degree i)); -- This is sum |x_i^b_i|.
	    output = R2^{Torshift} ** HH_(n-cohodeg-1)(K ** N);
	    -- output = truncate(twist, output);
	    -- One might prefer to truncate at twist. As it is, we output a module that is only known
	    -- to be correct in degrees at least twist. But this seems OK, especially since we print an explanation.
	    -- Truncating can be slow.
	    );
	);
    -- For c = cohodeg > 0, the map from the output module to H^c(X,F(*)) is an isomorphism in degrees >= twist+min(j,0)
    -- and surjective in degrees >= twist-(maxdeg-maxdeg1)+min(j,0). Moreover, when cohodeg > 0 and j <= 0,
    -- the output module is 0, and so surjectivity implies isomorphism in that case.
    -- For cohodeg = 0, the map from the output module to H^0(X,F(*)) is always injective, and it is surjective
    -- (hence an isomorphism) in degrees >= twist+min(j,0).
    -- We draw the following conclusions.
    if not opts.Print then return (
	-- When Print => false, we return a sequence (b, c, M0), meaning that M0 is a module that maps to H^cohodeg(X, F(*)),
	-- the map is an isomorphism in degrees at least b, and it is surjective in degrees at least c.
	if j === -infinity or (cohodeg > 0 and j <= 0 and maxdeg1 === -infinity) then (-infinity, -infinity, output)
	else (
	    if maxdeg1 >= maxdeg0 or cohodeg == 0 then (twist + min(j,0), twist + min(j,0), output)
	    else ( -- Now we in particular have cohodeg > 0.
		if maxdeg1 === -infinity then (twist + min(j,0), -infinity, output)
		else (twist + min(j,0), twist - (maxdeg - maxdeg1) + min(j,0), output))));
    if j === -infinity or (cohodeg > 0 and j <= 0 and maxdeg1 === -infinity) then (
	<< "The following module is correct in all degrees.")
    else (
	if maxdeg1 >= maxdeg0 or cohodeg == 0 then (
	    << "The following module is correct in degrees >= " << twist + min(j,0) << ".")
	else ( -- Now we in particular have cohodeg > 0.
	    if maxdeg1 === -infinity then (
		<< "The following module maps isomorphically to the cohomology in degrees >= " << twist + min(j,0)
		<< " and surjectively in all degrees.")
	    else (
		<< "The following module maps isomorphically to the cohomology in degrees >= " << twist + min(j,0)
		<< " and surjectively in degrees >= " << twist - (maxdeg - maxdeg1) + min(j,0) << ".")
	    )
	);
    output)

-- HH^p(X, F)
--
-- The base ring should be a field. Note that it is usually faster
-- to work over Z/p for a prime number p <= 32767, say ZZ/31991, rather than over Q.
--
-- As well as caching individual cohomology groups, we try to fix the modules involved,
-- so that Macaulay2 automatically remembers Ext calculations done with them.
cohomology(ZZ,                    CoherentSheaf) := Module => opts -> (p,    F) -> cohomology(p, variety F, F, opts)
cohomology(ZZ,     AffineVariety, CoherentSheaf) := Module => opts -> (p, X, F) -> (
    checkVariety(X, F);
    if p == 0 then module F else (ring X)^0)
cohomology(ZZ, ProjectiveVariety, CoherentSheaf) := Module => opts -> (p, X, F) -> (
    -*
    checkVariety(X, F);
    F.cache.HH   ??= new MutableHashTable;
    if F.cache.HH#?p then return F.cache.HH#p;
    -- TODO: only need basis(0, G) in the end, is this too much computation?
    G := if p == 0 then twistedGlobalSectionsModule(F, 0) -- HH^0 F(>=0)
    else (
	-- pushforward F to PP^n
	M := flattenModule module F;
	S := ring M;
	-- TODO: both n and w need to be adjusted for the multigraded case
	n := dim S-1;
	w := S^{-n-1};
	-- using Serre duality for coherent sheaves on schemes with mild
	-- singularities, Cohen–Macaulay schemes, not just smooth schemes.
	-- TODO: check that X is proper (or at least finite type)
	Ext^(n-p)(M, w));
    k := coefficientRing ring X;
    F.cache.HH#p = k^(rank source basis(0, G)))
    *-
    checkVariety(X, F);
    F.cache.HH   ??= new MutableHashTable;
    if F.cache.HH#?p then return F.cache.HH#p;
    k := coefficientRing ring variety F;
    F.cache.HH#p = k^(hh^p(F)))

-- HH^p(OO_X)
cohomology(ZZ,          SheafOfRings) := Module => opts -> (p,    O) -> cohomology(p, O^1, opts)
cohomology(ZZ, Variety, SheafOfRings) := Module => opts -> (p, X, O) -> cohomology(p, X, O^1, opts)

-- This function "localCohomology" returns a sequence (b0, b1, M0),
-- meaning that M0 is a module that maps to H^i(X, F(*)) (with i > 0),
-- the map is an isomorphism in degrees at least b0 (with b0 <= the input b),
-- and the map is surjective in degrees at least b1.
-- (These bounds need not be optimal.) Here F can be a coherent sheaf on a closed subspace
-- of a weighted projective space.
--
-- This is essentially Macaulay2's function for local cohomology from local.m2, adjusted to interpret
-- the truncation option correctly when the ring has generators in several degrees.
--
localCohomology = (i, F, b) -> (
    -- this is local cohomology for the maximal ideal
    if b == -infinity then error "not implemented yet";
    R2 := ring module F;
    M := currentModuleBaseRing F;
    R1 := ring M; -- R1 is a graded polynomial ring, and M is a simplified R1-module that represents the sheaf F.
    degs := degrees R1;
    n := #degs; -- So P = Proj R1 has dimension n-1.
    sumOfWeights := first fold(plus, degs);
    -- Thus sumOfWeights = sum_i |x_i|, where R1 = k[x_0,...,x_(n-1)].
    -- Note that degrees R1 should be a list of the form {{1},{9},{15},{22}}, hence the "first".
    --
    -- Macaulay2's function for local cohomology (corrected below)
    -- gives accurate answers even if R has generators in different degrees;
    -- but the degree from which it starts computing is off by sigma = sumOfWeights - n,
    -- which explains the change made here to the definition of the dualizing module ww.
    -- If H^i(X, F(*)) is bounded below, however, then that is irrelevant;
    -- the output is the whole cohomology, regardless of the given bound "b".
    R1.cache ??= new MutableHashTable;
    ww := R1.cache.Dualizing ??= R1^{-sumOfWeights};
    -- We fix the dualizing module ww, as a graded R-module. As a result, Macaulay2 automatically remembers Ext^(n-i)(M, ww),
    -- in case another function has computed that module earlier.
    E := minimalPresentation Ext^(n-i)(M,ww);
    complete := false;
    result := if dim E <= 0 then (
	complete = true;
	Ext^n(E, ww))
    else (
	topdegree := -(first min degrees E); -- In this case, H^i(X, F(*)) is unbounded below,
	-- and topdegree is the top degree in which it is not zero.
	if b > topdegree then return (topdegree + 1, topdegree + 1, R2^0);
	truncatedDual(E, b, sumOfWeights));
    -- output = minimalPresentation (result ** R2); -- DELETE. (We don't need to return a minimal presentation.)
    output := result ** R2; -- This can be somewhat slow, because computing the tensor product requires Macaulay2
    -- to find a presentation of the R1-module "result" (which will usually be given as a subquotient). This happens even though
    -- "result" is already the R1-module underlying an R2-module. At least Macaulay2's tensor product algorithm
    -- avoids finding a presentation in the case when R1 = R2.
    if complete then (-infinity, -infinity, output)
    else (b, b, output))

-- Given a graded module M over a graded polynomial ring, return a graded module
-- that agrees with the k-dual of M in degrees >= -e. For speed,
-- the output may be nonzero in degrees less than e. A precursor of this command
-- is in Macaulay2's file "local.m2". The polynomial ring may have generators in different degrees.
-- For convenience, we include the sum of the weights of the ring as a third input.
truncatedDual = (M, e, sumOfWeights) -> (
    -- find (k-dual M), truncated in degrees >= e.
    -- depends on truncate methods
    R := ring M;
    n := numgens R;
    degs := flatten degrees R; -- This is a list of the form {1,9,15,22}, say.
    R.cache ??= new MutableHashTable;
    ww := R.cache.Dualizing ??= R^{-sumOfWeights};
    -- We will define M1 to be a quotient module of M that has finite length and that agrees with M in degrees at most -e.
    degList := flatten degrees M; -- The degrees of the generators of M, in the form {2,3,...}.
    gensM := generators M; sourceM := source gensM;
    gensToKill := positions(degList, i -> (i > -e)); -- We will kill these generators in M1.
    gensToKeep := positions(degList, i -> (i <= -e));
    rels1 := apply(gensToKill, i -> M_i);
    rels2 := flatten apply(gensToKeep, i -> apply(n, j -> ((R_j)^-((degList#i - 1 + e)//degs#j)*(M_i))));
    -- That is, multiply each generator of M with degree d_i <= -e by each variable x_j to the power roundup((-e+1-d_i)/a_j).
    rels12 := rels1 | rels2;
    M1 := minimalPresentation (M / image map(M, , matrix rels12)); -- At the moment, just writing M/rels12 would typically not
    -- be recognized by Macaulay2 as being homogeneous, hence this more complex formulation.
    Ext^n(M1,ww))

-----------------------------------------------------------------------------
-- Module of twisted global sections Γ_*(F)
-----------------------------------------------------------------------------

-- This is an approximation of Gamma_* F, at least with an inclusion from Gamma_>=0 F
-- TODO: optimize caching: if HH^0(F(>=b)) is cached above, does this need to be cached?
-- TODO: should F>=0 be hardcoded? I think this is OK, especially since the function HH^0(F(>=b))
-- returns all of H^0(X,F(*)) if that is bounded below.
minimalPresentation SheafOfRings  := prune SheafOfRings  := SheafOfRings  => opts -> identity
minimalPresentation CoherentSheaf := prune CoherentSheaf := CoherentSheaf => opts -> (
    F -> F.cache#(symbol minimalPresentation => opts) ??= tryHooks(
	(minimalPresentation, CoherentSheaf), (opts, F), (opts, F) -> (
	    if not isProjective variety F then return sheaf minimalPresentation module F;
	    -- That handles a sheaf on an affine variety.
	    if F.cache.?Twist then (-- Here F was defined as a twist of another sheaf, say F = E(shift). We reduce to the calculation for E.
		shift := first F.cache.Twist#0; -- Here F.cache.Twist#0 should be a degree in the form {3}, and then shift would be 3.
		H := minimalPresentation F.cache.Twist#1; -- Here F.cache.Twist#1 is the original sheaf E.
		Gmap := (H.cache.pruningMap)(shift);
		-- We mainly record F.cache.TorsionFree (as a quotient of F.module) for use in SheafMaps.m2.
		F.cache.TorsionFree = ((F.cache.Twist#1).cache.TorsionFree)(shift);
	        F.cache.GlobalSectionLimit = -shift + (F.cache.Twist#1).cache.GlobalSectionLimit)
	    -- Now F was not defined as a twist. This is the default algorithm.
	    else (
		if not F.cache.?SaturationMap then HH^0(F(>=0), Degree => NonPrint);
		Gmap = sheaf(F.variety, F.cache.SaturationMap));
	    G := target Gmap;
	    G.cache.pruningMap = Gmap;
	    G)))

-----------------------------------------------------------------------------
-- Projective bundles
-----------------------------------------------------------------------------
-- TODO: add isVectorSpace, then given a vector space V with basis elements
-- V_1 .. V_n support defining PP(V) = Proj Sym V = Proj kk[V_1..V_n].

-- TODO: is this correct?
symmetricAlgebra CoherentSheaf := Ring => opts -> F -> symmetricAlgebra(HH^0 F(>=0), opts)
-- TODO: is the dual right?
-- TODO: add isLocallyFree and make sure F is locally free first?
ProjectiveSpace(CoherentSheaf) := ProjectiveVariety => F -> tryHooks((ProjectiveSpace, CoherentSheaf), F,
    F -> Proj flattenRing(symmetricAlgebra dual F, Result => Thing))

-----------------------------------------------------------------------------
-- Sheaf Hom and Ext
-----------------------------------------------------------------------------

sheafHom = method(TypicalValue => CoherentSheaf, Options => options Hom)
sheafHom(SheafOfRings, SheafOfRings)  :=
sheafHom(SheafOfRings, CoherentSheaf) :=
sheafHom(CoherentSheaf, SheafOfRings)  :=
sheafHom(CoherentSheaf, CoherentSheaf) := CoherentSheaf => opts -> (F, G) -> (
    assertSameVariety(F, G); sheaf(variety F, Hom(module F, module G, opts)))

sheafExt = new ScriptedFunctor from {
    superscript => i -> new ScriptedFunctor from {
	-- sheafExt^1(F, G)
	argument => X -> applyMethod''(sheafExt, functorArgs(i, X))
	},
    argument => X -> applyMethod''(sheafExt, X)
    }

sheafExt(ZZ, SheafOfRings, SheafOfRings)  :=
sheafExt(ZZ, SheafOfRings, CoherentSheaf) :=
sheafExt(ZZ, CoherentSheaf, SheafOfRings)  :=
sheafExt(ZZ, CoherentSheaf, CoherentSheaf) := CoherentSheaf => options Ext.argument >> opts -> (i, F, G) -> (
    assertSameVariety(F, G); sheaf(variety F, Ext^i(module F, module G, opts)))

-----------------------------------------------------------------------------
-- Global Ext via efficient truncation.
-- We build on ideas from the algorithm in:
-- Gregory G. Smith, Computing global extension modules,
-- Journal of Symbolic Computation, 29 (2000) 729-746.
-- See documentation of Ext^ZZ(CoherentSheaf,CoherentSheaf) for examples.
-----------------------------------------------------------------------------

-- Given an integer m, a complex D of graded modules over a singly graded ring R2 (a quotient
-- of a polynomial ring R1) and an integer b, return an integer e (or -infinity)
-- such that: if B is any ideal in the polynomial ring R1 such that B contains R1 in sufficiently high degrees
-- and B is in graded degree >= e, then the map
--   Ext^m_R1(B, D)_{>=b} -> H^m(X, D~(>=b))
-- is an isomorphism. Here X = Proj R2. The function returns a sequence (b0, b1, e),
-- meaning that the output of Ext^m(F, D~(>=b)) (using the number e) will be a module that maps isomorphically
-- to Ext^m_X(F, D~(*)) in degrees >= b0 and surjectively in degrees >= b1.
degreeBound = (m, D, b) -> (
    D' := flattenComplex D; -- This is a complex over a polynomial ring R1.
    R1 := ring D; -- This should be a singly graded algebra.
    degs := flatten degrees R1; -- This is a list of the form {1,9,15,22}, say.
    n := #degs; -- So P = Proj R1 has dimension n-1.
    sumOfWeights := fold(plus, degs);
    -- Thus sumOfWeights = sum_i |x_i|, where R1 = k[x_0,...,x_(n-1)].
    bettitable := betti freeResolution(D', LengthLimit => n - m - min D');
    -- The Betti numbers of the minimal free resolution ... -> F_(1 + min D') -> F_(min D') -> 0 of D'
    -- over R1, correct out to F_{n-m}.
    nonzerokeys := select(keys bettitable, k -> bettitable#k != 0);
    relevantpart0 := select(nonzerokeys, (i,d,h) -> (i == n-m));
    relevantpart1 := select(nonzerokeys, (i,d,h) -> (i == n-m-1));
    maxdeg0 := max apply(relevantpart0, (i,d,h) -> h); -- The maximum degree of a generator of F_{n-m}.
    maxdeg1 := max apply(relevantpart1, (i,d,h) -> h); -- The maximum degree of a generator of F_{n-m-1}.
    maxdeg := max(maxdeg0, maxdeg1); -- The maximum degree of a generator of F_{n-m} or F_{n-m-1}.
    -- It is -infinity if those two free modules are zero. Then the map Ext^c_R1(B, D')_a -> H^c(X, D~(a))
    -- is an isomorphism for all a >= b, for any homogeneous ideal B in R1 that lives
    -- in degrees > maxdeg - sumOfWeights - b and that contains a power of the irrelevant ideal.
    -- So we define e = maxdeg + 1 - sumOfWeights - b.
    -- Also, for this choice of e and hence M2, the map from Ext^c to H^c is surjective
    -- in degrees >= b - (maxdeg - maxdeg1). That adds information if maxdeg1 < maxdeg0.
    e := maxdeg + 1 - sumOfWeights - b;
    if e === -infinity then b1 := -infinity else b1 = b - (maxdeg - maxdeg1) + min(e,0);
    (b + min(e,0), b1, maxdeg + 1 - sumOfWeights - b))


protect TruncateDegree
-- Ext^m(F, G(>=b)), for coherent sheaves F and G over a projective scheme
-- (or weighted projective stack) X = Proj R2 and an integer m,
-- returns a graded R2-module that agrees with Ext^m_X(F, G(*)) in degrees >= b.
-- If we introduce commands for Ext between complexes of sheaves, this function may need to watch
-- for the case where F is a sheaf and G is a complex of sheaves. The option "MinimalGenerators => NonPrint"
-- avoids printing; instead, it returns a sequence (b0, b1, M0) with M0 a module
-- that maps to Ext^m_X(F, G(*)), isomorphically in degrees >= b0 and surjectively in degrees >= b1.
Ext(ZZ, SheafOfRings,  SumOfTwists) := Module => opts -> (m, O, S) -> Ext^m(O^1, S)
Ext(ZZ, CoherentSheaf, SumOfTwists) := Module => opts -> (m, F, S) -> (
    (G, b) := (S#0, S#1#0); -- Here G should be a coherent sheaf.
    if F.cache.?Twist or G.cache.?Twist then ( -- Here F or G was defined as a twist of another sheaf,
	-- say F = F0(c) and G = G0(d). We reduce to the calculation for F0 and G0, to take advantage of caching.
	F0 := F; c := 0; G0 := G; d := 0;
	if F.cache.?Twist then (
	    c = first F.cache.Twist#0;
	    F0 = F.cache.Twist#1);
	if G.cache.?Twist then (
	    d = first G.cache.Twist#0;
	    G0 = G.cache.Twist#1);
	(b0,b1,E0) := Ext^m(F0, G0(>= b + d - c), MinimalGenerators => NonPrint);
	E := E0(d - c);
	b0 = b0 - (d - c); b1 = b1 - (d - c);
	E.cache.TruncateDegree = E0.cache.TruncateDegree)
    else (-- Now the sheaves F and G were not defined as twists.
	N := target currentModuleMap G; -- A simplified module that represents G.
	R2 := assertWeightedZZGraded ring N;
	degs := flatten degrees R2; -- This is a list of the form {1,9,15,22}, say.
	n := #degs;
	M := target currentModuleMap F;
	Mres := freeResolution(M, LengthLimit => m+1);
	HomMN := Hom(Mres, N);
	e := 0;
	(b0, b1, e) = degreeBound(m, HomMN, b);
	-- We need to construct a homogeneous ideal M2 in the graded polynomial ring R1 that is concentrated
	-- in degrees at least e and that contains a power of the irrelevant ideal (x_0,...,x_(n-1)). The following
	-- seems like the best choice for efficiency. We tensor the Koszul complex that resolves this ideal
	-- over R1 with R2, so we can work entirely with R2-modules.
	if e <= 0 then (
	    -- In this case, we can take M2 = R1, which gives the following.
	    E = HH^m(HomMN))
	else (
	    -- Here e > 0.
	    M2gens := apply(n, i -> ((R2_i)^-((-e)//degs#i)));
	    -- That is, the list M2gens consists of each variable x_i to the power b_i := roundup(e/a_i).
	    koszulRes := (koszulComplex(M2gens, Concentration => (max(0, n-m-2), n-1)))[n-1];
	    -- Thus koszulRes is in homological degrees 0, -1, ..., -min(n-1, m+1).
	    Torshift := fold(plus, apply(M2gens, i -> first degree i)); -- This is sum_i a_i b_i, which is at least ne.
	    E = (HH^m(koszulRes ** HomMN))(Torshift));
	-- The map Ext^m_R2(M2 tensor_(R1) R2, Hom(Mres, N)) -> Ext^m_X(F, G(*)) is an isomorphism in graded degrees >= b,
	-- and so we will return the first module. We don't truncate or prune it; truncating can be slow,
	-- and this module will often be simpler (in terms of generators and relations) than a truncation
	-- of it. The user is told what the output means. Eventually, we could add information
	-- about surjectivity of the map in some degrees, as in cohomologyDirect.
	E.cache.TruncateDegree = e);
    -- Here e is the degree at which the polynomial ring R1 was truncated to form M2;
    -- this may be used later for computing the Yoneda extension.
    if opts.MinimalGenerators === NonPrint then return (b0, b1, E);
    if b0 === -infinity then	<< "The following module is correct in all degrees."
    else if b0 == b1 then << "The following module is correct in degrees >= " << b0 << "."
    else if b1 === -infinity then << "The following module maps isomorphically to Ext in degrees >= " << b0
    << " and surjectively in all degrees."
    else << "The following module maps isomorphically to Ext in degrees >= " << b0
    << " and surjectively in degrees >= " << b1 << ".";
    E)

Ext(ZZ, SheafOfRings, SheafOfRings)  :=
Ext(ZZ, SheafOfRings, CoherentSheaf) :=
Ext(ZZ, CoherentSheaf, SheafOfRings)  :=
Ext(ZZ, CoherentSheaf, CoherentSheaf) := Module => opts -> (n, F, G) -> (
    E := (Ext^n(F, G(>=0), opts ++ { MinimalGenerators => NonPrint }))#2;
    -- With the NonPrint option, Ext returns a sequence (b0,b1,M0), and we just want the module M0.
    k := coefficientRing ring E;
    V := k^(hilbertFunction(0, E));
    V.cache.formation = FunctionApplication { Ext, (n, F, G) };
    V.cache.TruncateDegree = E.cache.TruncateDegree;
    V)

-----------------------------------------------------------------------------
-- hh: Hodge decomposition
-----------------------------------------------------------------------------
-- TODO: HodgeTally for pretty printing the Hodge diamond

hhOptions = new OptionTable from {
    Degree => 0
    }

-- Modeled on Ext, defined as a ScriptedFunctor in complexes.m2.
hh = new ScriptedFunctor from {
    superscript => i -> new ScriptedFunctor from {
	-- hh^i(F), hh^i(F, a, b), and so on
	argument => hhOptions >> opts -> X -> applyMethodWithOpts''(hh, functorArgs(i, X), opts)
	},
    argument => hhOptions >> opts -> X -> applyMethodWithOpts''(hh, X, opts)
    }

-- using Hodge symmetry and Serre duality to ease the computation
-- TODO: is minimum necessarily the most efficient?
min'pq := d -> (p,q) -> min{(p,q), (q,p), (d-p,d-q), (d-q,d-p)}

-- The Hodge numbers of a projective variety over a field.
-- By definition, hh^(p,q)(X) = dim HH^q(X, Omega^p).
hh(Sequence, ProjectiveVariety) := ZZ => (pq, X) -> (
    -- p and q are swapped here, because cotangentSheaf seems to be the
    -- slowest part of this algorithm, so we minimize the exterior powers
    (q,p) := (min'pq dim X) pq;
    if not X.cache.?hh   then X.cache.hh = new MutableHashTable;
    if X.cache.hh#?(p,q) then X.cache.hh#(p,q) else X.cache.hh#(p,q) = (
	hh^q reflexiveDifferentials(p, X)))

-- This function hh^i(F) computes coherent sheaf cohomology for a coherent sheaf
-- on a closed subspace in a projective space, or more generally in a weighted projective space.
-- If you want to compute cohomology with many twists, the functions hh^i(F,b1,b2) or hh^i(F(*))
-- should be faster than running this program repeatedly.
-- The base ring should be a field. Note that it is usually faster
-- to work over Z/p for a prime number p <= 32767, say ZZ/31991, rather than over Q.
--
-- This program computes only the dimension of the cohomology, not the cohomology as a vector space.
-- The algorithm uses local duality, as the function HH^i(F(>=a)) does when i>0.
-- It should be faster, in most cases, since it has less to compute.
--
-- The distinction between a WPS as a stack X and its associated coarse moduli space, f: X -> V, does not matter
-- for computing cohomology. Indeed, a finitely generated graded module M determines a coherent sheaf M~
-- on X, and it is a fact that H^i(X, M~) = H^i(V, f_*(M~)) for every i. Twists are interpreted by tensoring with
-- the line bundles O(c) on the stack, which ensures that H^i(X,M~(c)) = H^i(X,M(c)~).
--
-- We allow the option hh^i(F, Degree => a) to mean hh^i(F(a)). We expect people to use the latter notation,
-- but (in effect) we translate it into the first notation for better caching. (We only need to store information
-- about one sheaf, not its twists.)
--
hh(ZZ, SheafOfRings)  := ZZ => opts -> (cohodeg, O) -> hh^cohodeg(O^1, opts)
hh(ZZ, CoherentSheaf) := ZZ => opts -> (cohodeg, F) -> (
    if not isProjective variety F then ( -- We give a correct answer in the affine case, although that's not our main focus.
	return if cohodeg != 0 then 0
	else if dim F > 0 then infinity
	else degree F);
    if F.cache.?Twist then return hh^cohodeg(F.cache.Twist#1, Degree => opts.Degree + first F.cache.Twist#0);
    -- Thus we reduce to the case where the sheaf F was not defined as a twist.
    R2 := ring module F;
    M := currentModuleBaseRing F;
    R1 := assertWeightedZZGraded ring M; -- R1 is a graded polynomial ring, and M is a simplified R1-module that represents the sheaf F.
    -- In particular, we have arranged that M has no m-torsion (where m is the maximal ideal R1_(>0)).
    A := degreesRing R1; -- This is a ring of the form "ZZ[T]" (meaning Z[T,T^(-1)]).
    T := A_0; -- This is the variable in the ring A.
    degs := flatten degrees R1; -- This is a list of the form {1,9,15,22}.
    n := #degs; -- So P = Proj R1 has dimension n-1.
    sumOfWeights := fold(plus, degs); -- This is the sum of the weights, that is, n+sigma in Symonds's notation,
    -- for P = P^{n-1}(a_0,...,a_(n-1)).
    R1.cache ??= new MutableHashTable;
    ww := R1.cache.Dualizing ??= R1^{-sumOfWeights};
    -- We fix the dualizing module ww, as a graded R1-module. As a result, Macaulay2 automatically remembers Ext^j(M, ww)
    -- (for a number j) in case another function has computed that module earlier.
    output := 0;
    if cohodeg == 0 then (
	output = hilbertFunction(-(opts.Degree), Ext^(n-1-cohodeg)(M, ww));
	-- We have arranged that H^0_m(M) = M_tors is zero, so we don't need to subtract its graded dual Ext^(n-cohodeg)(M, ww).
	output = output + hilbertFunction(opts.Degree, M)
	)
    else (
	output = hilbertFunction(-(opts.Degree), Ext^(n-1-cohodeg)(M, ww))
	)
    )


-- Replace T by T^{-1} for an element of ZZ[T] (meaning Z[T,T^(-1)]). This replaces
-- "substitute(f, T => T^(-1))", which is slow.
invertvar = (f) -> (
    A := ring f; -- This should be a ring of the form ZZ[T].
    T := A_0; -- This is the variable in that ring.
    coeffseq := (coefficients f)_1; -- This is a column matrix of the coefficients (ignoring the monomials).
    exposeq := flatten exponents f; -- This is the list of exponents of the monomials, in the form {-1, 2, 3}.
    monseq := {apply(exposeq, x -> T^(-x))}; -- This is the list of inverted monomials, in the form {{T^1, T^-2, T^-3}}.
    monomialmatrix := matrix monseq; -- This is the corresponding row matrix.
    (monomialmatrix*coeffseq)_(0,0))


-- This function hh^i(F,b1,b2) computes cohomology with coefficients in a coherent sheaf F
-- on a closed subspace of a weighted projective space, with all twists in an interval [b1,b2].
-- This should be faster than running hh^i(F(b)) separately for many twists b.
-- In some cases, you might prefer hh^i(F(*)), which computes the cohomology in all twists.
-- The base ring should be a field. Note that it is usually faster
-- to work over Z/p for a prime number p <= 32767, say ZZ/31991, rather than over Q.
--
-- This program computes only the Hilbert series of the cohomology, not the cohomology as a module.
-- The algorithm uses local duality, as in the function HH^i(F(>=b)) that computes the module.
-- It should be faster, in most cases, since it has less to compute.
--
-- The distinction between a WPS as a stack X and its associated coarse moduli space, f: X -> V, does not matter
-- for computing cohomology. Indeed, a finitely generated graded module M determines a coherent sheaf M~
-- on X, and it is a fact that H^i(X, M~) = H^i(V, f_*(M~)) for every i. Twists are interpreted by tensoring with
-- the line bundles O(c) on the stack, which ensures that H^i(X,M~(c)) = H^i(X,M(c)~).
--
-- We allow the option hh^i(F, b1, b2, Degree => a) to mean hh^i(F(a), b1, b2). We expect people to use the latter notation,
-- but (in effect) we translate it into the first notation for better caching. (We only need to store information
-- about one sheaf, not its twists.)
--
hh(ZZ, SheafOfRings,  ZZ, ZZ) := RingElement => opts -> (cohodeg, O, b1, b2) -> hh^cohodeg(O^1, b1, b2, opts)
hh(ZZ, CoherentSheaf, ZZ, ZZ) := RingElement => opts -> (cohodeg, F, b1, b2) -> (
    checkProjective variety F;
    if F.cache.?Twist then return hh^cohodeg(F.cache.Twist#1, b1, b2, Degree => opts.Degree + first F.cache.Twist#0);
    -- Thus we reduce to the case where the sheaf F was not defined as a twist.
    b1 = opts.Degree + b1; b2 = opts.Degree + b2;
    if instance(F, SheafOfRings) then F = F^1; -- That makes F a CoherentSheaf.
    if not instance(F, CoherentSheaf) or not instance(b1, ZZ) or not instance(b2, ZZ) or b1>b2 then (
	error "the input should be in the form hh^i(F,b1,b2), with F a coherent sheaf and b1 <= b2 integers");
    R2 := ring module F;
    M := currentModuleBaseRing F;
    R1 := assertWeightedZZGraded ring M; -- R1 is a graded polynomial ring, and M is a simplified R1-module that represents the sheaf F.
    -- In particular, we have arranged that M has no m-torsion (where m is the maximal ideal R1_(>0)).
    A := degreesRing R1; -- This is a ring of the form "ZZ[T]" (meaning Z[T,T^(-1)]).
    T := A_0; -- This is the variable in the ring A.
    degs := flatten degrees R1; -- This is a list of the form {1,9,15,22}.
    n := #degs; -- So P = Proj R1 has dimension n-1.
    sumOfWeights := fold(plus, degs); -- This is the sum of the weights, that is, n+sigma in Symonds's notation,
    -- for P = P^{n-1}(a_0,...,a_(n-1)).
    R1.cache ??= new MutableHashTable;
    ww := R1.cache.Dualizing ??= R1^{-sumOfWeights};
    -- We fix the dualizing module ww, as a graded R1-module. As a result, Macaulay2 automatically remembers Ext^j(M, ww)
    -- (for a number j) in case another function has computed that module earlier.
    output := 0;
    extmodule1 := Ext^(n-1-cohodeg)(M, ww);
    poincare extmodule1; -- This caches the Poincare polynomial of extmodule1 = Ext^(n-1-cohodeg)(M, ww),
    -- automatically used by hilbertSeries in what follows.
    if cohodeg == 0 then (
	-- We have arranged that H^0_m(M) = M_tors is zero, so we don't need to subtract off Ext^n(n-cohodeg)(M, ww),
	-- the graded dual to H^0_m(M).
	output = hilbertSeries(extmodule1, Order => -b1+1);
	output = invertvar(output); -- This is what we want, in degrees at least b1.
	output = output + hilbertSeries(M, Order => b2+1);
	output = part(b1, b2, output) -- We remove terms of degree below b1 or above b2.
	)
    else (
	output = hilbertSeries(extmodule1, Order => -b1+1);
	output = invertvar(output); -- This is the answer, in degrees at least b1.
	output = part(b1, b2, output) -- We remove terms of degree above b2.
	);
    output*T^-(opts.Degree))

-- Compute f1+f2, for f1 a "Divide" (a kind of rational function in a variable T)
-- and f2 a Laurent polynomial in T (as an element of "ZZ[T]" = Z[T,T^(-1)]. The output is again a Divide.
--
add = (f1, f2) -> (
    num1 := value numerator f1;  -- The numerator is a Laurent polynomial in ZZ[T]. (We need to say "value" because
    -- if the numerator is, say, T^3 (rather than a bigger polynomial), then its type may be "Power". To allow different
    -- types of input, we will likewise use value(f2) in what follows.)
    den1 := denominator f1; -- The denominator is a "Product", something like (1-T^2)(1-T^3).
    Divide{num1 + (value f2) * (value den1), den1})

-- Compute f1*f2, for f1 a "Divide" (a kind of rational function in a variable T)
-- and f2 a Laurent polynomial in T (as an element of "ZZ[T]" = Z[T,T^(-1)]. The output is again a Divide.
--
mult = (f1, f2) -> Divide(value(numerator f1)*value(f2),denominator f1)
-- The numerator of f1 is a Laurent polynomial in ZZ[T]. (We need to say "value" because
-- if the numerator is, say, T^3 (rather than a bigger polynomial), then its type may be "Power". To allow different
-- types of input, we likewise use value(f2) here.)
-- The denominator of f1 is a "Product", something like (1-T^2)(1-T^3).

-- Compute the top degree of a series in T and T^{-1}. The input is positiveseries (a "Divide" in a variable T)
-- and negativeseries (a "Divide" in a variable U, viewed as T^(-1)). It is assumed that all terms of negativeseries
-- have degree (in terms of T) below the degree of all terms of positiveseries. The output
-- could be infinity (if positiveseries is not a polynomial in T) or -infinity (if both inputs are 0).
topDegree = (positiveseries, negativeseries) -> (
    posseries := reduceHilbert positiveseries;
    negseries := reduceHilbert negativeseries; -- We simplify the input functions, so we can see whether they are polynomials.
    A := ring numerator negseries;
    U := A_0;
    num := 0;
    if value(denominator(posseries)) != 1 then infinity
    else (  -- Here posseries is a polynomial in T.
	num = numerator(posseries);
	if num != 0 then (degree(num))_0
	else ( -- Here posseries is 0.
	    num = numerator(negseries);
	    if num != 0 then (degree(substitute(num, U => U^(-1))))_0
	    else -infinity -- Here both inputs are zero.
	    )
	)
    )

-- This function (usually called as hh^i(F(*)))
-- computes coherent sheaf cohomology on a closed subspace of a weighted projective space with all twists.
-- For the input hh^i(F(>=b)), the number b is ignored, as the output explains. The base ring should be a field.
-- The output is a sequence listing the infimum of weights c such that H^i(X,F(c)) is not zero (possibly -infinity or,
-- if the cohomology is zero in all weights, infinity), the supremum of such weights,
-- a rational function in T, and a rational function in U = T^{-1}, such that the sum of these two functions
-- as Laurent series is sum_c h^i(X, F(c)) T^c (the sum over all integers c). (The two functions do not overlap,
-- as formal series in T.)
--
-- This program computes only the Hilbert series of the cohomology, not the cohomology as a module.
-- The algorithm, using local duality, is the same as that used for hh^i(F) and hh^i(F,b1,b2),
-- and slightly different from that used for the module HH^i(F(>=b)). It should be faster, in most cases.
-- The base ring should be a field. Note that it is usually faster
-- to work over Z/p for a prime number p <= 32767, say ZZ/31991, rather than over Q.
--
-- The distinction between a WPS as a stack X and its associated coarse moduli space, f: X -> V, does not matter
-- for computing cohomology. Indeed, a finitely generated graded module M determines a coherent sheaf M~
-- on X, and it is a fact that H^i(X, M~) = H^i(V, f_*(M~)) for every i. Twists are interpreted by tensoring with
-- the line bundles O(c) on the stack, which ensures that H^i(X,M~(c)) = H^i(X,M(c)~).
--
-- We allow the option hh^i(F, Degree => a) to mean hh^i(F(a)). We expect people to use the latter notation,
-- but (in effect) we translate it into the first notation for better caching. (We only need to store information
-- about one sheaf, not its twists.)
--
hh(ZZ, SumOfTwists) := Sequence => opts -> (cohodeg, sumoftwists) -> (
    -- Compute H^{cohodeg}(X,F(a)) for all integers a, as a sum of two Laurent polynomials,
    -- where F is a CoherentSheaf (or a SheafOfRings) on a closed subspace of a weighted projective space.
    F := sumoftwists#0; -- For an input of the form hh^i(F(>=b)), the number b is ignored. Note that,
    -- if F is input as a SheafOfRings, SumOfTwists automatically turns it into a CoherentSheaf; so that's what this function receives.
    if F.cache.?Twist then return hh^cohodeg((F.cache.Twist#1)(*), Degree => opts.Degree + first F.cache.Twist#0);
    -- Thus we reduce to the case where the sheaf F was not defined as a twist.
    R2 := ring module F;
    M := currentModuleBaseRing F;
    R1 := assertWeightedZZGraded ring M; -- R1 is a graded polynomial ring, and M is a simplified R1-module that represents the sheaf F.
    -- In particular, we have arranged that M has no m-torsion (where m is the maximal ideal R1_(>0)).
    A := degreesRing R1; -- This is a ring of the form "ZZ[T]" (meaning Z[T,T^(-1)]).
    T := A_0; -- This is the variable in the ring A.
    U := getSymbol "U";
    B := newRing(A,Variables=>{U},MonomialOrder=>{MonomialSize=>32,Weights=>{-1},GroupLex=>1,Position=>Up},Inverses=>true);
    U = B_0;
    -- Thus B is a ring of the form "ZZ[U]" (meaning Z[U,U^(-1)]). We think of U as meaning T^(-1).
    degs := flatten degrees R1; -- This is a list of the form {1,9,15,22}.
    n := #degs; -- So P = Proj R1 has dimension n-1.
    sumOfWeights := fold(plus, degs); -- This is the sum of the weights, that is, n+sigma in Symonds's notation,
    -- for P = P^{n-1}(a_0,...,a_(n-1)).
    R1.cache ??= new MutableHashTable;
    w := R1.cache.Dualizing ??= R1^{-sumOfWeights};
    -- We fix the dualizing module w, as a graded R1-module. As a result, Macaulay2 automatically remembers Ext^j(M, w)
    -- (for a number j) in case another function has computed that module earlier.
    bottomdeg := 0;
    topdeg := 0;
    positiveseries := 0;
    negativeseries := 0;
    hilb1 := 0; hilb0 := 0;
    extmodule1 := Ext^(n-1-cohodeg)(M, w); -- Macaulay2 automatically caches this, in case Ext was computed earlier or will be computed later.
    poincare extmodule1; -- This caches the Poincare polynomial of extmodule1 = Ext^(n-1-cohodeg)(M, w),
    -- automatically used by hilbertSeries in what follows.
    if cohodeg == 0 then (
	hilbM := mult(hilbertSeries(M, Reduce => true), T^(-opts.Degree)); -- This is a "Divide".
	if dim extmodule1 <= 0 then ( -- Here H^1_m(M) has finite length, and so (equivalently) H^0(X,F(*)) is bounded below.
	    -- In this case, we will return the output as a rational function in T.
	    hilb1 = T^(opts.Degree) * value numerator hilbertSeries(extmodule1, Reduce => true); -- The graded dual of this is local cohomology H^1_m(M),
	    -- which has finite length in the case at hand. So its Hilbert series is a Laurent polynomial.
	    -- We arranged that H^0_m(M) = M_tors is zero; so we don't need to subtract off the Hilbert series of Ext^n(M, w),
	    -- the graded dual of H^0_m(M).
	    hilb10 := substitute(hilb1, T => T^(-1)); -- This is a Laurent polynomial in ZZ[T] = Z[T, T^(-1)].
	    positiveseries = add(hilbM, hilb10);
	    negativeseries = 0;
	    if value numerator positiveseries == 0 then (  -- In this case, H^0(X,M(*)) = 0.
		topdeg = -infinity;
		bottomdeg = infinity)
	    else (
		topdeg = infinity;
		bottomdeg = first min exponents value numerator positiveseries -- This gives the bottom degree of the output, positiveseries.
		)
	    )
	else ( -- Here extmodule1 is not bounded above, and so H^0(X, F(*)) is not bounded below.
	    -- We will return the output as a rational function in T (of degree >=0 as a series in T) plus a Laurent polynomial in U = T^(-1)
	    -- (of degree >0 as a series in U, hence of degree <0 as a series in T).
	    hilb1 = mult(hilbertSeries(extmodule1, Reduce => true), T^(opts.Degree));
	    pospart1 := hilbertSeries(extmodule1, Order => 1 - opts.Degree) * T^(opts.Degree); -- This is a Laurent polynomial (not series)
	    -- in degrees <= 0, but we'll change T to T^{-1}; hence the name "pospart1".
	    negpart1 := add(hilb1, -pospart1); -- This is a "Divide", the part of hilb1 in T-degree > 0.
	    -- We arranged that H^0_m(M) = M_tors is zero, so we don't need to subtract off a contribution from Ext^n(M,w),
	    -- the graded dual of H^0_m(M).
	    pospart10 := substitute(pospart1, T => T^(-1)); -- A polynomial in T.
	    negpart10 := substitute(negpart1, T => U); -- A "Divide" in U, of U-degree >= 1 as a series.
	    negpartM := hilbertSeries(M, Order => opts.Degree) * T^(-opts.Degree); -- The part of hilbM of T-degree < 0.
	    positiveseries = add(hilbM, -negpartM + pospart10);
	    negativeseries = add(negpart10, substitute(negpartM, T => U^(-1)));
	    topdeg = topDegree(positiveseries, negativeseries);
	    bottomdeg = -topDegree(negativeseries, positiveseries) -- This works even if these degrees are infinity or -infinity.
	    )
	)
    else (  -- Here we are computing cohomology in some degree > 0.
	hilb1 = mult(hilbertSeries(extmodule1, Reduce => true), T^(opts.Degree)); -- Here extmodule1 = Ext^{n-1-cohodeg)(M,w).
	-- This is a bounded below series in T. It remains to replace T by U = T^(-1),
	-- compute some properties, and give the output in the form we want. (If H^cohodeg(X, F(*)) is bounded above and below,
	-- we will write the output in terms of T.)
	if value numerator hilb1 == 0 then (
	    bottomdeg = infinity;
	    topdeg = -infinity;
	    positiveseries = 0;
	    negativeseries = 0)
	else (  -- Here the output is not zero.
	    topdeg = -first min exponents value numerator hilb1; -- This is minus the bottom degree of the numerator,
	    -- which is a Laurent polynomial in T.
	    if value denominator hilb1 == 1 then (  -- In this case, H^(cohodeg)(X, F(*)) is bounded above and below.
		bottomdeg = -first degree value numerator hilb1;
		positiveseries = substitute(hilb1, T => T^(-1));
		negativeseries = 0)
	    else (
		bottomdeg = -infinity;
		positiveseries = 0;
		negativeseries = substitute(hilb1, T=>U)
		)
	    )
	);
    ("The following is correct in all degrees. Bottom degree:", bottomdeg, "top degree:", topdeg, "cohomology as a series in T:", positiveseries,
	"plus cohomology as a series in U = T^(-1):", negativeseries))
