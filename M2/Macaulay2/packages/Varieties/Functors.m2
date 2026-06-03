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

-- For F a coherent sheaf on a scheme X, with X a closed subscheme of Y (writing i: X -> Y for the inclusion),
-- return the direct image sheaf i_*(F) on Y. This function requires (and partly checks)
-- that X and Y are given as Proj(S/I) and Proj(S/J),
-- with the same polynomial ring S in both cases, such that J is contained in I; or likewise in the affine case.
-- This function also works for subspaces of a weighted projective space.
-- The option Prune => false avoids simplifying the output.
directImage = {Prune => true} >> opts -> (F, Y) -> (
    if instance(F, SheafOfRings) then F = F^1; -- That makes F a CoherentSheaf.
    M := module F;
    imap := map(ring M, ring Y);
    if not isWellDefined imap then error "ring map not well defined";
    if opts.Prune then sheaf pushFwd(imap, M) else sheaf pushFwd(imap, M, NoPrune => true))

-- For F a coherent sheaf on a scheme Y, with X a closed subscheme of Y (writing i: X -> Y for the inclusion),
-- pullback(F, X) returns the pullback sheaf i^*(F) on X. This function assumes (and partly checks)
-- that X and Y are given as Proj(S/I) and Proj(S/J),
-- with the same polynomial ring S in both cases, such that J is contained in I; or likewise in the affine case.
-- This function also works for subspaces of a weighted projective space.
-- The option Prune => false avoids simplifying the output.
pullback(CoherentSheaf, Variety) := {Prune => true} >> opts -> (F, X) -> (
    M := module F;
    if not isWellDefined map(ring X, ring M) then error "ring map not well defined";
    output := M ** (ring X);
    if opts.Prune then sheaf minimalPresentation output else sheaf output)

pullback(SheafOfRings, Variety) := {Prune => true} >> opts -> (F, X) -> (
    M := module (F^1);
    if not isWellDefined map(ring X, ring M) then error "ring map not well defined";
    output := M ** (ring X);
    if opts.Prune then sheaf minimalPresentation output else sheaf output)

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
-- We expect not to apply this function when F was defined as a twist (i.e., when F.cache.?twist is true), to simplify caching.
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
-- We expect not to apply this function when F was defined as a twist (i.e., when F.cache.?twist is true), to simplify caching.
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

-- TODO: add tests:
-- - global sections of sheafHom are Hom
-- TODO: implement for multigraded ring using emsbound
-- TODO: this can change F.module to the result!
-- NOTE: this may have elements in degrees below bound as well, is that a bug?
twistedGlobalSectionsModule = (F, bound) -> (
    -- compute global sections module Gamma_(d >= bound)(X, F(d))
    A := ring variety F;
    -- FIXME: this line, as opposed to
    --  cokernel presentation module F
    -- breaks the test added in 975d780470.
    -- However, we need to keep the information
    -- cached in M, for instance if M is a Hom module.
    M := module F;
    if degreeLength A =!= 1 then error "expected degree length 1";
    -- quotient by HH^0_m(M) to kill the torsion
    -- TODO: pass the appropriate irrelevant ideal
    N := killLocalH0 M;
    -- pushforward to the projective space
    N' := flattenModule N;
    S := ring N';
    -- TODO: both n and w need to be adjusted for the multigraded case
    n := dim S-1;
    w := S^{-n-1}; -- canonical sheaf on P^n
    -- Note: bound=infinity signals that HH^1_m(M) = 0, ie. M is saturated
    -- in other words, don't search for global sections not already in M
    -- TODO: what would pdim N' < n, hence En = 0, imply?
    -- pdim N' < n means N' has depth >= 1 already
    p := if bound === infinity or pdim N' < n then 0 else (
	-- En is the dual of the HH^1_m(N'), so its degrees
	-- tell us how far to look for the extension
	-- 0 -> N' -> Gamma_*(N'^~) -> HH^1_m(N') -> 0
	En := Ext^n(N', w); -- the top Ext
	if dim En <= 0 -- 0-module or 0-dim module (i.e. finite length)
	then 1 + max degreeList En - min degreeList En
	else 1 - first min degrees En - bound);
    -- this can only happen if bound=-infinity, e.g. from calling H^0(F(*)) = H^0(F(>=(-infinity))
    if p === infinity then error "the global sections module is not finitely generated";
    -- caching these to be used later in prune SheafMap
    F.cache.TorsionFree = N;
    F.cache.GlobalSectionLimit = max(0, p);
    -- this is the module Gamma_* F
    G := minimalPresentation if p <= 0 then N else target(
	-- TODO: substitute with appropriate irrelevant ideal here
	Bp := (ideal vars A)^[p];
	-- consider the sequence 0 -> B^[p] -> A -> A/B^[p] -> 0
	inc := inducedMap(module A, module Bp);
	iso := inducedMap(Hom(A, N), N);
	-- we compute the map N -> Gamma_* F as a limit by
	-- applying Hom(-,N) to the sequence above
	-- c.f. the limit from minimalPresentation hook
	-- and emsbound in NormalToricVarieties/Sheaves.m2
	phi := Hom(inc, N, MinimalGenerators => true) * iso);
    -- now we compute the center map in the sequence
    -- 0 -> HH^0_B(M) -> M -> Gamma_* F -> HH^1_B(M) -> 0
    iota := inverse G.cache.pruningMap; -- map from Gamma_* F to its minimal presentation
    quot := inducedMap(N, M);           -- map from M to N = M/HH^0_B(M)
    F.cache.SaturationMap = if p <= 0 then iota * quot else iota * phi * quot;
    G)

-----------------------------------------------------------------------------
-- cohomology
-----------------------------------------------------------------------------
-- TODO: add hooks for X not finite type over k?

-- HH^p(X, OO_X)
cohomology(ZZ,          SheafOfRings) := Module => opts -> (p,    O) -> cohomology(p, variety O, O^1, opts)
cohomology(ZZ, Variety, SheafOfRings) := Module => opts -> (p, X, O) -> cohomology(p,         X, O^1, opts)

-- HH^p(X, F(>=b))
cohomology(ZZ,                    SumOfTwists) := Module => opts -> (p,    S) -> cohomology(p, variety S, S, opts)
cohomology(ZZ, ProjectiveVariety, SumOfTwists) := Module => opts -> (p, X, S) -> (
    checkVariety(X, S);
    (F, b) := (S#0, S#1#0);
    F.cache.HH        ??= new MutableHashTable;
    -- TODO: when p>0, HH^p(F(*)) gives a "not implemented yet" error
    F.cache.HH#(p, b) ??= if p == 0
    then twistedGlobalSectionsModule(F, b)
    else HH^(p+1)(module F, Degree => b))

-- HH^p(X, F)
cohomology(ZZ,                    CoherentSheaf) := Module => opts -> (p,    F) -> cohomology(p, variety F, F, opts)
cohomology(ZZ,     AffineVariety, CoherentSheaf) := Module => opts -> (p, X, F) -> (
    checkVariety(X, F);
    if p == 0 then module F else (ring X)^0)
cohomology(ZZ, ProjectiveVariety, CoherentSheaf) := Module => opts -> (p, X, F) -> (
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
	    if F.cache.?twist then (-- Here F was defined as a twist of another sheaf, say F = E(shift). We reduce to the calculation for E.
		shift := first F.cache.twist#0; -- Here F.cache.twist#0 should be a degree in the form {3}, and then shift would be 3.
		H := minimalPresentation F.cache.twist#1; -- Here F.cache.twist#1 is the original sheaf E.
		Gmap := (H.cache.pruningMap)(shift);
		-- We mainly record F.cache.TorsionFree (as a quotient of F.module) for use in SheafMaps.m2.
		F.cache.TorsionFree = ((F.cache.twist#1).cache.TorsionFree)(shift);
	        F.cache.GlobalSectionLimit = -shift + (F.cache.twist#1).cache.GlobalSectionLimit)
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
-- The following algorithm appears in:
-- Gregory G. Smith, Computing global extension modules,
-- Journal of Symbolic Computation, 29 (2000) 729-746.
-- See documentation of Ext^ZZ(CoherentSheaf,CoherentSheaf) for examples.
-----------------------------------------------------------------------------

protect TruncateDegree
-- TODO: implement Ext with DegreeLimit as an optimization
Ext(ZZ, SheafOfRings,  SumOfTwists) :=
Ext(ZZ, CoherentSheaf, SumOfTwists) := Module => opts -> (m,F,G') -> (
    assertSameVariety(F, G');
    X := variety F;
    checkProjective X;
    checkVariety(X, F);
    G := G'#0;
    e := G'#1#0;
    M := module F;
    N := module G;
    R := ring M;
    r := -infinity;
    E := if dim M === 0 or m < 0 then R^0 else (
	l := min(dim N, m);
	P := resolution flattenModule N;
	p := length P;
	n := dim ring P - 1;
	-- global Ext is composition of sheaf Ext and cohomology
	-- so we compute it as a Grothendieck spectral sequence
	-- in this case, it degenerates
	if p < n-l then Ext^m(M, N, opts) else (
	    a := max apply(n-l..p,j -> (max degrees P_j)#0-j);
	    r = a+e-m+1;
	    Ext^m(truncate(r, M), N, opts)));
    -- When MinimalGenerators => false is given, we don't truncate
    -- or prune E because the majority of uses of this function are
    -- for computing Ext of sheaves, in which case only basis(0, E)
    -- is needed and truncation is an unnecessary computation.
    E = if opts.MinimalGenerators then prune truncate(e, E) else E;
    -- This is the degree at which M was truncated, which
    -- is used later for computing the Yoneda extension.
    E.cache.TruncateDegree = r;
    E)

Ext(ZZ, SheafOfRings, SheafOfRings)  :=
Ext(ZZ, SheafOfRings, CoherentSheaf) :=
Ext(ZZ, CoherentSheaf, SheafOfRings)  :=
Ext(ZZ, CoherentSheaf, CoherentSheaf) := Module => opts -> (n,F,G) -> (
    E := Ext^n(F, G(>=0), opts ++ { MinimalGenerators => false });
    k := coefficientRing ring E;
    V := k^(rank source basis(0, E));
    V.cache.formation = FunctionApplication { Ext, (n, F, G) };
    V.cache.TruncateDegree = E.cache.TruncateDegree;
    V)

-----------------------------------------------------------------------------
-- hh: Hodge decomposition
-----------------------------------------------------------------------------
-- TODO: HodgeTally for pretty printing the Hodge diamond

hh = new ScriptedFunctor from {
    superscript => (
	pq -> new ScriptedFunctor from {
	    argument => X -> (
		a := (pq,X);
		f := lookup_hh ( class \ a );
		if f === null then error "no method available";
		f a
		)
	    }
	)
    }

hh(Sequence,ProjectiveVariety) := (pq,X) -> if X.cache.?hh and X.cache.hh#?pq then X.cache.hh#pq else (
    (p,q) := pq;
    if class p =!= ZZ or class q =!= ZZ then error "expected integer superscripts";
    d := dim X;
    pqs := { (p,q), (q,p), (d-p,d-q), (d-q,d-p) };
    (p,q) = min { (p,q), (q,p), (d-p,d-q), (d-q,d-p) };
    h := rank HH^q cotangentSheaf(p,X);
    if not X.cache.?hh then X.cache.hh = new MutableHashTable;
    scan(pqs, pq -> X.cache.hh#pq = h);
    h)

euler ProjectiveVariety := X -> (
    d := dim X;
    sum(0 .. d, j -> hh^(j,j) X + 2 * sum(0 .. j-1, i -> (-1)^(i+j) * hh^(i,j) X)))
