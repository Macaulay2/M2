---------------------------------------------------------------------------
-- PURPOSE : Computation of quotient, saturation, and annihilator
--
-- UPDATE HISTORY : created 14 April 2018 at M2@UW;
--                  updated November 2020
--
-- TODO : 1. why are there shadowed symbols?
--        2. any strategies to incorporate from Shimoyama-Yokoyama.m2
--           or greedySat in GTZ.m2 or getSaturation in newGTZ.m2
--        3. which algorithms for Ideals can be adapted to submodules?
--        4. move radical here, perhaps intersect as well
--        5. employ storefun! Maybe call it declare
---------------------------------------------------------------------------
newPackage(
    "Saturation",
    Version => "0.2",
    Date => "November 8, 2020",
    Headline => "quotient, saturation, and annihilator routines for ideals and modules",
    Authors => {
	{Name => "Justin Chen",    Email => "justin.chen@math.gatech.edu"},
	{Name => "Mahrud Sayrafi", Email => "mahrud@umn.edu",        HomePage => "https://math.umn.edu/~mahrud"},
	{Name => "Mike Stillman",  Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike"}},
    Keywords => {"Commutative Algebra"},
    PackageExports => { "Elimination" },
    AuxiliaryFiles => true,
    DebuggingMode => false
    )

export { "isSupportedInZeroLocus" }

exportFrom_Core { "saturate", "annihilator" }

importFrom_Core { "nonnull", "printerr", "raw", "rawColon", "rawSaturate", "newMonomialIdeal", "eliminationInfo" }

-- TODO: where should these be placed?
trim MonomialIdeal := MonomialIdeal => opts -> (cacheValue (symbol trim => opts)) ((I) -> monomialIdeal trim(module I, opts))

-- TODO: pick a better name and move to interpreter for speed?
-- this is similar to override, except:
--   keys in opts with value null are ignored (TODO: is this an okay way to remove options?)
--   keys in opts that don't exist in def don't produce an error
override' := (def, opts) -> nonnull apply(keys def, key -> if opts#?key and opts#key =!= null then key => opts#key)

-- TODO: is this the right name for this function?
ambient' = method()
ambient' Module := Module => ambient
ambient' Ideal  := Ideal  => I -> (if instance(I, MonomialIdeal) then monomialIdeal else ideal) 1_(ring I)

-- TODO: remove this once the Ideal vs. MonomialIdeal dichotomy is resolved
uniform' := L -> all(L, l -> instance(l, Ideal)) or uniform L

-- this is a light version of isSubset for modules and ideals, which doesn't compute a gb
-- TODO: move these improvements to isSubset, for instance isSubset(0, A) should not compute gb of A
-- TODO: add heuristics to decide when is it okay to compute a gb of B, e.g. 2 vars?
isSubset' := (A, B) -> A == 0 or isSubset(entries transpose gens A, entries transpose gens B)

--Ideal % Matrix            :=
--remainder(Ideal,  Matrix) := Matrix => (I, m) -> remainder(gens I, m)
--Module % Matrix           :=
--remainder(Module, Matrix) := Matrix => (M, m) -> remainder(gens M, m)

-- This is a map from method keys to strategy hash tables
algorithms := new MutableHashTable from {}

--------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------

cacheHit := type -> if debugLevel > 0 then printerr("Cache hit on a ", synonym type, "! 🎉");

-- given a ring R, determines if R is a poly ring over ZZ or a field
isFlatPolynomialRing = R -> isPolynomialRing R and (isField(kk := coefficientRing R) or kk === ZZ)

-- TODO: can this work with multigraded ideals?
isGRevLexRing = (R) -> (
     -- returns true if the monomial order in the polynomial ring R
     -- is graded reverse lexicographic order, w.r.t. the first degree
     -- vector in the ring.
     mo := (options monoid R).MonomialOrder;
     if #mo === 0 then return false;
     mo = select(mo, x -> x#0 =!= MonomialSize and x#0 =!= Position);
     isgrevlex := mo#0#0 === GRevLex and mo#0#1 === apply(degrees R, first);
     #mo === 1 and isgrevlex and all(mo, x -> x#0 =!= Weights and x#0 =!= Lex))

-- Helper for Linear strategies
-- currently this is false for x+1, but in all use cases homogeneity is required anyway
isLinearForm := f -> degreeLength ring f > 0 and sum degree f === 1 and same(degree \ terms f)

-- Return (R1, R1<-R, R<-R1), where generators i and n are switched
-- TODO: can this be simplified using newRing?
grevLexRing = method()
grevLexRing(ZZ, Ring) := (i, R) -> (
    X := local X;
    n := numgens R;
    degs := degrees R;
    if i === n - 1 then return (R, identity, identity);
    perm := toList splice(0..i-1, n-1, i+1..n-2, i);
    R1 := (coefficientRing R)[X_1..X_n, Degrees => degs_perm, MonomialSize => 16];
    fto := map(R1, R, (generators R1)_perm);
    fback := map(R, R1, (generators R)_perm);
    (R1, fto, fback))

--------------------------------------------------------------------
-- Quotients
--------------------------------------------------------------------
-- quotient methods:
-- 1. syzygies
-- 2. use elimination methods? I forget how?
-- 3. case: x is a variable, I is homogeneous
--    case: x is a polynomial
--    case: x is an ideal
-- 4. DegreeLimit: stop at a certain degree
-- 5. BasisElementLimit: stop after one element
-- 6. PairLimit: stop after 100 S-pairs

-- keys: the second object in the quotient
QuotientOptions = new SelfInitializingType of BasicList
QuotientOptions.synonym = "quotient options"

-- keys: TODO: BasisElementLimit, DegreeLimit, PairLimit
QuotientComputation = new Type of MutableHashTable
QuotientComputation.synonym = "quotient computation"

new QuotientComputation from Sequence := (C, S) -> (
    (A, B) := S;
    cacheKey := QuotientOptions{ mingens B };
    -- TODO: try to find other compatible cacheKeys that can be used in the computation
    try A.cache#cacheKey else A.cache#cacheKey = new QuotientComputation from { Result => null })

isComputationDone = method(TypicalValue => Boolean, Options => true)
isComputationDone QuotientComputation := Boolean => options quotient >> opts -> container -> (
    -- this function determines whether we can use the cached result, or further computation is necessary
    try container.Result =!= null -* TODO: BasisElementLimit, DegreeLimit, PairLimit *- else false)

cacheComputation = method(TypicalValue => CacheFunction, Options => true)
cacheComputation QuotientComputation := CacheFunction => options quotient >> opts -> container -> new CacheFunction from (
    -- this function takes advantage of FunctionClosures by modifying the container
    computation -> (
	if isComputationDone(opts, container) then ( cacheHit class container; container.Result ) else
	if (result := computation(opts, container)) =!= null then ( container.Result = result )))

--quotient = method(...) -- defined in m2/quotient.m2
quotient(Ideal,  Ideal)       := Ideal  => opts -> (I, J) -> quotientHelper(I, J, (quotient, Ideal, Ideal), opts)
quotient(Ideal,  Number)      :=
quotient(Ideal,  RingElement) := Ideal  => opts -> (I, f) -> quotient(I, ideal f, opts)
Ideal  : Ideal                := Ideal  =>         (I, J) -> quotient(I, J)
Ideal  : Number               :=
Ideal  : RingElement          := Ideal  =>         (I, f) -> quotient(I, f)

-- TODO: why is this the right thing to do?
quotient(MonomialIdeal, RingElement) := MonomialIdeal => opts -> (I, f) -> (
    quotient(I, if size f === 1 and leadCoefficient f == 1 then monomialIdeal f else ideal f, opts))
MonomialIdeal : RingElement          := MonomialIdeal => opts -> (I, f) -> quotient(I, f)

quotient(Module, Ideal)       := Module => opts -> (M, I) -> quotientHelper(M, I, (quotient, Module, Ideal), opts)
quotient(Module, Number)      :=
quotient(Module, RingElement) := Module => opts -> (M, f) -> quotient(M, ideal f, opts)
Module : Ideal                := Module =>         (M, I) -> quotient(M, I)
Module : Number               :=
Module : RingElement          := Module =>         (M, f) -> quotient(M, f)

-- annihilator of (B+A)/A, where A and B have a common ambient module
-- note: if A is an ideal and B=f, then this is isomorphic to R/(A:f)
quotient(Module, Module)      := Ideal  => opts -> (M, N) -> quotientHelper(M, N, (quotient, Module, Module), opts)
Module : Module               := Ideal  =>         (M, N) -> quotient(M, N)

-- TODO: the methods with "Number" should be made unnecessary via https://github.com/Macaulay2/M2/issues/1519

-- Helper for quotient methods
quotientHelper = (A, B, key, opts) -> (
    if ring B === ZZ then B = sub(B, ring A);

    strategy := opts.Strategy;
    doTrim := if opts.MinimalGenerators then trim else identity;
    cast := if instance(A, MonomialIdeal) then monomialIdeal else ideal;

    -- this logic runs the strategies in order, or the specified strategy
    computation := (opts, container) -> (
	if (R := ring A) =!= ring B then error "expected objects in the same ring";
	if instance(B, RingElement) then B = ideal B;
	if uniform' {A, B} and ambient' A != ambient' B
	then error "expected objects to be contained in the same ambient object";
	-- note: if B \subset A then A:B should be "everything", but computing
	-- a gb for A can be slow, so isSubset' doesn't compute a gb
	if isSubset'(B, A) then return if uniform' {A, B} then cast 1_R else ambient' A;
	-- note: ideal(..A..) :         f    = A <==> f is nzd / A
	-- note: ideal(..A..) : ideal(..B..) = A <==>
	-- note: module(.A.)  : ideal(..B..) = A <==> B is not contained in any associated primes of A
	-- TODO: can either of the above be efficiently checked?
	-- TODO: module(.A.)  : module(.B.)  = ? <==> A \subset B
	if instance(B, Ideal) then -- see the above TODO item
	if isSubset'(ambient' A, B) then return A;
	-- TODO: add speedup for when isSubset'(A, B), being cautious of (a3):(a3,bc) in kk[abc]/ac

	-- TODO: what would adding {SyzygyLimit => opts.BasisElementLimit, BasisElementLimit => null} do?
	opts = new OptionTable from override'(options gb, opts ++ {Strategy => null});
	runHooks(key, (opts, A, B), Strategy => strategy));

    -- this is the logic for caching partial quotient computations. A.cache contains an option:
    --   QuotientOptions{ mingens B } => QuotientComputation{ Result }
    container := new QuotientComputation from (A, B);

    -- the actual computation of quotient occurs here
    C := (cacheComputation(opts, container)) computation;

    if C =!= null then doTrim C else if strategy === null
    then error("no applicable strategy for ", toString key)
    else error("assumptions for quotient strategy ", toString strategy, " are not met"))

--------------------------------------------------------------------
-- Algorithms for Ideal : Ideal
algorithms#(quotient, Ideal, Ideal) = new MutableHashTable from {
    Iterate => (opts, I, J) -> (
	R := ring I;
	-- TODO: extend this into a strategy for any PID
	if R === ZZ then return ideal sub(gcd I_* / gcd flatten(I_*, J_*), ZZ);
	fold(first entries mingens J, ideal 1_R, (f, M1) ->
	    if generators(f * M1) % generators I == 0 then M1
	    else intersect(M1, quotient(I, ideal f, opts, Strategy => Quotient)))),

    -- FIXME: potential bug with nonstandard fields, see the last test in quotient-test.m2
    Quotient => (opts, I, J) -> (
	R := ring I;
	mR := transpose generators J ** (R / I);
	-- if J is a single element, this is the same as
	-- computing syz gb(matrix{{f}} | generators I, ...)
	g := syz gb(mR, opts,
	    Strategy   => LongPolynomial,
	    Syzygies   => true,
	    SyzygyRows => 1);
	-- The degrees of g are not correct, so we fix that here:
	-- g = map(R^1, null, g);
	lift(ideal g, R)),

    -- TODO
    Linear => (opts, I, J) -> (
	-- assumptions: J is a single linear element, and everything is homogeneous
	if not isHomogeneous I
	or not isHomogeneous J or not isLinearForm J_0
	then return null;
	stderr << "warning: quotient strategy Linear is not yet implemented" << endl; null),

    Monomial => (opts, I, J) -> (
	R := ring I;
	if not isMonomialIdeal I
	or not isMonomialIdeal J
	or not isPolynomialRing R
	or not isCommutative R
	then return null;
	cast := if instance(I, MonomialIdeal) then identity else ideal;
	-- TODO: make sure (monomialIdeal, MonomialIdeal) isn't forgetful
	cast newMonomialIdeal(R, rawColon(raw monomialIdeal I, raw monomialIdeal J))),
    }

-- Installing hooks for Ideal : Ideal
scan({Quotient, Iterate-*, Linear*-, Monomial}, strategy ->
    addHook(key := (quotient, Ideal, Ideal), algorithms#key#strategy, Strategy => strategy))

--------------------------------------------------------------------
-- Algorithms for Module : Ideal
algorithms#(quotient, Module, Ideal) = new MutableHashTable from {
    Iterate => (opts, M, J) -> (
	-- This is the iterative version, where M is a
	-- submodule of F/K, or ideal, and J is an ideal.
	M1 := super M;
	m := generators M | relations M;
	scan(numgens J, i -> (
		f := J_i;
		if generators(f*M1) % m != 0 then (
		    M2 := quotient(M, f, opts, Strategy => Quotient);
		    M1 = intersect(M1, M2);
		    )));
	M1),

    Quotient => (opts, M, J) -> (
	m := generators M;
	F := target m;
	if M.?relations then m = m | M.relations;
	j := transpose generators J;
	g := (j ** F) | (target j ** m);
	-- We would like to be able to inform the engine that
	-- it is not necessary to compute various of the pairs
	-- of the columns of the matrix g.
	h := syz gb(g, opts,
	    Strategy   => LongPolynomial,
	    Syzygies   => true,
	    SyzygyRows => numgens F);
	if M.?relations then subquotient(h % M.relations, M.relations) else image h
	),

    -- TODO:
    Linear => (opts, M, J) -> (
	-- assumptions: J is a single linear element, and everything is homogeneous
	if not isHomogeneous M
	or not isHomogeneous J or not isLinearForm J_0
	then return null;
	stderr << "warning: quotient strategy Linear is not yet implemented" << endl; null),
    }

-- Installing hooks for Module : Ideal
scan({Quotient, Iterate-*, Linear*-}, strategy ->
    addHook(key := (quotient, Module, Ideal), algorithms#key#strategy, Strategy => strategy))

--------------------------------------------------------------------
-- Algorithms for Module : Module
algorithms#(quotient, Module, Module) = new MutableHashTable from {
    Iterate => (opts, I, J) -> (
	R := ring I;
	M1 := ideal 1_R;
	m := generators I | relations I;
	scan(numgens J, i -> (
		f := image (J_{i});
		-- it used to say f ** M1, but that can't have been right.
		-- I'm just guessing that M1 * f is better.  (drg)
		if generators(M1 * f) % m != 0
		then (
		    M2 := quotient(I, f, opts, Strategy => Quotient);
		    M1 = intersect(M1, M2);
		    )));
	M1),

    Quotient => (opts, M, J) -> (
	m := generators M;
	if M.?relations then m = m | M.relations;
	j := adjoint(generators J, (ring J)^1, source generators J);
	F := target m;
	g := j | (dual source generators J ** m);
	-- << g << endl;
	-- We would like to be able to inform the engine that
	-- it is not necessary to compute various of the pairs
	-- of the columns of the matrix g.
	h := syz gb(g, opts,
	    Strategy   => LongPolynomial,
	    Syzygies   => true,
	    SyzygyRows => 1);
	ideal h),
    }

-- Installing hooks for Module : Module
scan({Quotient, Iterate}, strategy ->
    addHook(key := (quotient, Module, Module), algorithms#key#strategy, Strategy => strategy))

--------------------------------------------------------------------
-- Saturations
--------------------------------------------------------------------
-- TODO:
-- - should saturate(I) use the irrelevant ideal when multigraded?

-- keys: the second ideal of saturation
SaturateOptions = new SelfInitializingType of QuotientOptions
SaturateOptions.synonym = "saturate options"

-- keys: TODO: BasisElementLimit, DegreeLimit, PairLimit
SaturateComputation = new Type of QuotientComputation
SaturateComputation.synonym = "saturate computation"

new SaturateComputation from Sequence := (C, S) -> (
    (A, B) := S;
    cacheKey := SaturateOptions{ mingens B };
    -- TODO: try to find other compatible cacheKeys, perhaps of type QuotientOptions, that can be used in the computation
    try A.cache#cacheKey else A.cache#cacheKey = new SaturateComputation from { Result => null })

-- TODO: isComputationDone and cacheComputation now can inherit from QuotientComputation,
-- but perhaps there is something smarter that can be done in this specific case

-- saturate = method(Options => options saturate) -- defined in m2/quotient.m2
saturate(Ideal,  Ideal)       := Ideal  => opts -> (I, J) -> saturateHelper(I, J, (saturate, Ideal, Ideal), opts)
saturate(Ideal,  Number)      :=
saturate(Ideal,  RingElement) := Ideal  => opts -> (I, f) -> saturateHelper(I, f, (saturate, Ideal, RingElement), opts)
saturate Ideal                := Ideal  => opts ->  I     -> saturate(I, ideal vars ring I, opts)

saturate(MonomialIdeal, RingElement) := MonomialIdeal => opts -> (I, f) -> (
    saturate(I, if size f === 1 and leadCoefficient f == 1 then monomialIdeal f else ideal f, opts))

saturate(Module, Ideal)       := Module => opts -> (M, J) -> saturateHelper(M, J, (saturate, Module, Ideal), opts)
saturate(Module, Number)      :=
saturate(Module, RingElement) := Module => opts -> (M, f) -> saturateHelper(M, f, (saturate, Module, RingElement), opts)
saturate Module               := Module => opts ->  M     -> saturate(M, ideal vars ring M, opts)

-- TODO: is M / saturate 0_M a correct computation of saturation of M?
saturate(Vector, Ideal)       := Module => opts -> (v, J) -> saturate(image matrix {v}, J, opts)
saturate(Vector, Number)      :=
saturate(Vector, RingElement) := Module => opts -> (v, f) -> saturate(image matrix {v}, f, opts)
saturate Vector               := Module => opts ->  v     -> saturate(image matrix {v}, opts)

-- used when P = decompose irr
saturate(Ideal,  List)        := Ideal  => opts -> (I, L) -> fold(L, I, (J, I) -> saturate(I, J, opts))
saturate(Module, List)        := Module => opts -> (M, L) -> fold(L, M, (J, M) -> saturate(M, J, opts))
saturate(Vector, List)        := Module => opts -> (v, L) -> saturate(image matrix {v}, L, opts)

-- TODO: the methods with "Number" should be unnecessary via https://github.com/Macaulay2/M2/issues/1519

-- Helper for saturation methods
saturateHelper = (A, B, key, opts) -> (
    if ring B === ZZ then B = sub(B, ring A);

    -- this is only here because some methods are not implemented for RingElement
    B' := if instance(B, RingElement) or instance(B, Number) then ideal B else B;

    strategy := opts.Strategy;
    doTrim := if opts.MinimalGenerators then trim else identity;

    -- this logic runs the strategies in order, or the specified strategy
    computation := (opts, container) -> (
	if (R := ring A) =!= ring B then error "expected objects in the same ring";
	-- note: if B \subset A then A:B^infty should be "everything", but computing
	-- a gb for A can be slow, so isSubset' doesn't compute a gb
	-- TODO: if radical A is cached and B \subset radical A then A : B^infty = ambient A
	-- alternatively, can radical containment be efficiently checked?
	if isSubset'(B', A) then return ambient' A;
	-- note: ideal(..A..) :            f^infty = A <==> f is nzd /A
	-- note: ideal(..A..) : ideal(..B..)^infty = A <==> B is not contained in any associated primes of A
	-- TODO: can either of the above be efficiently checked?
	if isSubset'(ambient' A, B') then return A;
	-- TODO: add speedup for when isSubset'(A, B'), being cautious of (a3):(a3,bc) in kk[abc]/ac

	opts = new OptionTable from override'(options gb, opts ++ {Strategy => null});
	runHooks(key, (opts, A, B), Strategy => strategy));

    -- this is the logic for caching partial saturation computations. A.cache contains an option:
    --   SaturateOptions{ mingens B } => SaturateComputation{ Result }
    container := new SaturateComputation from (A, B');

    -- the actual computation of saturation occurs here
    C := (cacheComputation(opts, container)) computation;

    if C =!= null then doTrim C else if strategy === null
    then error("no applicable strategy for ", toString key)
    else error("assumptions for saturation strategy ", toString strategy, " are not met"))

-- Helper for GRevLex strategy
saturationByGRevLexHelper := (I, v, opts) -> (
    R := ring I;
    (R1, fto, fback) := grevLexRing(index v, R);
    g1 := groebnerBasis(fto I, Strategy => "F4");
    (g1', maxpower) := divideByVariable(g1, R1_(numgens R1 - 1));
    if maxpower == 0 then (I, 0) else (ideal fback g1', maxpower))

--------------------------------------------------------------------
-- Algorithms for Module : Ideal^infinity
algorithms#(saturate, Module, Ideal) = new MutableHashTable from {
    Iterate => (opts, M, I) -> (
	M' := quotient(M, I, opts); while M' != M do ( M = M'; M' = quotient(M, I, opts) ); M ),

    -- TODO: if decompose is already cached, do this strategy instead
    Decompose => (opts, M, I) -> saturate(M, decompose I, Strategy => Iterate),
    }

-- Installing hooks for Module : Ideal^infinity
scan({Decompose, Iterate}, strategy ->
    addHook(key := (saturate, Module, Ideal), algorithms#key#strategy, Strategy => strategy))

--------------------------------------------------------------------
-- Algorithms for Module : RingElement^infinity
algorithms#(saturate, Module, RingElement) = new MutableHashTable from {
    Iterate => (opts, M, f) -> (
	M' := quotient(M, f, opts); while M' != M do ( M = M'; M' = quotient(M, f, opts) ); M ),
    }

-- Installing hooks for Module : RingElement^infinity
scan({Iterate}, strategy ->
    addHook(key := (saturate, Module, RingElement), algorithms#key#strategy, Strategy => strategy))

--------------------------------------------------------------------
-- Algorithms for Ideal : Ideal^infinity
algorithms#(saturate, Ideal, Ideal) = new MutableHashTable from {
    -- TODO: this is sometimes faster than Eliminate
    Iterate => (opts, I, J) -> (
	R := ring I;
	m := transpose generators J;
	while (
	    S := (ring I)/I;
	    m = m ** S;
	    I = ideal syz gb(m, Syzygies => true);
	    I != 0) do ();
	ideal (presentation ring I ** R)),

    Eliminate => (opts, I, J) -> intersect apply(J_*, g -> saturate(I, g, opts)),

    GRevLex => (opts, I, J) -> (
	-- FIXME: this might not be necessary, but the code isn't designed for this case.
	if not isFlatPolynomialRing ring I
	or not isHomogeneous I
	or not isHomogeneous J
	or not isGRevLexRing ring I
	then return null;
	-- First check that all generators are variables of the ring
	-- TODO: can this strategy work with generators of the irrelevant ideal?
	if any(index \ J_*, v -> v === null) then return null;
	-- Saturate with respect to each variable separately
	L := for g in J_* list saturationByGRevLexHelper(I, g, opts);
	-- Intersect them all
	-- TODO: when exactly is I returned?
	if any(last \ L, x -> x == 0) then I
	else intersect(first \ L)),

    Monomial => (opts, I, J) -> (
	R := ring I;
	if not isMonomialIdeal I
	or not isMonomialIdeal J
	or not isPolynomialRing R
	or not isCommutative R
	then return null;
	cast := if instance(I, MonomialIdeal) then identity else ideal;
	-- TODO: make sure (monomialIdeal, MonomialIdeal) isn't forgetful
	cast newMonomialIdeal(R, rawSaturate(raw monomialIdeal I, raw monomialIdeal J))),
    }

-- Installing hooks for Ideal : Ideal^infinity
scan({Iterate, Eliminate, GRevLex, Monomial}, strategy ->
    addHook(key := (saturate, Ideal, Ideal), algorithms#key#strategy, Strategy => strategy))

--------------------------------------------------------------------
-- Algorithms for Ideal : RingElement^infinity
algorithms#(saturate, Ideal, RingElement) = new MutableHashTable from {
    Iterate => (opts, I, f) -> saturate(I, ideal f, opts ++ {Strategy => Iterate}), -- backwards compatibility
    Linear => (opts, I, f) -> (
	-- assumptions for this case:
	--   (1) the ring is of the form k[x1..xn].  No quotients, k a field or ZZ, grevlex order
	--   (2) all variables have degree 1.
	--   (3) I is homogeneous
	--   (4) f = homog linear form
	R := ring I;
	if not isFlatPolynomialRing R
	or not isGRevLexRing R
	or not all(gens R, g -> sum degree g == 1)
	or not isHomogeneous I
	or not isHomogeneous f or not isLinearForm f
	then return null;
	-- TODO: what does this do?
	res := newCoordinateSystem(R, matrix{{f}});
	fto := res#1;
	fback := res#0;
	v := R_(numgens R - 1);
	g := gens gb(fto I, opts);
	ideal fback first divideByVariable(g, v)),

    Bayer => (opts, I, f) -> (
	-- Bayer method. This may be used if I, f are homogeneous.
	-- Basic idea: in a ring R[z]/(f - z), with the RevLex order, compute GB of I.
	-- assumptions for this case:
	--   (1) the ring is of the form k[x1..xn].  No quotients, k a field or ZZ
	--   (2) J is homogeneous
	--   (3) I = homog, generated by one element
	R := ring I;
	if not isFlatPolynomialRing R
	or not isHomogeneous I
	or not isHomogeneous f or sum degree f === 0
	then return null;
	n := numgens R;
	degs := append(degrees R, degree f);
	X := local X;
	R1 := (coefficientRing R)[X_0 .. X_n, Degrees => degs, MonomialSize => 16];
	i  := map(R1, R, (vars R1)_{0..n-1});
	f1 := i f;
	I1 := ideal (i generators I);
	A  := R1/(f1 - R1_n); -- TODO: add to ideal instead of quotient?
	iback := map(R, A, vars R | f);
	IA := generators I1 ** A;
	g := groebnerBasis(IA, Strategy => "F4"); -- TODO: compare with MGB
	(g1, notused) := divideByVariable(g, A_n);
	ideal iback g1),

    Eliminate => (opts, I, f) -> (
	-- Eliminate(t, (I, t * f - 1))
	-- assumptions for this case:
	--  I is an ideal in a flat polynomial ring (ring of the form k[x1..xn], no quotients, k a field or ZZ)
	--  f is an ideal, generated by one elem
	R := ring I;
	if not isFlatPolynomialRing R then return null;
	(R1, fto, fback) := eliminationInfo R;
	J := ideal(R1_0 * fto f - 1) + fto I;
	g := groebnerBasis(J, Strategy => "F4"); -- TODO: compare with MGB
	p1 := selectInSubring(1, g);
	ideal fback p1),

    GRevLex => (opts, I, v) -> (
	-- FIXME: this might not be necessary, but the code isn't designed for this case.
	if not isHomogeneous I
	or not isFlatPolynomialRing ring I
	or not isGRevLexRing ring I
	then return null;
	-- First check that v is a variable of the ring
	-- TODO: can this strategy work with generators of the irrelevant ideal?
	if index v === null then return null;
	-- Saturate with respect to each variable separately
	first saturationByGRevLexHelper(I, v, opts)),

    "Unused" => (opts, I, f) -> (
	-- NOT USED; TODO: assumptions?
	R := ring I;
	I1 := ideal 1_R;
	while I1 != I do (
	    I1 = I;
	    I = ideal syz gb(matrix{{f}} | generators I,
		Syzygies   => true,
		SyzygyRows => 1)
	    );
	I)
    }

-- Installing hooks for Ideal : RingElement^infinity
scan({"Unused", Iterate, Eliminate, GRevLex, Bayer, Linear}, strategy ->
    addHook(key := (saturate, Ideal, RingElement), algorithms#key#strategy, Strategy => strategy))

--------------------------------------------------------------------
-- isSupportedInZeroLocus
--------------------------------------------------------------------

-- Note: this function isn't cached because the usecase in VirtualResolutions doesn't
-- require it but it does take advantage of precomputed saturation of the annihilator
isSupportedInZeroLocus = method()
isSupportedInZeroLocus(Ideal, Ideal)  := (B, I) -> isSupportedInZeroLocus(B, comodule I)
isSupportedInZeroLocus(Ideal, Module) := (B, M) -> (
    -- Returns true if M is supported only on the zero locus of B
    S := ring B;
    -- There are two ways to check this:
    -- 1. saturate the annihilator of M with respect to B and compare it to ideal(1)
    -- In general, computing the saturation is unnecessary, but when the saturation of the
    -- annihilator is known, it's faster to check whether saturate(annihilator M, B) == ideal 1
    if M.cache.?annihilator then (
	N := annihilator M;
	container := new SaturateComputation from (N, B);
	try isComputationDone container then ( cacheHit class container; return saturate(N, B) == ideal 1_S ) else true);
    -- 2. check that a high enough power of elements of B annihilates M
    n := numgens S;
    all(first entries mingens B, g -> (
	supp := index \ support g;
	perm := toList(set(0..n-1) - set supp) | supp;
	R := (coefficientRing S)[(S_*)_perm,
	    MonomialSize  => 16,
	    MonomialOrder => {Position => Up, n - #supp, #supp}];
	-- TODO: can we compute gb once at the top instead?
	G := groebnerBasis(sub(presentation M, R), Strategy => "F4"); -- TODO: try "MGB"
	-- TODO: is "ann coker" necessary?
	0 != ann coker selectInSubring(1, leadTerm G))))

--------------------------------------------------------------------
-- Annihilators
--------------------------------------------------------------------

-- annihilator = method(Options => {Strategy => null}) -- defined in m2/quotient.m2
annihilator RingElement := Ideal => opts -> f -> annihilator(ideal f,  opts)
annihilator Ideal       := Ideal => opts -> I -> annihilator(module I, opts)
annihilator Module      := Ideal => opts -> (cacheValue symbol annihilator) (
    M -> annihilatorHelper(M, (annihilator, Module), opts))

-- Helper for annihilator methods
annihilatorHelper = (A, key, opts) -> (
    R := ring A;
    if isWeylAlgebra R then error "annihilator has no meaning for objects over a Weyl algebra";
    -- TODO: add more instant checks
    f := presentation A;
    -- TODO: is this any different from A == 0?
    F := target f;
    if numgens F === 0 then return ideal 1_R;

    strategy := opts.Strategy;

    C := runHooks(key, (opts, A), Strategy => strategy);

    if C =!= null then C else if strategy === null
    then error("no applicable strategy for ", toString key)
    else error("assumptions for annihilator strategy ", toString strategy, " are not met"))

-- Algorithms for annihilator Module
algorithms#(annihilator, Module) = new MutableHashTable from {
    -- TODO: annihilator routines for MonomialIdeals?
    Quotient     => (opts, M) -> (
	f := presentation M;
	image f : target f),
    Intersection => (opts, M) -> (
	f := presentation M;
	F := target f;
	intersect apply(numgens F, i -> ideal modulo(F_{i}, f))),
    }

-- Installing hooks for annihilator Module
scan({Quotient, Intersection}, strategy ->
    addHook(key := (annihilator, Module), algorithms#key#strategy, Strategy => strategy))

--------------------------------------------------------------------
----- Development section
--------------------------------------------------------------------

saturationByGRevLex     = (I,J) -> saturate(I, J, Strategy => GRevLex)
saturationByElimination = (I,J) -> saturate(I, J, Strategy => Eliminate)
intersectionByElimination =  L  -> intersect(L,   Strategy => Eliminate)

--------------------------------------------------------------------
----- Tests section
--------------------------------------------------------------------

load "./Saturation/quotient-test.m2"
load "./Saturation/saturate-test.m2"
load "./Saturation/annihilator-test.m2"

--------------------------------------------------------------------
----- Documentation section
--------------------------------------------------------------------

beginDocumentation()

-- TODO: review
load "./Saturation/doc.m2"
load "./Saturation/quotient-doc.m2"
load "./Saturation/saturate-doc.m2"
load "./Saturation/annihilator-doc.m2"

end--

restart
debugLevel = 1
debug needsPackage "Saturation"

kk = ZZ/32003
R = kk(monoid[x_0, x_1, x_2, x_3, x_4, Degrees => {2:{1, 0}, 3:{0, 1}}, Heft => {1,1}])
B0 = ideal(x_0,x_1)
B1 = ideal(x_2,x_3,x_4)

I = ideal(x_0^2*x_2^2*x_3^2+44*x_0*x_1*x_2^2*x_3^2+2005*x_1^2*x_2^2*x_3^2+12870
     *x_0^2*x_2*x_3^3-725*x_0*x_1*x_2*x_3^3-15972*x_1^2*x_2*x_3^3-7768*x_0^2*x_2
     ^2*x_3*x_4-13037*x_0*x_1*x_2^2*x_3*x_4-14864*x_1^2*x_2^2*x_3*x_4+194*x_0^2*
     x_2*x_3^2*x_4-2631*x_0*x_1*x_2*x_3^2*x_4-2013*x_1^2*x_2*x_3^2*x_4-15080*x_0
     ^2*x_3^3*x_4-9498*x_0*x_1*x_3^3*x_4+5151*x_1^2*x_3^3*x_4-12401*x_0^2*x_2^2*
     x_4^2+4297*x_0*x_1*x_2^2*x_4^2-13818*x_1^2*x_2^2*x_4^2+7330*x_0^2*x_2*x_3*x
     _4^2-13947*x_0*x_1*x_2*x_3*x_4^2-12602*x_1^2*x_2*x_3*x_4^2-14401*x_0^2*x_3^
     2*x_4^2+8101*x_0*x_1*x_3^2*x_4^2-1534*x_1^2*x_3^2*x_4^2+8981*x_0^2*x_2*x_4^
     3-11590*x_0*x_1*x_2*x_4^3+1584*x_1^2*x_2*x_4^3-13638*x_0^2*x_3*x_4^3-5075*x
     _0*x_1*x_3*x_4^3-14991*x_1^2*x_3*x_4^3,x_0^7*x_2-6571*x_0^6*x_1*x_2+13908*x
     _0^5*x_1^2*x_2+11851*x_0^4*x_1^3*x_2+14671*x_0^3*x_1^4*x_2-14158*x_0^2*x_1^
     5*x_2-15190*x_0*x_1^6*x_2+6020*x_1^7*x_2+5432*x_0^7*x_3-8660*x_0^6*x_1*x_3-
     3681*x_0^5*x_1^2*x_3+11630*x_0^4*x_1^3*x_3-4218*x_0^3*x_1^4*x_3+6881*x_0^2*
     x_1^5*x_3-6685*x_0*x_1^6*x_3+12813*x_1^7*x_3-11966*x_0^7*x_4+7648*x_0^6*x_1
     *x_4-10513*x_0^5*x_1^2*x_4+3537*x_0^4*x_1^3*x_4+2286*x_0^3*x_1^4*x_4+733*x_
     0^2*x_1^5*x_4+11541*x_0*x_1^6*x_4+660*x_1^7*x_4);

--              B0       B1
-- GRevLex      25.95s   0.18s
-- Eliminate    28.35s   0.29s
-- Iterate      60.02s   0.05s
for B in {B0, B1} do (
    for strategy in {GRevLex, Eliminate, Iterate} do
    print(strategy, (try elapsedTime saturate(I, B, Strategy => strategy);)))

ans1 = elapsedTime saturationByGRevLex(saturationByGRevLex(I, B0), B1); -- 25.53s
ans2 = elapsedTime saturationByGRevLex(saturationByGRevLex(I, B1), B0); -- 22.93s

elapsedTime saturationByGRevLex(I, x_0); -- 9.01s
elapsedTime saturationByGRevLex(I, x_1); -- 8.77s

-- TODO: what a discrepency
ans3 = elapsedTime saturationByElimination(saturationByElimination(I, B0), B1); -- 49.22s
ans4 = elapsedTime saturationByElimination(saturationByElimination(I, B1), B0); -- 28.63


elapsedTime J1 = saturationByElimination(I, x_0);
elapsedTime J2 = saturationByElimination(I, x_1);
elapsedTime J = intersectionByElimination(J1,J2);
elapsedTime J' = intersectionByElimination(J2,J1);
elapsedTime J'' = intersect(J1,J2);
elapsedTime J''' = intersect(J2,J1);
J == J'
J == J''

time gens gb I;
J2 = elapsedTime saturationByElimination(I, x_0);
assert isHomogeneous J2
J2' = elapsedTime saturationByElimination(I, x_1);

J2 = elapsedTime saturationByElimination(I, ideal(x_0,x_1));
J2' = elapsedTime saturationByElimination(J2, ideal(x_2,x_3,x_4));

J1 = elapsedTime saturate(I, x_0);
J1' = elapsedTime saturate(I, x_1);
J1 == J2
J1' == J2'

betti J2
betti J1

restart
load "./Saturation/badsaturations.m2"

-- TODO: how was this so fast before??
J = paramRatCurve({2,2},{3,3},{4,2});
elapsedTime genSat(J,2) -- 200 sec
elapsedTime genSat2(J,2) -- 50 sec
elapsedTime genSat3(J,2) -- 35 sec

J = paramRatCurve({2,2},{3,3},{5,2});
elapsedTime genSat(J,2) -- 691 sec
elapsedTime genSat2(J,2) -- 104 sec
elapsedTime genSat3(J,2) -- 71 sec

J = paramRatCurve({2,2},{3,4},{4,3});
elapsedTime genSat(J,2) --  sec
elapsedTime genSat2(J,2) --  sec
elapsedTime genSat3(J,2) -- 75 sec

I = ideal J_*_{5,13}
use ring I
elapsedTime I1 = saturate(I, x_0);
elapsedTime (I2,pow) = saturationByGRevLex(I,x_0);
I1 == I2

elapsedTime I1 = saturate(I, x_1);
elapsedTime (I2,pow) = saturationByGRevLex(I,x_1);
I1 == I2
elapsedTime J1 = intersectionByElimination(I1,I2);

elapsedTime I1 = saturationByGRevLex(I, B0);
elapsedTime I2 = saturationByGRevLex(I1, B1);

elapsedTime saturationByGRevLex(saturationByGRevLex(I, B0), B1);
elapsedTime saturationByGRevLex(saturationByGRevLex(I, B1), B0);

elapsedTime saturationByElimination(saturationByElimination(I, B0), B1);
elapsedTime saturationByElimination(saturationByElimination(I, B1), B0);

elapsedTime J0a = saturationByGRevLex(I,x_0);
elapsedTime J0b = saturationByGRevLex(I,x_1);
--J1 = elapsedTime intersectionByElimination(first J0a,first J0b);
La = elapsedTime trim first J0a;
Lb = elapsedTime trim first J0b;
J1 = elapsedTime intersectionByElimination(La, Lb);
J1a = elapsedTime saturationByGRevLex(J1,x_2);
J1b = elapsedTime saturationByGRevLex(J1,x_3);
J1c = elapsedTime saturationByGRevLex(J1,x_4);
J1a#1, J1b#1, J1c#1
J1ab = elapsedTime intersectionByElimination(J1a,J1b);
elapsedTime J2 = intersectionByElimination{first J1a, first J1b, first J1c};
elapsedTime saturationByGRevLex(I,B0);

saturationByElimination(I,x_0);

(R1,fto,fback) = grevLexRing(0,S)
L = fto I;
satL = ideal first divideByVariable(gens gb L, R1_4);
fback satL
oo == I1
leadTerm oo
ideal oo
(R1,fto,fpack) = grevLexRing(1,S)
use S

R = ZZ/101[a..d]
I = ideal"ab-ac,b2-cd"
I1 = saturate(I,a)
elapsedTime (I2,pow) = saturationByGRevLex(I,a);
I1 == I2
pow
(R1,fto,fback) = grevLexRing(0,R)
fto I
fto

----------------------------
-- Benchmarking example:
restart
needsPackage "Saturation"

R = ZZ/101[vars(0..14)]
M = genericMatrix(R, a, 3, 5)
I = minors(3, M);
codim I
d = 4
J = ideal((gens I) * random(R^10, R^d));

-- algorithm; d =   2    3    4    5
-- null          0.45   40
-- Linear         N/A  N/A  N/A  N/A
-- Iterate       0.41   40
-- Quotient        22  271
elapsedTime J'  = quotient(J, I);
for strategy in {Linear, Iterate, Quotient} do
print(strategy, (try (elapsedTime J'  === quotient(J, I, Strategy => strategy)) else "not applicable"))

-- algorithm; d =   2    3    4    5
-- null          0.45  430
-- GRevLex        N/A  N/A  N/A  N/A
-- Eliminate     2.87  378
-- Iterate         20  575
elapsedTime J'' = saturate(J, I);
for strategy in {GRevLex, Eliminate, Iterate} do
print(strategy, (try (elapsedTime J'' === saturate(J, I, Strategy => strategy)) else "not applicable"))

elapsedTime quotient(J, I, Strategy => Iterate);
elapsedTime saturate(J, I, Strategy => Eliminate);

degree I
elapsedTime(J : I_0);
