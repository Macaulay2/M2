-- TODO: move this to an appropriate package
-- TODO: now we can defined intersect for Set, CoherentSheaf, etc.
-- TODO: add tests
-- TODO: add intersection with a ring, via selectInSubring
-- TODO: how to cache partial computation?

needs "matrix1.m2"
needs "shared.m2"

-- This is a map from method keys to strategy hash tables
algorithms := new MutableHashTable from {}

-----------------------------------------------------------------------------
-- utilities
-----------------------------------------------------------------------------

-- TODO: can this be simplified using newRing?
-- also used in Saturation package
eliminationInfo = method()
eliminationInfo Ring := (cacheValue symbol eliminationInfo) (R -> (
	X := local X;
	n := numgens R;
	R1 := (coefficientRing R)[X_0..X_n, MonomialOrder => Eliminate 1, MonomialSize => 16];
	fto := map(R1, R, drop(generators R1, 1));
	fback := map(R, R1, matrix{{0_R}} | vars R);
	(R1, fto, fback)))

-----------------------------------------------------------------------------
-- General intersect method
-----------------------------------------------------------------------------

-- intersect is now declared as a binary associative method in shared.m2,
-- so the methods (intersect, Sequence) and (intersect, List) are pre-installed.
-- Here, we override both to check for specializations for handling uniform lists.
-- As a backup, at the end the raw binary function closure is called by using
-- a VisibleList as input instead of List or Sequence.
intersect List     :=
intersect Sequence := true >> opts -> L -> (
    -- If the arguments are of the same type, we look for a specialized
    -- function based on the output type rather than the input types.
    if uniform L then (
	type := class L#0; -- type of the result; e.g. Module
	func := lookup(symbol intersect, type); -- e.g. Module.intersect
	if func =!= null then return func(opts, L));
    intersect(opts, new VisibleList from L))

-----------------------------------------------------------------------------
-- Intersection of ideals and modules
-----------------------------------------------------------------------------

doTrim := (opts, C) -> if opts.MinimalGenerators then trim C else C;

intersectHelper := (L, key, opts) -> (
    -- For now, this is only for intersection of ideals and modules
    -- TODO: this line may need to move, but otherwise this helper can be used for any class
    if not same apply(L, ring) then error "intersect: expected objects with the same ring";
    C := runHooks(key, (opts, L), Strategy => (strategy := opts.Strategy));
    if C =!= null then doTrim(opts, C) else if strategy === null
    then error("no applicable method for ", toString key)
    else error("assumptions for intersect strategy ", toString strategy, " are not met"))

-----------------------------------------------------------------------------

idealIntersectOpts :=
moduleIntersectOpts := {
    Strategy          => null,
    MinimalGenerators => true
    }

-- ideally this should be unnecessary, but some code seems to depend on this
intersect Ideal  := Ideal  =>  idealIntersectOpts >> opts -> I -> doTrim(opts, I)
intersect Module := Module => moduleIntersectOpts >> opts -> M -> doTrim(opts, M)

-- intersect is a MethodFunctionBinary, so arbitrary lists
-- or sequences are handled associatively from left, that is:
--   installing a method (intersect, T, T) => T enables intersect(T, T, ...)
--   installing a method (intersect, S, T) => T enables intersect(S, T, T, ...)
--   installing a method (intersect, S, S) => S enables intersect(S, S, T, T, ...)
--   installing a method (intersect, T, S) => S enables intersect(S, T, S, T, ...)
intersect(Ideal,  Ideal)  :=  Ideal =>  idealIntersectOpts >> opts -> L -> intersectHelper(L, (intersect, Ideal,  Ideal),  opts)
intersect(Module, Module) := Module => moduleIntersectOpts >> opts -> L -> intersectHelper(L, (intersect, Module, Module), opts)

-- Specializations for intersecting many objects at once, e.g. Modules,
-- can be installed on (symbol intersect, T), which calls T.intersect
Ideal.intersect  =  idealIntersectOpts >> opts -> L -> intersectHelper(L, (intersect, Ideal,  Ideal),  opts)
Module.intersect = moduleIntersectOpts >> opts -> L -> intersectHelper(L, (intersect, Module, Module), opts)

-----------------------------------------------------------------------------

-- The algorithm below is optimized for intersecting all modules at once.
algorithms#(intersect, Module, Module) = new MutableHashTable from {
    Default => (opts, L) -> (
	M := L#0;
	R := ring M;
	-- check that the modules are compatible
	if not same apply(L, ambient)
	or not same apply(L, N -> N.?relations)
	or not same apply(L, N -> N.?relations and (N.relations == M.relations or image N.relations == image M.relations))
	then error "intersect: all modules must be submodules of the same module";
	--
	relns := directSum apply(L, N -> if N.?relations then generators N | N.relations else generators N);
	g := map(R^(#L), R^1, table(#L, 1, x -> 1)) ** id_(ambient M);
	h := modulo(g, relns);
	--
	if M.?relations then h = compress( h % M.relations );
	subquotient( h, if M.?relations then M.relations )),
    }

-- Installing hooks for intersect(Module, Module)
scan({Default}, strategy ->
    addHook(key := (intersect, Module, Module), algorithms#key#strategy, Strategy => strategy))

-----------------------------------------------------------------------------

algorithms#(intersect, Ideal, Ideal) = new MutableHashTable from {
    Default => (opts, L) -> ideal intersect(opts, apply(L, module)),

    -- TODO: can this be extended to do more than 2 at once?
    "Elimination" => (opts, L) -> (
	R := ring L#0;
	-- TODO: is this the right assumption? would a quotient ring work?
	if not isPolynomialRing R
	or not isCommutative R
	-- or not (isField(kk := coefficientRing R) or kk === ZZ)
	then return null;
	(R', fto, fback) := eliminationInfo R;
	fold(L, (I, J) -> (
		I' := R'_0 * fto I;
		J' := (1 - R'_0) * fto J;
		U := I' + J';
		g := generators gb U;
		--g := groebnerBasis(U, Strategy => "MGB"); -- TODO: try "MGB"
		--g := groebnerBasis(U, Strategy => "F4"); -- TODO: this failed check_4 PushForward
		fback ideal selectInSubring(1, g)))),

    Monomial => (opts, L) -> (
	R := ring L#0;
	if not isPolynomialRing R
	or not isCommutative R
	or not all(L, isMonomialIdeal)
	then return null;
	-- TODO: make rawIntersect return MonomialIdeal when inputs are MonomialIdeals, then simplify this
	cast := if instance(L#0, MonomialIdeal) then monomialIdeal else ideal;
	cast generators fold(L, (I, J) ->
	    newMonomialIdeal(R, rawIntersect(raw monomialIdeal I, raw monomialIdeal J)))),
    }

-- Installing hooks for intersect(Ideal, Ideal)
scan({Default, "Elimination", Monomial}, strategy ->
    addHook(key := (intersect, Ideal, Ideal), algorithms#key#strategy, Strategy => strategy))
