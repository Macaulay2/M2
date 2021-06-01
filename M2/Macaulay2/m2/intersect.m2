-- TODO: move this to an appropriate package
-- TODO: now we can defined intersect for Set, CoherentSheaf, etc.
-- TODO: add tests
-- TODO: add intersection with a ring, via selectInSubring
-- TODO: how to cache partial computation?

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

intersect List     :=
intersect Sequence := -* [same as input type] => *- true >> opts -> L -> (
    if not #L > 0 then error "intersect: expected at least one object";
    -- This will be the type of the result.
    type :=
    -- TODO: simplify this by either removing MonomialIdeal from top level
    -- or write a function to find a common category for a list of objects
    if all(L, l -> instance(l, Ideal))  then class L#0 else -- either Ideal or MonomialIdeal
    if uniform toList L                 then class L#0
    else error "intersect: expected objects of the same type";
    -- In a sense, the method is installed on the output type, not the input types
    -- e.g. Module.intersect
    func := lookup(symbol intersect, type);
    if func =!= null then func(opts, L)
    else error("intersect: no method for objects of type " | synonym type))

-----------------------------------------------------------------------------
-- Intersection of ideals and modules
-----------------------------------------------------------------------------

intersectHelper := (L, key, opts) -> (
    -- For now, this is only for intersection of ideals and modules
    -- TODO: this line may need to move, but otherwise this helper can be used for any class
    if not same apply(L, ring) then error "intersect: expected objects in the same ring";

    strategy := opts.Strategy;
    doTrim := if opts.MinimalGenerators then trim else identity;

    C := runHooks(key, (opts, L), Strategy => strategy);

    if C =!= null then doTrim C else if strategy === null
    then error("no applicable method for ", toString key)
    else error("assumptions for intersect strategy ", toString strategy, " are not met"))

-----------------------------------------------------------------------------

idealIntersectOpts :=
moduleIntersectOpts := {
    Strategy          => null,
    MinimalGenerators => true
    }

intersect Ideal  := Ideal  =>  idealIntersectOpts >> opts -> identity
intersect Module := Module => moduleIntersectOpts >> opts -> identity

Ideal.intersect  =  idealIntersectOpts >> opts -> L -> intersectHelper(L, (intersect, Ideal,  Ideal),  opts)
Module.intersect = moduleIntersectOpts >> opts -> L -> intersectHelper(L, (intersect, Module, Module), opts)

-----------------------------------------------------------------------------

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
