-- Copyright 1996 Michael E. Stillman
-- TODO: adjust PushForward.m2 package to add a strategy
-- TODO: lift(M, S) should call pushForward in some cases

needs "basis.m2"
needs "modules.m2"
needs "ringmap.m2"

-----------------------------------------------------------------------------
-- Local variables
-----------------------------------------------------------------------------

algorithms := new MutableHashTable from {}

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

inf := t -> if t === infinity then -1 else t

-----------------------------------------------------------------------------
-- helpers for pushForward
-----------------------------------------------------------------------------

-- valid values are (a) {J,cleanup code}   -- J is the aux matrix and the code to derive
--						the answer is the 2nd element.
--               or (b) {answer}           -- answer is a matrix

--subringOptions := mergeOptions(options gb, Strategy => , UseHilbertFunction => true

-----------------------------------------------------------------------------
-- pushForward
-----------------------------------------------------------------------------

pushForward = method (
    Options => {
	MonomialOrder         => Eliminate, -- default is an elimination order
	UseHilbertFunction    => true,      -- if possible
	StopBeforeComputation => false,
	DegreeLimit           => {},
	PairLimit             => infinity,
	Strategy              => null,      -- use the best choice, used to be NonLinear
	-- unused options:
	-- BasisElementLimit         => infinity,  -- number of generators of GB in the subring
	-- StopWithMinimalGenerators => false      -- determine the minimal generators of the subring
	}
    )

-- keys:
PushforwardContext = new SelfInitializingType of Context
PushforwardContext.synonym = "pushforward context"

new PushforwardContext from RingMap := (C, f) -> PushforwardContext{f}

-- keys:
PushforwardComputation = new Type of Computation
PushforwardComputation.synonym = "pushforward computation"

-- TODO:
new PushforwardComputation from Sequence := (C, S) -> new PushforwardComputation from {
    MonomialOrder         => Eliminate, -- default is an elimination order
    UseHilbertFunction    => true,      -- if possible
    StopBeforeComputation => false,
    DegreeLimit           => {},
    PairLimit             => infinity,
    Result                => null}

isComputationDone PushforwardComputation := Boolean => options pushForward >> opts -> container -> (
    -- MES: There is NO way to check this yet!!
    instance(container.Result, Module)
    and ( opts.StopBeforeComputation == container.StopBeforeComputation or not container.StopBeforeComputation )
    and opts.DegreeLimit <= container.DegreeLimit
    and opts.PairLimit   <= container.PairLimit)

updateComputation(PushforwardComputation, Module) := Module => options pushForward >> opts -> (container, result) -> (
    container.StopBeforeComputation =
	 opts.StopBeforeComputation;
    container.DegreeLimit = opts.DegreeLimit;
    container.PairLimit   = opts.PairLimit;
    container.Result      = result)

-----------------------------------------------------------------------------

pushForward(RingMap, Module) := Module => opts -> (f, M) -> (
    R := ring M;
    assert( target f === R );
    strategy := opts.Strategy;

    -- this logic runs the strategies in order, or the specified strategy
    computation := (opts, container) -> (
	runHooks((pushForward, RingMap, Module), (opts, f, M), Strategy => strategy));

    -- this is the logic for caching partial pushforward computations. M.cache contains an option:
    --   PushforwardContext{} => PushforwardComputation{ Result, ... }
    container := fetchComputation(PushforwardComputation, M, (f, M), new PushforwardContext from f);

    -- the actual computation of the pushforward occurs here
    C := (cacheComputation(opts, container)) computation;

    if C =!= null then C else if strategy === null
    then error("no applicable strategy for computing pushforward")
    else error("assumptions for pushForward strategy ", toString strategy, " are not met"))

-----------------------------------------------------------------------------
-- strategies for pushForward
-----------------------------------------------------------------------------

ordertab := new HashTable from {
    Eliminate    => (nR, nS) -> Eliminate nR,
    ProductOrder => (nR, nS) -> ProductOrder{nR, nS},
    Lex          => (nR, nS) -> Lex,
    }

pushNonLinear := (opts, f, M) -> (
    -- given f: R --> S, and M an S-module, finite over R,
    -- returns R-presentation matrix for the pushforward of M
    -- written by Mike Stillman and David Eisenbud
    (R, S) := (target f, source f);
    deglen := degreeLength S;
    n1 := numgens R; -- TODO: what if R is a tower?

    monorder := opts.MonomialOrder;
    monorder  = if ordertab#?monorder then (ordertab#monorder)(numgens R, numgens S)
    else error("pushForward: MonomialOrder option expected one of ",
	demark_", " \\ toString \ keys ordertab);

    J := graphIdeal(f, MonomialOrder => monorder, VariableBaseName => local X);
    G := ring J;
    m := presentation M;
    xvars := map(G, R, submatrix(vars G, toList(0..n1-1)));
    m1 := presentation (cokernel xvars m  **  cokernel generators J);

    if opts.UseHilbertFunction and all({f, m}, isHomogeneous) then (
	-- compare with kernel RingMap
	hf := poincare cokernel m;
	T := degreesRing G;
	hf = hf * product(degrees source generators J, d -> 1 - T_d);
	-- cache poincare
	poincare cokernel m1 = hf);

    mapbackdeg := d -> take(d, -deglen);
    -- that choice of degree map was chosen to make the symmetricPower functor homogeneous, but it doesn't have much
    -- else to recommend it.
    -- we should really be *lifting* the result to S along the natural map S ---> G
    mapback := map(S, G, map(S^1, S^n1, 0) | vars S, DegreeMap => mapbackdeg );

    -- let's at least check it splits f's degree map:
    for i from 0 to deglen-1 do (
	e := for j from 0 to deglen-1 list if i === j then 1 else 0;
	if mapbackdeg f.cache.DegreeMap e =!= e
	then error "not implemented yet: unexpected degree map of ring map");

    g := gb(m1,
	StopBeforeComputation => opts.StopBeforeComputation,
	DegreeLimit           => opts.DegreeLimit,
	PairLimit             => opts.PairLimit);
    -- MES: check if the monomial order restricts to S.  If so, then do `` forceGB result ''
    mapback selectInSubring(if numgens target f > 0 then 1 else 0, generators g))

-*
pushLinear := opts -> (f,M) -> (
    -- assumptions here:
    -- (a) f is homogeneous linear, and the linear forms are independent
    --
    -- First bring M over to a ring with an elimination order, which eliminates
    -- the variables 'not in' f.
    m := presentation M;
    R := target f;
    S := source f;
    Rbase := ring m;
    fmat := lift(f.matrix,Rbase);
    n := numgens source f.matrix;
    n1 := numgens R - n;
    k := coefficientRing Rbase;
    X := local X;
    N := monoid [VariableBaseName => X, Variables => numgens R, MonomialOrder => Eliminate n1];
    R1 := k N;
    (Fto,Ffrom) := newCoordinateSystem(R1, fmat);
    m1 := Fto m;
    m1 = presentation (cokernel m1 ** cokernel Fto presentation R);
    if isHomogeneous f and isHomogeneous m then (
        hf := poincare cokernel m;
        T := (ring hf)_0;
        poincare cokernel m1 = hf;
        );
    gbopts := applyPairs(gbDefaults, (k,v) -> if opts#?k and k =!= Strategy then (k,opts#k) else (k,v));
    g := selectInSubring(1, generators gb(m1,gbopts));
    -- now map the answer back to S = source f.
    mapback := map(S, R1, map(S^1, S^n1, 0) | submatrix(vars S, {0..n-1}));
    mapback g
    )
*-

-- Note: for now, the strategies must return a RawMatrix
algorithms#(pushForward, RingMap, Module) = new MutableHashTable from {
    Default => (opts, f, M) -> (
	if not isHomogeneous f
	or not isHomogeneous M
	then return null;
	M = cokernel presentation M;
	M1 := M / ideal f.matrix;
	M2 := subquotient(matrix basis M1, relations M);
	cokernel pushNonLinear(opts, f, M2)),

    -- By Justin Chen, see https://github.com/Macaulay2/M2/issues/1522
    Quotient => (opts, f, M) -> (
	(R, S) := (f.target, f.source);
	-- R & S should be over the same ambient polynomial ring
	if ring presentation R =!= ring presentation S
	or not member(S, R.baseRings) -- TODO: remove when https://github.com/Macaulay2/M2/issues/2103 if fixed
	then return null;
	ringRel := presentation R ** S;
	liftRingRel := id_(lift(ambient M, S)) ** ringRel;
	-- TODO: make trimming optional?
	trim subquotient(
	    lift(generators M, S),
	    lift(relations M, S) | liftRingRel)),
    }

-- Installing hooks for pushForward
scan({Default, Quotient}, strategy ->
    addHook(key := (pushForward, RingMap, Module), algorithms#key#strategy, Strategy => strategy))

-----------------------------------------------------------------------------
-- kernel
-----------------------------------------------------------------------------

kernel Matrix := Module => opts -> (cacheValue symbol kernel) ((m) -> (
	  N := source m;
	  if m == 0 then return N;
	  P := target m;
	  if m.?RingMap then (
	       f := m.RingMap;
	       n := map(target m,f source m,raw m);
	       p := pushNonLinear(options pushForward, f, coimage n);
	       image p)
	  else (
	       m = matrix m;
	       if P.?generators then m = P.generators * m;
	       h := modulo(m, if P.?relations then P.relations);
	       if N.?generators then h = N.generators * h;
	       subquotient( h, if N.?relations then N.relations))))
kernel RingElement := Module => options -> (m) -> kernel (matrix {{m}},options)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
