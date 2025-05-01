-----------------------------------------------------------------------------
-* Pushforwards of coherent sheaves *-
-----------------------------------------------------------------------------

decomposePushforwardPresentation = method()
decomposePushforwardPresentation(List, Matrix) := (d, f) -> (
    if #d == 1 then decomposePushforwardPresentation(d#0, f)
    else error "not implemented for multigraded maps")
decomposePushforwardPresentation(ZZ, Matrix) := (d, f) -> (
    tardegrees := degrees target f;
    srcdegrees := degrees source f;
    cube := flatten \ entries \ latticePoints hypercube(degreeLength ring f, 0, d - 1);
    tarclasses := apply(cube, i -> positions(tardegrees, deg -> deg % d == i));
    srcclasses := apply(cube, i -> positions(srcdegrees, deg -> deg % d == i));
    -- sorts the degrees of source and column
    tarclasses = apply(tarclasses, ell -> ell_(last \ sort \\ reverse \ toList pairs tardegrees_ell));
    srcclasses = apply(srcclasses, ell -> ell_(last \ sort \\ reverse \ toList pairs srcdegrees_ell));
    apply(tarclasses, srcclasses, (tarclass, srcclass) -> submatrix(f, tarclass, srcclass)))

pushForward' = (f, M, opts) -> (
    if not isHomogeneous f
    or not isHomogeneous M
    then return null;
    M = cokernel presentation M;
    M1 := M / ideal f.matrix;
    M2 := subquotient(matrix basis M1, relations M);
    cokernel pushNonLinear(opts, f, M2))

protect Pushforwards
pushForward(RingMap, SheafOfRings)  := opts -> (f, O) -> (
    X := variety O;
    X.cache.Pushforwards   ??= new MutableHashTable;
    X.cache.Pushforwards#f ??= pushForward(f, O^1, opts))
pushForward(RingMap, CoherentSheaf) := opts -> (f, G) -> (
    G.cache.Pushforwards   ??= new MutableHashTable )#f ??= (
    d := degree f(first gens source f); -- FIXME: this should be the degree of the map
    if #d == 1 then d = d#0 else error "not implemented for multigraded maps";
    S := source f;
    m0 := presentation pushForward'(f, module G, opts);
    m1 := first decomposePushforwardPresentation(d, m0);
    m2 := sub(m1, S); -- TODO: do better than sub?
    (tardegs, srcdegs) := toSequence(-degrees m2 // d);
    -- TODO: how long does this take? is it worth caching?
    sheaf prune coker map(S^tardegs, S^srcdegs, m2))

-----------------------------------------------------------------------------
-- TODO: fix in Core
-----------------------------------------------------------------------------

ordertab := new HashTable from {
    Eliminate    => (nR, nS) -> Eliminate nR,
    ProductOrder => (nR, nS) -> ProductOrder{nR, nS},
    Lex          => (nR, nS) -> Lex,
    }

pushNonLinear = (opts, f, M) -> (
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
    S' := newRing(S, Degrees => take(degrees G, n1 - numgens G));
    mapback := map(S', G, map(S'^1, S'^n1, 0) | vars S', DegreeMap => mapbackdeg );

    -- let's at least check it splits f's degree map:
    for i from 0 to numgens S - 1 do (
	e := degree S'_i;
	if mapbackdeg f.cache.DegreeMap e =!= e
	then error "not implemented yet: unexpected degree map of ring map");

    g := gb(m1,
	StopBeforeComputation => opts.StopBeforeComputation,
	DegreeLimit           => opts.DegreeLimit,
	PairLimit             => opts.PairLimit);
    -- MES: check if the monomial order restricts to S.  If so, then do `` forceGB result ''
    mapback selectInSubring(if numgens target f > 0 then 1 else 0, generators g))

-- addHook((pushForward, RingMap, Module), Strategy => "DefaultFixed",
--     (opts, f, M) -> (
-- 	if not isHomogeneous f
-- 	or not isHomogeneous M
-- 	then return null;
-- 	M = cokernel presentation M;
-- 	M1 := M / ideal f.matrix;
-- 	M2 := subquotient(matrix basis M1, relations M);
-- 	cokernel pushNonLinear(opts, f, M2)))
