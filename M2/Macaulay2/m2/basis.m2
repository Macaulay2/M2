-- Copyright 1995-2002 by Daniel R. Grayson and Michael Stillman
-- Rewritten 2021 by Mahrud Sayrafi
-* TODO:
 1. why does methods(basis, ZZ, List) only give methods for Ring and Ideal?
 2. why isn't (basis, Matrix) implemented?
 3. update rawSubmatrixByDegree to accept partial degrees,
    then simplify adjustComputation
 4. can liftBasis be done with a tensor or pushforward computation?
    see https://github.com/Macaulay2/M2/issues/1522
 5. see tests/normal/basis3.m2:
      assert try (basis(0, ZZ/101[a, Degrees => {0}]); false) else true
    This probably needs to be fixed in e/matrix-kbasis.cpp
 6. is there any way to better take advantage of the cache when Truncate => true?
 7. there is almost no caching when lower and upper limit have rank > 1
*-

needs "max.m2" -- for InfiniteNumber
needs "modules2.m2"
needs "computations.m2"

-----------------------------------------------------------------------------
-- Local variables
-----------------------------------------------------------------------------

algorithms := new MutableHashTable from {}

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

inf := t -> if t === infinity then -1 else t

-----------------------------------------------------------------------------
-- helpers for basis
-----------------------------------------------------------------------------

-- the output of this is used in BasisContext
getVarlist = (R, varlist) -> (
    if varlist === null then toList(0 .. numgens R - 1)
    else if instance(varlist, List) then apply(varlist, v ->
	-- TODO: what if R = ZZ?
	if instance(v, R)  then index v else
	if instance(v, ZZ) then v
	else error "expected list of ring variables or variable indices")
    else error "expected list of ring variables or variable indices")

findHeftandVars = (R, varlist, ndegs) -> (
    -- returns (varlist, heftval)
    -- such that varlist is a subset of varlist
    -- consisting of those vars whose degree is not 0 on the first ndegs slots
    -- and heft is an integer vector of length ndegs s.t. heft.deg(x) > 0 for each variable x in varlist
    varlist = getVarlist(R, varlist);
    if ndegs == 0 or #varlist == 0 then return (varlist, {});
    if degreeLength R == ndegs and #varlist == numgens R then (
	heftvec := heft R;
	if heftvec =!= null then return (varlist, heftvec));
    --
    zerodeg := toList(ndegs:0);
    posvars := select(varlist, x -> R_x != 0 and take(degree R_x, ndegs) != zerodeg);
    degs := apply(posvars, x -> take(degree R_x, ndegs));
    heftvec = findHeft(degs, DegreeRank => ndegs);
    if heftvec =!= null then (posvars, heftvec)
    else error("heft vector required that is positive on the degrees of the variables " | toString posvars))

-- TODO: can this be done via a tensor or push forward?
-- c.f. https://github.com/Macaulay2/M2/issues/1522
liftBasis = (M, phi, B, offset) -> (
    -- lifts a basis B of M via a ring map phi
    (R, S) := (phi.target, phi.source);
    (n, m) := degreeLength \ (R, S);
    offset  = if offset =!= null then splice offset else toList( n:0 );
    if R === S then return map(M, , B, Degree => offset);
    r := if n === 0 then rank source B else (
	lifter := phi.cache.DegreeLift;
	if not instance(lifter, Function)
	then rank source B else (
	    zeroDegree := toList(m:0);
	    apply(pack(n, degrees source B),
		deg -> try - lifter(deg - offset) else zeroDegree)));
    map(M, S ^ r, phi, B, Degree => offset))

-----------------------------------------------------------------------------
-- basis
-----------------------------------------------------------------------------

basis = method(TypicalValue => Matrix,
    Options => new OptionTable from {
	Strategy   => null,
	SourceRing => null,     -- defaults to ring of the module, but accepts the coefficient ring
	Variables  => null,     -- defaults to the generators of the ring
	Degree     => null,     -- offset the degree of the resulting matrix
	Limit      => infinity, -- upper bound on the number of basis elements to collect
	Truncate   => false     -- if true, then generators of higher degree are kept
	}
    )

-- keys: Variables
-- TODO: perhaps we can use selectInSubring on
-- a cached basis with a larger variable set
BasisContext = new SelfInitializingType of Context
BasisContext.synonym = "basis context"

new BasisContext from Sequence := (C, S) -> BasisContext{getVarlist S}

BasisComputation = new Type of Computation
BasisComputation.synonym = "basis computation"

protect LowerLimit
protect UpperLimit
ExtraOpts := { LowerLimit => null, UpperLimit => null }

new BasisComputation from Sequence := (C, S) -> new BasisComputation from {
    LowerLimit => S#0,
    UpperLimit => S#1,
    DegreeRank => degreeLength S#2,
    Limit      => infinity,
    Truncate   => false,
    Variables  => null,
    Result     => null}

isComputationDone BasisComputation := Boolean => options basis ++ ExtraOpts >> opts -> container -> (
    -- this function determines whether we can use the cached result, or further computation is necessary
    instance(container.Result, RawMatrix)
    and(opts.Truncate   == container.Truncate or container.Truncate)
    and opts.LowerLimit >= container.LowerLimit
    and opts.UpperLimit <= container.UpperLimit
    and opts.Limit      <= container.Limit)

updateComputation(BasisComputation, RawMatrix) := RawMatrix => options basis ++ ExtraOpts >> opts -> (container, result) -> (
    container.Truncate   = opts.Truncate;
    container.LowerLimit = opts.LowerLimit;
    container.UpperLimit = opts.UpperLimit;
    container.Limit      = opts.Limit;
    container.Result     = result)

adjustComputation BasisComputation := RawMatrix => options basis ++ ExtraOpts >> opts -> container -> (
    -- TODO: make sure this is correct
    if container.DegreeRank == 0 then return container.Result;
    -- TODO: make sure this works with either a Matrix or a RawMatrix
    (lo, hi) := (opts.LowerLimit, opts.UpperLimit);
    degs := pack(container.DegreeRank, degrees source container.Result);
    -- TODO: would be better to do this in engine, but rawSubmatrixByDegree doesn't work with partial degrees
    cols := positions(degs, deg ->
	lo <= take(deg, #lo) and ( opts.Truncate or take(deg, #hi) <= hi ));
    if opts.Limit < #cols then cols = take(cols, opts.Limit);
    rawSubmatrix(container.Result, cols))

-----------------------------------------------------------------------------

basis Module := opts -> M -> basis(-infinity, infinity, M, opts)
basis Ideal  := opts -> I -> basis(module I, opts)
basis Ring   := opts -> R -> basis(module R, opts)
-- TODO: add? basis Matrix := opts -> m -> basis(-infinity, infinity, m, opts)

-----------------------------------------------------------------------------

basis(List,                           Module) := opts -> (deg,    M) -> basis( deg,   deg,  M, opts)
basis(ZZ,                             Module) := opts -> (deg,    M) -> basis({deg}, {deg}, M, opts)
basis(InfiniteNumber, InfiniteNumber, Module) :=
basis(InfiniteNumber, List,           Module) := opts -> (lo, hi, M) -> basisHelper(opts, lo, hi, M)
basis(InfiniteNumber, ZZ,             Module) := opts -> (lo, hi, M) -> basis( lo,   {hi},  M, opts)
basis(List,           InfiniteNumber, Module) :=
basis(List,           List,           Module) := opts -> (lo, hi, M) -> basisHelper(opts, lo, hi, M)
basis(ZZ,             InfiniteNumber, Module) := opts -> (lo, hi, M) -> basis({lo},   hi,   M, opts)
basis(ZZ,             ZZ,             Module) := opts -> (lo, hi, M) -> basis({lo},  {hi},  M, opts)

-----------------------------------------------------------------------------

basis(List,                           Ideal) :=
basis(ZZ,                             Ideal) := opts -> (deg,    I) -> basis(deg, deg, module I, opts)
basis(InfiniteNumber, InfiniteNumber, Ideal) :=
basis(InfiniteNumber, List,           Ideal) :=
basis(InfiniteNumber, ZZ,             Ideal) :=
basis(List,           InfiniteNumber, Ideal) :=
basis(List,           List,           Ideal) :=
basis(List,           ZZ,             Ideal) :=
basis(ZZ,             InfiniteNumber, Ideal) :=
basis(ZZ,             List,           Ideal) :=
basis(ZZ,             ZZ,             Ideal) := opts -> (lo, hi, I) -> basis(lo,  hi,  module I, opts)

-----------------------------------------------------------------------------

basis(List,                           Ring) :=
basis(ZZ,                             Ring) := opts -> (deg,    R) -> basis(deg, deg, module R, opts)
basis(InfiniteNumber, InfiniteNumber, Ring) :=
basis(InfiniteNumber, List,           Ring) :=
basis(InfiniteNumber, ZZ,             Ring) :=
basis(List,           InfiniteNumber, Ring) :=
basis(List,           List,           Ring) :=
basis(List,           ZZ,             Ring) :=
basis(ZZ,             InfiniteNumber, Ring) :=
basis(ZZ,             List,           Ring) :=
basis(ZZ,             ZZ,             Ring) := opts -> (lo, hi, R) -> basis(lo,  hi,  module R, opts)

-----------------------------------------------------------------------------

basis(List,                           Matrix) :=
basis(ZZ,                             Matrix) := opts -> (deg, M) -> basis(deg, deg, M, opts)
basis(InfiniteNumber, InfiniteNumber, Matrix) :=
basis(InfiniteNumber, List,           Matrix) :=
basis(InfiniteNumber, ZZ,             Matrix) :=
basis(List,           InfiniteNumber, Matrix) :=
basis(List,           List,           Matrix) :=
basis(ZZ,             InfiniteNumber, Matrix) :=
basis(ZZ,             ZZ,             Matrix) := opts -> (lo, hi, M) -> (
    BF := basis(lo, hi, target M, opts);
    BG := basis(lo, hi, source M, opts);
    -- TODO: is this general enough?
    BM := last coefficients(matrix (M * BG), Monomials => BF);
    map(image BF, image BG, BM))

-----------------------------------------------------------------------------

basisHelper = (opts, lo, hi, M) -> (
    R := ring M;
    n := degreeLength R;
    strategy := opts.Strategy;

    -- TODO: check that S is compatible; i.e. there is a map R <- S
    -- perhaps the map should be given as the option instead?
    S := if opts.SourceRing =!= null then opts.SourceRing else R;
    phi := map(R, S);

    if lo === -infinity then lo = {} else
    if lo ===  infinity then error "incongruous lower degree bound: infinity";
    if hi ===  infinity then hi = {} else
    if hi === -infinity then error "incongruous upper degree bound: -infinity";

    -- implement generic degree check
    if #lo != 0  and #lo > n
    or #hi != 0  and #hi > n then error "expected length of degree bound not to exceed that of ring";
    if lo =!= hi and #lo > 1 then error "degree rank > 1 and degree bounds differ";
    if not all(lo, i -> instance(i, ZZ)) then error("expected a list of integers: ", toString lo);
    if not all(hi, i -> instance(i, ZZ)) then error("expected a list of integers: ", toString hi);

    -- e.g., basis(4, 2, QQ[x])
    if #hi == 1 and #lo == 1 and hi - lo < {0}
    then return if S === R then map(M, S^0, {}) else map(M, S^0, phi, {});

    opts = opts ++ {
	LowerLimit => lo,
	UpperLimit => hi,
	Limit      => if opts.Limit == -1 then infinity else opts.Limit
	};

    -- this logic runs the strategies in order, or the specified strategy
    computation := (opts, container) -> (
	runHooks((basis, List, List, Module), (opts, lo, hi, M), Strategy => strategy));

    -- this is the logic for caching partial basis computations. M.cache contains an option:
    --   BasisContext{} => BasisComputation{ Result, ... }
    container := fetchComputation(BasisComputation, M, (lo, hi, M), new BasisContext from (R, opts.Variables));

    -- the actual computation of the basis occurs here
    B := (cacheComputation(opts, container)) computation;

    if B =!= null then liftBasis(M, phi, B, opts.Degree) else if strategy === null
    then error("no applicable strategy for computing bases over ", toString R)
    -- used to be: error "'basis' can't handle this type of ring";
    else error("assumptions for basis strategy ", toString strategy, " are not met"))

-----------------------------------------------------------------------------
-- strategies for basis
-----------------------------------------------------------------------------

basisDefaultStrategy = (opts, lo, hi, M) -> (
    R := ring M;
    A := ultimate(ambient, R); -- is ambient better or coefficientRing?

    -- the assumptions for the default strategy:
    if not (ZZ === A
	or isAffineRing A
	or isPolynomialRing A and isAffineRing coefficientRing A and A.?SkewCommutative
	or isPolynomialRing A and ZZ === coefficientRing A )
    then return null;

    (varlist, heftvec) := findHeftandVars(R, opts.Variables, max(#hi, #lo));

    m := generators gb presentation M;
    log := FunctionApplication { rawBasis, (
	    raw m,
	    lo, hi,
	    heftvec,
	    varlist,
	    opts.Truncate,
	    inf opts.Limit
	    )};
    M.cache#"rawBasis log" = Bag {log};
    B := value log;
    B)

-- Note: for now, the strategies must return a RawMatrix
algorithms#(basis, List, List, Module) = new MutableHashTable from {
    Default => basisDefaultStrategy,
    -- TODO: add separate strategies for skew commutative rings, vector spaces, and ZZ-modules
    -- TODO: add strategy to use already cached results
    }

-- Installing hooks for resolution
scan({Default}, strategy ->
    addHook(key := (basis, List, List, Module), algorithms#key#strategy, Strategy => strategy))
