-- Copyright 1995-2002 by Daniel R. Grayson and Michael Stillman
-* TODO:
 0. hookify, cache
 1. what are (basis, ZZ, List, *) methods for? why only Ring and Ideal?
 2. why isn't (basis, Matrix) implemented?
*-

needs "gb.m2"
needs "max.m2" -- for InfiniteNumber
needs "modules2.m2"
needs "ringmap.m2"

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- helpers for basis
-----------------------------------------------------------------------------

findHeftandVars := (R, varlist, ndegs) -> (
    -- returns (posvars, heftvec)
    -- such that posvars is a subset of varlist
    -- consisting of those vars whose degree is not 0 on the first ndegs slots
    -- and heftvec is an integer vector of length ndegs s.t. heftvec.deg(x) > 0 for each variable x in posvars
    if #varlist == 0 then return (varlist, {});
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

-----------------------------------------------------------------------------
-- basis
-----------------------------------------------------------------------------

basis = method(TypicalValue => Matrix,
    Options => {
	SourceRing => null, -- defaults to ring of the module, but accepts the coefficient ring
	Variables  => null,
	Degree     => null,
	Limit      => -1,
	Truncate   => false
	}
    )

-----------------------------------------------------------------------------

basis Module := opts -> M -> basis(-infinity, infinity, M, opts)
basis Ideal  := opts -> I -> basis(module I, opts)
basis Ring   := opts -> R -> basis(module R, opts)

-----------------------------------------------------------------------------

basis(List,                           Module) := opts -> (deg,    M) -> basis( deg,   deg,  M, opts)
basis(ZZ,                             Module) := opts -> (deg,    M) -> basis({deg}, {deg}, M, opts)
basis(InfiniteNumber, ZZ,             Module) := opts -> (lo, hi, M) -> basis( lo,   {hi},  M, opts)
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
    BM := last coefficients(matrix (M * BG), Monomials => BF);
    map(image BF, image BG, BM))

-----------------------------------------------------------------------------

basis(InfiniteNumber,InfiniteNumber,Module) :=
basis(List,InfiniteNumber,Module) :=
basis(InfiniteNumber,List,Module) :=
basis(List,List,Module) := opts -> (lo,hi,M) -> (
    R := ring M;
    if lo === infinity then error "incongruous lower degree bound: infinity";
    if hi === neginfinity then error "incongruous upper degree bound: -infinity";
    if lo === neginfinity then lo = {};
    if hi === infinity then hi = {};
    if #lo != 0 and #lo > degreeLength R or #hi != 0 and #hi > degreeLength R then error "expected length of degree bound not to exceed that of ring";
    if lo =!= hi and #lo > 1 then error "degree rank > 1 and degree bounds differ";
    if not all(lo, i -> instance(i,ZZ)) then error ("expected a list of integers: ", toString lo);
    if not all(hi, i -> instance(i,ZZ)) then error ("expected a list of integers: ", toString hi);

    A := ultimate(ambient,R);
    if not (
	isAffineRing A
	or
	isPolynomialRing A and isAffineRing coefficientRing A and A.?SkewCommutative
	or
	isPolynomialRing A and ZZ === coefficientRing A
	or
	ZZ === A
	) then error "'basis' can't handle this type of ring";
    var := opts.Variables;
    if var === null then var = toList(0 .. numgens R - 1)
    else if class var === List then (
	var = apply(var, v -> if instance(v,R) then index v
                              else if instance(v,ZZ) then v
                              else error "expected list of ring variables or integers"))
    else error "expected list of ring variables or integers";
    (varlist, heftvec) := if #lo == 0 and #hi == 0
                          then (var, () )
                          else findHeftandVars(R, var, max(#hi,#lo));

    pres := generators gb presentation M;

    M.cache#"rawBasis log" = log := FunctionApplication { rawBasis, (
	    raw pres,
	    lo, hi,
	    heftvec,
	    varlist,
	    opts.Truncate,
	    opts.Limit
	    )};
    f := value log;
    S := opts.SourceRing;
    off := splice opts.Degree;
    d := degreeLength R;
    if off === null then off = toList( d:0 );
    if S === null or S === R then map(M,,f,Degree => off)
    else (
	p := map(R,S);
	N := S ^ (
	    if d === 0 then rank source f
	    else (
		lifter := p.cache.DegreeLift;
		if not instance(lifter, Function)
		then rank source f
		else apply(pack(d,degrees source f), (
			zeroDegree := toList( degreeLength S : 0 );
			deg -> try - lifter (deg-off) else zeroDegree))));
	map(M,N,p,f,Degree => off)))
