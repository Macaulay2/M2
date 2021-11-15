--		Copyright 1995 by Daniel R. Grayson and Michael Stillman
-- TODO:
-- 1. set DegreeLimit to something other than null on initialization
-- 2. replace Resolution with ResolutionComputation
-- 3. integrate with the Complexes package

needs "chaincomplexes.m2"
needs "computations.m2"
needs "max.m2"

-----------------------------------------------------------------------------
-- Local variables
-----------------------------------------------------------------------------

algorithms := new MutableHashTable from {}

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

resLog := x -> if gbTrace > 0 then printerr x

inf := t -> if t === infinity then -1 else t

spots := C -> select(keys C, i -> instance(i, ZZ))

baseRing' := R -> ultimate(coefficientRing, R) -- Note: different from first R.baseRings

resolutionLengthLimit = (R, lengthLimit) -> (
    if lengthLimit == infinity then (
	A := baseRing' R;
	nvars := # generators(R, CoefficientRing => A);
	nvars + 1 + if A === ZZ then 1 else 0)
    else lengthLimit )

resolutionDegreeLimit = (R, degreeLimit) -> (
    degreeLimit = if degreeLimit =!= null      then  degreeLimit  else {};
    degreeLimit = if instance(degreeLimit, ZZ) then {degreeLimit} else degreeLimit;
    if #degreeLimit == degreeLength R and all(degreeLimit, d -> instance(d, ZZ))
    or #degreeLimit == 0 then degreeLimit
    else error "resolution: expected DegreeLimit and HardDegreeLimit to be a valid degree, multidegree, or null")

-----------------------------------------------------------------------------
-- helpers for resolution
-----------------------------------------------------------------------------

-- TODO: remove from Elimination and Saturation
-- given a ring R, determines if R is a poly ring over ZZ or a field
isFlatPolynomialRing = R -> isPolynomialRing R and (isField(kk := coefficientRing R) or kk === ZZ)

-- TODO: support QQ and large finite fields
fastnonminimalFieldCheck := (R, strategy) -> strategy < 4 or ( 0 < char R and char R < (1 << 15) ) or not instance(strategy, ZZ)

engineReady := M -> (
    R := ring M;
    -- FreeAlgebraQuotients have specialized engine routines
    if instance(R, FreeAlgebraQuotient) then return true;
    -- Needed to compute resolutions, (algorithms 0,1,2,3):
    --    Ring is (tower of) poly ring(s) over a field (or skew commutative, or quotient ring of such, or both)
    --    Ring is graded
    --    Ring is homogeneous in this grading
    --    Matrix is homogeneous in this grading
    -- Additional requirements for resolution algorithm 3 (which uses hilbert function):
    --    Ring is singly graded
    R.?Engine
    and heft R =!= null
    and isHomogeneous M
    and(isCommutative R or isSkewCommutative R)
    and(A := baseRing' R) =!= R and isField A)

-----------------------------------------------------------------------------
-- strategies for minimal resolutions
-----------------------------------------------------------------------------

resolutionByHomogenization := (opts, M) -> (
    R := ring M;
    if isHomogeneous M
    or not isCommutative R
    -- TODO: generalize to multigraded setting
    or not degreeLength R === 1
    then return null;
    resLog("using resolution by homogenization");
     f    := presentation M;
     p    := presentation R;
     A    := ring p;
     k    := coefficientRing A;
     n    := numgens A;
     X    := local X;
     N    := monoid [X_0 .. X_n, MonomialOrder => GRevLex, Join => false];
     A'   := k N;
     toA' := map(A',A,(vars A')_{0 .. n-1});
     p'   := toA' p;
     R'   := A'/(ideal p');
     toR' := map(R',R,(vars R')_{0 .. n-1});
     f'   := toR' f;
     pH   := homogenize(generators gb p', A'_n);     	  forceGB pH;
     RH   := A' / ideal pH;
     toRH := map(RH, R', vars RH);
     fH   := homogenize(toRH generators gb f',RH_n); 	  forceGB fH;
     MH   := cokernel fH;
     assert isHomogeneous MH;
     C    := resolution(MH, opts, LengthLimit => opts.LengthLimit);
     toR  := map(R, RH, vars R | 1);
     toR C)

-- TODO: confirm assumptions
resolutionBySchreyerFrame := (opts, M) -> (
    R := ring M;
    -- the main requirement is rawSchreyerSource
    -- FIXME: tower of polynomial rings is okay,
    -- but exclude polynomial rings over a quotient ring
    if not isPolynomialRing R
    or not isCommutative R
    then return null;
    resLog("using resolution by Schreyer orders");
    -- FIXME: this strategy can't continue from another strategy
    C := if M.cache.?resolution then M.cache.resolution else (
	m := schreyerOrder generators gb presentation M;
	M.cache.resolution = chainComplex m);
    i := length C;
    m = C.dd_i;
    while i < opts.LengthLimit and m != 0 do (
	m = map(R, rawKernelOfGB raw m);
	shield ( i = C.length = i + 1; C#i = source m; C.dd#i = m ));
    C)

resolutionBySyzygies := (opts, M) -> (
    resLog("using resolution by syzygyies");
    C := if M.cache.?resolution then M.cache.resolution
    else M.cache.resolution = chainComplex presentation M;
    i := length C;
    m := C.dd_i;
    while i < opts.LengthLimit and m != 0 do (
	m = syz m; -- TODO: pass options like DegreeLimit
	shield ( i = C.length = i + 1; C#i = source m; C.dd#i = m ));
    C)

resolutionInEngine := (opts, M) -> (
     R := ring M;
     if not engineReady M then return null;
     -- TODO: label strategy 1 and 2
     strategy := if instance(opts.Strategy, Number) then opts.Strategy else
     if opts.FastNonminimal then 4 else
     if isQuotientRing R or isSkewCommutative R then 2 else 1;
     if not fastnonminimalFieldCheck(R, strategy) then return null;
     local C;
     degreelimit := opts.DegreeLimit;
     lengthlimit := opts.LengthLimit;
     if not M.cache.?resolution 
     or M.cache.resolution.Resolution.length < lengthlimit
     or (M.cache.resolution.Resolution.Strategy === 4 and strategy =!= 4)
     or (M.cache.resolution.Resolution.Strategy =!= 4 and strategy === 4)
     then M.cache.resolution = (
          if flagInhomogeneity and not isHomogeneous M
	  then error "internal error: res: inhomogeneous matrix flagged";
	  g := presentation M;
	  if strategy === 0 or strategy === 4 or strategy === 4.1 then
	      g = generators gb g;  -- this is needed since the (current)
			      -- default algorithm, 0, needs a GB 
			      -- to be previously computed.
                              -- The non-minimal resolution algorithm 4 also needs this.
	      harddegreelimit := resolutionDegreeLimit(R, opts.HardDegreeLimit);
	  W := new Resolution;
	  W.ring = R;
	  W.length = lengthlimit;
	  W.DegreeLimit = degreelimit;
          W.Strategy = strategy;
	  log := FunctionApplication { rawResolution, (
		    raw g,			-- the matrix
		    true,			-- whether to resolve the cokernel of the matrix
		    lengthlimit,		-- how long a resolution to make, (hard : cannot be increased by stop conditions below)
		    false,			-- useMaxSlantedDegree
		    0,				-- maxSlantedDegree (is this the same as harddegreelimit?)
		    floor strategy,		-- algorithm (floor converts the experimental value 4.1 to 4, avoiding error message above)
		    opts.SortStrategy		-- strategy (is this the same as opts.SortStrategy?)
		    )};
	  W#"RawComputation log" = Bag {log};
     	  W.RawComputation = value log;
	  W.returnCode = rawStatus1 W.RawComputation;
	  new ChainComplex from W);
     C = M.cache.resolution;
     if C.?Resolution then (
	  W = C.Resolution;
	  if not W.?returnCode 
	  or not isComputationDone W.returnCode
	  or W.length < lengthlimit
	  or W.DegreeLimit < degreelimit
	  then (
	       -- clear info in C because W may change as we continue the computation:
	       scan(keys C,i -> if class i === ZZ then remove(C,i));
	       scan(keys C.dd,i -> if class i === ZZ then remove(C.dd,i));
	       remove(C,symbol complete);
	       if not opts.StopBeforeComputation then (
		    log = FunctionApplication { rawGBSetStop,
			 (
			      W.RawComputation,
			      -- fill these in eventually:
			      opts.StopBeforeComputation,	    -- always_stop
			      degreeToHeft(R,degreelimit),          -- degree_limit
			      0,				    -- basis_element_limit (not relevant for resolutions)
			      inf opts.SyzygyLimit,		    -- syzygy_limit
			      inf opts.PairLimit,		    -- pair_limit
			      0,				    -- codim_limit (not relevant for resolutions)
			      0,				    -- subring_limit (not relevant for resolutions)
			      false,				    -- just_min_gens
			      -- {lengthlimit}			    -- length_limit -- error if present is: "cannot change length of resolution using this algorithm"
			      {} 				    -- length_limit
			      )};
		    W#"rawGBSetStop log" = Bag {log};
		    value log;
		    rawStartComputation W.RawComputation;
		    W.returnCode = rawStatus1 W.RawComputation;
		    W.length = lengthlimit;
		    W.DegreeLimit = degreelimit;
		    )));
     C)

-----------------------------------------------------------------------------
-- resolution
-----------------------------------------------------------------------------

resolution = method(
    Options => {
	StopBeforeComputation	=> false,
	LengthLimit		=> infinity,	-- (infinity means numgens R)
	DegreeLimit		=> null,	-- slant degree limit
	SyzygyLimit		=> infinity,	-- number of min syzs found
	PairLimit		=> infinity,	-- number of pairs computed
	HardDegreeLimit		=> {},		-- throw out information in degrees above this one
	-- HardLengthLimit	=> infinity,	-- throw out information in lengths above this one
	SortStrategy		=> 0,		-- strategy choice for sorting S-pairs
	Strategy		=> null,	-- algorithm to use, usually 1, but sometimes 2
	FastNonminimal		=> false
	}
    )

-- keys: none so far
-- TODO: perhaps keys for different types of resolutions? e.g. virtual resolution?
ResolutionContext = new SelfInitializingType of Context
ResolutionContext.synonym = "resolution context"

new ResolutionContext from Module := (C, M) -> new C from {}

-- keys: LengthLimit
-- TODO: what else?
-- SyzygyLimit, HardDegreeLimit, StopBeforeComputation,
-- DegreeLimit, FastNonminimal, SortStrategy, PairLimit
ResolutionComputation = new Type of Computation
ResolutionComputation.synonym = "resolution computation"

new ResolutionComputation from HashTable := (C, H) -> merge(H, new HashTable from { Result => null }, last)

isComputationDone Resolution := Boolean => options resolution >> opts -> W -> (
    -- can returnCode ever be not defined?
    isComputationDone W.returnCode
    and opts.LengthLimit <= W.length
    and opts.DegreeLimit <= W.DegreeLimit)

isComputationDone ResolutionComputation := Boolean => options resolution >> opts -> container -> (
    -- this function determines whether we can use the cached result, or further computation is necessary
    C := if instance(container.Result, ChainComplex) then container.Result else return false;
    -- if FastNonminimal sets returnCode correctly, the "not container.FastNonminimal" condition can be removed
    if C.?Resolution and not container.FastNonminimal then isComputationDone(C.Resolution, opts) else
    ( opts.FastNonminimal == container.FastNonminimal or not container.FastNonminimal )
    and opts.DegreeLimit  <= container.DegreeLimit
    and opts.LengthLimit  <= container.LengthLimit)

updateComputation(ResolutionComputation, ChainComplex) := ChainComplex => options resolution >> opts -> (container, result) -> (
    container.FastNonminimal = opts.FastNonminimal;
    container.DegreeLimit    = opts.DegreeLimit;
    container.LengthLimit    = opts.LengthLimit; -- length result;
    container.Result         = result)

-- TODO: document this
-- TODO: how to combine this with caching?
protect ManualResolution -- not to be exported
storefuns#resolution = (M,C) -> M.cache.ManualResolution = C

resolution Module := ChainComplex => opts -> M -> (
    R := ring M;
    strategy := opts.Strategy;

    if M.cache.?ManualResolution then return (
	resLog "returning manually provided resolution";
	M.cache.ManualResolution);

    opts = opts ++ {
	DegreeLimit => resolutionDegreeLimit(R, opts.DegreeLimit),
	LengthLimit => resolutionLengthLimit(R, opts.LengthLimit),
	};

    -- this logic runs the strategies in order, or the specified strategy
    computation := (opts, container) -> (
	if isField R then return map(minimalPresentation M, R^0, 0);
	runHooks((resolution, Module), (opts, M), Strategy => strategy));

    -- this is the logic for caching partial resolution computations. M.cache contains an option:
    --   ResolutionContext{} => ResolutionComputation{ Result, LengthLimit, ... }
    container := fetchComputation(ResolutionComputation, M, new HashTable from opts, new ResolutionContext from M);

    -- the actual computation of the resolution occurs here
    C := (cacheComputation(opts, container)) computation;

    if C =!= null then C else if strategy === null
    then error("no applicable strategy for resolving over ", toString R)
    else error("assumptions for resolution strategy ", toString strategy, " are not met"))

resolution Ideal := ChainComplex => opts -> I -> resolution(
    if I.cache.?quotient then I.cache.quotient
    else I.cache.quotient = cokernel generators I, opts)

resolution Matrix := ChainComplexMap => opts -> f -> extend(
    resolution(target f, opts), resolution(source f, opts), matrix f)

-----------------------------------------------------------------------------

algorithms#(resolution, Module) = new MutableHashTable from {
    Number => (opts, M) -> (
	-- TODO: clarify the requirements for engine routines and top-level routines, then
	-- simplify the strategies, then add Engine and FastNonminimal as normal strategies
	R := ring M;
	if not char R < (1 << 15) then return null;
	if (C := resolutionNonminimal(opts, M)) =!= null then C
	else if (C = resolutionInEngine(opts, M)) =!= null then C
	else null),

    Engine => resolutionInEngine,

    "ZZ-module" => (opts, M) -> (
	if ZZ =!= baseRing' ring M then return null;
	resolutionBySyzygies(opts, M)),

    "Homogenization" => resolutionByHomogenization,

    "Schreyer" => resolutionBySchreyerFrame,

    "Syzygies" => resolutionBySyzygies,
    }

-- TODO: implement reverse lookup for hooks
algorithms#(resolution, Module)#ZZ =
algorithms#(resolution, Module)#RR = algorithms#(resolution, Module)#Number

-- Installing hooks for resolution
scan({"Schreyer", "Syzygies", "Homogenization", "ZZ-module", Engine, ZZ, RR}, strategy ->
    addHook(key := (resolution, Module), algorithms#key#strategy, Strategy => strategy))

-----------------------------------------------------------------------------
-- FastNonminimal strategy
-----------------------------------------------------------------------------

resolutionNonminimal = (opts, M) -> (
    R := ring M;
    -- options allowed:
    --    LengthLimit
    --    Strategy (values allowed: strats)
    strats := {4, 4.1, 5, 5.1};
    strategy := if member(opts.Strategy, strats) then opts.Strategy
    else if opts.FastNonminimal then (
	if opts.Strategy === null then first strats
	else error("resolution: expected FastNonminimal Strategy option to be one of: ", demark_", " \\ toString \ strats))
    else return null;
    -- requirements:
    --  1. M is a cokernel module over a polynomial ring R
    if not M.?relations
    --  2. R cannot be a quotient ring (currently), it must be a poly ring or skew poly ring.
    --    (no Weyl algebra here either.  Although maybe this could be relaxed).
    or not instance(R, PolynomialRing)
    or not(isCommutative R or isSkewCommutative R)
    --  3. if Strategy is 4 or 4.1, then a GB of the presentation matrix of M is computed.
    --     if Strategy is 5 or 5.1, it is assumed that the matrix: relations M, is already a Groebner basis
    --       If it is not a GB, then this function will give an answer (which one has to interpret carefully), but
    --       at least won't crash.
    --  4. currently, for Strategy == 4 or 5, the coefficient ring must be a prime field of char 2 <= p < 32767.
    or not fastnonminimalFieldCheck(R, strategy)
    --  5. M need not be homogeneous, or it may be multi-homogeneous.
    then return null;
    resLog("using resolution with FastNonminimal => true, Strategy => ", toString strategy);
    -- 
    -- the result computation is placed into M.cache.NonminimalResolutionComputation
    -- there are a number of functions that can be used to obtain information of the computation:
    --  . create the computation
    --  . restart after a stop
    --  . make a complex out of this
    --    i.e: get free modules, and maps
    --  . get a specific matrix
    --  . get a specific free module
    --  . minimal Betti numbers of M
    --  . what else?  constant strands? labels? parts of each matrix?
    -- TODO MES:  Quickly determine if this function is "active"
    --  (so we can call addHook).
    degreelimit := opts.DegreeLimit;
    lengthlimit := opts.LengthLimit;
    -- the resulting complex.
    C := if M.cache.?resolutionNonminimal
    and  M.cache.resolutionNonminimal.Resolution.length >= lengthlimit
    then M.cache.resolutionNonminimal
    else M.cache.resolutionNonminimal = (
        g := presentation M;
        if strategy < 5 then g = generators gb g;
        harddegreelimit := resolutionDegreeLimit(R, opts.HardDegreeLimit);
        W := new Resolution;
        W.ring = R;
        W.length = lengthlimit;
        W.DegreeLimit = degreelimit;
        W.Strategy = strategy;
        log := FunctionApplication { rawResolution, (
                raw g,					    -- the matrix
                true,					    -- whether to resolve the cokernel of the matrix
                lengthlimit,				    -- how long a resolution to make, (hard : cannot be increased by stop conditions below)
                false,					    -- useMaxSlantedDegree
                0,					    -- maxSlantedDegree (is this the same as harddegreelimit?)
                floor strategy,		    -- algorithm (floor converts the experimental value 4.1 to 4, avoiding error message above)
                opts.SortStrategy			    -- strategy (is this the same as opts.SortStrategy?)
                )};
        W#"RawComputation log" = Bag {log};
        W.RawComputation = value log;
        W.returnCode = rawStatus1 W.RawComputation;
	new ChainComplex from W);
    if C.?Resolution then (
        W = C.Resolution;
        if not W.?returnCode 
        or not isComputationDone W.returnCode
        or W.length < lengthlimit
        or W.DegreeLimit < degreelimit
        then (
            -- clear info in C because W may change as we continue the computation:
            scan(keys C,i -> if class i === ZZ then remove(C,i));
            scan(keys C.dd,i -> if class i === ZZ then remove(C.dd,i));
            remove(C,symbol complete);
            if not opts.StopBeforeComputation then (
                log = FunctionApplication { rawGBSetStop,
                    (
                        W.RawComputation,
                        -- fill these in eventually:
                        opts.StopBeforeComputation,	    -- always_stop
                        degreeToHeft(R,degreelimit),          -- degree_limit
                        0,				    -- basis_element_limit (not relevant for resolutions)
                        inf opts.SyzygyLimit,		    -- syzygy_limit
                        inf opts.PairLimit,		    -- pair_limit
                        0,				    -- codim_limit (not relevant for resolutions)
                        0,				    -- subring_limit (not relevant for resolutions)
                        false,				    -- just_min_gens
                        -- {lengthlimit}		    -- length_limit -- error if present is: "cannot change length of resolution using this algorithm"
                        {} 				    -- length_limit
                        )};
                W#"rawGBSetStop log" = Bag {log};
                value log;
                rawStartComputation W.RawComputation;
                W.returnCode = rawStatus1 W.RawComputation;
                W.length = lengthlimit;
                W.DegreeLimit = degreelimit;
                )));
    C)

addHook((resolution, Module), Strategy => FastNonminimal, resolutionNonminimal)

-----------------------------------------------------------------------------
-- status: status of a resolution computation
-----------------------------------------------------------------------------
-- TODO: extend to other computations

-- TODO: where and how should these be used?
getpairs  := g -> rawGBBetti(raw g, 1)
remaining := g -> rawGBBetti(raw g, 2)
nmonoms   := g -> rawGBBetti(raw g, 3)

-- Note: this needs betti.m2
status Resolution := opts -> r -> (
     r = raw r;
     b := new BettiTally;
     lab := ();
     f := (label,type) -> (
	  b = merge( applyValues(b, x->append(x,0)), applyValues(rawBetti(r,type), x->splice{#lab:0,x}), plus);
	  lab = append(lab,label);
	  );
     if opts#TotalPairs     === true then f("total pairs",1);
     if opts#PairsRemaining === true then f("pairs remaining",2);
     if opts#Monomials      === true then f("monomials",3);
     numops := # lab;
     if numops === 0 then error "expected at least one option to be true";
     b = applyKeys( b, (i,d,h) -> (h - i, i)); -- skew the degrees in the usual way; this way the Koszul complex occupies a horizontal line instead of a diagonal line
     k := keys b;
     fi := first \ k;
     la := last  \ k;
     mincol := min la;
     mincol = min(0,mincol);
     maxcol := max la;
     minrow := min fi;
     maxrow := max fi;
     zer := toList (numops : 0);
     b = table(toList (minrow .. maxrow), toList (mincol .. maxcol), (i,j) -> if b#?(i,j) then b#(i,j) else zer);
     leftside := apply( splice {"total:", apply(minrow .. maxrow, i -> toString i | ":")}, s -> (6-# s,s));
     totals := apply(transpose b, sum);
     b = transpose prepend(totals,b);
     b = applyTable(b, unsequence @@ toSequence);
     zer = unsequence toSequence zer;
     b = applyTable(b, bt -> if bt === zer then "." else toString bt);
     b = apply(b, col -> ( 
	       wid := 1 + max apply(col, i -> #i); 
	       apply(col, 
		    if numops == 1
		    then s -> (wid-#s, s)		    -- right justify
		    else s -> ( n := # s; w := (wid - n + 1)//2; (w, s, wid-w-n)) -- center
		    )
	       ));
     b = transpose prepend(leftside,b);
     toString unsequence lab || "" || stack apply(b, concatenate))

status ChainComplex := opts -> C -> (
    if C.?Resolution then status(C.Resolution, opts)
    else error "status: expected a resolution constructed in the engine")

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2"
-- End:
