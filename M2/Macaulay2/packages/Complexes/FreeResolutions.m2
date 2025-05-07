-- todo: (for 24 April 2023, 1 May 2023)
--  create doc node(s) for freeResolution
--
-- 1. landing page for freeResolution: just the basics.
--    (doesn't discuss any options), the basics. links to Complexes.
--    projectiveResolution, semifreeResolution need to be keywords, or mentioned.
--    injectiveResolution over exterior algebra?
-- 2. basic useful options: LengthLimit, DegreeLimit (partial computation)
-- 3.
--    Each strategy has a page.
--    The reason for this version.  Example: what it does well, what it can't handle.
-- 4. compare and contrast strategies.
--    when to use each one?
--    What if you want to keep the first matrix fixed?
--    What if you want it to give a minimal first matrix?
-- n. less used options.
--
--
--  make another pass through all of our code: enough tests, doc is good.  Add in Weyl algebra examples too, etc.
--  after that: start getting it to work with other packages.

-- Some things to keep in mind.
--   . write gradedModule function to make a complex out of a hashtable of modules (maps are zero).
--   . revisit augmentationMap, in case when the resolution 
--          messes with the module generators.

importFrom_Core { 
    "RawComputation", 
    "raw",
    "degreeToHeft", 
    "rawBetti", 
    "rawMinimalBetti",
    "rawStartComputation", 
    "rawGBSetStop", 
    "rawStatus1", 
    "rawGBBetti", 
    "rawResolution",
    "rawResolutionGetFree", 
    "rawResolutionGetMatrix",
    "unpackEngineBetti",
    "generatorSymbols",
    "hasNoQuotients",
    "Computation"
    }

importFrom_Core "Resolution"

ResolutionObject = new Type of MutableHashTable
ResolutionObject.synonym = "resolution object"
toString ResolutionObject := C -> toString raw C
raw ResolutionObject := X -> X.RawComputation

inf := t -> if t === infinity then -1 else t

freeResolution Module := Complex => opts -> M -> (
    -- This handles caching, hooks for different methods of computing 
    -- resolutions or over different rings which require different algorithms.
    --
    -- Nonminimal: true if the computation is constructed using the Nonminimal strategy.
    -- LengthLimit prescribes the length of the computed complex.
    -- DegreeLimit is a lower limit on what will be computed degree-wise, but more might be computed.
    R := ring M;
    local C;
    if M === R^0 or opts.LengthLimit < 0
    then (
        C = complex R^0;
        if not M.cache.?Resolution then 
            M.cache.Resolution = C;
        return C;
        );
    if M.cache.?Resolution then (
        C = M.cache.Resolution;
        if not C.cache.?LengthLimit or not C.cache.?DegreeLimit then
            error "internal error: Resolution should have both a LengthLimit and DegreeLimit";
        if C.cache.Nonminimal === (opts.Strategy === Nonminimal) and
           C.cache.LengthLimit >= opts.LengthLimit and 
           C.cache.DegreeLimit >= opts.DegreeLimit then (
               C' := naiveTruncation(C, -infinity, opts.LengthLimit);
               C'.cache.LengthLimit = opts.LengthLimit;
               C'.cache.DegreeLimit = C.cache.DegreeLimit;
               C'.cache.Module = C.cache.Module;
               return C';
               );
        remove(M.cache, symbol Resolution); -- will be replaced below
        );
    if M.cache.?ResolutionObject then (
        RO := M.cache.ResolutionObject;
        if (opts.Strategy === null and RO.Strategy =!= 4) or --4 is the magic number for Nonminimal.  In this case, we need to recompute the resolution.
            opts.Strategy === RO.Strategy
        then (
            if RO.isComputable(opts.LengthLimit, opts.DegreeLimit) -- this is informational: does not change RO.
            then (
                RO.compute(opts.LengthLimit, opts.DegreeLimit); -- it is possible to interrupt this and then the following lines do not happen.
                C = RO.complex(opts.LengthLimit);
                C.cache.Nonminimal = (RO.Strategy === 4); -- magic number: this means Nonminimal to the engine...
                C.cache.LengthLimit = if max C < opts.LengthLimit then infinity else opts.LengthLimit;
                C.cache.DegreeLimit = opts.DegreeLimit;
                C.cache.Module = M;
                M.cache.Resolution = C;
                return C;
                )
            );
        remove(M.cache, symbol ResolutionObject);
        );

    RO = new ResolutionObject from {
        symbol ring => ring M,
        symbol LengthLimit => opts.LengthLimit,
        symbol DegreeLimit => opts.DegreeLimit,
        symbol Strategy => opts.Strategy,
        symbol Module => M
        };
    M.cache.ResolutionObject = RO;

    -- the following will return a complex (or null), 
    C = runHooks((freeResolution, Module), (opts, M), Strategy => opts.Strategy);
    
    if C =!= null then (
        assert(instance(C, Complex));
        C.cache.Nonminimal = (RO.Strategy === 4); -- magic number: this means Nonminimal to the engine...
        C.cache.LengthLimit = if max C < opts.LengthLimit then infinity else opts.LengthLimit;
        C.cache.DegreeLimit = opts.DegreeLimit;
        C.cache.Module = M;
        M.cache.Resolution = C;
        return C;
        );    
    
    remove(M.cache, symbol ResolutionObject);
    if opts.Strategy === null then     
        error("no method implemented to handle this ring and module");
    error "provided Strategy does not handle this ring and module";        
    );

resolutionObjectInEngine = (opts, M, matM) -> (
    RO := M.cache.ResolutionObject;
    R := ring M;
    if RO.?RawComputation then error "internal error: our logic is wrong";

    lengthlimit := if opts.LengthLimit === infinity 
        then (
            if isSkewCommutative R then (
                -- we remove the ResolutionObject from M.cache since 
                -- otherwise it is in an incomplete and unrecoverable state
                remove(M.cache, symbol ResolutionObject);
                error "need to provide LengthLimit for free resolutions over skew-commutative rings";
                );
            flatR := first flattenRing R;
            if ideal flatR != 0 then (
                -- we remove the ResolutionObject from M.cache since 
                -- otherwise it is in an incomplete and unrecoverable state
                remove(M.cache, symbol ResolutionObject);
                error "need to provide LengthLimit for free resolutions over quotients of polynomial rings";
                );
            numgens flatR)
        else opts.LengthLimit;

    RO.RawComputation = rawResolution(
        raw matM,         -- the matrix
        true,             -- whether to resolve the cokernel of the matrix
        lengthlimit,      -- how long a resolution to make, (hard : cannot be increased by stop conditions below)
        false,            -- useMaxSlantedDegree
        0,                -- maxSlantedDegree (is this the same as harddegreelimit?)
        RO.Strategy,      -- algorithm number, 0, 1, 2 or 3...
        opts.SortStrategy, -- sorting strategy, for advanced use
        opts.ParallelizeByDegree -- only valid with Nonminimal
        );
    RO.returnCode = rawStatus1 RO.RawComputation; -- do we need this?

    RO.compute = (lengthlimit, degreelimit) -> (
        deglimit := if degreelimit === infinity then {} else degreeToHeft(R, degreelimit);
        rawGBSetStop(
            RO.RawComputation,
            false,                                      -- always_stop
            deglimit,                                   -- degree_limit
            0,                                          -- basis_element_limit (not relevant for resolutions)
            -1, -- inf opts.SyzygyLimit,                       -- syzygy_limit
            -1, -- inf opts.PairLimit,                         -- pair_limit
            0,                                          -- codim_limit (not relevant for resolutions)
            0,                                          -- subring_limit (not relevant for resolutions)
            false,                                      -- just_min_gens
            {}                                          -- length_limit
            );
        rawStartComputation RO.RawComputation;
        RO.returnCode = rawStatus1 RO.RawComputation;
        RO.DegreeLimit = degreelimit;
        );

    RO.isComputable = (lengthlimit, degreelimit) -> ( -- this does not mutate RO.
        -- returns Boolean value true if the given engine computation can compute the free res to this length and degree.
        lengthlimit <= RO.LengthLimit
        );

    RO.complex = (lengthlimit) -> (
        -- returns a Complex of length <= lengthlimit
        i := 0;
        modules := while i <= lengthlimit list (
            F := new Module from (R, rawResolutionGetFree(RO.RawComputation, i));
            if F == 0 then break;
            i = i+1;
            F
            );
        if #modules === 0 then return complex R^0;
        if #modules === 1 then return complex(modules#0, Base => 0);
        maps := hashTable for i from 1 to #modules-1 list (
            i => map(modules#(i-1), modules#i, rawResolutionGetMatrix(RO.RawComputation, i))
            );
        complex maps
        );

    if not opts.StopBeforeComputation then
        RO.compute(opts.LengthLimit, opts.DegreeLimit);
    RO.complex(opts.LengthLimit)
    )

resolutionInEngine1 = (opts, M) -> (
    -- opts are the options from resolution.  Includes Strategy, LengthLimit, DegreeLimit.
    -- M is a Module.
    
    -- first determine if this method applies.  
    -- Return null if not, as quickly as possible
    R := ring M;
    if not (
        R.?Engine and
        heft R =!= null and
        isHomogeneous M and
        isCommutative R and (
            A := ultimate(coefficientRing, R);
            A =!= R and isField A
        ))
    then return null;

    if gbTrace > 0 then
      << "[Doing freeResolution Strategy => 1]" << endl;

    RO := M.cache.ResolutionObject;  -- this exists already
    if RO.Strategy === null then RO.Strategy = 1
    else if RO.Strategy =!= 1 then error "our internal logic is flawed";

    matM := presentation M;
    resolutionObjectInEngine(opts, M, matM)
    )

resolutionInEngine0 = (opts, M) -> (
    -- opts are the options from resolution.  Includes Strategy, LengthLimit, DegreeLimit.
    -- M is a Module.
    
    -- first determine if this method applies.  
    -- Return null if not, as quickly as possible
    R := ring M;
    if not (
        R.?Engine and
        heft R =!= null and
        isHomogeneous M and
        isCommutative R and (
            A := ultimate(coefficientRing, R);
            A =!= R and isField A
        ))
    then return null;

    if gbTrace > 0 then
      << "[Doing freeResolution Strategy => 0]" << endl;
    RO := M.cache.ResolutionObject;  -- this exists already
    if RO.Strategy === null then RO.Strategy = 0
    else if RO.Strategy =!= 0 then error "our internal logic is flawed";

    gbM := gens gb presentation M;
    resolutionObjectInEngine(opts, M, gbM)
    )

resolutionInEngine2 = (opts, M) -> (
    -- opts are the options from resolution.  Includes Strategy, LengthLimit, DegreeLimit.
    -- M is a Module.
    
    -- first determine if this method applies.  
    -- Return null if not, as quickly as possible
    R := ring M;
    if not (
        R.?Engine and
        heft R =!= null and
        isHomogeneous M and
        (
            A := ultimate(coefficientRing, R);
            A =!= R and isField A
        ))
    then return null;

    if gbTrace > 0 then
      << "[Doing freeResolution Strategy => 2]" << endl;

    RO := M.cache.ResolutionObject;  -- this exists already
    if RO.Strategy === null then RO.Strategy = 2
    else if RO.Strategy =!= 2 then error "our internal logic is flawed";

    matM := presentation M;
    resolutionObjectInEngine(opts, M, matM)
    )

resolutionInEngine3 = (opts, M) -> (
    -- opts are the options from resolution.  Includes Strategy, LengthLimit, DegreeLimit.
    -- M is a Module.
    
    -- first determine if this method applies.  
    -- Return null if not, as quickly as possible
    R := ring M;
    if not (
        R.?Engine and
        heft R =!= null and
        isHomogeneous M and
        (
            A := ultimate(coefficientRing, R);
            A =!= R and isField A
        ))
    then return null;

    if gbTrace > 0 then 
      << "[Doing freeResolution Strategy => 3]" << endl;

    RO := M.cache.ResolutionObject;  -- this exists already
    if RO.Strategy === null then RO.Strategy = 3
    else if RO.Strategy =!= 3 then error "our internal logic is flawed";

    matM := presentation M;
    resolutionObjectInEngine(opts, M, matM)
    )

resolutionInEngine4 = (opts, M) -> (
    -- opts are the options from resolution.  Includes Strategy, LengthLimit, DegreeLimit.
    -- M is a Module.
    
    -- first determine if this method applies.  
    -- Return null if not, as quickly as possible
    R := ring M;
    if not (
        R.?Engine and
        heft R =!= null and
        (isSkewCommutative R or isCommutative R) and (
            A := ultimate(coefficientRing, R);
            A =!= R and isField A
        ))
    then return null;

    if gbTrace > 0 then
      << "[Doing freeResolution Strategy => 4 (Nonminima)]" << endl;
    RO := M.cache.ResolutionObject;  -- this exists already
    if RO.Strategy === null then RO.Strategy = 4
    else if RO.Strategy === Nonminimal then RO.Strategy = 4
    else error "our internal logic is flawed";

    gbM := gens gb presentation M;
    resolutionObjectInEngine(opts, M, gbM)
    )

resolutionInEngine = (opts, M) -> (
    R := ring M;
    if isQuotientRing R or isSkewCommutative R 
         or (isWeylAlgebra R and isHomogeneous M) then (
        M.cache.ResolutionObject.Strategy = 2;
        resolutionInEngine2(opts ++ {Strategy => 2}, M)
        )
    else (
        M.cache.ResolutionObject.Strategy = 1;
        resolutionInEngine1(opts ++ {Strategy => 1}, M)
        )
    )

resolutionOverField = (opts, M) -> (
    R := ring M;
    if not isField R then return null;
    RO := M.cache.ResolutionObject;
    
    RO.compute = (lengthlimit, degreelimit) -> (
        RO.Computation = minimalPresentation M;
        );

    RO.isComputable = (lengthlimit, degreelimit) -> true;

    RO.complex = (lengthlimit) -> (
        N := RO.Computation;
        C := complex N;
        C.cache.augmentationMap = map(complex M, C, i -> 
            if i === 0 then map(M, C_0, matrix N.cache.pruningMap));
        C);
    
    RO.compute(opts.LengthLimit, opts.DegreeLimit);
    RO.complex(opts.LengthLimit)
    )

resolutionOverZZ = (opts, M) -> (
    R := ring M;
    if R =!= ZZ then return null;
    RO := M.cache.ResolutionObject;
    
    RO.compute = (lengthlimit, degreelimit) -> (
        RO.Computation = minimalPresentation M;
        );

    RO.isComputable = (lengthlimit, degreelimit) -> true;

    RO.complex = (lengthlimit) -> (
        N := RO.Computation;
        C := complex {presentation N};
        C.cache.augmentationMap = map(complex M, C, i -> 
            if i === 0 then map(M, C_0, matrix N.cache.pruningMap));
        C);
    
    RO.compute(opts.LengthLimit, opts.DegreeLimit);
    RO.complex(opts.LengthLimit)
    )

resolutionBySyzygies = (opts, M) -> (
    -- if the ring R is an approximate field, return null.
    -- otherwise, 'syz' should be working for all other rings, and
    -- so this method should in principal work.
    R := ring M;
    -- TODO: return null for any rings such that syz is not available.
    -- TODO: are there any such rings?  Perhaps inexact fields...
    
    RO := M.cache.ResolutionObject;

    RO.SchreyerOrder = instance(R, PolynomialRing) or (
        baserings := R.baseRings;
        #baserings =!= 0 and instance(last baserings, PolynomialRing)
        );

    RO.SyzygyList = new MutableList from {
        if RO.SchreyerOrder then schreyerOrder presentation M 
        else presentation M
        };
    
    RO.compute = (lengthlimit, degreelimit) -> (
        if lengthlimit === infinity then (
            if isWeylAlgebra R then (
                -- TODO: there are better estimates on the global dimension
                -- of R depending on the coefficient ring
                K := ultimate(coefficientRing, R);
                lengthlimit = # gens(R, CoefficientRing => K);
                if K === ZZ then lengthlimit = lengthlimit + 1;
                )
            else if hasNoQuotients R and not isSkewCommutative R then (
                K = ultimate(coefficientRing, R);
                lengthlimit = # gens(R, CoefficientRing => K);
                if K === ZZ then lengthlimit = lengthlimit + 2; -- +1 if we can change the first matrix.
                )
            else (
                remove(M.cache, symbol ResolutionObject);
                error "require LengthLimit to be finite";
                );
            );
        m := RO.SyzygyList#(#RO.SyzygyList-1);
        for i from #RO.SyzygyList to lengthlimit-1 do (
            if numcols m == 0 then return;
            msyz := syz m; -- add degree limit...
            if RO.SchreyerOrder then msyz = schreyerOrder msyz;
            RO.SyzygyList#i = msyz;
            m = msyz;
            );
        );

    RO.isComputable = (lengthlimit, degreelimit) -> true;

    RO.complex = (lengthlimit) -> (
        syzmats := toList RO.SyzygyList;
        C := if numcols first syzmats === 0 then complex target first syzmats
             else (
                 if numcols last syzmats === 0 then syzmats = drop(syzmats, -1);
                 complex syzmats
                 );
        C.cache.augmentationMap = map(complex M, C, i -> map(M, C_0, 1));
        C);
    
    RO.compute(opts.LengthLimit, opts.DegreeLimit);
    RO.complex(opts.LengthLimit)
    )

protect HomogenizedModule
protect DehomogenizationMap
protect HomogenizedModuleResolution
protect Nonminimal

resolutionByHomogenization = (opts, M) -> (
    R := ring M;
    if isHomogeneous M or 
        not isCommutative R or 
        not degreeLength R === 1 
    then return null;

    RO := M.cache.ResolutionObject;

    f    := presentation M;
    p    := presentation R;
    A    := ring p;
    k    := coefficientRing A;
    if not isHomogeneous k then return null;
    n    := numgens A;
    X    := local X;
    N    := monoid [X_0 .. X_n, MonomialOrder => GRevLex, Join => false];
    A'   := k N;
    toA' := map(A',A,(vars A')_{0 .. n-1});
    p'   := toA' p;
    R'   := A'/(ideal p');
    toR' := map(R',R,(vars R')_{0 .. n-1});
    f'   := toR' f;
    pH   := homogenize(generators gb p', A'_n); 
    forceGB pH;
    RH   := A' / ideal pH;
    toRH := map(RH, R', vars RH);
    fH   := homogenize(toRH generators gb f',RH_n);
    forceGB fH;
    MH   := cokernel fH;
    if not isHomogeneous MH then 
        error "oops, our logic involving homogenization is incorrect";
    toR  := map(R, RH, vars R | 1);
    RO.HomogenizedModule = MH;
    RO.DehomogenizationMap = toR;

    RO.compute = (lengthlimit, degreelimit) -> (
        RO.HomogenizedModuleResolution = freeResolution(RO.HomogenizedModule, LengthLimit => lengthlimit, DegreeLimit => degreelimit);
        );

    RO.isComputable = (lengthlimit, degreelimit) -> true;

    RO.complex = (lengthlimit) -> (
        C := RO.DehomogenizationMap RO.HomogenizedModuleResolution;
        C.cache.augmentationMap = map(complex M, C, i -> map(M, target presentation M, 1)); -- TODO: might need some work to determine this?
        C);
    
    RO.compute(opts.LengthLimit, opts.DegreeLimit);
    RO.complex(opts.LengthLimit)
    )

addHook((freeResolution, Module), resolutionInEngine4, Strategy => Nonminimal)
addHook((freeResolution, Module), resolutionBySyzygies, Strategy => Syzygies)
addHook((freeResolution, Module), resolutionByHomogenization, Strategy => Homogenization)
addHook((freeResolution, Module), resolutionInEngine0, Strategy => 0)
addHook((freeResolution, Module), resolutionInEngine2, Strategy => 2)
addHook((freeResolution, Module), resolutionInEngine3, Strategy => 3)
addHook((freeResolution, Module), resolutionInEngine1, Strategy => 1)
addHook((freeResolution, Module), resolutionInEngine, Strategy => Engine)
addHook((freeResolution, Module), resolutionOverZZ, Strategy => OverZZ)
addHook((freeResolution, Module), resolutionOverField, Strategy => OverField)

-- TODO: compare this with the OverZZ strategy above
-- c.f. https://github.com/Macaulay2/M2/issues/3785
-- addHook((freeResolution, Module), Strategy => symbol LLL,
--     (o, M) -> if ring M === ZZ then complex compress LLL presentation M)


cechComplex = method()
cechComplex MonomialIdeal := Complex => B -> (
    if not isSquareFree B then error "expected squarefree monomial ideal";
    R := ring B;
    n := numgens R;
    g := gens R;
    makediff := (s) -> (
        if class s === Symbol then getSymbol("D" | toString s)
        else if class s === IndexedVariable then (
            ds := getSymbol("D"|toString first s); 
            ds_(last s)
            )
        );
    dg := for x in R.generatorSymbols list makediff x;
    weylPairs := for i to #g-1 list g#i => dg#i;
    W := (coefficientRing R)(monoid[g, dg, 
            WeylAlgebra => weylPairs,
            Degrees => entries (id_(ZZ^n) || - id_(ZZ^n))
            ]);
    phi := map(W, R);
    F := dual freeResolution module phi B;
    (lo, hi) := concentration F;
    modules := hashTable for i from lo to hi list i => (
        degs := degrees F_i;
        directSum for d in degs list (
            -- d is a list of -1's and 0's of length n
            W^{-d}/ideal for i to n-1 list if d#i === -1 then W_i * W_(n+i) + 1 else W_(n+i)
            )
        );
    maps := for i from lo+1 to hi list map(modules#(i-1), modules#i, dd^F_i);
    complex(maps, Base => lo)
    )

///
-- once cechComplex has been thought through, we can make a test from
-- this
  restart
  debug needsPackage("Complexes")

  R = ZZ/101[x,y,z]
  cechComplex monomialIdeal(x,y,z)

  R = ZZ/101[a..d]
  C = cechComplex monomialIdeal(a*c,b*c,a*d,b*d)
  prune HH C

  R = ZZ/101[s_0,s_1,t_0,t_1]
  I = monomialIdeal intersect(ideal(s_0,s_1), ideal(t_0,t_1))
  C = cechComplex I
  prune HH C
///

-- This local function comes from m2/betti.m2.
heftvec := (wt1, wt2) -> if wt1 =!= null then wt1 else if wt2 =!= null then wt2 else {}

truncate(BettiTally, ZZ, ZZ) := 
truncate(BettiTally, ZZ, InfiniteNumber) := 
truncate(BettiTally, InfiniteNumber, ZZ) :=
truncate(BettiTally, InfiniteNumber, InfiniteNumber) := BettiTally => {} >> opts -> (B, degreelimit, lengthlimit) -> (
    new BettiTally from for k in keys B list 
        if k#0 <= lengthlimit and 
           k#2 - k#0 <= degreelimit 
        then k => B#k 
        else continue
    )

-----------------------------------------------------------------------------
-- minimalBetti
-----------------------------------------------------------------------------

-- TODO: place this here, not in Core
-- minimalBetti = method(
--     TypicalValue => BettiTally,
--     Options => {
-- 	DegreeLimit => null,
-- 	LengthLimit => infinity,
-- 	Weights => null,
--     ParallelizeByDegree => false -- currently: only used over primes fields of positive characteristic
-- 	})

--- version 1.24.05 version of minimalBetti.
-*
minimalBetti Module := BettiTally => opts -> M -> (
    R := ring M;
    degreelimit := resolutionDegreeLimit(R, opts.DegreeLimit);
    lengthlimit := resolutionLengthLimit(R, opts.LengthLimit);
    -- check to see if a cached resolution is sufficient
    cacheKey := ResolutionContext{};
    if M.cache#?cacheKey and isComputationDone(C := M.cache#cacheKey,
	DegreeLimit => degreelimit, LengthLimit => lengthlimit)
    then return betti(C.Result.Resolution, Weights => opts.Weights);
    -- if not, compute a fast non-minimal resolution
    -- the following line is because we need to make sure we have the resolution
    -- either complete, or one more than the desired minimal betti numbers.
    
    -- We see if we can now compute a non-minimal resolution.
    -- If not, we compute a usual resolution.
    -- TODO: this isn't quite correct.
    useFastNonminimal := not isQuotientRing R and
      char R > 0 and char R < (1<<15);

    if not useFastNonminimal then 
        return betti resolution(M, DegreeLimit => degreelimit, LengthLimit => lengthlimit);
    -- At this point, we think we are good to use the faster algorithm.        
    -- First, we need to comppute the non-minimal resolution to one further step.
    if instance(opts.LengthLimit, ZZ) then lengthlimit = lengthlimit + 1;
    C = resolution(M,
	StopBeforeComputation => true, FastNonminimal => true, ParallelizeByDegree => opts.ParallelizeByDegree,
	DegreeLimit => degreelimit, LengthLimit => lengthlimit);
    rC := if C.?Resolution and C.Resolution.?RawComputation then C.Resolution.RawComputation
    -- TODO: when can this error happen?
    else error "cannot use 'minimalBetti' with this input. Input must be an ideal or module in a
    polynomial ring or skew commutative polynomial ring over a finite field, which is singly graded.
    These restrictions might be removed in the future.";
    --
    B := unpackEngineBetti rawMinimalBetti(rC,
	if opts.DegreeLimit =!= null     then {opts.DegreeLimit} else {},
	if opts.LengthLimit =!= infinity then {opts.LengthLimit} else {}
        );
    betti(B, Weights => heftvec(opts.Weights, heft R))
    )
minimalBetti Ideal := BettiTally => opts -> I -> minimalBetti(
    if I.cache.?quotient then I.cache.quotient
    else I.cache.quotient = cokernel generators I, opts
    )
*-

minimalBetti Module := BettiTally => opts -> M -> (
    R := ring M;
    degreelimit := opts.DegreeLimit;
    if degreelimit === null then degreelimit = infinity;
    lengthlimit := opts.LengthLimit;
    if M.cache.?Resolution then (
        -- check to see if we can use this cached resolution
        C := M.cache.Resolution;
        if not C.cache.Nonminimal and
           degreelimit <= C.cache.DegreeLimit and
           lengthlimit <= C.cache.LengthLimit 
        then (
            return truncate(betti(C, Weights => opts.Weights), degreelimit, lengthlimit);
            );
        );
    A := ultimate(coefficientRing, R);
    if not (
        R.?Engine and
        heft R =!= null and
        (isSkewCommutative R or isCommutative R) and (
            A =!= R and isField A
        ))
    then betti freeResolution(M, DegreeLimit => degreelimit, LengthLimit => lengthlimit);

    if lengthlimit === infinity then (
        -- reset lengthlimit
	nvars := # generators(R, CoefficientRing => A);
	lengthlimit = nvars + if A === ZZ then 1 else 0;
        );
    C = freeResolution(M, DegreeLimit => degreelimit, LengthLimit => lengthlimit + 1,
        Strategy => Nonminimal, StopBeforeComputation => true);
    rC := M.cache.ResolutionObject.RawComputation;
    B := unpackEngineBetti rawMinimalBetti(rC,
        if opts.DegreeLimit === infinity then {} else
	if opts.DegreeLimit =!= null     then {opts.DegreeLimit} else {},
	if opts.LengthLimit =!= infinity then {opts.LengthLimit} else {});
    betti(B, Weights => heftvec(opts.Weights, heft R))
    )

minimalBetti Ideal := BettiTally => opts -> I -> minimalBetti(comodule I, opts)

-*
-- older version of Core version of minimalBetti.  Can't use here?
minimalBetti(Module, Thing) := BettiTally => opts -> (M, junk) -> (
    R := ring M;
    degreelimit := resolutionDegreeLimit(R, opts.DegreeLimit);
    lengthlimit := resolutionLengthLimit(R, opts.LengthLimit);
    -- check to see if a cached resolution is sufficient
    cacheKey := ResolutionContext{};
    if M.cache#?cacheKey and isComputationDone(C := M.cache#cacheKey,
	DegreeLimit => degreelimit, LengthLimit => lengthlimit)
    then return betti(C.Result.Resolution, Weights => opts.Weights);
    -- if not, compute a fast non-minimal resolution
    -- the following line is because we need to make sure we have the resolution
    -- either complete, or one more than the desired minimal betti numbers.
    
    -- We see if we can now compute a non-minimal resolution.
    -- If not, we compute a usual resolution.
    -- TODO: this isn't quite correct.
    useFastNonminimal := not isQuotientRing R and
      char R > 0 and char R < (1<<15);

    if not useFastNonminimal then 
        return betti resolution(M, DegreeLimit => degreelimit, LengthLimit => lengthlimit);
    -- At this point, we think we are good to use the faster algorithm.        
    -- First, we need to compute the non-minimal resolution to one further step.
    if instance(opts.LengthLimit, ZZ) then lengthlimit = lengthlimit + 1;
    C = resolution(M,
	StopBeforeComputation => true, Strategy => Nonminimal,
	DegreeLimit => degreelimit, LengthLimit => lengthlimit);
    rC := if C.?Resolution and C.Resolution.?RawComputation then C.Resolution.RawComputation
    -- TODO: when can this error happen?
    else error "cannot use 'minimalBetti' with this input. Input must be an ideal or module in a
    polynomial ring or skew commutative polynomial ring over a finite field, which is singly graded.
    These restrictions might be removed in the future.";
    --
    B := unpackEngineBetti rawMinimalBetti(rC,
	if opts.DegreeLimit =!= null     then {opts.DegreeLimit} else {},
	if opts.LengthLimit =!= infinity then {opts.LengthLimit} else {});
    betti(B, Weights => heftvec(opts.Weights, heft R))
    )
minimalBetti Ideal := BettiTally => opts -> I -> minimalBetti(
    if I.cache.?quotient then I.cache.quotient
    else I.cache.quotient = cokernel generators I, opts
    )
*-
end--
