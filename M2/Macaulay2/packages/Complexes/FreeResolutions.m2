-- todo: created 7 Feb 2023:
--   we just did migration of Truncations.
--   todo: 2 tests in Truncations are failing due to changes we made. DONE
--   todo: get doc references both directions working (recompile M2). DONE
--   next thing: finish tests in Complexes (and FreeResolutions)
--               create doc node(s) for freeResolution
--
-- todo 27 Jan 2023:

--  take test/code at end of FreeResolutions.m2 and make honest tests of these.
--  make sure all resolutions types that we want are being done correctly.
--  write doc for free resolution code.
--  make another pass through all of our code: enough tests, doc is good.  Maybe add in Weyl algebra examples too, etc.
--  after that: start getting it to work with other packages.

-- todo for June/July 2022:
--   we just had finished resolutionBySyzgyies, although maybe some more testing is in order.
--   e.g.: interrupts, Weyl algebra.
--   need: Strategy => Inhomogeneous.
--   . take our example collection and make into a robust set of tests.
--   . write gradedModule function to make a complex out of a hashtable of modules (maps are zero).
--   . nonminimal resolutions
--   . revisit augmentationMap, in case when the resolution 
--          messes with the module generators.

-- "BUGS" found in M2:
--  1. Issue #2405
--   M === N maybe should check equality by pointer first:
--   caused a problem in constructing the augmentation map for resolutions
--   over a field (when field is inexact).
-*
     R = RR_53
     M = coker matrix{{1.3, 1.4}, {1.1, 1.5}, {.3, .6}}
     assert(M === M) -- this is good
     assert(not(M == M)) -- M===M but M != M...
*-

importFrom_Core { 
    "RawComputation", 
    "raw",
    "degreeToHeft", 
    "rawBetti", 
    "rawStartComputation", 
    "rawGBSetStop", 
    "rawStatus1", 
    "rawGBBetti", 
    "rawResolution",
    "rawResolutionGetFree", 
    "rawResolutionGetMatrix",
    "hasNoQuotients",
    "Computation"
    }

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
    local C;
    if opts.LengthLimit < 0 then (
        C = complex (ring M)^0;
        C.cache.Nonminimal = false;
        C.cache.LengthLimit = opts.LengthLimit;
        C.cache.DegreeLimit = infinity;
        C.cache.Module = M;
        return C;
        );
    if M.cache.?Resolution then (
        C = M.cache.Resolution;
        if not C.cache.?LengthLimit or not C.cache.?DegreeLimit then
            error "internal error: Resolution should have both a LengthLimit and DegreeLimit";
        if C.cache.Nonminimal === (opts.Strategy === "Nonminimal") and
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
        if (opts.Strategy === null and RO.Strategy =!= 4) or --4 is the magic number for "Nonminimal".  In this case, we need to recompute the resolution.
            opts.Strategy === RO.Strategy
        then (
            if RO.isComputable(opts.LengthLimit, opts.DegreeLimit) -- this is informational: does not change RO.
            then (
                RO.compute(opts.LengthLimit, opts.DegreeLimit); -- it is possible to interrupt this and then the following lines do not happen.
                C = RO.complex(opts.LengthLimit);
                C.cache.Nonminimal = (RO.Strategy === 4); -- magic number: this means "Nonminimal" to the engine...
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
        C.cache.Nonminimal = (RO.Strategy === 4); -- magic number: this means "Nonminimal" to the engine...
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
        opts.SortStrategy -- sorting strategy, for advanced use
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
        if #modules === 1 then return complex(modules#0, Base => 0);
        maps := hashTable for i from 1 to #modules-1 list (
            i => map(modules#(i-1), modules#i, rawResolutionGetMatrix(RO.RawComputation, i))
            );
        complex maps
        );
    
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
    else if RO.Strategy === "Nonminimal" then RO.Strategy = 4
    else error "our internal logic is flawed";

    gbM := gens gb presentation M;
    resolutionObjectInEngine(opts, M, gbM)
    )

resolutionInEngine = (opts, M) -> (
    R := ring M;
    if isQuotientRing R or isSkewCommutative R then (
        M.cache.ResolutionObject.Strategy = 2;
        resolutionInEngine2(opts ++ {Strategy => 2}, M)
        )
    else if isWeylAlgebra R and isHomogeneous M then (
        M.cache.ResolutionObject.Strategy = 3;
        resolutionInEngine3(opts ++ {Strategy => 3}, M)
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
        C := if numcols first syzmats === 0 then complex M
             else (
                 if numcols last syzmats === 0 then syzmats = drop(syzmats, -1);
                 complex syzmats
                 );
        C.cache.augmentationMap = map(complex M, C, i -> map(M, target presentation M, 1));
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

addHook((freeResolution, Module), resolutionInEngine4, Strategy => "Nonminimal")
addHook((freeResolution, Module), resolutionBySyzygies, Strategy => "Syzygies")
addHook((freeResolution, Module), resolutionByHomogenization, Strategy => "Homogenization")
addHook((freeResolution, Module), resolutionInEngine0, Strategy => 0)
addHook((freeResolution, Module), resolutionInEngine2, Strategy => 2)
addHook((freeResolution, Module), resolutionInEngine3, Strategy => 3)
addHook((freeResolution, Module), resolutionInEngine1, Strategy => 1)
addHook((freeResolution, Module), resolutionInEngine, Strategy => Engine)
addHook((freeResolution, Module), resolutionOverZZ, Strategy => "ZZ")
addHook((freeResolution, Module), resolutionOverField, Strategy => "Field")


debug Core
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


-- This local function comes from m2/betti.m2.
heftvec := (wt1, wt2) -> if wt1 =!= null then wt1 else if wt2 =!= null then wt2 else {}

-- XXX

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

    if not (
        R.?Engine and
        heft R =!= null and
        (isSkewCommutative R or isCommutative R) and (
            A := ultimate(coefficientRing, R);
            A =!= R and isField A
        ))
    then betti freeResolution(M, DegreeLimit => degreelimit, LengthLimit => lengthlimit);

    C = freeResolution(M, DegreeLimit => degreelimit, LengthLimit => lengthlimit + 1, Strategy => "Nonminimal");
    rC := M.cache.ResolutionObject.RawComputation;
    B := unpackEngineBetti rawMinimalBetti(rC,
        if opts.DegreeLimit === infinity then {} else
	if opts.DegreeLimit =!= null     then {opts.DegreeLimit} else {},
	if opts.LengthLimit =!= infinity then {opts.LengthLimit} else {});
    betti(B, Weights => heftvec(opts.Weights, heft R))
    )

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
    -- First, we need to comppute the non-minimal resolution to one further step.
    if instance(opts.LengthLimit, ZZ) then lengthlimit = lengthlimit + 1;
    C = resolution(M,
	StopBeforeComputation => true, FastNonminimal => true,
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


end--


restart
debug needsPackage("Complexes")
gbTrace=1
S = ZZ/101[vars(0..20)]
I = ideal for i from 1 to numgens S list S_(i-1)^i
M = S^1/I
F = freeResolution(M, Strategy => Engine)
-- control-c in the middle, look at M.cache.ResolutionObject
peek M.cache.ResolutionObject
F = freeResolution(M, LengthLimit => 4) -- BUG? Doesn't seem to finish.
F = freeResolution(M, Strategy => Engine, LengthLimit => 4) -- This one works.
assert isWellDefined F
F2 = freeResolution(M, LengthLimit => 2) -- BUG? Doesn't seem to finish.


-- XXXX

restart
debug loadPackage("Complexes")
gbTrace=1
kk = ZZ/101
A = kk[a,b,c]
B = A/(a^2, b^3-c^3)
C = B[d, Join => false]
I = ideal(c^2*d, a*b^2-c^2*d)
M = comodule I
freeResolution(M, LengthLimit => 10)
freeResolution(M, LengthLimit => 5, DegreeLimit => 4)
freeResolution(M, LengthLimit => 4)
freeResolution(M, LengthLimit => 6, DegreeLimit => 1)
M = comodule ideal(a*b-c*d, a^3-c^3, a*b^2-c*d^2)
F = freeResolution(M, Strategy => 2, LengthLimit => 5)
assert isWellDefined F

M = comodule ideal(a*b-c*d, a^3-c^3, a*b^2-c*d^2)
F = freeResolution(M, Strategy => 0, LengthLimit => 5)
assert isWellDefined F

M = comodule ideal(a*b-c*d, a^3-c^3, a*b^2-c*d^2)
F = freeResolution(M, Strategy => 3, LengthLimit => 6)
assert isWellDefined F

I = ideal(a*b-c*d, a^3-c^3, a*b^2-c*d^2)
F = freeResolution(I, Strategy => 4, LengthLimit => 6) -- nonminimal... NOT PUT BACK INTO Complexes yet...
F = freeResolution(I, Strategy => Nonminimal, LengthLimit => 6) -- nonminimal... NOT PUT BACK INTO Complexes yet... BUG
assert isWellDefined F

I = ideal I_*
F = freeResolution(I, DegreeLimit => 3, LengthLimit => 2)
F = freeResolution(I, DegreeLimit => 2, LengthLimit => 2)
F = freeResolution(I, LengthLimit => 4)


-- Resolution by syzygies
-- #1 Over ZZ needs separate hook. Done!
  restart
  needsPackage "Complexes"
  
  m = matrix{{2,3,7},{7,14,21}}
  M = coker m
  -- new code:
  C = freeResolution M -- BUG: not a resolution!!
  f = augmentationMap C
  assert isWellDefined f
  assert isQuasiIsomorphism f
  assert(HH coker f == 0)

  M = coker matrix {{2,3,7},{7,14,21}}
  C = freeResolution(M, Strategy => "Syzygies") 
  f = augmentationMap C
  assert isWellDefined f
  assert isQuasiIsomorphism f
  assert(HH coker f == 0)

  -- Weyl algebra example
  -- Exterior algebra example
  -- inhomog example
  -- Orlik-Solomon algebra
  needsPackage "HyperplaneArrangements"
  A = typeA 3
  E = ZZ/101[e_1..e_6, SkewCommutative => true]
  I = orlikSolomon(A, E)
  F = freeResolution(E^1/I, LengthLimit => 7, Strategy => "Syzygies")
  isWellDefined F
  -- TODO: write gradedModule function to make a complex out of a hashtable of modules (maps are zero).
  hashTable for i from 0 to length F - 1 list i => prune HH_i F
  betti F


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

---- Inhomogeneous
restart
needsPackage "Complexes"
kk = ZZ/32003
A = kk[a,b,c,d]
I = ideal"a2b-c2, abc-d2"
isHomogeneous I
debugLevel = 1
gbTrace=1
elapsedTime C1 = freeResolution I
assert isWellDefined C1
f = augmentationMap C1
assert isWellDefined f
assert isQuasiIsomorphism f
assert(coker f == 0)

I = ideal I_*
elapsedTime C2 = freeResolution(I, Strategy => "Syzygies")
peek C1.cache
peek C2.cache

I = ideal"a7b-c2, ab5c-d2, a5c-c2-d2, a3b3-c2d-a2"
isHomogeneous I
debugLevel = 1
gbTrace=1
elapsedTime C1 = freeResolution I
I = ideal I_*
elapsedTime C2 = freeResolution(I, Strategy => "Syzygies")
peek C1.cache
peek C2.cache

-- homogeneous quotient ring as coefficient ring
restart
loadPackage "Complexes"
kk = ZZ/32003
A = kk[s,t]/(s^3, t^3)
B = A[x,y,z, Join => false]
I = ideal(s*x-t*y, s^2*z^3-x^5)
isHomogeneous I
C = freeResolution(I, LengthLimit => 4)
isWellDefined C

-- Test of Strategy => Homogenization
restart
needsPackage "Complexes"
kk = ZZ/32003
A = kk[s,t]/(s^2-t^3, t^5)
B = A[x,y,z, Join => false]
I = ideal(s*x-t*y, s^2*z^3-x^4)
isHomogeneous I
debugLevel = 1
gbTrace=1
res I -- BUG! (one we don't care about)
res(I, Strategy => "Syzygies", LengthLimit => 3)
freeResolution(I, Strategy => "Syzygies", LengthLimit => 3)
debugLevel = 3
gbTrace = 1
printLevel = 1
I = ideal I_*
freeResolution(I, LengthLimit => 3) -- how to tell what strategy is being used

-- Test of interruptability: Strategy => Homogenization
restart
debug needsPackage "Complexes"
kk = ZZ/32003
R = kk[vars(0..17)]
I = ideal for x in gens R list x - random kk
debugLevel = 1
gbTrace=1
C = freeResolution(I, Strategy => "Homogenization")
-- control-c after a couple seconds
peek (coker gens I).cache.ResolutionObject
C = freeResolution(I, Strategy => "Homogenization")
-- look at the number of "."'s.  Totall should be about 18.  If much less,
-- then the computation correctly picked up from where it ended before.


-- Test of nonminimal resolutions
restart
debug needsPackage "Complexes"
kk = ZZ/32003
R = kk[a,b,c,d,e]
I = ideal"abc-de2, abd-c2d, ac2-bd2, abcde"
gbTrace=2
C = freeResolution(I, Strategy => "Nonminimal")
C = freeResolution(I, Strategy => "Nonminimal")
C = freeResolution I
C = freeResolution(I, Strategy => Engine) -- not recomputing
C = freeResolution(I, Strategy => 0) -- not recomputing
C = freeResolution(I, Strategy => 1) -- not recomputing
C = freeResolution(I, Strategy => 2) -- not recomputing
res(I, Strategy => 4) -- nonminimal
res I -- minimal
C = freeResolution I

-- Test of minimalBetti
-- YYY
restart
debug needsPackage "Complexes"
errorDepth=0
kk = ZZ/32003
R = kk[a,b,c,d,e]
gbTrace=2

I = ideal"abc-de2, abd-c2d, ac2-bd2, abcde"
M = comodule I
minimalBetti(M, DegreeLimit => infinity)
peek M.cache
peek M.cache.Resolution.cache
C = freeResolution(M, Strategy => "Nonminimal") -- should not recompute

I = ideal"abc-de2, abd-c2d, ac2-bd2, abcde"
M = comodule I
minimalBetti M
C = freeResolution(M, Strategy => "Nonminimal") -- should not recompute

I = ideal"abc-de2, abd-c2d, ac2-bd2, abcde"
M = comodule I
betti freeResolution M
minimalBetti M -- should not need computation

I = ideal"abc-de2, abd-c2d, ac2-bd2, abcde"
M = comodule I
freeResolution M
minimalBetti(M, DegreeLimit => 6, LengthLimit => 3) -- need to truncate it down
minimalBetti(M, DegreeLimit => 6, LengthLimit => 4) -- need to truncate it down
minimalBetti(M, DegreeLimit => 6)
minimalBetti M

restart
debug needsPackage "Complexes"
errorDepth=0
kk = ZZ/32003
R = kk[a,b,c,d,e]
gbTrace=2

-- TODO: we want Nonminimal to be able to take inhomogeneous input.
I = ideal"abc-de2, abd-c2d, ac2-bd, abcde-d2-e2"
M = comodule I
freeResolution(M, Strategy => "Nonminimal")
freeResolution(M, Strategy => 4)

-- XXX Start here: make an example for inhomg resolution computation. DONE
-- 25 July 2022.  (Done, sort of: Make examples, test interruptability (for Strategy => Homogenization))
-- We are now working on non minimal resolutions.  They seem to be working.
-- Todo: minimalBetti, doc, snapshot of res, betti.

-- *** need a suite of examples for towers/homogeneity, which algorithm is best?

-- Questions/TODO:
-- How do non-integer degree limits get handled?  e.g. by gb and freeResolution/res
