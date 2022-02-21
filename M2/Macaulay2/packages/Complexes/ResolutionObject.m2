-- todo 14 Feb 2022
-- our hook for resolutionInEngine (Strategy1) seems to be working.
--   (well, close: restarting a computation with large LengthLimit, resetting to
--   a small LengthLimit still computes lots of stuff in higher degrees.
--   See example: 20 generators, of degrees 1, ..., 20 below.
-- connect up Strategies 0, 2, 3?  
-- then inhomogeneous, resolutionBySyzygies...

importFrom_Core { "RawComputation", "raw" }
importFrom_Core { "degreeToHeft", 
    "rawBetti", 
    "rawStartComputation", 
    "rawGBSetStop", 
    "rawStatus1", 
    "rawGBBetti", "rawResolution",
    "rawResolutionGetFree", "rawResolutionGetMatrix"
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
    -- LengthLimit prescribes the length of the computed complex
    -- DegreeLimit is a lower limit on what will be computed degree-wise, but more might be computed.
    local C;
    if M.cache.?Resolution then (
        C = M.cache.Resolution;
        if not C.cache.?LengthLimit or not C.cache.?DegreeLimit then
            error "internal error: Resolution should have both a LengthLimit and DegreeLimit";
        if C.cache.LengthLimit >= opts.LengthLimit and C.cache.DegreeLimit >= opts.DegreeLimit then
            return naiveTruncation(C, -infinity, opts.LengthLimit);
        remove(M.cache, symbol Resolution); -- will be replaced below
        );
    if M.cache.?ResolutionObject then (
        RO := M.cache.ResolutionObject;
        if opts.Strategy === null or opts.Strategy === RO.Strategy
        then (
            if RO.isComputable(opts.LengthLimit, opts.DegreeLimit) -- this is informational: does not change RO.
            then (
                RO.compute(opts.LengthLimit, opts.DegreeLimit); -- it is possible to interrupt this and then the following lines do not happen.
                C = RO.complex(opts.LengthLimit);
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

    -- the following will return a complex (or error), 
    -- and perhaps modify M.cache.ResolutionObject, M.Resolution?    
    C = runHooks((freeResolution, Module), (opts, M), Strategy => opts.Strategy);
    
    if C =!= null then (
        assert(instance(C, Complex));
        C.cache.LengthLimit = if max C < opts.LengthLimit then infinity else opts.LengthLimit;
        C.cache.DegreeLimit = opts.DegreeLimit;
        C.cache.Module = M;
        M.cache.Resolution = C;
        return C;
        );    
    
    
    -- each hook must do the following:
    -- set Strategy.
    -- create any data it needs (in the RO object).
    -- place functions RO.isComputatable, RO.compute, RO.complex into RO. (or have some other way of doing that).
    -- or, perhaps, make a ResolutionObjectHook type, and have each hook create methods on that.
    -- either way, each hook has to create these functions...

    -- Question: where is the actual computation happening? In the hook!
    
    error("no method implemented to handle this ring and module");
    );

resolutionInEngine = (opts, M) -> (
    -- opts are the options from resolution.  Includes Strategy, LengthLimit, DegreeLimit.
    -- M is a Module.
    
    -- first determine if this method applies.  
    -- Return null if not, as quickly as possible
    R := ring M;
    if not (
        R.?Engine and
        heft R =!= null and
        isHomogeneous M and
        (isCommutative R or isSkewCommutative R) and (
            A := ultimate(coefficientRing, R);
            A =!= R and isField A
        ))
    then return null;

    -- otherwise: we set the Strategy to be Engine.
    RO := M.cache.ResolutionObject;  -- this must exist?
    if RO.Strategy === null then RO.Strategy = Engine;

    if RO.?RawComputation then error "internal error: our logic is wrong";

    lengthlimit := if opts.LengthLimit === infinity 
        then (
            flatR := first flattenRing R;
            if ideal flatR != 0 then 
                error "need to provide LengthLimit for free resolutions over quotients of polynomial rings";
            numgens flatR)
        else opts.LengthLimit;
    << "creating raw computation" << endl;
    RO.RawComputation = rawResolution(
        raw presentation M, -- the matrix
        true,             -- whether to resolve the cokernel of the matrix
        lengthlimit,      -- how long a resolution to make, (hard : cannot be increased by stop conditions below)
        false,            -- useMaxSlantedDegree
        0,                -- maxSlantedDegree (is this the same as harddegreelimit?)
        1,                -- algorithm number, 1 in this case.
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
        -- returns Boolean if the given engine computation can compute the free res to this length and degree.
        << "calling isComputable" << endl;
        lengthlimit <= RO.LengthLimit
        );

    RO.complex = (lengthlimit) -> (
        << "calling RO.complex" << endl;
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

addHook((freeResolution, Module), resolutionInEngine, Strategy => Engine)


--scan({Engine}, strategy ->
--    addHook(key := (resolution, Module), algorithms#key#strategy, Strategy => strategy))


end--
restart
debug loadPackage("Complexes")
load "Complexes/ResolutionObject.m2"
gbTrace=1
S = ZZ/101[a..d]
I = ideal(a*b-c*d, a^3-c^3, a*b^2-c*d^2)
M = S^1/I
F = freeResolution(M, Strategy => Engine)
remove(M.cache, symbol Resolution)
peek M.cache
assert isWellDefined F
F2 = freeResolution(M, LengthLimit => 2)
dd^F2
betti F2
F3 = freeResolution(M, LengthLimit => 3)
F3 = freeResolution(M, LengthLimit => 10)

I = ideal(a*b-c*d, a^3-c^3, a*b^2-c*d^2)
M = S^1/I
F1 = freeResolution(M, LengthLimit => 2)
F2 = freeResolution(M, LengthLimit => 3)
F3 = freeResolution(M, LengthLimit => 5)


restart
debug loadPackage("Complexes")
load "Complexes/ResolutionObject.m2"
gbTrace=1
S = ZZ/101[vars(0..20)]
I = ideal for i from 1 to numgens S list S_(i-1)^i
M = S^1/I
F = freeResolution(M, Strategy => Engine)
-- control-c in the middle, look at M.cache.ResolutionObject
peek M.cache.ResolutionObject
freeResolution(M, LengthLimit => 4)
assert isWellDefined F
F2 = freeResolution(M, LengthLimit => 2)

-- XXX
restart
debug loadPackage("Complexes")
load "Complexes/ResolutionObject.m2"
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
F = freeResolution(M, Strategy => 2)
assert isWellDefined F

M = comodule ideal(a*b-c*d, a^3-c^3, a*b^2-c*d^2)
F = freeResolution(M, Strategy => 0)
assert isWellDefined F

M = comodule ideal(a*b-c*d, a^3-c^3, a*b^2-c*d^2)
F = freeResolution(M, Strategy => 3)
assert isWellDefined F

I = ideal(a*b-c*d, a^3-c^3, a*b^2-c*d^2)
F = freeResolution(I, Strategy => 4) -- nonminimal...
assert isWellDefined F

I = ideal I_*
F = freeResolution(I, DegreeLimit => 3, LengthLimit => 2)
F = freeResolution(I, DegreeLimit => 2, LengthLimit => 2)
F = freeResolution I
-- change strategy
-- degree limit, changing degree limit
-- hooks
-- ComputationContext stuff...


use S
R = S/(a^2, b^2, c^2, d^2)
I = ideal(a,b,c,d)
F = freeResolution I
betti F
assert isWellDefined F

I = ideal(a,b,c,d)
F = freeResolution(I, LengthLimit => 3)
F = freeResolution(I, LengthLimit => 4)
F = freeResolution(I, LengthLimit => 6)
F = freeResolution(I, LengthLimit => 4)

W = resolutionObject(gens I, Strategy => 4)
compute W
F = complex W
assert isWellDefined F
assert(prune HH F == complex comodule I)

raw W
peek W
raw W

restart
R = ZZ/101[a..d]
M = coker vars R
C = res M
C.cache
debug Core
peek M.cache#(new ResolutionContext)
