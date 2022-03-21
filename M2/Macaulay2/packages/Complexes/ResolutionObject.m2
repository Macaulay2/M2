-- todo for 28 Feb 2022, or one week after that
-- problem: sometimes when error occurs, RO object should be removed?
-- (1)  see bug below in exterior algebra example. DONE
-- (2) take our example collection and make into a robust set of tests.
-- 
-- our hook for resolutionInEngine (Strategy1) seems to be working.
--   (well, close: restarting a computation with large LengthLimit, resetting to
--   a small LengthLimit still computes lots of stuff in higher degrees.
--   See example: 20 generators, of degrees 1, ..., 20 below.
-- connect up Strategies 0, 2, 3 DONE
-- then (3) inhomogeneous, resolutionBySyzygies (FIRST UP ON March 7), over a field (DONE), over ZZ.
-- then (4) nonminimal resolutions
-- (5) revisit augmentationMap, in case when the resolution 
--   messes with the module generators.

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
    if opts.LengthLimit < 0 then return complex (ring M)^0;
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
    << "creating raw computation" << endl;
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

    << "Doing freeResolution Strategy=>1" << endl;

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

    << "Doing freeResolution Strategy=>0" << endl;
    RO := M.cache.ResolutionObject;  -- this exists already
    if RO.Strategy === null then RO.Strategy = 0
    else if RO.Strategy =!= 0 then error "our internal logic is flawed";

    << "computing GB in preparation for computing the resolution" << endl;
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

    << "Doing freeResolution Strategy=>2" << endl;

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

    << "Doing freeResolution Strategy=>3" << endl;

    RO := M.cache.ResolutionObject;  -- this exists already
    if RO.Strategy === null then RO.Strategy = 3
    else if RO.Strategy =!= 3 then error "our internal logic is flawed";

    matM := presentation M;
    resolutionObjectInEngine(opts, M, matM)
    )

resolutionInEngine = (opts, M) -> (
    R := ring M;
    if isQuotientRing R or isSkewCommutative R then (
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
        RO.FieldComputation = minimalPresentation M;
        );

    RO.isComputable = (lengthlimit, degreelimit) -> true;

    RO.complex = (lengthlimit) -> (
        N := RO.FieldComputation;
        C := complex N;
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
    
    -- YYY: Start here. This should work with Weyl algebras, etc, 
    -- different lengthlimits.  degreelimits: perhaps.
    
    RO := M.cache.ResolutionObject;
    RO.SyzygyList = new MutableList from {presentation M};
    
    RO.compute = (lengthlimit, degreelimit) -> (
        -- finish
        m := RO.SyzygyList#(#RO.SyzygyList-1);
        );

    RO.isComputable = (lengthlimit, degreelimit) -> true;

    RO.complex = (lengthlimit) -> (
        -- rewrite
        N := RO.FieldComputation;
        C := complex N;
        C.cache.augmentationMap = map(complex M, C, i -> N.cache.pruningMap);
        C);
    
    RO.compute(opts.LengthLimit, opts.DegreeLimit);
    RO.complex(opts.LengthLimit)
    )

addHook((freeResolution, Module), resolutionBySyzygies, Strategy => "Syzygies")
addHook((freeResolution, Module), resolutionInEngine0, Strategy => 0)
addHook((freeResolution, Module), resolutionInEngine2, Strategy => 2)
addHook((freeResolution, Module), resolutionInEngine3, Strategy => 3)
addHook((freeResolution, Module), resolutionInEngine1, Strategy => 1)
addHook((freeResolution, Module), resolutionInEngine, Strategy => Engine)
addHook((freeResolution, Module), resolutionOverField, Strategy => "Field")

end--
restart
debug loadPackage("Complexes")
load "Complexes/ResolutionObject.m2"
gbTrace=1
S = ZZ/101[a..d]
I = ideal(a*b-c*d, a^3-c^3, a*b^2-c*d^2)
M = S^1/I
--F = freeResolution(M, Strategy => Engine)
F = freeResolution(M)

M = S^1/I
freeResolution(M, LengthLimit => 0)

M = S^1/I
freeResolution(M, LengthLimit => 1)

M = S^1/I
freeResolution(M, LengthLimit => -1)
    
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
F3 = freeResolution(M, Strategy => 2)
assert(dd^F3_1 == gens I)

I = ideal(a*b-c*d, a^3-c^3, a*b^2-c*d^2)
M = S^1/I
F3 = freeResolution(M, Strategy => 3)
assert(dd^F3_1 == gens I)

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
F = freeResolution(M, LengthLimit => 4)
assert isWellDefined F
F2 = freeResolution(M, LengthLimit => 2)

-- exterior algebra example
restart
debug loadPackage("Complexes")
load "Complexes/ResolutionObject.m2"
E = ZZ/101[a..d, SkewCommutative => true]
I = ideal"ab, acd"
C = freeResolution(I) -- gives an error
break
C = freeResolution(I, LengthLimit => 5)

I = ideal"ab, acd"
C = freeResolution(I, Strategy => 2, LengthLimit => 7)

I = ideal"ab, acd"
C2 = freeResolution(I, Strategy => 3, LengthLimit => 7)
assert(betti C === betti C2)

-- module over a quotient ring (old res.m2 version)
restart
R = ZZ/101[a..d]/(a^2-b^2, a*b*c)
I = ideal"ab, acd"
res(R^1/I, Strategy => "Syzygies")

-- module over a quotient ring
restart
debug loadPackage("Complexes")
load "Complexes/ResolutionObject.m2"
R = ZZ/101[a..d]/(a^2-b^2, a*b*c)
I = ideal"ab, acd"
C0 = freeResolution(I, LengthLimit => 6, Strategy => 0)

I = ideal"ab, acd"
C1 = freeResolution(I, LengthLimit => 6, Strategy => 1)

I = ideal"ab, acd"
C2 = freeResolution(I, LengthLimit => 6, Strategy => 2)

I = ideal"ab, acd"
C3 = freeResolution(I, LengthLimit => 6, Strategy => 3)

C0 == C1
C0 == C2
C0 == C3

C = freeResolution(I, LengthLimit => 6, Strategy => 0)
C = freeResolution(I, LengthLimit => 6, Strategy => 1)
C = freeResolution(I, LengthLimit => 7, Strategy => 1)
C = freeResolution(I, LengthLimit => 5, Strategy => 0)

I = ideal"ab, acd"

-- inhomogeneous example
restart
debug loadPackage("Complexes")
load "Complexes/ResolutionObject.m2"
R = ZZ/101[a..d]
I = ideal"a3-b2, abc-d, a3-d"
freeResolution(I, Strategy=>2)

prune HH C
C = freeResolution(I, LengthLimit => 6)
methods res
--

-- Over the rationals
restart
S = QQ[a..d]
I = ideal(a*b-c*d, a^3-c^3, a*b^2-c*d^2)
M = S^1/I
res M
--F = freeResolution(M, Strategy => Engine)
F = freeResolution(M)

-- Weyl algebra
restart
S = QQ[x,y,Dx,Dy,h, WeylAlgebra => {{x,Dx}, {y,Dy}, h}]
M = coker matrix{{x*Dx, y*Dx}}
isHomogeneous M
gbTrace=1
res(M, Strategy => 2)
I = ideal(a*b-c*d, a^3-c^3, a*b^2-c*d^2)
M = S^1/I
res M
--F = freeResolution(M, Strategy => Engine)
F = freeResolution(M)

-- Over a field
restart -- XXX
debug loadPackage("Complexes")
load "Complexes/ResolutionObject.m2"
gbTrace=1
kk = ZZ/32003
M = coker random(kk^3, kk^2)
F = freeResolution M
g = augmentationMap F
source g == F
target g == complex M
isWellDefined g
coker g == 0
ker g == 0 -- since this is an isomorphism

res M -- BUG: this is wrong. 



-- Over an inexact field
restart -- XXX
debug loadPackage("Complexes")
load "Complexes/ResolutionObject.m2"
gbTrace=1
kk = RR_53
kk = ZZ/101
M = coker random(kk^3, kk^2)
F = freeResolution M
g = augmentationMap F
source g == F
target g == complex M
isWellDefined g
coker g == 0
ker g == 0 -- since this is an isomorphism

res M


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
