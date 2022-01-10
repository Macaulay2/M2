-- todo 10 Dec 2021.
-- 1. res M -- needs length limit if the ring is not a polynomial ring over a field (or ZZ?)
-- 2. resolutionDegreeLimit: should check that argument is null, an integer, or a list of integers of the right length.
--      at the moment (1.19.1): it handles null, integer case, but doesn't verify it is a list otherwise.

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

resolutionObject = method(Options => {
        SortStrategy => 0, 
        Strategy => 0,
        LengthLimit => infinity,
        DegreeLimit => infinity})
resolutionObject Matrix := opts -> M -> (
    W := new ResolutionObject;
    W.ring = ring M;
    -- the following line needs to change logic.
    lengthlimit := if opts.LengthLimit === infinity then numgens W.ring + 1 else opts.LengthLimit;
    W.RawComputation = rawResolution(
      raw M,            -- the matrix
      true,             -- whether to resolve the cokernel of the matrix
      lengthlimit,      -- how long a resolution to make, (hard : cannot be increased by stop conditions below)
      false,	        -- useMaxSlantedDegree
      0,                -- maxSlantedDegree (is this the same as harddegreelimit?)
      opts.Strategy,    -- algorithm number
      opts.SortStrategy	-- strategy
      );
    W.Strategy = opts.Strategy;
    W.LengthLimit = lengthlimit;
    W.returnCode = rawStatus1 W.RawComputation;
    W
    )

compute = method(Options => true)
compute ResolutionObject := {
      DegreeLimit => {},
      SyzygyLimit => infinity,
      PairLimit => infinity
      } >> opts -> (W) -> (
    rawGBSetStop(
        W.RawComputation,
        false,                                      -- always_stop
        degreeToHeft(W.ring, inf opts.DegreeLimit), -- degree_limit
        0,                                          -- basis_element_limit (not relevant for resolutions)
        inf opts.SyzygyLimit,                       -- syzygy_limit
        inf opts.PairLimit,                         -- pair_limit
        0,                                          -- codim_limit (not relevant for resolutions)
        0,                                          -- subring_limit (not relevant for resolutions)
        false,                                      -- just_min_gens
        {}                                          -- length_limit
        );
    rawStartComputation W.RawComputation;
    W.returnCode = rawStatus1 W.RawComputation;
    W.DegreeLimit = opts.DegreeLimit;
    W
    )

-- moduleAt = method()
-- moduleAt(ResolutionObject, ZZ) := (W,i) ->
--     new Module from (W.ring, rawResolutionGetFree(W.RawComputation, i))

-- matrixAt = method()
-- matrixAt(ResolutionObject, ZZ) := (W,i) -> (
--     src := moduleAt(W, i); -- TODO: stash these
--     tar := moduleAt(W, i-1);
--     map(tar, src, rawResolutionGetMatrix(W.RawComputation, i))
--     )

complex ResolutionObject := Complex => opts -> W -> (
    lengthlimit := W.LengthLimit;
    modules := for i from 0 to lengthlimit list 
        new Module from (W.ring, rawResolutionGetFree(W.RawComputation, i));
    maps := hashTable for i from 1 to lengthlimit list (
        if modules#i == 0 then break;
        i => map(modules#(i-1), modules#i, rawResolutionGetMatrix(W.RawComputation, i))
        );
    complex maps
    )

freeResolution Module := Complex => opts -> M -> (
    -- TODO: handle caching and strategies and hooks via Mahrud's new method.
    local F;
    if opts.LengthLimit < 0 then error "expected a non-negative value for LengthLimit";
    
    if not M.cache.?freeResolution
      or M.cache.freeResolution.cache.LengthLimit < opts.LengthLimit
      then M.cache.freeResolution = (
          R := ring M;
          strategy := if instance(opts.Strategy, Number) then opts.Strategy else
              if isQuotientRing R or isSkewCommutative R then 2 else 1;
          -- some strategies require a GB, some don't.  The next line implements this choice.
          f := if member(strategy, {0,4}) then gens gb presentation M else presentation M;
          lengthlimit := defaultLengthLimit(ring M, 0, opts.LengthLimit);
          W := resolutionObject(f, 
              LengthLimit => lengthlimit,
              Strategy => strategy
              );
          compute(W, DegreeLimit => opts.DegreeLimit);
          -- TODO: check that this is complete.
          F = complex W;
          F.cache.LengthLimit = if length F < lengthlimit then infinity else lengthlimit;
          F.cache.Module = M;
          F
         );
    F = M.cache.freeResolution;
    if opts.LengthLimit < length F
    then (
        F = naiveTruncation(F, 0, opts.LengthLimit);
        F.cache.Module = M;
        );
    F
    )

end--
restart
debug needsPackage  "Complexes"
load "ResolutionObject.m2"
gbTrace=1
S = ZZ/101[a..d]
I = ideal(a*b-c*d, a^3-c^3, a*b^2-c*d^2)
F = freeResolution I
assert isWellDefined F
F2 = freeResolution(I, LengthLimit => 2)
dd^F2
betti F2
F3 = freeResolution(I, LengthLimit => 3)
F3 = freeResolution(I, LengthLimit => 10)

I = ideal(a*b-c*d, a^3-c^3, a*b^2-c*d^2)
F = freeResolution(I, Strategy => 2)
assert isWellDefined F

I = ideal(a*b-c*d, a^3-c^3, a*b^2-c*d^2)
F = freeResolution(I, Strategy => 0)
assert isWellDefined F

I = ideal(a*b-c*d, a^3-c^3, a*b^2-c*d^2)
F = freeResolution(I, Strategy => 3)
assert isWellDefined F

I = ideal(a*b-c*d, a^3-c^3, a*b^2-c*d^2)
F = freeResolution(I, Strategy => 4) -- nonminimal...
assert isWellDefined F

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
