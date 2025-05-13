-- moved from chaincomplexes.m2

-* TODO
- https://github.com/Macaulay2/M2/issues/647
- https://github.com/Macaulay2/M2/issues/2159
*-

-- needs "gb.m2" -- for GroebnerBasis
-- needs "res.m2" -- needed by minimalBetti
-- needs "chaincomplexes.m2"
-- needs "gradedmodules.m2"
-- needs "hilbert.m2"
-- needs "modules2.m2"

importFrom_Core {
    "rawMinimalBetti",
    "unpackEngineBetti",
}

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

-- local function for selecting and computing the appropriate heft
heftfun := wt -> d -> sum( min(#wt, #d), i -> wt#i * d#i )
heftvec := (wt1, wt2) -> if wt1 =!= null then wt1 else if wt2 =!= null then wt2 else {}

-----------------------------------------------------------------------------
-- betti
-----------------------------------------------------------------------------

betti Matrix := opts -> f -> betti(chainComplex f, opts)

betti Resolution := opts -> X -> (
    -- this version works only for rings of degree length 1
    -- currently if opts.Minimize is true, then an error is given
    -- unless the FastNonminimal=>true option was given for the free resolution.
    B := rawBetti(X.RawComputation, if opts.Minimize then 4 else 0); -- the raw version takes no weight option
    betti(B, Weights => heftvec(opts.Weights, heft ring X)))

betti ChainComplex :=
betti GradedModule := opts -> C -> (
    R := ring C;
    if C.?Resolution and degreeLength R === 1 and heft R === {1} then return betti(C.Resolution, opts);
    if opts.Minimize then error "Minimize=>true is currently only supported for res(...,FastNonminimal=>true)";
    complete C;
    heftfn := heftfun heftvec(opts.Weights, heft R);
    new BettiTally from flatten apply(
	select(pairs C, (i,F) -> class i === ZZ),
	(i,F) -> (
	    if not isFreeModule F then error("betti: expected module at spot ", toString i, " in chain complex to be free");
	    apply(pairs tally degrees F, (d,n) -> (i,d,heftfn d) => n))))

-----------------------------------------------------------------------------
-- minimalBetti
-----------------------------------------------------------------------------

-- method is defined in m2/betti.m2
minimalBetti Ideal  := BettiTally => opts -> I -> minimalBetti(comodule I, opts)
minimalBetti Module := BettiTally => opts -> M -> (
    R := ring M;
    weights := heftvec(opts.Weights, heft R);
    degreelimit := resolutionDegreeLimit(R, opts.DegreeLimit);
    lengthlimit := resolutionLengthLimit(R, opts.LengthLimit);
    -- check to see if a cached resolution is sufficient
    cacheKey := ResolutionContext{};
    if M.cache#?cacheKey and isComputationDone(C := M.cache#cacheKey,
	DegreeLimit => degreelimit, LengthLimit => lengthlimit)
    then return betti(C.Result, Weights => weights);
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
    betti(B, Weights => weights))

-----------------------------------------------------------------------------
-- constructing a chain complex with prescribed Betti table
-----------------------------------------------------------------------------

Ring ^ BettiTally := ChainComplex => (R,B) -> (
    -- donated by Hans-Christian von Bothmer
    -- given a betti Table B and a Ring R make a chainComplex
    -- with zero maps over R  that has betti diagram B.
    -- negative entries are ignored
    -- rational entries produce an error
    -- multigraded R's work only if the betti Tally contains degrees of the correct degree length
    F := new ChainComplex;
    F.ring = R;
    scan(sort pairs B, (k,n) -> (
	    (i, deg, wt) := k; -- (homological degree, multidegree, weight)
	    -- use F_i since it gives 0 if F#i is not there:
	    F#i = F_i ++ R^{n:-deg})); -- this could be a bit slow
    F)

-----------------------------------------------------------------------------
-- pdim and regularity
-----------------------------------------------------------------------------

-- used to be in hilbert.m2, now in betti.m2
pdim Module := M -> length resolution minimalPresentation M

regularity ChainComplex := opts -> C -> regularity betti(C, opts)
regularity        Ideal := opts -> I -> (
    if I == 0 then -infinity else if I == 1 then 0
    else 1 + regularity betti(resolution cokernel generators I, opts))
regularity       Module := opts -> M -> (
    if not isHomogeneous M then error "regularity: expected homogeneous module";
    regularity betti(resolution minimalPresentation M, opts))
