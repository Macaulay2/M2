--------------------------------------------------------------------
-- Faster strategy for ideals using LinearTruncations
--------------------------------------------------------------------
-- TODO:
--  1. get virtualOfPair to also return a map, and check if it is an isomorphism
--  2. can we increase the lower bound to speed up P3xP3 examples?

debug needsPackage "LinearTruncations"

-- TODO: confirm that M2 does ann comodule I === I always
isVirtualOfPair = method(Options => {IrrelevantIdeal => null})
isVirtualOfPair(List, Module) := opts -> (d, M) -> (
    -- Caveat: for modules that are not comodules of an ideal,
    -- this version may give false positives, but that is okay,
    -- because the multigraded regularity code will fall back
    -- to using cohomologyHashTable to check those degrees.
    -- TODO: what is the appropriate check for modules that removes the caveat?
    B := opts.IrrelevantIdeal;
    l := d + dimVector ring M;
    -- TODO: return false if l < all degrees of I
    F := virtualOfPair(res M, {l});
    -- For ideals, this is sufficient:
    -- saturate(ann HH^0 F, B) == ann M
    -- TODO: is caching helping here?
    isVirtual(B, F) and isIsomorphismOfSheaves(B, F.cache.winnowingMap))


isChiH0 = method(Options => {IrrelevantIdeal => null})
isChiH0(List, Module) := opts -> (d, M) -> (
    -- Check that Hilbert function and Hilbert polynomial match,
    -- this imposes a condition on the alternating sum of local cohomology dimensions
    -- We need to invert the search space
    H := hilbertPolynomial(variety ring opts.IrrelevantIdeal, M);
    hilbertFunction(d, M) == sub(H, sub(matrix{d}, QQ)))

multigradedRegularityTruncationStrategy = (X, M, opts) -> (
    -- TODO: also check that X and S are indeed a product of
    -- projective spaces and its Cox ring, otherwise return null
    if instance(M, Ideal) then M = comodule M;
    debugInfo := if debugLevel < 1 then identity else printerr;
    -- For products of projective space, the dimension is the
    -- number of variables minus the rank of the Picard group
    d := dim X;
    S := ring X;
    B := ideal X;
    r := regularity M;
    n := degreeLength S; -- rank of the Picard group
    degs := degrees M;
    -- element-wise minimum of the multi-degrees of generators of M
    mindegs := apply(n, i -> min(degs / (deg -> deg_i)));
    debugInfo demark_", " {
        "Pic X = ZZ^" | toString n,
        "dim X = " | toString d,
        "reg M = " | toString r,
        "mindegs = " | toString mindegs};
    H := hilbertPolynomial(X, M);
    debugInfo \ {
	"HP M = " | toString H,
    	"degs = " | toString degs};
    -- Based on findHashTableCorner from TateOnProducts, P is a multigraded
    -- polynomial ring with n generators for purposes of degree search
    -- TODO: use degreesRing instead? But over a field instead of ZZ
    P := multigradedPolynomialRing toList(n:0);
    -- TODO: why is this the right upper bound?
    high := if opts.UpperLimit =!= null then opts.UpperLimit else apply(n, i -> max({r} | degs / (deg -> deg_i)));
    -- this is just used for shifting the degrees
    low  := mindegs - toList(n:d);
    -- the combinatorial upperbound on regularity from betti numbers
    U0 := regularityBound M;
    -- extend U0 to degrees where the truncation is quasi-linear (see Theorem 2.9 of BES)
    U0  = findRegion({mindegs, high}, M, isQuasiLinear, Inner => U0, IrrelevantIdeal => B);
    -- limit U0 to degrees where H_B^1 vanishes
    U0  = findRegion({mindegs, high}, M, isChiH0,       Outer => U0, IrrelevantIdeal => B);
    debugInfo("Upper bound from LinearTruncations: " | toString U0);

    debugInfo("Searching from ", toString low, " to ", toString high);
    -- ideal of the upperbound
    U := ideal apply(U0, ell -> P_(ell - low));
    -- ideal of the lowerbound
    -- TODO: get the LowerLimit option to work again
    -*if opts.LowerLimit =!= null then opts.LowerLimit else*-
    L := trim ideal 0_P;
    -- this will contain the degrees to be checked with cohomologyHashTable
    R := trim ideal 0_P;
    -- Note: use a heap in the engine for this
    ht := new MutableHashTable from {1_P => P_(high - low)};
    while #ht > 0 do (
	-- TODO: would randomly picking elements work better?
	(elt, val) := min pairs ht;
	remove(ht, elt);
	-- the spot that we check regularity at
	deg := first exponents val + low;
	if elt % L == 0 then ( debugInfo("in L: " | toString deg); continue);
	if val % U == 0 then ( debugInfo("in U: " | toString deg) ) else (
	    if not         isChiH0(deg, M, IrrelevantIdeal => B)
	    or not isVirtualOfPair(deg, M, IrrelevantIdeal => B)
	    then ( debugInfo(" NOT  in regularity: " | toString deg); L = L + ideal elt; continue )
	    else ( debugInfo("maybe in regularity: " | toString deg); R = R + ideal val; )
	    );
	-- add new spots to the search
	scan(P_*, g -> (
		if not ht#?(g * elt)
		and not (g * elt) % L == 0
		then ht#(g * elt) = P_(high - low - first exponents (g * elt))));
	);
    L = (degrees trim L) / (deg -> high - deg);
    R = (degrees R) / (deg -> deg + low);
    debugInfo \ {
        "L = " | toString L,
        "R = " | toString R};
    --
    if #R > 0 then (
    	high' := apply(n, i -> max(R / (deg -> deg_i)));
    	low'  := apply(n, i -> min(R / (deg -> deg_i)) - d);
    	debugInfo("Calling the default strategy with adjusted high and low");
	-- TODO: clump them together in two regions instead of a huge one instead
    	R = multigradedRegularityDefaultStrategy(X, M, opts ++ { LowerLimit => low', UpperLimit => high' }));
    debugInfo("Recalculating minimal generators by adding U");
    R = U + ideal apply(R, ell -> P_(ell - low));
    -- FIXME: maybe remove this before release?
    if R != U then error concatenate(newline, 10:"💥", "\n💥💥 TELL LCH 💥💥\n", 10:"💥");
    -- retrieve the container
    container := opts.cache;
    container.LowerLimit = low;
    container.UpperLimit = high;
    container.Result = apply(flatten entries mingens R, g -> (flatten exponents g) + low));

-- The default strategy applies to both modules and ideals in a product of projective spaces,
-- but by using hooks we allow better strategies to be added later
addHook((multigradedRegularity, NormalToricVariety, Module), Strategy => "Truncation", multigradedRegularityTruncationStrategy)

end--
