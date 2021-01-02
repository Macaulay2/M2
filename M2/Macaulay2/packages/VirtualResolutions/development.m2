--------------------------------------------------------------------
-- Faster strategy for ideals using LinearTruncations
--------------------------------------------------------------------

debug needsPackage "LinearTruncations"

isVirtualOfPair = method(Options => {IrrelevantIdeal => null})
isVirtualOfPair(List, Ideal) := opts -> (d, I) -> (
    B := opts.IrrelevantIdeal;
    l := d + dimVector ring I;
    -- TODO: return false if l < all degrees of I
    F := virtualOfPair(I, {l});
    -- TODO: can caching help here?
    isVirtual(B, F) and saturate(ann HH^0 F, B) == I)

isChiH0 = method(Options => {IrrelevantIdeal => null})
isChiH0(List, Module) := opts -> (d, M) -> (
    -- Check that Hilbert function and Hilbert polynomial match,
    -- this imposes a condition on the alternating sum of local cohomology dimensions
    -- We need to invert the search space
    H := hilbertPolynomial(variety ring opts.IrrelevantIdeal, M);
    hilbertFunction(d, M) == sub(H, sub(matrix{d}, QQ)))

multigradedRegularityIdealStrategy = (X, I, opts) -> (
    -- TODO: also check that X and S are indeed a product of projective spaces and its Cox ring, otherwise return null
    if not instance(I, Ideal) then return null;
    M := comodule I;
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
    -- TODO: why is this the right upper bound?
    high := if opts.UpperLimit =!= null then opts.UpperLimit else apply(n, i -> max({r} | degs / (deg -> deg_i)));
    -- TODO: why is mindegs - toList(n:d) the right lower bound?
    low  := mindegs - toList(n:d);
    -- the upperbound on regularity using LinearTruncations
    T := findRegion({sum mindegs, sum high}, M, isQuasiLinear, IrrelevantIdeal => B);
    --T := regularityBound M; -- took 84 seconds on check_16
    debugInfo("Conjectural Upper Bound: " | toString T);
    --
    debugInfo("Searching from ", toString low, " to ", toString high);
    -- Based on findHashTableCorner from TateOnProducts, P is a multigraded
    -- polynomial ring with n generators for purposes of degree search
    -- TODO: use degreeRing instead? But over a field instead of ZZ
    P := multigradedPolynomialRing toList(n:0);
    -- ideal of the upperbound
    U := ideal apply(T, ell -> P_(ell - low));
    -- ideal of the lowerbound
    -- TODO: get the LowerLimit option to work again
    -*if opts.LowerLimit =!= null then opts.LowerLimit else*-
    L := trim ideal 0_P;
    -- a priori, U is contained in the regularity
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
	    if not isChiH0(deg, M, IrrelevantIdeal => B)
	    or not isVirtualOfPair(deg, I, IrrelevantIdeal => B)
	    then ( debugInfo("NOT in regularity :  " | toString deg); L = L + ideal elt; continue )
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
    	R = multigradedRegularityDefaultStrategy(X, M, opts ++ { LowerLimit => low', UpperLimit => high' }));
    debugInfo("Recalculating minimal generators by adding U");
    R = U + ideal apply(R, ell -> P_(ell - low));
    -- retrieve the container
    container := opts.cache;
    container.LowerLimit = low;
    container.UpperLimit = high;
    container.Result = sort apply(flatten entries mingens R, g -> (flatten exponents g) + low));

-- The default strategy applies to both modules and ideals in a product of projective spaces,
-- but by using hooks we allow better strategies to be added later
addHook((multigradedRegularity, NormalToricVariety, Module), Strategy => symbol Ideal, multigradedRegularityIdealStrategy)

end--
