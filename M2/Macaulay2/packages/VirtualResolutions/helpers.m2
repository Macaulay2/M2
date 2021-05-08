plotRegion = method()
plotRegion(Function, List, List) := (func, low, high) -> printerr netList(Boxes => false,
    table(min(high - low) + 1, max(high - low) + 1,
	(i, j) -> if func(j + first low, first high - i) then "." else "x"))
plotRegion(List, List, List) := (L, low, high) -> plotRegion(
    (i, j) -> any(L, ell -> i >= ell_0 and j >= ell_1), low, high)

-- input: the NormalToricVariety of a product of projective spaces or its Cox ring
-- output: the dimension vector for the product of projective spaces.
-- Note the dimension is ordered assuming the degree {1,0,...} is first.
dimVector = method()
dimVector NormalToricVariety := X -> dimVector entries transpose fromWDivToCl X
dimVector Ring := S -> dimVector degrees S
dimVector List := deg -> (
    degTally := tally deg; apply(rsort unique deg, i -> degTally_i - 1))

-- data for translating between NormalToricVarieties and TateOnProducts
importFrom_TateOnProducts { "BeilinsonBundles", "CohomRing", "TateData", "TateRingData", "ringData" }

-- input: NormalToricVariety (without the Cox ring cached)
-- output: NormalToricVariety, whose cached Cox ring has Tate Data
normalToricVarietyWithTateData = X -> (
    -- TODO: also check that X is a product of toricProjectiveSpaces
    S := ring X;
    if S.?TateData then X else (
	-- borrowed from productOfProjectiveSpaces in TateOnProducts.m2:288
	e := local e;
	h := local h;
	k := local k;
	kk := coefficientRing S;
	degs := degrees S;
	E := kk[e_0..e_(#degs-1), Degrees => degs, SkewCommutative => true];
	CR := ZZ[h,k];
	tateData := new MutableHashTable;
	tateData#Rings = (S,E);
	tateData#CohomRing = CR;
	tateData#BeilinsonBundles = new MutableHashTable;
	S.TateData = tateData;
	E.TateData = tateData;
	X))

-- input: multigraded polynomial ring with Tate Data
-- output: NormalToricVariety, with the given ring cached in it
normalToricVarietyFromTateData = S -> (
    if S.?variety and S.?variety.?ring and S.?variety.cache.ring.?TateData then return S.variety;
    if not S.?TateData then error "expected a ring with TateData";
    X := cartesianProduct apply(toSequence dimVector S, n -> toricProjectiveSpace(n, CoefficientRing => coefficientRing S));
    X.cache.ring = S; S.variety = X)

-- input: multigraded polynomial ring without Tate Data
-- output: a new multigraded polynomial ring with Tate Data (the generators will be different!)
-- TODO: change this to support arbitrary variable names
imbueRingWithTateData = S0 -> (
    if S0.?TateData then return S0;
    x := local x; e := local e;
    h := local h; k := local k;
    (S, E) := productOfProjectiveSpaces(dimVector S0,
	CoefficientField => coefficientRing S0,
	CohomologyVariables => {h, k}, Variables => {x, e});
    S.variety = normalToricVarietyFromTateData S; S)
