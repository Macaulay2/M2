---------------------------------------------------------------------------
-- PURPOSE : Construct, display, and study virtual resolutions for products
--           of projective spaces.
--
-- PROGRAMMERS : Ayah Almousa, Christine Berkesch, Juliette Bruce,
--               David Eisenbud, Daniel Erman, Michael Loper,
--               Mahrud Sayrafi, and Greg Smith.
--
-- UPDATE HISTORY : created 14 April 2018 at M2@UW;
--                  updated 15 April 2019 at IMA Coding Sprint.
--                  updated 16 April 2020 for JSAG
--                  updated 17 April 2021 with new regularity algorithm
---------------------------------------------------------------------------
newPackage ("VirtualResolutions",
    Version => "1.4",
    Date => "May 06, 2021",
    Headline => "Methods for virtual resolutions on products of projective spaces",
    Authors =>{
        {Name => "Ayah Almousa",       Email => "aka66@cornell.edu",   HomePage => "http://pi.math.cornell.edu/~aalmousa "},
        {Name => "Christine Berkesch", Email => "cberkesc@umn.edu",    HomePage => "http://math.umn.edu/~cberkesc/"},
        {Name => "Juliette Bruce",     Email => "jebruce2@wisc.edu",   HomePage => "https://juliettebruce.github.io"},
        {Name => "David Eisenbud",     Email => "de@msri.org",         HomePage => "http://www.msri.org/~de/"},
        {Name => "Michael Loper",      Email => "loper012@umn.edu",    HomePage => "http://math.umn.edu/~loper012/"},
        {Name => "Mahrud Sayrafi",     Email => "mahrud@umn.edu",      HomePage => "http://math.umn.edu/~mahrud/"}
        },
    Keywords => {"Commutative Algebra", "Homological Algebra"},
    PackageImports => {"Elimination", "Depth", "Saturation", "SpaceCurves"},
    PackageExports => {"NormalToricVarieties", "TateOnProducts", "LinearTruncations"},
    AuxiliaryFiles => true,
    DebuggingMode => false,
    Certification => {
	 "journal name" => "The Journal of Software for Algebra and Geometry",
	 "journal URI" => "https://msp.org/jsag/",
	 "article title" => "The virtual resolutions package for Macaulay2",
	 "acceptance date" => "19 May 2020",
	 "published article URI" => "https://msp.org/jsag/2020/10-1/p06.xhtml",
	 "published article DOI" => "10.2140/jsag.2020.10.51",
	 "published code URI" => "https://msp.org/jsag/2020/10-1/jsag-v10-n1-x06-VirtualResolutions.zip",
	 "release at publication" => "28038a52dcc3b0ad7adfd2562a9cd6b6414a6636",
	 "version at publication" => "1.2",
	 "volume number" => "10",
	 "volume URI" => "https://msp.org/jsag/2020/10-1/"
	 }
    )

importFrom_Core { "raw", "rawKernelOfGB" }
importFrom_LinearTruncations { "gradedPolynomialRing" }

export{
    "curveFromP3toP1P2",
    "idealSheafGens",
    "isVirtual",
    "virtualOfPair",
    "resolveViaFatPoint",
    "randomRationalCurve",
    "randomMonomialCurve",
    "randomCurveP1P2",
    "multigradedRegularity",
    -- TODO: should we export these or fix NormalToricVarieties/TateOnProducts?
    --"normalToricVarietyFromTateData",
    --"normalToricVarietyWithTateData",
    --"imbueRingWithTateData",
    -- Options
    "LowerLimit",
    "UpperLimit",
    "Attempt",
    "PreserveDegree",
    "GeneralElements"
    }

--------------------------------------------------------------------
--------------------------------------------------------------------
----- CODE
--------------------------------------------------------------------
--------------------------------------------------------------------

-- Helper for saturation with respect to irrelevant ideal
ourSaturation = (I,irr) -> saturate(I, decompose irr, Strategy => Eliminate);

-- Helper for isVirtual, checks whether a module sheafifies to the zero sheaf
-- TODO: export this, either here or in NormalToricVarieties. Could be renamed to isFiniteLength
-- TODO: the "Support" strategy sometimes causes engine error:
-- terminate called after throwing an instance of 'std::logic_error'
--   what():  ERROR: Inserted duplicate entry into a KD tree.
isZeroSheaf = method(TypicalValue => Boolean, Options => { Strategy => null })
isZeroSheaf(NormalToricVariety, Module) := opts -> (X, M) -> isZeroSheaf(ideal X, M, opts)
isZeroSheaf(Ideal,              Module) := opts -> (B, M) -> (
    -- TODO: either cache this, or isSupportedInZeroLocus
    if opts.Strategy === "Support" then isSupportedInZeroLocus(B, M)
    else (J := ann M) == 1 or J != 0 and ourSaturation(J, B) == 1)

isIsomorphismOfSheaves = method(TypicalValue => Boolean, Options => options isZeroSheaf)
isIsomorphismOfSheaves(Ideal, Matrix) := opts -> (B, f) -> (
    isZeroSheaf(B, ker f, opts) and isZeroSheaf(B, coker f, opts))

-- Helper for virtualOfPair
-- TODO: complete this into the dual of submatrixByDegrees
submatrixWinnow = (m, alphas) -> (
    col := positions(degrees source m, deg -> any(alphas, alpha -> all(alpha - deg, x -> x >= 0)));
    row := positions(degrees target m, deg -> any(alphas, alpha -> all(alpha - deg, x -> x >= 0)));
    submatrix(m, row, col))
submatrixWinnowMap = (phi, alphas) -> (
    col := positions(degrees source phi, deg -> any(alphas, alpha -> all(alpha - deg, x -> x >= 0)));
    submatrix(phi, , col))

--------------------------------------------------------------------
--------------------------------------------------------------------
--Input: F a free chain complex on Cox(X), alphas a list of degrees
--Output: A subcomplex of summands generated only in degrees in the list alphas.
--Given a ring and its free resolution, keeps only the summands in resolution of specified degrees
--If the list alphas contains only one element, the output will be summands generated in degree less than or equal to alpha.
--See Algorithm 3.4 of [BES]
--------------------------------------------------------------------
--------------------------------------------------------------------
protect winnowingMap
virtualOfPair = method(Options => {LengthLimit => infinity})
-- TODO: return a Matrix in the Module case and ChainComplexMap in the ChainComplex case
-- TODO: document the winnoingMap
virtualOfPair(Ideal,  List) := ChainComplex => opts -> (I, alphas) -> virtualOfPair(comodule I, alphas, opts)
virtualOfPair(Module, List) := ChainComplex => opts -> (M, alphas) -> (
    R := ring M;
    if M.cache.?resolution then return virtualOfPair(M.cache.resolution, alphas, opts);
    if any(alphas, alpha -> #alpha =!= degreeLength R) then error "degree has wrong length";
    m := schreyerOrder gens gb presentation M;
    m = submatrixWinnow(m, alphas);
    i := 2;
    L := {m} | while m != 0 and i <= opts.LengthLimit list (
        i = i + 1; m = map(R, rawKernelOfGB raw m); m = submatrixWinnow(m, alphas));
    chainComplex L)
virtualOfPair(ChainComplex, List) := ChainComplex => opts -> (F, alphas) -> (
    if any(alphas, alpha -> #alpha =!= degreeLength ring F) then error "degree has wrong length";
    L := chainComplex apply(min F .. max F - 1, i -> submatrixWinnow(F.dd_(i+1), alphas));
    -- winnowingMap is the map M --> HH_0 F
    M := HH_0 F;
    N := HH_0 L;
    -- TODO: if the pruning map is the same for two different alphas, the ability to use the
    -- same map would allow for a significant speedup in isVirtualOfPair and isIsomorphismOfSheaves
    -- how can we cache this in F?
    phi := map(M, source F.dd_0, id_(F_0));
    phi  = submatrixWinnowMap(phi, alphas);
    -- TODO: return a single map or a chain complex map?
    L.cache.winnowingMap = map(M, N, phi);
    L)

--------------------------------------------------------------------
--------------------------------------------------------------------
----- Input: (J,irr,A)=(Ideal,Ideal,List) where J defines a 0-dim
----- subscheme and irr is the irrelevant ideal
----- Output: A virtual resolution of S/J, which is potentially short.
----- Description: This function implements Theorem 4.1 of [BES].
----- In particular, it computes a virtual resolution of S/J by
----- computing a graded minimal free resolution of S/(J\cap B^A).
----- By the theorem this might be a short virtual resolution
--------------------------------------------------------------------
--------------------------------------------------------------------
resolveViaFatPoint = method()
resolveViaFatPoint(Ideal, Ideal, List) := ChainComplex => (J, irr, A) -> (
    L := decompose irr;
    if #A != #L then error("resolveViaFatPoint: expected exponent vector of length " | toString degreeLength irr);
    -- note: decompose doesn't necessarily return in the right order
    Q := intersect for X in L list (
        D := degree X_0;
        d := (select((0..#D-1), i -> D#i == 1))_0;
        X ^ (A#d)
        );
    res intersect (Q, J)
    )

--------------------------------------------------------------------
--------------------------------------------------------------------
-- This method checks if a given complex is a virtual resolution by computing
-- homology and checking whether its annihilator saturates to the whole ring.
-- Input: Ideal irr - the irrelevant ideal of the ring
--       Chain Complex C - proposed virtual resolution
-- Output: Boolean - true if complex is virtual resolution, false otherwise
-- Note: the Determinantal strategy is based on Theorem 1.3 of [Loper2019].
--------------------------------------------------------------------
--------------------------------------------------------------------
-- TODO: can this use winnowingMap?
isVirtual = method(TypicalValue => Boolean, Options => {Strategy => null})
isVirtual(NormalToricVariety, ChainComplex) := opts -> (X,   C) -> isVirtual(ideal X, C)
isVirtual(Ideal,              ChainComplex) := opts -> (irr, C) -> (
    S := ring irr;
    if S =!= ring C then error "isVirtual: expected objects in the same ring";
-- if strategy "determinantal is selected, the method checks virtuality
-- via the depth criterion on the saturated ideals of minors
    debugInfo := if debugLevel < 1 then identity else i -> printerr("isVirtual: the complex is not virtual at homological degree ", toString i);
    if opts.Strategy === "Determinantal" then (
        for i from 1 to length(C) do (
            if rank(source(C.dd_i)) != (rank(C.dd_i) + rank(C.dd_(i+1))) then (
                debugInfo i; return false));
        for i from 1 to length(C) do (
            minor := minors(rank(C.dd_i),C.dd_i);
            minorSat := ourSaturation(minor,irr);
            if depth(minorSat, S) < i then (
                debugInfo i; return false));
    return true);
-- default strategy is calculating homology and checking homology is
-- supported on irrelevant ideal
    for i from 1 to length(C) do (
        if not isZeroSheaf(irr, HH_i C) then (
            debugInfo i; return false));
    true)

--------------------------------------------------------------------
--------------------------------------------------------------------
-- Input: ZZ n - size of subset of generators to check
--       Ideal J - ideal of ring
--       Ideal irr - irrelevant ideal
-- Output: a list of ideals generated by subsets of size n of the generators of J
--         that give the same ideal as J up to saturation by the irrelevant ideal
--------------------------------------------------------------------
--------------------------------------------------------------------
idealSheafGens = method(Options => {GeneralElements => false})
idealSheafGens(ZZ, Ideal, Ideal) := List => opts -> (n, J, irr) -> (
    R := ring(J);
    k := coefficientRing(R);
    Jsat := ourSaturation(J,irr);
    if opts.GeneralElements == true then (
        degs := degrees(J);
        -- place of all unique degrees
        allmatches := unique(apply(degs, i -> positions(degs, j -> j == i)));
        -- creates an ideal where if degrees of generators match
        -- those generators are replaced by one generator that
        -- is a random combination of all generators of that degree
        J = ideal(apply(allmatches, i -> sum(apply(i, j -> random(k) * J_j))));
        );
    lists := subsets(numgens(J), n);
    output := {};
    apply(lists, l -> (
            I := ideal(J_*_l);
            if ourSaturation(I, irr) == Jsat then (
                output = append(output, I);
                );
            )
        );
    output
    )
idealSheafGens(ZZ, Ideal, NormalToricVariety) := List => opts -> (n, J, X) -> (
    idealSheafGens(n, J, ideal X)
    )

--------------------------------------------------------------------
--------------------------------------------------------------------
----- Input: (d,e,F)=(degree,degree,base ring)
----- Output: The ideal of a random rational curve in P1xP2 of
----- degree (d,e) defined over F.
----- Description: This randomly generates 2 forms of degree
----- d and 3 forms of degree e in the ring S (locally defined),
----- and computes the ideal defining the image of the map of the
----- associated map P^1---->P^1xP^2.
--------------------------------------------------------------------
--------------------------------------------------------------------
randomRationalCurve = method()
randomRationalCurve(ZZ, ZZ, Ring) := Ideal => (d, e, F) -> (
    -- Defines P1
    s := getSymbol "s";
    t := getSymbol "t";
    R := F(monoid[s,t]);
    --- Defines P1xP2
    x := getSymbol "x";
    y := getSymbol "y";
    S1 := F(monoid[x_0, x_1]);
    S2 := F(monoid[y_0,y_1,y_2]);
    S := tensor(S1,S2);
    --- Defines P1x(P1xP2)
    U := tensor(R,S);
    uVars := flatten entries vars U;
    --- Defines graph of morphisms in P1x(P1xP2)
    --M1 := matrix {apply(2,i->random({d,0,0},U)),{x_0,x_1}};
    M1 := matrix {apply(2,i->random({d,0,0},U)),{uVars#2,uVars#3}};
    --M2 := matrix {apply(3,i->random({e,0,0},U)),{y_0,y_1,y_2}};
    M2 := matrix {apply(3,i->random({e,0,0},U)),{uVars#4,uVars#5,uVars#6}};
    J := minors(2,M1)+minors(2,M2);
    --- Computes saturation and then eliminates producing curve in P1xP2
    J' := ourSaturation(J,ideal(uVars#0,uVars#1));
    --J' := saturate(J,ideal(uVars#0,uVars#1),MinimalGenerators=>false);
    I := sub(eliminate({uVars#0,uVars#1},J'),S);
    (T, E) := productOfProjectiveSpaces({1, 2},CoefficientField=>F);
    G := map(T,S,(flatten entries vars T));
    G(I)
    )

--------------------------------------------------------------------
--------------------------------------------------------------------
----- Input: (d,e)=(degree,degree)
----- Output: The ideal of a random rational curve in P1xP2 of
----- degree (d,e) defined over ZZ/101
--------------------------------------------------------------------
--------------------------------------------------------------------
randomRationalCurve (ZZ,ZZ) := Ideal => (d,e) -> (
    randomRationalCurve(d,e,ZZ/101)
    )

--------------------------------------------------------------------
--------------------------------------------------------------------
----- Input: (d,e,F)=(degree,degree,base ring)
----- Output: The ideal of a random rational curve in P1xP2 of degree (d,e).
----- Description: This randomly generates 2 monomials of degree
----- d and 3 monomials of degree e in the ring S (locally defined),
----- and computes the ideal defining the image of the map of the
----- associated map P^1---->P^1xP^2.
--------------------------------------------------------------------
--------------------------------------------------------------------
randomMonomialCurve = method()
randomMonomialCurve (ZZ,ZZ,Ring) := Ideal => (d,e,F) -> (
    --- Defines P1
    s := getSymbol "s";
    t := getSymbol "t";
    R := F[s,t];
    --- Defines P1xP2
    x := getSymbol "x";
    y := getSymbol "y";
    S1 := F(monoid[x_0, x_1]);
    S2 := F(monoid[y_0,y_1,y_2]);
    S := tensor(S1,S2);
    --- Defines P1x(P1xP2)
    U := tensor(R,S);
    uVars := flatten entries vars U;
    --- Choose random monomial to define map to P2.
    B := drop(drop(flatten entries basis({e,0,0},U),1),-1);
    f := (random(B))#0;
    --- Defines graph of morphisms in P1x(P1xP2)
    M1 := matrix {{(uVars#0)^d,(uVars#1)^d},{uVars#2,uVars#3}};
    M2 := matrix {{(uVars#0)^e,(uVars#1)^e,f},{uVars#4,uVars#5,uVars#6}};
    J := minors(2,M1)+minors(2,M2);
    --- Computes saturation and then eliminates producing curve in P1xP2
    J' := ourSaturation(J,ideal(uVars#0,uVars#1));
    I := sub(eliminate({uVars#0,uVars#1},J'),S);
    (T, E) := productOfProjectiveSpaces({1, 2},CoefficientField=>F);
    G := map(T,S,(flatten entries vars T));
    G(I)
    )

--------------------------------------------------------------------
--------------------------------------------------------------------
----- Input: (d,e)=(degree,degree)
----- Output: The ideal of a random rational curve in P1xP2 of
----- of degree (d,e) defined over ZZ/101.
--------------------------------------------------------------------
--------------------------------------------------------------------
randomMonomialCurve (ZZ,ZZ) := Ideal => (d,e) -> (
    randomMonomialCurve(d,e,ZZ/101)
    )

--------------------------------------------------------------------
--------------------------------------------------------------------
----- Input: (J)=(ideal of curve in P3)
----- Output: The ideal of a corresponding curve in P1xP2.
----- Description: Given a curve defined by the ideal J in P3,
----- this outputs the ideal I of the curve in P1xP2 given by
----- considering the projection P3---->P1 on the first two variables,
----- and the projection P3----->P2 on the last three variables.
--------------------------------------------------------------------
--------------------------------------------------------------------
curveFromP3toP1P2 = method(Options => {PreserveDegree => true})
curveFromP3toP1P2 (Ideal) := Ideal => opts -> (J) -> (
    --- Defines P3
    w := getSymbol "w";
    R := (coefficientRing ring J) monoid([w_0,w_1,w_2,w_3]);
    rVars := flatten entries vars R;
    J = sub(J,matrix{{R_0,R_1,R_2,R_3}});
    --- Base locus of projection
    BL1 := ideal(rVars#0,rVars#1);
    BL2 := ideal(rVars#1,rVars#2,rVars#3);
    BL := intersect(BL1,BL2);
    --- If PreserveDegree => true checks whether curve intersects base locus;
    --- this ensures the curve has the correct degree and genus.
    if opts.PreserveDegree == true then (
        if (ourSaturation(J+BL1,ideal(rVars))==ideal(rVars) or ourSaturation(J+BL2,ideal(rVars))==ideal(rVars)) then error "Given curve intersects places of projection.";
        );
    --- Defines P1xP2
    x := getSymbol "x";
    y := getSymbol "y";
    S1 := (coefficientRing ring J) monoid([x_0, x_1]);
    S2 := (coefficientRing ring J) monoid([y_0,y_1,y_2]);
    S := tensor(S1,S2);
    --- Defines P3x(P1xP2)
    U := tensor(R,S);
    uVars := flatten entries vars U;
    --- Place curve in P3x(P1xP2)
    C' := sub(J,U);
    --- Defines graph of projection
    M1 := matrix {{uVars#0,uVars#1},{uVars#4,uVars#5}};
    M2 := matrix {{uVars#1,uVars#2,uVars#3},{uVars#6,uVars#7,uVars#8}};
    D := minors(2,M1)+minors(2,M2);
    --- Intersects irrelevant ideal with base locus
    B1 := ideal(apply(4,i->uVars#i));
    B2 := ideal(apply(2,i->uVars#(4+i)));
    B3 := ideal(apply(3,i->uVars#(6+i)));
    B := intersect(B1,B2,B3,sub(BL,U));
    --- Computes saturation and then eliminates producing curve in P1xP2
    K := ourSaturation(C'+D,B);
    I := sub(eliminate({uVars#0,uVars#1,uVars#2,uVars#3},K),S);
    (T, E) := productOfProjectiveSpaces({1, 2},CoefficientField=>(coefficientRing ring J));
    G := map(T,S,(flatten entries vars T));
    G(I)
    )

--------------------------------------------------------------------
--------------------------------------------------------------------
----- Input: (d,e,F)=(degree,genus,base ring)
----- Output: The ideal of a random curve in P1xP2 defined over F.
----- Description: This randomly generates a curve of degree d
----- and genus g in P3, and then computes the ideal of the corresponding
----- curve in P1xP2 given by considering the projection
----- P3---->P1 on the first two variables.
----- and the projection P3----->P2 on the last three variables.
--------------------------------------------------------------------
--------------------------------------------------------------------
randomCurveP1P2 = method(Options => {Attempt => 1000})
randomCurveP1P2 (ZZ,ZZ,Ring) := Ideal => opts -> (d,g,F) -> (
    --- Defines P3
    z := getSymbol "z";
    R := F(monoid[z_0,z_1,z_2,z_3]);
    rVars := flatten entries vars R;
    --- Base locus of projection
    BL1 := ideal(rVars#0,rVars#1);
    BL2 := ideal(rVars#1,rVars#2,rVars#3);
    BL := intersect(BL1,BL2);
    --- Randomly generates curve in P3 until finds one not intersecting
    --- base locus of projection or until Bound is reached.
    C := ideal(0);
    apply(opts.Attempt,i->(
            C = curve(d,g,R);
            if class(C) === Curve then C = ideal(C);
            if (ourSaturation(C+BL1,ideal(rVars))!=ideal(rVars) and ourSaturation(C+BL2,ideal(rVars))!=ideal(rVars)) then break C;
            )
        );
    --- Checks whether curve in P3 intersects base locus of projection;
    --- this ensures the curve has the correct degree and genus.
    if (ourSaturation(C+BL1,ideal(rVars))==ideal(rVars) or ourSaturation(C+BL2,ideal(rVars))==ideal(rVars)) then error "Unable to find curve not intersecting places of projection.";
    --- Defines P1xP2
    curveFromP3toP1P2(C)
    )

--------------------------------------------------------------------
--------------------------------------------------------------------
----- Input: (d,e)=(degree,genus)
----- Output: The ideal of a random curve in P1xP2 over ZZ/101
--------------------------------------------------------------------
--------------------------------------------------------------------
randomCurveP1P2 (ZZ,ZZ) := Ideal => opts -> (d,g) -> (
    randomCurveP1P2(d,g,ZZ/101)
    )

--------------------------------------------------------------------
--------------------------------------------------------------------
-- Multigraded Regularity
--------------------------------------------------------------------
--------------------------------------------------------------------

-- helpers for connecting NormalToricVarieties and TateOnProducts
load "./VirtualResolutions/helpers.m2"

multigradedRegularity = method(
    TypicalValue => List,
    Options => {
        Strategy   => null,
        LowerLimit => null,
        UpperLimit => null
        }
    )

-- keys: none
MultigradedRegularityOptions = new SelfInitializingType of BasicList
MultigradedRegularityOptions.synonym = "multigraded regularity options"

-- keys: LowerLimit, UpperLimit, "HilbertPolynomial", "CohomologyTable"
MultigradedRegularityComputation = new Type of MutableHashTable
MultigradedRegularityComputation.synonym = "multigraded regularity computation"

-- if there is a compatible computation stored in M.cache,
-- returns the computation object, otherwise creates the entry:
--   MultigradedRegularityOptions{} => MultigradedRegularityComputation{ LowerLimit, UpperLimit, Result }
new MultigradedRegularityComputation from Ideal  :=
new MultigradedRegularityComputation from Module := (C, M) -> (
    if instance(M, Ideal) then M = comodule M;
    r := degreeLength ring M;
    -- TODO: are there any options that could go in MultigradedRegularityOptions?
    cacheKey := MultigradedRegularityOptions{};
    try M.cache#cacheKey else M.cache#cacheKey = new MultigradedRegularityComputation from {
        LowerLimit => toList(r :  infinity), -- lower bound of the region checked
        UpperLimit => toList(r : -infinity), -- upper bound of the region checked
        Result => null })

isComputationDone = method(TypicalValue => Boolean, Options => true)
isComputationDone MultigradedRegularityComputation := Boolean => options multigradedRegularity >> opts -> container -> (
    -- this function determines whether we can use the cached result, or further computation is necessary
    instance(container.Result, List)
    and container.LowerLimit <= min \ transpose{container.LowerLimit, opts.LowerLimit}
    and container.UpperLimit >= max \ transpose{container.UpperLimit, opts.UpperLimit})

cacheHit := type -> if debugLevel > 0 then printerr("Cache hit on a ", synonym type, "! 🎉");

cacheComputation = method(Options => true)
cacheComputation MultigradedRegularityComputation := options multigradedRegularity >> opts -> container -> (
    -- this function takes advantage of FunctionClosures by modifying the container
    computation -> (
        if isComputationDone(opts, container) then ( cacheHit class container; container.Result ) else
        if (result := computation(opts, container)) =!= null then ( container.Result = result )))

--------------------------------------------------------------------
--------------------------------------------------------------------
----- Input: (S,M) = (Ring, Module)
-- OR
----- Input: (X,M) = (NormalToricVariety,Module)
----- Output: A list consisting of the minimal elements of the
----- multigraded regularity of M.
----- Description: This computes the multigraded regularity of a
----- module as defined in Definition 1.1 of [Maclagan, Smith 2004].
----- It returns a list of the minimal elements.
----- Caveat: This assumes M is B-saturated already i.e. H^0_B(M)=0
--------------------------------------------------------------------
--------------------------------------------------------------------
-- TODO: instead of Ring, take the irrelevant ideal
multigradedRegularity(Ring,               Ideal)  := o -> (S, I) -> multigradedRegularityHelper( , S, I, o)
multigradedRegularity(NormalToricVariety, Ideal)  := o -> (X, I) -> multigradedRegularityHelper(X,  , I, o)
multigradedRegularity(Ring,               Module) := o -> (S, M) -> multigradedRegularityHelper( , S, M, o)
multigradedRegularity(NormalToricVariety, Module) := o -> (X, M) -> multigradedRegularityHelper(X,  , M, o)

multigradedRegularityHelper = (X, S, M, opts) -> (
    strategy := opts.Strategy;
    -- start from module over Cox ring of a NormalToricVariety and add Tate data
    if instance(X, NormalToricVariety) then X = normalToricVarietyWithTateData X
    -- start from module over productOfProjectiveSpaces and get module over Cox ring of a product of toricProjectiveSpaces
    else if S.?TateData then X = normalToricVarietyFromTateData S
    -- start from module over multigraded polynomial ring and get module over Cox ring of a product of toricProjectiveSpaces
    else X = normalToricVarietyFromTateData imbueRingWithTateData S;
    r := regularity M;
    n := degreeLength ring M;
    -- the multigraded regularity of the zero module is -infinity in every component
    -- TODO: use Hilbert polynomial to detect irrelevant modules quickly
    if M == 0 then return {toList(n : -infinity)};
    opts = opts ++ {
	-- from Proposition 3.7 of [BCHS22] we know reg M \subset mindegs + Eff X
	LowerLimit => if opts.LowerLimit =!= null then opts.LowerLimit else compMin degrees M,
	-- Note: an upper limit that works for all examples isn't known
	UpperLimit => if opts.UpperLimit =!= null then opts.UpperLimit else compMax join(degrees M, {toList(n : r)})};
    -- store a cached computation object in M
    --   MultigradedRegularityOptions{} => MultigradedRegularityComputation{ LowerLimit, UpperLimit, Result }
    container := new MultigradedRegularityComputation from M;
    -- the strategies are stored as hooks under this key
    key := (multigradedRegularity, NormalToricVariety, Module);
    -- this function attempts the strategies in reverse order, or only the specified strategy
    computation := (opts, container) -> runHooks(key, (X, M, opts ++ {cache => container}), Strategy => strategy);
    -- the actual computation of the strategies occurs here
    C := (cacheComputation(opts, container)) computation;
    if C =!= null then C else if strategy === null
    then error("no applicable strategy for ", toString key)
    else error("assumptions for computing multigraded regularity with strategy ", toString strategy, " are not met"))

-- This is the old strategy for products of projective spaces.
-- It is based on a direct sheaf cohomology calculation.
-- See [ABLS20]: https://msp.org/jsag/2020/10-1/p06.xhtml
multigradedRegularityCohomologySearchStrategy = (X, M, opts) -> (
    S := ring X;
    -- TODO: also check that X and S are indeed a product of
    -- projective spaces and its Cox ring, otherwise return null
    if instance(M, Ideal) then M = comodule M;
    if ring M =!= S then M = map(S, ring M, gens S) ** M;
    debugInfo := if debugLevel < 1 then identity else printerr;
    -- For products of projective space, the dimension is the
    -- number of variables minus the rank of the Picard group
    d := dim X;
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
    (low, high) := (opts.LowerLimit, opts.UpperLimit);
    debugInfo("Computing cohomologyHashTable from ", toString low, " to ", toString high);
    L := pairs cohomologyHashTable(M, low, high);
    --
    gt := new MutableHashTable;
    debugInfo("Beginning search in Picard group");
    -- TODO: rewrite this loop
    HP := x -> (map(QQ, ring H, x))(H);
    apply(L, ell -> (
            -- Check that Hilbert function and Hilbert polynomial match
            -- (this imposes a condition on the alternating sum of local cohomology dimensions)
            if hilbertFunction(ell_0_0, M) != HP(ell_0_0) then gt#(ell_0_0) = true;
            -- Check that higher local cohomology vanishes (i.e., H^i_B(M) = 0 for i > 1)
            if ell_1 != 0 and ell_0_1 > 0 then scan(diagonalMultidegrees(ell_0_1, n), j -> gt#(ell_0_0 + j) = true);
            ));
    -- retrieve the container
    container := opts.cache;
    container.LowerLimit = low;
    container.UpperLimit = high;
    debugInfo("Calculating minimal generators");
    if debugLevel > 0 and n == 2 then plotRegion((i, j) -> not gt#?{i, j}, low, high);
    -- Testing whether a degree is in gt is only conclusive (n:d) above the minimum degree given
    -- to cohomologyHashTable, and findRegion assumes that the region is closed under translation.
    container.Result = findRegion({low + toList(n:d), high}, M, (ell, M) -> not gt#?ell))

-- The default strategy applies to both modules and ideals in a product of projective spaces,
-- but by using hooks we allow better strategies to be added later
addHook((multigradedRegularity, NormalToricVariety, Module), Strategy => "CohomologySearch", multigradedRegularityCohomologySearchStrategy)

-- Faster strategy using LinearTruncations
load "./VirtualResolutions/development.m2"

--------------------------------------------------------------------
--------------------------------------------------------------------
----- Input: (C)=(ChainComplex)
----- Output: A resolution of the tail end of the complex appended
----- to the given complex.
----- Description: This function is not currently being exported,
----- but we hope it will eventually be useful in generating new
----- virtual resolutions. The key is we need a way, like for example
----- module primary decomposition to add irrlevence to a chain
----- complex before we apply resolveTail. (See comment.)
--------------------------------------------------------------------
--------------------------------------------------------------------

--TODO: Finish test
--      Add length limit
resolveTail = method()
resolveTail(ChainComplex) := ChainComplex => C -> (
    N := max support C;
    M := coker syz C.dd_N;
    -- TODO: add some component of the irrelevant ideal to M here.
    T := res M;
    L1 := for i from min C to max support C - 1 list matrix C.dd_(i+1);
    L2 := for i from min T to max support T - 1 list matrix T.dd_(i+1);
    chainComplex(L1 | L2)
    );

--------------------------------------------------------------------
--------------------------------------------------------------------
----- Beginning of the tests and the documentation
--------------------------------------------------------------------
--------------------------------------------------------------------

load "./VirtualResolutions/tests.m2"
beginDocumentation()
load "./VirtualResolutions/doc.m2"

end--

--------------------------------------------------------------------
--------------------------------------------------------------------
----- Beginning of the development section
--------------------------------------------------------------------
--------------------------------------------------------------------

restart
uninstallPackage "VirtualResolutions"
restart
installPackage("VirtualResolutions", FileName => "VirtualResolutions.m2")
restart
needsPackage("VirtualResolutions", FileName => "VirtualResolutions.m2")
elapsedTime check "VirtualResolutions"
viewHelp "VirtualResolutions"
