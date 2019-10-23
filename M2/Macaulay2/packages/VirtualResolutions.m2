---------------------------------------------------------------------------
-- PURPOSE : Construct, display, and study virtual resolutions for products
--           of projective spaces.
--
-- PROGRAMMERS : Ayah Almousa, Christine Berkesch, Juliette Bruce,
-- David Eisenbud, Daniel Erman, Michael Loper, Mahrud Sayrafi,
-- Greg Smith.
--
-- UPDATE HISTORY : created 14 April 2018 at M2@UW;
--                  updated 15 April 2019 at IMA Coding Sprint.
---------------------------------------------------------------------------
newPackage ("VirtualResolutions",
    Version => "1.0",
    Date => "April 14, 2018",
    Headline => "Methods for virtual resolutions on products of projective spaces",
    Authors =>{
        {Name => "Ayah Almousa",       Email => "aka66@cornell.edu",   HomePage => "http://pi.math.cornell.edu/~aalmousa "},
        {Name => "Christine Berkesch", Email => "cberkesc@umn.edu",    HomePage => "http://math.umn.edu/~cberkesc/"},
        {Name => "Juliette Bruce",     Email => "jebruce2@wisc.edu",   HomePage => "https://juliettebruce.github.io"},
        {Name => "David Eisenbud",     Email => "de@msri.org",         HomePage => "http://www.msri.org/~de/"},
        {Name => "Michael Loper",      Email => "loper012@umn.edu",    HomePage => "http://math.umn.edu/~loper012/"},
        {Name => "Mahrud Sayrafi",     Email => "mahrud@umn.edu",      HomePage => "http://math.umn.edu/~mahrud/"}
        },
    PackageExports => {
        "TateOnProducts",
        "NormalToricVarieties",
        "Elimination",
        "SpaceCurves",
        "Depth"
        },
    AuxiliaryFiles => true
    )

export{
    "curveFromP3toP1P2",
    "findGensUpToIrrelevance",
    "isVirtual",
    "virtualOfPair",
    "resolveViaFatPoint",
    "randomRationalCurve",
    "randomMonomialCurve",
    "randomCurveP1P2",
    "multigradedRegularity",
    -- Options
    "Attempt",
    "PreserveDegree",
    "GeneralElements"
    }

--------------------------------------------------------------------
--------------------------------------------------------------------
----- CODE
--------------------------------------------------------------------
--------------------------------------------------------------------


--------------------------------------------------------------------
--------------------------------------------------------------------
----- Input: (I,irr) = (ideal, ideal)
----- Output: saturation of I with respect to irr.
----- Description: This is the fast saturation from Colon.m2. Since
----- Colon.m2 might change at some point we have created this wrap
----- function to easily implement other saturations that might be
----- created. We hope this is eventually removed.
--------------------------------------------------------------------
--------------------------------------------------------------------
load("./VirtualResolutions/Colon.m2")
ourSaturation = (I,irr) -> saturationByElimination(I,irr)


--------------------------------------------------------------------
--------------------------------------------------------------------
--Input: F a free chain complex on Cox (X), alphas a list of degrees
--Output: A subcomplex of summands generated only in degrees in the list alphas.
--Given a ring and its free resolution, keeps only the summands in resolution of specified degrees
--If the list alphas contains only one element, the output will be summands generated in degree less than or equal to alpha.
--See Algorithm 3.4 of [BES]
--------------------------------------------------------------------
--------------------------------------------------------------------
virtualOfPair = method()
virtualOfPair (Ideal,        List) := (I, alphas) -> virtualOfPair(res I, alphas)
virtualOfPair (Module,       List) := (M, alphas) -> virtualOfPair(res M, alphas)
virtualOfPair (ChainComplex, List) := (F, alphas) -> (
    if any(alphas, alpha -> #alpha =!= degreeLength ring F) then error "degree has wrong length";
    L := apply(length F, i -> (
            m := F.dd_(i+1); apply(alphas, alpha -> m = submatrixByDegrees(m, (,alpha), (,alpha))); m));
    chainComplex L
    );


--------------------------------------------------------------------
--------------------------------------------------------------------
----- Input: (J,irr,A)=(Ideal,Ideal,List) where J defines a 0-dim
----- subscheme and irr is the irrelevant ideal
----- Output: A virtual resolution of S/J, which is potentially short.
----- Description: This function implements Theorem 4.1 of [BES].
----- In particular, it computes a virutal resolution of S/J by
----- computing a graded minimal free resolution of S/(J\cap B^A).
----- By the theorem this might be a short virtual resolution
--------------------------------------------------------------------
--------------------------------------------------------------------
resolveViaFatPoint = method()
resolveViaFatPoint(Ideal, Ideal, List) := ChainComplex => (J, irr, A) -> (
    L := decompose irr;
    if #A != #L then error "intersectionRes: expected exponent vector of the right length.";
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
-- Input: Ideal I (or module) - what the virtual resolution resolves
--       Ideal irr - the irrelevant ideal of the ring
--       Chain Complex C - proposed virtual resolution
-- Output: Boolean - true if complex is virtual resolution, false otherwise
-- Note: the Determinatal strategy is based on Theorem 1.3 of [Loper2019].
-- TODO: need to fix for modules; don't know how to saturate for modules
--------------------------------------------------------------------
--------------------------------------------------------------------
isVirtual = method(Options => {Strategy => null})
isVirtual (Ideal, Ideal, ChainComplex) := Boolean => opts -> (I, irr, C) -> (
    annHH0 := ideal(image(C.dd_1));
    Isat := ourSaturation(I,irr);
    annHH0sat := ourSaturation(annHH0,irr);
    if not(Isat == annHH0sat) then (
        if debugLevel >= 1 then print "isVirtual failed at homological degree 0";
        return false;
        );
-- if strategy "determinantal is selected, the method checks virtuality
-- via the depth criterion on the saturated ideals of minors
    if opts.Strategy === "Determinantal" then (
        for i from 1 to length(C) do (
            if rank(source(C.dd_i)) != (rank(C.dd_i) + rank(C.dd_(i+1))) then (
                if debugLevel >= 1 then print "isVirtual failed at homological degree " | toString i;
                return false;
                );
            );
        for i from 1 to length(C) do (
            minor := minors(rank(C.dd_i),C.dd_i);
            minorSat := ourSaturation(minor,irr);
            if depth(minorSat,ring(minorSat)) < i then (
                if debugLevel >= 1 then print "isVirtual failed at homological degree " | toString i;
                return false;
            );
        );
    true
    );
-- default strategy is calculating homology and checking homology is
-- supported on irrelevant ideal
    for i from 1 to length(C) do (
        annHHi := ann HH_i(C);
        if annHHi != ideal(sub(1,ring I)) then (
            if annHHi == 0 or ourSaturation(annHHi,irr) != ideal(sub(1,ring I)) then (
                if debugLevel >= 1 then print "isVirtual failed at homological degree " | toString i;
                return false;
                );
            );
        );
    true
    )

isVirtual (Module, Ideal, ChainComplex) := Boolean => opts -> (M, irr,C) -> (
    annM := ann(M);
    annHH0 := ann(HH_0(C));
    annMsat := ourSaturation(annM,irr);
    annHH0sat := ourSaturation(annHH0,irr);
    if not(annMsat == annHH0sat) then (
        if debugLevel >= 1 then print "isVirtual failed at homological degree 0";
        return false;
        );
-- if strategy "determinantal is selected, the method checks virtuality
-- via the depth criterion on the saturated ideals of minors
    if opts.Strategy === "Determinantal" then (
        for i from 1 to length(C) do (
            if rank(source(C.dd_i)) != (rank(C.dd_i) + rank(C.dd_(i+1))) then (
                if debugLevel >= 1 then print "isVirtual failed at homological degree " | toString i;
                return false;
                );
            );
        for i from 1 to length(C) do (
            minor := minors(rank(C.dd_i),C.dd_i);
            minorSat := ourSaturation(minor,irr);
            if depth(minorSat,ring(minorSat)) < i then (
                if debugLevel >= 1 then print "isVirtual failed at homological degree " | toString i;
                return false;
            );
        );
    true
    );
-- default strategy is calculating homology and checking homology is
-- supported on irrelevant ideal
    for i from 1 to length(C) do (
        annHHi := ann HH_i(C);
        if annHHi != ideal(sub(1,ring M)) then (
            if annHHi == 0 or ourSaturation(annHHi,irr) != ideal(sub(1,ring irr)) then (
                if debugLevel >= 1 then print "isVirtual failed at homological degree " | toString i;
                return false;
                );
            );
        );
    true
    )

isVirtual (Ideal, NormalToricVariety, ChainComplex) := Boolean => opts -> (I, X, C) -> (
    if ring I != ring X then error "ideal is not in Cox ring of normal toric variety";
    isVirtual(I, ideal X, C)
    )

isVirtual (Module, NormalToricVariety, ChainComplex) := Boolean => opts -> (M, X, C) -> (
    if ring M != ring X then error "module is not in Cox ring of normal toric variety";
    isVirtual(M, ideal X, C)
    )

--------------------------------------------------------------------
--------------------------------------------------------------------
-- Input: ZZ n - size of subset of generators to check
--       Ideal J - ideal of ring
--       Ideal irr - irrelevant ideal
-- Output: all subsets of size of the generators of J that give
--         the same ideal as J up to saturation by the irrelevant ideal
--         If the option GeneralElements is set to true, then
--         before outputting the subsets, the ideal generatered by the
--         general elements is outputted
--------------------------------------------------------------------
--------------------------------------------------------------------
findGensUpToIrrelevance = method(Options => {GeneralElements => false})
findGensUpToIrrelevance(ZZ, Ideal, Ideal) := List => opts -> (n, J, irr) -> (
    R := ring(J);
    k := coefficientRing(R);
    Jsat := ourSaturation(J,irr);
    comps := decompose irr;
    if opts.GeneralElements == true then (
        degs := degrees(J);
        -- place of all unique degrees
        allmatches := unique(apply(degs, i -> positions(degs, j -> j == i)));
        -- creates an ideal where if degrees of generators match
        -- those generators are replaced by one generator that
        -- is a random combination of all generators of that degree
        K := ideal(apply(allmatches, i -> sum(apply(i, j -> random(k) * J_j))));
        J = K;
        );
    lists := subsets(numgens(J), n);
    output := {};
    if opts.GeneralElements == true then output = {J};
    apply(lists, l -> (
            I := ideal(J_*_l);
            if ourSaturation(ourSaturation(I, comps_0), comps_1) == Jsat then (
                output = append(output, l);
                );
            )
        );
    output
    )

findGensUpToIrrelevance(ZZ, Ideal, NormalToricVariety) := List => opts -> (n, J, X) -> (
    findGensUpToIrrelevance(n, J, ideal X)
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
randomRationalCurve (ZZ,ZZ,Ring) := (d,e,F)->(
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
randomRationalCurve (ZZ,ZZ) := (d,e)->(
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
randomMonomialCurve (ZZ,ZZ,Ring) := (d,e,F)->(
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
    J' := saturate(J,ideal(uVars#0,uVars#1),MinimalGenerators=>false);
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
randomMonomialCurve (ZZ,ZZ) := (d,e)->(
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
curveFromP3toP1P2 (Ideal) := opts -> (J) ->(
    --- Defines P3
    R := ring J;
    rVars := flatten entries vars R;
    --- Base locus of projection
    BL1 := ideal(rVars#0,rVars#1);
    BL2 := ideal(rVars#1,rVars#2,rVars#3);
    BL := intersect(BL1,BL2);
    --- If PreserveDegree => true checks whether curve intersects base locus;
    --- this ensures the curve has the correct degree and genus.
    if opts.PreserveDegree == true then (
        if (saturate((J+BL1))==ideal(rVars)) or (saturate((J+BL2))==ideal(rVars)) then error "Given curve intersects places of projection.";
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
randomCurveP1P2 (ZZ,ZZ,Ring) := opts -> (d,g,F)->(
    --- Defines P3
    z := getSymbol "z";
    R := F(monoid[z_0,z_1,z_2,z_3]);
    rVars := flatten entries vars R;
    --- Base locus of porjection
    BL1 := ideal(rVars#0,rVars#1);
    BL2 := ideal(rVars#1,rVars#2,rVars#3);
    BL := intersect(BL1,BL2);
    --- Randomly generates curve in P3 until finds one not intersecting
    --- base locus of projection or until Bound is reached.
    C := ideal(0);
    apply(opts.Attempt,i->(
            C = curve(d,g,R);
            if class(C) === Curve then C = ideal(C);
            if (saturate(C+BL1)!=ideal(rVars)) and (saturate(C+BL2)!=ideal(rVars)) then break C;
            )
        );
    --- Checks whether curve in P3 intersects base locus of projection;
    --- this ensures the curve has the correct degree and genus.
    if (saturate(C+BL1)==ideal(rVars)) or (saturate(C+BL2)==ideal(rVars)) then error "Unable to find curve not intersecting places of projection.";
    --- Defines P1xP2
    curveFromP3toP1P2(C)
    )

--------------------------------------------------------------------
--------------------------------------------------------------------
----- Input: (d,e)=(degree,genus)
----- Output: The ideal of a random curve in P1xP2 over ZZ/101
--------------------------------------------------------------------
--------------------------------------------------------------------
randomCurveP1P2 (ZZ,ZZ) := randomCurveP1P2 => opts -> (d,g)->(
    randomCurveP1P2(d,g,ZZ/101)
    )

--------------------------------------------------------------------
--------------------------------------------------------------------
----- Input: S = Cox ring of a product of projective spaces.
-- OR
----- Input: X = normalToricVariety of a product of projective spaces.
----- Output: The dimension vector for the product of projective spaces.
----- Note the dimension is ordered assuming the degree {1,0,...} is first.
--------------------------------------------------------------------
--------------------------------------------------------------------
dimVector = method()
dimVector(Ring) := (S) -> (
    deg := degrees S;
    degTally := tally deg;
    apply(rsort unique deg, i->(degTally_i - 1))
    )

dimVector(Thing) := (X) -> (
    S := ring X;
    deg := degrees S;
    degTally := tally deg;
    apply(rsort unique deg, i->(degTally_i - 1))
    )


-- Helper function for multigradedRegularity
-- borrowed from LinearTruncations:
multigradedPolynomialRing = n -> (
    x := local x;
    xx := flatten apply(#n, i -> apply(n_i+1, j -> x_(i,j)));
    degs := flatten apply(#n, i -> apply(n_i+1, k ->
            apply(#n, j -> if i == j then 1 else 0)));
    ZZ/32003[xx, Degrees=>degs]
    )

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
----- Caveat: This assumes M is B-saturated already i.e. H^1_I(M)=0
--------------------------------------------------------------------
--------------------------------------------------------------------
multigradedRegularity = method()
multigradedRegularity(Thing,              Ideal)  := List => (T, I)  -> multigradedRegularity(T, comodule I)
multigradedRegularity(Ring,               Module) := List => (S, M') -> multigradedRegularity(null, S, M')
multigradedRegularity(NormalToricVariety, Module) := List => (X, M)  -> multigradedRegularity(X, null, M)
-- Note: some hacking is involved to deal with the differences between productOfProjectiveSpaces and toricProjectiveSpaces
multigradedRegularity(Thing, Thing, Module) := List => (X, S, M) -> (
    if class X === NormalToricVariety then (
        -- go from module over NormalToricVariety to module over productOfProjectiveSpaces
        -- assuming that the NormalToricVariety is a tensor product of toricProjectiveSpaces
        S = ring X;
        (S', E') := productOfProjectiveSpaces(dimVector X, CoefficientField => coefficientRing S);
        M' := coker (map(S', S, gens S'))(presentation M);
        ) else (
        -- go from module over productOfProjectiveSpaces to module over tensor product of toricProjectiveSpaces
        (S', E') = productOfProjectiveSpaces(dimVector S, CoefficientField => coefficientRing S);
        M' = coker (map(S', S, gens S'))(presentation M);
        X = fold((A,B) -> A**B, dimVector(S)/(i->toricProjectiveSpace(i, CoefficientRing => coefficientRing S)));
        M = coker (map(ring X, S, gens ring X))(presentation M);
        S = ring X;
        );
    n := #(degrees S)_0;
    r := regularity M;
    H := hilbertPolynomial(X, M);
    -- We only search in the positive cone and up to the regularity of M
    L := pairs cohomologyHashTable(M', toList(n:0), toList(n:r));
    -- Based on findHashTableCorner from TateOnProducts
    P := multigradedPolynomialRing toList(n:0);
    gt := new MutableHashTable;
    apply(L, ell -> (
            -- Check that Hilbert function and Hilbert polynomial match
            -- (this imposes a condition on the alternating sum of local cohomology dimensions)
            if hilbertFunction(ell_0_0, M) != (map(QQ, ring H, ell_0_0))(H) then (
                gt#(ell_0_0) = true;
                );
            -- Check that higher local cohomology vanishes (i.e., H^i_I(M) = 0 for i > 1)
            if ell_1 != 0 and ell_0_1 > 0 then (
                gt#(ell_0_0) = true;
                apply(n, j -> gt#(ell_0_0 + degree P_j) = true);
                );
            )
        );
    low := apply(n, i -> min (L / (ell -> ell_0_0_i - 1)));
    I := ideal apply(L, ell -> if not gt#?(ell_0_0) then product(n, j -> P_j^(ell_0_0_j - low_j)) else 0);
    apply(flatten entries mingens I, g -> (flatten exponents g) + low)
    )

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
resolveTail(ChainComplex) := C ->(
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
----- Begining of the tests and the documentation
--------------------------------------------------------------------
--------------------------------------------------------------------

load ("./VirtualResolutions/tests.m2")
beginDocumentation()
load ("./VirtualResolutions/doc.m2")

end--

--------------------------------------------------------------------
--------------------------------------------------------------------
----- Begining of the development section
--------------------------------------------------------------------
--------------------------------------------------------------------

restart
uninstallPackage "VirtualResolutions"
restart
installPackage "VirtualResolutions"
restart
needsPackage "VirtualResolutions"
elapsedTime check "VirtualResolutions"
viewHelp "VirtualResolutions"
