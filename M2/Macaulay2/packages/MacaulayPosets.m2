newPackage(
    "MacaulayPosets",
    Version => "1.0",
    Date => "July 1, 2025",
    Headline => "Macaulay posets",
        Authors => { {Name => "Penelope Beall", Email => "pbeall@ucdavis.edu", HomePage=>"https://pbeall.github.io"}, {Name => "Yu Olivier Li", Email => "yol1@st-andrews.ac.uk"}  },
    PackageExports => {
        "Posets"
    },
    Keywords => {"Combinatorial Commutative Algebra"}
)
export {
    "PosetMap",
    "isRankPreserving",
    "getMons",
    "getPoset",
    "upperShadow",
    "lowerShadow",
    "macaulayOrders",
    "isMacaulay",
    "isAdditive",
    "posetWedgeProduct",
    "posetFiberProduct",
    "posetClosedProduct",
    "posetConnectedSum",
    "ringFiberProduct",
    "ringConnectedSum",
    "MaxDegree",
    "MaxTime",
    "TikZ",
    "Visual",
    "AllOrders"
}
needsPackage "Posets";
needsPackage "Visualize";

--Some methods use the following variable to avoid taking too long. They first set timeout to several seconds after currentTime(). When currentTime() reaches timeout, macaulayHelper (used by macaulayOrders and isMacaulay) and isAdditive return a String. For isAdditive, time will first be spent in macaulayOrders, and then more time may be spent in the body of isAdditive after macaulayOrders is used.
timeout=0;

--macaulayOrders will use this to keep track of up to what level the poset is Macaulay
macaulayDegree=0;

--------------------------------------------------
--- Poset maps
--------------------------------------------------

PosetMap = new Type of HashTable;
PosetMap.synonym = "map of posets";

source(PosetMap) := Poset => f -> f.source;
target(PosetMap) := Poset => f -> f.target;

map(Poset, Poset, HashTable) := PosetMap => opts -> (Q,P,f) -> (
    -- check that the domain of f is P
    if not set keys f == set vertices P then error "The keys must be vertices of the source Poset.";
    
    -- check that the codomain of f is contained in Q
    if not isSubset(values f, vertices Q) then error "The image must be contained in the codomain.";
    
    g := new PosetMap from join({
        symbol source => P,
        symbol target => Q},
        pairs f
    );
    
    -- check monotonicity
    scan(coveringRelations P, c -> if not compare(Q, g(c#0), g(c#1)) then error "The resulting PosetMap would not be monotone.");
    
    g
);
map(Poset, Poset, List) := PosetMap => opts -> (Q,P,L) -> (
    -- Either everything in L a vertex of Q,
    if instance(scan(L, i -> if not isMember(i, vertices Q) then break false), Nothing) then (
        return( map(Q, P, new HashTable from apply(#P_*, i->P_*#i=>L#i)) );
    );
    -- or everything in L is an Option and not everything in L is a vertex of Q
    if instance(scan(L, i -> if not instance(i, Option) then break false), Nothing) then (
        return map(Q, P, new HashTable from L );
    );
    error("List elements must all be vertices of the target or all be Options.")
);
PosetMap Thing := Thing => (f,p) -> (
    if not f#?p then error "The element is not in the source of the map.";
    
    f#p
);
PosetMap==PosetMap := Boolean => (f,g) -> (
    if not ((source f)==(source g) and (target f)==(target g)) then return false;
    
    instance(scan((vertices source f), i->if not (f(i)==g(i)) then break false), Nothing)
);
PosetMap*PosetMap := PosetMap => (f,g) -> (
    map(target f, source g, apply(vertices source g, p -> f(g(p))))
);
image(PosetMap, List) := List => (f, L) -> (
    unique apply(select(L, l->member(l,vertices source f)), p->f(p))
);
image(PosetMap) := List => (f) -> (
    image(f, vertices source f)
);
isInjective(PosetMap) := Boolean => (f) -> (
    --use finiteness
    #(image f) == #(vertices source f)
); 
isSurjective(PosetMap) := Boolean => (f) -> (
    isSubset(vertices target f, image f)
);
isRankPreserving = method();
isRankPreserving(PosetMap) := Boolean => (f) -> (
    if (not isRanked source f) or (not isRanked target f) then return false;
    
    targetRankFunction := rankFunction target f;
    targetRank := new HashTable from apply(#(vertices target f), i -> (vertices target f)#i => targetRankFunction#i);
    
    sourceRankFunction := rankFunction source f;
    sourceRank := new HashTable from apply(#(vertices source f), i -> (vertices source f)#i => sourceRankFunction#i);
    
    instance(scan(vertices source f, p -> (
        if not (sourceRank#p == targetRank#(f(p))) then break false;
    )), Nothing)
);

--------------------------------------------------
--- Getting posets of polynomial rings with any homogeneous ideal
--------------------------------------------------

-- Suppose s is a monomial in a ring S. This returns what degree s would be if S was replaced by newRing(S, Degrees=>toList((numgens S):1))
theDegree = (s) -> (
    S := ring s;
    
    if s == 0_S then return -infinity;
    
    sum := 0;
    for g in (gens S) do (
        if not (g == 0_S) then (
            sum = sum + degree(g,s);
        );
    );
    sum
)

isHomogeneousWrtVars = (I) -> (
    S := (ring I)/I;
    isHomogeneous newRing(S, Degrees=>toList((numgens S):1))
);

-- First function is to get all the unique monomial representations in the quotient ring. The function super basis is the cornerstone for this. Everything else is just manipulating the data. 

getMons = method(TypicalValue => List, Options => { MaxDegree => 10 } );

getMons(PolynomialRing, Ideal) := options -> (R, I) -> (
    M := {};
    if not isField baseRing R then return "The base ring of the polynomial ring must be a field.";
    if not isHomogeneousWrtVars I then return "The given ring must be homogeneous.";
    try R/I then S := R/I else return "The first input needs to be a polynomial ring R, second input needs to be an ideal of R";
    
    try super basis(S) then ( -- If the quotient ring has a finite number of elements (ie. the super basis returns a finite number of monomials): 
        M = super basis(S);
        M = flatten entries M;
    ) else ( -- If the quotient ring doesn't have a finite number of elements:
        M = new MutableList;
        
        print( "Ideal is not finite over the base (Quotient ring has infinitely many monomials)" );
        print( concatenate("Monomials up to degree ", toString(options.MaxDegree), " given.")  );
        G := gens S; -- Get the generators of the quotient ring.
        minEl := 1;
		
        for i from 0 to (#G - 1) do (
            minEl = minEl * G_i^0;
            ); -- Get the minimal element of the ring. 
        M#(#M) = minEl;
		
        for i from 0 to (options.MaxDegree - 1) do (
            degreeiEls := select( M, a -> theDegree( a ) == i ); -- Choose the elements in M that have degree i.
            for j from 0 to (#degreeiEls - 1) do (
                for k from 0 to (#G - 1) do ( -- Multiply each element in degreeiEls by each generator.
                    subject := degreeiEls#j;
                    newMon := subject * G_k;
                    if ( (any(M, a -> a == newMon ) == false ) and ( theDegree (newMon) >= 0 ) ) then ( -- If the new element is not in M and isn't the idenitity do:
                        M#(#M) = newMon;
                        );
                    );
                );
            );
        M = new List from M;
        );
    M
    );

getMons( Ideal  ) := options -> (I) -> ( -- If the input is an ideal.
    R := ring I;
    if not isPolynomialRing R then return "The given ideal must be an ideal of a polynomial ring.";
    if not isField baseRing R then return "The base ring of the polynomial ring must be a field.";
    getMons(R, I, MaxDegree => options.MaxDegree)
    );

getMons( QuotientRing ) := options -> (S) -> ( -- If the input is a quotient ring.
    R := ambient(S);
    if not isPolynomialRing R then return "The given quotient ring must be the quotient of a polynomial ring R and an ideal of R.";
    if not isField baseRing R then return "The base ring of the polynomial ring must be a field.";
    I := ideal(S);
    getMons(R, I, MaxDegree => options.MaxDegree)
    );

getMons( PolynomialRing ) := options -> (R) -> (
    I := ideal(0_R);
    getMons( R, I, MaxDegree => options.MaxDegree)
    );

getCoverRels = (mons, R, I) -> (
    S := R/I;
    coverRels := new MutableList;
    for i from 0 to (#mons - 1) do (
        subject := mons_i;
        -- For each monomial in the mons list multiply them with another monomial.
        -- Then check if the monomial divides the result and the degree difference is one.  
        for j from 0 to (#mons - 1) do (
            if ( ( abs(   theDegree( mons_j ) - theDegree(subject)  ) == 1 ) and (mons_j % subject == 0)   ) then (
                coverRels#(#coverRels) = {subject, mons_j};
                );
            );
            
        );
    new List from coverRels
    );


getPoset = method(TypicalValue => Poset, Options => {MaxDegree => 10});

getPoset(PolynomialRing, Ideal) := options -> (R, I) -> (    
    Mons := getMons(R, I, MaxDegree => options.MaxDegree); -- Get the monomials
    if instance(Mons, String) then return Mons;
    CoverRels := getCoverRels(Mons, R, I); -- Get the covering Relations
    NewPoset := poset(Mons, CoverRels);
    NewPoset -- New Poset.
    );

getPoset(Ideal) := options -> (I) -> ( -- Given an ideal as an input.
    R := ring I;
        if not isPolynomialRing R then return "The given ideal must be an ideal of a polynomial ring.";
        if not isField baseRing R then return "The base ring of the polynomial ring must be a field.";
    getPoset(R, I, MaxDegree => options.MaxDegree)
    );

getPoset(QuotientRing) := options -> (S) -> ( -- Given a quotient ring as an input.
    R := ambient(S);
    if not isPolynomialRing R then return "The given quotient ring must be the quotient of a polynomial ring R and an ideal of R.";
    if not isField baseRing R then return "The base ring of the polynomial ring must be a field.";
    I := ideal(S);
    getPoset(R, I, MaxDegree => options.MaxDegree)
    );

getPoset(PolynomialRing) := options -> (R) -> (
    I := ideal(0_R);
    getPoset(R, I, MaxDegree => options.MaxDegree)
    );

--------------------------------------------------
--- Finding unique extremal elements of posets
--------------------------------------------------

checkUniqueMin = (poset) -> (
    flag := true;
    if ( #(minimalElements poset) == 1 ) then (
        flag = true; 
        ) else (
        flag = false;
        );
    flag
    );

getUniqueMin = (poset) -> (
    if (  checkUniqueMin(poset) == true ) then (
        el := minimalElements poset
        );
    el
    );

checkUniqueMax = (poset) -> (
    flag := true;
    if ( #(maximalElements poset) == 1 ) then (
        flag = true; 
        ) else (
        flag = false;
        );
    flag
    );

getUniqueMax = (poset) -> (
    if (  checkUniqueMax(poset) == true ) then (
        el := maximalElements poset
        );
    el
    );

--------------------------------------------------
--- Poset Operations
--------------------------------------------------

posetWedgeProduct = Posets -> (
    posets := splice {Posets};
    for i from 0 to (#posets - 1) do (
        if not ( instance(posets_i, Poset)  ) then (
            return concatenate("All elements in the list must be posets, element number ", toString(i+1), " is not a Poset.");
            );
        );
    checkPoset := null;
    for i from 0 to (#posets - 1) do ( -- Checks whether all of the posets have a unique element or not.
        checkPoset = posets_i;
        if checkUniqueMin(checkPoset) == false then (
            return "All posets must have a unique minimal element.";
            );
        );
    
    uniqueMinimalElements := flatten for P in posets list minimalElements(P); -- The minimal element for each poset. 
    noElsList := for P in posets list #(vertices P); -- Number of elements for each poset.
    
    allPosetEls := new MutableList;
    minElIndex := 0;
    
    for i from 0 to (#posets - 1) do (
        -- For the ith poset, we are creating a set with the same number of elements as the number of elements in the ground set.
        
        posetiGroundSet := splice {(i, 1) .. (i, noElsList_i) };
        -- We find the the position of the minimal element in the ith poset ground set.
        minElIndex = position( (posets_i)_*, b -> toString(b) == toString(uniqueMinimalElements_i) );
        -- Then replace the element of the same position in the new set.
        posetiGroundSet = insert(minElIndex, "min", posetiGroundSet  );
        posetiGroundSet = drop(posetiGroundSet, {minElIndex + 1, minElIndex + 1}  );
        allPosetEls#(#allPosetEls) = posetiGroundSet;
        );
    
    wedgeGroundSet := unique flatten new List from allPosetEls;
    subjectCoverRels := null;
    subjectGroundSet := null;
    subjectRelation := null;
    firstPostion := null;
    secondPostion := null;
    
    wedgeCoverRelations := flatten for i from 0 to (#posets - 1) list (
        subjectGroundSet = (posets_i)_*;
        subjectCoverRels = coveringRelations posets_i;
                
        for j from 0 to (#subjectCoverRels - 1) list (
            subjectRelation = subjectCoverRels_j;
            -- We find the positions of the elements in their respective poset ground set.
                        firstPosition := position( subjectGroundSet, x -> toString(x) == toString(subjectRelation_0)  );
            secondPosition := position( subjectGroundSet, x -> toString(x) == toString(subjectRelation_1)  );
                        -- We append the cover relation with the new ground set.
            flatten { (allPosetEls#i)_{firstPosition}, (allPosetEls#i)_{secondPosition}   }
            )
        );
    wedgePoset := poset( wedgeGroundSet, wedgeCoverRelations  );
    wedgePoset
    );


posetFiberProduct = PosetMaps -> (
    posetMaps := splice {PosetMaps};
    
    if 0 == #posetMaps then return "Number of PosetMaps must be larger than 0";
    
    scan(posetMaps, f-> (
        if not ( instance(f, PosetMap) ) then (
            return "All arguments must be PosetMaps.";
            );
        if not isRankPreserving f then (
            return "All PosetMaps must be rank-preserving.";
            );
        if not isInjective f then (
            return "All PosetMaps must be injective.";
            );
        ));
    fiberSource := source posetMaps#0;
    scan(posetMaps, f-> if not source f == fiberSource then return "All PosetMaps must have the same source");
    
    allPosetEls := apply(vertices fiberSource, b->(#posetMaps, b));
    allPosetCovers := {};
    
    scan(#posetMaps, i->(
        -- For the ith map, take its elements not in the image of the map and disjointify them from the other images
        posetiGroundSet := apply(select((target(posetMaps_i))_*, b -> not member(b, image posetMaps_i)), b -> (i, b));
        
        -- Also, turn the old cover relations into cover relations for the new poset
        preimagei := new HashTable from apply((source(posetMaps_i))_*, p -> (posetMaps_i)(p)=>p);
        
        posetiCoverRels := apply(coveringRelations(target(posetMaps_i)), c->apply((0,1), j->if preimagei#?(c#j) then (#posetMaps, preimagei#(c#j)) else (i, c#j)));
        
        allPosetEls = append(allPosetEls, posetiGroundSet);
        allPosetCovers = append(allPosetCovers, posetiCoverRels);
        ));
    
    fiberGroundSet := flatten allPosetEls;
    fiberCoverRelations := unique flatten allPosetCovers;
    
    fiberPoset := poset( fiberGroundSet, fiberCoverRelations );
    fiberPoset
    );

posetConnectedSum = PosetMaps -> (
    posetMaps := splice {PosetMaps};
    
    for i from 0 to (#posetMaps - 1) do (
        if not ( instance(posetMaps_i, PosetMap)  ) then (
            return concatenate("All elements in the list must be poset maps, element number ", toString(i+1), " is not a PosetMap.");
            );
        );
    scan(posetMaps, f-> ( -- Checks whether all of the poset maps are rank-preserving and injective
        if not isRankPreserving f then (
            return "All PosetMaps must be rank-preserving and injective.";
            );
        if not isInjective f then (
            return "All PosetMaps must be injective.";
            );
        ));
    
    -- take disjointified copies of the codomains, except add some glue according to the maps:
    i := 0;
    posets := apply(posetMaps, f -> (
        i = i + 1;
        
        iso := isomorphism(target f, dual target f);
        
        fInverse := hashTable apply(vertices source f, p-> f#p=>p);
        
        return labelPoset(target f, hashTable apply(vertices target f, p->(
            if member(p, image f) then return p=>(0,fInverse#p);
            if member(iso#p, image f) then return p=>(1+#posetMaps,fInverse#(iso#p));
            
            return  p=>(i,p);
        )));
    ));
    
    -- the desired poset is the non-disjoint union of these connected summands:    
    union(posets)    
    );

posetClosedProduct = Posets -> (
    posets := splice {Posets};
        for i from 0 to (#posets - 1) do (
            if not (instance(posets_i, Poset)) then (
                return concatenate("All elements in the list must be posets, element number ", toString(i+1), " is not a Poset.");
                );
            );
        
        
        posetRanks := {};
    checkPoset := null;
    
    for i from 0 to (#posets - 1) do (
        checkPoset = posets_i;
        if (checkUniqueMin(checkPoset) == false or checkUniqueMax(checkPoset) == false ) then (
            return "All posets must have a unique minimal element and a unique maximal element.";
            );
        posetRanks = append(posetRanks, #( rankPoset checkPoset  ) - 1 ); -- Gets the maximum rank of all the given lists.
        );
    posetRanks = unique posetRanks;
    if (#posetRanks > 1) then (
        print ("The given posets do not have the same rank, therefore the resulting poset will not be ranked.");
        );
    
    uniqueMinimalElements := {};
    uniqueMaximalElements := {};
    noElsList := {}; -- Number of elements for each poset.
    for i from 0 to (#posets - 1) do (
        uniqueMinimalElements = append( uniqueMinimalElements, minimalElements(posets_i)  );
        uniqueMaximalElements = append( uniqueMaximalElements, maximalElements(posets_i)  );
        noElsList = append(noElsList, # (posets_i)_* );
        );
    uniqueMinimalElements = flatten uniqueMinimalElements;
    uniqueMaximalElements = flatten uniqueMaximalElements;
    
    
    allPosetEls := {};
    posetiGroundSet := null;
    minElIndex := null;
    maxElIndex := null;
    
    for i from 0 to (#posets - 1) do (        
        posetiGroundSet = splice {(i, 1) .. (i, noElsList_i) };
        -- Replace the element in the minimum element index with a common minimal element m_l.
        minElIndex = position( (posets_i)_*, b -> toString(b) == toString(uniqueMinimalElements_i) );
        posetiGroundSet = insert(minElIndex, "min", posetiGroundSet  );
        posetiGroundSet = drop(posetiGroundSet, {minElIndex + 1, minElIndex + 1}  );
        
        -- Do the same as above but replace the eelmeent in the maximum element index with a common maximal element "max".
        maxElIndex = position( (posets_i)_*, b -> toString(b) == toString(uniqueMaximalElements_i) );
        posetiGroundSet = insert(maxElIndex, "max", posetiGroundSet  );
        posetiGroundSet = drop(posetiGroundSet, {maxElIndex + 1, maxElIndex + 1}  );
        
        allPosetEls = append(allPosetEls, posetiGroundSet);
        );
    
    
    closedGroundSet := unique flatten allPosetEls;
    closedCoverRelations := {};
    subjectGroundSet := null;
    subjectCoverRels := null;
    subjectRelation := null;
    firstPosition := null;
    secondPosition := null;
    
    for i from 0 to (#posets - 1) do (
        subjectGroundSet = (posets_i)_*;
        subjectCoverRels = coveringRelations posets_i;
        
        for j from 0 to (#subjectCoverRels - 1) do (
            subjectRelation = subjectCoverRels_j;
            -- We find the positions of the elmeents in their respective poset ground set.
            firstPosition = position( subjectGroundSet, x -> toString(x) == toString(subjectRelation_0)  );
            secondPosition = position( subjectGroundSet, x -> toString(x) == toString(subjectRelation_1)  );
            -- We append the cover relation with the new ground set.
            closedCoverRelations = append(closedCoverRelations, flatten { (allPosetEls_i)_{firstPosition}, (allPosetEls_i)_{secondPosition}   }   );
            );
        );
    closedPoset := poset( closedGroundSet, closedCoverRelations  );
    closedPoset
    );

--------------------------------------------------
--- Ring Operations
--------------------------------------------------

ringFiberProduct = method(Binary=>true);
ringConnectedSum = method(Binary=>true);

ringProductHelper = (I1, I2, connected) -> (
    R1 := ring I1;
    R2 := ring I2;
    if not ( isPolynomialRing R1 and isPolynomialRing R2 ) then return "Both quotient rings must be the quotient of a polynomial ring R and an ideal of R.";
    if not (  isField (coefficientRing R1) and isField (coefficientRing R2) ) then return "Both quotient rings must be quotients of a polynomial ring with base ring of a field.";
    if not (coefficientRing R1 === coefficientRing R2) then return "The coefficient field of both ambient rings of both quotient rings must be the same.";
    if not ( isHomogeneousWrtVars(I1) and isHomogeneousWrtVars(I2) ) then return "Both ideals of each quotient ring must be homogeneous.";
    
    try R1/I1 then S := R1/I1 else return "Second input must be an ideal of the first polynomial ring.";
    try R2/I2 then T := R2/I2 else return "Fourth input must be an ideal of the second polynomial ring.";
    
    ringTensorProduct := tensor(S, T);
    
    -- The ambient ring of the ring tensor product is the same as what we want for the ring fiber product.
    fiberRing := ambient ringTensorProduct; 
    fiberIdeal := ideal ringTensorProduct;
    extraIdeal := 0_fiberRing;
    extraIdealList := {};
    
    -- Get the two maps into the tensor product.
    -- We assume that the fiberRing gained from the tensor product will be in the correct form with the variables of the 1st ring at the start, and the variables of the 2nd ring at the end.
    R1toTensor := map(fiberRing, R1, take(gens fiberRing, #(gens R1)));
    R2toTensor := map(fiberRing, R2, take(gens fiberRing, -#(gens R2)));
    
    scan(gens R1, g->scan(gens R2, h->(
        extraIdealList = append(extraIdealList, R1toTensor(g) * R2toTensor(h));
    )));
    
    extraIdeal = ideal( extraIdealList );
    fiberIdeal = fiberIdeal + extraIdeal;
    
    if connected then (
        maxs := {0};
        
        scan({I1, I2}, I -> (
            P := getPoset I;
            
            maxP := maximalElements P;
            
            if not (1 == #maxP) then return "The monomial posets of the rings must have unique maximal elements.";
            
            maxs = append(maxs, maxP#0);
        ));
        
        fiberIdeal = fiberIdeal + ideal(R1toTensor(lift(maxs#1,R1)) - R2toTensor(lift(maxs#2,R2)));
    );
    
    fiberRing/fiberIdeal
    );

-- Since Binary=>true, these four methods should return something of the same type as the inputs.
ringFiberProduct(Ideal, Ideal) := (I1, I2) -> (
    ideal ringProductHelper(I1, I2, false)
);
ringConnectedSum(Ideal, Ideal) := (I1, I2) -> (
    ideal ringProductHelper(I1, I2, true)
);

ringFiberProduct(QuotientRing, QuotientRing) := (S, T) -> ( -- If our input is two quotient rings do: 
    ringProductHelper(ideal S, ideal T, false)
);
ringConnectedSum(QuotientRing, QuotientRing) := (S, T) -> (
    ringProductHelper(ideal S, ideal T, true)
);

--------------------------------------------------
--- Shadows
--------------------------------------------------

-- If P is a Poset and S is a subset of its ground set P_*, then let upperShadow(P,S) be the upper shadow of S in P, and similarly for lowerShadow.
shadowHelper = (P,S,p) -> (
    if not isSubset(S,P_*) then (
        return "The given set is not a subset of the given poset";
    );

    shadow := {};

    -- For each covering relation k={k#0,k#1} with k#0 in S, the element k#1 is supposed to be in the upper shadow of S.
    -- For each covering relation k={k#0,k#1} with k#1 in S, the element k#0 is supposed to be in the lower shadow of S.
    for k in coveringRelations(P) do (
        if any(S, s->s===k#p) then (
            shadow = append(shadow, k#(p-1));
        );
    );

    unique shadow
);
upperShadow = method();
lowerShadow = method();
upperShadow (Poset, List) := List => (P,S) -> shadowHelper(P,S,0);
lowerShadow (Poset, List) := List => (P,S) -> shadowHelper(P,S,1);

-- This returns the initial segment of size s in level d of the ranked poset P with respect to the order O.
-- O could be a list of elements of P, but it actually only needs to be a list containing the initial segment.
initialSegment = method();
initialSegment (Poset, List, ZZ, ZZ) := List => (P,O,d,s) -> (
    if not O == unique O then (
        return "The given order must not have duplicate entries.";
    );
    if (s>0 and #O==0) then (
        return "The order must contain the desired segment.";
    );
    
    dLvl := (rankPoset P)#d;
    
    if (0 > s or s > #dLvl) then (
        return "The size of the segment must be inclusively between 0 and the size of the dth level.";
    );

    segment := {};
    j := #O-1;
    for i from 0 to s-1 do (
        while not member(O#j, dLvl) do (
            j = j-1;
            if j < 0 then (
                return "The order must contain the desired segment.";
            );
        );
        segment = append(segment, O#j);
        j = j-1;
    );
    segment
);
finalSegment = method();
finalSegment (Poset, List, ZZ, ZZ) := List => (P,O,d,s) -> initialSegment(P,reverse O,d,s);

--------------------------------------------------
--- Macaulay orders
--------------------------------------------------

-- Suppose P is a ranked poset, and its subposet inclusively consisting of the levels from 0 to d is Macaulay with respect to the total order O.
-- The integer d is supposed to be inclusively between -1 and the rank of P.
-- Then, this returns all extensions of O with respect to which P is Macaulay, modulo relations between elements of different ranks.
-- But, if not returnAllOrders, then this instead returns only a list with at most one order.
macaulayHelper = (P,O,d,returnAllOrders) -> (
    macaulayDegree = max{macaulayDegree, d};
    if timeout <= currentTime() then return "timeout";
    
    levels := rankPoset P;
    d1orders := {{}}; -- This will be the subset of permutations(levels#(d+1)) consisting of all total orderings o of level d+1 such that the upper shadow of an initial segment in level d wrt O is an initial segment of level d+1 wrt o.
    
    if d+1 >= #levels then (
        -- If d is the rank of P, then it is already Macaulay with respect to O.
        return {O};
    ) else if d==-1 then (
        -- For level 0, there is no lower layer putting constraints on d1orders.
        d1orders = permutations(levels#(d+1));
    ) else (
        shadows := {{}};
        for s from 1 to #(levels#d) do (
            -- Let shadows#s be the upper shadow of the initial segment of size s in level d.
            shadows = append(shadows, upperShadow(P,initialSegment(P,O,d,s)));
        );
        -- There might also be elements of levels#(d+1) which are not in the upper shadow of levels#d
        shadows = append(shadows, levels#(d+1));
        
        for s from 1 to 1+#(levels#d) do (
            -- Suppose d1orders is the list of possible orderings of shadows#(s-1). Then, replace each ordering with its possible extensions to an ordering of shadows#s.
            extendedOrders := {};
            for o in d1orders do (
                for p in permutations(shadows#s - set shadows#(s-1)) do (
                    if timeout <= currentTime() then return "timeout";
                    
                    oPossible := true;
                    
                    -- The initial segment of size s in level d should have an upper shadow no larger than the upper shadow of any other subset of size s in level d.
                    for A in subsets(levels#d, s) do (
                        if #upperShadow(P,A) < #(shadows#s) then (
                            oPossible = false;
                            break;
                        );
                    );
                    
                    if oPossible then extendedOrders = append(extendedOrders, join(p, o));
                );
            );
            d1orders = extendedOrders;
        );
    );
    
    orders := {}; -- This will be the list of all orders with respect to which P is Macaulay, modulo relations between different levels. Or, if not returnAllOrders, this will have at most one order.
    for o in d1orders do (
        newOrders := macaulayHelper(P,join(O,o),d+1,returnAllOrders);
        
        if instance(newOrders, String) then return newOrders;
        
        orders = join(orders, newOrders);
        if (not returnAllOrders) and (not orders=={}) then (
            -- If we now have the desired number of orders, then we can stop searching.
            break;
        );
    );
    
    orders
);

macaulayOrders = method(TypicalValue => List, Options => {MaxTime => 600, TikZ => false, Visual=>false, AllOrders => true});
-- Modulo relations between elements of different rank, this returns the list of orders with respect to which P is Macaulay. But, if not returnAllOrders, this returns an order with respect to which P is Macaulay.
macaulayOrders (Poset) := options -> (P) -> (
    timeAllowed := false;
    for timeType in {ZZ,QQ,RR,InfiniteNumber} do (
        timeAllowed = timeAllowed or instance(options.MaxTime, timeType);
    );
    if not timeAllowed then return "The option MaxTime must be an integer, a rational, a real, or an infinite number.";
    if not instance(options.TikZ, Boolean) then return "The option TikZ must be a Boolean.";
    if not instance(options.AllOrders, Boolean) then return "The option AllOrders must be a Boolean.";
    if not isRanked(P) then return "The poset must be ranked.";
    
    timeout = currentTime() + options.MaxTime;
    macaulayDegree = 0;
    
    orders := macaulayHelper(P, {}, -1, options.AllOrders);
    
    if instance(orders, String) and orders=="timeout" then return concatenate("The maximum time has elapsed, but the subposet consisting of levels 0 to ", toString macaulayDegree, " is Macaulay.");
    
    if instance(orders, String) then return orders;
    
    if options.TikZ then (
        levels := rankPoset P;
        
        for order in orders do ( -- print a TikZ picture for each order
            TikzPicture := "\\begin{tikzpicture}\n";
            
            -- draw vertices
            for i from 0 to (#levels)-1 do (
                level := levels#i;
                j := -1;
                for k from 0 to (#level)-1 do (
                    -- find the next element of level wrt order
                    j = j + 1;
                    while not member(order#j, level) do j = j + 1;
                    
                    -- insert a node for this element
                    TikzPicture = concatenate(TikzPicture, "\tdraw \\node at (", toString(k - (#level-1)/2), ", ", toString(i), ") (", toString(j), ") {", tex order#j, "};\n");
                );
            );
            
            -- draw cover relations
            elementToInt := new MutableHashTable; -- As maps, the inverse of order
            for i from 0 to (#order)-1 do (
                elementToInt#(order#i) = i;
            );
            for cover in coveringRelations P do (
                TikzPicture = concatenate(TikzPicture, "\n\t\\draw (", toString elementToInt#(cover#0), ") -- (", toString elementToInt#(cover#1), ");");
            );
            
            print concatenate(TikzPicture, "\n\\end{tikzpicture}");
        );
    );
    
    if instance(options.Visual, String) or (instance(options.Visual, Boolean) and options.Visual) then (
        openPort if instance(options.Visual, String) then options.Visual else "8080";
        
        visualize P;
        
        -- visualize P with its Macaulay orders
        for order in orders do (
            elementToInt := new MutableHashTable; -- As maps, the inverse of order
            for i from 0 to (#order)-1 do (
                elementToInt#(order#i) = i;
            );
            
            visualize poset(apply(P_*, p -> elementToInt#p), apply(coveringRelations P, c -> {elementToInt#(c#0), elementToInt#(c#1)}));
        );
        
        closePort();
    );
    
    orders
);
macaulayOrders (QuotientRing) := options -> (S) -> (
    R := ambient S;
    I := ideal S;
    
    if not isPolynomialRing R then return "The ambient ring must be a polynomial ring.";
    if not isField baseRing R then return "The base ring of the polynomial ring must be a field.";
    if not isHomogeneousWrtVars I then return "The ring must be homogenous.";
    -- The ideal also should be level linearly independent
    
    macaulayOrders(getPoset(R,I), MaxTime => options.MaxTime, TikZ => options.TikZ, Visual => options.Visual, AllOrders => options.AllOrders)
);
macaulayOrders (Ideal) := options -> (I) -> (
    macaulayOrders((ring I)/I, MaxTime => options.MaxTime, TikZ => options.TikZ, Visual => options.Visual, AllOrders => options.AllOrders)
);

isMacaulay = method(TypicalValue => Boolean, Options => {MaxTime => 600, TikZ => false, Visual=>false});
isMacaulay (Poset) := options -> (P) -> (
    result := macaulayOrders(P, MaxTime => options.MaxTime, TikZ => options.TikZ, Visual => options.Visual, AllOrders => false);
    
    if instance(result, String) then return result;
    
    not result=={}
);
isMacaulay (QuotientRing) := options -> (S) -> (
    result := macaulayOrders(S, MaxTime => options.MaxTime, TikZ => options.TikZ, Visual => options.Visual, AllOrders => false);
    
    if instance(result, String) then return result;
    
    not result=={}
);
isMacaulay(Ideal) := options -> (I) -> (
    isMacaulay((ring I)/I, MaxTime => options.MaxTime, TikZ => options.TikZ, Visual => options.Visual)
);

--------------------------------------------------
--- Additivity
--------------------------------------------------

isAdditive = method(TypicalValue => Boolean, Options => {MaxTime => 600});
isAdditive (Poset) := options -> (P) -> (
    orders := macaulayOrders(P, MaxTime=>options.MaxTime);
    if instance(orders, String) then return orders;
    if 0 == #orders then return "The poset must be Macaulay.";
    
    levels := rankPoset P;
    
    -- check whether P is additive with respect to some Macaulay order
    for order in orders do (
        maybeAdditive := true;
        
        levelMin := -1;
        levelMax := -1;
        
        for rank from 0 to (#levels)-1 do (
            if timeout <= currentTime() then return "The maximum time has elapsed.";
            
            levelMin = levelMax+1;
            while (not levelMax+1 > #order) and (rank+1 >= #levels or not member(order#(levelMax+1), levels#(rank+1))) do levelMax = levelMax + 1;
            level := take(order, {levelMin, levelMax});
            
            shadows := new MutableHashTable;
            shadows#{} = upperShadow(P, {});
            
            -- compute shadows of nonempty segments
            for i from 0 to #level-1 do (
                for j from i to #level-1 do (
                    segment := take(level, {i,j});
                    shadows#segment = upperShadow(P, segment);
                );
            );
            
            -- check definition of Additive for this level
            for i from 0 to #level-1 do (
                for j from i to #level-1 do (
                    initialNewShadow := shadows#(take(level, {#level-(j-i+1), #level-1}));
                    newShadow := shadows#(take(level, {i,j})) - set shadows#(take(level, {j+1,#level-1}));
                    finalNewShadow := shadows#(take(level, {0, j-i})) - set shadows#(take(level, {j-i+1,#level-1}));
                    
                    maybeAdditive = maybeAdditive and #initialNewShadow >= #newShadow and #newShadow >= #finalNewShadow;
                    
                    if not maybeAdditive then break;
                );
                if not maybeAdditive then break;
            );
            if not maybeAdditive then break;
        );
        
        if maybeAdditive then return true;
    );
    
    false
);

--------------------------------------------------
--- Documentation
--------------------------------------------------

beginDocumentation();

doc ///

Node
    Key
        MacaulayPosets
    Headline
        a package for working with Macaulay posets and Macaulay rings
    Description
        Text
            This package contains a few methods for working with posets, rings, and the Macaulay property.
            
            Two boolean methods for ranked posets are implemented.
        Text
            @UL{{TO "isAdditive"},
            {TO "isMacaulay", ", also applicable to rings"}}@
        Text            
            Four poset operations and two ring operations are implemented.
        Text
            @UL{{TO "posetWedgeProduct", ", analogous to ", TO "ringFiberProduct"},
            {TO "posetClosedProduct", ", analogous to ", TO "ringConnectedSum"},
            {TO "posetFiberProduct"},
            {TO "posetConnectedSum"}}@
        Text
            To define the latter two poset operations, a new data type @TO "PosetMap"@ was created.
            
            To obtain the monomial poset of a ring, one can use @TO "getPoset"@, a generalization of @TO "standardMonomialPoset"@ from the @TO2(Posets, "Posets package")@ to ideals which are not necessarily monomial.
            
            This package was used in some of the work leading to the paper @HREF{"https://arxiv.org/abs/2502.15166"}@, where many of these operations and properties are discussed.
Node
    Key
        getMons
        (getMons, PolynomialRing, Ideal)
        (getMons, Ideal)
        (getMons, QuotientRing)
        (getMons, PolynomialRing)
    Headline
        calculates the monomials of a ring
    Usage
        getMons(R, I)
        getMons(I)
        getMons(S)
        getMons(R)
    Inputs
        R:PolynomialRing
        I:Ideal
            a homogeneous ideal of a polynomial ring
        S:QuotientRing
            a quotient of a polynomial ring R and a homogeneous ideal I of R
        MaxDegree=>Number
    Outputs
        :List
    Description
        Text
            Given a polynomial ring R and a homogeneous ideal of R (an ideal generated by homogeneous elements of R) called I, the function getMons gets the monomials of the quotient ring R/I.
        Example
            getMons( QQ[x, y], ideal(x^3, y^3) )
            getMons( QQ[x, y]/ideal( x*y - y^2, x^4, x^3 * y, x^2 * y^2  ) )
        Text
            For quotient rings with infinitely many monomials, the optional argument MaxDegree can be used to calculate monomials up to a certain degree. If this is left blank, the default value used will be 10. 
        Example
            getMons( QQ[x,y]/ideal(x^2 - y^2), MaxDegree=>10 )
    Caveat
        The optional argument MaxDegree is only meant to be used for quotient rings where there are infinitely many monomials. For quotient rings with finitely many monomials, MaxDegree will be ignored regardless of it's value.
    SeeAlso
        getPoset
Node
    Key 
        getPoset
            (getPoset, PolynomialRing, Ideal)
            (getPoset, Ideal)
            (getPoset, QuotientRing)
            (getPoset, PolynomialRing)
    Headline
        calculates the monomial poset of a ring
    Usage
         getPoset(R, I)
         getPoset(I)
         getPoset(S)
         getPoset(R)
    Inputs
        R:PolynomialRing
        I:Ideal
            a homogeneous ideal of a polynomial ring
        S:QuotientRing
            a quotient of a polynomial ring R and a homogeneous ideal I of R
                MaxDegree=>Number
    Outputs
        :Poset
    Description
        Text
            Given a polynomial ring R and a homogeneous ideal of R (an ideal generated by homogeneous elements of R) called I, the monomial poset of R/I  is the collection of @TO2(getMons, "monomials of R/I")@ equipped with the divisibility partial order.
        Example
            getPoset( QQ[x, y], ideal(x*y - y^2, x^4, x^3*y, x^2*y^2) ) 
        Text
            For posets with infinitely many monomials, the optional argument MaxDegree can be used to calculate monomials up to a certain degree to use in the poset. If this is left blank, the default value used will be 10.
        Example
            getPoset( QQ[x, y], ideal(x^2 - y^2), MaxDegree => 15 )    
    Caveat
        Like the function getMons, the optional argument MaxDegree is only meant to be used for quotient rings where there are infinitely many monomials. For quotient rings with finitely many monomials, MaxDegree will be ignored regardless of it's value.
    SeeAlso
        getMons
Node
    Key
        PosetMap
    Headline
        the class of all morphisms between posets
    Description
        Text
            Poset maps are used to obtain @TO2(posetFiberProduct, "fiber products")@ and @TO2(posetConnectedSum, "connected sums")@ of @TO2(Poset, "posets")@.
    SeeAlso
        posetFiberProduct
        posetConnectedSum
Node
    Key
        (source, PosetMap)
    Usage
        source f
    Inputs
        f:PosetMap
    Outputs
        :Poset
    SeeAlso
        PosetMap
        (target, PosetMap)
Node
    Key
        (target, PosetMap)
    Usage
        target f
    Inputs
        f:PosetMap
    Outputs
        :Poset
    SeeAlso
        PosetMap
        (source, PosetMap)
Node
    Key
        (image, PosetMap, List)
        (image, PosetMap)
    Headline
        the image of a poset map
    Usage
        image(f,L)
    Inputs
        f:PosetMap
        L:List
            a subset of @TT "vertices source f"@
    Outputs
        :List
    Description
        Text
            The image of the map $[3]\rightarrow[10]$ given by $i\mapsto 2i$ is computed below.
        Example
            f = map(chain 10, chain 3, {1=>2,2=>4,3=>6})
            image f
        Text
            The image of ${1,2}$ under this map is computed below.
        Example
            image(f, {1,2})
Node
    Key
        (isInjective, PosetMap)
    Usage
        isInjective f
    Inputs
        f:PosetMap
    Outputs
        :Boolean
    SeeAlso
        (isSurjective, PosetMap)
        isRankPreserving
Node
    Key
        (isSurjective, PosetMap)
    Usage
        isSurjective f
    Inputs
        f:PosetMap
    Outputs
        :Boolean
    SeeAlso
        (isInjective, PosetMap)
        isRankPreserving
Node
    Key
        isRankPreserving
        (isRankPreserving, PosetMap)
    Headline
        whether a poset map is rank-preserving
    Usage
        isRankPreserving f
    Inputs
        f:PosetMap
    Outputs
        :Boolean
    Description
        Text
            A @TO2(PosetMap, "poset map")@ $f: P\rightarrow Q$ between @TO2(isRanked, "ranked")@ posets is rank-preserving if and only if, for each $p\in P$, the rank of $p$ is equal to the rank of $f(p)$, assuming both $P$ and $Q$ have a rank $0$ element.
        Example
            isRankPreserving map(chain 2, chain 1, {1})
            isRankPreserving map(chain 2, chain 1, {2})
    SeeAlso
        (isInjective, PosetMap)
        (isSurjective, PosetMap)
Node
    Key
        (symbol ==, PosetMap, PosetMap)
    Headline
        whether two poset maps are equal
    Usage
        P==Q
    Inputs
        P:PosetMap
        Q:PosetMap
    Outputs
        :Boolean
Node
    Key
        (symbol SPACE, PosetMap, Thing)
    Headline
        applies a poset map to a poset element
    Usage
        P p
    Inputs
        P:PosetMap
        p:Thing
    Outputs
        :Thing
    Description
        Example
            f = map(product(chain 2, chain 2), chain 2, {{1,1},{1,2}})
            f 1
            f 2
Node
    Key
        posetWedgeProduct
    Headline 
        constructs the wedge product of several posets
    Usage
        posetWedgeProduct(L)
    Inputs
        L:Sequence
            a sequence of @TO2(Poset, "posets")@
    Outputs
        :Poset
    Description
        Text
            Given a list L of posets all with unique minimal elements, the function returns the wedge product of all the posets. The wedge product is defined as follows: 
            Suppose that for $1\leq i\leq t$ we have posets $P_i$ with unique least element $\ell_i$. Their $\bf{wedge product}$ is the set:
            \[ P_1 \vee P_2 \vee \cdots \vee  P_t = \left(\bigsqcup_{i=1}^t P_i \right)/ (\ell_1=\ell_2=\cdots  =\ell_t), \](meaning that we take the disjoint union of the sets $P_i$ in which we identify all the $\ell_i$ into one element) with the partial order $a\leq b$ if and only if $a\leq b$ in $P_i$ for some $i$.
            
            This is the special case of @TO "posetFiberProduct"@ where the domain of the map is a $1$-vertex poset.
        Example
            posetWedgeProduct( booleanLattice(3), chain(4), getPoset(QQ[x, y]/ideal(x^3, y^2)) )
    SeeAlso
        ringFiberProduct
        posetFiberProduct
        posetClosedProduct
Node
    Key
        posetFiberProduct
    Headline
        constructs the fiber product of several posets
    Usage
        posetFiberProduct(L)
    Inputs
        L:Sequence
            a sequence of rank-preserving injective @TO PosetMap@s.
    Outputs
        :Poset
    Description
        Text
            In this construction, several posets are glued together along a common subposet. Suppose $(A, \leq_A), (B, \leq_B), (C,\leq C)$ are posets and $\iota_A: C\rightarrow A$ and $\iota_B: C\rightarrow B$ are rank-preserving, injective, and monotone. Then, we have a fiber product \[A\times_C B = (A\setminus\iota_A(C))\sqcup(B\setminus\iota_B(C))\sqcup C\] with $a\geq c$ iff $a\geq_A \iota_A(c)$ for $a\in A$ and $c\in C$, and $b\geq c$ iff $b\geq_B \iota_A(c)$ for $b\in A$ and $c\in C$.
            
            This is a generalization of @TO "posetWedgeProduct"@.
        Example
            P = product(chain 2, chain 2)
            Q = product(chain 2, chain 3)
            f = map(Q, P, {{1,1}, {1,2}, {2,1}, {2,2}})
            g = map(Q, P, {{1,1}, {2,1}, {1,2}, {2,2}})
            areIsomorphic( posetFiberProduct(f, g), getPoset(ZZ/2[x,y]/monomialIdeal(x^3,x^2*y^2,y^3)) )
            areIsomorphic( posetFiberProduct(f, f), product(chain 2, adjoinMin posetWedgeProduct(chain 2, chain 2)) )
    SeeAlso
        posetWedgeProduct
        posetConnectedSum
Node
    Key
        posetClosedProduct
    Headline 
        constructs the closed product of several posets
    Usage
        posetClosedProduct(L)
    Inputs
        L:Sequence
            a sequence of posets without the brackets (see example)
    Outputs
        :Poset
    Description
        Text
            Given a list of posets that all have a unique minimal and maximal element, the function returns the closed product of all the posets. The closed product is defined as follows:
            Suppose that for $1\leq i\leq t$ we have posets $P_i$ with unique least element $\ell_i$ and unique largest element $L_i$. Their $\bf{closed product}$ is the set:
            \[ P_1 \diamond P_2 \diamond \cdots \diamond  P_t = \left(\bigsqcup_{i=1}^t P_i \right)/ (\ell_1=\ell_2=\cdots \ell_t, L_1=L_2=\cdots=L_t ), \] (meaning that we take the disjoint union of the sets $P_i$ in which we identify all the $\ell_i$ into one element and all the $L_i$ elements into one element) with the partial order $a\leq b$ if and only if $a\leq b$ in $\P_i$ for some $i$.
            
            This is the special case of @TO "posetConnectedSum"@ where the domain of the map is a $1$-vertex poset.
        Example
            posetClosedProduct(  chain(3), getPoset(QQ[x, y]/ideal(x^2, y^2))   )
    SeeAlso
        ringConnectedSum
        posetConnectedSum
        posetWedgeProduct
Node
    Key
        posetConnectedSum
    Headline
        constructs the connected sum of several posets
    Usage
        posetConnectedSum(L)
    Inputs
        L:Sequence
            of rank-preserving injective @TO PosetMap@s.
    Outputs
        :Poset
    Description
        Text
            In this construction, several posets are glued together at the top and at the bottom. Suppose $A,B$ are self-dual posets, $C$ is a poset, and $i_A: C\rightarrow A$ and $i_B: C\rightarrow B$ are rank-preserving, injective @TO2(PosetMap, "poset maps")@. Let $d_A: A\rightarrow A^{\operatorname{op}}$ and $d_B: B\rightarrow B^{\operatorname{op}}$ be the dual isomorphisms. The connected sum of $A$ and $B$ with respect to $i_A, i_B$ is the poset obtained by taking the disjoint union of $A$ and $B$, identifying the image of $i_A$ with the image of $i_B$, and identifying the image of $d_A\circ i_A$ with the image of $d_B\circ i_B$. This construction can also be generalized to connected sums of more than two posets.
            
            This is a generalization of @TO "posetClosedProduct"@.
            
            Here is the connected sum of the chain $[5]$ with itself with respect to the inclusion map $[2]\hookrightarrow[5]$ with $1\mapsto 1$ and $2\mapsto 2$.
        Example
            i = map(chain 5, chain 2, {1=>1, 2=>2})
            A = posetConnectedSum(i,i)
            areIsomorphic( A, adjoinMin adjoinMax product(chain 2, chain 2) )
        Text    
            Let $P = [2]\times[2]$, let $Q = [2]\times[5]$. There are two different connected sums over $P$ of $Q$ with itself. The map $f: P\rightarrow Q$ is the inclusion map with $(1,2)\mapsto(1,2)$ and $(2,1)\mapsto(2,1)$. The map $g: P\rightarrow Q$ is the other rank-preserving injection, with $(1,2)\mapsto(2,1)$ and $(2,1)\mapsto(1,2)$.
        Example
            P = product(chain 2, chain 2)
            Q = product(chain 2, chain 5)
            
            f = map(Q, P, {{1,1}, {1,2}, {2,1}, {2,2}})
            g = map(Q, P, {{1,1}, {2,1}, {1,2}, {2,2}})
            
            R = posetConnectedSum(f,g)
            S = posetConnectedSum(f,f)
            
            areIsomorphic(R, S)
            
            areIsomorphic(S, product(chain 2, A))
Node
    Key
        ringFiberProduct
        (ringFiberProduct, Ideal, Ideal)
        (ringFiberProduct, QuotientRing, QuotientRing)
        (ringFiberProduct, List)
        (ringFiberProduct, Sequence)
    Headline
        constructs the fiber product of several rings
    Usage
        ringFiberProduct(I, J, ...)
        ringFiberProduct(S, T, ...)
    Inputs
        I:Ideal
            a homogeneous ideal of a @TO PolynomialRing@.
        J:Ideal
            a homogeneous ideal of a @TO PolynomialRing@.
        S:QuotientRing
            a quotient of a polynomial ring by a homogeneous ideal
        T:QuotientRing
            a quotient of a polynomial ring by a homogeneous ideal
    Outputs
        :Ideal
        :QuotientRing
    Description
        Text
            Suppose we have rings $S_1 = R_1/I_1$ and $S_2 = R_2/I_2$ for some homogeneous ideals $I_1$ of $R_1 = K[x_1, ..., x_n]$ and $I_2$ of $R_2 = K[y_1, ..., y_m]$, where $K$ is a field. Their fiber product over $K$ is the ring: 
            \[ S_1 \times_K S_2 = K[x_1, ..., x_n, y_1, ..., y_m]/(I_1 + I_2 + (x_i y_j : 1 \leq i \leq n, 1 \leq j \leq m ))  \]
        Example
            ringFiberProduct( QQ[x, y]/ideal(x^2, y^2), QQ[a, b]/ideal(a^4, b^4)  )
            ringFiberProduct( ZZ/7[x, y, z]/ideal(x^2, y^3, z^4), ZZ/7[a, b, c]/ideal(a, b^3, c^3) )
    SeeAlso
        posetWedgeProduct
        ringConnectedSum
Node
    Key
        ringConnectedSum
        (ringConnectedSum, Ideal, Ideal)
        (ringConnectedSum, QuotientRing, QuotientRing)
        (ringConnectedSum, List)
        (ringConnectedSum, Sequence)
    Headline
        constructs the connected sum of several rings
    Usage
        ringConnectedSum(I, J, ...)
        ringConnectedSum(S, T, ...)
    Inputs
        I:Ideal
            a homogeneous ideal of a @TO PolynomialRing@.
        J:Ideal
            a homogeneous ideal of a @TO PolynomialRing@.
        S:QuotientRing
            a quotient of a polynomial ring by a homogeneous ideal
        T:QuotientRing
            a quotient of a polynomial ring by a homogeneous ideal
    Outputs
        :Ideal
        :QuotientRing
    Description
        Text
            Suppose we have Gorenstein rings $S_1 = R_1/I_1$ and $S_2 = R_2/I_2$ for some homogeneous ideals $I_1$ of $R_1 = K[x_1, ..., x_n]$ and $I_2$ of $R_2 = K[y_1, ..., y_m]$, where $K$ is a field. Let $m_1, m_2$ be the maximal elements of the @TO2(getMons, "monomial posets")@ of $S_1, S_2$, respectively. The connected sum of $S_1$ and $S_2$ is their @TO2(ringFiberProduct, "fiber product")@ mod $m_1-m_2$. In symbols, $S_1\#S_2 = \frac{S_1\times_K S_2}{(m_1-m_2)}$.
        Example
            S = ZZ/2[u,v,w]/(u^4, v^2-w^2, w^2-v*u)
            T = newRing(S, Variables=>{x,y,z})
            U = ringConnectedSum( S, T )
            
            ideal U
            
            isMacaulay S
            
            isMacaulay U
    SeeAlso
        posetClosedProduct
        ringFiberProduct
Node
    Key
        (map, Poset, Poset, List)
        (map, Poset, Poset, HashTable)
    Headline
        make a poset map
    Usage
        map(Q,P,L)
    Inputs
        P:Poset
            the source poset
        Q:Poset
            the target poset
        L:List
            of length @TT "#(vertices P)"@ and whose $i$th entry specifies the image of the $i$th element of @TT "P"@, or a @TO "List"@ of @TO "Option"@s whose keys are @TT "vertices P"@ and whose values are in @TT "vertices Q"@, or a @TO "HashTable"@ whose keys are @TT "vertices P"@ and whose values are in @TT "vertices Q"@
        Degree=>
            unused
        DegreeLift=>
            unused
        DegreeMap=>
            unused
    Outputs
        :PosetMap
            the poset map from @TT "P"@ to @TT "Q"@ specificed by @TT "L"@.
    Description
        Text
            A poset map is a monotone function $f: P\rightarrow Q$ from a poset $P$ to a poset $Q$, so $f(p)\leq f(q)$ whenever $p\leq q$.
            
            Let $P = [2]$ and $Q = [3]\times[3]$. Here are the three ways to obtain the map $\phi: P\rightarrow Q$ given by $\phi(1)=(1,1)$ and $\phi(2)=(1,2)$.
        Example
            P = chain 2
            Q = product(chain 3, chain 3)
            
            f = map(Q, P, {{1,1}, {1,2}})
            g = map(Q, P, {1=>{1,1}, 2=>{1,2}})
            h = map(Q, P, new HashTable from {1=>{1,1}, 2=>{1,2}})
            
            f == g
            g == h
        Text
            Potentially, some of the vertices of the target may be @TO2(Option, "options")@. When there is ambiguity, the list elements will be taken as vertices of the target as in the first of the three ways above.
        Example
            P = poset({1}, {})
            Q = poset({1, 1=>1}, {{1, 1=>1}})
            f = map(Q, P, {1=>1})
            f 1
Node
    Key
        (symbol *, PosetMap, PosetMap)
    Headline
        composition of poset maps
    Usage
        g * f
    Inputs
        f : PosetMap
        g : PosetMap
    Outputs
        : PosetMap
            the composition $g\circ f$
Node
    Key
        upperShadow
        (upperShadow, Poset, List)
    Headline
        the upper shadow of a subset of a poset
    Usage
        upperShadow(P,S)
    Inputs
        P:Poset
        S:List
            a subset of the ground set of the poset
    Outputs
        :List
    Description
        Text
            If $S$ is a subset of a poset, then the upper shadow of $S$ is the set of all elements of the poset which cover some element of $S$.
        Example
            upperShadow(divisorPoset 12, {2,3})
            
            S = QQ[x,y]/(x^4, y^4)
            x = S_0
            y = S_1
            
            upperShadow(getPoset S, {x^3, x^2*y})
    SeeAlso
        lowerShadow
Node
    Key
        lowerShadow
        (lowerShadow, Poset, List)
    Headline
        the lower shadow of a subset of a poset
    Usage
        lowerShadow(P,S)
    Inputs
        P:Poset
        S:List
            a subset of the ground set of the poset
    Outputs
        :List
    Description
        Text
            If $S$ is a subset of a poset, then the lower shadow of $S$ is the set of all elements of the poset which cover some element of $S$.
        Example
            lowerShadow(divisorPoset 12, {12})
            
            S = QQ[x,y]/(x^4, y^4)
            x = S_0
            y = S_1
            
            lowerShadow(getPoset S, {x^3, x^2*y})
    SeeAlso
        upperShadow
Node
    Key
        macaulayOrders
        (macaulayOrders, Poset)
        (macaulayOrders, QuotientRing)
        (macaulayOrders, Ideal)
    Headline
        finds all orders with respect to which the poset is Macaulay
    Usage
        macaulayOrders P
        macaulayOrders S
        macaulayOrders I
    Inputs
        P:Poset
        S:QuotientRing 
            a homogeneous level linearly independent quotient of a polynomial ring over a field
        I:Ideal
            a homogeneous ideal of a polynomial ring such that the quotient is level linearly independent
        MaxTime=>Number
        TikZ=>Boolean
        Visual=>Boolean
        AllOrders=>Boolean
    Outputs
        :List
            whose elements are lists representing orders with respect to which the poset is Macaulay, or a @TO "String"@ if time runs out or the input doesn't satisfy the conditions
    Description
        Text
            Given a poset with rank functon $r$, this method returns all Macaulay orders $<$ on the poset such that $r(p)<r(q)$ implies $p<q$.
            
            Given a quotient $S$ of a polynomial ring, this method returns Macaulay orders on the monomial poset of $S$.
            
            A total order $<$ on a poset $P$ is represented by a @TO "list"@. It is the permutation of the of the ground set of $P$ that is increasing with respect to $<$.
        Example
            macaulayOrders(product(chain 3, chain 3))
        Text
            The option @TO "TikZ"@ can print a Hasse diagram of the poset for each order $<$ such that the vertices in each level are ordered left to right according to $<$.
        Example
            macaulayOrders(ZZ/2[x,y]/(x^4,x^3-y^3,y^4), TikZ=>true)
    Caveat
        Given a @TO "QuotientRing"@ or an @TO "Ideal"@, this method will not verify level linear independence.
    SeeAlso
        isMacaulay
Node
    Key
        isMacaulay
        (isMacaulay, Poset)
        (isMacaulay, QuotientRing)
        (isMacaulay, Ideal)
    Headline
        whether a poset or ring is Macaulay
    Usage
        isMacaulay P
        isMacaulay S
        isMacaulay I
    Inputs
        P:Poset
        S:QuotientRing 
            a homogeneous level linearly independent quotient of a polynomial ring over a field
        I:Ideal
            a homogeneous ideal of a polynomial ring such that the quotient is level linearly independent
        MaxTime=>Number
        TikZ=>Boolean
        Visual=>Boolean
    Outputs
        :Boolean
            or a @TO "String"@ if time runs out or the input doesn't satisfy the conditions
    Description
        Text
            Suppose $P$ is a ranked poset and $<$ is a total order on the ground set of $P$. Let $\operatorname{Seg}_d n$ denote the largest $n$ elements of rank $d$ with respect to $<$. Suppose that for every integer $d$ between $0$ and the rank of $P$, and for every subset $A$ of the $d$th level of $P$, we have $\lvert\nabla_P\operatorname{Seg}_d\lvert A\rvert\rvert \leq \lvert\nabla_P(A)\rvert$ and $\nabla_P\operatorname{Seg}_d\lvert A\rvert = \operatorname{Seg}_{d+1}\lvert\nabla_P(A)\rvert$. Then, we say $P$ is Macaulay with respect to $<$. A Macaulay poset is a poset for which there exists an order with repsect to which it is Macaulay.
            
            There is an analogous property for rings, not to be confused with the Cohen-Macaulay property.
            
            Products of chains are Macaulay by the Clements-Lindstrom Theorem.
        Example
            isMacaulay booleanLattice 3
            isMacaulay(ZZ/2[x,y,z]/monomialIdeal(x^2,y^3,z^3))
        Text
            It is possible for a monomial poset to not be Macaulay.
        Example
            isMacaulay(ZZ/2[w,x,y,z]/monomialIdeal(w^4,x^2,y^2,z^2,x*w,y*w,z*w))
        Text
            If the poset is Macaulay, the option @TO "TikZ"@ can provide a Hasse diagram in which the vertices are horizontally ordered according to a Macaulay order.
        Example
            isMacaulay(ZZ/2[x,y]/monomialIdeal(x^2, y^3), TikZ=>true)
        Text
            If the input doesn't satisfy the conditions, or time runs out, then a @TO "String"@ will be returned.
        Example
            print isMacaulay(ZZ/2);
            print isMacaulay(ZZ/2[x,y]/ideal(x^3,x^2-y));
            print isMacaulay(chain 1, MaxTime=>0);
    Caveat
        Given a @TO "QuotientRing"@ or an @TO "Ideal"@, this method will not verify level linear independence.
    SeeAlso
        macaulayOrders
        isAdditive
        upperShadow
        lowerShadow
Node
    Key
        isAdditive
        (isAdditive, Poset)
    Headline
        whether a poset is additive
    Usage
        isAdditive P
    Inputs
        P:Poset
        MaxTime=>Number
    Outputs
        :Boolean
            or a @TO "String"@ if the method runs out of time
    Description
        Text
            See section 3.2 of @HREF "https://arxiv.org/pdf/2502.15166"@ for a definition of additivity.
            
            Boolean lattices are additive. Therefore, the @TO2(posetWedgeProduct, "wedge product")@ of a Boolean lattice with itself is Macaulay.
        Example
            B = booleanLattice 3
            isAdditive B
            isMacaulay posetWedgeProduct(B, B)
        Text
            Here is a non-additive poset.
        Example
            isAdditive poset({1,2,3,4,5,6}, {{1,2},{1,3},{2,4},{3,4},{3,5},{3,6}})
    SeeAlso
        isMacaulay
        upperShadow
        lowerShadow
Node
    Key
        MaxDegree
        [getMons,MaxDegree]
        [getPoset,MaxDegree]
    Headline
        the maximum degree of a monomial to calculate
    Usage
        getMons(R, I, MaxDegree=>Number)
        getPoset(R, I, MaxDegree=>Number)
    Description
        Text
            For getMons, the function starts at the lowest degree 0 and starts working out the monomials of 0, 1, 2, ... until the number given.
        Example
            getMons( QQ[x,y], ideal(x^2 - y^2), MaxDegree=>15 )
    SeeAlso
        getMons
        getPoset
Node
    Key
        MaxTime
        [macaulayOrders,MaxTime]
        [isMacaulay,MaxTime]
        [isAdditive,MaxTime]
    Headline
        how many seconds to spend before giving up
    Usage
        macaulayOrders(P, MaxTime=>Number)
        isMacaulay(P, MaxTime=>Number)
        isAdditive(P, MaxTime=>Number)
    Description
        Text
            Within one second of after this many seconds has elapsed, if the method has not halted yet, then the method will halt and return the @TO String@ "This poset is too large.".
            
            This should be set to an @TO2 {ZZ, "integer"}@, a @TO2 {QQ, "rational number"}@, a @TO2 {RR, "real number"}@, or an @TO InfiniteNumber@. If set to @TO "infinity"@, the method will run without a time limit.
        Example
            print isMacaulay(booleanLattice 2, MaxTime=>infinity)
            print isMacaulay(booleanLattice 9, MaxTime=>-1)
    SeeAlso
        macaulayOrders
        isMacaulay
        isAdditive
Node
    Key
        TikZ
        [macaulayOrders,TikZ]
        [isMacaulay,TikZ]
    Headline
        whether to print TikZ code
    Usage
        macaulayOrders(P, TikZ=>Boolean)
        isMacaulay(P, TikZ=>Boolean)
    Description
        Text
            Suppose the poset is Macaulay with respect to a total order $<$. If two vertices $p$ and $q$ have the same rank, then $p<q$ if and only if $p$ is to the left of $q$ in the Hasse diagram drawn with @TT "TikZ=>true"@. For @TO macaulayOrders@ with @TT "AllOrders=>true"@, there is a Hasse diagram for each order. For @TO isMacaulay@ and for @TO macaulayOrders@ with @TT "AllOrders=>false"@, there is at most one Hasse diagram.
        Example
            QQ[x,y]
            macaulayOrders(TikZ=>true, standardMonomialPoset monomialIdeal(x^3, x^2*y^2, y^3))
    SeeAlso
        macaulayOrders
        isMacaulay
        Visual
Node
    Key
        Visual
        [macaulayOrders,Visual]
        [isMacaulay,Visual]
    Headline
        whether to visualize the Macaulay orders
    Usage
        macaulayOrders(P, Visual=>Thing)
        isMacaulay(P, Visual=>Thing)
    Description
        Text
            If this option is set to a @TO String@ or to the @TO Boolean@ @TT "true"@, then the method will use @TO Visualize@ to create a visualization of the poset, and an additional visualization for each Macaulay order with the vertices labeled with integers according to the order. If set to a @TO String@, that port will be used. Otherwise, port "8080" will be used.
    SeeAlso
        macaulayOrders
        isMacaulay
        TikZ
Node
    Key
        AllOrders
        [macaulayOrders,AllOrders]
    Headline
        whether to return all orders instead of at most one order
    Usage
        macaulayOrders(P, AllOrders=>Boolean)
    Description
        Text
            With @TT "AllOrders=>false"@, the list returned by the method will have at most one element.
        Example
            B = booleanLattice 3
            #(macaulayOrders B)
            macaulayOrders(B, AllOrders=>false)
    SeeAlso
        macaulayOrders
///

--------------------------------------------------
--- Tests
--------------------------------------------------

-- Poset maps
TEST ///
    P = chain 1
    Q = chain 2
    f = map(Q, P, {1=>1})
    g = map(P, Q, {1=>1, 2=>1})
    assert(source f == P and source g == Q)
    assert(target f == Q and target g == P)
    assert(isInjective f and not isInjective g)
    assert(isSurjective g and not isSurjective f)
    assert(isRankPreserving f and not isRankPreserving g)
    assert(image f == {1})
    assert(f 1 == 1 and g 1 == 1 and g 2 == 1)
    assert(g*f*g == g)
    assert(not f == g)
///

-- figures from section 1
TEST ///
    -- figure 1
    S = ZZ/2[x,y,z]/monomialIdeal(x^2,y^3,z^2,x*y,y*z,z*x)
    assert isMacaulay S
    -- assert not isMacaulay tensor(S, S)
    
    -- figure 2
    S = ZZ/2[x,y]/monomialIdeal(x^3,x^2*y,x*y^2,y^3)
    T = ZZ/2[x,y]/monomialIdeal(x^2,y^2)
    assert isMacaulay S
    assert isMacaulay T
    assert not isMacaulay tensor(S, T)
    
    -- figure 3
    P = chain 2;
    Q = adjoinMin posetWedgeProduct(chain 2, chain 2);
    assert isMacaulay P
    assert isMacaulay Q
    assert not isMacaulay product(P, Q)
    
    -- figure 4
    R = ZZ/2[x]/monomialIdeal(x^2)
    S = ZZ/2[x,y]/(x^2-y^2,x^3,y^3)
    assert isMacaulay R
    assert isMacaulay S
    assert not isMacaulay tensor(R, S)
    
    -- figure 6
    S = ZZ/2[y,z]/monomialIdeal(y^3,y^2*z,y*z^2,z^3)
    T = ZZ/2[x]/monomialIdeal(x^2)
    assert isMacaulay S
    assert not isMacaulay tensor(S, T)
///

-- getPoset
TEST ///
    R = QQ[x, y]
    I = ideal(x^2, y^2)
    S = R/I
    x = S_0
    y = S_1
    
    assert(set getMons(R, I) === set {1_S, x, y, x*y})
	
	assert( 31 == #getMons( QQ[x, y], ideal(x^2 - y^2), MaxDegree => 15 ) )
    
    x = R_0
    y = R_1
    
    assert (  areIsomorphic( getPoset( R, ideal(x*y - y^2, x^4, x^3*y, x^2*y^2)  ), poset( {1, x, x^2, x^3, y, y^2, y^3}, { {1, x}, {1, y}, {x, x^2}, {x, y^2}, {x^2, x^3}, {x^2, y^3}, {y, y^2}, {y^2, y^3} }  ) ) )
///

-- Tests for the poset operations
TEST ///
-- posetWedgeProduct
assert areIsomorphic( posetWedgeProduct(chain 2, chain 2), adjoinMin poset({1,2}, {}) )

-- posetClosedProduct
assert areIsomorphic( posetClosedProduct(chain 3, chain 3), product(chain 2, chain 2) )

-- posetFiberProduct
P = product(chain 2, chain 2)
Q = product(chain 2, chain 3)

f = map(Q, P, {{1,1}, {1,2}, {2,1}, {2,2}})
g = map(Q, P, {{1,1}, {2,1}, {1,2}, {2,2}})

assert areIsomorphic( posetFiberProduct(f, g), getPoset(ZZ/2[x,y]/monomialIdeal(x^3,x^2*y^2,y^3)) )
assert areIsomorphic( posetFiberProduct(f, f), product(chain 2, adjoinMin posetWedgeProduct(chain 2, chain 2)) )

-- posetConnectedSum
i = map(chain 5, chain 2, {1=>1, 2=>2})
assert areIsomorphic( posetConnectedSum(i, i), adjoinMin adjoinMax product(chain 2, chain 2) )
///

-- Tests for the ring operations
TEST ///
-- ringFiberProduct
S1 = QQ[x, y]/ideal(x^2, y^2);
P1 = getPoset(S1);
assert( areIsomorphic(  posetWedgeProduct( P1, P1 ), getPoset(  ringFiberProduct( S1, S1)  )    )  )

S2 = ZZ/7[a, b, c]/ideal(a^3, b^3, c^3);
S3 = ZZ/7[x, y, z]/ideal(x^4, y^2, z);
assert( areIsomorphic( posetWedgeProduct(getPoset(S2), getPoset(S3) ), getPoset( ringFiberProduct(S2, S3)  ) ) )

-- ringConnectedSum
assert areIsomorphic( poset({0,1,2,3,4,5},{{0,1},{0,2},{1,3},{2,4},{3,5},{4,5}}), getPoset ringConnectedSum( ZZ/2[x]/monomialIdeal(x^4), ZZ/2[x]/monomialIdeal(x^4) ) );
assert areIsomorphic( getPoset ringConnectedSum( ZZ/2[x,y]/monomialIdeal(x^3,y^3), ZZ/2[x,y]/monomialIdeal(x^2,y^4) ), posetClosedProduct(product(chain 3, chain 3), product(chain 2, chain 4)));
///

-- Some wedge and closed product tests.
TEST ///
-- Example 2.5 
assert(   isMacaulay(   posetClosedProduct( getPoset(QQ[x, y, z]/ideal(x*y, x*z, y^2 - y*z, y^2 - z^2, x^3 - y^3  )  ), getPoset(QQ[x, y, z]/ideal(x*y, x*z, y^2 - y*z, y^2 - z^2, x^3 - y^3  )  ))    )     )
assert(   isMacaulay(   posetWedgeProduct( getPoset(QQ[x, y, z]/ideal(x*y, x*z, y^2 - y*z, y^2 - z^2, x^3 - y^3  )  ), getPoset(QQ[x, y, z]/ideal(x*y, x*z, y^2 - y*z, y^2 - z^2, x^3 - y^3  )  )    )    ) == false    )
///


TEST ///
-- Theorem 2.18
P = getPoset(QQ[x, y]/ideal(x^2, y^2));
assert (isAdditive(P))
assert (isAdditive( P ) == isMacaulay(posetWedgeProduct( P, P )) == isMacaulay(posetClosedProduct( P, P )))

-- Example 4.4
assert(   not isMacaulay( posetClosedProduct(getPoset(QQ[x, y, z]/ideal(x^2, y^2, z^2)  ), chain(4)  )  )    )
assert(   not isMacaulay( posetClosedProduct(getPoset(QQ[x, y, z]/ideal(x^2, y^2, z^2)  ), getPoset(QQ[x, y]/ideal(x^2, y^3))  )  )    )

-- Corollary 4.5
P3 = getPoset( QQ[x, y]/ideal(x^4, y^4)  );
assert(  isMacaulay( posetClosedProduct( P3, P3, P3 )  )  )
///


TEST ///
-- A test to check the code. 
assert(  getPoset(QQ[x, y]/ideal(x^3 - y^2)) == "The given ring must be homogeneous." )

-- Example 2.3
assert( isMacaulay( posetWedgeProduct(  chain(5), chain(4), chain(10) ) )   )
assert( isMacaulay( posetClosedProduct( chain(5), chain(5), chain(5)   ) )   )

-- Example 2.7
P27 = booleanLattice(2);
P27Top = adjoinMax(P27, "newMax");
P27Bottom = adjoinMin(P27, "newMin");

assert( isMacaulay( posetClosedProduct(P27Top, P27Bottom)  )   )
assert( not isMacaulay( posetWedgeProduct(P27Top, P27Bottom) ) )

-- Example 2.8
assert( isMacaulay(   posetWedgeProduct(chain(2), posetClosedProduct(chain(2), chain(2) )   )   )  )
///

TEST ///
-- Example 3.5 (non-Macaulay fiber products of box rings)
assert ( not isMacaulay( ringFiberProduct(  QQ[x, y, z]/ideal(x^2, y^2, z^2), QQ[w]/ideal(w^4)  )    ))

assert( not isMacaulay( ringFiberProduct( QQ[x, y, z]/ideal(x^2, y^2, z^2), QQ[v, w]/ideal(v^2, w^3)   ) )  )

assert( not isMacaulay( ringFiberProduct( ZZ/5[x, y]/ideal(x^3, y^3), ZZ/5[z]/ideal(z^5)  )  )  )
///

-- Upper shadows of levels of a Boolean lattice
TEST ///
B = booleanLattice 3
levels = rankPoset B
assert(set levels#1 === set upperShadow(B, levels#0))
assert(set levels#2 === set upperShadow(B, levels#1))
assert(set levels#3 === set upperShadow(B, levels#2))
assert(set {} === set upperShadow(B, levels#3))
///

-- Lower shadows of levels of a Boolean lattice
TEST ///
B = booleanLattice 3
levels = rankPoset B
assert(set {} === set lowerShadow(B, levels#0))
assert(set levels#1 === set lowerShadow(B, levels#2))
assert(set levels#2 === set lowerShadow(B, levels#3))
///

-- isMacaulay
TEST ///
assert not isMacaulay(QQ[w,x,y,z]/monomialIdeal(w^3,x*w^2,x^3, y^3,z*y^2,z^3, w*y,w*z,x*y,x*z))
///

TEST ///
assert isMacaulay(QQ[x,y]/(x^6,x^3*y,y^4,x^2*y^3-x^5))
///

TEST ///
for i from 1 to 3 do (
    for j from i to 3 do (
        for k from j to 3 do (
            assert isMacaulay product(chain i, product(chain j, chain k));
        );
    );
);

for i from 3 to 4 do assert isMacaulay booleanLattice i;
///

TEST ///
for i from 1 to 4 do (
    for j from i to 4 do (
        for k from j to 4 do (
            assert isMacaulay(QQ[x,y,z]/monomialIdeal(x^i,y^j,z^k, x*y,y*z,z*x));
        );
    );
);
///

-- macaulayOrders for a poset in which every Macaulay order has the same restrictions to each level
TEST ///
P = poset({0,1,2,3,4,5,6,7,8,9,10},{{0,1},{0,2},{1,3},{1,4},{2,5},{3,6},{3,7},{4,8},{5,9},{6,10}});

for order in macaulayOrders P do (
    for level in rankPoset P do (
        for i from 0 to (#level)-2 do (
            assert(level#i < level#(i+1));
        );
    );
);
///

-- macaulayOrders for a 3x3 box poset
TEST ///
R = QQ[x,y]
I = monomialIdeal(x^3,y^3)
S = R/I
P = standardMonomialPoset I
x = S_0
y = S_1
-- Suppose P is Macaulay wrt <. Then x<y iff x^2<y^2. Also, x^2<xy<y^2 or y^2<xy<x^2.
for order in macaulayOrders P do (
    xFound := false;
    yFound := false;
    
    xLy := false;
    
    for i from 0 to (#order)-1 do (
        xFound = xFound or x==order#i;
        yFound = yFound or y==order#i;
        
        if xFound and not yFound then (
            xLy = true;
            break;
        );
        if yFound and not xFound then (
            xLy = false;
            break;
        );
    );
    
    x2found := false;
    y2found := false;
    xyFound := false;
    for i from 0 to (#order)-1 do (
        x2found = x2found or x^2==order#i;
        y2found = y2found or y^2==order#i;
        xyFound = xyFound or x*y==order#i;
        
        if ((not x2found) and (not y2found)) then (
            assert(not xyFound);
        );
        if x2found and not y2found then (
            assert(xLy);
        );
        if y2found and not x2found then (
            assert(not xLy);
        );
        if x2found and y2found then (
            assert(xyFound);
        );
    );
);
///

-- AllOrders
TEST ///
for i from 1 to 3 do (
    for j from i to 3 do (
        for k from j to 3 do (
            assert(1 == #(macaulayOrders(AllOrders=>false, product(chain i, product(chain j, chain k)))));
        );
    );
);
///

TEST ///
assert isAdditive booleanLattice 3
assert not isAdditive poset({1,2,3,4,5,6}, {{1,2},{1,3},{2,4},{3,4},{3,5},{3,6}})
///

end--
