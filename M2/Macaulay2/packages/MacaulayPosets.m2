newPackage(
    "MacaulayPosets",
    Version => "1.0",
    Date => "April 5, 2026",
    Headline => "Macaulay posets",
    Authors => { 
        {
            Name => "Penelope Beall", 
            Email => "pbeall@ucdavis.edu", 
            HomePage=>"https://pbeall.github.io"
        }, 
        {
            Name => "Yu Olivier Li", 
            Email => "yol1@st-andrews.ac.uk"
        }
    },
    PackageExports => {
        "Posets"
    },
    PackageImports => {
        "Visualize"
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
    "TikZ",
    "Visual",
    "AllOrders"
}


--------------------------------------------------
--- Poset maps
--------------------------------------------------

PosetMap = new Type of HashTable;
PosetMap.synonym = "map of posets";

source PosetMap := Poset => (f) -> (f.source)
target PosetMap := Poset => (f) -> (f.target)

map (Poset, Poset, HashTable) := PosetMap => opts -> (Q, P, f) -> (
    -- check that the domain of f is P
    if not set keys f == set vertices P then error "the keys must be vertices of the source Poset";
    
    -- check that the codomain of f is contained in Q
    if not isSubset(values f, vertices Q) then error "the image must be contained in the codomain";
    
    g := new PosetMap from join({
        symbol source => P,
        symbol target => Q},
        pairs f
    );
    -- check monotonicity
    scan(coveringRelations P, c -> if not compare(Q, g(c#0), g(c#1)) then error "the resulting PosetMap would not be monotone");
    g
)

map (Poset, Poset, List) := PosetMap => opts -> (Q, P, L) -> (
    -- Either everything in L a vertex of Q,
    if isSubset(L, vertices Q) then (
        return map(Q, P, new HashTable from apply(#P_*, i -> P_*#i => L#i));
    );
    -- or everything in L is an Option and not everything in L is a vertex of Q
    if all(L, i -> instance(i, Option)) then (
        return map(Q, P, new HashTable from L);
    );
    error "list elements must all be vertices of the target or all be Options";
)

PosetMap Thing := Thing => (f, p) -> (
    if not f#?p then error "the element is not in the source of the map.";
    f#p
)

PosetMap == PosetMap := Boolean => (f, g) -> (
    if not ((source f) == (source g) and (target f) == (target g)) then return false;
    all(vertices source f, i -> f(i) == g(i))
)

PosetMap * PosetMap := PosetMap => (f, g) -> (
    map(target f, source g, apply(vertices source g, p -> f(g(p))))
)

image (PosetMap, List) := List => (f, L) -> (
    unique apply(select(L, l -> isMember(l, vertices source f)), p -> f(p))
)

image PosetMap := List => (f) -> (
    image(f, vertices source f)
)

isInjective PosetMap := Boolean => (f) -> (
    --use finiteness
    #(image f) == #(vertices source f)
)

isSurjective PosetMap := Boolean => (f) -> (
    isSubset(vertices target f, image f)
)

isRankPreserving = method();
isRankPreserving PosetMap := Boolean => (f) -> (
    if (not isRanked source f) or (not isRanked target f) then return false;
    
    targetRankFunction := rankFunction target f;
    targetRank := hashTable apply(#(vertices target f), i -> (vertices target f)#i => targetRankFunction#i);
    
    sourceRankFunction := rankFunction source f;
    sourceRank := hashTable apply(#(vertices source f), i -> (vertices source f)#i => sourceRankFunction#i);
    
    all(vertices source f, i -> sourceRank#i == targetRank#(f(i)))
)

--------------------------------------------------
--- Getting posets of polynomial rings with any homogeneous ideal
--------------------------------------------------

-- Suppose s is a monomial in a ring S. 
-- This returns what degree s would be if S was replaced by newRing(S, Degrees=>toList((numgens S):1))
theDegree = (s) -> (
    S := ring s;
    if s == 0_S then return -infinity;
    sum(unique gens S, g -> if g != 0_S then degree(g, s) else 0)
)

isHomogeneousWrtVars = (I) -> (
    S := (ring I)/I;
    isHomogeneous newRing(S, Degrees=>toList((numgens S):1))
)

-- First function is to get all the unique monomial representations in the quotient ring. 
-- The function super basis is the cornerstone for this. 
-- Everything else is just manipulating the data. 
getMons = method(Options => { MaxDegree => 10 } );
getMons (PolynomialRing, Ideal) := opts -> (R, I) -> (
    if not isField baseRing R then error "the base ring of the polynomial ring must be a field";
    if not isHomogeneousWrtVars I then error "the given ring must be homogeneous";
    try S := R/I else error "the first input needs to be a polynomial ring R, second input needs to be an ideal of R";
    
    -- If the quotient ring has a finite number of elements (i.e. the super basis returns a finite number of monomials): 
    try flatten entries super basis S 
    else ( 
        -- If the quotient ring doesn't have a finite number of elements:
        print "Ideal is not finite over the base (quotient ring has infinitely many monomials)";
        print concatenate("Monomials up to degree ", toString(opts.MaxDegree), " given");
        
        G := gens S;
        minEl := fold((a, b) -> a * b^0, 1, G);
        -- N#i will be the list of degree i monomials
        M := {minEl};
        M | flatten for i from 0 to (opts.MaxDegree - 1) list (
            -- Compute a basis for m*S_i (m = maximal ideal)
            M = unique flatten apply(M, m -> select( apply(G, g -> m*g), a -> a != 0_S ) )
        )
    )
)
getMons Ideal := opts -> (I) -> (
    R := ring I;
    if not isPolynomialRing R then error "the given ideal must be an ideal of a polynomial ring";
    if not isField baseRing R then error "the base ring of the polynomial ring must be a field";
    getMons(R, I, MaxDegree => opts.MaxDegree)
)
getMons QuotientRing := opts -> (S) -> (
    R := ambient S;
    if not isPolynomialRing R then error "the given quotient ring must be the quotient of a polynomial ring R and an ideal of R";
    if not isField baseRing R then error "the base ring of the polynomial ring must be a field";
    getMons(R, ideal S, MaxDegree => opts.MaxDegree)
)
getMons PolynomialRing := opts -> (R) -> (
    getMons(R, ideal(0_R), MaxDegree => opts.MaxDegree)
)

getCoverRels = (mons, R, I) -> (
    S := R/I;
    for monPair in subsets(mons, 2) list (
        -- For each monomial in the mons list multiply them with another monomial.
        -- Then check if the monomial divides the result and the degree difference is one.  
        (mon, otherMon) := toSequence monPair;
        if abs(theDegree mon - theDegree otherMon) != 1 then continue;
        if otherMon % mon == 0 then monPair
        else if mon % otherMon == 0 then reverse monPair
        else continue
    )
)

getPoset = method(TypicalValue => Poset, Options => {MaxDegree => 10});
getPoset (PolynomialRing, Ideal) := opts -> (R, I) -> (
    mons := getMons(R, I, MaxDegree => opts.MaxDegree);
    if instance(mons, String) then return mons;
    poset(mons, getCoverRels(mons, R, I))
)
getPoset Ideal := opts -> (I) -> ( -- Given an ideal as an input.
    R := ring I;
    if not isPolynomialRing R then error "the given ideal must be an ideal of a polynomial ring";
    if not isField baseRing R then error "the base ring of the polynomial ring must be a field";
    getPoset(R, I, MaxDegree => opts.MaxDegree)
)
getPoset QuotientRing := opts -> (S) -> ( -- Given a quotient ring as an input.
    R := ambient S;
    if not isPolynomialRing R then error "the given quotient ring must be the quotient of a polynomial ring R and an ideal of R";
    if not isField baseRing R then error "the base ring of the polynomial ring must be a field";
    getPoset(R, ideal S, MaxDegree => opts.MaxDegree)
)
getPoset PolynomialRing := opts -> (R) -> (
    getPoset(R, ideal(0_R), MaxDegree => opts.MaxDegree)
)


--------------------------------------------------
--- Poset Operations
--------------------------------------------------
posetWedgeProduct = method()
posetWedgeProduct List := Poset => posets -> (
    -- Check that the posets are actually posets with unique minimal elements.
    for idxPoset in pairs posets do (
        (idx, P) := idxPoset;
        if not instance(P, Poset) then (
            error concatenate("all elements in the list must be posets; element number ", toString(idx+1), " is not a Poset");
        );
        if #(minimalElements P) != 1 then (
            error "all posets must have a unique minimal element";
        );
    );    
    
    allPosetElements := for idxPoset in pairs posets list (
        (idx, P) := idxPoset;

        -- For the ith poset, we are creating a set with the same number of elements as the number of elements in the ground set.
        minimalElement := first minimalElements P;
        minimalElementIndex := position(P_*, b -> b === minimalElement);
        
        posetiGroundSet := toList((idx, 1) .. (idx, #P_*));
        -- Then replace the element of the same position in the new set.
        posetiGroundSet = insert(minimalElementIndex, "min", posetiGroundSet);
        drop(posetiGroundSet, {minimalElementIndex + 1, minimalElementIndex + 1})
    );
    
    wedgeGroundSet := unique flatten allPosetElements;
    wedgeCoverRelations := flatten for idxPoset in pairs posets list (
        (idx, P) := idxPoset;
        for coverRelation in coveringRelations P list (
            -- Find the positions of the elements in their respective poset ground set.
            firstPosition := position(P_*, x -> x === first coverRelation);
            secondPosition := position(P_*, x -> x === last coverRelation);
            {(allPosetElements#idx)_firstPosition, (allPosetElements#idx)_secondPosition}
        )
    );    
    poset(wedgeGroundSet, wedgeCoverRelations)
)
posetWedgeProduct (Poset, Poset) := Poset => (P, Q) -> (posetWedgeProduct {P, Q})


posetFiberProduct = method()
posetFiberProduct List := Poset => posetMaps -> (
    numberPosetMaps := #posetMaps;
    if numberPosetMaps == 0 then error "number of poset maps must be greater than 0";
    
    for idxf in pairs posetMaps do (
        (idx, f) := idxf;
        if not instance(f, PosetMap) then (
            error concatenate("all elements in the list must be poset maps; element number ", toString(idx+1), " is not a PosetMap");
        );
        if not isRankPreserving f then (
            error "all poset maps must be rank-preserving";
        );
        if not isInjective f then (
            error "all poset maps must be injective";
        );
    );
    fiberSource := source first posetMaps;
    for f in posetMaps do (
        if not source f == fiberSource then error "all poset maps must have the same source";
    );
    
    allPosetElements := apply(vertices fiberSource, b -> (numberPosetMaps, b));
    posetElementsAndCovers := for idxf in pairs posetMaps list (
        (idx, f) := idxf;
        -- For the ith map, take its elements not in the image of the map and disjointify them from the other images
        posetiGroundSet := apply(select((target f)_*, b -> not isMember(b, image f)), b -> (idx, b));
        
        -- Also, turn the old cover relations into cover relations for the new poset
        preimagei := hashTable for p in (source f)_* list f(p) => p;
        posetCoverRelations := for coveringRelation in coveringRelations target f list (
            for j in (0,1) list (
                posetElement := coveringRelation#j;
                if preimagei#?posetElement then (numberPosetMaps, preimagei#posetElement) else (idx, posetElement)
            )
        );
        {posetiGroundSet, posetCoverRelations}
    );
    allPosetElements = flatten append(allPosetElements, first \ posetElementsAndCovers);
    allPosetCovers := last \ posetElementsAndCovers;    
    poset(flatten allPosetElements, unique flatten allPosetCovers)
)
posetFiberProduct (PosetMap, PosetMap) := Poset => (P, Q) -> (posetFiberProduct {P, Q})


posetConnectedSum = method()
posetConnectedSum List := Poset => posetMaps -> (    
    for idxf in pairs posetMaps do (
        (idx, f) := idxf;
        if not instance(f, PosetMap) then (
            error concatenate("all elements in the list must be poset maps; element number ", toString(idx+1), " is not a PosetMap");
        );
        if not isRankPreserving f then (
            error "all poset maps must be rank-preserving and injective";
        );
        if not isInjective f then (
            error "all poset maps must be injective";
        );
    );
    
    -- Take disjointified copies of the codomains, except add some glue according to the maps.
    -- The desired poset is the non-disjoint union of these connected summands.
    union for idxf in pairs posetMaps list (
        (idx, f) := idxf;
        iso := isomorphism(target f, dual target f);
        fInverse := hashTable apply(vertices source f, p -> f#p => p);
        
        newLabels := hashTable for p in vertices target f list (
            if isMember(p, image f) then (
                p => (0, fInverse#p)
            ) else if isMember(iso#p, image f) then (
                p => (#posetMaps + 1, fInverse#(iso#p))
            ) else (
                p => (idx, p)
            )
        );
        labelPoset(target f, newLabels)
    )
)
posetConnectedSum (PosetMap, PosetMap) := Poset => (P, Q) -> (posetConnectedSum {P, Q})


posetClosedProduct = method()
posetClosedProduct List := Poset => posets -> (
    for idxPoset in pairs posets do (
        (idx, P) := idxPoset;
        if not instance(P, Poset) then (
            error concatenate("all elements in the list must be posets; element number ", toString(idx+1), " is not a Poset");
        );
    );
    
    posetRanks := for P in posets list (
        if (#(minimalElements P) != 1 or #(maximalElements P) != 1) then (
            error "all posets must have a unique minimal element and a unique maximal element";
        );
        #(rankPoset P) - 1
    );
    if #(unique posetRanks) > 1 then (
        print "the given posets do not have the same rank, therefore the resulting poset will not be ranked";
    );
    
    allPosetElements := for idxPoset in pairs posets list (
        (idx, P) := idxPoset;
        posetiGroundSet := toList((idx, 1) .. (idx, #P_*));

        -- Replace the element in the minimum element index with a common minimal element m_l.
        minimalElement := first minimalElements P;
        minimalElementIndex := position(P_*, b -> b === minimalElement);
        posetiGroundSet = insert(minimalElementIndex, "min", posetiGroundSet);
        posetiGroundSet = drop(posetiGroundSet, {minimalElementIndex + 1, minimalElementIndex + 1});
        
        -- Do the same as above but replace the element in the maximum element 
        -- index with a common maximal element "max".
        maximalElement := first maximalElements P;
        maximalElementIndex := position(P_*, b -> b === maximalElement);
        posetiGroundSet = insert(maximalElementIndex, "max", posetiGroundSet);
        drop(posetiGroundSet, {maximalElementIndex + 1, maximalElementIndex + 1})
    );

    closedGroundSet := unique flatten allPosetElements;    
    closedCoverRelations := flatten for idxPoset in pairs posets list (
        (idx, P) := idxPoset;
        for coverRelation in coveringRelations P list (
            firstPosition := position(P_*, x -> x === first coverRelation);
            secondPosition := position(P_*, x -> x === last coverRelation);
            {(allPosetElements#idx)_firstPosition, (allPosetElements#idx)_secondPosition}
        )
    );
    poset(closedGroundSet, closedCoverRelations)
)
posetClosedProduct (Poset, Poset) := Poset => (P, Q) -> (posetClosedProduct {P, Q})


--------------------------------------------------
--- Ring Operations
--------------------------------------------------
ringProductHelper := (I1, I2, connected) -> (
    -- I and J are ideals. Since the presentation of the fiber product and the
    -- connected sum are nearly the same, the `connected` argument is a flag
    -- determining which construction to use.
    R1 := ring I1;
    R2 := ring I2;
    if not (isPolynomialRing R1 and isPolynomialRing R2) then 
        error "both quotient rings must be the quotient of a polynomial ring R and an ideal of R";
    if not (isField coefficientRing R1 and isField coefficientRing R2) then 
        error "both quotient rings must be quotients of a polynomial ring with base ring of a field";
    if not (coefficientRing R1 === coefficientRing R2) then 
        error "the coefficient field of both ambient rings of both quotient rings must be the same";
    if not (isHomogeneousWrtVars I1 and isHomogeneousWrtVars I2) then 
        error "both ideals of each quotient ring must be homogeneous";
    
    try S := R1/I1 else error concatenate(toString(I1), " must be an ideal of ", toString(R1));
    try T := R2/I2 else error concatenate(toString(I2), " must be an ideal of ", toString(R2));
    
    -- The ambient ring of the ring tensor product is the same as what we want 
    -- for the ring fiber product.
    ringTensorProduct := S**T;
    fiberRing := ambient ringTensorProduct; 
    fiberIdeal := ideal ringTensorProduct;
    
    -- Get the two maps into the tensor product.
    -- We assume that the fiberRing gained from the tensor product will be in 
    -- the correct form with the variables of the 1st ring at the start, and 
    -- the variables of the 2nd ring at the end.
    R1toTensor := map(fiberRing, R1, take(gens fiberRing, #(gens R1)));
    R2toTensor := map(fiberRing, R2, take(gens fiberRing, -#(gens R2)));
    
    extraIdeal := ideal apply((gens R1)**(gens R2), (x,y) -> R1toTensor(x) * R2toTensor(y));
    fiberIdeal = fiberIdeal + extraIdeal;
    if connected then (
        maxs := for I in {I1, I2} list (
            maxP := maximalElements getPoset I;
            if #maxP != 1 then error "the monomial posets of the rings must have unique maximal elements";
            first maxP
        );
        fiberIdeal = fiberIdeal + ideal(R1toTensor lift(first maxs, R1) - R2toTensor lift(last maxs, R2));
    );
    fiberRing / fiberIdeal
)
ringFiberProduct = method(Binary=>true)
ringFiberProduct (Ideal, Ideal) := Ideal => (I1, I2) -> (
    ideal ringProductHelper(I1, I2, false)
)
ringFiberProduct (QuotientRing, QuotientRing) := QuotientRing => (S, T) -> (
    ringProductHelper(ideal S, ideal T, false)
)

ringConnectedSum = method(Binary=>true)
ringConnectedSum (Ideal, Ideal) := Ideal => (I1, I2) -> (
    ideal ringProductHelper(I1, I2, true)
)
ringConnectedSum (QuotientRing, QuotientRing) := QuotientRing => (S, T) -> (
    ringProductHelper(ideal S, ideal T, true)
)

--------------------------------------------------
--- Shadows
--------------------------------------------------
shadowHelper = (P, S, p) -> (
    if not isSubset(S, P_*) then error "the given set is not a subset of the given poset";

    -- For a cover relation x < y, we say x is the lower shadow and y is the upper shadow. 
    -- For each covering relation k={k#0,k#1} with k#0 in S, the element k#1 is
    -- supposed to be in the upper shadow of S.
    -- For each covering relation k={k#0,k#1} with k#1 in S, the element k#0 is 
    -- supposed to be in the lower shadow of S.
    unique for k in coveringRelations P list if any(S, s -> s === k#p) then k#(p - 1) else continue
)

lowerShadow = method()
lowerShadow (Poset, List) := List => (P, S) -> shadowHelper(P, S, 1)

upperShadow = method()
upperShadow (Poset, List) := List => (P, S) -> shadowHelper(P, S, 0)

-- This returns the initial segment of size s in level d of the ranked poset P 
-- with respect to the order O.
-- O could be a list of elements of P, but it actually only needs to be a list 
-- containing the initial segment.
initialSegment = method()
initialSegment (Poset, List, ZZ, ZZ) := List => (P, O, d, s) -> (
    if O != unique O then error "the given order must not have duplicate entries";
    if (s > 0 and #O == 0) then error "the order must contain the desired segment.";
    
    dLevel := (rankPoset P)#d;
    if (s < 0 or s > #dLevel) then (
        error "the size of the segment must be inclusively between 0 and the size of the dth level";
    );
    
    j := #O-1;
    for i from 0 to s-1 list (
        while not isMember(O#j, dLevel) do (
            j = j-1;
            if j < 0 then (
                error "the order must contain the desired segment";
            );
        );
        j = j-1;
        O#(j+1)
    )
)

finalSegment = method()
finalSegment (Poset, List, ZZ, ZZ) := List => (P, O, d, s) -> initialSegment(P, reverse O, d, s)

--------------------------------------------------
--- Macaulay orders
--------------------------------------------------

-- Suppose P is a ranked poset, and its subposet inclusively consisting of the 
-- levels from 0 to d is Macaulay with respect to the total order O.
-- The integer d is supposed to be inclusively between -1 and the rank of P.
-- Then, this returns all extensions of O with respect to which P is Macaulay, 
-- modulo relations between elements of different ranks.
-- But, if not returnAllOrders, then this instead returns only a list with at 
-- most one order.
macaulayHelper := (P, O, d, returnAllOrders) -> (
    levels := rankPoset P;
    if d + 1 >= #levels then (
        -- If d is the rank of P, then it is already Macaulay with respect to O.
        return {O};
    );
    
    -- This will be the subset of permutations(levels#(d+1)) consisting of all 
    -- total orderings o of level d+1 such that the upper shadow of an initial 
    -- segment in level d wrt O is an initial segment of level d+1 wrt o.
    d1orders := {{}}; 
    if d == -1 then (
        -- For level 0, there is no lower layer putting constraints on d1orders.
        d1orders = permutations(levels#(d+1));
    ) else (
        shadows := flatten append({{{}}}, for s from 1 to #(levels#d) list (
                -- Let shadows#s be the upper shadow of the initial segment of 
                -- size s in level d.
                upperShadow(P, initialSegment(P, O, d, s))
            )
        );
        -- There might also be elements of levels#(d+1) which are not in the 
        -- upper shadow of levels#d
        shadows = append(shadows, levels#(d+1));

        for s from 1 to #(levels#d) + 1 do (
            -- Suppose d1orders is the list of possible orderings of shadows#(s-1). 
            -- Then, replace each ordering with its possible extensions to an 
            -- ordering of shadows#s.
            d1orders = flatten for o in d1orders list (
                for p in permutations(shadows#s - set shadows#(s-1)) list (                    
                    -- The initial segment of size s in level d should have an 
                    -- upper shadow no larger than the upper shadow of any other 
                    -- subset of size s in level d.
                    if any(subsets(levels#d, s), A -> #upperShadow(P, A) < #(shadows#s)) then (
                        continue
                    );
                    join(p, o)
                )
            );
        );
    );
    
    -- This will be the list of all orders with respect to which P is Macaulay, 
    -- modulo relations between different levels. Or, if not returnAllOrders, 
    -- this will have at most one order.
    orders := {};
    flatten for o in d1orders when (returnAllOrders or orders == {}) list (
        orders = macaulayHelper(P, join(O,o), d+1, returnAllOrders);
        if instance(orders, String) then return orders;
        orders
    )
)

macaulayOrders = method(TypicalValue => List, 
                        Options => {
                            TikZ => false, 
                            Visual=>false, 
                            AllOrders => true
                            }
                        )
-- Modulo relations between elements of different rank, this returns the list 
-- of orders with respect to which P is Macaulay. But, if not returnAllOrders, 
-- this returns an order with respect to which P is Macaulay.
macaulayOrders Poset := opts -> (P) -> (
    if not instance(opts.TikZ, Boolean) then error "the option TikZ must be a Boolean";
    if not instance(opts.AllOrders, Boolean) then error "the option AllOrders must be a Boolean";
    if not isRanked(P) then error "the poset must be ranked";
    
    orders := macaulayHelper(P, {}, -1, opts.AllOrders);
    
    if opts.TikZ then (
        levels := rankPoset P;
        for order in orders do ( -- Print a TikZ picture for each order.
            TikzPicture := "\\begin{tikzpicture}\n";
            
            -- draw vertices
            for i from 0 to (#levels)-1 do (
                level := levels#i;
                j := -1;
                for k from 0 to (#level)-1 do (
                    -- Find the next element of level wrt order.
                    j = j + 1;
                    while not isMember(order#j, level) do j = j + 1;
                    -- Insert a node for this element.
                    TikzPicture = concatenate(TikzPicture, "\tdraw \\node at (", toString(k - (#level-1)/2), ", ", toString(i), ") (", toString(j), ") {", tex order#j, "};\n");
                );
            );
            
            -- Draw cover relations
            elementToInt := hashTable apply(0 ..< #order, i -> order#i => i);    -- As maps, the inverse of order
            tikzPicture := concatenate for coverRelation in coveringRelations P list (
                "\n\t\\draw (", toString elementToInt#(first coverRelation), ") -- (", toString elementToInt#(last coverRelation), ");"
            );
            print concatenate(tikzPicture, "\n\\end{tikzpicture}");
        );
    );
    
    if instance(opts.Visual, String) or (instance(opts.Visual, Boolean) and opts.Visual) then (
        openPort if instance(opts.Visual, String) then opts.Visual else "8080";
        visualize P;
        -- visualize P with its Macaulay orders
        for order in orders do (
            elementToInt := hashTable apply(0 ..< #order, i -> order#i => i);    -- As maps, the inverse of order            
            visualize poset(apply(P_*, p -> elementToInt#p), apply(coveringRelations P, c -> {elementToInt#(first c), elementToInt#(last c)}));
        );
        closePort();
    );
    orders
)
macaulayOrders QuotientRing := opts -> (S) -> (
    R := ambient S;
    I := ideal S;
    
    if not isPolynomialRing R then error "the ambient ring must be a polynomial ring";
    if not isField baseRing R then error "the base ring of the polynomial ring must be a field";
    if not isHomogeneousWrtVars I then error "the ring must be homogeneous";
    -- The ideal also should be level linearly independent
    
    macaulayOrders(getPoset(R,I), TikZ => opts.TikZ, Visual => opts.Visual, AllOrders => opts.AllOrders)
)
macaulayOrders Ideal := opts -> (I) -> (
    macaulayOrders((ring I)/I, TikZ => opts.TikZ, Visual => opts.Visual, AllOrders => opts.AllOrders)
)

isMacaulay = method(TypicalValue => Boolean, 
                    Options => {
                        TikZ => false, 
                        Visual => false
                        }
                    );
isMacaulay Poset := opts -> (P) -> (
    result := macaulayOrders(P, TikZ => opts.TikZ, 
                                Visual => opts.Visual, 
                                AllOrders => false); 
    #result != 0
)
isMacaulay QuotientRing := opts -> (S) -> (
    result := macaulayOrders(S, TikZ => opts.TikZ, 
                                Visual => opts.Visual, 
                                AllOrders => false);
    #result != 0
)
isMacaulay Ideal := opts -> (I) -> (
    isMacaulay((ring I)/I, TikZ => opts.TikZ, 
                           Visual => opts.Visual)
)

--------------------------------------------------
--- Additivity
--------------------------------------------------

isAdditive = method(TypicalValue => Boolean)
isAdditive Poset := Boolean => (P) -> (    
    -- Check whether P is additive with respect to some Macaulay order.
    orders := macaulayOrders P;
    levels := rankPoset P;
    for order in orders do (        
        levelMin := -1;
        levelMax := -1;
        -- Check its additivity for all ranks.
        if all(0 ..< #levels, r -> (                
                levelMin = levelMax+1;
                while (levelMax+1 <= #order) and (r+1 >= #levels or not isMember(order#(levelMax+1), levels#(r+1))) do (
                    levelMax += 1;
                );
                level := take(order, {levelMin, levelMax});

                shadows := new MutableHashTable;
                shadows#{} = upperShadow(P, {});
                -- Compute shadows of nonempty segments.
                levelIntervals := subsets(0 ..< #level, 2) | apply(toList(0 ..< #level), i -> {i,i});                
                for idxPair in levelIntervals do (
                    -- idxPair *should* already be sorted.
                    segment := take(level, idxPair);
                    shadows#segment = upperShadow(P, segment);
                );

                -- Check additivity between levels.
                all(levelIntervals, idxPair -> (
                        (i, j) := toSequence idxPair;
                        initialNewShadow := shadows#(take(level, {#level-(j-i+1), #level-1}));
                        newShadow := shadows#(take(level, {i, j})) - set shadows#(take(level, {j+1, #level-1}));
                        finalNewShadow := shadows#(take(level, {0, j-i})) - set shadows#(take(level, {j-i+1, #level-1}));
                        #initialNewShadow >= #newShadow and #newShadow >= #finalNewShadow
                    )
                )
            )
        ) then return true;
    );
    false
)



--------------------------------------------------
--- Documentation
--------------------------------------------------

beginDocumentation()

doc ///

Node
    Key
        MacaulayPosets
    Headline
        a package for working with Macaulay posets and Macaulay rings
    Description
        Text
            This package contains a few methods for working with posets, rings, 
            and the Macaulay property.
            
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
            To define the latter two poset operations, a new data type 
            @TO "PosetMap"@ was created.
            
            To obtain the monomial poset of a ring, one can use @TO "getPoset"@, 
            a generalization of @TO "standardMonomialPoset"@ from the 
            @TO2(Posets, "Posets package")@ to ideals which are not necessarily 
            monomial.
            
            This package was used in some of the work leading to the paper 
            @HREF{"https://arxiv.org/abs/2502.15166"}@, where many of these 
            operations and properties are discussed.
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
        getMons I
        getMons S
        getMons R
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
            Given a polynomial ring R and a homogeneous ideal of R (an ideal 
            generated by homogeneous elements of R) called I, the function 
            getMons gets the monomials of the quotient ring R/I.
        Example
            R = QQ[x,y]
            I = ideal(x^3, y^3)
            getMons(R, I)

            J = ideal(x*y - y^2, x^4, x^3 * y, x^2 * y^2)
            getMons(R / J)
        Text
            For quotient rings with infinitely many monomials, the optional 
            argument MaxDegree can be used to calculate monomials up to a 
            certain degree. If this is left blank, the default value used will 
            be 10. 
        Example
            I = ideal(x^2 - y^2)
            getMons(R/I, MaxDegree=>10)
    Caveat
        The optional argument MaxDegree is only meant to be used for quotient 
        rings where there are infinitely many monomials. For quotient rings with 
        finitely many monomials, MaxDegree will be ignored regardless of it's value.
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
         getPoset I
         getPoset S
         getPoset R
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
            Given a polynomial ring R and a homogeneous ideal of R (an ideal 
            generated by homogeneous elements of R) called I, the monomial 
            poset of R/I  is the collection of @TO2(getMons, "monomials of R/I")@ 
            equipped with the divisibility partial order.
        Example
            R = QQ[x,y]
            I = ideal(x*y - y^2, x^4, x^3*y, x^2*y^2)
            getPoset(R, I) 
        Text
            For posets with infinitely many monomials, the optional argument 
            MaxDegree can be used to calculate monomials up to a certain degree 
            to use in the poset. If this is left blank, the default value used 
            will be 10.
        Example
            J = ideal(x^2 - y^2)
            getPoset(R, J, MaxDegree => 15)    
    Caveat
        Like the function getMons, the optional argument MaxDegree is only meant 
        to be used for quotient rings where there are infinitely many monomials. 
        For quotient rings with finitely many monomials, MaxDegree will be ignored 
        regardless of it's value.
    SeeAlso
        getMons
Node
    Key
        PosetMap
    Headline
        the class of all morphisms between posets
    Description
        Text
            Poset maps are used to obtain @TO2(posetFiberProduct, "fiber products")@ 
            and @TO2(posetConnectedSum, "connected sums")@ of @TO2(Poset, "posets")@.
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
        image(f, L)
    Inputs
        f:PosetMap
        L:List
            a subset of @TT "vertices source f"@
    Outputs
        :List
    Description
        Text
            The image of the map $[3]\rightarrow[10]$ given by $i\mapsto 2i$ is 
            computed below.
        Example
            f = map(chain 10, chain 3, {1=>2, 2=>4, 3=>6})
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
            A @TO2(PosetMap, "poset map")@ $f: P\rightarrow Q$ between 
            @TO2(isRanked, "ranked")@ posets is rank-preserving if and only if, 
            for each $p \in P$, the rank of $p$ is equal to the rank of $f(p)$, 
            assuming both $P$ and $Q$ have a rank $0$ element.
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
        (posetWedgeProduct, List)
        (posetWedgeProduct, Poset, Poset)
    Headline 
        constructs the wedge product of several posets
    Usage
        posetWedgeProduct L
    Inputs
        L:List
            a list of @TO2(Poset, "posets")@
        P:Poset
        Q:Poset
    Outputs
        :Poset
    Description
        Text
            Given a list L of posets all with unique minimal elements, the 
            function returns the wedge product of all the posets. The wedge 
            product is defined as follows: 
            Suppose that for $1\leq i\leq t$ we have posets $P_i$ with unique 
            least element $\ell_i$. Their $\bf{wedge product}$ is the set:
            \[ P_1 \vee P_2 \vee \cdots \vee  P_t = \left(\bigsqcup_{i=1}^t P_i \right)/ (\ell_1=\ell_2=\cdots  =\ell_t), \]
            (meaning that we take the disjoint union of the sets $P_i$ in which 
            we identify all the $\ell_i$ into one element) with the partial order 
            $a \leq b$ if and only if $a\leq b$ in $P_i$ for some $i$.
            
            This is the special case of @TO "posetFiberProduct"@ where the 
            domain of the map is a $1$-vertex poset.
        Example
            R = QQ[x,y]
            I = ideal(x^3, y^2)
            posetWedgeProduct {booleanLattice 3, chain 4, getPoset(R/I)}
    SeeAlso
        ringFiberProduct
        posetFiberProduct
        posetClosedProduct
Node
    Key
        posetFiberProduct
        (posetFiberProduct, List)
        (posetFiberProduct, PosetMap, PosetMap)
    Headline
        constructs the fiber product of several posets
    Usage
        posetFiberProduct L
        posetFiberProduct(f, g)
    Inputs
        L:List
            a list of rank-preserving injective @TO PosetMap@s.
        f:PosetMap
        g:PosetMap
    Outputs
        :Poset
    Description
        Text
            In this construction, several posets are glued together along a 
            common subposet. Suppose $(A, \leq_A), (B, \leq_B), (C, \leq_C)$ 
            are posets and $\iota_A: C \rightarrow A$ and $\iota_B: C \rightarrow B$ 
            are rank-preserving, injective, and monotone. Then, we have a fiber 
            product \[A \times_C B = (A \setminus \iota_A(C)) \sqcup (B \setminus \iota_B(C)) \sqcup C\] 
            with $a \geq c$ iff $a \geq_A \iota_A(c)$ for $a \in A$ and $c \in C$, 
            and $b \geq c$ iff $b \geq_B \iota_A(c)$ for $b \in A$ and $c \in C$.
            
            This is a generalization of @TO "posetWedgeProduct"@.
        Example
            P = product(chain 2, chain 2)
            Q = product(chain 2, chain 3)
            f = map(Q, P, {{1,1}, {1,2}, {2,1}, {2,2}})
            g = map(Q, P, {{1,1}, {2,1}, {1,2}, {2,2}})

            R = (ZZ/2)[x,y]
            I = monomialIdeal(x^3, x^2 * y^2, y^3)
            areIsomorphic(posetFiberProduct(f, g), getPoset(R/I))

            chainWedgePoset = product(chain 2, adjoinMin posetWedgeProduct(chain 2, chain 2))
            areIsomorphic(posetFiberProduct(f, f), chainWedgePoset)
    SeeAlso
        posetWedgeProduct
        posetConnectedSum
Node
    Key
        posetClosedProduct
        (posetClosedProduct, List)
        (posetClosedProduct, Poset, Poset)
    Headline 
        constructs the closed product of several posets
    Usage
        posetClosedProduct L
        posetClosedProduct(P, Q)
    Inputs
        L:List
            a list of posets
        P:Poset
        Q:Poset
    Outputs
        :Poset
    Description
        Text
            Given a list of posets that all have a unique minimal and maximal 
            element, the function returns the closed product of all the posets. 
            The closed product is defined as follows:
            Suppose that for $1 \leq i \leq t$ we have posets $P_i$ with unique 
            least element $\ell_i$ and unique largest element $L_i$. Their 
            $\bf{closed product}$ is the set:
            \[ P_1 \diamond P_2 \diamond \cdots \diamond  P_t = \left( \bigsqcup_{i=1}^t P_i \right) / (\ell_1 = \ell_2 = \cdots = \ell_t, L_1 = L_2 = \cdots = L_t ), \] 
            (meaning that we take the disjoint union of the sets $P_i$ in which 
            we identify all the $\ell_i$ into one element and all the $L_i$ 
            elements into one element) with the partial order $a \leq b$ if and 
            only if $a \leq b$ in $P_i$ for some $i$.
            
            This is the special case of @TO "posetConnectedSum"@ where the 
            domain of the map is a $1$-vertex poset.
        Example
            R = QQ[x,y]
            I = ideal(x^2, y^2)
            posetClosedProduct(chain 3, getPoset(R/I))
    SeeAlso
        ringConnectedSum
        posetConnectedSum
        posetWedgeProduct
Node
    Key
        posetConnectedSum
        (posetConnectedSum, List)
        (posetConnectedSum, PosetMap, PosetMap)
    Headline
        constructs the connected sum of several posets
    Usage
        posetConnectedSum L
        posetConnectedSum(f, g)
    Inputs
        L:List
            of rank-preserving injective @TO PosetMap@s.
        f:PosetMap
        g:PosetMap
    Outputs
        :Poset
    Description
        Text
            In this construction, several posets are glued together at the top 
            and at the bottom. Suppose $A,B$ are self-dual posets, $C$ is a 
            poset, and $i_A: C \rightarrow A$ and $i_B: C \rightarrow B$ are 
            rank-preserving, injective @TO2(PosetMap, "poset maps")@. Let 
            $d_A: A \rightarrow A^{\operatorname{op}}$ and 
            $d_B: B \rightarrow B^{\operatorname{op}}$ be the dual isomorphisms. 
            The connected sum of $A$ and $B$ with respect to $i_A, i_B$ is the 
            poset obtained by taking the disjoint union of $A$ and $B$, 
            identifying the image of $i_A$ with the image of $i_B$, and 
            identifying the image of $d_A \circ i_A$ with the image of $d_B \circ i_B$. 
            This construction can also be generalized to connected sums of more 
            than two posets.
            
            This is a generalization of @TO "posetClosedProduct"@.
            
            Here is the connected sum of the chain $[5]$ with itself with 
            respect to the inclusion map $[2]\hookrightarrow[5]$ with 
            $1 \mapsto 1$ and $2 \mapsto 2$.
        Example
            i = map(chain 5, chain 2, {1=>1, 2=>2})
            A = posetConnectedSum(i, i)
            areIsomorphic( A, adjoinMin adjoinMax product(chain 2, chain 2) )
        Text    
            Let $P = [2] \times [2]$, let $Q = [2] \times [5]$. There are two 
            different connected sums over $P$ of $Q$ with itself. The map 
            $f: P \rightarrow Q$ is the inclusion map with $(1,2) \mapsto (1,2)$ 
            and $(2,1) \mapsto (2,1)$. The map $g: P \rightarrow Q$ is the other 
            rank-preserving injection, with $(1,2) \mapsto (2,1)$ and $(2,1) \mapsto (1,2)$.
        Example
            P = product(chain 2, chain 2)
            Q = product(chain 2, chain 5)
            
            f = map(Q, P, {{1,1}, {1,2}, {2,1}, {2,2}})
            g = map(Q, P, {{1,1}, {2,1}, {1,2}, {2,2}})
            
            R = posetConnectedSum(f, g)
            S = posetConnectedSum(f, f)
            
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
        ringFiberProduct(I, J)
        ringFiberProduct(S, T)
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
            Suppose we have rings $S_1 = R_1/I_1$ and $S_2 = R_2/I_2$ for some 
            homogeneous ideals $I_1$ of $R_1 = K[x_1, \dots, x_n]$ and $I_2$ of 
            $R_2 = K[y_1, \dots, y_m]$, where $K$ is a field. Their fiber product 
            over $K$ is the ring: 
            \[ S_1 \times_K S_2 = K[x_1, \dots, x_n, y_1, \dots, y_m] / (I_1 + I_2 + (x_i y_j : 1 \leq i \leq n, 1 \leq j \leq m )) \]
        Example
            R = QQ[x,y]
            I = ideal(x^2, y^2)

            S = QQ[a,b]
            J = ideal(a^4, b^4)
            ringFiberProduct(R/I, S/J)
        Example
            R = (ZZ/7)[x,y,z]
            I = ideal(x^2, y^3, z^4)

            S = (ZZ/7)[a,b,c]
            J = ideal(a, b^3, c^3)
            ringFiberProduct(R/I, S/J)
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
            Suppose we have Gorenstein rings $S_1 = R_1/I_1$ and $S_2 = R_2/I_2$ 
            for some homogeneous ideals $I_1$ of $R_1 = K[x_1, \dots, x_n]$ and 
            $I_2$ of $R_2 = K[y_1, \dots, y_m]$, where $K$ is a field. Let $m_1, m_2$ 
            be the maximal elements of the @TO2(getMons, "monomial posets")@ of 
            $S_1, S_2$, respectively. The connected sum of $S_1$ and $S_2$ is their 
            @TO2(ringFiberProduct, "fiber product")@ mod $m_1-m_2$. In symbols, 
            $S_1 \# S_2 = \frac{S_1 \times_K S_2}{(m_1-m_2)}$.
        Example
            R = (ZZ/2)[u,v,w]
            I = ideal(u^4, v^2-w^2, w^2-v*u)
            S = R/I

            T = newRing(S, Variables=>{x,y,z})
            U = ringConnectedSum(S, T)
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
        map(Q, P, L)
    Inputs
        P:Poset
            the source poset
        Q:Poset
            the target poset
        L:List
            of length @TT "#(vertices P)"@ and whose $i$th entry specifies the image of the $i$th element of @TT "P"@, or a @TO "List"@ of @TO "Option"@s whose keys are @TT "vertices P"@ and whose values are in @TT "vertices Q"@, or a @TO "HashTable"@ whose keys are @TT "vertices P"@ and whose values are in @TT "vertices Q"@
    Outputs
        :PosetMap
            the poset map from @TT "P"@ to @TT "Q"@ specified by @TT "L"@.
    Description
        Text
            A poset map is a monotone function $f: P\rightarrow Q$ from a poset 
            $P$ to a poset $Q$, so $f(p)\leq f(q)$ whenever $p\leq q$.
            
            Let $P = [2]$ and $Q = [3] \times [3]$. Here are the three ways to 
            obtain the map $\phi: P \rightarrow Q$ given by $\phi(1) = (1,1)$ and 
            $\phi(2) = (1,2)$.
        Example
            P = chain 2
            Q = product(chain 3, chain 3)
            
            f = map(Q, P, {{1,1}, {1,2}})
            g = map(Q, P, {1=>{1,1}, 2=>{1,2}})
            h = map(Q, P, hashTable {1=>{1,1}, 2=>{1,2}})
            
            f == g
            g == h
        Text
            Potentially, some of the vertices of the target may be @TO2(Option, "options")@. 
            When there is ambiguity, the list elements will be taken as vertices 
            of the target as in the first of the three ways above.
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
        f:PosetMap
        g:PosetMap
    Outputs
        :PosetMap
            the composition $g \circ f$
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
            If $S$ is a subset of a poset, then the upper shadow of $S$ is the 
            set of all elements of the poset which cover some element of $S$.
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
            If $S$ is a subset of a poset, then the lower shadow of $S$ is the 
            set of all elements of the poset which cover some element of $S$.
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
        TikZ=>Boolean
        Visual=>Boolean
        AllOrders=>Boolean
    Outputs
        :List
            whose elements are lists representing orders with respect to which the poset is Macaulay
    Description
        Text
            Given a poset with rank function $r$, this method returns all 
            Macaulay orders $<$ on the poset such that $r(p) < r(q)$ implies 
            $p < q$.
            
            Given a quotient $S$ of a polynomial ring, this method returns 
            Macaulay orders on the monomial poset of $S$.
            
            A total order $<$ on a poset $P$ is represented by a @TO "list"@. 
            It is the permutation of the of the ground set of $P$ that is 
            increasing with respect to $<$.
        Example
            macaulayOrders(product(chain 3, chain 3))
        Text
            The option @TO "TikZ"@ can print a Hasse diagram of the poset for 
            each order $<$ such that the vertices in each level are ordered left 
            to right according to $<$.
        Example
            R = (ZZ/2)[x,y]
            I = ideal(x^4, x^3-y^3, y^4)
            macaulayOrders(R/I, TikZ=>true)
    Caveat
        Given a @TO "QuotientRing"@ or an @TO "Ideal"@, this method will not 
        verify level linear independence.
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
        TikZ=>Boolean
        Visual=>Boolean
    Outputs
        :Boolean
    Description
        Text
            Suppose $P$ is a ranked poset and $<$ is a total order on the ground 
            set of $P$. Let $\operatorname{Seg}_d n$ denote the largest $n$ 
            elements of rank $d$ with respect to $<$. Suppose that for every 
            integer $d$ between $0$ and the rank of $P$, and for every subset $A$ 
            of the $d$th level of $P$, we have $\lvert\nabla_P\operatorname{Seg}_d\lvert A\rvert\rvert \leq \lvert\nabla_P(A)\rvert$ 
            and $\nabla_P\operatorname{Seg}_d\lvert A\rvert = \operatorname{Seg}_{d+1}\lvert\nabla_P(A)\rvert$. 
            Then, we say $P$ is Macaulay with respect to $<$. A Macaulay poset is 
            a poset for which there exists an order with respect to which it is 
            Macaulay.
            
            There is an analogous property for rings, not to be confused with 
            the Cohen-Macaulay property.
            
            Products of chains are Macaulay by the Clements-Lindstrom Theorem.
        Example
            isMacaulay booleanLattice 3

            R = (ZZ/2)[x,y,z]
            I = monomialIdeal(x^2, y^3, z^3)
            isMacaulay(R/I)
        Text
            It is possible for a monomial poset to not be Macaulay.
        Example
            S = (ZZ/2)[w,x,y,z]
            J = monomialIdeal(w^4, x^2, y^2, z^2, x*w, y*w, z*w)
            isMacaulay(S/J)
        Text
            If the poset is Macaulay, the option @TO "TikZ"@ can provide a Hasse 
            diagram in which the vertices are horizontally ordered according to 
            a Macaulay order.
        Example
            R = (ZZ/2)[x,y]
            I = monomialIdeal(x^2, y^3)
            isMacaulay(R/I, TikZ=>true)
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
    Outputs
        :Boolean
    Description
        Text
            See section 3.2 of @HREF "https://arxiv.org/pdf/2502.15166"@ for a 
            definition of additivity.
            
            Boolean lattices are additive. Therefore, the 
            @TO2(posetWedgeProduct, "wedge product")@ of a Boolean lattice with 
            itself is Macaulay.
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
            For getMons, the function starts at the lowest degree 0 and starts 
            working out the monomials of 0, 1, 2, ... until the number given.
        Example
            R = QQ[x,y]
            I = ideal(x^2 - y^2)
            getMons(R, I, MaxDegree=>15)
    SeeAlso
        getMons
        getPoset
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
            Suppose the poset is Macaulay with respect to a total order $<$. If 
            two vertices $p$ and $q$ have the same rank, then $p<q$ if and only 
            if $p$ is to the left of $q$ in the Hasse diagram drawn with 
            @TT "TikZ=>true"@. For @TO macaulayOrders@ with @TT "AllOrders=>true"@, 
            there is a Hasse diagram for each order. For @TO isMacaulay@ and for 
            @TO macaulayOrders@ with @TT "AllOrders=>false"@, there is at most 
            one Hasse diagram.
        Example
            R = QQ[x,y]
            I = monomialIdeal(x^3, x^2*y^2, y^3)
            macaulayOrders(standardMonomialPoset I, TikZ=>true)
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
            If this option is set to a @TO String@ or to the @TO Boolean@ 
            @TT "true"@, then the method will use @TO Visualize@ to create a 
            visualization of the poset, and an additional visualization for 
            each Macaulay order with the vertices labeled with integers 
            according to the order. If set to a @TO String@, that port will 
            be used. Otherwise, port "8080" will be used.
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
            With @TT "AllOrders=>false"@, the list returned by the method will 
            have at most one element.
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
    assert(f != g)
///

-- isMacaulay
TEST ///
    KK = ZZ/2;
    
    S = KK[x,y,z]/monomialIdeal(x^2,y^3,z^2,x*y,y*z,z*x)
    assert isMacaulay S
    -- assert not isMacaulay tensor(S, S)
    
    -- figure 5 in an upcoming revision to https://arxiv.org/abs/2502.15166
    P = chain 2;
    Q = adjoinMin posetWedgeProduct(chain 2, chain 2);
    assert isMacaulay P
    assert isMacaulay Q
    assert not isMacaulay product(P, Q)
    
    -- figure 6 in an upcoming revision to https://arxiv.org/abs/2502.15166
    R = KK[x]/monomialIdeal(x^2)
    S = KK[y,z]/(y^2-z^2,y^3,z^3)
    assert isMacaulay R
    assert isMacaulay S
    assert not isMacaulay tensor(R, S)
    
    -- figure 7 in an upcoming revision to https://arxiv.org/abs/2502.15166
    S = KK[y,z]/monomialIdeal(y^3,y^2*z,y*z^2,z^3)
    T = KK[x]/monomialIdeal(x^2)
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
    
    assert(#getMons(QQ[x, y], ideal(x^2 - y^2), MaxDegree => 15) == 31)
    
    x = R_0
    y = R_1
    

    computedP = getPoset(R, ideal(x*y - y^2, x^4, x^3*y, x^2*y^2))
    actualP = poset({1, x, x^2, x^3, y, y^2, y^3}, {{1, x}, {1, y}, {x, x^2}, {x, y^2}, {x^2, x^3}, {x^2, y^3}, {y, y^2}, {y^2, y^3}})
    assert(areIsomorphic(computedP, actualP))
///

-- Tests for the poset operations
TEST ///
    -- posetWedgeProduct
    assert areIsomorphic(posetWedgeProduct(chain 2, chain 2), adjoinMin poset({1,2}, {}))

    -- posetClosedProduct
    assert areIsomorphic(posetClosedProduct(chain 3, chain 3), product(chain 2, chain 2))

    -- posetFiberProduct
    P = product(chain 2, chain 2)
    Q = product(chain 2, chain 3)

    f = map(Q, P, {{1,1}, {1,2}, {2,1}, {2,2}})
    g = map(Q, P, {{1,1}, {2,1}, {1,2}, {2,2}})

    assert areIsomorphic(posetFiberProduct(f, g), getPoset(ZZ/2[x,y]/monomialIdeal(x^3,x^2*y^2,y^3)))
    assert areIsomorphic(posetFiberProduct(f, f), product(chain 2, adjoinMin posetWedgeProduct(chain 2, chain 2)))

    -- posetConnectedSum
    i = map(chain 5, chain 2, {1=>1, 2=>2})
    assert areIsomorphic(posetConnectedSum(i, i), adjoinMin adjoinMax product(chain 2, chain 2))
///

-- Tests for the ring operations
TEST ///
    -- ringFiberProduct
    S = QQ[x,y] / ideal(x^2, y^2);
    P = getPoset S;
    assert(areIsomorphic(posetWedgeProduct(P, P), getPoset ringFiberProduct(S, S)))

    KK = ZZ/7
    S = KK[a,b,c] / ideal(a^3, b^3, c^3);
    T = KK[x,y,z] / ideal(x^4, y^2, z);
    assert(areIsomorphic(posetWedgeProduct(getPoset S, getPoset T), getPoset ringFiberProduct(S, T)))

    -- ringConnectedSum
    KK = ZZ/2;
    P = poset({0,1,2,3,4,5}, {{0,1},{0,2},{1,3},{2,4},{3,5},{4,5}});
    S = KK[x] / monomialIdeal(x^4);
    connectedSumPoset = getPoset ringConnectedSum(S, S);
    assert(areIsomorphic(connectedSumPoset, P));

    R = KK[x,y];
    S = R / monomialIdeal(x^3, y^3);
    use R;
    T = R / monomialIdeal(x^2, y^4);
    connectedSumPoset = getPoset ringConnectedSum(S, T);
    closedProductPoset = posetClosedProduct(product(chain 3, chain 3), product(chain 2, chain 4));
    assert(areIsomorphic(connectedSumPoset, closedProductPoset));
///

-- Some wedge and closed product tests.
TEST ///
    R = QQ[x,y,z];
    I = ideal(x*y, x*z, y^2 - y*z, y^2 - z^2, x^3 - y^3)
    P = getPoset(R/I);
    assert(isMacaulay posetClosedProduct(P, P));
    assert(not isMacaulay posetWedgeProduct(P, P))
///


TEST ///
    -- Theorem A in https://arxiv.org/abs/2502.15166
    P = getPoset(QQ[x, y]/ideal(x^2, y^2));
    assert (isAdditive P)
    assert (isAdditive P == isMacaulay posetWedgeProduct(P, P) == isMacaulay(posetClosedProduct(P, P)))
    
    R = QQ[x,y]
    I = ideal(x^4, y^4)
    P = getPoset(R/I);
    assert(isMacaulay posetClosedProduct {P, P, P})
    
    -- Two assertions for Theorem B in https://arxiv.org/abs/2502.15166
    R = QQ[x,y,z]
    I = ideal(x^2, y^2, z^2)
    P = getPoset(R/I);
    assert(not isMacaulay posetClosedProduct(P, chain 4))

    R = QQ[x,y]
    I = ideal(x^2, y^3)
    assert(not isMacaulay posetClosedProduct(P, getPoset(R/I)))
///


TEST ///
    assert(null == try getPoset(QQ[x, y]/ideal(x^3 - y^2)))
    
    assert(isMacaulay posetWedgeProduct {chain 5, chain 4, chain 10})
    assert(isMacaulay posetClosedProduct {chain 5, chain 5, chain 5})
    
    P27 = booleanLattice 2;
    P27Top = adjoinMax(P27, "newMax");
    P27Bottom = adjoinMin(P27, "newMin");

    assert(isMacaulay posetClosedProduct(P27Top, P27Bottom))
    assert(not isMacaulay posetWedgeProduct(P27Top, P27Bottom))
    
    assert(isMacaulay posetWedgeProduct(chain 2, posetClosedProduct(chain 2, chain 2)))
///

TEST ///
    S = QQ[x, y, z] / ideal(x^2, y^2, z^2)
    T = QQ[w] / ideal(w^4)
    assert(not isMacaulay ringFiberProduct(S, T))

    S = QQ[x, y, z] / ideal(x^2, y^2, z^2)
    T = QQ[v, w] / ideal(v^2, w^3)
    assert(not isMacaulay ringFiberProduct(S, T))

    S = ZZ/5[x, y]/ideal(x^3, y^3)
    T = ZZ/5[z] / ideal(z^5)
    assert(not isMacaulay ringFiberProduct(S, T))
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
    R = QQ[x,y,z,w]
    I = monomialIdeal(w^3,x*w^2,x^3, y^3,z*y^2,z^3, w*y,w*z,x*y,x*z)
    assert not isMacaulay(R/I)
///

TEST ///
    -- figure 3 in https://arxiv.org/abs/2510.22843
    R = QQ[x,y]
    I = ideal(x^6,x^3*y,y^4,x^2*y^3-x^5)
    assert isMacaulay(R/I)
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
    R = QQ[x,y,z]
    for i from 1 to 4 do (
        for j from i to 4 do (
            for k from j to 4 do (
                I = monomialIdeal(x^i,y^j,z^k, x*y,y*z,z*x);
                assert isMacaulay(R/I);
            );
        );
    );
///

-- macaulayOrders for a poset in which every Macaulay order has the same restrictions to each level
TEST ///
    P = poset({0,1,2,3,4,5,6,7,8,9,10},{{0,1},{0,2},{1,3},{1,4},{2,5},{3,6},{3,7},{4,8},{5,9},{6,10}});

    for macaulayOrder in macaulayOrders P do (
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
    for macaulayOrder in macaulayOrders P do (
        xFound = false;
        yFound = false;
        
        xLy = false;
        for posetElement in macaulayOrder do (
            xFound = xFound or x == posetElement;
            yFound = yFound or y == posetElement;
            
            if xFound and not yFound then (
                xLy = true;
                break;
            );
            if yFound and not xFound then (
                xLy = false;
                break;
            );
        );
        
        x2found = false;
        y2found = false;
        xyFound = false;
        for posetElement in macaulayOrder do (
            x2found = x2found or x^2 == posetElement;
            y2found = y2found or y^2 == posetElement;
            xyFound = xyFound or x*y == posetElement;
            
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
                assert(#(macaulayOrders(AllOrders=>false, product(chain i, product(chain j, chain k)))) == 1);
            );
        );
    );
///

TEST ///
    assert isAdditive booleanLattice 3
    assert not isAdditive poset({1,2,3,4,5,6}, {{1,2},{1,3},{2,4},{3,4},{3,5},{3,6}})
///

end