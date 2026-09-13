bottPython = method();
bottPython HomogeneousVectorBundle := E -> (
    X := E#"underlyingVariety";
    R := X#"rootSystem";
    rho := toPython entries halfSumOfRoots(R);
    posRoots := elements positiveRoots(R);
    rootNorms := R#((keys R)#1);
    sRoots := toList (1..rank(R))/(i -> simpleRoot(R,i));
    M := R#((keys R)#0);
    fractions := import "fractions";
    Fraction := (fractions)@@("Fraction");
    S := toPython (E#"weights"/(s -> entries s));
    m := toPython E#"multiplicities";
    d := toPython dim(X);
    posRoots = toPython (posRoots/(r -> entries r));
    simpleRoots := toPython (sRoots/(r -> entries r));
    M = entries M/(r -> r/(p -> (Fraction numerator p) / (Fraction denominator p)));
    M = toPython M;
    rootNorms = toPython rootNorms;
    bottP := import "bott_py";
    --bottPy := symbol bottPy;
    result := bottP@@("bottPy")(S, m, rho,simpleRoots, posRoots, rootNorms, M, d);
    E#"cohomology" = toList(value result)
    );

bott = method(); 
bott HomogeneousVectorBundle := E -> (
    if E#"cohomology" =!= null then return E#"cohomology";
    X := E#"underlyingVariety";
    R := X#"rootSystem";
    ro := halfSumOfRoots(R);
    d := dim(X);
    H := apply(d+1, i -> 0);
    H = new MutableList from H;
    S := E#"weights";
    m := E#"multiplicities";
    for i from 0 to #S-1 do (
        l := weight(R, entries S#i); 
        if not isSingular(l+ro,R) then (
            (l1,q) := dominantConjugate(l+ro,R);
            h := weylFormula(l1-ro,R);
            H#q = H#q + m#i * h;
            );
        );
    E#"cohomology" = toList(H)
    );


cohomology FiltrationBundle := o -> E -> (
    if E#"cohomology" =!= null then return (E#"cohomology",new HashTable from {});
    if E#"totally reducible" then return (bottPython E,new HashTable from {});
    F := reverse E#"factors";
    H1 := bottPython F#0;
    H2 := bottPython F#1;
    if (bottPython(F#0 * dual F#1))#1 =!= 0 then (
        H := centralMemberCohomology(H1,H2,new HashTable from {},new HashTable from {});
        )
    else H = (H1 + H2,new HashTable from {}); -- if the ses splits the cohomology are known
    -- start with the nested ses
    for i from 2 to #F-1 do (
        H = centralMemberCohomology(H#0,bottPython F#i,H#1,new HashTable from {});
        );
    R := (E#"underlyingVariety")#"rootSystem";
    d := dim(E#"underlyingVariety");
    ro := halfSumOfRoots(R);
    M := new MutableList;
    i := 0;
    for l in E#"weights" do (
        if not isSingular(l+ro,R) then (
            M#i = (dominantConjugate(l+ro,R))#1;
            i = i + 1;
            );
        );
    M = set(0..d) - set(toList M);
    H' := new MutableList from H#0;
    for m in toList M do (H')#m = sub(0,ring (H#0)_0);
    E#"cohomology" = toList H'; 
    E#"cohomologyBounds" = H#1;
    (E#"cohomology", E#"cohomologyBounds")
    );

-- this method is written just to allow to compute cohomology using the shortcut HH
homology FiltrationBundle := o -> E -> (
    if E#"cohomology" =!= null then return (E#"cohomology",new HashTable from {});
    if E#"totally reducible" then return (bottPython E,new HashTable from {});
    F := reverse E#"factors";
    H1 := bottPython F#0;
    H2 := bottPython F#1;
    if ((bottPython(F#0 * dual F#1))#0)#1 =!= 0 then (
        H := centralMemberCohomology(H1,H2,new HashTable from {},new HashTable from {});
        )
    else H = (H1 + H2,new HashTable from {}); -- if the ses splits the cohomology are known
    -- start with the nested ses
    for i from 2 to #F-1 do (
        H = centralMemberCohomology(H#0,bottPython F#i,H#1,new HashTable from {});
        );
    R := (E#"underlyingVariety")#"rootSystem";
    d := dim(E#"underlyingVariety");
    ro := halfSumOfRoots(R);
    M := new MutableList;
    i := 0;
    for l in E#"weights" do (
        if not isSingular(l+ro,R) then (
            M#i = (dominantConjugate(l+ro,R))#1;
            i = i + 1;
            );
        );
    M = set(0..d) - set(toList M);
    H' := new MutableList from H#0;
    for m in toList M do (H')#m = sub(0,ring (H#0)_0);
    E#"cohomology" = toList H'; 
    E#"cohomologyBounds" = H#1;
    (E#"cohomology", E#"cohomologyBounds")
    );


eulerCharacteristic = method();
eulerCharacteristic FiltrationBundle := E -> (
    if E#"eulerCharacteristic" =!= null then return E#"eulerCharacteristic";
    H := (cohomology E)#0;
    c := 0;
    for i from 0 to #H-1 do (
        c = c + ((-1)^i) * H#i;
        );
    E#"eulerCharacteristic" = lift(c,ZZ)
    );

chi FiltrationBundle := E -> eulerCharacteristic(E)


-- compute the cohomology of a homogeneous bundle restricted to a subvariety
cohomologyRestriction = method();
cohomologyRestriction (HomogeneousVariety,HomogeneousVectorBundle,FiltrationBundle) := (X,F,G) -> (
    K := reverse koszul(X,F);
    tK := apply(K, f -> tensorProduct(f,G));
    cohom := apply(tK, f -> first cohomology f);
    A := cohom#0;
    B := cohom#1;
    oldBoundsA := new HashTable from {};
    oldBoundsB := new HashTable from {};
    C := cokerCohomology (A,B,oldBoundsA,oldBoundsB), bounds := C#1, C = C#0;
    for k from 2 to #cohom -1 do (
        A = C;
        oldBoundsA = bounds;
        B = cohom#k;
        C = cokerCohomology (A,B,oldBoundsA,oldBoundsB), bounds = C#1, C = C#0;
        );
    C
    )

cohomologyRestriction (EmbeddedVariety,FiltrationBundle) := (Y,G) -> cohomologyRestriction(Y#"ambientSpace",Y#"normalBundle",G)


eulerCharacteristic (HomogeneousVariety,HomogeneousVectorBundle,FiltrationBundle) := (X,F,G) -> (
    K := koszul(X,F);
    tK := apply(K, f -> eulerCharacteristic tensorProduct(f,G));
    c := 0;
    for i from 0 to #tK-1 do (
        c = c + ((-1)^i) * tK#i;
        );
    return c;
    )

chi (HomogeneousVariety,HomogeneousVectorBundle,FiltrationBundle) := (X,F,G) -> eulerCharacteristic(X,F,G)

eulerCharacteristic (EmbeddedVariety,FiltrationBundle) := (Y,G) -> eulerCharacteristic(Y#"ambientSpace",Y#"normalBundle",G)

chi (EmbeddedVariety,FiltrationBundle) := (Y,G) -> eulerCharacteristic(Y,G)

-- chi(Y,Omega^p_Y)
eulerCharacteristicCotangent = method();
eulerCharacteristicCotangent (ZZ,EmbeddedVariety) := (p,Y) -> (
    if Y#"cohomology" =!= null then return lift(sum apply(#((Y#"cohomology")#p), i -> ((-1)^i)*((Y#"cohomology")#p)#i),ZZ);
    X := Y#"ambientSpace";
    F := Y#"normalBundle";
    F' := dual F;
    cotangent := homogeneousCotangentBundle X;
    Vcotangent := multipleExteriorPower(p, cotangent);
    SymF' := multipleSymmetricPower(p, F');
    comp := compositions(2,p);
    L := for m in comp list (
        tensorProduct(SymF'#(m#0),Vcotangent#(m#1))
        );
    (sum apply(#L, i -> ((-1)^i)*eulerCharacteristic(X,F,(L)#i)))*(-1)^p
    )

chiCotangent = method();
chiCotangent  (ZZ,EmbeddedVariety) := (p,Y) -> eulerCharacteristicCotangent(p,Y)

-- chi(Y,A^pT_Y)
eulerCharacteristicTangent = method();
eulerCharacteristicTangent (ZZ,EmbeddedVariety) := (p,Y) -> (
    X := Y#"ambientSpace";
    F := Y#"normalBundle";
    cotangent := homogeneousCotangentBundle X;
    Vtangent := multipleExteriorPower(p, dual cotangent);
    SymF := multipleSymmetricPower(p, F);
    comp := compositions(2,p);
    L := for m in comp list (
        tensorProduct(SymF#(m#0),Vtangent#(m#1))
        );
    (sum apply(#L, i -> ((-1)^i)*eulerCharacteristic(X,F,(L)#i)))*(-1)^p
    )

chiTangent = method();
chiTangent  (ZZ,EmbeddedVariety) := (p,Y) -> eulerCharacteristicTangent(p,Y)

-- chi(Y,O_Y)
eulerCharacteristicStructure = method();
eulerCharacteristicStructure EmbeddedVariety := Y -> (
    X := Y#"ambientSpace";
    F := Y#"normalBundle";
    K := koszul(X,F);
    K = apply(K, f -> eulerCharacteristic f);
    c := 0;
    for i from 0 to #K-1 do (
        c = c + ((-1)^i) * K#i;
        );
    return c;
    )

chiStructure = method();
chiStructure EmbeddedVariety := Y -> eulerCharacteristicStructure Y

-- determine the cohomology of the cokernel C in the long exact cohomology sequence from a short exact sequence 0 -> A -> B -> C -> 0
-- the method does not check whether the given cohomology lists are compatible with the ses
-- in the code A and B are already the cohomology lists of the bundles
-- R is the ring common to all entries of A and B
cokerCohomology = method();
cokerCohomology(List, List,HashTable, HashTable) := (A, B, oldBoundsA, oldBoundsB) -> (
    -- create common ring: determine the already used generators
    old := join(generators ring A_0, generators ring B_0);
    x := symbol x;
    R := ZZ[old, x_#old..x_(#old + #A - 1)];
    -- redefine A, B and create the cokernel C
    oldAToR := map(R, ring A_0, apply(#(generators ring A_0), i -> (generators R)_i));
    oldBToR := map(R, ring B_0, apply(#(generators ring B_0), i -> (generators R)_(i + #(generators ring A_0))));
    A = apply(A, a -> oldAToR(a));
    B = apply(B, b -> oldBToR(b));
    C := (generators R)_{-#A..-1};
    -- save bounds on old variables
    oldBounds := new MutableHashTable;
    for v in keys oldBoundsA do (
        oldBounds#(oldAToR(v)) = new HashTable from {
            {"variableName", oldAToR(v)},
            {"min", ((oldBoundsA)#v)#"min"},
            {"max", ((oldBoundsA)#v)#"max"}
            };
        );
    for v in keys oldBoundsB do (
        oldBounds#(oldBToR(v)) = new HashTable from {
            {"variableName", oldBToR(v)},
            {"min", ((oldBoundsB)#v)#"min"},
            {"max", ((oldBoundsB)#v)#"max"}
            };
        );
    oldBounds = new HashTable from oldBounds; 
    -- long exact sequence: shuffle three lists
    les := mingle(A,B,C);  
    -- split into shorter sequences whenever a zero appears and determine linear equation from it
    equations := {};
    subsequence := {};
    notSolved := {}; 
    for v in join(les, {0}) do (
        -- continue building subsequence
        if not zero v then subsequence = append(subsequence, v);
        -- finished a (non-empty) subsequence so append and start again
        if zero v and #subsequence > 0 then (
            equations = append(equations, sum(apply(#subsequence, i -> (-1)^i * subsequence_i)));
            -- if the subsequence has more than one variable then we save it in notSolved
            if #(set subsequence * set C) > 1 then notSolved = notSolved | {subsequence};
            subsequence = {};
            );
        );
    toReturn := apply(C, e -> e % ideal equations);
    -- return the resulting coker in a trimmed ring
    appearingVars := select(unique flatten (toReturn / (c -> flatten entries first coefficients(c))), v -> (degree v)_0 > 0);
    y := symbol y;
    S := ZZ[y_0..y_(#appearingVars - 1)];
    substitutions := (for i from 0 to #appearingVars - 1 list appearingVars_i => y_i);
    gensC := generators ring C_0;
    substitutions = substitutions | apply(select(gensC, g -> not member(g, appearingVars)), g -> g => 20000);
    toReturn = toReturn / (v -> sub(v, substitutions));
    -- creates new bounds
    bounds := new MutableHashTable;
    for e in appearingVars do (
        bounds#(sub(e, substitutions)) = new MutableHashTable from {
            {"variableName", sub(e, substitutions)},
            {"min", 0},
            {"max", infinity}
            };
        );
    -- add the new bounds
    for l in notSolved do (
        -- case A#0 and B# =!= 0, beginning of the sequence
        if not isMember(l_0,C) and not isMember(l_1,C) then (
            if #l == 6 then (
                try (bounds#(sub(l_5,substitutions)))#"max" = myMax(l_4,oldBounds);
                --if (bounds#(substitute((l_4)_R,S)))#"max" == (bounds#(substitute((l_4)_R,S)))#"min" then rels = rels | {(bounds#(substitute((l_4)_R,S))) - (bounds#(substitute((l_4)_R,S)))#"max"};
                )
            else if #l%3 == 0 then (
                try (bounds#(sub(l_(-1),substitutions)))#"max" = myMax(l_(-2),oldBounds);
                -- could add a condition on the second-to-last unknown
                )
            else (
                try (bounds#(sub(l_(-2),substitutions)))#"min" = myMin(l_(-1),oldBounds);
                -- could add a condition on the second-to-last unknown
                );
            )   
        -- case in the middle of the long sequence
        else (
            if #l == 4 then (
                try (bounds#(sub((l_3),substitutions)))#"min" = max{0,myMin(l_2 - l_1,oldBounds)};
                try (bounds#(sub((l_3),substitutions)))#"max" = myMax(l_2,oldBounds);
                --if (bounds#(substitute((l_3)_R,S)))#"max" == (bounds#(substitute((l_3)_R,S)))#"min" then rels = rels | {(bounds#(substitute((l_3)_R,S))) - (bounds#(substitute((l_3)_R,S)))#"max"};
                )
            else if #l == 5 then (
                if isMember(l_4,C) then (
                    try (bounds#(sub((l_4),substitutions)))#"min" = max{0,myMin(l_3-l_2,oldBounds)};
                    try (bounds#(sub((l_4),substitutions)))#"max" = myMax(l_3,oldBounds);
                    --if (bounds#(substitute((l_4)_R,S)))#"max" == (bounds#(substitute((l_4)_R,S)))#"min" then rels = rels | {(bounds#(substitute((l_4)_R,S))) - (bounds#(substitute((l_4)_R,S)))#"max"};
                    )
                else (
                    try (bounds#(sub((l_3),substitutions)))#"min" = myMin(l_4,oldBounds);
                    try (bounds#(sub((l_3),substitutions)))#"max" = myMax(l_2 + l_4,oldBounds);
                    --if (bounds#(substitute((l_3)_R,S)))#"max" == (bounds#(substitute((l_3)_R,S)))#"min" then rels = rels | {(bounds#(substitute((l_3)_R,S))) - (bounds#(substitute((l_3)_R,S)))#"max"};
                    );
                )
            else if #l == 6 then (
                try (bounds#(sub((l_4),substitutions)))#"min" = max{0,myMin(l_5,oldBounds),myMin(l_5+l_3-l_2,oldBounds)};
                --if (bounds#(substitute((l_4)_R,S)))#"max" == (bounds#(substitute((l_4)_R,S)))#"min" then rels = rels | {(bounds#(substitute((l_4)_R,S))) - (bounds#(substitute((l_4)_R,S)))#"max"};
                )
            else if #l%3 == 1 then (
                try (bounds#(sub(l_(-1),substitutions)))#"max" = myMax(l_(-2),oldBounds);
                -- could add a condition on the second-to-last unknown
                )
            else if #l%3 == 2 then (
                if isMember(l_4,C) then (
                    try (bounds#(sub((l_(-1)),substitutions)))#"max" = myMax(l#(-2),oldBounds);
                    -- could add a condition on the second-to-last unknown
                    )
                else (
                    try (bounds#(sub((l_(-2)),substitutions)))#"min" = myMin(l#(-1),oldBounds);
                    -- could add a condition on the second-to-last unknown
                    );
                )
            else (
                try (bounds#(sub((l(-2)),substitutions)))#"min" = myMin(l#(-1),oldBounds);
                -- could add a condition on the second-to-last unknown
                );
            );
        );  
    bounds = new HashTable from (keys bounds / (v -> v => new HashTable from bounds#v));
    (toReturn, bounds)
    )

-- order among bounds
myMax = method();
myMax (RingElement, HashTable) := (p, oldBounds) -> (
    R := ring p;
    T := terms p;
    toReturn := 0;
    for t in T do (
        var := (flatten entries monomials t)#0;
        coeff := coefficient (var,t);
        if var == 1 then toReturn  = toReturn + coeff 
        else if coeff > 0 then toReturn  = toReturn + coeff * (oldBounds#var)#"max"
        else toReturn  = toReturn + coeff * (oldBounds#var)#"min";
        );
    toReturn
    )

-- order among bounds
myMin = method();
myMin (RingElement, HashTable) := (p, oldBounds) -> (
    R := ring p;
    T := terms p;
    toReturn := 0;
    for t in T do (
        var := (flatten entries monomials t)#0;
        coeff := coefficient (var,t);
        if var == 1 then toReturn  = toReturn + coeff 
        else if coeff > 0 then toReturn  = toReturn + coeff * (oldBounds#var)#"min"
        else toReturn  = toReturn + coeff * (oldBounds#var)#"max";
        );
    toReturn
    )



-- determine the cohomology of the central term C in the long exact cohomology sequence from a short exact sequence 0 -> A -> C -> B -> 0
-- in the code A and B are already the cohomology lists of the bundles
-- R is the ring common to all entries of A and B
centralMemberCohomology = method();
centralMemberCohomology(List, List,HashTable, HashTable) := (A, B, oldBoundsA, oldBoundsB) -> (
    -- create common ring: determine the already used generators
    old := join(generators ring A_0, generators ring B_0);
    x := symbol x;
    R := ZZ[old, x_#old..x_(#old + #A - 1)];
    -- redefine A, B and create the cokernel C
    oldAToR := map(R, ring A_0, apply(#(generators ring A_0), i -> (generators R)_i));
    oldBToR := map(R, ring B_0, apply(#(generators ring B_0), i -> (generators R)_(i + #(generators ring A_0))));
    A = apply(A, a -> oldAToR(a));
    B = apply(B, b -> oldBToR(b));
    C := (generators R)_{-#A..-1};  
    -- save the bounds on the old variables
    oldBounds := new MutableHashTable;
    for v in keys oldBoundsA do (
        oldBounds#(oldAToR(v)) = new HashTable from {
            {"variableName", oldAToR(v)},
            {"min", ((oldBoundsA)#v)#"min"},
            {"max", ((oldBoundsA)#v)#"max"}
            };
        );
    for v in keys oldBoundsB do (
        oldBounds#(oldBToR(v)) = new HashTable from {
            {"variableName", oldBToR(v)},
            {"min", ((oldBoundsB)#v)#"min"},
            {"max", ((oldBoundsB)#v)#"max"}
            };
        );
    oldBounds = new HashTable from oldBounds;
    -- long exact sequence: shuffle three lists
    les := mingle(A,C,B);
    -- split into shorter sequences whenever a zero appears and determine linear equation from it
    equations := {};
    subsequence := {};
    notSolved := {}; -- keeps track of the subsequences with unknowns
    for v in join(les, {0}) do (
        -- continue building subsequence
        if not zero v then subsequence = append(subsequence, v);
        -- finished a (non-empty) subsequence so append and start again
        if zero v and #subsequence > 0 then (
            equations = append(equations, sum(apply(#subsequence, i -> (-1)^i * subsequence_i)));
            -- if the subsequence has more than one unknown then we save it in notSolved
            if #(set subsequence * set C) > 1 then notSolved = notSolved | {subsequence};
            subsequence = {};
            );
        );
    toReturn := apply(C, e -> e % ideal equations);
    -- return the resulting coker in a trimmed ring
    appearingVars := select(unique flatten (toReturn / (c -> flatten entries first coefficients(c))), v -> (degree v)_0 > 0);
    y := symbol y;
    S := ZZ[y_0..y_(#appearingVars - 1)];
    substitutions := (for i from 0 to #appearingVars - 1 list appearingVars_i => y_i);
    gensC := generators ring C_0;
    substitutions = substitutions | apply(select(gensC, g -> not member(g, appearingVars)), g -> g => 20000);
    toReturn = toReturn / (v -> sub(v, substitutions));
    -- create the new bounds
    bounds := new MutableHashTable;
    for e in appearingVars do (
        bounds#(sub(e, substitutions)) = new MutableHashTable from {
            {"variableName", sub(e, substitutions)},
            {"min", 0},
            {"max", infinity}
            };
        );
    -- insert the information we have on the bounds
    for l in notSolved do (
        if #l == 4 then (
            try (bounds#(sub((l_3),substitutions)))#"min" = max{0,myMin(l_2 - l_1,oldBounds)};
            try (bounds#(sub((l_3),substitutions)))#"max" = myMax(l_2,oldBounds);
            --if (bounds#(substitute((l_3)_R,S)))#"max" == (bounds#(substitute((l_3)_R,S)))#"min" then rels = rels | {(bounds#(substitute((l_3)_R,S))) - (bounds#(substitute((l_3)_R,S)))#"max"};
            )
        else if #l == 5 then (
            if isMember(l_4,C) then (
                try (bounds#(sub((l_4),substitutions)))#"min" = max{0,myMin(l_3-l_2,oldBounds)};
                try (bounds#(sub((l_4),substitutions)))#"max" = myMax(l_3,oldBounds);
                --if (bounds#(substitute((l_4)_R,S)))#"max" == (bounds#(substitute((l_4)_R,S)))#"min" then rels = rels | {(bounds#(substitute((l_4)_R,S))) - (bounds#(substitute((l_4)_R,S)))#"max"};
                )
            else (
                try (bounds#(sub((l_3),substitutions)))#"min" = myMin(l_4,oldBounds);
                try (bounds#(sub((l_3),substitutions)))#"max" = myMax(l_2 + l_4,oldBounds);
                --if (bounds#(substitute((l_3)_R,S)))#"max" == (bounds#(substitute((l_3)_R,S)))#"min" then rels = rels | {(bounds#(substitute((l_3)_R,S))) - (bounds#(substitute((l_3)_R,S)))#"max"};
                );
            )
        else if #l == 6 then (
            try (bounds#(sub((l_4),substitutions)))#"min" = max{0,myMin(l_5,oldBounds),myMin(l_5+l_3-l_2,oldBounds)};
            --if (bounds#(substitute((l_4)_R,S)))#"max" == (bounds#(substitute((l_4)_R,S)))#"min" then rels = rels | {(bounds#(substitute((l_4)_R,S))) - (bounds#(substitute((l_4)_R,S)))#"max"};
            )
        else if #l%3 == 1 then (
            try (bounds#(sub(l_(-1),substitutions)))#"max" = myMax(l_(-2),oldBounds);
            -- could add a condition on the second-to-last unknown
            )
        else if #l%3 == 2 then (
            if isMember(l_4,C) then (
                try (bounds#(sub((l_(-1)),substitutions)))#"max" = myMax(l#(-2),oldBounds);
                -- could add a condition on the second-to-last unknown
                )
            else (
                try (bounds#(sub((l_(-2)),substitutions)))#"min" = myMin(l#(-1),oldBounds);
                -- could add a condition on the second-to-last unknown
                );
            )
        else (
            try (bounds#(sub((l(-2)),substitutions)))#"min" = myMin(l#(-1),oldBounds);
            -- could add a condition on the second-to-last unknown
            );
        );
    bounds = new HashTable from (keys bounds / (v -> v => new HashTable from bounds#v));
    (toReturn, bounds)    
    )



-- determine the cohomology of the kernel A in the long exact cohomology sequence from a short exact sequence 0 -> A -> B -> C -> 0
-- the code does not check whether the cohomologies of B and C are compatible with the ses
kerCohomology=method();
kerCohomology(List, List,HashTable, HashTable) := (B, C, oldBoundsB, oldBoundsC) -> (
    -- create common ring: determine the already used generators
    old := join(generators ring B_0, generators ring C_0);
    x := symbol x;
    R := ZZ[old, x_#old..x_(#old + #B - 1)];
    -- redefine B, C and create the cokernel A
    oldBToR := map(R, ring B_0, apply(#(generators ring B_0), i -> (generators R)_i));
    oldCToR := map(R, ring C_0, apply(#(generators ring C_0), i -> (generators R)_(i + #(generators ring B_0))));
    B = apply(B, b -> oldBToR(b));
    C = apply(C, c -> oldCToR(c));
    A := (generators R)_{-#B..-1};
    -- save the bounds on the old variables
    oldBounds := new MutableHashTable;
    for v in keys oldBoundsB do (
        oldBounds#(oldBToR(v)) = new HashTable from {
            {"variableName", oldBToR(v)},
            {"min", ((oldBoundsB)#v)#"min"},
            {"max", ((oldBoundsB)#v)#"max"}
            };
        );
    for v in keys oldBoundsC do (
        oldBounds#(oldCToR(v)) = new HashTable from {
            {"variableName", oldCToR(v)},
            {"min", ((oldBoundsC)#v)#"min"},
            {"max", ((oldBoundsC)#v)#"max"}
            };
        );
    oldBounds = new HashTable from oldBounds; 
    -- long exact sequence: shuffle three lists
    les := mingle(A,B,C);  
    -- split into shorter sequences whenever a zero appears and determine linear equation from it
    equations := {};
    subsequence := {};
    notSolved := {}; -- keeps track of the subsequences with unknowns
    for v in join(les, {0}) do (
        -- continue building subsequence
        if not zero v then subsequence = append(subsequence, v);
        -- finished a (non-empty) subsequence so append and start again
        if zero v and #subsequence > 0 then (
            equations = append(equations, sum(apply(#subsequence, i -> (-1)^i * subsequence_i)));
            -- if the subsequence has more than one unknown then we save it in notSolved
            if #(set subsequence * set A) > 1 then notSolved = notSolved | {subsequence};
            subsequence = {};
            );
        );
    -- determine equations from Euler characteristic being 0 for every sequence
    toReturn := apply(A, e -> e % ideal equations);
    --notSolved = notSolved /(l -> apply(l, i -> i % ideal equations));
    -- return the resulting coker in a trimmed ring
    appearingVars := select(unique flatten (toReturn / (c -> flatten entries first coefficients(c))), v -> (degree v)_0 > 0);
    y := symbol y;
    S := ZZ[y_0..y_(#appearingVars - 1)];
    substitutions := (for i from 0 to #appearingVars - 1 list appearingVars_i => y_i);
    gensA := generators ring A_0;
    substitutions = substitutions | apply(select(gensA, g -> not member(g, appearingVars)), g -> g => 20000);
    toReturn = toReturn / (v -> sub(v, substitutions));
    -- create the new bounds
    bounds := new MutableHashTable;
    for e in appearingVars do (
        bounds#(sub(e, substitutions)) = new MutableHashTable from {
            {"variableName", sub(e, substitutions)},
            {"min", 0},
            {"max", infinity}
            };
        );
    -- insert the information we have on the bounds
    -- to be adapted and improved
    -*
    for l in notSolved do (
        -- case in which B#0 and C#0 are non-zero, i.e. we are at the beginning of the long sequence
        if not isMember(l_0,C) and not isMember(l_1,C) then (
            if #l == 6 then (
                try (bounds#(sub(l_5,substitutions)))#"max" = myMax(l_4,oldBounds);
                --if (bounds#(substitute((l_4)_R,S)))#"max" == (bounds#(substitute((l_4)_R,S)))#"min" then rels = rels | {(bounds#(substitute((l_4)_R,S))) - (bounds#(substitute((l_4)_R,S)))#"max"};
                )
            else if #l%3 == 0 then (
                try (bounds#(sub(l_(-1),substitutions)))#"max" = myMax(l_(-2),oldBounds);
                -- could add a condition on the second-to-last unknown
                )
            else (
                try (bounds#(sub(l_(-2),substitutions)))#"min" = myMin(l_(-1),oldBounds);
                -- could add a condition on the second-to-last unknown
                );
            )   
        -- case in the middle of the long sequence
        else (
            if #l == 4 then (
                try (bounds#(sub((l_3),substitutions)))#"min" = max{0,myMin(l_2 - l_1,oldBounds)};
                try (bounds#(sub((l_3),substitutions)))#"max" = myMax(l_2,oldBounds);
                --if (bounds#(substitute((l_3)_R,S)))#"max" == (bounds#(substitute((l_3)_R,S)))#"min" then rels = rels | {(bounds#(substitute((l_3)_R,S))) - (bounds#(substitute((l_3)_R,S)))#"max"};
                )
            else if #l == 5 then (
                if isMember(l_4,C) then (
                    try (bounds#(sub((l_4),substitutions)))#"min" = max{0,myMin(l_3-l_2,oldBounds)};
                    try (bounds#(sub((l_4),substitutions)))#"max" = myMax(l_3,oldBounds);
                    --if (bounds#(substitute((l_4)_R,S)))#"max" == (bounds#(substitute((l_4)_R,S)))#"min" then rels = rels | {(bounds#(substitute((l_4)_R,S))) - (bounds#(substitute((l_4)_R,S)))#"max"};
                    )
                else (
                    try (bounds#(sub((l_3),substitutions)))#"min" = myMin(l_4,oldBounds);
                    try (bounds#(sub((l_3),substitutions)))#"max" = myMax(l_2 + l_4,oldBounds);
                    --if (bounds#(substitute((l_3)_R,S)))#"max" == (bounds#(substitute((l_3)_R,S)))#"min" then rels = rels | {(bounds#(substitute((l_3)_R,S))) - (bounds#(substitute((l_3)_R,S)))#"max"};
                    );
                )
            else if #l == 6 then (
                try (bounds#(sub((l_4),substitutions)))#"min" = max{0,myMin(l_5,oldBounds),myMin(l_5+l_3-l_2,oldBounds)};
                --if (bounds#(substitute((l_4)_R,S)))#"max" == (bounds#(substitute((l_4)_R,S)))#"min" then rels = rels | {(bounds#(substitute((l_4)_R,S))) - (bounds#(substitute((l_4)_R,S)))#"max"};
                )
            else if #l%3 == 1 then (
                try (bounds#(sub(l_(-1),substitutions)))#"max" = myMax(l_(-2),oldBounds);
                -- could add a condition on the second-to-last unknown
                )
            else if #l%3 == 2 then (
                if isMember(l_4,C) then (
                    try (bounds#(sub((l_(-1)),substitutions)))#"max" = myMax(l#(-2),oldBounds);
                    -- could add a condition on the second-to-last unknown
                    )
                else (
                    try (bounds#(sub((l_(-2)),substitutions)))#"min" = myMin(l#(-1),oldBounds);
                    -- could add a condition on the second-to-last unknown
                    );
                )
            else (
                try (bounds#(sub((l(-2)),substitutions)))#"min" = myMin(l#(-1),oldBounds);
                -- could add a condition on the second-to-last unknown
                );
            );
        );
    *-
    bounds = new HashTable from (keys bounds / (v -> v => new HashTable from bounds#v));
    (toReturn, bounds)
    )



-- computes the cohomology of the cokernel in a long exact sequence
longExactSequenceCoker = method();
longExactSequenceCoker (List, List) := (listOfCohom, listOfBounds) -> (
    i := 0;
    save := {};
    K := listOfCohom_i;
    bounds := listOfBounds_i;
    while i < #(listOfCohom)-1 do (
        save = save | {K, listOfCohom_(i+1)};
        (K,bounds) = cokerCohomology(K,listOfCohom_(i+1),bounds,listOfBounds_(i+1));
        i = i+1;
        );
    (K,bounds)
    )

-- computes the cohomology of the kernel in a long exact sequence
longExactSequenceKer = method();
longExactSequenceKer (List, List) := (listOfCohom, listOfBounds) -> (
    i := 1;
    save := {};
    d := #listOfCohom;
    K := listOfCohom_(-i);
    bounds := listOfBounds_(-i);
    while i < d do (
        save = save | {listOfCohom_(-(i+1)),K};
        (K,bounds) = kerCohomology(listOfCohom_(-(i+1)),K,listOfBounds_(-(i+1)),bounds);
        i = i+1;
        );
    (K,bounds)
    )


-------------------------- hodgeNumbers ---------------------------------


-- compute the Hodge numbers of a homogeneous variety
hodgeNumbers = method(Options => {Verbose => false});
hodgeNumbers HomogeneousVariety := o -> X -> (
    cotangent := homogeneousCotangentBundle X;
    if o.Verbose then << "computing the exterior powers of the cotangent bundle of X" << endl;
    n := dim X;
    Vcotangent := timedIf(o.Verbose, () -> multipleExteriorPower(floor(n/2), cotangent));
    if o.Verbose then << "computing the cohomologies" << endl;
    hodge := new MutableHashTable;
    timedIf(o.Verbose, () -> (
        for p from 0 to floor(n/2) do (
            hodge#p = (cohomology Vcotangent#p)#0;
            );
        ));
    new HashTable from hodge
    );

-- compute the Hodge numbers of a homogeneous variety up to the (k-1)th row
hodgeNumbers (ZZ,HomogeneousVariety) := o -> (k,X) -> (
    cotangent := homogeneousCotangentBundle X;
    if o.Verbose then << "computing the exterior powers of the cotangent bundle of X" << endl;
    n := dim X;
    if k > n then k = n;
    Vcotangent := timedIf(o.Verbose, () -> multipleExteriorPower(floor(k/2), cotangent));
    if o.Verbose then << "computing the cohomologies" << endl;
    hodge := new MutableHashTable;
    timedIf(o.Verbose, () -> (
        for p from 0 to floor(k/2) do (
            hodge#p = (cohomology Vcotangent#p)#0;
            );
        ));
    new HashTable from hodge
    );
    
-- compute the Hodge numbers of a subvariety
hodgeNumbers EmbeddedVariety := o -> Y -> (
    if Y#"cohomology" =!= null then return (Y#"cohomology",Y#"cohomologyBounds");
    X := Y#"ambientSpace";
    F := Y#"normalBundle";
    F' := dual F;
    cotangent := homogeneousCotangentBundle X;
    K := {};
    if o.Verbose then << "computing the koszul complex" << endl;
    timedIf(o.Verbose, () -> (
        if F#?"koszul" then (
            K = reverse F#"koszul";
            )
        else (
            D := determinant F';
            r := rank F;
            S := {D};
            S' := {structureSheaf X};
            extTemp := multipleExteriorPower(floor((r-1)/2),F);
            for i from 1 to floor((r-1)/2) do (
                S = S|{tensorProduct(extTemp#i,D)};
                S' = {dual extTemp#i}|S';
                );
            if even r then S = S|{exteriorPower(lift(r/2,ZZ),F')}|S';
            if odd r then S = S|S';
            F#"koszul" = reverse S;
            K = reverse F#"koszul";
            );
        ));
    n := Y#"dimension";
    if o.Verbose then << "computing the exterior powers of the cotangent bundle of X" << endl;
    Vcotangent := timedIf(o.Verbose, () -> multipleExteriorPower(floor(n/2), cotangent));
    if o.Verbose then << "computing the symmetric powers of the dual of the normal bundle of Y" << endl;
    SymF' := timedIf(o.Verbose, () -> multipleSymmetricPower(floor(n/2), F'));
    hodge := new MutableHashTable;
    listOfCohom := {};
    listOfBounds := {};
    cohom := {};
    bounds := new HashTable from {};
    appearingVars := {};
    numTotAppVars := 0;
    numAppVars := new MutableHashTable from {};
    if o.Verbose then << "computing the cohomologies"  << endl;
    -- now if the dimension of Y is high (greater than 5) I use the first method that saves on tensor powers
    if n > 5 then (
        timedIf(o.Verbose, () -> (
            -- p is the exterior power of the cotangent bundle I am currently considering
            pages := new MutableHashTable;
            start := 1;
            for i from 1 to floor(n/2)+1 do (
                for p from start to floor(n/2) do (
                    if i == 1 then pages#(i,p) = apply(K, k -> tensorProduct(k,Vcotangent#p))
                    else if i == p+1 then pages#(i,p) = apply(K, k -> tensorProduct(k,SymF'#p))
                    else pages#(i,p) = apply(pages#(i,i-1), l -> tensorProduct(l,Vcotangent#(p-i+1)));
                    );
                if i =!= 1 then start = start + 1;
                );
            pages = new HashTable from pages;
            -- first compute the cohomology of O_Y
            cohom = cohomologyRestriction(X,F,structureSheaf X);
            bounds = merge(bounds,new HashTable from {}, (i,j) -> {i,j});
            hodge#0 = cohom;
            appearingVars = appearingVars | select(unique flatten apply(cohom, v -> (
                        if #(degree v) > 0 then flatten entries first coefficients v else {})), a -> a != 1);
            numAppVars#0 = #appearingVars - numTotAppVars;
            numTotAppVars = #appearingVars;
            if odd n then hodge#(n) = reverse take(cohom, n+1);
            if even n then hodge#(n) = reverse take(cohom, n+1);
            for p from 1 to floor(n/2) do (
                listOfCohom = {};
                listOfBounds = {};
                listOfCohomTemp := {};
                listOfBoundsTemp := {};
                cohomTemp := {};
                temp := {};
                for i from 1 to p+1 do (
                    cohomTemp = apply(pages#(i,p), k -> cohomology(k));
                    listOfCohomTemp =  apply(cohomTemp, v -> v#0);
                    listOfBoundsTemp = apply(cohomTemp, v -> v#1);
                    temp = longExactSequenceCoker(listOfCohomTemp, listOfBoundsTemp);
                    listOfCohom = listOfCohom | {temp#0};
                    listOfBounds = listOfBounds | {temp#1};
                    );
                cohom = longExactSequenceCoker(reverse listOfCohom, reverse listOfBounds);
                bounds = merge(bounds,cohom#1, (i,j) -> {i,j});
                hodge#p = cohom#0;
                -- save any unknowns that appear in the Hodge numbers
                appearingVars = appearingVars | select(unique flatten apply(cohom#0, v -> (
                            if #(degree v) > 0 then flatten entries first coefficients v else {})), a -> a != 1);
                numAppVars#p = #appearingVars - numTotAppVars;
                numTotAppVars = #appearingVars;
                if odd n then hodge#(n-p) = reverse take(cohom#0, n+1);
                if even n and p =!= lift(n/2,ZZ) then hodge#(n-p) = reverse take(cohom#0, n+1);
                );
            ));
        )
    -- if the dimension of Y is less or equal than five I save memory but I compute the same thing multiple times
    else (
        timedIf(o.Verbose, () -> (
            -- p is the exterior power of the cotangent bundle I am currently considering
            for p from 0 to floor(n/2) do (
                comp := compositions(2,p);
                L := for m in comp list (
                    tensorProduct(SymF'#(m#0),Vcotangent#(m#1))
                    );
                listOfCohom = {};
                listOfBounds = {};
                listOfCohomTemp := {};
                listOfBoundsTemp := {};
                cohomTemp := {};
                temp := {};
                for l in L do (
                    cohomTemp = apply(K, k -> cohomology(tensorProduct(k,l)));
                    listOfCohomTemp =  apply(cohomTemp, v -> v#0);
                    listOfBoundsTemp = apply(cohomTemp, v -> v#1);
                    temp = longExactSequenceCoker(listOfCohomTemp, listOfBoundsTemp);
                    listOfCohom = listOfCohom | {temp#0};
                    listOfBounds = listOfBounds | {temp#1};
                    );
                cohom = longExactSequenceCoker(listOfCohom, listOfBounds);
                bounds = merge(bounds,cohom#1, (i,j) -> {i,j});
                hodge#p = cohom#0;
                -- save any unknowns that appear in the Hodge numbers
                appearingVars = appearingVars | select(unique flatten apply(cohom#0, v -> (
                            if #(degree v) > 0 then flatten entries first coefficients v else {})), a -> a != 1);
                numAppVars#p = #appearingVars - numTotAppVars;
                numTotAppVars = #appearingVars;
                if odd n then hodge#(n-p) = reverse take(cohom#0, n+1);
                if even n and p =!= lift(n/2,ZZ) then hodge#(n-p) = reverse take(cohom#0, n+1);
                );
            ));
        );
    -- before imposing the relations we need to make sure that all the Hodge numbers
    -- are in the same ring, so that we can build there the ideal of relations
    if o.Verbose then << "computing the various bounds on the unknowns" << endl;
    timedIf(o.Verbose, () -> (
        x := symbol x;
        finalRing := ZZ[x_0..x_(numTotAppVars-1), MonomialOrder => Weights => for i from 1 to numTotAppVars list i];
        --
        kounter := 0;
        bounds = new MutableHashTable from bounds;
        for p from 0 to floor(n/2) do (
            ZtoX := map(finalRing, ring first hodge#p,
                (
                    remainingVars := gens ring first hodge#p;
                    apply(appearingVars, A -> remainingVars = delete(A,remainingVars));
                    subTo1 := apply(remainingVars, V -> V => 1);
                    subTo1 | for ell from kounter to kounter + numAppVars#p - 1 list (
                        appearingVars_(ell) => (gens finalRing)_ell
                        )
                    )
                );
            kounter = kounter + numAppVars#p;
            hodge#p = apply(hodge#p,v -> ZtoX(v));
            for a in keys bounds do (
                try b := ZtoX(a) then (
                    bounds#b = new HashTable from {
                        {"variableName", b},
                        {"min", ((bounds)#a)#"min"},
                        {"max", ((bounds)#a)#"max"}
                        };
                    remove(bounds,a);
                    );
                );	
            if odd n then hodge#(n-p) = apply(hodge#(n-p),v -> ZtoX(v));
            if even n and p =!= lift(n/2,ZZ) then hodge#(n-p) = apply(hodge#(n-p),v -> ZtoX(v));
            );
        -- now we start computing the relations
        eqns:={};
        -- conjugation
        eqns = eqns | flatten for p from 0 to n list (
            for q from p+1 to #(hodge#p) - 1 list (
                if hodge#?q and p <= #(hodge#q)-1 then (hodge#p)#q - (hodge#q)#p
                )
            );
        -- *-Hodge reflection
        eqns = eqns | flatten for p from 0 to n list (
            for q from 0 to #(hodge#p) - 1 list (
                if hodge#?(n-q) and n-p <= #(hodge#(n-q))-1 then (hodge#p)#q - (hodge#(n-q))#(n-p)
                )
            );
        eqns = eqns | flatten for p from 0 to n list (
            for q from n+1 to #(hodge#p) -1 list (hodge#p)#q
            );
        eqns = eqns | flatten for p from 0 to n list (
            for q from n+1 to #(hodge#p) - 1 list (hodge#p)#q
            );
        eqns = select(eqns, b -> b =!= null);
        eqns = select(eqns, b -> b != 0);
        -- sanity check
        if any(flatten apply(flatten entries mingens ideal eqns,G->degree G), d->d!=1) then error("Something went wrong with relations in Hodge diamond");
        appearingVars = {};
        if #eqns > 0 then (
            for p in keys hodge do (
                hodge#p = apply(hodge#p, i -> i % ideal eqns);
                appearingVars = appearingVars | select(unique flatten (hodge#p / (c -> flatten entries first coefficients(c))), v -> (degree v)_0 > 0);
                );
            appearingVars = unique appearingVars;
            for a in keys bounds do (
                if not isMember(a % ideal eqns, appearingVars) then remove(bounds,a);
                );
            );
        Y#"cohomology" = new HashTable from hodge;
        Y#"cohomologyBounds" = new HashTable from bounds;
        ));
    (Y#"cohomology",Y#"cohomologyBounds")
    )

-- compute the Hodge numbers of a subvariety up to the (k-1)th row
hodgeNumbers (ZZ,EmbeddedVariety) := o -> (k,Y) -> (
    X := Y#"ambientSpace";
    F := Y#"normalBundle";
    F' := dual F;
    if k >= dim Y then return hodgeNumbers Y;
    cotangent := homogeneousCotangentBundle X;
    if o.Verbose then << "computing the koszul complex" << endl;
    -- here I rewrite the whole koszul complex
    K := {};
    timedIf(o.Verbose, () -> (
        if F#?"koszul" then (
            K = reverse F#"koszul";
            )
        else (
            D := determinant F';
            r := rank F;
            S := {D};
            S' := {structureSheaf X};
            extTemp := multipleExteriorPower(floor((r-1)/2),F);
            for i from 1 to floor((r-1)/2) do (
                S = S|{tensorProduct(extTemp#i,D)};
                S' = {dual extTemp#i}|S';
                );
            if even r then S = S|{exteriorPower(lift(r/2,ZZ),F')}|S';
            if odd r then S = S|S';
            F#"koszul" = reverse S;
            K = reverse F#"koszul";
            );
        ));
    n := Y#"dimension";
    if o.Verbose then << "computing the exterior powers of the cotangent bundle of X" << endl;
    Vcotangent := timedIf(o.Verbose, () -> multipleExteriorPower(floor(k/2), cotangent));
    if o.Verbose then << "computing the symmetric powers of the dual of the normal bundle of Y" << endl;
    SymF' := timedIf(o.Verbose, () -> multipleSymmetricPower(floor(k/2), F'));
    hodge := new MutableHashTable;
    listOfCohom := {};
    listOfBounds := {};
    cohom := {};
    bounds := new HashTable from {};
    appearingVars := {};
    numTotAppVars := 0;
    numAppVars := new MutableHashTable from {};
    if o.Verbose then << "computing the cohomologies"  << endl;
    timedIf(o.Verbose, () -> (
        -- p is the exterior power of the cotangent bundle I am currently considering
        for p from 0 to floor(k/2) do (
            comp := compositions(2,p);
            L := for m in comp list (
                tensorProduct(SymF'#(m#0),Vcotangent#(m#1))
                );
            listOfCohom = {};
            listOfBounds = {};
            listOfCohomTemp := {};
            listOfBoundsTemp := {};
            cohomTemp := {};
            temp := {};
            for l in L do (
                cohomTemp = apply(K, k -> cohomology(tensorProduct(k,l)));
                listOfCohomTemp =  apply(cohomTemp, v -> v#0);
                listOfBoundsTemp = apply(cohomTemp, v -> v#1);
                temp = longExactSequenceCoker(listOfCohomTemp, listOfBoundsTemp);
                listOfCohom = listOfCohom | {temp#0};
                listOfBounds = listOfBounds | {temp#1};
                );
            cohom = longExactSequenceCoker(listOfCohom, listOfBounds);
            bounds = merge(bounds,cohom#1, (i,j) -> {i,j});
            hodge#p = cohom#0;
            -- save any unknowns that appear in the Hodge numbers
            appearingVars = appearingVars | select(unique flatten apply(cohom#0, v -> (
                        if #(degree v) > 0 then flatten entries first coefficients v else {})), a -> a != 1);
            numAppVars#p = #appearingVars - numTotAppVars;
            numTotAppVars = #appearingVars;
            if odd n then hodge#(n-p) = reverse take(cohom#0, n+1);
            if even n and p =!= lift(n/2,ZZ) then hodge#(n-p) = reverse take(cohom#0, n+1);
            );
        ));
    -- before imposing the relations we need to make sure that all the Hodge numbers
    -- are in the same ring, so that we can build there the ideal of relations
    << "Warning: Hodge relations not imposed" << endl; -- to be improved
    (new HashTable from hodge, new HashTable from bounds)
    )

-- diamond presentation of the Hodge numbers
displayHN = method(Options => {DisplayBounds => false});
displayHN HomogeneousVariety := o -> X -> (
    hn := new MutableHashTable from (hodgeNumbers (X, Verbose => false));
    n := dim X;
    for i from floor(n/2) + 1 to n do (
        hn#i = toList((n+1):0);
        );
    nOfLines := #(keys hn)-1;
    D := for line from 0 to nOfLines list (
        apply(nOfLines-line, i -> null) | (flatten for p from 0 to (line-1) list ({(hn#p)_(line-p), null})) | {(hn#line)_0} | apply(nOfLines-line, i -> null)
        );
    D = for i from 0 to #D-1 list {i} | D_i;
    return netList(D, Alignment=>Center);
    )

-- diamond presentation of the Hodge numbers
displayHN (ZZ,HomogeneousVariety) := o -> (k,X) -> (
    hn := new MutableHashTable from (hodgeNumbers(k,X));
    n := dim X;
    if k >= n then k = n;
    for i from floor(k/2) + 1 to k do (
        hn#i = toList((k+1):0);
        );
    nOfLines := #(keys hn)-1;
    D := for line from 0 to nOfLines list (
        apply(nOfLines-line, i -> null) | (flatten for p from 0 to (line-1) list ({(hn#p)_(line-p), null})) | {(hn#line)_0} | apply(nOfLines-line, i -> null)
        );
    D = for i from 0 to #D-1 list {i} | D_i;
    return netList(D, Alignment=>Center);
    )

-- diamond presentation of the Hodge numbers
displayHN EmbeddedVariety := o -> Y -> (
    hn := (hodgeNumbers (Y,Verbose => false));
    bounds := hn#1;
    hn = hn#0;
    nOfLines := #(keys hn)-1;
    D := for line from 0 to nOfLines list (
        apply(nOfLines-line, i -> null) | (flatten for p from 0 to (line-1) list ({(hn#p)_(line-p), null})) | {(hn#line)_0} | apply(nOfLines-line, i -> null)
        );
    D = for i from 0 to #D-1 list {i} | D_i;
    if o.DisplayBounds then return (netList(D, Alignment=>Center),bounds);
    return netList(D, Alignment=>Center)
    )

-- diamond presentation of the Hodge numbers
displayHN (ZZ,EmbeddedVariety) := o -> (k,Y) -> (
    if k >= dim Y then (
        hn := (hodgeNumbers (Y,Verbose => false));
        )
    else (
        hn = (hodgeNumbers (k,Y,Verbose => false));
        );
    bounds := hn#1;
    hn = hn#0;
    nOfLines := k;
    D := for line from 0 to nOfLines list (
        apply(nOfLines-line, i -> null) | (flatten for p from 0 to (line-1) list (if hn#?p then {(hn#p)_(line-p), null} else {"*",null})) | (if hn#?line then {(hn#line)_0} else {"*"}) | apply(nOfLines-line, i -> null)
        );
    D = for i from 0 to #D-1 list {i} | D_i;
    if o.DisplayBounds then return (netList(D, Alignment=>Center),bounds);
    return netList(D, Alignment=>Center)
    )



-------------------------- hochschildNumbers ---------------------------------

-- compute the Hochschild numbers of a subvariety 
hochschildNumbers = method(Options => {Verbose => false});
hochschildNumbers EmbeddedVariety := o -> Y -> (
    X := Y#"ambientSpace";
    F := Y#"normalBundle";
    F' := dual F;
    tangent := homogeneousTangentBundle X;
    if o.Verbose then << "computing the koszul complex" << endl;
    -- here I rewrite the whole koszul complex
    K := {};
    timedIf(o.Verbose, () -> (
        if F#?"koszul" then (
            K = reverse F#"koszul";
            )
        else (
            D := determinant F';
            r := rank F;
            S := {D};
            S' := {structureSheaf X};
            extTemp := multipleExteriorPower(floor((r-1)/2),F);
            for i from 1 to floor((r-1)/2) do (
                S = S|{tensorProduct(extTemp#i,D)};
                S' = {dual extTemp#i}|S';
                );
            if even r then S = S|{exteriorPower(lift(r/2,ZZ),F')}|S';
            if odd r then S = S|S';
            F#"koszul" = reverse S;
            K = reverse F#"koszul";
            );
        ));
    n := Y#"dimension";
    if o.Verbose then << "computing the exterior powers of the tangent bundle of X" << endl;
    Vtangent := timedIf(o.Verbose, () -> multipleExteriorPower(n, tangent));
    if o.Verbose then << "computing the symmetric powers of the normal bundle of Y" << endl;
    SymF := timedIf(o.Verbose, () -> (multipleSymmetricPower(n, F)));
    hochschild := new MutableHashTable;
    listOfCohom := {};
    listOfBounds := {};
    cohom := {};
    bounds := new HashTable from {};
    appearingVars := {};
    numTotAppVars := 0;
    numAppVars := new MutableHashTable from {};
    if o.Verbose then << "computing the cohomologies"  << endl;
    timedIf(o.Verbose, () -> (
        -- p is the exterior power of the tangent bundle I am currently considering
        for p from 0 to n do (
            comp := compositions(2,p);
            L := for m in comp list (
                tensorProduct(Vtangent#(m#0),SymF#(m#1))
                );
            listOfCohom = {};
            listOfBounds = {};
            listOfCohomTemp := {};
            listOfBoundsTemp := {};
            cohomTemp := {};
            temp := {};
            for l in L do (
                cohomTemp = apply(K, k -> cohomology(tensorProduct(k,l)));
                listOfCohomTemp =  apply(cohomTemp, v -> v#0);
                listOfBoundsTemp = apply(cohomTemp, v -> v#1);
                temp = longExactSequenceCoker(listOfCohomTemp, listOfBoundsTemp);
                listOfCohom = listOfCohom | {temp#0};
                listOfBounds = listOfBounds | {temp#1};
                );
            cohom = longExactSequenceKer(listOfCohom, listOfBounds);
            bounds = merge(bounds,cohom#1, (i,j) -> {i,j});
            hochschild#p = cohom#0;
            -- save any unknowns that appear in the Hochschild numbers
            appearingVars = appearingVars | select(unique flatten apply(cohom#0, v -> (
                        if #(degree v) > 0 then flatten entries first coefficients v else {})), a -> a != 1);
            numAppVars#p = #appearingVars - numTotAppVars;
            numTotAppVars = #appearingVars;
            );
        ));
    -- we need to make sure that all the Hochschild numbers are in the same ring
    if o.Verbose then << "computing the various bounds on the unknowns" << endl;
    timedIf(o.Verbose, () -> (
        x := symbol x;
        finalRing := ZZ[x_0..x_(numTotAppVars-1), MonomialOrder => Weights => for i from 1 to numTotAppVars list i];
        --
        kounter := 0;
        bounds = new MutableHashTable from bounds;
        for p from 0 to n do (
            ZtoX := map(finalRing, ring first hochschild#p,
                (
                    remainingVars := gens ring first hochschild#p;
                    apply(appearingVars, A -> remainingVars = delete(A,remainingVars));
                    subTo1 := apply(remainingVars, V -> V => 1);
                    subTo1 | for ell from kounter to kounter + numAppVars#p - 1 list (
                        appearingVars_(ell) => (gens finalRing)_ell
                        )
                    )
                );
            kounter = kounter + numAppVars#p;
            hochschild#p = apply(hochschild#p,v -> ZtoX(v));
            for a in keys bounds do (
                try b := ZtoX(a) then (
                    bounds#b = new HashTable from {
                        {"variableName", b},
                        {"min", ((bounds)#a)#"min"},
                        {"max", ((bounds)#a)#"max"}
                        };
                    remove(bounds,a);
                    );
                );	
            );
        ));
    (new HashTable from hochschild,new HashTable from bounds)
    )

-- parallelogram presentation of the Hochschild numbers
displayHochN = method();
displayHochN EmbeddedVariety := Y -> (
    hn := (hochschildNumbers Y)#0;
    n := dim Y;
    nOfLines := 2*n+1;
    D := for line from 0 to n list (
        apply(line, i -> -1) | take(hn#line,n+1) | apply(n-line, i -> -1)
        );
    D = entries transpose matrix D;
    D = D/(r -> r/(e -> if e==-1 then null else e));
    D = for i from 0 to #D-1 list {i} | D_i;
    return netList(D, Alignment=>Center);
    )



---------------------------- volume --------------------------------


-- Hilbert polynomial of a vector bundle E with respect to an ample line bundle L on a homogeneous variety: chi(L^k * E)
hilbertPolynomial(HomogeneousVectorBundle,HomogeneousVectorBundle) := o -> (E,L) -> (
    if rank L =!= 1 then error "the Hilbert polynomial should be considered with respect to an ample line bundle";
    if not isAmple(L) then error "the Hilbert polynomial should be considered with respect to an ample line bundle";
    X := L#"underlyingVariety";
    R := X#"rootSystem";
    P := X#"parabolicSubgroup";
    l := (L#"weights")#0;
    k := symbol k;
    Sring := QQ[k];
    d := dim(X);
    S := E#"weights";
    m := E#"multiplicities";
    ro := halfSumOfRoots(R);
    Lr := toList(positiveRoots(R));
    rootNorms := R#((keys R)#1);
    M := transpose inverse promote(cartanMatrix(R),QQ); -- change of basis from the weight basis to the root basis
    h := k*0;
    for i from 0 to #S-1 do (
        w := weight(R, entries S#i);
        v := k*l + w;
        num := apply(Lr, s -> sum apply(apply(entries(M*(v+ro)),entries(s),(x,y)->x*y),rootNorms,(x,y)->x*y)//norm(R,s));
        den := apply(Lr, s -> floor(2*scalarProduct(R,ro,s)//norm(R,s)));
        h = h + m#i*product(num)/product(den);
        );
    return h;
    );


-- Hilbert polynomial of a vector bundle E with respect to an ample line bundle L on a subvariety: chi(Y,L^k * E)
hilbertPolynomial(HomogeneousVectorBundle,HomogeneousVectorBundle,EmbeddedVariety) := o -> (E,L,Y) -> (
    if rank L =!= 1 then error "the Hilbert polynomial should be considered with respect to a line bundle";
    if not isAmple(L) then error "the Hilbert polynomial should be considered with respect to an ample line bundle (already on the ambient variety)";
    X := L#"underlyingVariety";
    R := X#"rootSystem";
    P := X#"parabolicSubgroup";
    l := (L#"weights")#0;
    F := Y#"normalBundle";
    K := koszul(X,F);
    tK := apply(K, f -> tensorProduct(f,E));
    chis := apply(tK, f -> hilbertPolynomial(f,L));
    x := symbol x;
    finalRing := QQ[x];
    toX := chis/( c -> map(finalRing, ring c, {x}));
    h := sum apply(#tK,i -> (toX#i)(chis#i * (-1)^i));
    return h;
    );

-- compute the volume of a homogeneous variety as the leading term of the hilbert polynomial of O_X with respect to K_X^\vee (which is ample) multiplied by dim X !
volumeFano = method();
volumeFano HomogeneousVariety := X -> (
    c := chern(1,X);
    R := X#"rootSystem";
    P := X#"parabolicSubgroup";
    RP := rootSystem(R,P#"parabolic");
    LX := sort toList ((set toList(1..rank R)) - ((X#"parabolicSubgroup")#"parabolic")); 
    E := structureSheaf X;
    v := mixWeights(weight(RP,apply(rank R - #LX, i -> 0)), entries c, LX);
    KX' := homogeneousVectorBundle({weight(R,v)},{1},X);
    h := hilbertPolynomial(E,KX');
    d := first degree h;
    if d =!= dim X then error "something wrong in computing the hilbert polynomial";
    lift(d! * leadCoefficient h,ZZ)
    )    


-- compute the volume of a homogeneous variety as the leading term of the hilbert polynomial of O_Y with respect to K_Y^\vee multiplied by dim Y !
volumeFano EmbeddedVariety := Y -> (
    c := chern(1,Y);
    X := Y#"ambientSpace";
    R := X#"rootSystem";
    P := X#"parabolicSubgroup";
    RP := rootSystem(R,P#"parabolic");
    LX := sort toList ((set toList(1..rank R)) - ((X#"parabolicSubgroup")#"parabolic")); 
    L := structureSheaf X;
    v := mixWeights(weight(RP,apply(rank R - #LX, i -> 0)), entries c, LX);
    KY' := homogeneousVectorBundle({weight(R,v)},{1},X);
    if not isAmple(KY') then error "we can compute the volume only of a subvariety with anti-ample canonical bundle (already on the ambient variety)";
    h := hilbertPolynomial(L,KY',Y);
    d := first degree h;
    if d =!= dim Y then error "something wrong in computing the hilbert polynomial";
    lift(d! * leadCoefficient h,ZZ)
    )    



-- compute the following list of invariants for a homogeneous variety (volume,{chiT^1,chiT^2,..chiT^maxChiT},{chiO_X,chiCot^1,chiCot^2,..},Hodge numbers)
invariants = method(Options => {doHodge => true, maxChiT => 1});
invariants HomogeneousVariety := o -> X -> (
    vol := volumeFano X;
    powersT := multipleExteriorPower(o.maxChiT, homogeneousTangentBundle X);
    powersCot := multipleExteriorPower(floor(dim X/2), homogeneousCotangentBundle X);
    chiT := for k from 1 to o.maxChiT list (
        eulerCharacteristic(powersT#k)
        );
    chiCot := for k from 0 to floor(dim X/2) list (
        eulerCharacteristic(powersCot#k)
        );
    hodge := (new HashTable from {},new HashTable from {});
    if o.doHodge then (
        hodge = hodgeNumbers (X,Verbose => false); -- this is a pair of two hash tables
        -- the first is the Hodge numbers and the second the bounds on the unknowns
        );
    (vol,chiT,chiCot,hodge)
    )


-- compute the following list of invariants for a subvariety (volume (if strongly Fano),{chiT^1,chiT^2,..chiT^maxChiT},{chiO_Y,chiCot^1,chiCot^2,..},Hodge numbers)
invariants EmbeddedVariety := o -> Y -> (
    F := Y#"normalBundle";
    X := Y#"ambientSpace";
    if all(entries (chern(1,X) - chern(1,F)), i -> i > 0) then vol := volumeFano Y
    else vol = -1;
    chiT := for k from 1 to o.maxChiT list (
        eulerCharacteristicTangent(k,Y)
        );
    chiCot := for k from 0 to floor(dim Y/2) list (
        eulerCharacteristicCotangent(k,Y)
        );
    hodge := (new HashTable from {},new HashTable from {});
    if o.doHodge then (
        hodge = hodgeNumbers (Y,Verbose => false); -- this is a pair of two hash tables
        -- the first is the Hodge numbers and the second the bounds on the unknowns
        );
    (vol,chiT,chiCot,hodge)
    )
-------------------------------------------------------------------------------------------

-- to be done: Hodge numbers twisted 

-- compute the twisted Hochschild numbers h^q(A^pT_Y * E)
hochschildNumbersTwisted = method(Options => {Verbose => false});
hochschildNumbersTwisted (EmbeddedVariety,HomogeneousVectorBundle) := o -> (Y,E) -> (
    X := Y#"ambientSpace";
    F := Y#"normalBundle";
    F' := dual F;
    R := X#"rootSystem";
    P := X#"parabolicSubgroup";
    RP := rootSystem(R,P#"parabolic");
    tangent := homogeneousTangentBundle X;
    if o.Verbose then << "computing the koszul complex" << endl;
    -- here I rewrite the whole koszul complex
    K := {};
    timedIf(o.Verbose, () -> (
        if F#?"koszul" then (
            K = reverse F#"koszul";
            )
        else (
            D := determinant F';
            r := rank F;
            S := {D};
            S' := {structureSheaf X};
            extTemp := multipleExteriorPower(floor((r-1)/2),F);
            for i from 1 to floor((r-1)/2) do (
                --extTemp = exteriorPower(i,F);
                S = S|{tensorProduct(extTemp#i,D)};
                S' = {dual extTemp#i}|S';
                );
            if even r then S = S|{exteriorPower(lift(r/2,ZZ),F')}|S';
            if odd r then S = S|S';
            F#"koszul" = reverse S;
            K = reverse F#"koszul";
            );
        ));
    n := Y#"dimension";
    if o.Verbose then << "computing the exterior powers of the tangent bundle of X" << endl;
    Vtangent := timedIf(o.Verbose, () -> multipleExteriorPower(n, tangent));
    if o.Verbose then << "computing the symmetric powers of the normal bundle of Y" << endl;
    SymF := timedIf(o.Verbose, () -> (multipleSymmetricPower(n, F)));
    hochschild := new MutableHashTable;
    listOfCohom := {};
    listOfBounds := {};
    cohom := {};
    bounds := new HashTable from {};
    appearingVars := {};
    numTotAppVars := 0;
    numAppVars := new MutableHashTable from {};
    if o.Verbose then << "computing the cohomologies"  << endl;
    timedIf(o.Verbose, () -> (
        -- p is the exterior power of the tangent bundle I am currently considering
        for p from 0 to n do (
            comp := compositions(2,p);
            L := for m in comp list (
                tensorProduct(Vtangent#(m#0),SymF#(m#1))*E
                );
            listOfCohom = {};
            listOfBounds = {};
            listOfCohomTemp := {};
            listOfBoundsTemp := {};
            cohomTemp := {};
            temp := {};
            for l in L do (
                cohomTemp = apply(K, k -> cohomology(tensorProduct(k,l)));
                listOfCohomTemp =  apply(cohomTemp, v -> v#0);
                listOfBoundsTemp = apply(cohomTemp, v -> v#1);
                temp = longExactSequenceCoker(listOfCohomTemp, listOfBoundsTemp);
                listOfCohom = listOfCohom | {temp#0};
                listOfBounds = listOfBounds | {temp#1};
                );
            cohom = longExactSequenceKer(listOfCohom, listOfBounds);
            bounds = merge(bounds,cohom#1, (i,j) -> {i,j});
            hochschild#p = cohom#0;
            -- save any unknowns that appear in the Hochschild numbers
            appearingVars = appearingVars | select(unique flatten apply(cohom#0, v -> (
                        if #(degree v) > 0 then flatten entries first coefficients v else {})), a -> a != 1);
            numAppVars#p = #appearingVars - numTotAppVars;
            numTotAppVars = #appearingVars;
            );
        ));
    -- we need to make sure that all the Hochschild numbers are in the same ring
    if o.Verbose then << "computing the various bounds on the unknowns" << endl;
    timedIf(o.Verbose, () -> (
        x := symbol x;
        finalRing := ZZ[x_0..x_(numTotAppVars-1), MonomialOrder => Weights => for i from 1 to numTotAppVars list i];
        --
        kounter := 0;
        bounds = new MutableHashTable from bounds;
        for p from 0 to n do (
            ZtoX := map(finalRing, ring first hochschild#p,
                (
                    remainingVars := gens ring first hochschild#p;
                    apply(appearingVars, A -> remainingVars = delete(A,remainingVars));
                    subTo1 := apply(remainingVars, V -> V => 1);
                    subTo1 | for ell from kounter to kounter + numAppVars#p - 1 list (
                        appearingVars_(ell) => (gens finalRing)_ell
                        )
                    )
                );
            kounter = kounter + numAppVars#p;
            hochschild#p = apply(hochschild#p,v -> ZtoX(v));
            for a in keys bounds do (
                try b := ZtoX(a) then (
                    bounds#b = new HashTable from {
                        {"variableName", b},
                        {"min", ((bounds)#a)#"min"},
                        {"max", ((bounds)#a)#"max"}
                        };
                    remove(bounds,a);
                    );
                );	
            );
        ));
    (new HashTable from hochschild,new HashTable from bounds)
    )
    
    
-- chi(Y,A^pT_Y*E_Y)
eulerCharacteristicTangentTwisted = method();
eulerCharacteristicTangentTwisted (ZZ,EmbeddedVariety,HomogeneousVectorBundle) := (p,Y,E) -> (
    X := Y#"ambientSpace";
    F := Y#"normalBundle";
    R := X#"rootSystem";
    cotangent := homogeneousCotangentBundle X;
    Vtangent := multipleExteriorPower(p, dual cotangent);
    SymF := multipleSymmetricPower(p, F);
    comp := compositions(2,p);
    L := for m in comp list (
        tensorProduct(SymF#(m#0),Vtangent#(m#1))*E
        );
    (sum apply(#L, i -> ((-1)^i)*eulerCharacteristic(X,F,(L)#i)))*(-1)^p
    )
