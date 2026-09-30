

-- tensor product of two completely reducible homogeneous bundles
tensorProduct (HomogeneousVectorBundle, HomogeneousVectorBundle) := (E1,E2) -> (
    if not (E1#"underlyingVariety" == E2#"underlyingVariety") then error "expected vector bundles on the same variety";
    X := E1#"underlyingVariety";
    RP := E1#"rootSystem";
    T := new MutableHashTable;
    Ttemp := new MutableHashTable;
    R := X#"rootSystem";
    LX := sort toList ((set toList(1..rank R)) - ((X#"parabolicSubgroup")#"parabolic")); 
    if rank(E2) < rank(E1) then ( -- make sure that E1 is the one of lower rank
        E := E1;
        E1 = E2;
        E2 = E;
        );
    if rank(E1) == 1 then return homogeneousVectorBundle(apply(E2#"weights", l -> l + (E1#"weights")#0), E2#"multiplicities",X);
    if E1#"irreducible" and E2#"irreducible" then (
        l1 := (E1#"parabolicWeights")#0;
        W := dominantCharacterHighestWeight(RP,l1);
        l2 := (E2#"parabolicWeights")#0;
        Ttemp = tensorProduct(l1,l2,RP,W);
        Ttemp = new HashTable from Ttemp;
        d := (rank(E2)*chern(1,E1) + rank(E1)*chern(1,E2))/(rank(E1)*rank(E2)); 
        -- this d must be the SLOPE of every summand that I compute with the representations
        for t in keys Ttemp do (
            E := homogeneousVectorBundle(toGlobalWeights({t},X#"parabolicSubgroup"), {1}, X);
            c := lift(d-chern(1,E)/rank E,ZZ);
            l' := weight(R,mixWeights(t,entries c,LX));
            if T#?(l') then T#(l') = T#(l') + Ttemp#t
            else T#(l') = Ttemp#t;
            );
        T = new HashTable from T;
        E = homogeneousVectorBundle(apply(keys T, t -> weight(R,entries t)), values T, X);
        return E;
        )
    else (
        S := homogeneousVectorBundle({},{},X);
        for i from 0 to #(E1#"weights")-1 do (
            for j from 0 to #(E2#"weights")-1 do (
                E = tensorProduct(homogeneousVectorBundle({(E1#"weights")#i},{1},X),homogeneousVectorBundle({(E2#"weights")#j},{1},X));
                S = S + homogeneousVectorBundle(E#"weights",apply(E#"multiplicities", k -> k * (E1#"multiplicities")#i * (E2#"multiplicities")#j),X);
                );
            );
        return S;
        );
    )

-- tensor product of a completely reducible bundle with a filtration
tensorProduct (FiltrationBundle, HomogeneousVectorBundle) := (F,E) -> (
    X := E#"underlyingVariety";
    return filtrationBundle(apply(F#"factors", f -> f*E),X);
    )

tensorProduct (HomogeneousVectorBundle, FiltrationBundle) := (E,F) -> (
    X := E#"underlyingVariety";
    return filtrationBundle(apply(F#"factors", f -> f*E),X);
    )
    

-- tensor product of more than two bundles
tensorProduct List := S -> (
    E := S#0;
    for i from 1 to length S -1 do (
        E = tensorProduct(E,S#i);
        );
    E
    )

HomogeneousVectorBundle * HomogeneousVectorBundle := (E1,E2) -> tensorProduct(E1,E2)    

FiltrationBundle * HomogeneousVectorBundle := (E1,E2) -> tensorProduct(E1,E2)    

HomogeneousVectorBundle * FiltrationBundle := (E1,E2) -> tensorProduct(E1,E2)    



-- aux functions for summing vector bundles
HashTable + HashTable := (T1,T2) -> (
    T := new MutableHashTable;
    for t1 in keys T1 do (
        if T2#?t1 then T#t1 = T1#t1 + T2#t1
        else T#t1 = T1#t1;
        );
    for t2 in keys T2 do (
        if not T#?t2 then T#t2 = T2#t2;
        );
    return T;
    )

ZZ ** HashTable := (m,T1) -> (
    T := new MutableHashTable;
    for t1 in keys T1 do (
        T#t1 = m * T1#t1;
        );
    return T;
    )

-- direct sum of two vector bundles
HomogeneousVectorBundle + HomogeneousVectorBundle := (E1,E2) -> (
    if not (E1#"underlyingVariety" == E2#"underlyingVariety") then error "expected vector bundles on the same variety";
    T1 := new MutableHashTable;
    T2 := new MutableHashTable;
    for i from 0 to #(E1#"weights")-1 do (
        T1#((E1#"weights")#i) = (E1#"multiplicities")#i;
        );
    for i from 0 to #(E2#"weights")-1 do (
        T2#((E2#"weights")#i) = (E2#"multiplicities")#i;
        );
    T1 = new HashTable from T1;
    T2 = new HashTable from T2;
    T := T1 + T2;
    homogeneousVectorBundle(keys T, values T, E1#"underlyingVariety")
    )

-------------------------- symmetric power -------------------------------------------------------------------------

-- symmetric power of a completely reducible vector bundle
symmetricPower (ZZ,HomogeneousVectorBundle) := (n,E) -> (
    X := E#"underlyingVariety";
    r := rank E;
    R := X#"rootSystem";
    S := homogeneousVectorBundle({},{},X);
    RP := E#"rootSystem";
    Ttemp := new MutableHashTable;
    T := new MutableHashTable;
    LX := sort toList ((set toList(1..rank X#"rootSystem")) - ((X#"parabolicSubgroup")#"parabolic")); 
    if n == 1 then return E;
    if n == 0 then return structureSheaf X;
    if r == 1 then return tensorProduct(toList apply(1..n, i -> E));
    if E#"irreducible" then (
        l := (E#"parabolicWeights")#0;
        if rank RP == 0 then Ttemp = new HashTable from {(l,1)} -- case of the complete flag
        else Ttemp = new HashTable from symmetricPower(n,l,RP);
        d := binomial(r+n-1,n-1)*chern(1,E)/binomial(r+n-1,n); 
        for t in keys Ttemp do (
            E' := homogeneousVectorBundle(toGlobalWeights({t},X#"parabolicSubgroup"),{1},X);
            c := lift(d-chern(1,E')/rank(E'),ZZ);
            l' := weight(R,mixWeights(t,entries c,LX));
            T#l' = Ttemp#t; 
            );
        T = new HashTable from T;
        E' := homogeneousVectorBundle(apply(keys T, t -> weight(R,entries t)), values T, X);
        return E';
        )
    else if #(E#"weights") == 1 then (
        m := (E#"multiplicities")#0;
        E' = homogeneousVectorBundle(E#"weights",{1},X);
        L := apply(partitions(n,m), k -> conjugate k);
        ListBundles := new MutableHashTable from {};
        listSym := multipleSymmetricPower(n,E');
        for l in L do (
            l' := join(toList l, m - #l : 0);
            ListBundles#(l') = tensorProduct (apply(l', i -> listSym#i));
            );
        ListBundles = new HashTable from ListBundles;
        M := kunnethPartitions(n,m);
        M = new HashTable from (tally M);
        for g in keys M do (
            for i from 1 to M#g do (
                S = S + ListBundles#g;
                );
            );	
        return S;
        )
    else (
        k := #(E#"weights");
        L = compositions(k,n);
        ListBundles = new MutableHashTable from {};
        listSym = for i from 0 to k-1 list (
            multipleSymmetricPower(n,homogeneousVectorBundle({(E#"weights")#i},{(E#"multiplicities")#i},X))
            );
        for l in L do (
            ListBundles#l = tensorProduct (toList apply(0..#l-1, i -> (listSym#i)#(l#i)));
            );
        ListBundles = new HashTable from ListBundles;
        for l in L do (
            S = S + ListBundles#l;
            );	
        return S;
        );
    )

-- returns the list of symmetric powers of B from 0 to k, computed inductively
multipleSymmetricPower = method();
multipleSymmetricPower(ZZ, HomogeneousVectorBundle) := (k,B) -> (
    if k < 0 then error "the power must be a non-negative integer";
    X := B#"underlyingVariety";
    if k == 0 then return {structureSheaf(X)};
    if k == 1 then return {structureSheaf(X),B};
    R := X#"rootSystem";
    S := homogeneousVectorBundle({},{},X);
    RP := B#"rootSystem";
    LX := sort toList ((set toList(1..rank X#"rootSystem")) - ((X#"parabolicSubgroup")#"parabolic")); 
    rB := rank B;
    output := {structureSheaf(X)};
    if B#"irreducible" then (
        l := (B#"parabolicWeights")#0; -- component without line bundle
        -- we build the symmetric powers of B in a smart way
        -- we keep in memory the computations already done
        if rank RP == 0 then Ttemp := apply(k+1, i -> new HashTable from {(l,1)}) -- case of the complete flag
        else Ttemp = multipleSymmetricPowerLie(k, l, RP);
        for i from 1 to k do (
            T := new MutableHashTable;
            d := binomial(rB+i-1,i-1)*chern(1,B)/binomial(rB+i-1,i); 
            -- this d must be the SLOPE of every summand that I compute with the representations
            for t in keys Ttemp#i do (
                B' := homogeneousVectorBundle(toGlobalWeights({t},X#"parabolicSubgroup"),{1},X);
                c := lift(d-chern(1,B')/rank(B'),ZZ);
                l' := weight(R,mixWeights(t,entries c,LX));
                if T#?(l') then T#(l') = T#(l') + (Ttemp#i)#t
                else T#(l') = (Ttemp#i)#t;
                );
            T = new HashTable from T;
            B' := homogeneousVectorBundle(apply(keys T, t -> weight(R,entries t)), values T, X);
            output = output | {B'};
            );
        return output;
        );
    -- non-irreducible case
    m := B#"multiplicities";
    M := B#"parabolicWeights";
    M' := B#"weights";
    result := new HashTable from pack (mingle (M,m),2);
    if rank RP == 0 then ( -- case of the complete flag
        result = new HashTable from apply(keys result, l -> (l,apply(k+1, i -> new HashTable from {(l,1)})));
        )
    else (
        result = multipleSymmetricPowerLie(k,result,RP); 
        result = new MutableHashTable from result;
        for v in keys result do (
            result#v = join(result#v, k - #(result#v) + 1 : new HashTable from {});
            );
        result = new HashTable from result;
        );
    len := #M;
    newResult := new MutableHashTable;
    -- case with a single summand
    if len == 1 then (
        m = m#0;
        w := M#0;
        j := 0;
        E := homogeneousVectorBundle({M'#j},{1},X);
        rE := rank E;
        newResult#w = {structureSheaf(X)};
        for i from 1 to k do (
            T := new MutableHashTable;
            S := homogeneousVectorBundle({},{},X);
            if keys(result#w)#i == {} then E' := S;
            if keys(result#w)#i =!= {} then (
                d := binomial(rE+i-1,i-1)*chern(1,E)/binomial(rE+i-1,i); 
                -- this d must be the SLOPE of every summand that I compute with the representations
                for t in keys (result#w)#i do (
                    E' := homogeneousVectorBundle(toGlobalWeights({t},X#"parabolicSubgroup"),{1},X);
                    c := lift(d-chern(1,E')/rank(E'),ZZ);
                    l' := weight(R,mixWeights(t,entries c,LX));
                    if T#?(l') then T#(l') = T#(l') + ((result#w)#i)#t
                    else T#(l') = ((result#w)#i)#t;
                    );
                T = new HashTable from T;
                E' = homogeneousVectorBundle(apply(keys T, t -> weight(R,entries t)), values T, X);
                );
            -- up to here I have turned all the powers of the first bundle into vector bundles
            newResult#w = newResult#w | {E'};
            ListBundles := new MutableHashTable from {};
            L := apply(partitions(i,m), s -> conjugate s);
            for l in L do (
                l' := join(toList l, m - #l : 0);
                ListBundles#(l') = tensorProduct (apply(l', s -> (newResult#w)#s));
                );
            ListBundles = new HashTable from ListBundles;
            N := kunnethPartitions(i,m);
            N = new HashTable from (tally N);
            for g in keys N do (
                for i from 1 to N#g do (
                    S = S + ListBundles#g;
                    );
                );	
            output = output | {S};
            );
        return join(output, k - #output + 1 : homogeneousVectorBundle({},{},X));
        );
    -- case with more than one summand
    powers := {};
    for j from 0 to len -1 do (
        output := {structureSheaf(X)};
        m' := m#j;
        w := M#j;
        E := homogeneousVectorBundle({M'#j},{1},X);
        rE := rank E;
        newResult#w = {structureSheaf(X)};
        for i from 1 to k do (
            T := new MutableHashTable;
            S := homogeneousVectorBundle({},{},X);
            if keys(result#w)#i =!= {} then (
                d := binomial(rE+i-1,i-1)*chern(1,E)/binomial(rE+i-1,i);
                -- this d must be the SLOPE of every summand that I compute with the representations
                for t in keys (result#w)#i do (
                    E' := homogeneousVectorBundle(toGlobalWeights({t},X#"parabolicSubgroup"),{1},X);
                    c := lift(d-chern(1,E')/rank(E'),ZZ);
                    l' := weight(R,mixWeights(t,entries c,LX));
                    if T#?(l') then T#(l') = T#(l') + ((result#w)#i)#t
                    else T#(l') = ((result#w)#i)#t;
                    );
                T = new HashTable from T;
                E' := homogeneousVectorBundle(apply(keys T, t -> weight(R,entries t)), values T, X);
                );
            if keys(result#w)#i == {} then E' = S;
            -- up to here I have turned all the powers of the first bundle into vector bundles
            newResult#w = newResult#w | {E'};
            ListBundles := new MutableHashTable from {};
            L := apply(partitions(i,m'), s -> conjugate s);
            for l in L do (
                l' := join(toList l, m' - #l : 0);
                ListBundles#(l') = tensorProduct (apply(l', s -> (newResult#w)#s));
                );
            ListBundles = new HashTable from ListBundles;
            N := kunnethPartitions(i,m');
            N = new HashTable from (tally N);
            for g in keys N do (
                for i from 1 to N#g do (
                    S = S + ListBundles#g;
                    );
                );	
            output = output | {S};
            );
        powers = powers | {join(output, k - #output + 1 : homogeneousVectorBundle({},{},X))};
        );
    output = {structureSheaf(X),B};
    for j from 2 to k do (
        S := homogeneousVectorBundle({},{},X);
        L := compositions(len,j);
        for l in L do (
            S = S + tensorProduct (apply(toList(0..#l-1), s -> (powers#s)#(l#s)));
            );
        output = output | {S};
        );
    return join(output, k - #output + 1 : {homogeneousVectorBundle({},{},X)});
    )

-- given A->B->C returns the list {V^kA, V^k-1A B,V^k-2A S^2B,...,A S^k-1B,V^kB} of which S^kC is the coker
-- clearly I am assuming that there is an injective map from A to B
cokernelSym = method();
cokernelSym (ZZ,HomogeneousVectorBundle,HomogeneousVectorBundle) := (k,A,B) -> (
    if k < 0 then error "the power must be a non-negative integer";
    X := A#"underlyingVariety";
    R := X#"rootSystem";
    P := X#"parabolicSubgroup";
    LX := sort toList ((set toList(1..rank R)) - (P#"parabolic")); 
    rA := rank A;
    rB := rank B;
    rC := rB - rA;
    if k == 0 then return structureSheaf(X);
    if k == 1 then return {A,B};
    if k > rC then return homogeneousVectorBundle({},{},X);
    -- we build the exterior powers of B in a smart way
    -- we keep in memory the computations already done
    outputA := multipleExteriorPower(k,A);
    outputB := multipleSymmetricPower(k,B);
    comp := compositions(2,k);
    output := {};
    for m in comp do (
        output = output | {tensorProduct(outputA#(m#0),outputB#(m#1))};
        );
    return output;
    )


-- given A->B->C returns the list {S^kB,S^k-1B C,S^k-2B V^2C,..V^kC} of which S^kA is the ker
-- clearly I am assuming that there is a surjective map from B to C
kernelSym = method();
kernelSym (ZZ,HomogeneousVectorBundle,HomogeneousVectorBundle) := (k,B,C) -> (
    if k < 0 then error "the power must be a non-negative integer";
    X := B#"underlyingVariety";
    R := X#"rootSystem";
    P := X#"parabolicSubgroup";
    LX := sort toList ((set toList(1..rank R)) - (P#"parabolic"));
    rB := rank B;
    rC := rank C;
    rA := rB - rC;
    if k == 0 then return structureSheaf(X);
    if k == 1 then return {B,C};
    if k > rA then return homogeneousVectorBundle({},{},X);
    -- we build the exterior powers of B in a smart way
    -- we keep in memory the computations already done
    outputC := multipleExteriorPower(k,C);
    outputB := multipleSymmetricPower(k,B);
    comp := compositions(2,k);
    output := {};
    for m in comp do (
        output = output | {tensorProduct(outputB#(m#0),outputC#(m#1))};
        );
    return output;
    )

-------------------------- exterior power ------------------------------------------------------------------------

-- exterior power of a homogeneous bundle
exteriorPower (ZZ,HomogeneousVectorBundle) := o -> (n,E) -> (
    X := E#"underlyingVariety";
    if n == 0 then return structureSheaf(X);
    if n == 1 then return E;
    r := rank E;
    if n == r then return determinant E;
    if n > r then return homogeneousVectorBundle({},{},X);
    if  n > r/2 then return tensorProduct(determinant E,exteriorPower(r-n,dual E));
    R := X#"rootSystem";
    S := homogeneousVectorBundle({},{},X);
    RP := E#"rootSystem";
    Ttemp := new MutableHashTable;
    T := new MutableHashTable;
    LX := sort toList ((set toList(1..rank X#"rootSystem")) - ((X#"parabolicSubgroup")#"parabolic")); 
    if E#"irreducible" then (
        l := (E#"parabolicWeights")#0; -- component without line bundle
        if rank RP == 0 then Ttemp = new HashTable from {(l,1)} -- case of the complete flag
        else Ttemp = new HashTable from exteriorPower(n,l,RP);
        d := binomial(r-1,n-1)*chern(1,E)/binomial(r,n); 
        -- this d must be the SLOPE of every summand that I compute with the representations
        for t in keys Ttemp do (
            E' := homogeneousVectorBundle(toGlobalWeights({t},X#"parabolicSubgroup"),{1},X);
            c := lift(d-chern(1,E')/rank(E'),ZZ);
            l' := weight(R,mixWeights(t,entries c,LX));
            T#(l') = Ttemp#t;
            );
        T = new HashTable from T;
        E' := homogeneousVectorBundle(apply(keys T, t -> weight(R,entries t)), values T, X);
        return E';
        )
    else if #(E#"weights") == 1 then (
        m := (E#"multiplicities")#0;
        E' = homogeneousVectorBundle(E#"weights",{1},X);
        L := apply(partitions(n,m), k -> conjugate k);
        ListBundles := new MutableHashTable from {};
        listExt := multipleExteriorPower(n, E');
        for l in L do (
            l' := join(toList l, m - #l : 0);
            ListBundles#(l') = tensorProduct (apply(toList l, i -> listExt#i));
            );
        ListBundles = new HashTable from ListBundles;
        M := kunnethPartitions(n,m);
        M = new HashTable from (tally M);
        for g in keys M do (
            for i from 1 to M#g do (
                S = S + ListBundles#g;
                );
            );	
        return S;
        )
    else (
        k := #(E#"weights");
        L = compositions(k,n);
        ListBundles = new MutableHashTable from {};
        listExt = for i from 0 to k-1 list (
            multipleExteriorPower(n,homogeneousVectorBundle({(E#"weights")#i},{(E#"multiplicities")#i},X))
            );
        for l in L do (
            ListBundles#l = tensorProduct (apply(toList(0..#l-1), i -> (listExt#i)#(l#i)));
            );
        ListBundles = new HashTable from ListBundles;
        for l in L do (
            S = S + ListBundles#l;
            );	
        return S;
        );
    )



-- inductive function that computes the multiplicities of the partitions in the kunneth formula
kunnethPartitions = method();
kunnethPartitions (ZZ,ZZ) := (n,m) -> (
    if m == 1 then return {{n}};
    memory := {};
    for k from 0 to n do (
        R := kunnethPartitions(n-k,m-1);
        for l in R do (
            memory = memory | {flatten kunnethPartitions(k,1) | flatten l};
            );
        );
    apply(memory, i -> reverse sort i)
    );


-- returns the list of exterior powers of B from 0 to k, computed inductively
multipleExteriorPower = method();
multipleExteriorPower(ZZ, HomogeneousVectorBundle) := (k,B) -> (
    if k < 0 then error "the power must be a non-negative integer";
    X := B#"underlyingVariety";
    if k == 0 then return {structureSheaf(X)};
    if k == 1 then return {structureSheaf(X),B};
    R := X#"rootSystem";
    S := homogeneousVectorBundle({},{},X);
    RP := B#"rootSystem";
    LX := sort toList ((set toList(1..rank X#"rootSystem")) - ((X#"parabolicSubgroup")#"parabolic")); 
    rB := rank B;
    output := {structureSheaf(X)};
    -- we build the exterior powers of B in a smart way
    -- we keep in memory the computations already done
    if B#"irreducible" then (
        l := (B#"parabolicWeights")#0; -- component without line bundle
        if rank RP == 0 then Ttemp := apply(k+1, i -> new HashTable from {(l,1)}) -- case of the complete flag
        else Ttemp = multipleExteriorPowerLie(k, l, RP);
        for i from 1 to min(k, rB) do (
            T := new MutableHashTable;
            d := binomial(rB-1,i-1)*chern(1,B)/binomial(rB,i); 
            -- this d must be the SLOPE of every summand that I compute with the representations
            for t in keys Ttemp#i do (
                B' := homogeneousVectorBundle(toGlobalWeights({t},X#"parabolicSubgroup"),{1},X);
                c := lift(d-chern(1,B')/rank(B'),ZZ);
                l' := weight(R,mixWeights(t,entries c,LX));
                if T#?(l') then T#(l') = T#(l') + (Ttemp#i)#t
                else T#(l') = (Ttemp#i)#t;
                );
            T = new HashTable from T;
            B' := homogeneousVectorBundle(apply(keys T, t -> weight(R,entries t)), values T, X);
            output = output | {B'};
            );
        return join(output, k - #output + 1 : homogeneousVectorBundle({},{},X));
        );
    -- non-irreducible case
    m := B#"multiplicities";
    M := B#"parabolicWeights";
    M' := B#"weights";
    result := new HashTable from pack (mingle (M,m),2);
    if rank RP == 0 then ( -- case of the complete flag
        result = new HashTable from apply(keys result, l -> (l,apply(max(rB+1,k+1), i -> new HashTable from {(l,1)})));
        )
    else (
        result = multipleExteriorPowerLie(k,result,RP); 
        result = new MutableHashTable from result;
        for v in keys result do (
            result#v = join(result#v, k - #(result#v) + 1 : new HashTable from {});
            );
        result = new HashTable from result;
        );
    len := #M;
    newResult := new MutableHashTable;
    -- case with a single summand
    if len == 1 then (
        m = m#0;
        w := M#0;
        j := 0;
        E := homogeneousVectorBundle({M'#j},{1},X);
        rE := rank E;
        newResult#w = {structureSheaf(X)};
        for i from 1 to min(k, rB) do (
            T := new MutableHashTable;
            S := homogeneousVectorBundle({},{},X);
            if keys(result#w)#i =!= {} then (
                d := binomial(rE-1,i-1)*chern(1,E)/binomial(rE,i); 
                -- this d must be the SLOPE of every summand that I compute with the representations
                for t in keys (result#w)#i do (
                    E' := homogeneousVectorBundle(toGlobalWeights({t},X#"parabolicSubgroup"),{1},X);
                    c := lift(d-chern(1,E')/rank(E'),ZZ);
                    l' := weight(R,mixWeights(t,entries c,LX));
                    if T#?(l') then T#(l') = T#(l') + ((result#w)#i)#t
                    else T#(l') = ((result#w)#i)#t;
                    );
                T = new HashTable from T;
                E' := homogeneousVectorBundle(apply(keys T, t -> weight(R,entries t)), values T, X);
                );
            if keys(result#w)#i == {} then E' = S;
            -- up to here I have turned all the powers of the first bundle into vector bundles
            newResult#w = newResult#w | {E'};
            ListBundles := new MutableHashTable from {};
            L := apply(partitions(i,m), s -> conjugate s);
            for l in L do (
                l' := join(toList l, m - #l : 0);
                ListBundles#(l') = tensorProduct (apply(l', s -> (newResult#w)#s));
                );
            ListBundles = new HashTable from ListBundles;
            N := kunnethPartitions(i,m);
            N = new HashTable from (tally N);
            for g in keys N do (
                for i from 1 to N#g do (
                    S = S + ListBundles#g;
                    );
                );	
            output = output | {S};
            );
        return join(output, k - #output + 1 : homogeneousVectorBundle({},{},X));
        );
    -- case with more than one summand
    powers := {};
    for j from 0 to len -1 do (
        output := {structureSheaf(X)};
        m' := m#j;
        w := M#j;
        E := homogeneousVectorBundle({M'#j},{1},X);
        rE := rank E;
        newResult#w = {structureSheaf(X)};
        for i from 1 to k do (
            T := new MutableHashTable;
            S := homogeneousVectorBundle({},{},X);
            if i <= rE and keys(result#w)#i =!= {} then (
                d := binomial(rE-1,i-1)*chern(1,E)/binomial(rE,i);
                -- this d must be the SLOPE of every summand that I compute with the representations
                for t in keys (result#w)#i do (
                    E' := homogeneousVectorBundle(toGlobalWeights({t},X#"parabolicSubgroup"),{1},X);
                    c := lift(d-chern(1,E')/rank(E'),ZZ);
                    l' := weight(R,mixWeights(t,entries c,LX));
                    if T#?(l') then T#(l') = T#(l') + ((result#w)#i)#t
                    else T#(l') = ((result#w)#i)#t;
                    );
                T = new HashTable from T;
                E' := homogeneousVectorBundle(apply(keys T, t -> weight(R,entries t)), values T, X);
                );
            if i > rE or keys(result#w)#i == {} then E' = S;
            -- up to here I have turned all the powers of the first bundle into vector bundles
            newResult#w = newResult#w | {E'};
            ListBundles := new MutableHashTable from {};
            L := apply(partitions(i,m'), s -> conjugate s);
            for l in L do (
                l' := join(toList l, m' - #l : 0);
                ListBundles#(l') = tensorProduct (apply(l', s -> (newResult#w)#s));
                );
            ListBundles = new HashTable from ListBundles;
            N := kunnethPartitions(i,m');
            N = new HashTable from (tally N);
            for g in keys N do (
                for i from 1 to N#g do (
                    S = S + ListBundles#g;
                    );
                );	
            output = output | {S};
            );
        powers = powers | {join(output, k - #output + 1 : homogeneousVectorBundle({},{},X))};
        );
    output = {structureSheaf(X),B};
    for j from 2 to k do (
        S := homogeneousVectorBundle({},{},X);
        L := compositions(len,j);
        for l in L do (
            S = S + tensorProduct (apply(toList (0..#l-1), s -> (powers#s)#(l#s)));
            );
        output = output | {S};
        );
    return join(output, k - #output + 1 : {homogeneousVectorBundle({},{},X)});
    )


-- computes the factors that make up the filtration to compute the wedge power
-- of a list of bundles representing the filtration bundle E1->E->F0
-- this convention is the one Macaulay2 uses for the order of compositions in compositions
-- what it returns is the filtration bundle that has as factors the powers of the factors of E
-- given by compositions(number of factors of E,k)
centralMemberWedge = method();
centralMemberWedge (ZZ,List) := (k,S) -> (
    X := (S#0)#"underlyingVariety";
    C := compositions(length S,k);
    F := apply(C, c -> tensorProduct(for i from 0 to length S - 1 list exteriorPower(c#i,S#i)));
    filtrationBundle(F,X)
    )


-- given A->B->C returns the list {S^kA, S^k-1A B,S^k-2A V^2B,...,A V^k-1B,V^kB} of which V^kC is the coker
-- clearly I am assuming that there is an injective map from A to B
cokernelWedge = method();
cokernelWedge (ZZ,HomogeneousVectorBundle,HomogeneousVectorBundle) := (k,A,B) -> (
    if k < 0 then error "the power must be a non-negative integer";
    X := A#"underlyingVariety";
    R := X#"rootSystem";
    P := X#"parabolicSubgroup";
    LX := sort toList ((set toList(1..rank R)) - (P#"parabolic")); 
    rA := rank A;
    rB := rank B;
    rC := rB - rA;
    if k == 0 then return structureSheaf(X);
    if k == 1 then return {A,B};
    if k > rC then return homogeneousVectorBundle({},{},X);
    if  k > rC/2 then (
        -- I compute the determinant of C (exact, does not depend on the map)
        c := entries (chern(1,B) - chern(1,A));
        P = sort toList P#"parabolic";
        L := {};
        for i from 1 to rank R do (
            if not isMember(i,P) then (
                L = L | {c#0};
                c = drop(c,1);
                )
            else (
                L = L | {0};
                );
            );
        detC := homogeneousVectorBundle({weight(R,L)},{1},X);
        return apply(kernelWedge(rC-k,dual B, dual A), f -> tensorProduct(detC,f));
        );
    -- we build the exterior powers of B in a smart way
    -- we keep in memory the computations already done
    outputB := multipleExteriorPower(k,B);
    outputA := multipleSymmetricPower(k,A);
    comp := compositions(2,k);
    output := {};
    for m in comp do (
        output = output | {tensorProduct(outputA#(m#0),outputB#(m#1))};
        );
    return output;
    )


-- given A->B->C returns the list {V^kB,V^k-1B C,V^k-2B S^2C,..S^kC} of which V^kA is the ker
-- clearly I am assuming that there is a surjective map from B to C
kernelWedge = method();
kernelWedge (ZZ,HomogeneousVectorBundle,HomogeneousVectorBundle) := (k,B,C) -> (
    if k < 0 then error "the power must be a non-negative integer";
    X := B#"underlyingVariety";
    R := X#"rootSystem";
    P := X#"parabolicSubgroup";
    LX := sort toList ((set toList(1..rank R)) - (P#"parabolic")); 
    rB := rank B;
    rC := rank C;
    rA := rB - rC;
    if k == 0 then return structureSheaf(X);
    if k == 1 then return {B,C};
    if k > rA then return homogeneousVectorBundle({},{},X);
    if  k > rA/2 then (
        -- I compute the determinant of A (exact, does not depend on the map)
        c := entries (chern(1,B) - chern(1,C));
        P = sort toList P#"parabolic";
        L := {};
        for i from 1 to rank R do (
            if not isMember(i,P) then (
                L = L | {c#0};
                c = drop(c,1);
                )
            else (
                L = L | {0};
                );
            );
        detA := homogeneousVectorBundle({weight(R,L)},{1},X);
        return apply(cokernelWedge(rA-k,dual B, dual C), f -> tensorProduct(detA,f));
        );
    -- we build the exterior powers of B in a smart way
    -- we keep in memory the computations already done
    outputB := multipleExteriorPower(k,B);
    outputC := multipleSymmetricPower(k,C);
    comp := compositions(2,k);
    output := {};
    for m in comp do (
        output = output | {tensorProduct(outputB#(m#0),outputC#(m#1))};
        );
    return output;
    )
    
    
exteriorPower (ZZ,FiltrationBundle) := o -> (n,E) -> (
    X := E#"underlyingVariety";
    r := rank E;
    if n == 0 then return structureSheaf(X);
    if n == 1 then return E;
    if n > r then return homogeneousVectorBundle({},{},X);
    if n == r then return determinant E;
    if  n > r/2 then return tensorProduct(exteriorPower(r-n,dual E),determinant E);
    centralMemberWedge (n,E#"factors")
    )



multipleExteriorPower(ZZ, FiltrationBundle) := (k,B) -> (
    if k < 0 then error "the power must be a non-negative integer";
    X := B#"underlyingVariety";
    S := B#"factors";
    if k == 0 then return {structureSheaf(X)};
    if k == 1 then return {structureSheaf(X),B};
    -- we build the exterior powers of B in a smart way
    -- we keep in memory the computations already done
    output := {structureSheaf(X),B}; 
    S = apply(S, s -> multipleExteriorPower(k,s)); -- I do all the heavy computations here
    for j from 2 to k do (
        C := compositions(length S,j);
        F := apply(C, c -> tensorProduct(for i from 0 to length S - 1 list (S#i)_(c#i)));
        output = output | {filtrationBundle(F,X)};
        );
    output
    )



------------------------ koszul --------------------------------------------------------------------


-- builds the Koszul complex of Y=(X,F) as a list of bundles {E_0=O_X,E_1=F',...,E_r=det(F')}
koszul (HomogeneousVariety,HomogeneousVectorBundle) := (X,F) -> (
    if F#?"koszul" then (
        return F#"koszul";
        )
    else (
        F' := dual F;
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
        return F#"koszul";
        );
    )


--------------------------- determinant ------------------------------------------------------------

determinantCoeff = method();
determinantCoeff(RootSystem,ZZ,Vector) := (R,k,v) -> (
    w := new MutableList from rank R:0;
    w#(k-1) = 1;
    w = weight(R,toList w);
    return scalarProduct(R,weight(R,entries v),w)/scalarProduct(R,w,w);
    );

determinant HomogeneousVectorBundle := o -> E -> (
    X := E#"underlyingVariety";
    R := X#"rootSystem";
    P := X#"parabolicSubgroup";
    P0 := P#"parabolic";
    if E#"irreducible" then (
        r := rank E;
        if r == 1 then return E;
        t := (E#"weights")#0;
        indexes := flatten {0,sort toList(set(1..rank(R))-P0),1+rank R};
        l := new MutableList from (rank R:0);
        for i from 1 to #indexes - 2 do (
            L := sort toList connectedComponent(R,P0,indexes#i);
            pos := position(L, l -> l == indexes#i);
            R' := rootSystem(R,parabolic(R,set L));
            L = L/(i -> i -1 );
            coeff := determinantCoeff(R',pos+1, t^L);
            l#(indexes#i-1) = lift(coeff*r,ZZ);
            );
        l = weight(R,toList l);
        return homogeneousVectorBundle({l},{1},X);
        )
    else if #(E#"weights") == 1 then (	
        t = (E#"weights")#0;
        r = rank homogeneousVectorBundle({t},{1},X);
        indexes = flatten {0,sort toList(set(1..rank(R))-P0),1+rank R};
        l = new MutableList from (rank R:0);
        for i from 1 to #indexes - 2 do (
            L := sort toList connectedComponent(R,P0,indexes#i);
            pos := position(L, l -> l == indexes#i);
            R' := rootSystem(R,parabolic(R,set L));
            L = L/(i -> i -1 );
            coeff := determinantCoeff(R',pos+1, t^L);
            l#(indexes#i-1) = lift(coeff*r,ZZ);
            );
        l = weight(R,toList l);
        return homogeneousVectorBundle({((E#"multiplicities")#0)*l},{1},X);
        )
    else (
        M := {};
        indexes = flatten {0,sort toList(set(1..rank(R))-P0),1+rank R};
        for t in E#"weights" do (
            r = rank homogeneousVectorBundle({t},{1},X);
            l = new MutableList from (rank R:0);
            for i from 1 to #indexes - 2 do (
                L := sort toList connectedComponent(R,P0,indexes#i);
                pos := position(L, l -> l == indexes#i);
                R' := rootSystem(R,parabolic(R,set L));
                L = L/(i -> i -1 );
                coeff := determinantCoeff(R',pos+1, t^L);
                l#(indexes#i-1) = lift(coeff*r,ZZ);
                );
            l = toList l;
            M = M | {l};
            );
        M = transpose matrix M;
        l = M * vector(E#"multiplicities");
        return homogeneousVectorBundle({l},{1},X);
        );
    )


determinant FiltrationBundle := o -> F -> (
    c := entries chern(1,F);
    X := F#"underlyingVariety";
    R := X#"rootSystem";
    P := X#"parabolicSubgroup";
    P = sort toList P#"parabolic";
    L := {};
    for i from 1 to rank R do (
        if not isMember(i,P) then (
            L = L | {c#0};
            c = drop(c,1);
            )
        else (
            L = L | {0};
            );
        );
    homogeneousVectorBundle({weight(R,L)},{1},X)
    )






------------------------------------------------ the following methods are taken from WeylGroups ---------------------------------------------------------

--(internal function) Finding the labels of the neighbors of the i-th vertex in a Dynkin diagram
neighbors = method()
neighbors(DynkinDiagram,ZZ) := (D,i) -> set join(D#(i-1)#0,D#(i-1)#1,D#(i-1)#2,D#(i-1)#3,D#(i-1)#4)

--(internal function) Reindexing elements according to a list to obtain a coherent Dynkin diagram
reindex=(D,L)->
(
    T:=new HashTable from for i from 0 to #L-1 list L#i => i+1; 
    new DynkinDiagram from apply(D,x->applyTable(x,z->T#z))
    )



-- given a root system with nodes marked by P, and given k not in P,
-- I want to find the parts of P that attach to k
connectedComponent = method();
connectedComponent (RootSystem,Parabolic,ZZ) := (R,P0,k) ->(
    f := P0 + set {k}; -- I also mark k
    D := dynkinDiagram R;
    P :=  sort toList P0;
    test := 0; -- if this value is 0 then it must repeat the loop because I have updated the connected component
    vertcompolist:=set {k}; -- will contain the sets of vertices of P that attach to k
    while test == 0 do (
        test = 1;
        for i in P do (
            if neighbors(D,i)*vertcompolist =!=set{} and not member(i,vertcompolist) then (
                vertcompolist = vertcompolist + set{i};
                test = 0;
                );
            );
        );
    return vertcompolist;
    );

    
--Finding the connected components of a Dynkin diagram
myConnectedComponents=method()
myConnectedComponents(DynkinDiagram) := (D) ->
(
    vertcompolist:={}; --will contain the sets of vertices of the connected components
    for i from 1 to #D do
    (
        verticompo:=set{i}; --set of vertices of connected component of i
        vertnoticompolist:={}; --list of other connected components vertices sets 
        for j from 0 to #vertcompolist-1 do if neighbors(D,i)*vertcompolist#j =!=set{} then 
        (
            verticompo=vertcompolist#j+verticompo;
            )
        else 
        (
            vertnoticompolist=append(vertnoticompolist,vertcompolist#j);
            );
        vertcompolist=append(vertnoticompolist,verticompo);
        );
    vertcompolist=apply(vertcompolist,toList); --transform the connected components into lists
    vertcompolist=apply(vertcompolist,sort); --sort each of these lists 
    dyncompolist:=applyTable(vertcompolist,i->D#(i-1)); --make a list with the edge data of the corresponding vertices
    --apply(dyncompolist,vertcompolist,reindex)
    new DynkinDiagram from dyncompolist
    )
    
--Finding the connected components of a Dynkin diagram
--connectedComponents=method()
connectedComponents(DynkinDiagram) := (D) ->
(
    vertcompolist:={}; --will contain the sets of vertices of the connected components
    for i from 1 to #D do
    (
        verticompo:=set{i}; --set of vertices of connected component of i
        vertnoticompolist:={}; --list of other connected components vertices sets 
        for j from 0 to #vertcompolist-1 do if neighbors(D,i)*vertcompolist#j =!=set{} then 
        (
            verticompo=vertcompolist#j+verticompo;
            )
        else 
        (
            vertnoticompolist=append(vertnoticompolist,vertcompolist#j);
            );
        vertcompolist=append(vertnoticompolist,verticompo);
        );
    vertcompolist=apply(vertcompolist,toList); --transform the connected components into lists
    vertcompolist=apply(vertcompolist,sort); --sort each of these lists 
    dyncompolist:=applyTable(vertcompolist,i->D#(i-1)); --make a list with the edge data of the corresponding vertices
    apply(dyncompolist,vertcompolist,reindex)
    --new DynkinDiagram from (dyncompolist,vertcompolist)
    )

    
--(internal function) Returning the position (in the list) of the small side of a double edge of an irreducible Dynkin diagram if it has one.
smallSideDoubleEdge= (D)->
(
    for i from 0 to #D-1 do if (D#i#2 !={}) then return({i});
    return({});
    ) 

--(internal function) Returning the position (in the list) of the big side of a double edge of an irreducible Dynkin diagram if it has one.
bigSideDoubleEdge= (D)->
(
    for i from 0 to #D-1 do if (D#i#1 !={}) then return({i});
    return({});	
    ) 

--(internal function) Testing if an irreducible Dynkin diagram has a triple edge 
isG2= (D)->
(
    for i from 0 to #D-1 do if (D#i#3 !={} or D#i#4 !={}) then return(true);
    return(false);
    ) 

--(internal function) Returning the number (ie at least 1) of the branch point of an irreducible Dynkin diagram if it has one 
branchPoint= (D)->
(
    for i from 0 to #D-1 do if (#(D#i#0)>2) then return({i+1});
    return({});	
    ) 

--(internal function) Says whether the vertex at position i (as in a list) in the Dynkin diagram D is an end point. 
isEndPoint=(D,i)->
(
    (#(D#i#0)+#(D#i#1)+#(D#i#2)<2)
    )

--Finds the indices all end points of a Dynkin diagram D
--endVertices=method()
endVertices(DynkinDiagram) := (D) ->
(
    set for i from 0 to #D-1 list if (#(D#i#0)+#(D#i#1)+#(D#i#2)<2) then i+1 else continue
    )




--(internal function) Finding the type of a connected Dynkin diagram (it should not be empty)
connectedDynkinType=method()
connectedDynkinType(DynkinDiagram):=(D)->
(
    if isG2(D) then {"G",2}
    else
    (
        brp:=branchPoint(D);
        if brp=={} then
        (
            sm:=smallSideDoubleEdge(D);
            bg:=bigSideDoubleEdge(D);
            if sm=={} and bg=={} then {"A",rank(D)}
            else
            (
                if isEndPoint(D,sm#0) then {"B",rank(D)}
                else if isEndPoint(D,bg#0) then {"C", rank(D)} else {"F",4}
                )
            )
        else
        (
            if #(endVertices(D)*neighbors(D,brp#0))>1 then {"D",rank(D)} else {"E", rank(D)}
            )
        )
    )

--dynkinType = method()
dynkinType(DynkinDiagram) := (D) ->
new DynkinType from apply(connectedComponents(D),connectedDynkinType) -- I only changed this part to remove the sort
-- this way the order is preserved but the pieces are still sometimes flipped

dynkinType(RootSystem) := (R) -> dynkinType(dynkinDiagram(R))

---------------------------------------------- dual of bundles -------------------------------------------------------------------------

repDual = method();
repDual (DynkinType,Weight) := (D,l) -> (
    n := (D#0)#1 - 1;
    if (D#0)#0 == "A" then (
        l = reverse entries l;
        )
    else if (D#0)#0 == "D" and (D#0)#1 % 2 == 1 then (
        l =  entries l^(flatten {toList(0..n-2),n,n-1});
        )
    else if (D#0)#0 == "E" and (D#0)#1 == 6 then (
        l = entries l^{5,1,4,3,2,0};
        )
    else (
        l = entries l;
        );
    return weight(rootSystem D,l);
    )


pieces = method();
pieces (DynkinDiagram,List) := (D,nodi) -> (
    n := #D;
    l := {};
    lTemp := {};
    for i from 0 to n-1 do (
        if isMember(i+1,lTemp) then (
            lTemp = unique(lTemp | flatten D#i);
            )
        else (
            if i =!= 0 then l = l | {lTemp};
            lTemp = unique flatten {i+1,flatten D#i};
            );
        );
    l = l | {lTemp};
    L := {};
    for i in nodi do (
        for m in l do (
            M := {};
            for j in m do (
                if j >= i then (
                    M = M | {j+1};
                    )		    
                else (
                    M = M | {j};
                    );
                );
            L = L | {M};
            );
        l = L;
        L = {};
        );
    return l;
    )


dual HomogeneousVectorBundle := E -> (
    E = E#1; -- I need this because in the definition of dual the options were written in a strange way...
    X := E#"underlyingVariety";
    R := X#"rootSystem";
    P := X#"parabolicSubgroup";
    if X#"picardRank" == ((X#"dynkinType")#0)#1 then ( -- case of the complete flag
        return homogeneousVectorBundle(apply(E#"weights", t -> -t),E#"multiplicities",X);
        );
    -- inserted to avoid a problem with the numbering of the E's
    if ((dynkinType(R))#0)#0 == "E" then return dualE E;
    RP := rootSystem(R,P#"parabolic");
    D := apply(connectedComponents(dynkinDiagram RP),connectedDynkinType);
    piece := pieces(dynkinDiagram RP, sort toList(set(toList(1..rank R)) - P#"parabolic"));
    piece = piece/(s -> sort s);
    d := 0;
    -- cases other than E
    if E#"irreducible" then (
        r := rank E;
        t := (E#"weights")#0;
        t1 := (toGlobalWeights(E#"parabolicWeights",P))#0; -- component without line bundle
        c1 := chern(1,homogeneousVectorBundle({t1},{1},X));
        l := {};
        if #D =!= #piece then error "I am making a mistake in splitting for the dual";
        for k from 0 to #D-1 do (
            d = dynkinType {D#k};
            -- this check only serves to avoid an indexing error in the two antennae of type D
            if ((dynkinType(R))#0)#0 == "D" and d#0 == {"A", 3} and k == #D -1 and not member(rank R - 3, P#"parabolic")  then l = l | {first entries t^(apply(piece#k,i -> i-1))} | {last entries t^(apply(piece#k,i -> i-1))} | {(entries t^(apply(piece#k,i -> i-1)))#1}
            else l = l | entries repDual(d,weight(rootSystem d, t^(apply(piece#k,i -> i-1))));
            );
        l = weight(RP,l);
        t2 := (toGlobalWeights({l},P))#0;
        c2 := chern(1,homogeneousVectorBundle({t2},{1},X));
        v := -(c1 + c2)/r;
        v = mixWeights((toParabolicWeights({t2},P))#0, (entries v)/(i -> lift(i,ZZ)), toList ((set toList(1..rank R)) - (P#"parabolic")));
        t3 := t - t1; -- line bundle component
        return homogeneousVectorBundle({weight(R, entries(vector v-t3))},{1},X);
        )
    else if #(E#"weights") == 1 then (
        t = (E#"weights")#0;
        r = rank homogeneousVectorBundle({t},{1},X);
        t1 = (toGlobalWeights(E#"parabolicWeights",P))#0; -- component without line bundle
        c1 = chern(1,homogeneousVectorBundle({t1},{1},X));
        l = {};
        for k from 0 to #D-1 do (
            d = dynkinType {D#k};
            if ((dynkinType(R))#0)#0 == "D" and d#0 == {"A", 3} and k == #D -1 and not member(rank R - 3, P#"parabolic")  then l = l | {first entries t^(apply(piece#k,i -> i-1))} | {last entries t^(apply(piece#k,i -> i-1))} | {(entries t^(apply(piece#k,i -> i-1)))#1}
            else l = l | entries repDual(d,weight(rootSystem d, t^(apply(piece#k,i -> i-1))));
            );
        l = weight(RP,l);
        t2 = (toGlobalWeights({l},P))#0;
        c2 = chern(1,homogeneousVectorBundle({t2},{1},X));
        v = -(c1 + c2)/r;
        v = mixWeights((toParabolicWeights({t2},P))#0, (entries v)/(i -> lift(i,ZZ)), toList ((set toList(1..rank X#"rootSystem")) - ((X#"parabolicSubgroup")#"parabolic")));
        t3 = t - t1; -- line bundle component
        return homogeneousVectorBundle({weight(R, entries(vector v-t3))},E#"multiplicities",X);
        )
    else (
        w := {};
        for i from 0 to #(E#"weights")-1 do (
            t = (E#"weights")#i;
            r = rank homogeneousVectorBundle({t},{1},X);
            t1 = (toGlobalWeights(E#"parabolicWeights",P))#i; -- component without line bundle
            c1 = chern(1,homogeneousVectorBundle({t1},{1},X));
            l = {};
            for k from 0 to #D-1 do (
                d = dynkinType {D#k};
                if ((dynkinType(R))#0)#0 == "D" and d#0 == {"A", 3} and k == #D -1 and not member(rank R - 3, P#"parabolic")  then l = l | {first entries t^(apply(piece#k,i -> i-1))} | {last entries t^(apply(piece#k,i -> i-1))} | {(entries t^(apply(piece#k,i -> i-1)))#1}
                else l = l | entries repDual(d,weight(rootSystem d, t^(apply(piece#k,i -> i-1))));
                );
            l = weight(RP,l);
            t2 = (toGlobalWeights({l},P))#0;
            c2 = chern(1,homogeneousVectorBundle({t2},{1},X));
            v = -(c1 + c2)/r;
            v = mixWeights((toParabolicWeights({t2},P))#0, (entries v)/(i -> lift(i,ZZ)), toList ((set toList(1..rank X#"rootSystem")) - ((X#"parabolicSubgroup")#"parabolic")));
            t3 = t - t1; -- line bundle component
            w = w | {weight(R, entries(vector v-t3))};
            );
        return homogeneousVectorBundle(w,E#"multiplicities",X);
        );
    )

-- dual in the type E case
dualE = method();
dualE HomogeneousVectorBundle := E -> (
    X := E#"underlyingVariety";
    R := X#"rootSystem";
    P := X#"parabolicSubgroup";
    RP := rootSystem(R,P#"parabolic");
    if E#"irreducible" then (
        r := rank E;
        t := (E#"weights")#0;
        t1 := (toGlobalWeights(E#"parabolicWeights",P))#0; -- component without line bundle
        c1 := chern(1,homogeneousVectorBundle({t1},{1},X));
        l := weight(RP, dualListE(t,X));
        t2 := (toGlobalWeights({l},P))#0;
        c2 := chern(1,homogeneousVectorBundle({t2},{1},X));
        v := -(c1 + c2)/r;
        v = mixWeights((toParabolicWeights({t2},P))#0, (entries v)/(i -> lift(i,ZZ)), toList ((set toList(1..rank R)) - (P#"parabolic")));
        t3 := t - t1; -- line bundle component
        return homogeneousVectorBundle({weight(R, entries(vector v-t3))},{1},X);
        )
    else if #(E#"weights") == 1 then (
        t = (E#"weights")#0;
        r = rank homogeneousVectorBundle({t},{1},X);
        t1 = (toGlobalWeights(E#"parabolicWeights",P))#0; -- component without line bundle
        c1 = chern(1,homogeneousVectorBundle({t1},{1},X));
        l = weight(RP, dualListE(t,X));
        t2 = (toGlobalWeights({l},P))#0;
        c2 = chern(1,homogeneousVectorBundle({t2},{1},X));
        v = -(c1 + c2)/r;
        v = mixWeights((toParabolicWeights({t2},P))#0, (entries v)/(i -> lift(i,ZZ)), toList ((set toList(1..rank X#"rootSystem")) - ((X#"parabolicSubgroup")#"parabolic")));
        t3 = t - t1; -- line bundle component
        return homogeneousVectorBundle({weight(R, entries(vector v-t3))},E#"multiplicities",X);
        )
    else (
        w := {};
        for i from 0 to #(E#"weights")-1 do (
            t = (E#"weights")#i;
            r = rank homogeneousVectorBundle({t},{1},X);
            t1 = (toGlobalWeights(E#"parabolicWeights",P))#i; -- component without line bundle
            c1 = chern(1,homogeneousVectorBundle({t1},{1},X));
            l = weight(RP, dualListE(t,X));
            t2 = (toGlobalWeights({l},P))#0;
            c2 = chern(1,homogeneousVectorBundle({t2},{1},X));
            v = -(c1 + c2)/r;
            v = mixWeights((toParabolicWeights({t2},P))#0, (entries v)/(i -> lift(i,ZZ)), toList ((set toList(1..rank X#"rootSystem")) - ((X#"parabolicSubgroup")#"parabolic")));
            t3 = t - t1; -- line bundle component
            w = w | {weight(R, entries(vector v-t3))};
            );
        return homogeneousVectorBundle(w,E#"multiplicities",X);
        );
    );

    

dual FiltrationBundle := F -> (
    F = F#1; -- I need this because in the definition of dual the options were written in a strange way...
    X := F#"underlyingVariety";
    S := F#"factors";
    S = reverse S;
    S = apply(S, s -> dual s);
    filtrationBundle(S,X)
    )
    
-- aux method for duality in the E case
dualListE = method();
dualListE (Vector,HomogeneousVariety) := (t,X) -> (
    R := X#"rootSystem";
    P := X#"parabolicSubgroup";
    RP := rootSystem(R,P#"parabolic");
    d := 0;
    l := {};
    -- case in which 4 is marked
    if not member(4,P#"parabolic") then (
        if not member(2,P#"parabolic") then (
            if not member(3,P#"parabolic") then (
                if member(1,P#"parabolic") then l = entries t^{0};
                )
            else (
                if member(1,P#"parabolic") then l = entries t^{2,0}
                else  l = entries t^{2};
                );
            -- I complete l with what happens in the entries after 4
            R' := rootSystem(rootSystemA(rank R -4), parabolic(rootSystemA(rank R -4),set ((toList(P#"parabolic" * set{5,6,7,8}))/(i -> i -4))));
            D := apply(connectedComponents(dynkinDiagram R'),connectedDynkinType);
            piece := pieces(dynkinDiagram R', sort toList(set(toList(1..rank R)) - set ((toList(P#"parabolic" * set{5,6,7,8}))/(i -> i -4))));
            for k from 0 to #D-1 do (
                d = dynkinType {D#k};
                l = l | entries repDual(d,weight(rootSystem d, t^(apply(piece#k,i -> i-1+4))));
                );
            )
        -- case in which 4 is marked but 2 is not
        else (
            if not member(3,P#"parabolic") then (
                if member(1,P#"parabolic") then l = entries t^{0,1}
                else l = entries t^{1};
                )
            else (
                if member(1,P#"parabolic") then l = entries t^{2,1,0}
                else  l = entries t^{1,2};
                );
            -- I complete l with what happens in the entries after 4
            R' = rootSystem(rootSystemA(rank R -4), parabolic(rootSystemA(rank R -4),set ((toList(P#"parabolic" * set{5,6,7,8}))/(i -> i -4))));
            D = apply(connectedComponents(dynkinDiagram R'),connectedDynkinType);
            piece = pieces(dynkinDiagram R', sort toList(set(toList(1..rank R)) - set ((toList(P#"parabolic" * set{5,6,7,8}))/(i -> i -4))));
            for k from 0 to #D-1 do (
                d = dynkinType {D#k};
                l = l | entries repDual(d,weight(rootSystem d, t^(apply(piece#k,i -> i-1+4))));
                );
            );
        )
    -- case in which 4 is not marked
    else (
        -- case in which 4 is not marked but 2 is
        if not member(2,P#"parabolic") then (
            if not member(5,P#"parabolic") then (
                if not member(3,P#"parabolic") then (
                    if member(1,P#"parabolic") then l = entries t^{0,3}
                    else l = entries t^{3};
                    )
                else (
                    if member(1,P#"parabolic") then l = entries t^{3,2,0}
                    else  l = entries t^{3,2};
                    );
                -- I complete l with what happens in the entries after 5
                R' = rootSystem(rootSystemA(rank R -5), parabolic(rootSystemA(rank R -5),set ((toList(P#"parabolic" * set{6,7,8}))/(i -> i -5))));
                D = apply(connectedComponents(dynkinDiagram R'),connectedDynkinType);
                piece = pieces(dynkinDiagram R', sort toList(set(toList(1..rank R)) - set ((toList(P#"parabolic" * set{6,7,8}))/(i -> i -5))));
                for k from 0 to #D-1 do (
                    d = dynkinType {D#k};
                    l = l | entries repDual(d,weight(rootSystem d, t^(apply(piece#k,i -> i-1+5))));
                    );
                )
            -- case in which 5 is not marked
            else (
                if not member(3,P#"parabolic") then (
                    if member(1,P#"parabolic") then l = entries t^{0};
                    -- I complete l with what happens in the entries after 3
                    R' = rootSystem(rootSystemA(rank R -3), parabolic(rootSystemA(rank R -3),set ((toList(P#"parabolic" * set{4,5,6,7,8}))/(i -> i -3))));
                    D = apply(connectedComponents(dynkinDiagram R'),connectedDynkinType);
                    piece = pieces(dynkinDiagram R', sort toList(set(toList(1..rank R)) - set ((toList(P#"parabolic" * set{4,5,6,7,8}))/(i -> i -3))));
                    for k from 0 to #D-1 do (
                        d = dynkinType {D#k};
                        l = l | entries repDual(d,weight(rootSystem d, t^(apply(piece#k,i -> i-1+3))));
                        );
                    )
                else (
                    if not member(1,P#"parabolic") then (
                        if rank R == 6 then (
                            if not member(6,P#"parabolic") then l = entries t^{4,3,2}
                            else l = entries t^{5,4,3,2};
                            )
                        else if rank R == 7 then (
                            if not member(6,P#"parabolic") then (
                                l = entries t^{4,3,2};
                                if member(7,P#"parabolic") then l = l | entries t^{6};
                                )
                            else (
                                if not member(7,P#"parabolic") then l = entries t^{5,4,3,2}
                                else l = entries t^{6,5,4,3,2};
                                );
                            )
                        else (
                            if not member(6,P#"parabolic") then (
                                l = entries t^{4,3,2};
                                -- I complete l with what happens in the entries after 6
                                R' = rootSystem(rootSystemA(rank R -3), parabolic(rootSystemA(rank R -6),set ((toList(P#"parabolic" * set{7,8}))/(i -> i -6))));
                                D = apply(connectedComponents(dynkinDiagram R'),connectedDynkinType);
                                piece = pieces(dynkinDiagram R', sort toList(set(toList(1..rank R)) - set ((toList(P#"parabolic" * set{7,8}))/(i -> i -6))));
                                for k from 0 to #D-1 do (
                                    d = dynkinType {D#k};
                                    l = l | entries repDual(d,weight(rootSystem d, t^(apply(piece#k,i -> i-1+6))));
                                    );				    )
                            else (
                                if not member(7,P#"parabolic") then (
                                    l = entries t^{5,4,3,2};
                                    if member(8,P#"parabolic") then l = l | entries t^{7};
                                    )
                                else (
                                    if not member(8,P#"parabolic") then l = entries t^{6,5,4,3,2}
                                    else l = entries t^{7,6,5,4,3,2};
                                    );
                                );
                            );
                        )
                    else (
                        -- node 1 not marked
                        if rank R == 6 then (
                            if not member(6,P#"parabolic") then l = entries t^{4,3,2,0}
                            else l = entries t^{5,4,3,2,0};
                            )
                        else if rank R == 7 then (
                            if not member(6,P#"parabolic") then (
                                l = entries t^{4,3,2,0};
                                if member(7,P#"parabolic") then l = l | entries t^{6};
                                )
                            else (
                                if not member(7,P#"parabolic") then l = entries t^{5,4,3,2,0}
                                else l = entries t^{6,5,4,3,2,0};
                                );
                            )
                        else (
                            if not member(6,P#"parabolic") then (
                                l = entries t^{4,3,2,0};
                                -- I complete l with what happens in the entries after 6
                                R' = rootSystem(rootSystemA(rank R -3), parabolic(rootSystemA(rank R -6),set ((toList(P#"parabolic" * set{7,8}))/(i -> i -6))));
                                D = apply(connectedComponents(dynkinDiagram R'),connectedDynkinType);
                                piece = pieces(dynkinDiagram R', sort toList(set(toList(1..rank R)) - set ((toList(P#"parabolic" * set{7,8}))/(i -> i -6))));
                                for k from 0 to #D-1 do (
                                    d = dynkinType {D#k};
                                    l = l | entries repDual(d,weight(rootSystem d, t^(apply(piece#k,i -> i-1+6))));
                                    );				    )
                            else (
                                if not member(7,P#"parabolic") then (
                                    l = entries t^{5,4,3,2,0};
                                    if member(8,P#"parabolic") then l = l | entries t^{7};
                                    )
                                else (
                                    if not member(8,P#"parabolic") then l = entries t^{6,5,4,3,2,0}
                                    else l = entries t^{7,6,5,4,3,2,0};
                                    );
                                );
                            );
                        );
                    );
                );
            )
        -- case in which neither 4 nor 2 are marked
        else (		
            if not member(5,P#"parabolic") then (
                if not member(3,P#"parabolic") then (
                    if not member(1,P#"parabolic") then l = entries t^{3,1}
                    else l = entries t^{0,3,1};
                    )
                else (
                    if not member(1,P#"parabolic") then l = entries t^{2,1,3}
                    else l = entries t^{1,0,3,2};
                    );
                -- I complete l with what happens in the entries after 5
                R' = rootSystem(rootSystemA(rank R -5), parabolic(rootSystemA(rank R -5),set ((toList(P#"parabolic" * set{6,7,8}))/(i -> i -5))));
                D = apply(connectedComponents(dynkinDiagram R'),connectedDynkinType);
                piece = pieces(dynkinDiagram R', sort toList(set(toList(1..rank R)) - set ((toList(P#"parabolic" * set{6,7,8}))/(i -> i -5))));
                for k from 0 to #D-1 do (
                    d = dynkinType {D#k};
                    l = l | entries repDual(d,weight(rootSystem d, t^(apply(piece#k,i -> i-1+5))));
                    );
                )
            -- case node 5 not marked
            else (
                if not member(3,P#"parabolic") then (
                    if member(1,P#"parabolic") then l = entries t^{0};
                    if rank R == 6 then (
                        if not member(6,P#"parabolic") then l = l | entries t^{4,3,1}
                        else l = l | entries t^{5,4,3,1};
                        )
                    else if rank R == 7 then (
                        if not member(6,P#"parabolic") then (
                            l = l | entries t^{4,3,1};
                            if member(7,P#"parabolic") then l = l | entries t^{6};
                            )
                        else (
                            if not member(7,P#"parabolic") then l = l | entries t^{5,4,3,1}
                            else l = l | entries t^{6,5,4,3,1};
                            );
                        )
                    else (
                        if not member(6,P#"parabolic") then (
                            l = l | entries t^{4,3,1};
                            if not member(7,P#"parabolic") then (
                                if member(8,P#"parabolic") then l = l | entries t^{7};
                                )
                            else (
                                if not member(8,P#"parabolic") then l = l | entries t^{6}
                                else l = l | entries t^{7,6};
                                );
                            )
                        else (
                            if not member(7,P#"parabolic") then (
                                l = l | entries t^{5,4,3,1};
                                if  member(8,P#"parabolic") then l = l | entries t^{7};
                                )
                            else (
                                if not member(8,P#"parabolic") then l = l | entries t^{6,5,4,3,1}
                                else l = l | entries t^{7,6,5,4,3,1};
                                );
                            );		    
                        );
                    )
                --case node 3 not marked
                else (
                    if not member(1,P#"parabolic") then (
                        -- here I need to change the antennae depending on the parity of the length
                        if rank R == 6 then (
                            if not member(6,P#"parabolic") then l = entries t^{1,2,3,4}
                            else l = entries t ^{2,1,3,4,5};
                            )
                        else if rank R == 7 then (
                            if not member(6,P#"parabolic") then (
                                l = entries t ^{1,2,3,4};
                                if member(7,P#"parabolic") then l = l | entries t^{6};
                                )
                            else if not member(7,P#"parabolic") then l = entries t ^{2,1,3,4,5}
                            else l = entries t ^{1,2,3,4,5,6};
                            )
                        else (
                            if not member(6,P#"parabolic") then (
                                l =  entries t ^{1,2,3,4};
                                -- I complete l with what happens in the entries after 6
                                R' = rootSystem(rootSystemA(rank R -6), parabolic(rootSystemA(rank R -6),set ((toList(P#"parabolic" * set{7,8}))/(i -> i -6))));
                                D = apply(connectedComponents(dynkinDiagram R'),connectedDynkinType);
                                piece = pieces(dynkinDiagram R', sort toList(set(toList(1..rank R)) - set ((toList(P#"parabolic" * set{7,8}))/(i -> i -6))));
                                for k from 0 to #D-1 do (
                                    d = dynkinType {D#k};
                                    l = l | entries repDual(d,weight(rootSystem d, t^(apply(piece#k,i -> i-1+6))));
                                    );
                                )
                            else if not member(7,P#"parabolic") then (
                                l = entries t ^{2,1,3,4,5};
                                if member(8,P#"parabolic") then l = l | entries t^{7};
                                )
                            else if not member(8,P#"parabolic") then (
                                l = entries t ^{1,2,3,4,5,6};
                                )
                            else (
                                l = entries t ^{2,1,3,4,5,6,7};
                                );
                            );
                        )
                    -- here I am in the case where the first node is not marked
                    else (
                        if rank R == 6 then (
                            l = entries t^{0,4,2,3,1};
                            )
                        else if rank R == 7 then (
                            if not member(6,P#"parabolic") then (
                                l = entries t^{0,4,2,3,1};
                                if member(7,P#"parabolic") then l = l | entries t^{6};
                                )
                            else l = entries t ^{5,1,4,3,2,0};
                            )
                        else (
                            if not member(6,P#"parabolic") then (
                                l = entries t^{0,4,2,3,1};
                                -- I complete l with what happens in the entries after 6
                                R' = rootSystem(rootSystemA(rank R -6), parabolic(rootSystemA(rank R -6),set ((toList(P#"parabolic" * set{7,8}))/(i -> i -6))));
                                D = apply(connectedComponents(dynkinDiagram R'),connectedDynkinType);
                                piece = pieces(dynkinDiagram R', sort toList(set(toList(1..rank R)) - set ((toList(P#"parabolic" * set{7,8}))/(i -> i -6))));
                                for k from 0 to #D-1 do (
                                    d = dynkinType {D#k};
                                    l = l | entries repDual(d,weight(rootSystem d, t^(apply(piece#k,i -> i-1+6))));
                                    );
                                )
                            else if not member(7,P#"parabolic") then (
                                l = entries t ^{5,1,2,3,4,0};
                                if member(8,P#"parabolic") then l = l | entries t^{7};
                                )
                            else (
                                l = entries t^{0,1,2,3,4,5,6,7};
                                );
                            );
                        );
                    );
                );
            );
        );
    return l;
    )
