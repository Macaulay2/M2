

dominantCharacterHighestWeight = method();
dominantCharacterHighestWeight (RootSystem,Weight) := (R,l) -> (
    posRoots := elements positiveRoots(R);
    rho := entries halfSumOfRoots(R);
    M := R#((keys R)#0);
    rootNorms := R#((keys R)#1);
    sRoots := toList (1..rank(R))/(i -> simpleRoot(R,i));
    l' := entries l;
    myPoincare := new MutableHashTable;
    if not R#?"poincare1" then (
        x := local x;
        ZZ[x];
        for t in subsets toList(1..rank(R)) do (
            myPoincare#(toSequence t) = sub(poincareSeries(R,parabolic(R,set t),x), x=>1);
            );
        R#"poincare1" = new HashTable from myPoincare;
        );
    myPoincare = R#"poincare1";
    -- start the translation into python;
    fractions := import "fractions";
    Fraction := (fractions)@@("Fraction");
    posRoots = toPython (posRoots/(r -> entries r));
    rho = toPython rho;
    lam := toPython l';
    simpleRoots := toPython (sRoots/(r -> entries r));
    M = entries M/(r -> r/(p -> (Fraction numerator p) / (Fraction denominator p)));
    M = toPython M;
    rootNorms = toPython rootNorms;
    poinc := toPython myPoincare;
    dominant := import "dominantCharacterHighestWeight";
    result := dominant@@("dominantCharacterHighestWeight")(posRoots,lam, rho, simpleRoots, rootNorms, M, poinc);
    character := value pythonValue stripNumpyRepr toString result;
    character = new Tally from character;
    character = character/(v -> weight(R,toList v));
    return character;
    )

--------------- --------- tensor product -------------------------------------

-- tensor product of two irreducible representations
tensorProduct = method();
tensorProduct (Weight,Weight,RootSystem,HashTable) := (l1,l2,R,W) -> (
    l1' := entries l1;
    l2' := entries l2;
    rho := entries halfSumOfRoots(R);
    tau := reducedDecomposition longestWeylGroupElement(R);
    sRoots := toList (1..rank(R))/(i -> simpleRoot(R,i));
    -- start the translation into python
    rho = toPython rho;
    lam1 := toPython l1';
    lam2 := toPython l2';
    tau = toPython tau;
    simpleRoots := toPython (sRoots/(r -> entries r));
    W =  toPython (W/(w -> toSequence entries w));
    tensorP := import "tensorProduct";
    result := tensorP@@("tensorProduct")(lam1, lam2, simpleRoots, rho, W, tau);
    output := value pythonValue stripNumpyRepr toString result;
    output = new Tally from output;
    output = output/(v -> weight(R,toList v));
    return new HashTable from output;
    )


-- tensor product of two completely reducible representations
tensorProduct (HashTable,HashTable,RootSystem) := (L1,L2,R) -> (
    T := new MutableHashTable;
    Ttemp := new MutableHashTable;
    for i from 0 to #(keys L1)-1 do (
        l1 := (keys L1)#i;
        for j from 0 to #(keys L2)-1 do (
            l2 := (keys L2)#j;
            if weylFormula(l1,R) <= weylFormula(l2,R) then (
                W := dominantCharacterHighestWeight(R,l1); -- space for improvements?
                Ttemp = tensorProduct(l1,l2,R,W);
                )
            else (
                W = dominantCharacterHighestWeight(R,l2);
                Ttemp = tensorProduct(l2,l1,R,W);
                );
            for l in keys Ttemp do (
                if T#?l then T#l = T#l + Ttemp#l * (values L1)#i * (values L2)#j
                else T#l = Ttemp#l * (values L1)#i * (values L2)#j;
                );
            );
        );
    return T;
    )



----------------------- symmetric power ---------------------------------------------------------------------------

-- nth symmetric power of an irreducible representation
symmetricPower (ZZ, Weight,RootSystem) := (n,l,R) -> (
    if n < 0 then error "the power must be a non-negative integer"
    else if n == 0 then return new HashTable from {weight(R,toList(rank(R):0)) => 1}
    else if n == 1 then return new HashTable from {l => 1}
    else (
        posRoots := elements positiveRoots(R);
        rho := entries halfSumOfRoots(R);
        tau := reducedDecomposition longestWeylGroupElement(R);
        M := R#((keys R)#0);
        rootNorms := R#((keys R)#1);
        sRoots := toList (1..rank(R))/(i -> simpleRoot(R,i));
        l' := entries l;
        myPoincare := new MutableHashTable;
        if not R#?"poincare1" then (
            x := local x;
            ZZ[x];
            for t in subsets toList(1..rank(R)) do (
                myPoincare#(toSequence t) = sub(poincareSeries(R,parabolic(R,set t),x), x=>1);
                );
            R#"poincare1" = new HashTable from myPoincare;
            );
        myPoincare = R#"poincare1";
        -- start the translation into python;
        fractions := import "fractions";
        Fraction := (fractions)@@("Fraction");
        posRoots = toPython (posRoots/(r -> entries r));
        rho = toPython rho;
        lam := toPython l';
        simpleRoots := toPython (sRoots/(r -> entries r));
        M = entries M/(r -> r/(p -> (Fraction numerator p) / (Fraction denominator p)));
        M = toPython M;
        n = toPython n;
        tau = toPython tau;
        rootNorms = toPython rootNorms;
        poinc := toPython myPoincare;
        sym := import "symmetricPower";
        result := sym@@("symmetricPower")(n, lam, posRoots, rho, simpleRoots, rootNorms, M, poinc, tau);
        decomp := value pythonValue stripNumpyRepr toString result;
        decomp = new Tally from decomp;
        decomp = decomp/(v -> weight(R,toList v));
        return new HashTable from decomp;
        );
    )

-- returns the list of symmetric powers of an irreducible representation from 0 to k, computed inductively for efficiency
-- improvements: I could make it so I can optionally pass memoryAdams, so that if I have already computed it for the weights of the bundle then I don't recompute it
multipleSymmetricPowerLie = method();
multipleSymmetricPowerLie(ZZ, Weight, RootSystem) := (k, l, R) -> (
    if k < 0 then error "the power must be a non-negative integer"
    else if k == 0 then return new HashTable from {weight(R,toList(rank(R):0)) => 1}
    else if k == 1 then return new HashTable from {l => 1}
    else (
        posRoots := elements positiveRoots(R);
        rho := entries halfSumOfRoots(R);
        tau := reducedDecomposition longestWeylGroupElement(R);
        M := R#((keys R)#0);
        rootNorms := R#((keys R)#1);
        sRoots := toList (1..rank(R))/(i -> simpleRoot(R,i));
        l' := entries l;
        myPoincare := new MutableHashTable;
        if not R#?"poincare1" then (
            x := local x;
            ZZ[x];
            for t in subsets toList(1..rank(R)) do (
                myPoincare#(toSequence t) = sub(poincareSeries(R,parabolic(R,set t),x), x=>1);
                );
            R#"poincare1" = new HashTable from myPoincare;
            );
        myPoincare = R#"poincare1";
        -- start the translation into python;
        fractions := import "fractions";
        Fraction := (fractions)@@("Fraction");
        posRoots = toPython (posRoots/(r -> entries r));
        rho = toPython rho;
        lam := toPython l';
        irr := toPython true;
        simpleRoots := toPython (sRoots/(r -> entries r));
        M = entries M/(r -> r/(p -> (Fraction numerator p) / (Fraction denominator p)));
        M = toPython M;
        k = toPython k;
        tau = toPython tau;
        rootNorms = toPython rootNorms;
        poinc := toPython myPoincare;
        sym := import "multipleSymmetricPower";
        result := sym@@("multipleSymmetricPower")(k, lam, posRoots, rho, simpleRoots, rootNorms, M, poinc, tau,irr);
        decomp := value pythonValue stripNumpyRepr toString result;
        decomp = decomp/(d -> new Tally from d);
        decomp = decomp/(tal -> new HashTable from tal/(v -> weight(R,toList v)));
        return decomp;
        );
    )

-- given a hash table whose keys are the weights and whose values are the multiplicities, i.e. a completely reducible representation,
-- returns a hash table whose keys are the same weights and whose values are the symmetric powers of these weights
-- the multiplicities are therefore useless in the current version
-- the idea is to do kunneth directly on the vector bundles
multipleSymmetricPowerLie (ZZ, HashTable, RootSystem) := (k, L, R) -> (
    if k < 0 then error "the power must be a non-negative integer"
    else if k == 0 then return new HashTable from {weight(R,toList(rank(R):0)) => 1}
    else if k == 1 then return L
    else (
        posRoots := elements positiveRoots(R);
        rho := entries halfSumOfRoots(R);
        tau := reducedDecomposition longestWeylGroupElement(R);
        M := R#((keys R)#0);
        rootNorms := R#((keys R)#1);
        sRoots := toList (1..rank(R))/(i -> simpleRoot(R,i));
        L' := new Tally from L;
        L' = L'/(l -> toSequence entries l);
        L' = new HashTable from L';
        L' = for l in keys L' list (
            (l,L'#l)
            );
        myPoincare := new MutableHashTable;
        if not R#?"poincare1" then (
            x := local x;
            ZZ[x];
            for t in subsets toList(1..rank(R)) do (
                myPoincare#(toSequence t) = sub(poincareSeries(R,parabolic(R,set t),x), x=>1);
                );
            R#"poincare1" = new HashTable from myPoincare;
            );
        myPoincare = R#"poincare1";
        -- start the translation into python;
        fractions := import "fractions";
        Fraction := (fractions)@@("Fraction");
        posRoots = toPython (posRoots/(r -> entries r));
        rho = toPython rho;
        L' = toPython L';
        irr := toPython false;
        simpleRoots := toPython (sRoots/(r -> entries r));
        M = entries M/(r -> r/(p -> (Fraction numerator p) / (Fraction denominator p)));
        M = toPython M;
        k = toPython k;
        tau = toPython tau;
        rootNorms = toPython rootNorms;
        poinc := toPython myPoincare;
        sym := import "multipleSymmetricPower";
        result := sym@@("multipleSymmetricPower")(k, L', posRoots, rho, simpleRoots, rootNorms, M, poinc, tau,irr);
        decomp := value pythonValue stripNumpyRepr toString result;
        output := new MutableHashTable;
        for w in keys decomp do (
            dec := decomp#w/(d -> new Tally from d);
            output#(weight(R,toList w)) = dec/(tal -> new HashTable from tal/(v -> weight(R,toList v)));
            );
        return new HashTable from output;
        );
    )


---------------------------- exterior power ----------------------------------------------------------------------

-- nth exterior power of an irreducible representation
exteriorPower (ZZ, Weight, RootSystem) := o -> (n,l,R) -> (
    d := weylFormula(l,R);
    if n < 0 then error "the power must be a non-negative integer"
    else if n == 0 or n == d then return new HashTable from {weight(R,toList(rank(R):0)) => 1}
    else if n == 1 then return new HashTable from {l => 1}
    else if n > d then return new HashTable from {}
    else (
        posRoots := elements positiveRoots(R);
        rho := entries halfSumOfRoots(R);
        tau := reducedDecomposition longestWeylGroupElement(R);
        M := R#((keys R)#0);
        rootNorms := R#((keys R)#1);
        sRoots := toList (1..rank(R))/(i -> simpleRoot(R,i));
        l' := entries l;
        myPoincare := new MutableHashTable;
        if not R#?"poincare1" then (
            x := local x;
            ZZ[x];
            for t in subsets toList(1..rank(R)) do (
                myPoincare#(toSequence t) = sub(poincareSeries(R,parabolic(R,set t),x), x=>1);
                );
            R#"poincare1" = new HashTable from myPoincare;
            );
        myPoincare = R#"poincare1";
        -- start the translation into python;
        fractions := import "fractions";
        Fraction := (fractions)@@("Fraction");
        posRoots = toPython (posRoots/(r -> entries r));
        rho = toPython rho;
        lam := toPython l';
        simpleRoots := toPython (sRoots/(r -> entries r));
        M = entries M/(r -> r/(p -> (Fraction numerator p) / (Fraction denominator p)));
        M = toPython M;
        n = toPython n;
        tau = toPython tau;
        rootNorms = toPython rootNorms;
        poinc := toPython myPoincare;
        alt := import "exteriorPower";
        result := alt@@("exteriorPower")(n, lam, posRoots, rho, simpleRoots, rootNorms, M, poinc, tau);
        decomp := value pythonValue stripNumpyRepr toString result;
        decomp = new Tally from decomp;
        decomp = decomp/(v -> weight(R,toList v));
        return new HashTable from decomp;
        );
    )


-- returns the list of exterior powers of an irreducible representation from 0 to k, computed inductively
-- improvement: I could make it so I can optionally pass memoryAdams, so that if I have already computed it for the weights of the bundle then I don't recompute it
multipleExteriorPowerLie = method();
multipleExteriorPowerLie(ZZ, Weight, RootSystem) := (k, l, R) -> (
    if k < 0 then error "the power must be a non-negative integer"
    else if k == 0 then return new HashTable from {weight(R,toList(rank(R):0)) => 1}
    else if k == 1 then return new HashTable from {l => 1}
    else (
        posRoots := elements positiveRoots(R);
        rho := entries halfSumOfRoots(R);
        tau := reducedDecomposition longestWeylGroupElement(R);
        M := R#((keys R)#0);
        rootNorms := R#((keys R)#1);
        sRoots := toList (1..rank(R))/(i -> simpleRoot(R,i));
        l' := entries l;
        myPoincare := new MutableHashTable;
        if not R#?"poincare1" then (
            x := local x;
            ZZ[x];
            for t in subsets toList(1..rank(R)) do (
                myPoincare#(toSequence t) = sub(poincareSeries(R,parabolic(R,set t),x), x=>1);
                );
            R#"poincare1" = new HashTable from myPoincare;
            );
        myPoincare = R#"poincare1";
        -- start the translation into python;
        fractions := import "fractions";
        Fraction := (fractions)@@("Fraction");
        posRoots = toPython (posRoots/(r -> entries r));
        rho = toPython rho;
        lam := toPython l';
        irr := toPython true;
        simpleRoots := toPython (sRoots/(r -> entries r));
        M = entries M/(r -> r/(p -> (Fraction numerator p) / (Fraction denominator p)));
        M = toPython M;
        k = toPython k;
        tau = toPython tau;
        rootNorms = toPython rootNorms;
        poinc := toPython myPoincare;
        ext := import "multipleExteriorPower";
        result := ext@@("multipleExteriorPower")(k, lam, posRoots, rho, simpleRoots, rootNorms, M, poinc, tau,irr);
        decomp := value pythonValue stripNumpyRepr toString result;
        decomp = decomp/(d -> new Tally from d);
        decomp = decomp/(tal -> new HashTable from tal/(v -> weight(R,toList v)));
        return decomp;
        );
    )

-- given a hash table whose keys are the weights and whose values are the multiplicities, i.e. a completely reducible representation
-- returns a hash table whose keys are the same weights and whose values are the symmetric powers of these weights
-- the multiplicities are therefore useless in the current version
-- the idea is to do kunneth directly on the vector bundles
multipleExteriorPowerLie (ZZ, HashTable, RootSystem) := (k, L, R) -> (
    if k < 0 then error "the power must be a non-negative integer"
    else if k == 0 then return new HashTable from {weight(R,toList(rank(R):0)) => 1}
    else if k == 1 then return L
    else (
        posRoots := elements positiveRoots(R);
        rho := entries halfSumOfRoots(R);
        tau := reducedDecomposition longestWeylGroupElement(R);
        M := R#((keys R)#0);
        rootNorms := R#((keys R)#1);
        sRoots := toList (1..rank(R))/(i -> simpleRoot(R,i));
        L' := new Tally from L;
        L' = L'/(l -> toSequence entries l);
        L' = new HashTable from L';
        L' = for l in keys L' list (
            (l,L'#l)
            );
        myPoincare := new MutableHashTable;
        if not R#?"poincare1" then (
            x := local x;
            ZZ[x];
            for t in subsets toList(1..rank(R)) do (
                myPoincare#(toSequence t) = sub(poincareSeries(R,parabolic(R,set t),x), x=>1);
                );
            R#"poincare1" = new HashTable from myPoincare;
            );
        myPoincare = R#"poincare1";
        -- start the translation into python;
        fractions := import "fractions";
        Fraction := (fractions)@@("Fraction");
        posRoots = toPython (posRoots/(r -> entries r));
        rho = toPython rho;
        L' = toPython L';
        irr := toPython false;
        simpleRoots := toPython (sRoots/(r -> entries r));
        M = entries M/(r -> r/(p -> (Fraction numerator p) / (Fraction denominator p)));
        M = toPython M;
        k = toPython k;
        tau = toPython tau;
        rootNorms = toPython rootNorms;
        poinc := toPython myPoincare;
        ext := import "multipleExteriorPower";
        result := ext@@("multipleExteriorPower")(k, L', posRoots, rho, simpleRoots, rootNorms, M, poinc, tau,irr);
        decomp := value pythonValue stripNumpyRepr toString result;
        output := new MutableHashTable;
        for w in keys decomp do (
            dec := decomp#w/(d -> new Tally from d);
            output#(weight(R,toList w)) = dec/(tal -> new HashTable from tal/(v -> weight(R,toList v)));
            );
        return new HashTable from output;
        );
    )

