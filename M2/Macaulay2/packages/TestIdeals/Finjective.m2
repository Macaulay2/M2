---------------------------------------------------------------------
---------------------------------------------------------------------
--**************************************
--*** HSLGModule computes the stable ***
--*** image under trace of \omega    ***
--*** This is dual to the stable     ***
--*** kernel of Frobenius acting on  ***
--*** local cohomology               ***
--**************************************

HSLGModule = method(Options => {FrobeniusRootStrategy => Substitution});
                       --it returns two ideals, an element and an integer
                       --The first ideal is an ideal isomorphic to the non-F-injective module and the
                       --and the second is an ideal isomorphic to the canonical module, in which the parameter
                       --resides.  The locus where these two ideals differ (plus the non-CM locus) is the
                       --locus where the ring does not have rational singularities.
                       --the final element is the element of the ambient polynomial ring which is used to
                       --induce the canonical trace map (or a list of elements)
                       --finally, it also outputs the associated HSL(G)-number.
                       --{HSLMod, CanMod, u, HSL#}


HSLGModule(Ring) := o-> (R1) -> (
    J1 := trim canonicalIdeal(R1);
    HSLGModule(R1, J1, FrobeniusRootStrategy=>o.FrobeniusRootStrategy)
);

HSLGModule(Ring, Ideal) := o-> (R1, canIdeal) -> (
    S1 := ambient R1;
	I1 := ideal R1;
    J1 := sub(canIdeal, S1);
    u1 := frobeniusTraceOnCanonicalModule(I1, J1+I1);
--    powList := apply(u1, zz->1);
    --NEED TO CHANGE (and below), we should not have the full list of us there.
    HSLGModule(R1, canIdeal, u1)
);

HSLGModule(Ring, Ideal, List) := o-> (R1, canIdeal, u1) -> (
    S1 := ambient R1;
	I1 := ideal R1;
    J1 := sub(canIdeal, S1);

    curIdeal := ideal(sub(0, R1));
    curHSL := 1;
    curHSLList := null;
    i := 0;
    while (i < #u1) do (
        curHSLList = HSLGModule(1, {1}, {u1#i}, canIdeal, FrobeniusRootStrategy=>o.FrobeniusRootStrategy);
        curIdeal = curIdeal + curHSLList#0;
        curHSL = lcm(curHSL, curHSLList#3);
        i = i+1;
    );
    return {curIdeal, canIdeal, u1, curHSL};
);

HSLGModule(Ideal) := o -> (canIdeal) -> (
    R1 := ring canIdeal;
    HSLGModule(R1, canIdeal, FrobeniusRootStrategy=>o.FrobeniusRootStrategy)
);

HSLGModule(Number, RingElement, Ideal, List) := o -> (tt, ff, canIdeal, u1) -> (
    R1 := ring ff;
    S1 := ambient R1;
    if (not (ring (u1#0) === S1)) then error "HSLGModule: Exptected u1 to be in the ambient polynomial ring.";
	I1 := ideal R1;
    J1 := sub(canIdeal, S1);
    pp := char S1;
    tt = tt*1/1;
    fractionDivided := decomposeFraction(pp, tt);
        -- fraction divided writes tt = (a/(p^b(p^c-1))
    aa := fractionDivided#0;
    bb := fractionDivided#1;
    cc := fractionDivided#2;
    if (bb > 0) then (
        error "HSLGModule: Cannot compute the HSLG module associated to something with p in denominator.";
    );
    if (cc == 0) then (
        cc = 1;
        aa = aa*(pp-1);
    );
    newExp := floor( (pp^cc-1)/(pp-1) );
--    uList := append(u1, ff);
--    powList = append(powList, aa);
    --now we have to do a sum again since Macaulay2 might not find one u in the nongraded case.
    curIdeal := ideal(sub(0, R1));
    curHSL := 1;
    curHSLList := null;
    i := 0;
    while (i < #u1) do (
        curHSLList = HSLGModule(cc, {newExp, aa}, {u1#i, ff}, canIdeal, FrobeniusRootStrategy=>o.FrobeniusRootStrategy);
        curIdeal = curIdeal + curHSLList#0;
        curHSL = lcm(curHSL, curHSLList#3);
        i = i+1;
    );
    {curIdeal, canIdeal, u1, curHSL}
);

HSLGModule(Number, RingElement) := o -> (tt, ff) -> (
    R1 := ring ff;
    canIdeal := trim canonicalIdeal(R1);
    S1 := ambient R1;
	I1 := ideal R1;
    J1 := sub(canIdeal, S1);
    u1 := frobeniusTraceOnCanonicalModule(I1, J1+I1);
    return HSLGModule(tt, ff, canIdeal, u1);
);

HSLGModule(List, List, Ideal, List) := o -> (tList, fList, canIdeal, u1) -> (
    if ( not (#tList == #fList) ) then error "HSLGModule: expected the lists to have the same lengths.";
    if ( #fList == 0 ) then error "HSLGModule: expected a list of length > 0.";
    R1 := ring ((fList)#0);
    S1 := ambient R1;
	I1 := ideal R1;
    J1 := sub(canIdeal, S1);
    pp := char S1;
    tList = apply(tList, tt -> tt*1/1);
    fractionDividedList2 := apply(tList, tt -> decomposeFraction(pp, tt));
        -- fraction divided writes tt = (a/(p^b(p^c-1))
    fractionDividedList := apply(fractionDividedList2, myList -> ( if (myList#2 == 0) then {(pp-1)*(myList#0), myList#1, 1} else myList ) );
--    aaList := apply(fractionDividedList, zz->zz#0);
    bbList := apply(fractionDividedList, zz->zz#1);
    ccList := apply(fractionDividedList, zz->zz#2);

    if (any(bbList, val -> (val > 0))) then (
        error "HSLGModule: Cannot compute the HSLG module associated to something with p in denominator.";
    );
    ccLCM := lcm(ccList);
    newExpList := apply(fractionDividedList, myList -> (myList#0)*floor( (pp^(ccLCM) - 1)/(pp^(myList#2)-1) ) );
--    uList := u1 | fList;
--    powList := (apply(u1, tt -> floor((pp^(ccLCM) - 1)/(pp - 1))) ) | newExpList;
--    HSLGModule(ccLCM, powList, uList, canIdeal, FrobeniusRootStrategy=>o.FrobeniusRootStrategy)
    curIdeal := ideal(sub(0, R1));
    curHSL := 1;
    curHSLList := null;
    i := 0;

    while (i < #u1) do (
        curHSLList = HSLGModule(ccLCM, {floor((pp^(ccLCM) - 1)/(pp - 1))} | newExpList, {u1#i} | apply(fList, gg -> sub(gg, S1)), canIdeal, FrobeniusRootStrategy=>o.FrobeniusRootStrategy);
        curIdeal = curIdeal + curHSLList#0;
        curHSL = lcm(curHSL, curHSLList#3);
        i = i+1;
    );
    {curIdeal, canIdeal, u1, curHSL}
)

HSLGModule(List, List) := o -> (tList, fList) -> (
    if ( not (#tList == #fList) ) then error "HSLGModule: expected the lists to have the same lengths.";
    if ( #fList == 0 ) then error "HSLGModule: expected a list of length > 0.";
    R1 := ring ((fList)#0);
    canIdeal := trim canonicalIdeal(R1);
    S1 := ambient R1;
	I1 := ideal R1;
    J1 := sub(canIdeal, S1);
    u1 := frobeniusTraceOnCanonicalModule(I1, J1+I1);
    return HSLGModule(tList, fList, canIdeal, u1);
);

--the next one is the internal function that does the real work
--you pass it the base frobeniusRoot to work with
--the list of exponents of the u's (most frequently 1)
--the list of u's
--the canonical ideal (or whatever you want to run HSL on), this one is
--it computes sigma(canIdeal, f^s g^t h^l ...)
HSLGModule(ZZ, List, List, Ideal) :=  o-> (ee, expList, u1, canIdeal) -> (
    R1 := ring canIdeal;
    S1 := ambient R1;
    I1 := ideal R1;
    J1 := sub(canIdeal, S1) + I1;
    u2 := apply(u1, gg -> sub(gg, S1));
    --now we do the HSLG computation
    idealIn := J1;
    idealOut := frobeniusRoot(ee, expList, u1, idealIn, FrobeniusRootStrategy=>o.FrobeniusRootStrategy);
    HSLCount := 0;
    while (not (idealIn + I1 == idealOut + I1)) do (
        idealIn = idealOut;
        idealOut = frobeniusRoot(ee, expList, u2, idealIn + I1, FrobeniusRootStrategy=>o.FrobeniusRootStrategy);
        HSLCount = HSLCount+1;
    );
    {sub(idealIn, R1), canIdeal, u1, HSLCount}
);

---------------------------------------------------------------------
---------------------------------------------------------------------
--****************************************************
--*** isFInjective checks if a ring is F-injective ***
--****************************************************


isFInjective = method(Options => {FrobeniusRootStrategy => Substitution, CanonicalStrategy => Katzman, AssumeCM => false, AssumeReduced => true, AssumeNormal => false, IsLocal => false});
--originally written by Drew Ellingson, with assistance from Karl Schwede

isFInjective(Ring) := o-> (R1) ->
(
    d := dim R1;
    S1 := ambient R1;
    I1 := ideal R1;
    FS := null; -- this is the pushFwd of (R-F_*R) to a map of (ambient R) modules. computationally expensive. Only computed if needed.
    F := map(R1,S1);
    d1 := dim S1;
    i := d1-d+1;
    myMap := null; -- maps between ext groups used to test F-injectivity. Computationally expensive and computed where needed.
    flag := true;
    flagPushComputed := false;

    -- F-Injectivity fast to compute on dim(S)-dim(R), so we check there seperately by default
    if (o.CanonicalStrategy === Katzman) then (
        if (isFInjectiveCanonicalStrategy(R1, IsLocal => o.IsLocal, FrobeniusRootStrategy=>o.FrobeniusRootStrategy) == false) then ( -- if F-injectivity fails in top dimension, no need to try any others
        	return false;
    	);
    )
    else (
        i = i - 1;
    );
    --if we assume Cohen-Macaulay, then we are already done
    if (o.AssumeCM == true) then return true;
    --otherwise we next construct G : R -> F_* R
    G := null;

    if (o.AssumeNormal == true) then (d1 = d1 - 2) else if (o.AssumeReduced == true) then (d1 = d1-1);

    while ((i <= d1) and (flag == true)) do (
    	--if ambient pushforward is faster in the next line, use it instead
	    if (Ext^i(S1^1/I1, S1^1) != 0) then (
    	    if (G === null) then G = frobPFMap(1,R1);
	        --note we only compute the Frobenius pushforward if the ring is not CM
            if (flagPushComputed == false) then (
                FS = pushFwdToAmbient(R1,G);   --pushforward Frobenius
                flagPushComputed == true;
            );

	        myMap = Ext^i(FS,S1^1); --functorial Ext
	        if ((dim coker myMap) != -1) then (flag = false;);
	    );
	    i = i + 1;
	);
	flag
);

--the following is an internal function, it checks if is F-injective at the top cohomology (quickly)
isFInjectiveCanonicalStrategy = method(Options => {FrobeniusRootStrategy => Substitution, IsLocal => false});

isFInjectiveCanonicalStrategy(Ring) := o->(R1) -> (
    S1 := ambient R1;
	I1 := ideal R1;
	canIdeal := trim canonicalIdeal(R1);
    J1 := sub(canIdeal, S1) + I1;
    u1 := frobeniusTraceOnCanonicalModule(I1, J1+I1);
    curIdeal := ideal(sub(0, S1));
    i := 0;
    while (i < #u1) do (
        curIdeal = curIdeal + frobeniusRoot(1, {1}, {u1#i}, J1);
        i = i+1;
    );
    if (o.IsLocal == true) then (
        myMax := maxIdeal(S1);
        paramFideal := curIdeal : J1;
        return not isSubset(paramFideal, myMax);
    )
    else (
        if (curIdeal + I1 == J1 + I1) then return true else return false;
    );
);


--the following are internal functions
--this creates the ring map A -> F^e_* A
frob = method();
frob(ZZ,Ring) := RingMap => (n,A)-> ( map(A,A,apply(gens A,i -> i^((char A)^n))));

needsPackage "PushForward";
--this one computes the Frobenius pushforward of a module F^e_* M
frobPF = method();
frobPF(ZZ,Ring):= Sequence => (n,A) -> (pushFwd(frob(n,A)));
frobPF(Module, ZZ, Ring) := Module => (M,n,A) -> (
        pushFwd(frob(n,A),M);
);

--this one construct the map R -> F^e_* R
frobPFMap = method();
frobPFMap(ZZ, Ring) := Matrix => (n,A) -> (
	frobList := frobPF(n,A);
	map(frobList#0, A^1, (frobList#2)(sub(1, A)))
);

--give a map of modules represented by a matrix A over R = S/I, we'd like to represent the module over S.
--This tries to do this faster than pushFwd.
pushFwdToAmbient = method();
pushFwdToAmbient(Ring, Matrix) := (R, A) ->(
    S := ambient(R);
    dimMax := (numRows A);
--    SMatrix := matrix(apply(flatten entries A, i -> {sub(i,S)}));
    SMatrix := sub(matrix A, S);
--    oldDefMatrix := matrix(apply( entries presentation target A, j -> apply(j,k -> sub(k, S))));
    oldDefMatrix := sub(presentation target A, S);
    modMatrix := presentation(S^(dimMax)/(ideal(R)));
    defMatrix := oldDefMatrix | modMatrix;
--    matrixOfZeros := matrix{ apply(numRows modMatrix, z->0) }

    outputMap := map(coker defMatrix,S^1/(ideal(R)), SMatrix  );
    outputMap
);
