----------------------------------------------------------------
--************************************************************--
--Functions for computing parameter test modules and ideals   --
--************************************************************--
----------------------------------------------------------------


--This function computes the parameter test module of a ring, it returns it as a submodule of a canonical ideal.
--this is a slightly modified function originally written by Moty Katzman for "Parameter test ideals of Cohen Macaulay rings"
--it returns the lift of the canonical module to the ambient ring
--needsPackage "Divisor";

canonicalIdeal = method(Options=>{Attempts=>10});

canonicalIdeal(Ring) := o->(R1) -> (
    S1 := ambient R1;
	I1 := ideal R1;
	dR := dim R1;
	dS := dim S1;
	varList := first entries vars S1;
	degList := {};
	if (#varList > 0) then (
    	if (#(degree(varList#0)) == 1) then (
	    	degList = apply(varList, q -> (degree(q))#0); )
    	else (
	    	degList = apply(varList, q -> (degree(q))); );
    );
	M1 := (Ext^(dS - dR)(S1^1/I1, S1^{-(sum degList)}))**R1;
	embedAsIdeal(M1, Attempts=>o.Attempts)
);


--the following function computes the u of a canonical ideal in a polynomial ring
--it uses previous work of Katzman
finduOfIdeal = method();

finduOfIdeal(Ideal, Ideal) := (defIdeal, canIdeal) -> (
	Ip := frobenius( defIdeal );
	tempIdeal := intersect( (frobenius( canIdeal )) : canIdeal, Ip : defIdeal );

	M1 := compress ((gens tempIdeal)%(gens Ip));
	first first entries M1
);

--****************************************************
--*****Karl rewrote this *****
--****************************************************

--this function finds the generators of the intersection of
--J^{[p]} : J and I^{[p]} : I where I is the defining ideal and J is the canonical
--ideal lifted to the ambient ring (in a maximal way).
frobeniusTraceOnCanonicalModule = (defIdeal, canIdeal) -> (
	Ip := frobenius( defIdeal );
	tempIdeal := intersect( (frobenius( canIdeal )) : canIdeal, Ip : defIdeal );

	M1 := compress ((gens tempIdeal)%(gens Ip));
	first entries M1
)

testModule = method(Options => {FrobeniusRootStrategy => Substitution, AssumeDomain=>false}); --a rewritten function to construct the (parameter) test (sub)module of a given ring.
                       --it returns two ideals and an element.
                       --The first ideal is an ideal isomorphic to the test module and the
                       --and the second is an ideal isomorphic to the canonical module, in which the parameter
                       --resides.  The locus where these two ideals differ (plus the non-CM locus) is the
                       --locus where the ring does not have rational singularities.
                       --the final element is the element of the ambient polynomial ring which is used to
                       --induce the canonical trace map
                       --This function can also compute \tau(omega, f^t) (again as a submodule of omega).
                       --

testModule(Ring) := o -> (R1) -> (
    J1 := canonicalIdeal(R1);
    testModule(R1, J1, FrobeniusRootStrategy=>o.FrobeniusRootStrategy)
);

testModule(Ring, Ideal) := o->(R1, canIdeal) -> (
    S1 := ambient R1;
	I1 := ideal R1;
    J1 := sub(canIdeal, S1);
    C1 := testElement(R1, AssumeDomain=>o.AssumeDomain);

    u1 := frobeniusTraceOnCanonicalModule(I1, J1+I1);
    tau := I1;
    if (#u1 > 1) then(
        print "testModule: Multiple trace map for omega generators (Macaulay2 failed to find the principal generator of a principal ideal).  Using them all.";
        j := 0;
        while (j < #u1) do (
            tau = tau + ascendIdeal(1, u1#j, C1*J1*R1, FrobeniusRootStrategy=>o.FrobeniusRootStrategy);
            j = j+1;
        );
    )
    else (
        u1 = u1#0;
        tau = ascendIdeal(1, u1, C1*J1*R1, FrobeniusRootStrategy => o.FrobeniusRootStrategy);
    );

    (sub(tau, R1), sub(J1, R1), u1)
);



testModule(Number, RingElement) := o-> (tt, ff) ->(
    tt = tt/1;
    R1 := ring ff;
    S1 := ambient R1;
    canIdeal := canonicalIdeal(R1);
    I1 := ideal R1;
    J1 := sub(canIdeal, S1);
    u1 := frobeniusTraceOnCanonicalModule(I1, J1+I1);
    testModule(tt, ff, canIdeal, u1, FrobeniusRootStrategy => o.FrobeniusRootStrategy)
);

testModule(Number, RingElement, Ideal, List) := o -> (tt, ff, canIdeal, u1) -> (
    tt = tt/1;
    R1 := ring ff;
    pp := char R1;
    S1 := ambient R1;
    fff := sub(ff, S1);
    I1 := ideal R1;
    J1 := sub(canIdeal, S1);


    C1 := testElement(R1, AssumeDomain=>o.AssumeDomain);
    fractionDivided := decomposeFraction(pp, tt);

    -- fraction divided writes tt = (a/(p^b(p^c-1))
    -- the point is that
    -- tau(\omega, f^t) = (tau( omega, f^{a/(p^c-1)}) * u1^{(p^b-1)/(p-1)} )^{[1/p^b]}
    aa := fractionDivided#0;
    bb := fractionDivided#1;
    cc := fractionDivided#2;
    --we need to managed the case when ttt = aa/(pp^cc - 1) is huge, we handle this as folows.
    -- tau(\omega, ff^ttt) = \tau(\omega, ff^{ttt - floor(ttt)} ) * ff^{floor(ttt)}
    -- we do this because we never want to actually compute ff^{floor(ttt)}
    ttt := 0;
    aaa := 0;
    ccc := 0;
    newIntegerPart := 0;
    newFractionalPart := 0;
    fractionDivided2 := {};
    if (cc > 0) then (
        ttt = aa/(pp^cc-1);
        newIntegerPart = floor(ttt);
        newFractionalPart = ttt - newIntegerPart;
        fractionDivided2 = decomposeFraction(pp, newFractionalPart);
        aaa = fractionDivided2#0;
        ccc = fractionDivided2#2;
    )
    else ( -- this is the case when t = a/p^b
        aaa = 0;
        ccc = 1;
        newIntegerPart = aa;
    );
    tau := I1;
    curTau := I1;
    if (#u1 > 1) then(
        if (debugLevel > 0) then (
            print "testModule: Multiple trace map for omega generators (Macaulay2 failed to find the principal generator of a principal ideal).  Using them all.";
        );
        j := 0;
        while (j < #u1) do (
            curTau = ascendIdeal(ccc, {floor((pp^ccc - 1)/(pp-1)),  aaa}, {u1#j, fff}, (ideal(fff))*C1*J1*R1, FrobeniusRootStrategy=>o.FrobeniusRootStrategy);
                --note, we only have an ideal(ff) in the test element here since by construction,
                --aaa/(pp^ccc-1) is less than 1.
                --if we need to take more roots, do so...
            curTau = sub(curTau, S1);
            curTau = frobeniusRoot(bb, {floor((pp^bb - 1)/(pp-1)), newIntegerPart}, {u1#j, fff}, curTau, FrobeniusRootStrategy => o.FrobeniusRootStrategy);
            tau = tau + curTau;
            j = j+1;
        );
    )
    else (
        u1 = u1#0;
        curTau = ascendIdeal(ccc, {floor((pp^ccc - 1)/(pp-1)),  aaa}, {u1, fff}, (ideal(fff^(min(1, aaa))))*C1*J1*R1, FrobeniusRootStrategy=>o.FrobeniusRootStrategy);
                --note, we only have an ideal(ff) in the test element here since by construction,
                --aaa/(pp^ccc-1) is less than 1.
                --if we need to take more roots, do so...
        curTau = sub(curTau, S1);
        tau = frobeniusRoot(bb, {floor((pp^bb - 1)/(pp-1)), newIntegerPart}, {u1, fff}, curTau, FrobeniusRootStrategy => o.FrobeniusRootStrategy);
    );

    (sub(tau, R1), sub(J1, R1), u1)
);


--Write a testModule function that computes \tau(omega, f^s g^t h^u) for example
--this works similarly to testModule(QQ, RingElement, Ideal, List), but it takes lists of elements.
--Note if you specify u1 and something a bit different that canIdeal (but that is u compatible), this can be used to still compute
--a test module of a certain Cartier module.
testModule(List, List, Ideal, List) := o -> (ttList, ffList, canIdeal, u1) -> (
    ff := ffList#0;
    R1 := ring ff;
    pp := char R1;
    S1 := ambient R1;
    fff := sub(ff, S1);
    I1 := ideal R1;
    J1 := sub(canIdeal, S1);

    ffList = apply(ffList, zz->sub(zz, S1));
    C1 := testElement(R1, AssumeDomain=>o.AssumeDomain);
    fractionDividedList := apply(ttList, tt -> decomposeFraction(pp, tt));

    -- fraction divided writes tt = (a/(p^b(p^c-1))
    -- the point is that
    -- tau(\omega, f^t) = (tau( omega, f^{a/(p^c-1)}) * u1^{(p^b-1)/(p-1)} )^{[1/p^b]}
    bbList := apply(fractionDividedList, zz->zz#1);
    ccList := apply(fractionDividedList, zz->zz#2);
    --we need to write all of our fractions with the same denominator.
    lcmCs := lcm(apply(ccList, zz -> (if zz == 0 then 1 else zz))); --take the lcm of the cs,
                                                                   --but ignore those cs that
                                                                   --are equal to zero
    maxBs := max(bbList);
    --next we create the list of aa values for ascending the ideal
    aaListForCs := apply(fractionDividedList, zz -> (
                                                    if ((zz#2) == 0) then 0 else
                                                        (zz#0)*floor(pp^(maxBs - (zz#1)))*floor( (pp^lcmCs - 1)/(pp^(zz#2) - 1))
                                                 )
                        );
    --we need to managed the case when ttt = aa/(pp^cc - 1) is huge, we handle this as folows.
    -- tau(\omega, ff^ttt) = \tau(\omega, ff^{ttt - floor(ttt)} ) * ff^{floor(ttt)}
    -- we do this because we never want to actually compute ff^{floor(ttt)}
    aaListForCsReduced := apply(aaListForCs, aa -> (aa % (pp^lcmCs - 1)) );
    aaListForAfterAscension := apply(#fractionDividedList, nn -> (
                                                    if ( ((fractionDividedList#nn)#2) > 0 ) then floor((aaListForCs#nn)/(pp^lcmCs - 1)) else
                                                        ((fractionDividedList#nn)#0)*floor(pp^(maxBs - ((fractionDividedList#nn)#1)))
                                                 )
                        );

    tau := I1;
    curTau := I1;
    prodList := apply(#ffList, iii -> (ffList#iii)^(min(1, aaListForCsReduced#iii)) );
    if (#u1 > 1) then(
        print "testModule: Multiple trace map for omega generators (Macaulay2 failed to find the principal generator of a principal ideal).  Using them all.";
        j := 0;
        while (j < #u1) do (
            curTau = ascendIdeal(lcmCs, append(aaListForCsReduced, floor((pp^lcmCs - 1)/(pp-1))), append(ffList, u1), (product(prodList))*C1*J1*R1, FrobeniusRootStrategy=>o.FrobeniusRootStrategy);
                --note, we only have an ideal(ff) in the test element here since by construction,
                --aaa/(pp^ccc-1) is less than 1.
                --if we need to take more roots, do so...
            curTau = sub(curTau, S1);

            curTau = frobeniusRoot(maxBs, append(aaListForAfterAscension, floor((pp^maxBs - 1)/(pp-1))), append(ffList, u1), curTau, FrobeniusRootStrategy => o.FrobeniusRootStrategy);

            tau = tau + curTau;
            j = j+1;
        );
    )
    else (
        u1 = u1#0;
        curTau = ascendIdeal(lcmCs, append(aaListForCsReduced, floor((pp^lcmCs - 1)/(pp-1))), append(ffList, u1), (product(prodList))*C1*J1*R1, FrobeniusRootStrategy=>o.FrobeniusRootStrategy);
                --note, we only have an ideal(ff) in the test element here since by construction,
                --aaa/(pp^ccc-1) is less than 1.
                --if we need to take more roots, do so...
        curTau = sub(curTau, S1);
        tau = frobeniusRoot(maxBs, append(aaListForAfterAscension, floor((pp^maxBs - 1)/(pp-1))), append(ffList, u1), curTau, FrobeniusRootStrategy => o.FrobeniusRootStrategy);
    );

    (sub(tau, R1), sub(J1, R1), u1)
);

testModule(List, List) := o-> (ttList, ffList) ->(
    R1 := ring (ffList#0);
    S1 := ambient R1;
    canIdeal := canonicalIdeal(R1);
    I1 := ideal R1;
    J1 := sub(canIdeal, S1);
    u1 := frobeniusTraceOnCanonicalModule(I1, J1+I1);
    testModule(ttList, ffList, canIdeal, u1, FrobeniusRootStrategy => o.FrobeniusRootStrategy)
);


--we also can compute the parameter test ideal (in a Cohen-Macaulay ring)

parameterTestIdeal = method(Options => {FrobeniusRootStrategy => Substitution});

parameterTestIdeal(Ring) := o-> (R1) -> (
    testMod := testModule(R1);
    (testMod#0) : (testMod#1)
);


--Below is an isCohenMacaulay function.  There are other implementations of this in the packages
--Depth and LexIdeals.  This one has the advantage that it works even if the ring is not local/graded.
--If you pass it the Local=>true option, then it calls the isCM function in Depth.


--warning, this only works if R is equidimensional.  If Spec R has disjoint components of different dimensions
--then this function will return false, even if R is Cohen-Macaulay.
isCohenMacaulay = method(Options => {IsLocal => false});

isCohenMacaulay(Ring) := o->(R1) ->(
    if (o.IsLocal == true) then (
        isCM(R1)
    )
    else (
        S1 := ambient R1;
        I1 := ideal R1;
        M1 := S1^1/I1;
        dimS := dim S1;
        dimR := dim R1;
        flag := true;
        P1 := res M1;

        if (length P1 == dimS - dimR) then (
            true
        )
        else if (isHomogeneous(I1) == true) then (
            false
        )
        else (
            RHom := Hom(P1, S1^1);
            i := dimS - dimR + 1;
            while ((flag == true) and (i <= dimS))do (
                if (dim (HH^i(RHom)) > -1) then (
                    flag = false;
                );
                i = i+1;
            );
            flag
        )
    )
);

--next we write an isFRational function

isFRational = method(Options => {AssumeDomain => false, IsLocal => false, AssumeCM => false, FrobeniusRootStrategy=>Substitution });

isFRational(Ring) := o->(R1) ->(
    flag := true;
    --first verify if it is CM
    if (o.AssumeCM == false) then(
        if (not isCohenMacaulay(R1, IsLocal => o.IsLocal)) then (
            flag = false;
        );
    );
    --next verify if it is Frational
    if (flag == true) then (
        --note we don't compute the test module if we know that the ring is not CM.
        MList := testModule(R1, AssumeDomain=>o.AssumeDomain, FrobeniusRootStrategy=>o.FrobeniusRootStrategy);
        if (o.IsLocal == true) then (
            paraTestIdeal := (MList#0):(MList#1);
            myMaxIdeal := sub(maxIdeal(ambient R1), R1);
            flag = not isSubset(paraTestIdeal, myMaxIdeal);
        )
        else (
            if (isSubset(MList#1, MList#0) == false) then (
                flag = false;
            )
        );
    );

    flag
);
