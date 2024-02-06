----------------------------------------------------------------
--************************************************************--
--Functions for computing parameter test modules and ideals   --
--************************************************************--
----------------------------------------------------------------


--This function computes the parameter test module of a ring, it returns it as a submodule of a canonical ideal.
--this is a slightly modified function originally written by Moty Katzman for "Parameter test ideals of Cohen Macaulay rings"
--it returns the lift of the canonical module to the ambient ring
--needsPackage "Divisor";

canonicalIdeal = method( Options => { Attempts => 10 } )

canonicalIdeal Ring := o -> R1 ->
(
    S1 := ambient R1;
    I1 := ideal R1;
    dR := dim R1;
    dS := dim S1;
    varList := first entries vars S1;
    degList := {};
    if #varList > 0 then
    (
	if #(degree(varList#0)) == 1 then
	    degList = apply(varList, q -> (degree(q))#0)
    	else degList = apply(varList, q -> (degree(q)))
    );
    M1 := (Ext^(dS - dR)(S1^1/I1, S1^{-(sum degList)}))**R1;
    embedAsIdeal(M1, Attempts => o.Attempts)
)

--this function finds the generators of the intersection of
--J^{[p]} : J and I^{[p]} : I where I is the defining ideal and J is the canonical
--ideal lifted to the ambient ring (in a maximal way).
frobeniusTraceOnCanonicalModule = method()

frobeniusTraceOnCanonicalModule ( Ideal, Ideal ) := ( defIdeal, canIdeal ) ->
(
    canonical := canIdeal + defIdeal;
    Ip := frobenius defIdeal;
    tempIdeal := intersect( frobenius( canonical ) : canonical, Ip : defIdeal );
    M1 := compress( (gens tempIdeal) % (gens Ip) );
    first entries M1
)

testModule = method(
    Options =>
    {
	FrobeniusRootStrategy => Substitution,
	AssumeDomain => false,
	CanonicalIdeal => null,
	CurrentRing => null,
	GeneratorList => null
    }
)
--A rewritten function to construct the (parameter) test (sub)module of a given ring.
--It returns two ideals and an element.
--The first ideal is an ideal isomorphic to the test module and the
--and the second is an ideal isomorphic to the canonical module, in which the parameter
--resides.  The locus where these two ideals differ (plus the non-CM locus) is the
--locus where the ring does not have rational singularities.
--The final element is the element of the ambient polynomial ring which is used to
--induce the canonical trace map.
--This function can also compute \tau(omega, f^t) (again as a submodule of omega).

installMethod(testModule,
    o -> () ->
    (
        R1 := o.CurrentRing;
        canIdeal := o.CanonicalIdeal;
	--if canonical ideal is given, use it to get the ring.
        if canIdeal =!= null then R1 = ring canIdeal;
	--if no canonical ideal is given, compute it.
        if R1 =!= null and canIdeal === null then canIdeal = canonicalIdeal R1;
        if canIdeal === null then
	    error "testModule: cannot compute the testModule with no arguments or optional arguments";
        S1 := ambient R1;
        I1 := ideal R1;
        J1 := sub( canIdeal, S1 );
        C1 := testElement( R1, AssumeDomain => o.AssumeDomain );
        u1 := o.GeneratorList;
	--if no u is given, compute it.
        if u1 === null then u1 = frobeniusTraceOnCanonicalModule( I1, J1 );
        tau := I1;
        if #u1 > 1 then
        (
            if debugLevel > 0 then
	        print "testModule: Multiple trace map for omega generators (Macaulay2 failed to find the principal generator of a principal ideal). Using them all.";
            j := 0;
            while j < #u1 do
            (
                tau = tau + ascendIdeal( 1, u1#j, C1*J1*R1, FrobeniusRootStrategy => o.FrobeniusRootStrategy );
                j = j+1
            )
        )
        else
        (
            u1 = u1#0;
            tau = ascendIdeal( 1, u1, C1*J1*R1, FrobeniusRootStrategy => o.FrobeniusRootStrategy )
        );
        ( trim sub(tau, R1), trim sub(J1, R1), u1 )
    )
)

testModule Ring := o -> R -> testModule( o, CurrentRing => R )

--testModule ( Ring, Ideal ) := o -> ( R1, canIdeal ) ->
--(
--    S1 := ambient R1;
--    I1 := ideal R1;
--    J1 := sub(canIdeal, S1);
--    C1 := testElement(R1, AssumeDomain => o.AssumeDomain);
--    u1 := frobeniusTraceOnCanonicalModule( I1, J1 );
--    tau := I1;
--    if #u1 > 1 then
--    (
--        print "testModule: Multiple trace map for omega generators (Macaulay2 failed to find the principal generator of a principal ideal).  Using them all.";
--        j := 0;
--        while j < #u1 do
--        (
--            tau = tau + ascendIdeal(1, u1#j, C1*J1*R1, FrobeniusRootStrategy=>o.FrobeniusRootStrategy);
--           j = j+1;
--        );
--    )
--    else
--    (
--        u1 = u1#0;
--        tau = ascendIdeal(1, u1, C1*J1*R1, FrobeniusRootStrategy => o.FrobeniusRootStrategy);
--    );
--    (sub(tau, R1), sub(J1, R1), u1)
--)

testModule ( Number, RingElement ) := o -> (tt, ff) ->
(
    tt = sub(tt,QQ);
    R1 := o.CurrentRing;
    if (R1 === null) then R1 = ring ff;
    S1 := ambient R1;
    canIdeal := o.CanonicalIdeal;
    if (canIdeal === null) then canIdeal = canonicalIdeal(R1);
    I1 := ideal R1;
    J1 := sub(canIdeal, S1);

    u1 := o.GeneratorList;
    if (u1 === null) then u1 = frobeniusTraceOnCanonicalModule( I1, J1 );
    internalTestModule(tt, ff, canIdeal, u1, FrobeniusRootStrategy=>o.FrobeniusRootStrategy, AssumeDomain=>o.AssumeDomain)
)

testModule ( List, List ) := o -> ( ttList, ffList ) ->
(
    R1 := o.CurrentRing;
    if (R1 === null) then R1 = ring (ffList#0);
    S1 := ambient R1;
    canIdeal := o.CanonicalIdeal;
    if (canIdeal === null) then canIdeal = canonicalIdeal(R1);
    I1 := ideal R1;
    J1 := sub(canIdeal, S1);

    u1 := o.GeneratorList;
    if (u1 === null) then u1 = frobeniusTraceOnCanonicalModule( I1, J1 );

    internalTestModule(ttList, ffList, canIdeal, u1, FrobeniusRootStrategy=>o.FrobeniusRootStrategy, AssumeDomain=>o.AssumeDomain)
)

internalTestModule = method( Options => { FrobeniusRootStrategy => Substitution, AssumeDomain => false } )

internalTestModule ( Number, RingElement, Ideal, List ) := o -> (tt, ff, canIdeal, u1) ->
(
    tt = tt/1;
    R1 := ring ff;
    pp := char R1;
    S1 := ambient R1;
    fff := sub(ff, S1);
    I1 := ideal R1;
    J1 := sub(canIdeal, S1);

    C1 := testElement(R1, AssumeDomain => o.AssumeDomain);
    ( aa, bb, cc ) := decomposeFraction( pp, tt );
    -- fraction divided writes tt = (a/(p^b(p^c-1))
    -- the point is that
    -- tau(\omega, f^t) = (tau( omega, f^{a/(p^c-1)}) * u1^{(p^b-1)/(p-1)} )^{[1/p^b]}
    --we need to managed the case when ttt = aa/(pp^cc - 1) is huge, we handle this as follows.
    -- tau(\omega, ff^ttt) = \tau(\omega, ff^{ttt - floor(ttt)} ) * ff^{floor(ttt)}
    -- we do this because we never want to actually compute ff^{floor(ttt)}
    local aaa;
    local ccc;
    local newIntegerPart;
    if cc > 0 then
    (
        ttt := aa/(pp^cc-1);
        newIntegerPart = floor(ttt);
        newFractionalPart := ttt - newIntegerPart;
        (aaa, ccc) = drop( decomposeFraction(pp, newFractionalPart), {1,1} )
    )
    else
    ( -- this is the case when t = a/p^b
        (aaa, ccc) = (0,1);
        newIntegerPart = aa;
    );
    tau := I1;
    curTau := I1;
    if #u1 > 1 then
    (
        if debugLevel > 0 then
            print "internalTestModule: Multiple trace map for omega generators (Macaulay2 failed to find the principal generator of a principal ideal).  Using them all.";
        j := 0;
        while j < #u1 do
	(
            curTau = ascendIdeal( ccc, {floor((pp^ccc - 1)/(pp-1)),  aaa}, {u1#j, fff}, (ideal(fff))*C1*J1*R1, FrobeniusRootStrategy => o.FrobeniusRootStrategy );
                --note, we only have an ideal(ff) in the test element here since by construction,
                --aaa/(pp^ccc-1) is less than 1.
                --if we need to take more roots, do so...
            curTau = sub(curTau, S1);
            curTau = frobeniusRoot(bb, {floor((pp^bb - 1)/(pp-1)), newIntegerPart}, {u1#j, fff}, curTau, FrobeniusRootStrategy => o.FrobeniusRootStrategy);
            tau = tau + curTau;
            j = j+1
        );
    )
    else (
        u1 = u1#0;
        curTau = ascendIdeal(ccc, {floor((pp^ccc - 1)/(pp-1)),  aaa}, {u1, fff}, (ideal(fff^(min(1, aaa))))*C1*J1*R1, FrobeniusRootStrategy => o.FrobeniusRootStrategy );
                --note, we only have an ideal(ff) in the test element here since by construction,
                --aaa/(pp^ccc-1) is less than 1.
                --if we need to take more roots, do so...
        curTau = sub(curTau, S1);
        tau = frobeniusRoot(bb, {floor((pp^bb - 1)/(pp-1)), newIntegerPart}, {u1, fff}, curTau, FrobeniusRootStrategy => o.FrobeniusRootStrategy);
    );

    (sub(tau, R1), sub(J1, R1), u1)
)

--Write a testModule function that computes \tau(omega, f^s g^t h^u) for example
--this works similarly to testModule(QQ, RingElement, Ideal, List), but it takes lists of elements.
--Note if you specify u1 and something a bit different that canIdeal (but that is u compatible), this can be used to still compute
--a test module of a certain Cartier module.
internalTestModule ( List, List, Ideal, List ) := o -> ( ttList, ffList, canIdeal, u1 ) ->
(
    ff := ffList#0;
    R1 := ring ff;
    pp := char R1;
    S1 := ambient R1;
    fff := sub(ff, S1);
    I1 := ideal R1;
    J1 := sub(canIdeal, S1);

    ffList = apply(ffList, zz->sub(zz, S1));
    C1 := testElement(R1, AssumeDomain => o.AssumeDomain);
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
    --we need to managed the case when ttt = aa/(pp^cc - 1) is huge, we handle this as follows.
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
    if #u1 > 1 then
    (
        print "internalTestModule: Multiple trace map for omega generators (Macaulay2 failed to find the principal generator of a principal ideal).  Using them all.";
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
    else
    (
        u1 = u1#0;
        curTau = ascendIdeal(lcmCs, append(aaListForCsReduced, floor((pp^lcmCs - 1)/(pp-1))), append(ffList, u1), (product(prodList))*C1*J1*R1, FrobeniusRootStrategy=>o.FrobeniusRootStrategy);
                --note, we only have an ideal(ff) in the test element here since by construction,
                --aaa/(pp^ccc-1) is less than 1.
                --if we need to take more roots, do so...
        curTau = sub(curTau, S1);
        tau = frobeniusRoot(maxBs, append(aaListForAfterAscension, floor((pp^maxBs - 1)/(pp-1))), append(ffList, u1), curTau, FrobeniusRootStrategy => o.FrobeniusRootStrategy);
    );

    (sub(tau, R1), sub(J1, R1), u1)
)



--we also can compute the parameter test ideal (in a Cohen-Macaulay ring)

parameterTestIdeal = method(Options => { FrobeniusRootStrategy => Substitution } )

parameterTestIdeal(Ring) := o-> (R1) -> (
    testMod := testModule(CurrentRing => R1,o);
    (testMod#0) : (testMod#1)
)


--Below is an isCohenMacaulay function.  There are other implementations of this in the packages
--Depth and LexIdeals.  This one has the advantage that it works even if the ring is not local/graded.
--If you pass it the Local=>true option, then it calls the isCM function in Depth.


--warning, this only works if R is equidimensional.  If Spec R has disjoint components of different dimensions
--then this function will return false, even if R is Cohen-Macaulay.
isCohenMacaulay = method( Options => { AtOrigin => false } )

isCohenMacaulay Ring := o -> R1 ->
(
    if o.AtOrigin then isCM R1
    else
    (
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
)

isFRational = method(
    Options =>
    {
	AssumeDomain => false,
	AtOrigin => false,
	AssumeCM => false,
	FrobeniusRootStrategy => Substitution
    }
)

isFRational Ring := o -> R1 ->
(
    flag := true;
    --first verify if it is CM
    if not o.AssumeCM then
        if not isCohenMacaulay(R1, AtOrigin => o.AtOrigin) then flag = false;
    --next verify if it is Frational
    if flag then
    (
        --note we don't compute the test module if we know that the ring is not CM.
        MList := testModule(CurrentRing => R1, passOptions( o, { AssumeDomain, FrobeniusRootStrategy } ) );
        if o.AtOrigin then
	(
            paraTestIdeal := (MList#0):(MList#1);
            myMaxIdeal := sub(maxIdeal(ambient R1), R1);
            flag = not isSubset(paraTestIdeal, myMaxIdeal);
        )
        else (
            if not isSubset(MList#1, MList#0) then flag = false
        );
    );

    flag
)
