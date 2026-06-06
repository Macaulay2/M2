--****************************************************
--****************************************************
--This file contains functions related to test ideals.
--****************************************************
--****************************************************

--*********************
--Preliminary functions
--*********************

-- This function computes the element f in the ambient ring S of R=S/I such that
-- I^{[p^e]}:I = (f) + I^{[p^e]}.
-- If there is no such unique element, the function returns an error.

QGorensteinGenerator = method()

QGorensteinGenerator ( ZZ, Ring ) := ( e, R ) ->
(
    S := ambient R; -- the ambient ring
    I := ideal R; -- the defining ideal
    gensList := I_*;
    p := char R;
    --principal ideals shouldn't have colons computed
    if #gensList == 1 then return (gensList#0)^(p^e - 1);
    Ie := frobenius( e, I );
    J := trim ( Ie : I ); --compute the colon
    J = trim sub( J, S/Ie ); -- extend colon ideal to S/Ie
    L := J_*; -- grab generators
    if #L != 1 then
        error "QGorensteinGenerator: this ring does not appear to be (Q-)Gorenstein, or you might need to work on a smaller chart. Or the index may not divide p^e-1 for the e you have selected.  Alternately it is possible that Macaulay2 failed to trim a principal ideal.";
    lift( L#0, S )
)

QGorensteinGenerator Ring := R -> QGorensteinGenerator( 1, R )

-- Finds a test element of a ring R = k[x, y, ...]/I (or at least an ideal
-- containing a nonzero test element).  It views it as an element of the
-- ambient ring of R. It returns an ideal with some of these elements in it.
-- One could make this faster by not computing the entire Jacobian/singular
-- locus. Instead, if we just find one element of the Jacobian not in I, then
-- that would also work and perhaps be substantially faster.
-- It assumes that R is a reduced ring.
testElement = method( Options => { AssumeDomain => false } )

testElement Ring := o -> R1 ->
(
    -- Marcus I believe wrote this code to look at random minors instead of all
    -- minors. Note in the current version this will not terminate if the ring
    -- is not generically reduced.
    I1 := ideal R1;
    n1 := #(gens R1) - dim R1;
    M1 := jacobian I1;
    r1 := rank target M1;
    c1 := rank source M1;
    testEle := sub( 0, ambient R1 );
    primesList := {};
    primesList = if o.AssumeDomain then { I1 } else minimalPrimes I1;
    curMinor := ideal sub( 0, ambient R1 );
    while any( primesList, II -> isSubset( ideal testEle, II ) ) do
    (
	curMinor = ( minors( n1, M1, First => {randomSubset(r1,n1),randomSubset(c1,n1)}, Limit => 1 ) )_*;
	if #curMinor > 0 then
	    testEle = if o.AssumeDomain then first curMinor else
	        testEle + random( coefficientRing R1 )*( first curMinor );
    );
    testEle % I1
)

--****************************
--****************************
--**New test ideal functions**
--****************************
--****************************

--the following is the new function for computing test ideals written by Karl.

testIdeal = method(
    Options =>
    {
	MaxCartierIndex => 10,
	FrobeniusRootStrategy => Substitution,
	QGorensteinIndex => 0,
	AssumeDomain => false
     }
)

testIdeal Ring := o -> R1 ->
(
    canIdeal := canonicalIdeal R1;
    pp := char R1;
    cartIndex := 0;
    fflag := false;
    computedFlag := false;
    curIdeal := ideal 0_R1;
    locPrincList := null;
    computedTau := ideal 0_R1;
    if o.QGorensteinIndex > 0 then
    (
        cartIndex = o.QGorensteinIndex;
        fflag = true
    )
    else
        while not fflag and cartIndex < o.MaxCartierIndex do
	(
            cartIndex = cartIndex + 1;
            curIdeal = reflexivePower( cartIndex, canIdeal );
            locPrincList = isLocallyPrincipalIdeal curIdeal;
            if locPrincList#0 then fflag = true
        );
    if cartIndex <= 0 or not fflag then error "testIdeal: Ring does not appear to be Q-Gorenstein, perhaps increase the option MaxCartierIndex.  Also see the documentation for isFRegular.";
    if (pp-1) % cartIndex == 0 then
    (
        J1 := testElement( R1, AssumeDomain => o.AssumeDomain );
        h1 := sub(0, ambient R1);
        try h1 = QGorensteinGenerator( 1, R1 ) then (
            computedTau = ascendIdeal( 1, h1, sub( ideal J1, R1 ), FrobeniusRootStrategy => o.FrobeniusRootStrategy );
            computedFlag = true
        )
        else computedFlag = false
    );
    if not computedFlag then --if we haven't already computed it
    (
        gg := first first entries gens trim canIdeal;
        dualCanIdeal := (ideal(gg) : canIdeal);
        nMinusKX := reflexivePower(cartIndex, dualCanIdeal);
        gensList := first entries gens trim nMinusKX;

        runningIdeal := ideal 0_R1;
        omegaAmb := sub( canIdeal, ambient R1 ) + ideal R1;
    	u1 := frobeniusTraceOnCanonicalModule( ideal R1, omegaAmb );
        for x in gensList do
            runningIdeal = runningIdeal + (internalTestModule(1/cartIndex, sub(x, R1), canIdeal, u1, passOptions(o, { FrobeniusRootStrategy, AssumeDomain }) ))#0;
        newDenom := reflexify( canIdeal * dualCanIdeal );
        computedTau = ( runningIdeal*R1 ) : newDenom;
    );
    computedTau
)

--this computes \tau(R, f^t)
testIdeal ( Number, RingElement ) := o -> ( t1, f1 ) ->
    testIdeal( { t1/1 }, { f1 }, o )

testIdeal ( List, List ) := o -> ( tList, fList ) ->
(
    R1 := ring fList#0;
    canIdeal := canonicalIdeal R1;
    pp := char R1;
    cartIndex := 0;
    fflag := false;
    computedFlag := false;
    curIdeal := ideal 0_R1;
    locPrincList := null;
    computedTau := ideal 0_R1;
    if o.QGorensteinIndex > 0 then cartIndex = o.QGorensteinIndex
    else
        while not fflag and cartIndex < o.MaxCartierIndex do
	(
            cartIndex = cartIndex + 1;
            curIdeal = reflexivePower( cartIndex, canIdeal );
            locPrincList = isLocallyPrincipalIdeal curIdeal;
            if locPrincList#0 then fflag = true
        );
    if not fflag then error "testIdeal: Ring does not appear to be Q-Gorenstein, perhaps increase the option MaxCartierIndex.  Also see the documentation for isFRegular.";
    if (pp-1) % cartIndex == 0 then
    (
        J1 := testElement( R1, AssumeDomain => o.AssumeDomain );
        h1 := sub(0, ambient R1);
        try (h1 = QGorensteinGenerator( 1, R1)) then
	    computedTau = internalTestModule(tList, fList, ideal 1_R1, { h1 }, passOptions( o, { FrobeniusRootStrategy, AssumeDomain } ) )
        else computedFlag = false;
    );
    if not computedFlag then
    (
        gg := first first entries gens trim canIdeal;
        dualCanIdeal := ( ideal gg : canIdeal );
        nMinusKX := reflexivePower( cartIndex, dualCanIdeal );
        gensList := first entries gens trim nMinusKX;

        runningIdeal := ideal 0_R1;
        omegaAmb := sub( canIdeal, ambient R1 );
    	u1 := frobeniusTraceOnCanonicalModule( ideal R1, omegaAmb );
        t2 := append( tList, 1/cartIndex );
        f2 := fList;
        for x in gensList do
	(
            f2 = append( fList, x );
            runningIdeal = runningIdeal + ( internalTestModule( t2, f2, canIdeal, u1, passOptions( o, { FrobeniusRootStrategy, AssumeDomain } ) ) )#0;
        );
        newDenom := reflexify( canIdeal * dualCanIdeal );
        computedTau = ( runningIdeal*R1 ) : newDenom;
    );
    computedTau
)

--We can now check F-regularity

isFRegular = method(
    Options =>
    {
	AssumeDomain => false,
	DepthOfSearch => 2,
	MaxCartierIndex => 10,
	AtOrigin => false,
	FrobeniusRootStrategy => Substitution,
	QGorensteinIndex => 0
    }
)

isFRegular Ring := o -> R1 ->
(
    if o.QGorensteinIndex == infinity then
        return nonQGorensteinIsFregular( o.DepthOfSearch, {0}, {1_R1}, R1, passOptions( o, { AssumeDomain, FrobeniusRootStrategy } ) );
  --      return nonQGorensteinIsFregular( o.DepthOfSearch, {0}, {1_R1}, R1, AssumeDomain => o.AssumeDomain, FrobeniusRootStrategy => o.FrobeniusRootStrategy );
    tau := testIdeal( R1, passOptions( o, { AssumeDomain, MaxCartierIndex, FrobeniusRootStrategy, QGorensteinIndex } ) );
--    tau := testIdeal( R1, AssumeDomain => o.AssumeDomain, MaxCartierIndex => o.MaxCartierIndex, FrobeniusRootStrategy => o.FrobeniusRootStrategy, QGorensteinIndex => o.QGorensteinIndex);
    if o.AtOrigin then isSubset( ideal 1_R1, tau + maxIdeal R1 )
    else isSubset( ideal 1_R1, tau )
)

isFRegular ( Number, RingElement ) := o -> ( tt, ff ) ->
(
    tt = tt/1;
    if o.QGorensteinIndex == infinity then
        return nonQGorensteinIsFregular( o.DepthOfSearch, {tt}, {ff}, ring ff, passOptions( o, { AssumeDomain, FrobeniusRootStrategy } ) );
--        return nonQGorensteinIsFregular( o.DepthOfSearch, {tt}, {ff}, ring ff, AssumeDomain => o.AssumeDomain,  FrobeniusRootStrategy => o.FrobeniusRootStrategy );
    R1 := ring ff;
    tau := testIdeal( tt, ff, passOptions( o, { AssumeDomain, MaxCartierIndex, FrobeniusRootStrategy, QGorensteinIndex } ) );
--    tau := testIdeal( tt, ff, AssumeDomain => o.AssumeDomain, MaxCartierIndex => o.MaxCartierIndex, FrobeniusRootStrategy => o.FrobeniusRootStrategy, QGorensteinIndex => o.QGorensteinIndex );
    if o.AtOrigin then isSubset( ideal 1_R1, tau + maxIdeal R1 )
    else isSubset( ideal 1_R1, tau )
)

isFRegular ( List, List ) := o -> ( ttList, ffList ) ->
(
    if o.QGorensteinIndex == infinity then
        return nonQGorensteinIsFregular( o.DepthOfSearch, ttList, ffList, ring ffList#0, passOptions( o, { AssumeDomain, FrobeniusRootStrategy } ) );
--        return nonQGorensteinIsFregular( o.DepthOfSearch, ttList, ffList, ring ffList#0, AssumeDomain => o.AssumeDomain,  FrobeniusRootStrategy => o.FrobeniusRootStrategy );
    R1 := ring ffList#0;
    tau := testIdeal( ttList, ffList, passOptions( o, { AssumeDomain,, MaxCartierIndex, FrobeniusRootStrategy, QGorensteinIndex } ) );
--    tau := testIdeal( ttList, ffList, AssumeDomain => o.AssumeDomain, MaxCartierIndex => o.MaxCartierIndex, FrobeniusRootStrategy => o.FrobeniusRootStrategy, QGorensteinIndex => o.QGorensteinIndex);
    if o.AtOrigin then isSubset( ideal 1_R1, tau + maxIdeal R1 )
    else isSubset( ideal 1_R1, tau )
)

--the following function is an internal function, it tries to prove a ring is F-regular (it can't prove it is not F-regular, it will warn the user if DebugLevel is elevated)
nonQGorensteinIsFregular = method(
    Options => { AssumeDomain => false, FrobeniusRootStrategy => Substitution }
)

nonQGorensteinIsFregular ( ZZ, List, List, Ring ) := o -> ( n1, ttList, ffList, R1 ) ->
(
    e := 1;
    cc := (sub(product(apply(#ffList, i -> (ffList#i)^(ceiling(ttList#i)) )), ambient(R1)))*testElement(R1, AssumeDomain => o.AssumeDomain);
    pp := char R1;
    I1 := ideal R1;
    ff1List := apply(ffList, ff->sub(ff, ambient(R1)));
    J1 := I1;
    testApproximate := I1;
    while e < n1 do
    (
        J1 = (frobenius(e, I1)) : I1;
        testApproximate = frobeniusRoot(e, apply(ttList, tt -> ceiling(tt*(pp^e - 1))), ff1List, cc*J1,  FrobeniusRootStrategy => o.FrobeniusRootStrategy);
        if (isSubset(ideal(sub(1, ambient(R1))), testApproximate)) then return true;
        e = e+1;
    );
    if debugLevel > 0 then print "isFRegular: This ring does not appear to be F-regular.  Increasing DepthOfSearch will let the function search more deeply.";
    false
)

----------------------------------------------------------------
--************************************************************--
--Functions for checking whether a ring/pair is F-pure--
--************************************************************--
----------------------------------------------------------------

-- Given an ideal I of polynomial ring R
-- this uses Fedder's Criterion to check if R/I is F-pure
-- Recall that this involves checking if I^[p]:I is in m^[p]
-- Note:  We first check if I is generated by a regular sequence.

isFPure = method(
    Options => { FrobeniusRootStrategy => Substitution, AtOrigin => false }
)

isFPure Ring := o -> R1 ->
    isFPure( ideal R1, o )

isFPure Ideal := o -> I1 ->
(
    p1 := char ring I1;
    if o.AtOrigin then
    (
        maxideal := maxIdeal I1;
        if codim(I1) == numgens(I1) then
	(
	    L := flatten entries gens I1;
	    not isSubset(
		ideal( product( L, l -> l^(p1-1) ) ),
		frobenius maxideal
	    )
    	)
        else not isSubset( frobenius( I1 ) : I1, frobenius maxideal )
    )
    else (
        nonFPureLocus := frobeniusRoot( 1, frobenius( I1 ) : I1, FrobeniusRootStrategy => o.FrobeniusRootStrategy );
        nonFPureLocus == ideal( sub( 1, ring I1 ) )
    )
)
