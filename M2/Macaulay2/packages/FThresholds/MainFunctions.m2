--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
---------------------------------------------------------------------------------
-- CONTENTS
---------------------------------------------------------------------------------
--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

---------------------------------------------------------------------------------
-- Nu computations

-- Main functions: nuList, nu, muList, mu

-- Auxiliary Functions: nu1, binarySearch, binarySearchRecursive, linearSearch,
--     testPower, testRoot, testFrobeniusPower, nuInternal

---------------------------------------------------------------------------------
-- FThreshold approximations

-- Main functions: fptApproximation, ftApproximation, 
--     criticalExponentApproximation

---------------------------------------------------------------------------------
-- FThreshold computations and estimates

-- Main function: fpt

-- Auxiliary functions: fSig, guessFPT(?)

----------------------------------------------------------------------------------
-- FPT/F-Jumping exponent check

-- Main functions: compareFPT, isFPT, isFJumpingExponent

-- Auxiliary functions: getNonzeroGenerator, isLocallyPrincipalIdeal, 
-- getDivisorIndex, compareFPTPoly, isFJumpingExponentPoly


--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
---------------------------------------------------------------------------------
-- Functions for computing (nu_I)^J(p^e), (nu_f)^J(p^e)
---------------------------------------------------------------------------------
--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

---------------------------------------------------------------------------------
-- nu1(I,J) finds the maximal N such that I^N is not contained in J, i.e., nu_I^J(1)
nu1 = method( TypicalValue => ZZ )

nu1 ( Ideal, Ideal ) :=  ZZ => ( I, J ) ->
(
    if not isSubset( I, radical J ) then
        error "nu1: The first ideal is not contained in the radical of the second";
    d := 1;
    while not isSubset( I^d, J ) do d = d + 1;
    d - 1
)

-- for polynomials, we use fastExponentiation
nu1 ( RingElement, Ideal ) := ZZ => ( f, J ) ->
(
    if not isSubset( ideal f, radical J ) then
        error "nu1: The polynomial is not contained in the radical of the ideal";
    d := 1;
    while not isSubset( ideal fastExponentiation( d, f ), J ) do d = d + 1;
    d - 1
)

---------------------------------------------------------------------------------
-- TESTS
---------------------------------------------------------------------------------

-- purpose is to verify containment in Frobenius powers

-- testRoot(J,a,I,e) checks whether J^a is a subset of I^[p^e] by checking whether (J^a)^[1/p^e] is a subset of I
testRoot = ( J, a, I, e ) -> isSubset( frobeniusRoot( e, a, J ), I )

-- testPower(J,a,I,e) checks whether J^a is  a subset of I^[p^e], directly
testPower = method( TypicalValue => Boolean )

testPower ( Ideal, ZZ, Ideal, ZZ ) := Boolean => ( J, a, I, e ) ->
    isSubset( J^a, frobenius( e, I ) )

-- for polynomials, use fastExponentiation
testPower ( RingElement, ZZ, Ideal, ZZ ) := Boolean => ( f, a, I, e ) ->
    isSubset( ideal fastExponentiation( a, f ), frobenius( e, I ) )

-- testFrobeniusPower(J,a,I,e) checks whether J^[a] is a subset of I^[p^e]
testFrobeniusPower = method( TypicalValue => Boolean )

testFrobeniusPower ( Ideal, ZZ, Ideal, ZZ ) := Boolean => ( J, a, I, e ) ->
    isSubset( frobeniusPower( a, J ), frobenius( e, I ) )

testFrobeniusPower ( RingElement, ZZ, Ideal, ZZ ) := Boolean => ( f, a, I, e ) ->
    testRoot( f, a, I, e )

-- hash table to select test function from option keyword
test := new HashTable from
    {
	FrobeniusPower => testFrobeniusPower,
	FrobeniusRoot => testRoot,
	StandardPower => testPower
    }

---------------------------------------------------------------------------------
-- SEARCH FUNCTIONS
---------------------------------------------------------------------------------

-- Each *Search(I,J,e,a,b,testFunction) searches for the last n in [a,b) such that
-- testFunction(I,n,J,e) is false, assuming that test(I,a,J,e) is false and test(I,b,J,e)
-- is true.

-- Non-recursive binary search, based on our previous code
binarySearch = ( I, J, e, startPt, endPt, testFunction ) ->
(
    a := startPt;
    b := endPt;
    local c;
    while b > a+1 do
    (
	c = floor( (a+b)/2 );
	if testFunction( I, c, J, e ) then b = c else a = c
    );
    a
)

-- Recursive binary search
binarySearchRecursive = ( I, J, e, a, b, testFunction ) ->
(
    if b <= a+1 then return a;
    c := floor( (a+b)/2 );
    if testFunction( I, c, J, e )
        then binarySearchRecursive( I, J, e, a, c, testFunction )
	else binarySearchRecursive( I, J, e, c, b, testFunction )
)

-- Linear search
linearSearch = ( I, J, e, a, b, testFunction ) ->
(
    c := a+1;
    while not testFunction( I, c, J, e ) do c = c+1;
    c-1
)

-- hash table to select search function from option keyword
search := new HashTable from
    {
	Binary => binarySearch,
	BinaryRecursive => binarySearchRecursive,
	Linear => linearSearch
    }

---------------------------------------------------------------------------------
-- OPTION PACKAGES
---------------------------------------------------------------------------------

optMuList :=
{
    UseColonIdeals => false,
    Search => Binary
}

optNuList := optMuList | {ContainmentTest => null, UseSpecialAlgorithms => true}

optMu := optMuList | { ComputePreviousNus => true }
optNu := optNuList | { ComputePreviousNus => true }

---------------------------------------------------------------------------------
-- INTERNAL FUNCTION
---------------------------------------------------------------------------------

nuInternal = optNu >> o -> ( n, f, J ) ->
(
    --------------------
    -- A TRIVIAL CASE --
    --------------------
    -- Return answer in a trivial case (per Blickle-Mustata-Smith convention)
    if f == 0 then return toList( (n+1):0 );

    -----------------
    -- SOME CHECKS --
    -----------------
    -- Verify if option values are valid
    checkOptions( o,
	{
	    ComputePreviousNus => Boolean,
	    ContainmentTest => { StandardPower, FrobeniusRoot, FrobeniusPower, null },
	    Search => { Binary, Linear, BinaryRecursive },
	    UseColonIdeals => Boolean,
	    UseSpecialAlgorithms => Boolean
	}
    );
    -- Check if f is in a polynomial ring over a finite field
    if not isPolynomialRingOverFiniteField ring f then
        error "nuInternal: expected polynomial or ideal in a polynomial ring over a finite field";
    -- Check if f is a principal ideal; if so, replace it with its generator, 
    --   so that fastExponentiation can be used
    isPrincipal := false;  
    g := f;
    if isIdeal g then
    (
        if numgens( g = trim g ) == 1 then
        (
	    isPrincipal = true;
	    g = g_*_0
        )
    )
    else isPrincipal = true;    
    -----------------------------------------
    
    p := char ring g;

    -----------------------------------------
    -- WHEN SPECIAL ALGORITHMS CAN BE USED --
    -----------------------------------------
    -- Deal with some special cases for principal ideals
    if isPrincipal and J == maxIdeal g then
    (
	if not isSubset( ideal g, J ) then 
	    error "nuInternal: the polynomial is not in the homogeneous maximal ideal";
        if not isSubset( ideal g^(p-1), frobenius J ) then -- fpt = 1
	    return apply( n+1, i -> p^i-1 );
        if o.UseSpecialAlgorithms then
        (
	    fpt := null;
	    if isDiagonal g then fpt = diagonalFPT g;
            if isBinomial g then fpt = binomialFPT g;
	    if fpt =!= null then 
	        return apply( n+1, i -> p^i*adicTruncation( p, i, fpt ) )
        )
    );

    -----------
    -- SETUP --
    -----------
    searchFct := search#(o.Search);
    conTest := o.ContainmentTest;
    -- choose appropriate containment test, if not specified by user
    if conTest === null then
	conTest = if isPrincipal then FrobeniusRoot else StandardPower;
    testFct := test#(conTest);
    local N;
    nu := nu1( g, J ); -- if f is not in rad(J), nu1 will return an error
    theList := { nu };

    --------------------------------------
    -- WHEN COMPUTE PREVIOUS NUS IS OFF --
    --------------------------------------
    if not o.ComputePreviousNus then
    (
	-- This computes nu in a non-recursive way
	if n == 0 then return theList;
 	N = if isPrincipal or conTest === FrobeniusPower
	     then p^n else (numgens trim J)*(p^n-1)+1;
     	return { searchFct( g, J, n, nu*p^n, (nu+1)*N, testFct ) }
    );
    ---------------------------------
    -- WHEN USE COLON IDEALS IS ON --
    ---------------------------------
    if o.UseColonIdeals and isPrincipal then
    -- colon ideals only work for polynomials
    (
	-- This computes nu recursively, using colon ideals.
	-- Only nu(p)'s are computed, but with respect to ideals other than J
	I := J;
	scan( 1..n, e ->
	    (
		I = I : ideal( fastExponentiation( nu, g ) );
		nu =  last nuInternal( 1, g, I, ContainmentTest => conTest );
	      	theList = append( theList, p*(last theList) + nu );
	      	I = frobenius I
	    )
	)
    )
    else
    ----------------------
    -- EVERY OTHER CASE --
    ----------------------
    (
	N = if isPrincipal or conTest === FrobeniusPower
	     then p else (numgens trim J)*(p-1)+1;
	scan( 1..n, e ->
	    (
		nu = searchFct( g, J, e, p*nu, (nu+1)*N, testFct );
    	       	theList = append( theList, nu )
    	    )
    	)
     );
     theList
)

---------------------------------------------------------------------------------
-- EXPORTED METHODS
---------------------------------------------------------------------------------

nuList = method( Options => optNuList, TypicalValue => List );

nuList ( ZZ, Ideal, Ideal ) := List => o -> ( e, I, J ) ->
    nuInternal( e, I, J, o )

nuList ( ZZ, RingElement, Ideal ) := List => o -> ( e, I, J ) ->
    nuInternal( e, I, J, o )

nuList ( ZZ, Ideal ) :=  List => o -> ( e, I ) ->
    nuList( e, I, maxIdeal I, o )

nuList ( ZZ, RingElement ) := List => o -> ( e, f ) ->
    nuList( e, f, maxIdeal f, o )

nu = method( Options => optNu, TypicalValue => ZZ );

nu ( ZZ, Ideal, Ideal ) := ZZ => o -> ( e, I, J ) ->
    last nuInternal( e, I, J, o )

nu ( ZZ, RingElement, Ideal ) := ZZ => o -> ( e, f, J ) ->
    last nuInternal( e, f, J, o )

nu ( ZZ, Ideal ) := ZZ => o -> ( e, I ) -> nu( e, I, maxIdeal I, o )

nu ( ZZ, RingElement ) := ZZ => o -> ( e, f ) -> nu( e, f, maxIdeal f, o )

-- Mus can be computed using nus, by using ContainmentTest => FrobeniusPower. 
-- For convenience, here are some shortcuts:

muList = method( Options => optMuList, TypicalValue => List )

muList ( ZZ, Ideal, Ideal ) := List => o -> (e, I, J) ->
    nuList( e, I, J, o, ContainmentTest => FrobeniusPower )

muList ( ZZ, Ideal ) := List => o -> (e, I) ->
    nuList( e, I, o, ContainmentTest => FrobeniusPower )

muList ( ZZ, RingElement, Ideal ) := List => o -> (e, f, J) ->
    nuList( e, f, J, o, ContainmentTest => FrobeniusPower )

muList ( ZZ, RingElement ) := List => o -> (e, f) ->
    nuList( e, f, o, ContainmentTest => FrobeniusPower )

mu = method( Options => optMu, TypicalValue => ZZ )

mu ( ZZ, Ideal, Ideal ) := ZZ => o -> (e, I, J) ->
    nu( e, I, J, ContainmentTest => FrobeniusPower )

mu ( ZZ, Ideal ) := ZZ => o -> (e, I) ->
    nu( e, I, ContainmentTest => FrobeniusPower )

mu ( ZZ, RingElement, Ideal ) := ZZ => o -> (e, f, J) ->
    nu( e, f, J, ContainmentTest => FrobeniusPower )

mu ( ZZ, RingElement ) := ZZ => o -> (e, f) ->
    nu(e, f, ContainmentTest => FrobeniusPower )

--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
---------------------------------------------------------------------------------
-- Functions for approximating, guessing, estimating F-Thresholds and crit exps
---------------------------------------------------------------------------------
--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

--Approximates the F-pure Threshold
--Gives a list of nu_I(p^d)/p^d for d=1,...,e
fptApproximation = method( TypicalValue => List )

fptApproximation ( ZZ, Ideal ) := List => ( e, I ) ->
(
     p := char ring I;
     nus := nuList( e, I );
     apply( nus, 0..e, (n,k) -> n/p^k )
)

fptApproximation ( ZZ, RingElement ) := List => ( e, f ) ->
    fptApproximation( e, ideal f )

--Approximates the F-Threshold with respect to an ideal J
--More specifically, this gives a list of nu_I^J(p^d)/p^d for d=1,...,e

ftApproximation = method( TypicalValue => List )

ftApproximation ( ZZ, Ideal, Ideal ) := List => ( e, I, J ) ->
(
    if not isSubset( I, radical J ) then
        error "ftApproximation: F-threshold undefined";
    p := char ring I;
    nus := nuList( e, I, J );
    apply( nus, 0..e, (n,k) -> n/p^k )
)

ftApproximation ( ZZ, RingElement, Ideal ) := List => ( e, f, J ) ->
   ftApproximation( e, ideal(f), J )

criticalExponentApproximation = method( TypicalValue => List )

criticalExponentApproximation ( ZZ, Ideal, Ideal ) := List => ( e, I, J ) ->
(
    if not isSubset( I, radical J ) then
        error "criticalExponentApproximation: critical exponent undefined";
    p := char ring I;
    mus := muList( e, I, J );
    apply( mus, 0..e, (n,k) -> n/p^k )
)

criticalExponentApproximation (ZZ, RingElement, Ideal) := List => ( e, f, J ) ->
    criticalExponentApproximation( e, ideal f, J )

--Gives a list of guesses for the F-pure threshold of f.  It returns a list of all numbers in
--the range suggested by nu(e,  ) with maxDenom as the maximum denominator
fptGuessList = ( f, e, maxDenom ) ->
(
    n := nu(e,f);
    p := char ring f;
    findNumberBetween( maxDenom, n/(p^e-1), (n+1)/p^e )
)

----------------------------------------------------------------
--************************************************************--
--Auxiliary functions for F-signature and Fpt computations.   --
--************************************************************--
----------------------------------------------------------------

--- Computes the F-signature for a specific value a/p^e
--- Input:
---	f - some polynomial in two or three variables in a ring R of PRIME characteristic
---	a - some positive integer between 0 and p^e
---	e - some positive integer
--- Output:
---	returns value of the F-signature of the pair (R, f^{a/p^e})
--- Code is based on work of Eric Canton
fSig := ( f, a, e ) ->
(
     R := ring f;
     p := char R;
     1 - p^( -e * dim( R ) ) * degree( frobenius( e, maxIdeal R ) + ideal( fastExponentiation( a, f ) ) )
)

-- isInteger checks if a rational number is an integer
isInteger := x -> x == floor x
    
-- guessFPT takes a polynomial f, endpoints a and b of an interval that contains 
-- the F-pure threshold of f, and a positive integer that tells the max number 
-- of checks the user wants to perform.
-- It returns either fpt(f), if found, or an interval containing it, if not.
-- It currently chooses numbers in the interval with minimal denominator.
-- In the future, different strategies should be implemented (e.g., use 
-- only/first denominators that are multiple of the characteristic). 
guessFPT := { Verbose => false } >> o -> ( f, a, b, maxChecks ) ->
(
    if o.Verbose then print "\nStarting guessFPT ...";
    -- Check if fpt is the upper bound b         
    if isFPT( b, f ) then 
    (
        if o.Verbose then print( "\nfpt is the right-hand endpoint." ); 
        return b
    )
    else if o.Verbose then print "\nThe right-hand endpoint is not the fpt ...";
    -- Check if fpt is the lower bound a
    if maxChecks >= 2 then 
        if not isFRegular( a, f ) then 
	(
	    if o.Verbose then 
	        print( "\nfpt is the left-hand endpoint." ); 
	    return a
	)
        else if o.Verbose then print "\nThe left-hand endpoint is not the fpt ...";
    -- Now proceed with more checks, selecting numbers in the current 
    --   interval with minimal denominator
    counter := 3;
    local t;
    local comp;
    ( A, B ) := ( a, b );
    d := 2; 
    while counter <= maxChecks do
    (   
	-- search for number with minimal denominator in the open interval (A,B)
	while floor(d*B) < ceiling(d*A) or isInteger(d*B) or isInteger(d*A) do 
            d = d+1;
        t = ceiling(d*A)/d;  	
        comp = compareFPT(t,f);
	if comp == 0 then  -- found exact FPT!
	(
	    if o.Verbose then
	        print( "\nguessFPT found the exact value for fpt(f) in try number " | toString( counter ) | "." );
	    return t
	); 
        if comp == 1 then B = t; -- fpt < t       
	if comp == -1 then A = t; -- fpt > t
	counter = counter + 1
    );
    if o.Verbose then 
        print( "\nguessFPT narrowed the interval down to ( " | toString( A ) | ", " | toString( B ) | " ) ..." );    
    { A, B }
)

-- The default number of "random" checks to be performed
maxChecks := 3;

-- F-pure threshold estimation, at the origin.
-- e is the max depth to search in.
-- FRegularityCheck is whether the last isFRegularPoly is run 
--   (which can take a significant amount of time).
fpt = method(
    Options =>
        {
	    DepthOfSearch => 1,
	    FRegularityCheck => false,
	    MaxChecks => maxChecks,
	    UseSpecialAlgorithms => true,
	    UseFSignature => false,	    
	    Verbose => false
	}
)

fpt RingElement := o -> f ->
(
    ------------------------------
    -- DEAL WITH A TRIVIAL CASE --
    ------------------------------
    if f == 0 then return 0;

    ---------------------
    -- RUN SEVERAL CHECKS
    ---------------------
    -- Check if option values are valid
    checkOptions( o,
        {
	    DepthOfSearch => ( k -> instance( k, ZZ ) and k > 0 ),
	    FRegularityCheck => Boolean,
	    MaxChecks => ( k -> instance( k, ZZ ) and k >= 0 ),
	    UseSpecialAlgorithms => Boolean,
	    UseFSignature => Boolean,	    
	    Verbose => Boolean
	}
    );
    -- Check if polynomial has coefficients in a finite field
    if not isPolynomialOverFiniteField f  then
        error "fpt: expected polynomial with coefficients in a finite field";
    -- Check if polynomial is in the homogeneous maximal ideal
    M := maxIdeal f;   -- The maximal ideal we are computing the fpt at
    p := char ring f;
    if not isSubset( ideal f, M ) then
        error "fpt: polynomial is not in the homogeneous maximal ideal";

    ----------------------
    -- CHECK IF FPT = 1 --
    ----------------------
    if o.Verbose then print "\nStarting fpt ...";
    if not isSubset( ideal f^(p-1), frobenius M ) then
    (
        if o.Verbose then print "\nnu(1,f) = p-1, so fpt(f) = 1.";
        return 1
    );
    if o.Verbose then print "\nfpt is not 1 ...";
    
    
    ---------------------------------------------
    -- CHECK IF SPECIAL ALGORITHMS CAN BE USED --
    ---------------------------------------------
    if o.UseSpecialAlgorithms then
    (
	if o.Verbose then print "\nVerifying if special algorithms apply...";
	if isDiagonal f then
	(
	    if o.Verbose then
	        print "\nPolynomial is diagonal; calling diagonalFPT ...";
            return diagonalFPT f
        );
        if isBinomial f then
        (
            if o.Verbose then
	        print "\nPolynomial is a binomial; calling binomialFPT ...";
            return binomialFPT f
        );
        if isBinaryForm f then
        (
            if o.Verbose then
	        print "\nPolynomial is a binary form; calling binaryFormFPT ...";
            return binaryFormFPT( f, Verbose => o.Verbose )
        )
    );
    if o.Verbose then print "\nSpecial fpt algorithms were not used ...";

    -----------------------------------------------
    -- COMPUTE NU TO FIND UPPER AND LOWER BOUNDS --
    -----------------------------------------------
    e := o.DepthOfSearch;
    n := nu( e, f );
    LB := n/(p^e-1); -- lower bound (because of forbidden intervals)
    UB := (n+1)/p^e; -- upper bound
    strictLB := false; -- at this point, LB and UB *could* be the fpt
    strictUB := false;
    if o.Verbose then
    (
         print( "\nnu has been computed: nu = nu(" | toString e | ",f) = " | toString n | " ..." );
	 print( "\nfpt lies in the interval [ nu/(p^e-1), (nu+1)/p^e ] = [ " | toString LB | ", " | toString UB | " ] ..." )
    );

    --------------------
    -- CALL GUESS FPT --
    --------------------
    if o.MaxChecks > 0 then
    (
	guess := guessFPT( f, LB, UB, o.MaxChecks, Verbose => o.Verbose );
	if class guess =!= List then return guess; -- guessFPT was successful
	-- if not sucessful, adjust bounds and their strictness
	( LB, UB ) = toSequence guess; 
	strictUB = true;
	if o.MaxChecks >= 2 then strictLB = true	
    );

    ---------------------------------------
    -- F-SIGNATURE INTERCEPT COMPUTATION --
    ---------------------------------------
    if o.UseFSignature then
    (
        if o.Verbose then print "\nBeginning F-signature computation ...";
        s1 := fSig( f, n-1, e );
        if o.Verbose then
	    print( "\nFirst F-signature computed: s(f,(nu-1)/p^e) = " | toString s1 | " ..." );
        s2 := fSig( f, n, e );
        if o.Verbose then
            print( "\nSecond F-signature computed: s(f,nu/p^e) = " | toString s2 | " ..." );
        -- Compute intercept of line through ((nu-1)/p^2,s1) and (nu/p^e,s2)
        int := xInt( (n-1)/p^e, s1, n/p^e, s2 );
        if o.Verbose then
            print("\nComputed F-signature secant line intercept: " | toString int | " ...");
        -- Now check to see if F-signature line crosses at UB. If so, then that's the fpt
        if UB == int then
        (
	    if  o.Verbose then
	        print "\nF-signature secant line crosses at the upper bound, so that is the fpt.";
	    return int
        );
        -- Compare the intercept with the current lower bound        
	if LB < int then
        (
	    if o.Verbose then
	        print "\nF-signature intercept is an improved lower bound ...";
	    LB = int;
	    strictLB = false;
        )
        else if o.Verbose then
            print "\nF-signature computation failed to find an improved lower bound ...";
    );

    ------------------------------
    -- FINAL F_REGULARITY CHECK --
    ------------------------------
    if o.FRegularityCheck and not strictLB then
    (
	if o.Verbose then print "\nStarting final check ...";
        if not isFRegular( LB, f ) then
        (
	   if o.Verbose then
	       print "\nFinal check successful; fpt is the lower bound.";
	   return LB
      	)
	else
	(
	    if o.Verbose then print "\nFRegularityCheck did not find the fpt ...";
	    strictLB = true
	)
    );
    if o.Verbose then
    (
	print "\nfpt failed to find the exact answer; try increasing the value of DepthOfSearch or MaxChecks.";
        print(
	    "\nfpt lies in the interval " |
	    ( if strictLB then "( " else "[ " ) |
	    toString LB |
	    ", " |
	    toString UB |
	    ( if strictUB then " )." else " ]." )
        )
    );
    { LB, UB }
)

-- Special template for products of linear forms in two variables 
fpt ( List, List ) := o -> ( L, m ) ->
    binaryFormFPT( L, m, Verbose => o.Verbose )

--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
---------------------------------------------------------------------------------
-- Functions for checking if given numbers are F-jumping numbers
---------------------------------------------------------------------------------
--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

--compareFPT see if a number is less than, equal to, or greater than the FPT.
--It returns -1 if less
--it returns 0 if equal
--it returns 1 if greater
compareFPT = method(
    Options =>
    {
	MaxCartierIndex => 10,
	FrobeniusRootStrategy => Substitution,
	AssumeDomain => true,
	QGorensteinIndex => 0
    },
    TypicalValue => ZZ
)

--gets a nonzero generator of an ideal.
getNonzeroGenerator := I -> 
(
    gen := select( 1, I_*, x -> x != 0 );
    if gen === {} then null else first gen
)

isLocallyPrincipalIdeal := I -> 
(
    localGen := getNonzeroGenerator I;
    R := ring I;
    if localGen === null then return { false, 0_R };
    inverseIdeal := ideal( localGen ) : I;
    idealProduct := inverseIdeal * I;
    if reflexify idealProduct  == idealProduct then 
        return { true, inverseIdeal }
    else return { false, 0_R }
)

--helper function for compareFPT
getDivisorIndex := ( maxIndex, divisorialIdeal ) -> 
(
    fflag := false;
    cartIndex := 0;
    curIdeal := null;
    locPrincList := null;
    while not fflag and cartIndex < maxIndex do 
    (
        cartIndex = cartIndex + 1;
        curIdeal = reflexivePower( cartIndex, divisorialIdeal );
        locPrincList = isLocallyPrincipalIdeal curIdeal;
        if locPrincList#0 then fflag = true
    );
    if cartIndex <= 0 or not fflag then 
        error "getDivisorIndex: Ring does not appear to be Q-Gorenstein; perhaps increase the option MaxCartierIndex.  Also see the documentation for isFRegular.";
    return cartIndex
)

compareFPT(Number, RingElement) := ZZ => o -> (t, f) -> 
(
    -- Check if option values are valid
    checkOptions( o,
        {
	    MaxCartierIndex => ZZ,
	    FrobeniusRootStrategy => { Substitution, MonomialBasis },
	    AssumeDomain => Boolean,
	    QGorensteinIndex => ZZ
	}
    );

    --first we gather background info on the ring (QGorenstein generators, etc.)
    R1 := ring f;
    if isPolynomial f then return compareFPTPoly( t, f );
    S1 := ambient R1;
    I1 := ideal R1;
    canIdeal := canonicalIdeal R1;
    pp := char R1;
    cartIndex := 0;
    fList := { f };
    tList := { t };
    local computedTau;
    local computedHSLGInitial;
    local computedHSLG;
    --computedTau := ideal(sub(0, R1));
    if o.QGorensteinIndex > 0 then cartIndex = o.QGorensteinIndex
    else cartIndex = getDivisorIndex(o.MaxCartierIndex, canIdeal);
    h1 := 0_S1;
    --first we do a quick check to see if the test ideal is easy to compute
    if (pp-1)%cartIndex == 0 then 
    (
        J1 := testElement R1;
        try h1 = QGorensteinGenerator( 1, R1 ) then 
	(
            computedTau = testModule( tList, fList, ideal 1_R1, { h1 }, FrobeniusRootStrategy => o.FrobeniusRootStrategy, AssumeDomain=>o.AssumeDomain );
            if isUnitIdeal computedTau#0 then return -1  
	    --at this point we know that this is not the FPT
        )
        else h1 = 0_S1
    );
    --now compute the test ideal in the general way (if the index does not divide...)
    gg := first (trim canIdeal)_*;
    dualCanIdeal :=  ideal( gg ) : canIdeal;
    nMinusKX := reflexivePower( cartIndex, dualCanIdeal );
    gensList := (trim nMinusKX)_*;

    runningIdeal := ideal 0_R1;
    omegaAmb := sub( canIdeal, S1 ) + ideal R1;
    u1 := frobeniusTraceOnCanonicalModule( I1, omegaAmb );

    t2 := append( tList, 1/cartIndex );
    f2 := fList;

    for x in gensList do 
    (
        f2 = append( fList, x );
        runningIdeal = runningIdeal + (testModule( t2, f2, canIdeal, u1, FrobeniusRootStrategy => o.FrobeniusRootStrategy, AssumeDomain=>o.AssumeDomain ) )#0;
    );

    newDenom := reflexify( canIdeal * dualCanIdeal );
    computedTau = ( runningIdeal * R1 ) : newDenom;
    if isUnitIdeal computedTau then return -1;  
    --at this point we know that this is not the FPT

    --now we have to run the sigma computation
    if h1 != 0_S1 then 
    (
        baseTau:= testModule( 0/1, 1_R1, ideal 1_R1, { h1 }, FrobeniusRootStrategy => o.FrobeniusRootStrategy, AssumeDomain=>o.AssumeDomain );
        if isProper baseTau#0 then 
	    error "compareFPT: The ambient ring must be F-regular."; 
	    --the ambient isn't even F-regular
        decomposedExponent := decomposeFraction( pp, t, NoZeroC => true );
        (a1,b1,c1) := toSequence decomposedExponent;
        if a1 > (pp^c1-1) then
	(
            a1quot := floor( (a1-1)/(pp^c1-1) );
            a1rem := a1 - (pp^c1-1)*a1quot;
            computedHSLGInitial = HSLGModule( { a1rem/(pp^c1-1) }, { f }, baseTau#0, { h1 } );
            computedHSLG = frobeniusRoot( b1, { ceiling( (pp^b1 - 1)/(pp-1) ), a1quot }, { h1, sub( f, S1 ) }, sub( computedHSLGInitial#0, S1 ) );
        )
        else (
            computedHSLGInitial = HSLGModule( { a1/(pp^c1-1) }, { f }, baseTau#0, { h1 } ); 
	    --the e is assumed to be 1 here since we are implicitly doing stuff
            computedHSLG = frobeniusRoot( b1, ceiling( (pp^b1-1)/(pp-1) ), h1, sub( computedHSLGInitial#0, S1 ) );
        );
        if isProper( computedHSLG+I1 ) then return 1; 
	--the fpt we picked is too big
    )
    else
    --there should be an algorithm that works here
        error "compareFPT:  The current version requires that (p-1)K_R is Cartier (at least for the sigma part of the computation).  This error can also occur for non-graded rings that are Q-Gorenstein if there is a principal ideal that Macaulay2 cannot find the generator of.";
    return 0 
    --it is the FPT!
)

compareFPTPoly = method(Options => {FrobeniusRootStrategy => Substitution})

compareFPTPoly(Number, RingElement) := o -> (t, f) -> (
    --first we gather background info on the ring (QGorenstein generators, etc.)
    S1 := ring f;
    pp := char S1;
    cartIndex := 0;
    fList := { f };
    tList := { t };
    local computedTau;
    local computedHSLG;
    local computedHSLGInitial;
    --computedTau := ideal(sub(0, R1));

    h1 := 1_S1;
    --first we do a quick check to see if the test ideal is easy to compute
    computedTau = testModule( tList, fList, ideal 1_S1, { h1 }, o, AssumeDomain => true );
    if isUnitIdeal computedTau#0 then return -1;  
    --at this point we know that this is not the FPT

    --now we have to run the sigma computation
    decomposedExponent := decomposeFraction( pp, t, NoZeroC => true );
    (a1,b1,c1) := toSequence decomposedExponent;
    if a1 > (pp^c1-1) then
    (
        a1quot := floor( (a1-1)/(pp^c1-1) );
        a1rem := a1 - (pp^c1-1)*a1quot;
        computedHSLGInitial = HSLGModule( { a1rem/(pp^c1-1) }, { f }, ideal 1_S1, { h1 } );
        computedHSLG = frobeniusRoot( b1, { ceiling( (pp^b1-1)/(pp-1) ), a1quot }, { h1, f }, computedHSLGInitial#0 );
    )
    else 
    (
        computedHSLGInitial = HSLGModule( { a1/(pp^c1-1) }, { f }, ideal 1_S1, { h1 } ); 
	--the e is assumed to be 1 here since we are implicitly doing stuff
        computedHSLG = frobeniusRoot( b1, ceiling( (pp^b1-1)/(pp-1) ), h1, computedHSLGInitial#0 );
    );
    if isProper computedHSLG then return 1; 
    --the fpt we picked is too small

    return 0 --it is the FPT!
)

-- isInForbiddenInterval takes a prime number p and a rational number t
-- and checks whether t lies in an interval of the form (a/p^e,a/(p^e-1)), 
-- for some e. If it does, it cannot be an FPT in characteristic p.
isInForbiddenInterval = method( TypicalValue => Boolean )

isInForbiddenInterval ( ZZ, QQ ) := Boolean => ( p, t ) ->  
(
    if t < 0 or t > 1 then return true;
    (a,b,c) := toSequence decomposeFraction( p, t );
    valid := true;
    e := 1;
    while valid and e <= b+c do
    (
        if floor( (p^e-1)*t ) != p^e * adicTruncation( p, e, t ) then 
	    valid = false;
	e = e+1    
    );
    not valid 
)

isInForbiddenInterval ( ZZ, ZZ ) := Boolean => ( p, t ) -> 
    isInForbiddenInterval( p, t/1 )

--isFPT, determines if a given rational number is the FPT of a pair in a
-- polynomial ring.

isFPT = method( 
    Options => 
    {
	MaxCartierIndex => 10, 
	FrobeniusRootStrategy => Substitution, 
	AssumeDomain => true, 
	QGorensteinIndex => 0
    },
    TypicalValue => Boolean
)

-- Dan: We should use the "Origin" option somehow...
isFPT ( Number, RingElement ) := Boolean => o -> ( t, f ) ->
(
    if isInForbiddenInterval( char ring f, t ) then false else
        0 == compareFPT(t/1, f, o )
)

-- isFJumpingExponent determines if a given rational number is an
-- F-jumping exponent
--***************************************************************************
--This needs to be speeded up, like the above function
--***************************************************************************

-- Dan: isn't is safer to have AssumeDomain default to "false" here?
isFJumpingExponent = method( 
    Options => 
    {
	MaxCartierIndex => 10, 
	FrobeniusRootStrategy => Substitution, 
	AssumeDomain=>true, 
	QGorensteinIndex => 0
    }, 
    TypicalValue => Boolean 
)

isFJumpingExponent ( Number, RingElement ) := Boolean => o -> ( t, f ) ->
(
    -- Check if option values are valid
    checkOptions( o,
        {
	    MaxCartierIndex => ZZ,
	    FrobeniusRootStrategy => { Substitution, MonomialBasis },
	    AssumeDomain => Boolean,
	    QGorensteinIndex => ZZ
	}
    );

    --first we gather background info on the ring (QGorenstein generators, etc.)
    R1 := ring f;
    if class R1 === PolynomialRing then return isFJumpingExponentPoly(t, f);
    S1 := ambient R1;
    I1 := ideal R1;
    canIdeal := canonicalIdeal R1;
    pp := char R1;
    cartIndex := 0;
    fList := {f};
    tList := {t};
    computedTau := null;
    computedHSLG := null;
    computedHSLGInitial := null;
    --computedTau := ideal(sub(0, R1));

    if o.QGorensteinIndex > 0 then cartIndex = o.QGorensteinIndex
    else cartIndex = getDivisorIndex(o.MaxCartierIndex, canIdeal);
    h1 := 0_S1;
    --first we do a quick check to see if the test ideal is easy to compute
    if (pp-1) % cartIndex == 0 then 
    (
        J1 := testElement R1;
        try h1 = QGorensteinGenerator( 1, R1 ) then
            computedTau = testModule(tList, fList, ideal 1_R1, {h1}, FrobeniusRootStrategy => o.FrobeniusRootStrategy, AssumeDomain => o.AssumeDomain)
        else h1 = 0_S1
    )
    else--there should be an algorithm that works here
        error "isFJumpingExponent:  The current version requires that (p-1)K_R is Cartier (at least for the sigma part of the computation).  This error can also occur for non-graded rings that are Q-Gorenstein if there is a principal ideal that Macaulay2 cannot find the generator of.";

    --now compute the test ideal in the general way (if the index does not divide...)
    if computedTau =!= null then 
    ( --this code will be enabled eventually
        gg := first first entries gens trim canIdeal;
        dualCanIdeal := (ideal(gg) : canIdeal);
        nMinusKX := reflexivePower(cartIndex, dualCanIdeal);
        gensList := first entries gens trim nMinusKX;

        runningIdeal := ideal 0_R1;
        omegaAmb := sub(canIdeal, S1) + ideal(R1);
        u1 := (frobeniusTraceOnCanonicalModule(I1, omegaAmb));

        t2 := append(tList, 1/cartIndex);
        f2 := fList;

        for x in gensList do (
            f2 = append(fList, x);
            runningIdeal = runningIdeal + (testModule(t2, f2, canIdeal, u1, FrobeniusRootStrategy => o.FrobeniusRootStrategy, AssumeDomain=>o.AssumeDomain))#0;
        );

        newDenom := reflexify(canIdeal*dualCanIdeal);
        computedTau = (runningIdeal*R1) : newDenom;
    );
    --now we have to run the sigma computation
    if h1 != 0_S1 then 
    (
        baseTau := testModule(0/1, 1_R1, ideal 1_R1, {h1}, FrobeniusRootStrategy => o.FrobeniusRootStrategy, AssumeDomain => o.AssumeDomain );
        decomposedExponent := decomposeFraction( pp, t, NoZeroC => true );
        (a1,b1,c1) := toSequence decomposedExponent;
        if a1 > (pp^c1-1) then
	(
            a1quot := floor( (a1-1)/(pp^c1 - 1));
            a1rem := a1 - (pp^c1-1)*a1quot;
            computedHSLGInitial = HSLGModule({a1rem/(pp^c1-1)}, {f}, baseTau#0, {h1});
            computedHSLG = frobeniusRoot(b1, {ceiling( (pp^b1 - 1)/(pp-1) ), a1quot}, {h1, sub(f, S1)}, sub(computedHSLGInitial#0, S1))
        )
        else (
            computedHSLGInitial = HSLGModule({a1/(pp^c1 - 1)}, {f}, baseTau#0, {h1}); --the e is assumed to be 1 here since we are implicitly doing stuff
            computedHSLG = frobeniusRoot(b1, ceiling( (pp^b1 - 1)/(pp-1) ), h1, sub(computedHSLGInitial#0, S1))
        )
    )
    else--there should be an algorithm that works here
        error "isFJumpingExponent:  The current version requires that (p-1)K_R is Cartier (at least for the sigma part of the computation).  This error can also occur for non-graded rings that are Q-Gorenstein if there is a principal ideal that Macaulay2 cannot find the generator of.";
    return not isSubset( computedHSLG, I1 + sub(computedTau, S1) )
)

isFJumpingExponentPoly = method( 
    Options => { FrobeniusRootStrategy => Substitution } 
)

isFJumpingExponentPoly ( Number, RingElement ) := o -> ( t, f ) ->
(
    S1 := ring f;
    pp := char S1;
    cartIndex := 1;
    fList := {f};
    tList := {t};
    computedTau := null;
    computedHSLG := null;
    computedHSLGInitial := null;
    --computedTau := ideal(sub(0, R1));

    h1 := sub(1, S1);
    --first we do a quick check to see if the test ideal is easy to compute
    computedTau = (testModule(tList, fList, ideal(sub(1, S1)), {h1}, FrobeniusRootStrategy => o.FrobeniusRootStrategy, AssumeDomain=>true))#0;

    --now we have to run the sigma computation
    decomposedExponent := decomposeFraction(pp, t, NoZeroC => true);
    a1 := decomposedExponent#0;
    b1 := decomposedExponent#1;
    c1 := decomposedExponent#2;
    if (a1 > (pp^c1-1)) then(
        a1quot := floor( (a1-1)/(pp^c1 - 1));
        a1rem := a1 - (pp^c1-1)*a1quot;
        computedHSLGInitial = HSLGModule({a1rem/(pp^c1-1)}, {f}, ideal(sub(1, S1)), {h1});
        computedHSLG = frobeniusRoot(b1, {ceiling( (pp^b1 - 1)/(pp-1) ), a1quot}, {h1, f}, computedHSLGInitial#0);
    )
    else (
        computedHSLGInitial = HSLGModule({a1/(pp^c1 - 1)}, {f}, ideal(sub(1, S1)), {h1}); --the e is assumed to be 1 here since we are implicitly doing stuff
        computedHSLG = frobeniusRoot(b1, ceiling( (pp^b1 - 1)/(pp-1) ), h1, computedHSLGInitial#0);
    );

    return (not isSubset(computedHSLG, computedTau));
)
