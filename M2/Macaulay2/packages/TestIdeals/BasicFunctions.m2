--*************************************************
--*************************************************
--*************************************************
--*************************************************
--This file is used for doing basic computations
--i.e. things using only lists, numbers, etc.
-- that support other functions in the TestIdeals
--package.
--*************************************************
--*************************************************
--*************************************************
--*************************************************

--*************************************************
--Basic Manipulations with Numbers
--*************************************************
--===================================================================================

--Extend denominator and numerator to integers
denominator ZZ := x -> 1

numerator ZZ := x -> x

--===================================================================================

--Finds the fractional part of a number.
fracPart = x -> x - floor(x)

--===================================================================================

--Computes floor(log_b x), correcting problems due to rounding.
floorLog = method( TypicalValue => ZZ )

floorLog ( Number, Number ) := ZZ => ( b, x ) ->
(
    if b <= 1 then error "floorLog: expected the first argument to be greater than 1";
    if x <= 0 then error "floorLog: expected the second argument to be positive";
    if x < b then return 0;
    flog := floor( log_b x );
    while b^flog <= x do flog = flog + 1;
    flog - 1
)

--===================================================================================

multiplicativeOrder = method( TypicalValue => ZZ )

--eulerphi := n -> value(apply(factor n, i-> (i#0-1)*((i#0)^(i#1-1))))
--cyclicOrdPGroup := (pp, nn) -> ( return (factor(pp-1))*(new Power from {pp, (nn-1)}) );

--Finds the multiplicative order of a modulo b.
multiplicativeOrder ( ZZ, ZZ ) := ZZ => ( a, b ) ->
(
    if gcd( a, b ) != 1 then error "multiplicativeOrder: Expected numbers to be relatively prime";
    if b == 1 then return 1;
    maxOrder := lcm(apply(toList apply(factor b, i-> factor ((i#0-1)*((i#0)^(i#1-1)))), tt -> value tt));
    primeFactorList := sort unique apply( subsets( flatten apply(toList factor maxOrder, myPower -> apply(myPower#1, tt->myPower#0))), tt -> product tt);
--    potentialOrderList := sort unique flatten  apply(flatten apply(toList apply(toList factor b, tt -> cyclicOrdPGroup(tt#0, tt#1)), tt -> toList tt), myPower -> subsets apply(myPower#1, tt->myPower#0));
    i := 0;
    while i < #primeFactorList do 
    (
        if powermod(a, primeFactorList#i, b) == 1 then return primeFactorList#i;
        i = i + 1
    );
    error "multiplicativeOrder: Something went wrong; failed to find the multiplicative order";
)

--===================================================================================

decomposeFraction = method( TypicalValue => Sequence, Options => { NoZeroC => false } )

-- This function takes in a fraction t and a prime p and spits out a list
-- {a,b,c}, where t = a/(p^b*(p^c-1)). If c = 0, then this means that t = a/p^b.
-- Alternately, if NoZeroC => true, then we will always write t = a/p^b(p^c - 1),
-- even if it means increasing a.
decomposeFraction ( ZZ, QQ ) := Sequence => o -> ( p, t ) ->
(
    if not isPrime p then error "decomposeFraction: first argument must be a prime number";
    a := numerator t; -- finding a is easy, for now
    den := denominator t;
    b := 1;
    while den % p^b == 0 do b = b+1;
    b = b-1;
    temp := denominator( t*p^b );
    local c;
    if temp == 1 then c = 0 else
    (
        c = multiplicativeOrder( p, temp );
        a = lift( a*(p^c-1)/temp, ZZ ) -- fix a
    );
    if o.NoZeroC and c == 0 then
    (
        a = a*(p-1);
        c = 1
    );
    (a,b,c)
)

decomposeFraction( ZZ, ZZ ) := List => o -> (p, t) -> decomposeFraction(p, t/1, o)

--===================================================================================

--*************************************************
--Base p-expansions
--*************************************************

--===================================================================================

adicDigit = method()

--Gives the e-th digit of the non-terminating base p expansion of x in [0,1].
adicDigit ( ZZ, ZZ, QQ ) := ZZ => ( p, e, x ) ->
(
    if p <= 1 then error "adicDigit: Expected first argument to be greater than 1";
    if e <= 0 then error "adicDigit: Expected second argument to be positive";
    if x < 0 or x > 1 then error "adicDigit: Expected last argument to be in the interval [0,1]";
    if x == 0 then return 0;
    lift( ( adicTruncation( p, e, x ) - adicTruncation( p, e-1, x ) ) * p^e, ZZ )
)

adicDigit ( ZZ, ZZ, ZZ ) := ZZ => ( p, e, x ) -> adicDigit( p, e, x/1 )

--Creates list containing e-th digits of non-terminating base p expansion of list of numbers.
adicDigit ( ZZ, ZZ, List ) := ZZ => ( p, e, u ) -> apply( u, x -> adicDigit( p, e, x ) )

--===================================================================================

adicExpansion = method()

--Computes the terminating base p expansion of a positive integer.
--Gives expansion in reverse... so from left to right it gives
--the coefficient of 1, then of p, then of p^2, and so on

adicExpansion( ZZ, ZZ ) := List => ( p, N ) ->
(
    if p <= 1 then error "adicExpansion: Expected first argument to be greater than 1";
    if N < 0 then error "adicExpansion: Expected second argument to be nonnegative";
    if N < p then { N } else prepend( N % p, adicExpansion( p, N // p ) )
    -- would this be faster if it were tail-recursive? we could do this w/ a helper function.
)

--Special case for adic expansion of integers 0 or 1
adicExpansion( ZZ, ZZ, ZZ ) := List => ( p, e, x ) -> adicExpansion( p, e, x/1 )

--Creates a list of the first e digits of the non-terminating base p expansion of x in [0,1].
adicExpansion( ZZ, ZZ, QQ ) := List => ( p, e, x ) ->
(
    if p <= 1 then error "adicExpansion: Expected first argument to be greater than 1";
    if x < 0 or x > 1 then error "adicExpansion: Expected the last argument to be in the interval [0,1]";
    apply( e, i -> adicDigit( p, i+1, x ) )
)

--===================================================================================

adicTruncation = method()

--Gives the e-th truncation of the non-terminating base p expansion of a rational
-- number, unless that number is zero.

adicTruncation ( ZZ, ZZ, QQ ) := QQ => ( p, e, x ) ->
(
    if p <= 1 then error "adicTruncation: Expected first argument to be greater than 1";
    if e < 0 then error "adicTruncation: Expected second argument to be nonnegative";
    if x < 0 then error "adicTruncation: Expected third argument to be nonnegative (or a list of nonegative numbers)";
    if x == 0 then 0 else ( ceiling( p^e*x ) - 1 ) / p^e
)

adicTruncation( ZZ, ZZ, ZZ ) := List => ( p, e, x ) -> adicTruncation( p, e, x/1 )

--truncation threads over lists.
adicTruncation ( ZZ, ZZ, List ) := List => ( p, e, u ) ->
    apply( u, x -> adicTruncation( p, e, x ) )

--===================================================================================

--- write n=a*p^e+a_{e-1} p^{e-1} + \dots + a_0 where 0\leq a_j <p
--- DS: so it's just like doing adicExpansion but giving up after p^e and just returning whatever number's left
--- DS: this could be merged with adicExpansion. Should it be?
--- note: I changed the calling order here should change to be consistent with adicExpansion
--- The change I made was switching the order of the first two arguments
baseP1 = ( p, n, e ) ->
(
    a := n // p^e;
    answer := 1 : a; -- this generates the list (a)
    m := n - a * p^e;
    f := e - 1;
    while f >= 0 do
    (
        d := m // p^f;
        answer = append( answer, d );
        m = m - d * p^f;
        f = f - 1
    );
    answer
)

--===================================================================================

--*************************************************
--Tests for various types of polynomials
--*************************************************

--===================================================================================

--isPolynomial(F) checks if F is a polynomial
isPolynomial = method( TypicalValue => Boolean )

isPolynomial RingElement := Boolean => F -> isPolynomialRing ring F

--===================================================================================

--isPolynomialOverPosCharField(F) checks if F is a polynomial over a field
--of positive characteristic
isPolynomialOverPosCharField = method( TypicalValue => Boolean )

isPolynomialOverPosCharField RingElement := Boolean => F ->
    isPolynomial F and isField( kk := coefficientRing ring F ) and ( char kk ) > 0

--===================================================================================

--isPolynomialOverFiniteField(F) checks if F is a polynomial over a finite field.
isPolynomialOverFiniteField = method( TypicalValue => Boolean )

isPolynomialOverFiniteField RingElement := Boolean => F ->
    isPolynomialOverPosCharField F and (coefficientRing ring F)#?order

--===================================================================================

--isPolynomialOverPrimeField(F) checks if F is a polynomial over ZZ/p.
isPolynomialOverPrimeField = method( TypicalValue => Boolean )

isPolynomialOverPrimeField RingElement := Boolean => F ->
    isPolynomial F and isFinitePrimeField coefficientRing ring F

--===================================================================================

--*************************************************
--Miscelaneous
--*************************************************

--===================================================================================

-- maxIdeal returns the ideal generated by the variables of a polynomial ring
maxIdeal = method( TypicalValue => MonomialIdeal )

maxIdeal PolynomialRing := MonomialIdeal => R -> monomialIdeal R_*

maxIdeal QuotientRing := MonomialIdeal => R -> ideal R_*

--Not used
--maxIdeal RingElement := Ideal => f -> maxIdeal ring f

maxIdeal Ideal := MonomialIdeal => I -> maxIdeal ring I

--===================================================================================

-- passOptions selects a subset of options from an OptionTable
passOptions = method()

passOptions ( OptionTable, List ) := (o, L) -> 
    new OptionTable from apply( L, k -> k => o#k )

--===================================================================================