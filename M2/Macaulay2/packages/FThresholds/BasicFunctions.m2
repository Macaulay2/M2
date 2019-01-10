--*************************************************
--*************************************************
--*************************************************
--*************************************************
--This file is used for doing basic computations 
--i.e. things using only lists, numbers, etc.
-- that support other functions in the Fsing
--package.  
--*************************************************
--*************************************************
--*************************************************
--*************************************************

--*************************************************
--Basic Manipulations with Numbers 
--*************************************************
--===============================================================================

denominator ZZ := x -> 1
numerator ZZ := x -> x

--===============================================================================

--Finds the fractional part of a number.
fracPart = x -> x - floor x

--===============================================================================

--*************************************************
--Information Regarding Factors and Factorization
--*************************************************

--===============================================================================

--Returns a list of factors of a number with repeats.
numberToPrimeFactorList = n ->
(
    prod := factor n;
    flatten apply( toList prod, x -> toList( x#1:x#0 ) )
)

--===============================================================================

--Returns a list of all factors of number.
--Has funny order...
allFactors = n ->
(
     if n < 1 then error "properFactors: expected an integer greater than 1";
     powSet := subsets numberToPrimeFactorList n; 
     toList set apply( powSet, x -> product x )
)

--===============================================================================

--*************************************************
--Finding Numbers in Given Range
--*************************************************

--===============================================================================

--This function finds rational numbers in the range of the interval
--with the given denominator
findNumberBetweenWithDenom = ( d, a, b ) ->
(
     A := ceiling( a*d ); 
     B := floor( b*d ); 
     toList( A..B )/d
)

--This function finds rational numbers in the range of 
--the interval, with a given maximum denominator. 
findNumberBetween = ( maxDenom, a, b ) ->
(
    denominators := toList( 1..maxDenom ); 
    outList := {};
    local d;
    while denominators != {} do 
    (
	d = last denominators;
	factors := allFactors d;
	-- remove all factors of d
	denominators = select( denominators, x -> not member( x, factors) );
	outList = outList | findNumberBetweenWithDenom( d, a, b )
    );
    sort unique outList
)

--for backwards compatibility
--findNumberBetween( ZZ, List ) := ( maxDenom, myInterv )-> findNumberBetween( maxDenom, myInterv#0, myInterv#1);

--===============================================================================

--*************************************************
--Manipulations with Vectors   
--*************************************************

--===============================================================================

 
--===============================================================================

getNumAndDenom = method( TypicalValue => List )

-- Takes a rational vector u and returns a pair (a,q), where a
--is an integer vector and q an integer such that u=a/q.
getNumAndDenom List := List => u -> 
(
    den := lcm apply( u, denominator );
    a := apply( u, n -> lift( n*den, ZZ ) );
    ( a, den )        
)

--===============================================================================

--Selects or finds positions of nonzero, zero, positive entries in a list
selectNonzero = L -> select( L, x -> x != 0 )
selectPositive = L -> select( L, x -> x > 0 )
nonzeroPositions = L -> positions( L, x -> x != 0 )
zeroPositions = L -> positions( L, zero )

--===============================================================================

--*************************************************
--Tests for various types of polynomials   
--*************************************************

--===============================================================================

--isPolynomial(F) checks if F is a polynomial
isPolynomial = method( TypicalValue => Boolean )

isPolynomial RingElement := Boolean => F -> isPolynomialRing( ring F ) 

--===============================================================================

--isPolynomialOverPosCharField(F) checks if F is a polynomial over a field
--of positive characteristic
isPolynomialOverPosCharField = method( TypicalValue => Boolean )

isPolynomialOverPosCharField RingElement := Boolean => F ->
    isPolynomial F and isField( kk := coefficientRing ring F ) and ( char kk ) > 0

--===============================================================================

--isPolynomialOverFiniteField(F) checks if F is a polynomial over a finite field.
isPolynomialOverFiniteField = method( TypicalValue => Boolean )

-- This was reverted so that users with older M2 version could load 

--isPolynomialOverFiniteField (RingElement) := Boolean => F ->
--    isPolynomialOverPosCharField( F ) and isFinitePrimeField(coefficientRing ring F)

isPolynomialOverFiniteField RingElement := Boolean => F ->
    isPolynomialOverPosCharField F and  ( try (coefficientRing ring F)#order then true else false )

--===============================================================================

--isPolynomialRingOverFiniteField(R) checks if R is a polynomial over a 
-- finite field.
isPolynomialRingOverFiniteField = method( TypicalValue => Boolean )

isPolynomialRingOverFiniteField Ring := Boolean => R ->
    isPolynomialRing R and  (try (coefficientRing R)#order then true else false)

--===============================================================================

--Determines whether a polynomial f is a diagonal polynomial (i.e., of the form 
--x_1^(a_1)+...+x_n^(a_n)) over a field of positive characteristic 
isDiagonal = method( TypicalValue => Boolean )

isDiagonal RingElement := Boolean => f ->
    isPolynomialOverPosCharField( f ) and 
    ( product( exponents( f ), v -> #(positions( v, x -> x != 0 )) ) == 1 )

--===============================================================================

--Returns true if the polynomial is a monomial
isMonomial = method( TypicalValue => Boolean )

isMonomial RingElement := Boolean => f -> 
    isPolynomial f and #( terms f ) == 1

--===============================================================================

--Returns true if the polynomial is a binomial over a field of positive characteristic
isBinomial = method( TypicalValue => Boolean )

isBinomial (RingElement) := Boolean => f -> 
    isPolynomialOverPosCharField f and #( terms f ) == 2

--===============================================================================
  
--isBinaryForm(F) checks if F is a homogeneous polynomial in two variables.
--WARNING: what we are really testing is if the *ring* of F is a polynomial ring 
-- in two variables, and not whether F explicitly involves two variables. 
-- (For example, if F=x+y is an element of QQ[x,y,z], this test will return "false"; if G=x is an element of QQ[x,y], this test will return "true".)
isBinaryForm = method( TypicalValue => Boolean )

isBinaryForm RingElement := Boolean => F ->
    isPolynomial F and numgens ring F == 2 and isHomogeneous F 

--===============================================================================

--isNonconstantBinaryForm(F) checks if F is a nonconstant homogeneous polynomial 
-- in two variables. See warning under "isBinaryForm".
isNonConstantBinaryForm = method( TypicalValue => Boolean )

isNonConstantBinaryForm RingElement := Boolean => F -> 
    isBinaryForm F  and ( degree F )#0 > 0

--===============================================================================

--isLinearBinaryForm(F) checks if F is a linearform in two variables. See warning 
--under "isBinaryForm".
isLinearBinaryForm = method( TypicalValue => Boolean )

isLinearBinaryForm RingElement := Boolean => F -> 
    isBinaryForm F and ( degree F )#0 == 1

--===============================================================================

--*************************************************
--Miscelaneous
--*************************************************

--===============================================================================

--Finds the x-intercept of a line passing through two points
xInt = ( x1, y1, x2, y2 ) ->
(
    if x1 == x2 then error "xInt: x1==x2 no intersection";
    x1-(y1/((y1-y2)/(x1-x2)))
)

--===============================================================================

-- maxIdeal returns the ideal generated by the variables of a polynomial ring
maxIdeal = method( TypicalValue => Ideal )

maxIdeal PolynomialRing := Ideal => R -> 
(
    if not isPolynomialRing R then error "maxIdeal: expected a polynomial ring, or an ideal or element of a polynomial ring";
    monomialIdeal R_*
)

maxIdeal RingElement := Ideal => f -> maxIdeal ring f

maxIdeal Ideal := Ideal => I -> maxIdeal ring I

--===============================================================================

-- isProper and isUnitIdeal check if an ideal is proper or the unit ideal 
isUnitIdeal = method( TypicalValue => Boolean )

isUnitIdeal Ideal := Boolean => I ->  isSubset( ideal 1_(ring I), I )

isProper = method( TypicalValue => Boolean )

isProper Ideal := Boolean => I -> not isUnitIdeal I

--===============================================================================

-- Factorization of polynomials and splitting fields --

-- factorsAndMultiplicities(F) factors the RingElement F and returns a list of pairs of 
-- the form {factor,multiplicity}.
factorsAndMultiplicities = method( TypicalValue => List )

factorsAndMultiplicities RingElement := List => F -> 
    apply( toList factor F, toList )

--splittingField returns the splittingField of a polynomial over a finite field
splittingField = method( TypicalValue => GaloisField )

splittingField RingElement := GaloisField => F -> 
(
    if not isPolynomialOverFiniteField F 
        then error "splittingField expects a polynomial over a finite field";
    p := char ring F;
    ord := ( coefficientRing ring F )#order;
    factors := first transpose factorsAndMultiplicities F;
    deg := lcm selectPositive( flatten apply( factors, degree ) );
    GF( p, deg * floorLog( p, ord ) )
)

--===============================================================================

-- checkOptions checks whether the option values passed to a function are valid.
-- The arguments are:
-- 1. An option table.
-- 2. A list of the form { Option => check, ... } where check is an expected 
--    Type or a list of valid values or a function that must return true for 
--    valid values of Option.
-- If an option value is not appropriate, a user-friendly error message 
-- is returned. 

checkOptions = method()

checkOptions ( OptionTable, List ) := (o, L) -> 
(
    opts := new HashTable from L;
    scanKeys( opts, k -> 
	(
	    if instance( opts#k, VisibleList ) and not member( o#k, opts#k ) then
	        (
	            error ( "checkOptions: value for option " | toString( k ) | " must be an element of " | toString( opts#k ) )
		);   
	    if instance( opts#k, Type ) and not instance( o#k, opts#k ) then
	        (
		    error ( "checkOptions: value for option " | toString( k ) | " must be of class " | toString( opts#k ) )
		);
	    if instance( opts#k, Function ) and not opts#k o#k  then
	        (
		    error ( "checkOptions: value for option " | toString( k ) | " is not valid" )
		)
	) 
    )
)


