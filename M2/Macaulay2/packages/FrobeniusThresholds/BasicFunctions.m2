--*******************************************************************************
--*******************************************************************************
-- This file is used for doing basic computations i.e. things using only lists, 
-- numbers, etc. that support other functions in the FThresholds  package.
--*******************************************************************************
--*******************************************************************************

--*******************************************************************************
-- Manipulations with Lists
--*******************************************************************************

--===============================================================================

getNumAndDenom = method( TypicalValue => List )

-- Takes a rational vector u and returns a pair (a,q), where a
-- is an integer vector and q an integer such that u=a/q.
getNumAndDenom List := List => u ->
(
    den := lcm apply( u, denominator );
    a := apply( u, n -> lift( n*den, ZZ ) );
    ( a, den )
)

--===============================================================================

-- Selects or finds positions of nonzero, zero, positive entries in a list
selectNonzero = L -> select( L, x -> x != 0 )
selectPositive = L -> select( L, x -> x > 0 )
nonzeroPositions = L -> positions( L, x -> x != 0 )
zeroPositions = L -> positions( L, zero )
positivePositions = L -> positions( L, x -> x > 0 )

--===============================================================================

--*******************************************************************************
-- Tests for various types of polynomials and other objects
--*******************************************************************************

--===============================================================================

-- isPolynomial(F) checks if F is a polynomial
isPolynomial = method( TypicalValue => Boolean )

isPolynomial RingElement := Boolean => F -> isPolynomialRing ring F

--===============================================================================

-- isPolynomialOverPosCharField(F) checks if F is a polynomial over a field
-- of positive characteristic
isPolynomialOverPosCharField = method( TypicalValue => Boolean )

isPolynomialOverPosCharField RingElement := Boolean => F ->
    isPolynomial F and isField( kk := coefficientRing ring F ) and char kk > 0

--===============================================================================

-- isDefinedOverFiniteField checks if somethethig is a polynomial ring over a
-- finite field, or an element or ideal in such ring
isDefinedOverFiniteField = method( TypicalValue => Boolean )

isDefinedOverFiniteField Ring := Boolean => R ->
    isPolynomialRing R and (try (coefficientRing R)#order then true else false)

isDefinedOverFiniteField Ideal := Boolean => I ->
    isDefinedOverFiniteField ring I

isDefinedOverFiniteField RingElement := Boolean => f ->
    isDefinedOverFiniteField ring f

--===============================================================================

-- Determines whether a polynomial f is a diagonal polynomial (i.e., of the form
-- x_1^(a_1)+...+x_n^(a_n)) over a field of positive characteristic
isDiagonal = method( TypicalValue => Boolean )

isDiagonal RingElement := Boolean => f ->
    isPolynomialOverPosCharField f  and
        product( exponents f, v -> #(positions( v, x -> x != 0 )) ) == 1

--===============================================================================

-- Returns true if the polynomial is a monomial
isMonomial = method( TypicalValue => Boolean )

isMonomial RingElement := Boolean => f -> isPolynomial f and #( terms f ) == 1

--===============================================================================

-- Returns true if the polynomial is a binomial over a field of positive 
-- characteristic
isBinomial = method( TypicalValue => Boolean )

isBinomial RingElement := Boolean => f ->
    isPolynomialOverPosCharField f and #( terms f ) == 2

--===============================================================================

-- isBinaryForm(F) checks if F is a (standard) homogeneous polynomial in two 
-- variables.
-- WARNING: what we are really testing is if the *ring* of F is a polynomial ring
-- in two variables, and not whether F explicitly involves two variables. (For 
-- example, if F = x+y is an element of QQ[x,y,z], this test will return "false";
-- if G = x is an element of QQ[x,y], this test will return "true".)
isBinaryForm = method( TypicalValue => Boolean )

isBinaryForm RingElement := Boolean => F ->
    isPolynomial F and 
        numgens ring F == 2 and 
        same apply( exponents F, i -> sum i )
-- isHomogeneous is avoided above to account for non-standard gradings

--===============================================================================

-- isLinearBinaryForm(F) checks if F is a linearform in two variables. See 
-- warning under "isBinaryForm".
isLinearBinaryForm = method( TypicalValue => Boolean )

isLinearBinaryForm RingElement := Boolean => F ->
    isBinaryForm F and first apply( exponents F, i -> sum i) == 1

--===============================================================================

--*******************************************************************************
-- Handling of options
--*******************************************************************************

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

checkOptions ( OptionTable, List ) := ( o, L ) ->
(
    opts := new HashTable from L;
    scanKeys( opts, k ->
	(
	    if instance( opts#k, VisibleList ) and not member( o#k, opts#k ) then
                error ( "checkOptions: value for option " | toString k | " must be an element of " | toString opts#k );
	    if instance( opts#k, Type ) and not instance( o#k, opts#k ) then
		error ( "checkOptions: value for option " | toString k | " must be of class " | toString opts#k );
	    if instance( opts#k, Function ) and not opts#k o#k  then
		error ( "checkOptions: value for option " | toString k | " is not valid" )
	)
    )
)

--===============================================================================

-- passOptions selects a subset of options from an OptionTable
passOptions = method()

passOptions ( OptionTable, List ) := ( o, L ) ->
    new OptionTable from apply( L, k -> k => o#k )

--===============================================================================

--*******************************************************************************
-- Creating and testing ideals
--*******************************************************************************

--===============================================================================

-- maxIdeal returns the ideal generated by the variables of the ambient ring

maxIdeal = method( TypicalValue => MonomialIdeal )

maxIdeal PolynomialRing := MonomialIdeal => R ->
(
    if not isPolynomialRing R then 
        error "maxIdeal: expected a polynomial ring, or an ideal or element of a polynomial ring";
    monomialIdeal R_*
)

maxIdeal RingElement := MonomialIdeal => f -> maxIdeal ring f

maxIdeal Ideal := MonomialIdeal => I -> maxIdeal ring I

--===============================================================================

-- isProper and isUnitIdeal check if an ideal is proper or the unit ideal

isUnitIdeal = method( TypicalValue => Boolean )

isUnitIdeal Ideal := Boolean => I ->  dim I == -1

isProper = method( TypicalValue => Boolean )

isProper Ideal := Boolean => I -> not isUnitIdeal I

--===============================================================================

--*******************************************************************************
-- Finding rational numbers in an interval
--*******************************************************************************

--===============================================================================

-- FINDING RATIONAL NUMBERS IN AN INTERVAL

-- This function is meant to estimate the computational cost
-- of comparing a rational number t with the fpt of a polynomial.
-- Right now, the standard way to compute this cost is as a
-- linear function of the numbers (a, b, c) returned by 
-- decomposeFraction(p, t). In the future, we may try using the 
-- base p expansion of a in this estimate.
 
cost = method()

-- These are the default weights, so the computatinal cost is 
-- assumed to be proportional to b + 1.5*c.
defaultWeights := { 0, 1, 1.5 }

-- This is for the case where the user passes his/her own weights.
-- The computational cost computed with the user's weights is
-- given priority, by being placed first in the list returned.
cost ( ZZ, QQ, List ) := ( p, t, userWeights ) ->
(
     decomp := decomposeFraction( p, t );
     userCost := sum( decomp, userWeights, ( i, j ) -> i*j );
     defaultCost := sum( decomp, defaultWeights, ( i, j ) -> i*j );
     { userCost, defaultCost }
)

-- This is for the case where the user passes his/her own cost function.
-- The computational cost computed with the user's function is given 
-- priority, by being placed first in the list returned.
cost ( ZZ, QQ, Function ) := ( p, t, userFunction ) ->
(
     ( a, b, c ) := decomposeFraction( p, t );
     userCost := try userFunction t  else 
         try userFunction( p, t ) else userFunction( p, a, b, c );
     defaultCost := sum( { a, b, c }, defaultWeights, ( i, j ) -> i*j );
     { userCost, defaultCost }
)

-- This is for the case where the user does not pass anything.
cost ( ZZ, QQ, Nothing ) := ( p, t, userFunction ) ->
(
     decomp := decomposeFraction( p, t );
     { sum( decomp, defaultWeights, ( i, j ) -> i*j ) }
)

-- In the code below, rational numbers are expressed as {numerator, denominator}, 
-- to avoid repeated calls to the numerator and denominator functions. 
-- The function num converts such a list to an actual number.
num = a -> a#0/a#1 

-- **Farey sums**
-- Suppose a/b and c/d are reduced fractions, and that there is no rational number
-- with denominator <= max(b,d) between them. Then p/q := (a+c)/(b+d) is a reduced 
-- fraction, known as the Farey sum (or mediant) of a/b and c/d. Moreover,  a/b, 
-- p/q, c/d are consecutive elements in the list of rational numbers with 
-- denominator <= b+d.

-- The commands below are meant to create and refine lists of rational numbers in 
-- an interval (A,B). However, to generate such numbers and refine existing 
-- lists, we need to also include one (single) number <=A and another >= B. 

-- The function 'refine' takes one such list, and inserts Farey sums between each 
-- consecutive pair of numbers. If the first Farey sum inserted is <= A, the 
-- first element of the original list is dropped; likewise, if the last Farey 
-- sum inserted is >= B, the last element of the original list is dropped.
-- This allows us to "zero in" on the interval (A,B).
refine = ( A, B, oldList ) -> 
(
    L := oldList;
    L = apply( toList( 0..(#L-2) ), i -> { L#i, L#i + L#(i+1) } );
    L = flatten L;
    if num last L < B then L = append( L, last oldList );
    if num L#1 <= A then L = drop( L, 1 );
    L
)

-- findNumbers takes a list of rational numbers and refines it until there are 
-- minNumber elements in (A,B).
findNumbers = ( A, B, minNumber, oldList ) -> 
(
    L := oldList;
    -- the "+2" below compensates for the fact that the first and last 
    -- numbers on the list are actually not in (A,B).
    while #L < minNumber + 2 do L = refine(A, B, L); 
    L
)

-- fptGuess takes a list of numbers and refines so it includes at least minNumber 
-- elements in (A,B). Returns a sorted list with ranks (costs) and numbers, and 
-- the raw list of rational numbers, expressed as pairs.
-- This relies on the fact that the function 'sort' sorts lists of lists 
-- lexicographically.
fptGuess = ( p, A, B, minNumber, userCriterion, oldList ) ->
(
    if A >= B then 
        error "fptWeightedGuessList: Expected third argument to be greater than second";
    numList := findNumbers( A, B, minNumber, oldList );
    -- distance from the midpoint of the interval will be used as tie-breakers    
    midpt := (B - A)/2; 
    -- remove first and last #s, which are not in (A,B), and compute weights 
    numCostList := apply( drop( drop( numList, 1 ), -1 ), 
        t -> join( cost( p, num t, userCriterion ), { abs( num(t) - midpt ), num t } )
    );
    ( sort numCostList, numList )
)

--===============================================================================

--*******************************************************************************
-- Miscellaneous
--*******************************************************************************

--===============================================================================

-- Finds the x-intercept of a line passing through two points
xInt = ( x1, y1, x2, y2 ) ->
(
    if y1 == y2 then error "xInt: y1 == y2, so there is no intersection";
    x1 - y1 * ( x1 - x2 )/( y1 - y2 )
)

--===============================================================================

-- Factorization of polynomials and splitting fields --

-- factorsAndMultiplicities(F) factors the RingElement F and returns a list of pairs of
-- the form {factor,multiplicity}.
factorsAndMultiplicities = method( TypicalValue => List )

factorsAndMultiplicities RingElement := List => F ->
    apply( toList factor F, toList )

-- splittingField returns the splittingField of a polynomial over a finite field
splittingField = method( TypicalValue => GaloisField )

splittingField RingElement := GaloisField => F ->
(
    if not isDefinedOverFiniteField F
        then error "splittingField: expected a polynomial over a finite field";
    p := char ring F;
    ord := ( coefficientRing ring F )#order;
    factors := first transpose factorsAndMultiplicities F;
    deg := lcm selectPositive flatten apply( factors, degree );
    GF( p, deg * floorLog( p, ord ) )
)

--===============================================================================

