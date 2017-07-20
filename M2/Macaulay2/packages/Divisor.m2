newPackage( "Divisor",
Version => "0.1sa", Date => "May 23rd, 2017", Authors => {
     {Name => "Karl Schwede",
     Email=> "kschwede@gmail.com",
     HomePage=> "http://www.math.utah.edu/~schwede"
     },
     {Name=> "Zhaoning Yang",
     Email=> "zyy5054@gmail.com",
     HomePage => "http://sites.psu.edu/zhaoningyang"
     }
}, --this file is in the public domain
Headline => "A package for working with Weil divisors.", DebuggingMode => true, Reload=>true)
export{
    --objects
	"BasicDiv",
	"WDiv",
	"QDiv",
	"RDiv",
    --methods for defining divisors and related operations
	"divisor",
	"rationalDivisor",
	"realDivisor",
	"zeroDivisor",
	"verifyDivisor",
    --accessing data
	"getPrimeList",
	"getPrimeCount",
	"getGBList",
	"getCoeffList",
	"simplifyDiv",
	"coeff",
	"getAmbientRing",
	"isDivAmbient", 
	"sameDivAmbient", 
	"getPrimeDivisors",
     --simple operations
	"floorDiv",
	"ceilingDiv",
	"divPlus",
	"divMinus",
	"applyToCoefficients",
    --conversion
	"toWDiv",
	"toQDiv",
	"toRDiv",
    --divisors to modules and functorial properties
	"divisorToModule",
	"divisorToIdeal",
	"idealToDivisor",
	"idealWithSectionToDivisor",
	"moduleToDivisor",
	"moduleWithSectionToDivisor",
	"divPullBack",
	"findElementOfDegree", 
	"getLinearDiophantineSolution",		--has Unsafe option
	"canonicalDivisor", --has IsGraded option
	"ramificationDivisor",
    --tests and related constructions
    	"isWDiv",
	"isEffective",
	"isDivPrime",
	"isDivPrincipal", --has IsGraded option
    	"isDivReduced",
    	"isCartier", --has IsGraded option
    	"isLinearEquivalent", --has IsGraded option
    	"isQCartier", --has IsGraded option
    	"isQLinearEquivalent", --has IsGraded option
    	"isDivGraded",
    	"nonCartierLocus", --has IsGraded option
    	"isSNC", --has IsGraded option
    	"isZeroDivisor",
    	"isVeryAmple", --assumes graded
    --functions for getting maps to projective space from divisors (graded only)
	"baseLocus",
	"mapToProjectiveSpace",  
    --general useful functions not directly related to divisors
        "idealPower",
        "reflexifyIdeal",
	"reflexifyModule", 
	"reflexifyModuleWithMap",
	"isReflexive",
	"reflexivePower",
	"torsionSubmodule",
	"dualizeIdeal",
	"moduleToIdeal", --has IsGraded option
	"moduleWithSectionToIdeal",
	"isDomain",
	"isRegular", --has IsGraded option
    --options
    	"Unsafe", --an option, if set true then the above commands avoid doing any checks
	"CoeffType", --an option, one can set the coefficient type
	"AmbRing", --an option, one can specify the ambient ring during divisor construction
    	"MTries", --an option, used to try to embed a module into a ring as an ideal in a random way
	"KnownNormal", --an option, used to specify that the ring is known to be normal
	"KnownCartier", --an option, used to specify that the divisor is known to be Cartier
	"IsGraded", --an option, if you specify it in several arguments it assumes we are working on a projective variety
	"ReturnMap", --an option, for moduleToIdeal and moduleWithSectionToIdeal which returns the map from the module to R^1
	"Primes", --a potential value for the divPullBack Strategy option
	"Sheaves" --a potential value for the divPullBack Strategy option	
}

----------------------------------------------------------------
--************************************************************--
--Structure of our divisor objects and their display------------
--************************************************************--
----------------------------------------------------------------
	
BasicDiv = new Type of HashTable;
RDiv = new Type of BasicDiv;
QDiv = new Type of RDiv;
WDiv = new Type of QDiv;

--Divisors have keys equal to Grobner bases of prime height-1 ideals.  They have values which are pairs.  The first entry in the value is the coefficient, the second is an ideal which hopefully the user will recognize.

--we can also control how they display
--they should display as something like
--**************************************
-- 5*Div(x,y) + -2*Div(y,z) of QQ[x,y,z]/(y^2-x*z)
--*************************************

net BasicDiv := t -> (
	valList := getCoeffList(t);
	primeList := getPrimeList(t);
	myStr := "";
	i := 0;
	j := 0;
	genList := {};
	if (#valList > 0) then (
		while (i < #valList) do(
			if (i > 0) then myStr = myStr | " + ";
			myStr = myStr | toString(valList#i) | "*Div(";
			genList = first entries gens (primeList#i);
			for j from 0 to (#genList-1) do (
				if (j > 0) then myStr = myStr | ", ";
				myStr = myStr | toString(genList#j);
			);
			myStr = myStr | ")";
			i = i+1
		);
	)
	else(
		myStr = "0, the zero divisor"
	);
	myStr = myStr | " of " | toString(getAmbientRing(t));
	myStr
)


----------------------------------------------------------------
--************************************************************--
--Divisor construction -----------------------------------------
--************************************************************--
----------------------------------------------------------------



--the following is an internal function for divisors, it's basically the collision function
pairPlus = (l1, l2) ->( --there can be two kinds of inputs, ordinary divisor pairs
	output := null;
	if ((instance(l1, BasicList) == true) and (instance(l2, BasicList) == true)) then (
		output = {l1#0 + l2#0, l1#1}
	)
	else if ((instance(l1, Ring) == true) and (instance(l2, Ring) == true)) then (
		output = l1
	);
	
	output
)


--the following is the basic construction function for the divisor
--it is passed a list of coefficients and a list of prime height one ideals
--several options are available, perhaps the most important is Unsafe, if set to true then the function doesn't check whether the lists 
--are made of the right stuff (height 1 ideals, etc.)

divisor = method(Options => {Unsafe => false, CoeffType => ZZ, AmbRing => null});  

divisor(BasicList, BasicList) := o ->(l1, l2) -> 
(
	divList := new List from {}; 		--the Weil Divisor
	coeffList := l1; 					--list of coefficient
	idealList := l2; 				--list of height one prime ideals	
	flag := true;	
	N := #coeffList;
	RTest := o.AmbRing;
	
	--initial specification of the ambient ring
	--these checks are performed regardless of the Unsafe option
	if (N > 0) then ( --if there are ideals to compare
		RTest = ring ( idealList#0 );
		if (( not (o.AmbRing === null)) and (not (RTest === o.AmbRing) ) ) then (
			error "divisor: Specified ambient ring does not match the ideals given."; 
			flag = false;
		);
	)
	else ( --otherwise use the users ambient ring
		RTest = o.AmbRing; 
		if (RTest === null) then RTest = ZZ; --or specify ZZ if the user didn't use one
	);
	
	--now we do the checks
	if (o.Unsafe == false) then (
		M := #idealList;
		if (N != M) then
		(
			error "divisor: lists should have the same length";
			flag = false
		);
		
		--check that all the ideals are really ideals
		if (flag == true) then ( flag = all(idealList, z->instance(z, Ideal)); );
		if (flag == false) then ( error "divisor: all ideals should actually be ideals"; );
				
		--now that RTest is set and we know we have ideal, check that all the ideals have the same ring
		if (flag == true) then ( flag = all(idealList, z->(RTest === ring z)); );
		if (flag == false) then ( error "divisor: all ideals should lie in the same ambient ring"; );
		
		--next check that the coefficients are actually coefficients
		if (flag == true) then ( flag = all(coeffList, z->instance(z, o.CoeffType)); );
		if (flag == false) then ( error ("divisor: coefficients should all be in "| toString(o.CoeffType) | " you can use CoeffType=>... to specify a different set of coefficients (ie, RR or QQ)" ); );
		
		--next check that every ideal is prime
		if (flag == true) then ( flag = all(idealList, z->isPrime(z)); );
		if (flag == false) then ( error "divisor: all ideals should be prime"; );
		
		--next check that every ideal has the right height (note if you pass a non-normal ambient ring, this won't work)
		d1 := dim RTest;
		if (flag == true) then ( flag = all(idealList, z->(d1 - dim(z) == 1) ); );
		if (flag == false) then ( error "divisor: all ideals should have height 1"; );
	);
	--if we had no errors, then 
	--I realize that since we are calling error, we'll never get here if there are errors, but earlier versions didn't actually
	-- call error (and I suppose we might change it back)...
	if (flag == true) then
	(
		divList = toList(apply(0..(N-1), i -> ( (first entries gens (gb idealList#i) ) => {coeffList#i, idealList#i} ) ));
		divList = append(divList, "ambRing" => RTest);
		--if we have a common coefficient ring type
		if (o.CoeffType === ZZ) then 
			new WDiv from ( hashTable(pairPlus, divList) ) 
		else if (o.CoeffType === QQ) then
			new QDiv from ( hashTable(pairPlus, divList) )  
		else if (o.CoeffType === RR) then
			new RDiv from ( hashTable(pairPlus, divList) )  
		else 
			new BasicDiv from ( hashTable(pairPlus, divList) ) 

	)
);

--the user may also pass in a single list consisting of pairs {n, P} where n is a coefficient and P is a height 1 prime ideal
divisor(BasicList) := o ->(myList) -> (
	myList2 := transpose myList;
	divisor(myList2#0, myList2#1, Unsafe=>o.Unsafe, CoeffType=>o.CoeffType, AmbRing=>o.AmbRing)
);

--gives an effective divisor corresponding to the ideal, i.e. V(I)

divisor(Ideal) := o ->(I1) -> ( -idealToDivisor(I1) );

--gives an effective divisor corresponding to a ring element

divisor(RingElement) := o ->(f1) -> ( 
	if (instance(ring f1, FractionField) == true) then (
		-idealToDivisor(ideal(numerator(f1))) + idealToDivisor(ideal(denominator(f1))))
	else (
		-idealToDivisor(ideal(f1)))
);

--Given user input we construct a divisor which consists of two BasicList one for the irreducible codimensional one closed subspaces
-- (i.e. prime ideal of height one). Another for the set of rational coefficients we attached to each of them.

rationalDivisor = method(Options => {Unsafe => false, AmbRing=>null});

rationalDivisor(BasicList, BasicList) := o ->(l1, l2) -> 
(
	divisor(l1, l2, CoeffType=>QQ, Unsafe=>o.Unsafe, AmbRing=>o.AmbRing)
);

--the user may also pass in a single list consisting of pairs {n, P} where n is a coefficient and P is a height 1 prime ideal

rationalDivisor(BasicList) := o ->(myList) -> (
	myList2 := transpose myList;
	rationalDivisor(myList2#0, myList2#1, Unsafe=>o.Unsafe, AmbRing=>o.AmbRing)
);

--Given user input we construct a divisor which consists of two BasicList one for the irreducible codimensional one closed subspaces
-- (i.e. prime ideal of height one). Another for the set of rational coefficients we attached to each of them.

realDivisor = method(Options => {Unsafe => false, AmbRing=>null});

realDivisor(BasicList, BasicList) := o ->(l1, l2) -> 
(
	divisor(l1, l2, CoeffType=>RR, Unsafe => o.Unsafe, AmbRing=>o.AmbRing)
);

--the user may also pass in a single list consisting of pairs {n, P} where n is a coefficient and P is a height 1 prime ideal

realDivisor(BasicList) := o ->(myList) -> (
	myList2 := transpose myList;
	realDivisor(myList2#0, myList2#1, Unsafe=>o.Unsafe, AmbRing=>o.AmbRing)
);

zeroDivisor = method();

zeroDivisor(Ring) := (R1) -> (
	divisor(sub(1, R1))
);

--the following function is used to verify that a divisor is valid, it checks to make sure the coefficients are the right type, that the ideals are prime and height 1, etc.

verifyDivisor = method(Options => {Verbose=>true});

verifyDivisor(BasicDiv) := o->(D1) -> (
	myType := class D1;
	myList := getCoeffList(D1);
	flag := true;
	--first we check the coefficients
	if (myType === WDiv) then (
		flag = all(myList, z->instance(z, ZZ));
		if ((o.Verbose == true) and (flag == false)) then (print "verifyDivisor: Not all coefficients are integers");
	)
	else if (myType === QDiv) then (
		flag = all(myList, z->instance(z, QQ));
		if ((o.Verbose == true) and (flag == false)) then (print "verifyDivisor: Not all coefficients are rational numbers");
	)
	else if (myType === RDiv) then (
		flag = all(myList, z->instance(z, RR));
		if ((o.Verbose == true) and (flag == false)) then (print "verifyDivisor: Not all coefficients are real numbers");
	);
	
	--now check to see if the ideals are prime and height one and in the same ambient ring
	if (flag == true) then(
		myList = getPrimeList(D1);
		myAmb := getAmbientRing(D1);
		--first we check the ambient ring
		flag = all(myList, z -> (myAmb === ring z));
		if ((o.Verbose == true) and (flag == false)) then (print "verifyDivisor: Not all ideals have the same ambient ring");
		--next we check primality
		if (flag == true) then ( 
			flag = all(myList, z->(isPrime(z)));
			if ((o.Verbose == true) and (flag == false)) then (print "verifyDivisor: Not all ideals are prime");
		);
		
		--finally we check dimension
		if (flag == true) then (
			d1 := dim myAmb;
			flag = all(myList, z->(d1 - dim(z) == 1));
			if ((o.Verbose == true) and (flag == false)) then (print "verifyDivisor: Not all ideals are height one");
		);
	);
	
	flag
);

----------------------------------------------------------------
--************************************************************--
--Accessing divisor data----------------------------------------
--************************************************************--
----------------------------------------------------------------

--Get the list of height one prime ideals of a divisor.

getPrimeList = method();

getPrimeList( BasicDiv ) := ( D ) -> 
(
	--we don't want the ambient ring in our prime list do we
	D1 := select(D, z -> instance(z, BasicList));
	valList := values D1;
	if (#valList > 0) then (
		valList2 := transpose valList;
		valList2#1
	)
	else {}
);

--get the number of primes

getPrimeCount = method();

getPrimeCount( BasicDiv ) := ( D ) -> 
(
	#(keys D) - 1
);

--we can also get the list of Grobner bases

getGBList = method();

getGBList(BasicDiv) := (D) -> ( 
	D1 := select(D, z -> instance(z, BasicList));
	keys D1 
);

--Get the list of coefficients of a divisor

getCoeffList = method();

getCoeffList( BasicDiv ) := ( D ) -> ( 	
	D1 := select(D, z -> instance(z, BasicList));
	valList := values D1;
	if (#valList > 0) then (
		valList2 := transpose valList;
		valList2#0
	)
	else {}
);

--Given a divisor D and a irreducible codimensional one subspace C
--we would like to know what is the coefficient of this component inside this divisor
--it does NOT check whether or not P is prime

coeff = method();

coeff(Ideal, BasicDiv) := (P, D) ->
(
	n := 0;
	if (instance(D, QDiv)) then n = 0/1;
	if (instance(D, RDiv)) then n = 0.0;
	flag := D #? (first entries gens (gb P) );
	if( flag == true ) then ( n = (D # (first entries gens (gb P) ))#0 );
	n
);

--in this version, the second argument is a list of generators of the appropriate Groebner basis
coeff(BasicList, BasicDiv) := (l1, D) ->
(
	n := 0;
	if (instance(D, QDiv)) then n = 0/1;
	if (instance(D, RDiv)) then n = 0.0;
	flag := D #? l1;
	if( flag == true ) then ( n = (D # l1)#0 );
	n
);



--Given a divisor D, we want to know what is the ambient ring.

getAmbientRing = method();

getAmbientRing( BasicDiv ) := (D) ->
(	
	D#"ambRing"
);

--The next function gets a list of prime divisors of a given divisor
--warning, it accesses the underlying structure of the HashTable, 

getPrimeDivisors = method();

getPrimeDivisors( BasicDiv ) := (D) ->
(
	gbList := getGBList(D);
	ambRing := getAmbientRing(D);

	myList := apply(gbList, z -> {z => {1, (D#z)#1}, "ambRing"=>ambRing});

	apply(myList, z -> new WDiv from z)
);


----------------------------------------------------------------
--************************************************************--
--Other basic common methods for divisors.----------------------
--************************************************************--
----------------------------------------------------------------

--simplifyDiv simply removes prime divisors with coefficient 0 (it also keeps the flag specifying the ambient ring of course)

simplifyDiv = method();

simplifyDiv( BasicDiv ) := (D)  -> ( select(D, x -> ( if (instance(x, Ring)) then true else (x#0 != 0) )) );	

--applyFunctionToDivisorCoefficients applies the function to the coefficients of the divisor

applyToCoefficients = method(Options => {CoeffType => null, Unsafe => true});

applyToCoefficients( BasicDiv, Function) := o -> (D, hh) -> (
	myClass := class D;
	if (o.CoeffType === ZZ) then (myClass = WDiv)
	else if (o.CoeffType === QQ) then (myClass = QDiv)
	else if (o.CoeffType === RR) then (myClass = RDiv);

	tempDiv := simplifyDiv(new myClass from applyValues(D, x -> (if (instance(x, Ring)) then x else {hh(x#0), x#1} )) );
	if (o.Unsafe == false) then (
		if (not (verifyDivisor(tempDiv))) then (error "applyToCoefficients: the ouput of this function is not a valid divisor, did you set the CoeffType option properly?";);
	);
	tempDiv	
);

--Given a rational/real divisor, we return a Weil Divisor, such that new coefficients are obtained 
--from taking ceilings from the given one

ceilingDiv = method();

ceilingDiv( RDiv ) := ( D ) -> ( applyToCoefficients(D, ceiling, CoeffType=>ZZ) );
--new WDiv from applyValues(D, x -> (if (instance(x, Ring)) then x else {ceiling (x#0), x#1} )) );

--Given a rational/real divisor, we return a Weil divisor for which new coefficients are obtained
--from taking floors from the given one

floorDiv = method();

floorDiv( RDiv ) := ( D ) -> ( applyToCoefficients(D, floor, CoeffType=>ZZ) );

--Given a divisor D, we want to return positive/negative part of D

divPlus = method();

divPlus( RDiv ) := (D) -> ( select(D, x -> (if (instance(x, Ring)) then true else (x#0) > 0) ) );	

divMinus = method();

divMinus( RDiv ) := (D) ->
(
	E := select(D, x -> (if (instance(x, Ring)) then true else ((x#0) < 0)) );
	applyValues(E, x -> (if (instance(x, Ring)) then x else {(-1) * (x#0), x#1}))
);

--checks whether the ambient ring of a divisor is equal to something
isDivAmbient = method();

isDivAmbient( BasicDiv, Ring ) := (D1, R1) ->
(
	R2 := getAmbientRing(D1);
	R1 === R2
);

--checks whether the ambient ring of two divisors agree
sameDivAmbient = method();

sameDivAmbient( BasicDiv, BasicDiv ) := (D1, D2) -> 
(
	getAmbientRing(D1) === getAmbientRing(D2)
);

----------------------------------------------------------------
--************************************************************--
--Conversion and Comparing method among divisors.---------------
--************************************************************--
----------------------------------------------------------------


--Comparing Divisors


--Given two real Divisors, we want to test whether or not they are equal.

RDiv == RDiv := (D, E) ->
(
	target := simplifyDiv( D - E );
	#target == 1
);

--Conversion among divisors
--Given a Weil Divisor, it is naturally a rational divisor.

toQDiv = method();

toQDiv( WDiv ) := (D) -> 
(
	E := simplifyDiv( D );
	coeffList := apply(getCoeffList E, x -> (1/1) * x );
	rationalDivisor(coeffList, getPrimeList E, Unsafe=>true, AmbRing=>getAmbientRing(D) )
);

toQDiv( QDiv ) := (D) -> ( D ); --do nothing to an honest Q-divisor

--Given a Weil Divisor, it is naturally a real divisor.

toRDiv = method();

toRDiv( WDiv ) := (D) -> 
(
	E := simplifyDiv( D );
	coeffList := apply(getCoeffList E, x -> (1.0) * x );
	realDivisor(coeffList, getPrimeList E, Unsafe=>true, AmbRing=>getAmbientRing(D))
);


--Given a rational Divisor, it is naturally a real divisor.

toRDiv( QDiv ) := (D) -> 
(
	E := simplifyDiv( D );
	coeffList := apply(getCoeffList E, x -> (1.0) * x );
	realDivisor(coeffList, getPrimeList E, Unsafe=>true, AmbRing=>getAmbientRing(D))
);

toRDiv( RDiv ) := (D) -> ( D ); --do nothing to an honest R-divisor

--Given a rational/real divisor, if all its coefficients are of the integer type, we want to turn it to a Weil Divisor.

toWDiv = method();

toWDiv( RDiv ) := ( D ) ->
(
	coeffList := new List from {};
	if ( isWDiv( D ) != true ) then
	(
		error "toWDiv: this is not a Weil divisor"
	)
	else
	(
		coeffList = apply(getCoeffList D, x -> floor x);
		divisor(coeffList, getPrimeList D, Unsafe=>true, AmbRing=>getAmbientRing(D))
	)	
);

----------------------------------------------------------------
--************************************************************--
--Group operations on divisors.---------------------------------
--************************************************************--
----------------------------------------------------------------

--Multiplication of a divisor by scalar (integer, rational, real numbers).

ZZ * BasicDiv :=  (n, D) ->
(
	simplifyDiv( applyValues(D, x -> if (instance(x, Ring)) then x else {n * (x#0), x#1}) )
);

QQ * WDiv := (r, D) ->
(
	E := toQDiv( D );
	r * E
);


QQ * RDiv :=  (r, D) ->
(
	simplifyDiv( applyValues(D, x -> if (instance(x, Ring)) then x else {r * (x#0), x#1}) )
);

RR * QDiv := (x, D) ->
(
	E := toRDiv( D );
	x * E
);


RR * RDiv := (y, D) -> 
( 
	simplifyDiv( applyValues(D, x -> if (instance(x, Ring)) then x else {y * (x#0), x#1}) ) 
);

--Addition

BasicDiv + BasicDiv := (D, E) -> 
(	
	flag := sameDivAmbient(D, E);
	myType := Nothing;
		
	if ( flag == false ) then 
	(
		error "(BasicDiv + BasicDiv): the two divisors should have the same ambient ring"
	)
	else
	(
		myType = BasicDiv;
		if (instance(D, WDiv)== true) then myType = class E
		else if (instance(E, WDiv)== true) then myType = class D
		else if (instance(D, QDiv)== true) then myType = class E
		else if (instance(E, QDiv)== true) then myType = class D
		else if (instance(D, RDiv)== true) then myType = class E
		else if (instance(E, RDiv)== true) then myType = class D;

		simplifyDiv( new myType from merge(D, E, pairPlus) )
	)
);	



--Subtraction between two divisors

BasicDiv - BasicDiv := (D, E) -> (D + (-E) );

--Negative of a divisor

-BasicDiv := (D) -> ( (-1) * D );


----------------------------------------------------------------
--************************************************************--
--Functorial Properties and divisor module identifications------
--************************************************************--
----------------------------------------------------------------

--Given a divisor D, we want to construct its corresponding sheaf O(D).

divisorToModule = method ();

divisorToModule( WDiv ) := (D) ->
(
	R := getAmbientRing( D );
	E := divPlus( D );
	F := divMinus( D );
	E1 := apply(getPrimeCount(E), i -> (  idealPower( (getCoeffList E)#i,  (getPrimeList E)#i) )  ); --idealPower here yields a massive speedup
	F1 := apply(getPrimeCount(F), i -> (  idealPower( (getCoeffList F)#i,  (getPrimeList F)#i) )  ); 
	prodE := ideal(sub(1, R));
	prodF := ideal(sub(1, R));
	if ( #(E1) != 0 ) then --these two computations might be reasonable to eventually put in separate threads
	(
		prodE = product( E1 )
	);
	if ( #(F1) != 0 ) then
	(
		prodF = product( F1 )
	);
--	prodE = reflexifyIdeal(prodE); --these lines seem to speed things up, sometimes... (and more often they slow things down)
--	prodF = reflexifyIdeal(prodF); --but when they speed things up it seems like a huge benefit, and when it slows things down it's only ~3 times slower
	                               --maybe eventually this could be something that is done with multiple threads... (I'm leaving them commented out for now)
	dual := (prodE*R^1) ** ( Hom(prodF*R^1, R^1) );
	Hom(dual, R^1)
);   


divisorToModule( QDiv ) := (D) ->
(
	divisorToModule( floorDiv(D) )
);  

divisorToModule( RDiv ) := (D) ->
(
	divisorToModule( floorDiv(D) )
);

divisorToIdeal = method();

divisorToIdeal(WDiv) := (D) -> (
	R := getAmbientRing( D );
	E := divPlus( D );
	F := divMinus( D );
	E1 := apply(getPrimeCount(E), i -> (  idealPower( (getCoeffList E)#i,  (getPrimeList E)#i) )  ); --this seems to result in a huge speedup
	F1 := apply(getPrimeCount(F), i -> (  idealPower( (getCoeffList F)#i,  (getPrimeList F)#i) )  );
	prodE := ideal sub(1, R);
	prodF := ideal sub(1, R);
	if ( #(E1) != 0 ) then
	(
		prodE = product( E1 )
	);
	if ( #(F1) != 0 ) then
	(
		prodF = product( F1 )
	);
	if (#(E1) != 0) then (
		dual := (prodE) * ( dualizeIdeal(prodF) );  
		dualizeIdeal(dual)
	)
	else (
		reflexifyIdeal(prodF)
	)
);

divisorToIdeal( QDiv ) := (D) ->
(
	divisorToIdeal( floorDiv(D) )
);  

divisorToIdeal( RDiv ) := (D) ->
(
	divisorToIdeal( floorDiv(D) )
);

--Given an ideal which is equal to its double dual, we want to find the corresponding divisor.  
--If the ideal is not reflexive, we reflexify it first.  

idealToDivisor = method();

idealToDivisor( Ideal ) := (I1) ->
(
	if  (I1 == 0*I1) then (error "idealToDivisor: cannot form divisor from the zero ideal";);
	--question, is it faster to reflexify the ideal or just check whether the ideal has too high of dimension... if multiprocess gets working, then we may want to check both...
	I2 := reflexifyIdeal(I1);
	L2 := {};
	if ( isSubset(ideal sub(1, ring I2), I2) == false ) then (L2 = minimalPrimes(I2););
	L0 := {}; --list of coefficients/integers
	top1 := 1;
	bottom1 := 0;
	i1 := 0;
	tmp := 0;
	flag := false;

	for i1 when (i1 < #L2) do (
		flag = true;
		top1 = 1;
		--we will do a binary search, but first we need to find out how high to go (exponential search, it should be fast)
		while (flag == true) do (
			top1 = top1*2;
			flag = isSubset(I2, reflexifyIdeal(idealPower(top1, (L2#i1)))); --ideal power gives us a huge speedup again
		);
		--now we do the binary search
		flag = true;
		bottom1 = 0;
		while (top1 > bottom1+1) do(
			tmp = floor((top1 + bottom1)/2);
			flag = isSubset(I2, reflexifyIdeal(idealPower(tmp, (L2#i1))));
			if (flag == false) then top1 = tmp else bottom1 = tmp;
		);
		L0 = join(L0, {-bottom1}); 
	);
	divisor(L0, L2, AmbRing=>(ring I1))
);

idealWithSectionToDivisor = method();

idealWithSectionToDivisor(RingElement, Ideal) := (f1, I1) -> (
	if (not (ring f1 === ring I1)) then ( error "idealWithSectionToDivisor: The ring element and the ideal do not live in the same ring.";);
	D1 := idealToDivisor(I1);
	D2 := divisor(f1);
	D2 + D1
);

moduleToDivisor = method(Options => {IsGraded => false});

moduleToDivisor( Ring, Module ) := o -> (R, M) ->
(
	I := 0;
	
	if (o.IsGraded == false) then ( 
		I = moduleToIdeal(R, M);	
		idealToDivisor( I )
	)
	else(
		L1 := moduleToIdeal(R, M, IsGraded => true);
		I = L1#0;
		l := (-1)*L1#1;
		--print l;
		D1 := idealToDivisor(I);
		--M2 := divisorToModule(D1);
		--M3 := prune (Hom(M2, M));
		--next we find an element of degree l
		els := findElementOfDegree(l, R);
		D1 - divisor(els#0) + divisor(els#1)
	) 
);	

moduleToDivisor(Module) := o-> (M) ->
(
	moduleToDivisor(ring M, M, IsGraded=>o.IsGraded)
);

moduleWithSectionToDivisor = method();

moduleWithSectionToDivisor(Matrix) := (f1) -> (
	L := moduleWithSectionToIdeal(f1);
	idealWithSectionToDivisor(L#0, L#1)
);

--Give a ring map f: R -> S for which we assume is finite or flat, we want to construct its pullback from Div X to Div Y
--where Div X = Spec S and Div Y = Spec R.  If the map is neither finite or flat, then this method can produce unexpected results unless the divisor is Cartier (which the function checks for).  

divPullBack = method(Options => {Strategy => Primes});

divPullBack(RingMap, RDiv) := o->(f, D) ->
(		
	if (isDivAmbient(D, source f) == false) then error "divPullBack: Expected the Divisor and the source of the map to have the same ambient ring.";
	
	if (o.Strategy === Primes) then (--we pull back individual prime ideals
		E := divisor({}, {}, AmbRing => (target f));
		L := getCoeffList D;
		PL := getPrimeList D;
		for i when (i < #L ) do
		(
			E = E - L#i * idealToDivisor( f( PL#i ) )		
		);
		E
	)
	else if (o.Strategy === Sheaves) then ( --we pullback a sheaf
		if (isWDiv(D) == false) then ( error "divPulllBack:  If you use the sheaf strategy, you must pullback a WDiv"; );
		toWDiv(D);
		DM := divMinus(D);
		IM := divisorToIdeal(-DM);
		g1 := 0;
		i := 0;
		genList := first entries gens IM;
		myFlag := false;
		while (myFlag == false) do (
			g1 = genList#i;
			i = i+1;
			myFlag = (f(g1) != 0);
		);
		if (myFlag == false) then (error "divPullBack: this divisor cannot pull back, it has a component which vanishes on the image of the map (this error will only occur for terms with negative coefficients)";);
		--the point of all that is that D + divisor(g1) is effective
		J := divisorToIdeal(-D - divisor(g1));
		J = f(J);
		divisor(J) - divisor(f(g1))
	)
);

--the following method returns an element of a given degree, it returns two elements {a,b} for the numerator and denominator, it returns {0,0} if no such element is possible
--first a purely internal function, given a list of integers {a,b,c,...}, this returns the list of coefficients {xa, xb, xc, ...} so that xa*a + xb*b + xc*c + ... = gcd{a,b,c,...}
bezoutNumbers := (l1) -> (
	mySize := #l1;
	if (mySize == 1) then ({1}) else (
		l2 := take(l1, 2);
		l3 := take(l1, -(mySize - 2));
		temp := gcdCoefficients toSequence l2;
		tempCoeff := take(temp, -2);
		tempGCD := temp#0;
		recursiveList := bezoutNumbers(prepend(tempGCD, l3));
		recursiveList2 := take(recursiveList, -(mySize-2));
		newCoeff := recursiveList#0;
		(newCoeff*tempCoeff) | recursiveList2
	)
);

--this finds an element of a specified degree.  Unfortunately, right now the second entry has to be an integer so this doesn't work for multi-degrees
findElementOfDegree = method();

findElementOfDegree(ZZ, Ring) := (n1, R1) ->  (
	varList := first entries vars R1;
	degList := flatten (degrees R1); --apply(varList, q -> (degree(q))#0);
	myGCD := gcd(degList);
	if (not (n1%(myGCD) == 0)) then error "findElementOfDegree: No element of the specified degree can be obtained";
	bezoutList := floor(n1/myGCD)*bezoutNumbers(degList);
	bezoutPositive := apply(bezoutList, z->max(0, z));
	bezoutNegative := (-1)*apply(bezoutList, z->min(0, z));
	{product(  apply(#varList, i -> (varList#i)^(bezoutPositive#i) )  ), product(  apply(#varList, i -> (varList#i)^(bezoutNegative#i) )  )}
);

--sometimes elements are given by multidegrees
findElementOfDegree(BasicList, Ring) := (l1, R1) ->  ( 
	if (not (all(l1, z->instance(z, ZZ)) ) ) then error "Expected a list with integer entries";
	
	if (#l1 == 0) then (error "findElementOfDegree: Expected a list of positive length";)
	else if (#l1 > 1) then (
		varList := first entries vars R1;
		degList := degrees R1;
		if ( not (#l1 == #(degList#0)) ) then error "findElementOfDegree: Vector is the wrong length";
		neg := sub(1, R1);
		pos := sub(1, R1);
		sol := apply(getLinearDiophantineSolution(l1, degList), z->floor(z));
		i := 0;
		while (i < #sol) do (
			if (sol#i > 0) then (pos = pos*((varList#i)^(sol#i)))
			else if (sol#i < 0) then (neg = neg*((varList#i)^(-sol#i)));
			i = i+1;
		);
		{pos, neg}
	)
	else ( --if this list is length 1, just use the euclidean algorithm as that as it may be faster
		findElementOfDegree(l1#0, R1)
	)
);

--Given a list of vectors v1, v_2, ... , vn, and a vector w all in ZZ^m we want to find integers a1, a2, ... an such that
--w = a1 * v1 + a2 * v2 + ... + an * vn. We return error if there is no such solution. The way to find a1, a2, ... , an 
--requires some knowledge on Smith Normal Form which says for any m * n integer matrix A, A = L* D * R where both L is in SL(m, ZZ)
--R is in SL(n, ZZ), and D = diag(d1, d2, ..., dr ) where di divides di + 1 (r = rank A).
--For the sake of simplicity, our basic list input will be a list of column vectors (which we immediately transpose).
--***it might be better to use Hermite matrices***

getLinearDiophantineSolution = method(Options => {Unsafe => false});

getLinearDiophantineSolution(BasicList, BasicList) := o ->(l1, l2) -> (	--the first entry is the target vector because that is simpler
	rowList := transpose l2;							--the list of integer rows that forms the integer matrix A
	A := matrix{ {0} };								--the integer matrix A in the matrix equation Ax = b
	D := matrix{ {0} };								--the diagonal matrix in smith normal form
	L := matrix{ {0} };								--matrix multiply A on the left which corresponds to column operations on A
	R := matrix{ {0} };								--matrix multiply A on the right which corresponds to row operations on A
	bEntries := l1;									--making the entries of target vector into a list
	b := vector( bEntries );						--the target vector in the LDE Ax = b
	flag := true;									--a boolean value used for checking
	n := 0;											--the number of columns of the integer matrix A
	m := #rowList;									--the number of rows of integer matrix A
	testNumber := 0;
	diagList := new List from {};					--the list of diagonal entries of the D in SNF of A
	r := 0;											--the number of nonzero elements in D
	diagMatrixEntry := new List from {};			--the row lists of the diagonal matrix D from SNF of A
	smithList := new List from {};
	
	if (o.Unsafe == false ) then (
		--We need to check all vectors have integer entry. And the length of the row list is equal to the length of vector b
		if( #bEntries != #rowList ) then (
			flag = false;
			error "Number of rows in the matrix should match the length of target vector"
		);
		if ( not( all(bEntries, i -> ( instance(i, ZZ) ) ) ) ) then (
			flag = false;
			error "Each entry of the target vector should be an integer"
		);
		for i when (i < #rowList ) do (
			if ( not( all (rowList#i, j ->( instance(j, ZZ) ) ) ) ) then (
				flag = false;
				error "Each entry of the ith row should be an integer"
			)
		);
		if ( m == 0) then (
			flag = false;
			error "The number of rows in the integer matrix of the linear Diophantine equation should be nonzero"
		) else (
			testNumber = #(rowList#0);
			if ( not( all(rowList, x -> (#x == testNumber) ) ) ) then (
				flag = false;
				error "The length of each row vector should be equal"
			)
		)
	);
	if ( flag == true ) then (	
		n = #(rowList#0);
		if ( n == 0 ) then (
			error "The number of columns in the integer matrix of the linear Diophantine equation should be nonzero"
		) else (
				A = matrix( rowList );							
				smithList = toList( smithNormalForm( A ) );		--smith normal form (D = L * A * R )
				D = smithList#0;								
				L = smithList#1;								
				R = smithList#2;								
				c := L * b;										--important vector for checking the existence of equation Ax = b
				cEntry := entries c;
				
				--The principle is here: the matrix solution Ax = b is equivalent to LAx = Lb = c. Now if we let y = R^(-1)x then 
				--the equation becomes (LAR)y = c which is Dy = c. So to check existence of Ax = b, it's enough to check the existence
				--of equation Dy = c which means we have to check is ci divisible by di for the first r entries.
				
				diagMatrixEntry = entries D;
				diagList = apply(min{m, n}, i -> ( diagMatrixEntry#i )#i );	--the list of diagonal entries of D
				diagList = select( diagList, x -> ( x != 0 ) );
				r = #(diagList);
				
				--To check if the system D y = c has a solution is very easy, we just need to check if each ci is divisible by di
				
				if ( not( all(r, i -> ( (cEntry#i)%(diagList#i) == 0 ) ) ) ) then (
					print "The linear Diophantine equation does not have a solution"
				)
				else (
					y := vector ( apply(n, i -> ( if (i > r - 1) then ( 0 ) else( (cEntry#i)/(diagList#i) ) ) ) ); --the solution of equation Dy = c		
					--As we said previously, y = R^(-1)x. So to get the solution of equation Ax = b, we only need to apply R to the vector y.			
					--output := flatten entries ( (inverse( R ) ) * (matrix y) );		
					output := flatten entries  ( R  * (matrix y) );		
--					1/0;	
					output
				)	
			)	
		)
);

--this solves A*x = l1
getLinearDiophantineSolution(BasicList, Matrix) := o-> (l1, A) -> (
	L := entries transpose A;
	getLinearDiophantineSolution(l1, L, Unsafe => o.Unsafe)
);


--used for construction of canonical divisors
canonicalDivisor = method(Options => {IsGraded => false});

canonicalDivisor(Ring) := o->(R1) -> (
	S1 := ambient R1;
	I1 := ideal R1;
	dR := dim R1;
	dS := dim S1;
	varList := first entries vars S1;
	degList := {};
	if (#(degree(varList#0)) == 1) then (
		degList = apply(varList, q -> (degree(q))#0); )
	else (
		degList = apply(varList, q -> (degree(q))); );
	M1 := Ext^(dS - dR)(S1^1/I1, S1^{-(sum degList)});
	moduleToDivisor(R1, M1**R1, IsGraded=>o.IsGraded)
);


--computes a ramification divisor of a finite map Y -> X of normal varieties.  It can also compute the relative canonical divisor of things like blowups (in which case, make sure the IsGraded flag is set to true)
--warning, the IsGraded functionality is not documented and may not work properly if X is not smooth.
ramificationDivisor = method(Options => {IsGraded => false});
--pass it an injective map between normal rings f1 : R1 -> S1 such that S1 is a finite R1 module.  The function assumes the two rings use the same coefficientRing.
ramificationDivisor(RingMap) := o->( f1 ) ->
(
	R1 := source f1;
	S1 := target f1;
	kk := coefficientRing S1;
	--do some sanity checking to prevent the user from getting an incorrect value because the format was wrong.
	if (o.IsGraded == false) then (
		if (not (kk === coefficientRing R1)) then error "Expected the map to be between rings with the same coefficient ring when IsGraded is set to false.";
	)
	else (
		if (not (kk === R1)) then error "Expected the coefficientRing of the target to be equal to the source when IsGraded is set to true.";
	);
	gradedMod := 0; --this is subtracted from a dimension later, it is set to 1 if IsGraded == true
	if (o.IsGraded == true) then gradedMod = 1;
	
	sourceList := first entries vars R1;
	targetList := first entries vars S1;
	numVars := #(targetList);
	YYY := local YYY;
	myMon := monoid[(sourceList|toList(YYY_1..YYY_numVars))];
	--R2 maps to S1 with flattened variables
	R2 := kk(myMon);
	f2 := map(S1, R2, (apply(sourceList,t->f1(t))) |targetList);
	K2 := ker f2;
	--R3 is the same ring with unflattened variables
	myMon3 := monoid[toList(YYY_1..YYY_numVars)];
	R3 := R1(myMon3);
	S3 := R3/(sub(K2, R3)); --a ring isomorphic to S1, we just wrote S3 = R3[stuff]
	J3 := minors(numVars-gradedMod, jacobian S3); --this should give us the locus where the map is not smooth
	J2 := sub(J3, R2); --sub this back into R2 (flattened variables)
	if (J2 == ideal(sub(0,R2))) then error "Cannot create divisor.  This map seems to be ramified everywhere, is the map inseparable?";
	divisor(f2(J2))
);



----------------------------------------------------------------
--************************************************************--
--Tests in divisors.--------------------------------------------
--************************************************************--
----------------------------------------------------------------

--Given a rational/real divisor, we want to test if this is a Weil Divisor

isWDiv = method();

isWDiv( RDiv ) := ( D ) ->
(
	coeffList := getCoeffList ( simplifyDiv( D ) );
	all(coeffList, x -> (ceiling x == floor x) )
);

--Given a divisor, we want to test is this divisor is effective or not.

isEffective = method();

isEffective(BasicDiv) := (D) -> (
	coeffList := getCoeffList ( simplifyDiv( D ) );
	all(coeffList, x->(x >= 0))
);


--Given a divisor D, we want to know is this divisor is prime or not.

isDivPrime = method();

isDivPrime( BasicDiv ) := (D) -> 
( 
	(getCoeffList (simplifyDiv D) ) == {1} 
);

--Given a divisor D, we want to know is this divisor is reduced or not.

isDivReduced = method();

isDivReduced( BasicDiv ) := (D) -> 
( 
	coeffList := getCoeffList ( simplifyDiv( D ) );
	all(coeffList, x->(x == 1))
);

--Given a divisor, we want to check if the corresponding module is globally principal or not

isDivPrincipal = method(Options => {IsGraded => false});

isDivPrincipal( WDiv ):= o -> (D) ->
(
	M := prune divisorToModule(D); 
	flag := false;
	if (o.IsGraded == false) then(
		flag = isFreeModule ( M );
		if ((flag == false) and (isDivGraded(D) == false)) then ( --let's try some other tricks to see if we can make it principal
			J1 := moduleToIdeal(M);
			if (#(first entries gens J1) == 1) then (flag = true;);
			if (flag == false) then (
				J1 = trim (J1 : (J1^0)); 
				if (#(first entries gens J1) == 1) then (flag = true;); 
			);
			if (flag == false) then ( flag = (1 == #(first entries gens gb J1)); );
			if (flag == false) then print "Warning, isDivPrincipal may give a false negative for a divisor defined by non-homogeneous ideals";
		)

	) 
	else ( --TODO:  Perhaps It would be faster to check whether or not M has a section that doesn't vanish anywhere instead of pruning M.
		if (isDivGraded(D) == false) then error "isDivPrincipal: Expected argument to be homogeneous if the IsGraded option is set to true.";
		if (isFreeModule ( M ) ) then(
			flag = (degrees M == {{0}})
		);
	);
	flag
);	

--Given a divisor, we want to check if the corresponding module is Cartier

isCartier = method(Options => {IsGraded => false});

isCartier( WDiv ) := o -> (D) ->
( --we rely on the fact that an ideal corresponds to a Cartier divisor if and only if I*I^{-1} is reflexive
  --David Eisenbud pointed out another option would be to compute a bunch of minors and see if they generate the unit ideal... I'll try this in some examples and see which is faster
	flag := false;
	R := getAmbientRing( D );
	if (o.IsGraded == false) then (
		ID := divisorToIdeal( D );
		IDminus := dualizeIdeal(ID); 
		myProduct := ID*IDminus;
		flag = (myProduct == reflexifyIdeal(myProduct))
	)
	else (
		if (isDivGraded(D) == false) then error "isCartier: Expected argument to be homogeneous if the IsGraded option is set to true.";
		myMax := ideal(first entries vars R);
		J1 := nonCartierLocus(D);
		L := saturate(J1, myMax);
		flag = isSubset(ideal(sub(1, R)), L)
	);
	flag
);	

--Get the non-Cartier locus


nonCartierLocus = method(Options => {IsGraded => false});
--TODO:  Compare this with computing minors of a presentation, I'm not sure if this will be faster or slower, there can be a lot of minors... (David Eisenbud suggested this)
nonCartierLocus( WDiv ) := o -> (D) ->
(
	R := getAmbientRing( D );
	OD := divisorToIdeal( D ); --I woulder if it would be better to saturate this first in the graded case... (or if we have multiple threads, do it at the same time we do the next command).
	ODminus:= dualizeIdeal(OD);
	I := OD*ODminus;
	J := annihilator ((reflexifyIdeal(I)*R^1) / (I*R^1));
	if (o.IsGraded == true) then (
		if (isDivGraded(D) == false) then error "nonCartierLocus: Expected argument to be homogeneous if the IsGraded option is set to true.";		
		J = saturate(J, ideal(first entries vars R))
	);
	J
);	

--Given two divisors D and E, we want to know whether they are linear equivalent or not.

isLinearEquivalent = method(Options => {IsGraded => false});

isLinearEquivalent( WDiv, WDiv ):= o->(D, E) ->
(
	isDivPrincipal(D-E, IsGraded=>o.IsGraded)
);		

--Given two rational divisor, we want to see if they are linearly equivalent

isQLinearEquivalent = method(Options => {IsGraded => false});

isQLinearEquivalent( QDiv, QDiv ):= o->(D1, D2) ->
(
	if (sameDivAmbient(D1, D2) == false) then error "isQLinearEquivalent: Expected the two divisors to have the same ambient ring";
	D1 = toQDiv(D1);
	D2 = toQDiv(D2);
	La := getCoeffList(D1);
	L1 := apply(#La, i -> denominator( La#i ) ); --list of denominators of coefficients of D1
	La = getCoeffList(D2);
	L2 := apply(#La, i -> denominator( La#i ) ); --list of denominators of coefficients of D2
	m1 := 1; if (#L1 > 0) then m1 = lcm( toSequence L1  );
	m2 := 1; if (#L2 > 0) then m2 = lcm( toSequence L2  );
	m := lcm(m1, m2);
	isLinearEquivalent( toWDiv( m * D1), toWDiv( m * D2), IsGraded=>o.IsGraded )
);


--checks whether mD1 is Cartier for any m from 1 to n1, if it is Cartier, it returns the Cartier-index.  If it is not Q-Cartier or if the Q-Cartier index is greater than n1, then it returns 0.  This can be quite slow for large values of n1.

--it would be good to compare this with a function that also uses the idealPower function
isQCartier = method(Options => {IsGraded => false});

isQCartier( ZZ, WDiv ):=o->(n1, D1) -> (
	if (n1 < 1) then error "isQCartier: Expected the first argument to be a positive integer";
	M1 := divisorToIdeal(D1);
	curModule := M1;
	S1 := ring M1;
	minusCurModule := dualizeIdeal(curModule);
	tempModule := trim (curModule*minusCurModule);
	i := 1;
	J := 0;
	if (o.IsGraded==false) then (
		flag := isReflexive(tempModule);
		while ((flag == false) and (i <= n1)) do(
			curModule = M1*curModule;
			minusCurModule = dualizeIdeal(curModule);
			curModule = dualizeIdeal(minusCurModule);
			tempModule = trim (curModule*minusCurModule);
			flag = isReflexive(tempModule);
			i = i+1;
		);
		if (flag == false) then i = 0;
	)
	else (
		if (isDivGraded(D1) == false) then error "isQCartier: Expected second argument to be homogeneous if the IsGraded option is set to true.";
		myMax := ideal(first entries vars S1);
		J = nonCartierLocus(D1);
		L := saturate(J, myMax);
		gflag := isSubset(ideal(sub(1, S1)), L);
		while ((gflag == false) and (i <= n1)) do(
			J = nonCartierLocus(i*D1);
			L = saturate(J, myMax);
			gflag = isSubset(ideal(sub(1, S1)), L);
			i = i+1;
		);
		if (gflag == false) then i = 0;
	);
	i
);


--we also can check it for Q-divisors
isQCartier(ZZ, QDiv) := o->(n1, D1) -> (
	La := getCoeffList(D1);
	L1 := apply(#La, i -> denominator( La#i ) ); --list of denominators of coefficients of D1
	m1 := lcm( toSequence L1  ); --this number clears the denominators
	m1*isQCartier(ceiling(n1/m1), IsGraded=>o.IsGraded, toWDiv(m1*D1))
);

isDivGraded = method();

isDivGraded (BasicDiv) := (D1) -> (
	pList := getPrimeList(D1);
	all(pList, isHomogeneous)
);

--checks whether a divisor is a SNC divisor
isSNC = method(Options => {IsGraded => false});

isSNC(BasicDiv) := o->(D1) -> (
	D1 = simplifyDiv(D1);
	j := 0;
	R1 := getAmbientRing(D1);
	d1 := dim R1;
	pList := getPrimeList(D1);
	idealSubsets := subsets pList;
	nonemptySubsets := select(idealSubsets, z->(#z > 0));
	toModOutBy := apply(nonemptySubsets, z -> sum(z));
	flag := isRegular(ideal(sub(0, getAmbientRing(D1))), IsGraded=>o.IsGraded);

	while ( (j < #toModOutBy) and flag ) do (
		if (o.IsGraded == false) then (
			flag = ((d1 - #(nonemptySubsets#j) == dim(R1/toModOutBy#j)) or (dim(R1/toModOutBy#j) == -infinity) );
			if (flag == true) then flag = isRegular(toModOutBy#j);
		)
		else (	
			if (isHomogeneous(toModOutBy#j) == false) then error "isSNC: Expected a homogeneous ideal";
			flag = ((d1 - #(nonemptySubsets#j) == dim(R1/toModOutBy#j)) or (dim(R1/toModOutBy#j) <= 0) );
			if (flag == true) then flag = isRegular(toModOutBy#j, IsGraded=>o.IsGraded);
		);
		j = j+1
	);
	
	flag 
);

isZeroDivisor = method();

isZeroDivisor(BasicDiv) := (D1) -> (
	D1 = simplifyDiv(D1);
	(#(getPrimeList(D1)) == 0)
);

----------------------------------------------------------------
--************************************************************--
--Global sections for divisors (base point free, etc)-----------
--************************************************************--
----------------------------------------------------------------

--given a Cartier divisor, we can find the map to projective space from the corresponding module
mapToProjectiveSpace = method(Options => {KnownCartier=>true});

mapToProjectiveSpace(WDiv) := o->(D1) -> (
	if (isDivGraded(D1) == false) then (error "mapToProjectiveSpace: Expected a graded/homogeneous divisor.";);
	if (o.KnownCartier == false) then (if (isCartier(D1, IsGraded=>true) == false) then (error "mapToProjectiveSpace: Expected a Cartier divisor."); );
	--this might be slower than the method done in the tutorial, say calling 
	--divisorToIdeal(-divPlus(D1)) and divisorToIdeal(-divMinus(D1)) 
	--and then proceeding as they did might be faster
	R1 := getAmbientRing(D1);
	M1 := prune divisorToModule(D1);
	L1 := moduleToIdeal(M1, IsGraded=>true);
	d1 := L1#1;
	M1 = L1#0*R1^{d1};
	b1 := super ((basis(0, M1))**R1);
	n1 := #(first entries b1);
	K1 := coefficientRing R1;
	YY:=local YY;
	myMon := monoid[toList(YY_1..YY_n1)];
	S1 := K1 myMon;
	map(R1, S1, first entries b1)
);

--finds the base locus of a module or divisor

baseLocus = method();

baseLocus(Module) := (M1) -> (
	b1 := basis(0, M1);
	saturate ann coker b1
);

baseLocus(WDiv) := (D1) -> (
	M1 := divisorToModule(D1);
	baseLocus(M1)
);

isVeryAmple = method(Options => {Verbose=>false});
needsPackage "RationalMaps";

isVeryAmple(WDiv) := o->(D1) -> (
    mapFromD1 := mapToProjectiveSpace(D1);
    if (#(first entries vars source mapFromD1) == 0) then (
        false)
    else (
        isEmbedding(mapFromD1, Verbose=>o.Verbose)
    )
);
 	 	
----------------------------------------------------------------
--************************************************************--
--Useful functions which don't interact with the divisor class--
--************************************************************--
----------------------------------------------------------------

idealPower = method(); -- it seems to be massively faster to reflexify ideals with few generators than ideals with many generators, at least some of the time...

idealPower(ZZ, Ideal) := (n, J) -> (
	genList := first entries gens J;
	ideal( apply(genList, z->z^n))
);

--we can also turn things into ideals directly

dualizeIdeal = method(Options => {KnownNormal=>true});

dualizeIdeal(Ideal) := o->(I1) -> (
	S1 := ring I1;
	assumeNormal := false;
	if (o.KnownNormal == true) then (assumeNormal = true;) else (assumeNormal = isNormal(S1););
	if (assumeNormal) then ( 
		if (I1 == ideal sub(0, S1)) then (
			I1
		)
		else(
			x := sub(0, S1);
			i := 0;	
			genList := first entries gens I1;
			while ((i < #genList) and (x == sub(0, S1))) do(
				x = genList#i;
				i = i+1;	
			);
			ideal(x) : I1
		)
		
	)
	else (
		inc := inducedMap(S1^1, I1*(S1^1));
		mydual := Hom(inc, S1^1);
		moduleToIdeal(mydual)
	)

);

--Given an ideal, we will frequently want to double-dualize / find the S2-ification.  The following function does this.

reflexifyIdeal = method(Options => {KnownNormal=>true});

--the first variant simply reflexifies an ideal

reflexifyIdeal(Ideal) := o->(I1) -> (
	S1 := ring I1;
	assumeNormal := false;
	if (o.KnownNormal == true) then (assumeNormal = true;) else (assumeNormal = isNormal(S1););
	if (assumeNormal) then ( 
		if (I1 == ideal sub(0, S1)) then (
			I1
		)
		else(
			x := sub(0, S1);
			i := 0;	
			genList := first entries gens I1;
			while ((i < #genList) and (x == sub(0, S1))) do(
				x = genList#i;
				i = i+1;	
			);
			ideal(x) : (ideal(x) : I1)
		)
		
	)
	else (
		inc := inducedMap(S1^1, I1*(S1^1));
		ddual := Hom(Hom(inc, S1^1), S1^1);
		annihilator(coker ddual)
	)
);

--we also reflexify modules
reflexifyModule = method();

reflexifyModule(Module) := (M1) -> (
	S1 := ring M1;
	prune (Hom(Hom(M1, S1^1), S1^1))
);

--we can reflexify modules and return the map to the doubleDual of the module
reflexifyModuleWithMap = method();

reflexifyModuleWithMap(Module) := (M1) ->(
	S1 := ring M1;
	gensMatrix := gens M1;
	h := map(M1, source gensMatrix, id_(source gensMatrix));
	ddh := Hom(Hom(h, S1^1), S1^1);
	map(target ddh, M1, matrix ddh)
);

isReflexive = method();

isReflexive(Module) := (M1) ->(
	g := reflexifyModuleWithMap(M1);
	(-1 == dim coker g)
);

isReflexive(Ideal) := (I1) ->(
	J1 := reflexifyIdeal(I1);
	(J1 == I1)
);

--we can also grab the torsion submodule since we are here
torsionSubmodule = method();

torsionSubmodule(Module) := (M1) -> (
	ker reflexifyModuleWithMap(M1)
);

--this method embeds a rank 1 module as a divisor
--this method is based on and inspired by code originally written by Moty Katzman, earlier versions can be found in 
-- http://katzman.staff.shef.ac.uk/FSplitting/ParameterTestIdeals.m2
--under canonicalIdeal

moduleToIdeal = method(Options => {MTries=>10, IsGraded=>false, ReturnMap=>false});

moduleToIdeal(Ring, Module) := o ->(R1, M2) -> 
(--turns a module to an ideal of a ring
--	S1 := ambient R1;
	flag := false;
	answer:=0;
--	M2 := prune M1;
--	myMatrix := substitute(relations M2, S1);
--	s1:=syz transpose substitute(myMatrix,R1);
--	s2:=entries transpose s1;
	s2 := entries transpose syz transpose presentation M2;
	h := null;
	--first try going down the list
	i := 0;
	t := 0;
	d1 := 0;
	while ((i < #s2) and (flag == false)) do (
		t = s2#i;
		h = map(R1^1, M2**R1, {t});
		if (isWellDefined(h) == false) then error "moduleToIdeal: Something went wrong, the map is not well defined.";
		if (isInjective(h) == true) then (
			flag = true;
			answer = trim ideal(t);
			if (o.IsGraded==true) then (
				--print {degree(t#0), (degrees M2)#0};
				d1 = degree(t#0) - (degrees M2)#0;
				answer = {answer, d1};
			);
			if (o.ReturnMap==true) then (
				answer = flatten {answer, h};
			)
		);
		i = i+1;
	);
	-- if that doesn't work, then try a random combination/embedding
     i = 0;
	while ((flag == false) and (i < o.MTries) ) do (
		coeffRing := coefficientRing(R1);
		d := sum(#s2, z -> random(coeffRing, Height=>100000)*(s2#z));
       -- print d;
		h = map(R1^1, M2**R1, {d});
		if (isWellDefined(h) == false) then error "moduleToIdeal: Something went wrong, the map is not well defined.";
		if (isInjective(h) == true) then (
			flag = true;
			answer = trim ideal(d);
			if (o.IsGraded==true) then (
				d1 = degree(d#0) - (degrees M2)#0;
				answer = {answer, d1};
			);
			if (o.ReturnMap==true) then (
				answer = flatten {answer, h};
			)
		);
        i = i + 1;
	);
	if (flag == false) then error "moduleToIdeal: No way found to embed the module into the ring as an ideal, are you sure it can be embedded as an ideal?";
	answer
);

moduleToIdeal(Module) := o ->(M1) ->
(
	S1 := ring M1;
	moduleToIdeal(S1, M1, MTries=>o.MTries, IsGraded=>o.IsGraded, ReturnMap=>o.ReturnMap)
);

--this variant takes a map from a free module of rank 1 and maps to another rank 1 module.  The function returns the second module as an ideal combined with the element 

moduleWithSectionToIdeal = method(Options => {MTries=>10, ReturnMap=>false});

moduleWithSectionToIdeal(Matrix) := o->(f1)->
(
	M1 := source f1;
	M2 := target f1;
	R1 := ring M1;
	if ((isFreeModule M1 == false) or (not (rank M1 == 1))) then error ("moduleWithSectionToIdeal: Error, source is not a rank-1 free module";);
	flag := false;
	answer:=0;
	s2 := entries transpose syz transpose presentation M2;
	h := null;
	--first try going down the list
	i := 0;
	t := 0;
	d1 := 0;
	while ((i < #s2) and (flag == false)) do (
		t = s2#i;
		h = map(R1^1, M2**R1, {t});
		if (isWellDefined(h) == false) then error "moduleWithSectionToIdeal: Something went wrong, the map is not well defined.";
		if (isInjective(h) == true) then (
			flag = true;
			answer = trim ideal(t);
			if (o.ReturnMap==true) then (
				answer = flatten {answer, h};
			);
		);
		i = i+1;
	);
	-- if that doesn't work, then try a random combination/embedding
	while ((flag == false) and (i < o.MTries) ) do (
		coeffRing := coefficientRing(R1);
		d := sum(#s2, z -> random(coeffRing, Height=>100000)*(s2#z));
		h = map(R1^1, M2**R1, {d});
		if (isWellDefined(h) == false) then error "moduleWithSectionToIdeal: Something went wrong, the map is not well defined.";
		if (isInjective(h) == true) then (
			flag = true;
			answer = trim ideal(d);
			if (o.ReturnMap==true) then (
				answer = flatten {answer, h};
			);
		);
	);
	
	if (flag == false) then error "moduleWithSectionToIdeal: No way found to embed the module into the ring as an ideal, are you sure it can be embedded as an ideal?";
	newMatrix := h*f1;
	flatten {first first entries newMatrix, answer}
);


isDomain = method();

isDomain(Ring) := (R1) -> (
	isPrime( ideal(sub(0, R1)))
);

--checks whether R/J1 is regular
isRegular  = method(Options => {IsGraded => false});

isRegular(Ideal) := o->J1 -> (
	--empty schemes are smooth (which is why we are first check whether ideals are the whole ring or contain the irrelevant ideal
	flag := false;
	if (o.IsGraded == true) then (
		if (isHomogeneous(J1) == false) then (error "isRegular: Expected a homogeneous ideal");
		myMax := ideal first entries vars ring J1;
		if (isSubset(myMax, J1)) then (flag = true) else (flag = (dim singularLocus J1 <= 0));
	)
	else ( 
		if (isSubset(ideal(sub(1, ring J1)), J1)) then (flag = true) else (flag = (dim singularLocus J1 == -infinity));
	);

	flag
);

reflexivePower = method();

reflexivePower(ZZ, Ideal) := (n1, I1) -> (
	reflexifyIdeal(idealPower(n1, I1))
);

--****************************************************--
--*****************Documentation**********************--
--****************************************************--

beginDocumentation();

doc /// 
	 Key
		Divisor	
     Headline
     	A package for divisors on normal rings (graded or not).
     Description
    	Text   
    	 A package for handling Weil divisors on normal rings, graded or not.
///

doc ///
	Key
	 BasicDiv
	Headline
	 the class of divisors with unspecified coefficients
	Description
	 Text
	  The class of divisors whose coefficients are unspecified, a base class.  Not typically for use.  All subtypes have the same essential structure.
	    
	 Text
	  The basic structure is a HashTable.  There is one key which has a value which specifies the ambient ring.  The other keys are a Groebner basis L for each prime ideal P in the support with corresponding value a list {n, P} where n is the coefficient of the prime and P is how the user entered the ideal initially.
	 Example
	  R = QQ[x,y,z]
	  D = divisor(x*y^2*z^3)
	  H = new HashTable from D
	SeeAlso
	 WDiv
	 QDiv
	 RDiv
///

doc ///
	Key
	 (net, BasicDiv)
	Headline
	 Controls how divisors are displayed to the user
	Description
	 Text
	  Currently divisors are listed as sums with the ambient ring stated at the end.
///

doc ///
	Key
	 WDiv
	Headline
	 the class of divisors with integer coefficients
	Description
	 Text
	  The class of divisors whose coefficients are integers.  Should be constructed by the divisor method.  For the underlying structure, see BasicDiv.
	SeeAlso
	 divisor
	 BasicDiv
	 QDiv
	 RDiv
///

doc ///
	Key
	 QDiv
	Headline
	 the class of divisors with rational coefficients
	Description
	 Text
	  The class of divisors whose coefficients are rational numbers.  Should be constructed with the rationalDivisor method or with divisor(..., coeffType=>QQ).  For the underlying structure, see BasicDiv.
	SeeAlso
	 divisor
	 rationalDivisor
	 BasicDiv
	 WDiv
	 RDiv
///;

doc ///
	Key
	 RDiv
	Headline
	 the class of divisors with real coefficients
	Description
	 Text
	  The class of divisors whose coefficients are real numbers.  Should be constructed with the realDivisor method or with divisor(..., coeffType=>RR).  For the underlying structure, see BasicDiv.
	SeeAlso
	 divisor
	 realDivisor
	 BasicDiv
	 WDiv
	 QDiv
///

doc ///
	Key
	 Unsafe
	Headline
	 An option used to tell divisor construction not to do various checks
	Description
	 Text
	  If set to true, then when divisor is called, there will be no checks as to whether the given ideals are prime of height one and all of the same ring, whether the coefficients are the right kind, etc.  Default value is false.
///

doc ///
	Key
	 CoeffType
	Headline
	 An option used to tell divisor construction that a particular type of coefficients are expected.
	Description
	 Text
	  Can be set to ZZ, QQ or RR (or any other Thing) when divisor is called, it checks whether the coefficient list is a list of instances of this type.  If it is set to ZZ, QQ or RR then a WDiv, QDiv or RDiv are created.  Default value is ZZ.
///

doc ///
	Key
	 AmbRing
	Headline
	 An option used to tell divisor construction that a particular ambient ring is expected.
	Description
	 Text
	  If set to a ring, then when calling divisor, the primes are all checked whether they are ideals in that ring. 
///

doc ///
	Key
	 MTries
	Headline
	 An option used by moduleToIdeal how many times to try embedding the module as an ideal in a random way.
	Description
	 Text
	  After making some canonical attempts, moduleToIdeal tries to embed the module into a ring as an ideal in a random way.  The value of this option is how many times that random embedding is attempted.  The default value is 10.
///

doc ///
	Key
	 IsGraded
	Headline
	 An option used by numerous functions which tells it to treat the divisors as if we were working on the Proj of the ambient ring.
	Description
	 Text
	  An option used by numerous functions which tells it to treat the divisors as if we were working on the Proj of the ambient ring.  In other words, setting it to true tells the function to ignore behavior at the irrelevant ideal (the ideal generated by vars Ring).  Default value is false.
///

doc /// 
	 Key
		divisor
		(divisor, BasicList, BasicList)
		(divisor, Ideal)
		(divisor, RingElement)
		(divisor, BasicList)
		[divisor, Unsafe]
		[divisor, CoeffType]
		[divisor, AmbRing]
     Headline
     	Constructor for (Weil/Q/R)-divisors
     Usage
     	D1 = divisor(l1, l2, Unsafe=>u, CoeffType=>v, AmbRing=>R)
     	D2 = divisor( I )
     	D3 = divisor( f )
     	D4 = divisor( l3, Unsafe=>u, CoeffType=>v, AmbRing=>R)
     Inputs
      	l1: BasicList
      		which describes the list of coefficients in integers
      	l2: BasicList
      		which describes the list of height one prime ideals that corresponds to codimension one irreducible subspaces
      	I: Ideal
      	f: RingElement
      	l3: BasicList
      		a list of pairs {c, P} where c is a coefficient of type v and P is a prime ideal
      	u: Boolean
      		if true, no checking of the primes (to see if they are height one etc.) will be attempted, false by default
      	v: Boolean
      		default is ZZ.  If ZZ, QQ, RR, then the divisor will be a WDiv, QDiv, or RDiv. Other coefficients can be specified too which yields a BasicDiv
      	R: Ring
      	        an option to specify a particular ambient ring
      		
     Outputs
      	 D1: WDiv
      	 D2: WDiv
      	 D3: WDiv
      	 D4: WDiv
     Description
      Text
		The general function for constructing Weil divisors.  If f is an element of a fraction field, it is handled appropriately.
      Example
       R = QQ[x,y,z]
       D = divisor({1,2,3}, {ideal(x), ideal(y), ideal(z)})
       E = divisor(x*y^2*z^3)
       F = divisor(ideal(x*y^2*z^3))
       G = divisor({{1, ideal(x)}, {2, ideal(y)}, {3, ideal(z)}})
      Text
       Creates the same Weil divisor with coefficients 1, 2 and 3 in four different ways       
      Example
       R = QQ[x,y,z]/ideal(x^2-y*z)
       D = divisor({2}, {ideal(x,y)})
       E = divisor(y)
      Text
       Creates the same Weil divisor in two different ways
      Example
       R = ZZ/7[x,y]
       D = divisor({-1/2, 2/1}, {ideal(y^2-x^3), ideal(x)}, CoeffType=>QQ)
      Text
       Constructs a Q-divisor
      Example
       R = ZZ/11[x,y,u,v]/ideal(x*y-u*v)
       D = divisor({1.1, -3.14159}, {ideal(x,u), ideal(x, v)}, CoeffType=>RR)
     SeeAlso
     	rationalDivisor
     	realDivisor
///

doc /// 
	 Key
		rationalDivisor
		(rationalDivisor, BasicList, BasicList)
		(rationalDivisor, BasicList)
		[rationalDivisor, AmbRing]
		[rationalDivisor, Unsafe]
     Headline
     	Constructs a Q-divisor
     Usage
     	D1 = rationalDivisor(l1, l2, Unsafe=>u, AmbRing=>R)
     	D2 = rationalDivisor( l3, Unsafe=>u, AmbRing=>R)
     Inputs
      	l1: BasicList
      		which describes the list of coefficients in rational numbers
      	l2: BasicList
      		which describes the list of height one prime ideals that corresponds to codimension one irreducible subspaces
      	l3: BasicList
      		a list of pairs {c, P} where c is a rational coefficient and P is a prime ideal
      	u: Boolean
      		if true, no checking of the primes (to see if they are height one etc.) will be attempted, false by default
   	R: Ring
      	        an option to specify a particular ambient ring

     Outputs
      	 D: QDiv
     Description
      Text
       Construct a divisor as a formal sum of height one prime ideals whose coefficients are rational numbers
      Example
       R = QQ[x,y,z]
       D = rationalDivisor({5/2, -3/5}, {ideal(x+y+z), ideal(x)})
     SeeAlso
     	divisor
     	realDivisor
///

doc /// 
	 Key
		realDivisor
		(realDivisor, BasicList, BasicList)
		(realDivisor, BasicList)
		[realDivisor, Unsafe]
		[realDivisor, AmbRing]
     Headline
     	Constructs an R-divisor
     Usage
     	D = realDivisor(l1, l2, Unsafe=>u, AmbRing=>R)
     	D = realDivisor(l3, Unsafe=>u, AmbRing=>R)
     Inputs
      	l1: BasicList
      		which describes the list of coefficients in real numbers
      	l2: BasicList
      		which describes the list of height one prime ideals that corresponds to codimension one irreducible subspaces
      	l3: BasicList
      		a list of pairs {c, P} where c is a real coefficient and P is a prime ideal
      	u: Boolean
      		if true, no checking of the primes (to see if they are height one etc.) will be attempted, false by default
      	R: Ring
      	        an option to specify a particular ring
     Outputs
      	 D: RDiv
     Description
      Text
       Construct a divisor as a formal sum of height one prime ideals whose coefficients are real numbers
      Example
       R = QQ[x,y,z]
       D = realDivisor({1.7, -2.3}, {ideal(x+y+z), ideal(x)})
     SeeAlso
     	divisor
     	rationalDivisor
///

doc ///
	Key
	 verifyDivisor
	 (verifyDivisor, BasicDiv)
	 [verifyDivisor, Verbose]
	Headline
	 Checks to make sure a divisor is valid
	Usage
	 b = verifyDivisor( D1 )
	Inputs
	 D1: BasicDiv
	Outputs
	 b: Boolean
	Description
	 Text
	  Given a WDiv/QDiv/RDiv/BasicDiv, this function tries to verify that this really is a valid divisor.  It checks that the coefficients are from the right ring (in the WDiv/QDiv/RDiv cases at least).  It also checks to make sure all the ideals are from the same ring, are prime, and have height one.  If Verbose is set to true (it is true by default), then it will print an error message explaining why the divisor was not valid.
	 Example
	  R = QQ[x,y];
	  verifyDivisor(divisor({1}, {ideal(x)}, Unsafe=>true))
	  verifyDivisor(divisor({1/2}, {ideal(x)}, Unsafe=>true))
	  verifyDivisor(divisor({1/2}, {ideal(x)}, Unsafe=>true, CoeffType=>QQ))
	  verifyDivisor(divisor({1}, {ideal(x,y)}, Unsafe=>true))
	  verifyDivisor(divisor({1}, {ideal(x^2)}, Unsafe=>true))
	  S = QQ[a,b];
	  verifyDivisor(divisor({1,2}, {ideal(x), ideal(a)}, Unsafe=>true))
	SeeAlso
	 divisor
	 Unsafe
///


doc ///
	Key
	 getPrimeList
	 (getPrimeList, BasicDiv)
	Headline
	 Get the list of height-one primes in the support of a divisor
	Usage
	 L1 = getPrimeList( D1 )
	Inputs
	 D1: BasicDiv
	Outputs
	 L1: List
	Description
	 Text
	  Returns the list of height-one prime ideals corresponding to the components of a BasicDiv.  Note that if you don't call simplifyDiv, this can return primes with coefficient equal to zero.
	 Example
	  R = QQ[x,y,u,v]/ideal(x*y-u*v)
	  D = divisor(x)
	  getPrimeList(D)
	  E = divisor(x*u)
	  getPrimeList(E)
	SeeAlso
	 getCoeffList
	 getGBList
///

doc ///
	Key
	 getPrimeCount
	 (getPrimeCount, BasicDiv)
	Headline
	 Get the number of height one primes in the support of the divisor
	Usage
	 L1 = getPrimeCount( D1 )
	Inputs
	 D1: BasicDiv
	Outputs
	 L1: List
	Description
	 Text
	  Returns the number of height one prime ideals corresponding to the components of a BasicDiv.  Note that if you don't call simplifyDiv, this can return primes with coefficient equal to zero.
	 Example
	  R = QQ[x,y,u,v]/ideal(x*y-u*v)
	  D = divisor(x)
	  getPrimeCount(D)
	  E = divisor(x*u)
	  getPrimeCount(E)
	SeeAlso
	 getCoeffList
	 getGBList
///

doc ///
	Key
	 getGBList
	 (getGBList, BasicDiv)
	Headline
	 Get the list of Groebner bases corresponding to the height-one primes in the support of a divisor
	Usage
	 L1 = getGBList( D1 )
	Inputs
	 D1: BasicDiv
	Outputs
	 L1: List
	Description
	 Text
	   Returns the list of Groebner bases associated to the height-one prime ideals corresponding to the components of a BasicDiv (or a WDiv, QDiv or RDiv)
	 Example
	  R = ZZ/7[x,y,u,v]/ideal(x*y-u*v)
	  D = divisor(x)
	  getGBList(D)
	SeeAlso
	 getCoeffList
	 getPrimeList
///
	
doc ///
	Key
	 getCoeffList
	 (getCoeffList, BasicDiv)
	Headline
	 Get the list of coefficients of a divisor
	Usage
	 L1 = getPrimeList( D1 )
	Inputs
	 D1: BasicDiv
	Outputs
	 L1: List
	Description
	 Text
	  Get the list of coefficients of a BasicDiv (or a WDiv, QDiv or RDiv).
	 Example
	  R = QQ[x,y,u,v]/ideal(x*y-u*v)
	  D = divisor(x)
	  getCoeffList(D)
	  E = divisor(x*u)
	  getCoeffList(E)
	  F = rationalDivisor({1/2, -2/3}, {ideal(x,u), ideal(x,v)})
	  getCoeffList(F)
	  G = realDivisor({0.5, -0.667}, {ideal(x,u), ideal(x,v)})
	  getCoeffList(G)
	SeeAlso
	 getPrimeList
	 getGBList
///

doc /// 
	Key
	 simplifyDiv
	 (simplifyDiv, BasicDiv)
  	Headline
   	 Removes primes with coefficient zero
   	Usage
   	 E1 = simplifyDiv( D1 )
     	Inputs
      	 D1: BasicDiv
     	Outputs
      	 E1: BasicDiv   
     	Description  	
     	 Text
     	  Returns a divisor where all entries with coefficient zero are removed. 
     	 Example
     	  R = QQ[x,y,z]
     	  D = divisor({1,0,-2}, {ideal(x), ideal(y), ideal(z)})
     	  simplifyDiv(D)
///

doc ///
	Key 
	 coeff
	 (coeff, Ideal, BasicDiv)
	 (coeff, BasicList, BasicDiv)
	Headline
	 Get the coefficient of a given ideal for a fixed divisor
	Usage
	 n = coeff(D, P)
	 m = coeff(D, L)
	Inputs
	 P: Ideal
	 D: BasicDiv
	 L: BasicList
	Outputs
	 n: Thing
	 m: Thing
	Description
	 Text
	  Returns the coefficient of D along the prime divisor associated to P (or generated by L).  Frequently n or m are integers, rational or real numbers if D is WDiv, QDiv or RDiv respectively
	 Example
	  R = QQ[x,y,u,v]/ideal(x*y-u*v)
	  D = divisor(x)
	  coeff(ideal(x,u), D)
	  E = divisor(x*u)
	  coeff(ideal(x,u), E)

///

doc ///
	 Key
		getAmbientRing
		(getAmbientRing, BasicDiv)
	Headline
		Get the ambient ring of a divisor
	Usage
		E1 = getAmbientRing( D1 )
	Inputs
		D1: BasicDiv
	Outputs
		E1: Ring
	Description
	  Text
	   Returns the ambient ring of a divisor.
	  Example
	   R = QQ[x, y, z] / ideal(x * y - z^2 )
	   D = divisor({1, 2}, {ideal(x, z), ideal(y, z)})
	   getAmbientRing( D )
	  Text
	   If the divisor was created with the Unsafe option there may be more than one ambient ring.  This function then returns one of those ambient rings.  
///

doc ///
	 Key
		isVeryAmple
		(isVeryAmple, WDiv)
		[isVeryAmple, Verbose]
	Headline
		Checks if a divisor is very ample.
	Usage
		b = isVeryAmple( D1, Verbose=>vb )
	Inputs
		D1: WDiv
		vb: Boolean
	Outputs
		b: Boolean
	Description
	  Text
	   Returns true if the divisor is very ample, otherwise it returns false.  It works by calling isEmbedding from the RationalMaps package.  If Verbose is set to true, it will print Verbose output from isEmbedding.
	  Example
	   R = QQ[x, y, z]/ideal(x^3 + y^3 - z^3);
	   D = divisor(ideal(x, y-z));
	   isVeryAmple(D)
	   isVeryAmple(2*D)
	   isVeryAmple(3*D)	   
///


doc ///
	 Key
		getPrimeDivisors
		(getPrimeDivisors, BasicDiv)
	Headline
		Returns the list of prime divisors of a given divisor
	Usage
		L1 = getPrimeDivisors( D1 )
	Inputs
		D1: BasicDiv
	Outputs
		L1: List
	Description
	  Text
	   Returns the list of prime divisors of a given divisor.  The prime divisors are all of the class WDiv
	  Example
	   R = QQ[x, y, z] 
	   D = divisor({-8, 2}, {ideal(x), ideal(y)})
	   getPrimeDivisors( D )
///

doc ///
	 Key
		isDivAmbient
		(isDivAmbient, BasicDiv, Ring)
	Headline
		Checks whether the ambient ring of a given divisor is the given ring
	Usage
		b = isDivAmbient( D1, R1 )
	Inputs
		D1: BasicDiv
		R1: Ring
	Outputs
		b: Boolean
	Description
	  Text
	   Returns true if the ambient ring of D1 is equal to (===) R1.  Otherwise it returns false.  
	  Example
	   R = QQ[x, y, z] / ideal(x * y - z^2 )
	   D = divisor({-1, 2}, {ideal(x, z), ideal(y, z)})
	   isDivAmbient(D, R)
	   S = QQ[a,b]
	   isDivAmbient(D, S)
	  Text
	   If D1 is the zero divisor, it always returns true.
	   
///

doc ///
	Key
		sameDivAmbient
		(sameDivAmbient, BasicDiv, BasicDiv)
	Headline
		Checks whether the ambient ring of the given divisors are equal
	Usage
		b = sameDivAmbient( D1, D2 )
	Inputs
		D1: BasicDiv
		D2: BasicDiv
	Outputs
		b: Boolean
	Description
	  Text
	   Returns true if the ambient ring of D1 is equal (===) to the ambient ring of D2. Otherwise it returns false.  
	  Example
	   R1 = QQ[x, y, z] / ideal(x * y - z^2)
	   R2 = QQ[a, b, c, d] / ideal(a * b - c * d)
	   D1 = divisor({1, -2}, {ideal(x, z), ideal(y, z)})
	   D2 = divisor({-3, 4}, {ideal(a, c), ideal(b, d)})
	   sameDivAmbient(D1, 2*D1)
	   sameDivAmbient(D1, D2)
	  Text
	   If either D1 or D2 is the zero divisor, it always returns true.
///


doc ///
	Key 
		floorDiv
		(floorDiv, RDiv)
	Headline
		Get a divisor whose coefficients are floors of the given divisor
	Usage
		E2 = floorDiv( E1 )
	Inputs
		E1: RDiv
	Outputs
		E2: WDiv
	Description
	 Text
	  Start with a rational/real divisor, we form a new divisor whose coefficients are obtained by taking the floor function
	 Example
	  R = QQ[x, y, u, v] / ideal(x * y - u * v)
	  D = divisor({1.2, -3.4, 5.7, -9.8}, {ideal(x, u), ideal(x, v), ideal(y, u), ideal(y, v)}, CoeffType => RR)
	  floorDiv( D )
	SeeAlso
		ceilingDiv
///

doc ///
	Key 
		ceilingDiv
		(ceilingDiv, RDiv)
	Headline
		Get a divisor whose coefficients are ceilings of the given divisor
	Usage
		E2 = ceilingDiv( E1 )
	Inputs
		E1: RDiv
	Outputs
		E2: WDiv
	Description
	 Text
	  Start with a rational/real divisor, we form a new divisor whose coefficients are obtained by taking the ceiling function 
	 Example
	  R = QQ[x, y, z] / ideal(x *y - z^2)
	  D = divisor({1/2, 4/3}, {ideal(x, z), ideal(y, z)}, CoeffType => QQ)
	  ceilingDiv( D )
	SeeAlso
		floorDiv
///

doc ///
	Key
	 divPlus
	 (divPlus, RDiv)
	Headline
	 Get the positive part of a given divisor
	Usage
	 F2 = divPlus( F1 )
	Inputs
	 F1: RDiv
	Outputs
	 F2: RDiv
	Description
	 Text
	  Return the positive part of a divisor
	 Example
	  R = QQ[x, y, u, v] / ideal(x * y - u * v)
	  D = divisor({1, -2, 3, -4}, {ideal(x, u), ideal(y, u), ideal(x, v), ideal(y, v)})
	  divPlus( D )
	SeeAlso
	  divMinus
///

doc ///
	Key
	 divMinus
	 (divMinus, RDiv)
	Headline
	 Get the negative part of a divisor
	Usage
	 F2 = divMinus( F1 )
	Inputs
	 F1: RDiv
	Outputs
	 F2: RDiv
	Description
	 Text
	  Return the negative part of a given divisor
	 Example
	  R = QQ[x, y, u, v] / ideal(x * y - u * v)
	  D = divisor({1, -3, 5, -7}, {ideal(x, u), ideal(x, v), ideal(y, u), ideal(y, v)})
	  divMinus( D )
	SeeAlso
	  divPlus
///

doc ///
	 Key
	  (symbol ==, RDiv, RDiv)
	 Headline
	  Check if two divisors are equal
	 Usage
	  b = (D == E)
	 Inputs
	 	D: RDiv
	 	E: RDiv
	 Outputs
	 	b: Boolean
	 Description
	 	Text
	 	 Returns true if the two divisors are equal
	 	Example 
	 	 R = QQ[x,y]
	 	 D = divisor(x*y)
	 	 E = divisor(x)
	 	 F = divisor(y)
	 	 D == E
	 	 D == E+F
	 	Text
	 	 Here is an example with rational coefficients compared with integer coefficients
	 	Example
	 	 R = QQ[x,y];
	 	 D = (1/2)*divisor(x)
	 	 D == 2*D
	 	 D + D == 2*D	
	 	 E = divisor(x)
	 	 D == E
	 	 2*D == E
///


doc ///
	 Key 
	 	toQDiv
	 	(toQDiv, WDiv)
	 	(toQDiv, QDiv)
	 Headline
	 	Turn a Weil divisor to a rational divisor
	 Usage
	 	D' = toQDiv( D )
	 Inputs
	 	D: WDiv
	 	D: QDiv
	 Outputs
	 	D': QDiv
	 Description
	  Text
	   Turn a Weil divisor into a rational divisor (or do nothing to a rational divisor).
	  Example
	   R = ZZ/5[x]
	   D = divisor(x)
	   E = toQDiv(D)
	   toQDiv(E)
	 SeeAlso
	 	toWDiv
	 	toRDiv
///

doc ///
	Key
		toRDiv
		(toRDiv, WDiv)
		(toRDiv, QDiv)
		(toRDiv, RDiv)
	Headline
		Turn a integer/rational divisor to a real divisor
	Usage
		D2 = toRDiv( D1 )
		E2 = toRDiv( E1 )
	Inputs
		D1: WDiv
		E1: QDiv
		E1: RDiv
	Outputs
		D2: RDiv
		E2: RDiv
	Description
	  Text
	   Turn a Weil divisor or a Q divisor into a real divisor (or do nothing to a real divisor).
	  Example
	   R = ZZ/5[x];
	   D = divisor(x)
	   E = (2/2)*D
	   F = toRDiv(E)
	   G = toRDiv(D)
	   F == G
	SeeAlso
		toWDiv
		toQDiv		
///

doc ///
	Key
	 	toWDiv
	 	(toWDiv, RDiv)
	Headline
		Turn a rational/real divisor with integer coefficients into to a Weil Divisor
	Usage
		E2 = toWDiv( E1 )
	Inputs
		E1: RDiv
	Outputs
		E2: WDiv
	Description
	 Text
	  Given a divisor with rational or real coefficients, but whose coefficients are actually integers, we first check if all coefficients are integers.
	  If so we make this divisor to a Weil divisor.  Otherwise, an error is thrown.
	 Example
	  R=QQ[x]
	  D=rationalDivisor({3/2}, {ideal(x)})
	  E=realDivisor({1.5}, {ideal(x)})
	  toWDiv(2*D)
	  toWDiv(2*E)
	  try toWDiv(D) then print "converted to a WDiv" else print "can't be converted to a WDiv"
	SeeAlso
		toQDiv
		toRDiv
		isWDiv
///

doc ///
	Key
		(symbol *, ZZ, BasicDiv)
	Headline
		Multiply a divisor by an integer
	Usage
		E = n * D
 	Inputs
 		n: ZZ
 		D: BasicDiv
 	Outputs
 		E: BasicDiv
 	Description
 	 Text
 	 	Multiply a divisor by an integer
 	 Example
 	  R = QQ[x,y]
 	  D = divisor(x)
 	  E = rationalDivisor({1/2}, {ideal(x)})
 	  F = realDivisor({0.5}, {ideal(x)})
 	  7*D
 	  2*E
 	  6*F
///

doc ///
	Key
		(symbol *, QQ, WDiv)
	Headline
		Multiply a Weil divisor by a rational number
	Usage
		E = r * D
 	Inputs
 		r: QQ
 		D: WDiv
 	Outputs
 		E: QDiv
 	Description
 	 Text
 	 	Multiply a Weil divisor by a rational number
 	 Example
 	  R = QQ[x,y]
 	  D = divisor(x^2*y)
 	  (5/6)*D
///



doc ///
	Key
		(symbol *, RR, QDiv)
	Headline
		Multiply a rational divisor by a real number
	Usage
		E = x * D
 	Inputs
 		x: RR
 		D: QDiv
 	Outputs
 		E: RDiv
 	Description
 	 Text
 	 	Multiply a rational divisor by a real number 
 	 Example 
 	 	R = QQ[x, y, z]
 	 	D = divisor(x * y * z^2)
 	 	(1.414) * D
///

doc ///
	Key
		(symbol *, QQ, RDiv)
	Headline
		Multiply a real divisor by a rational number
	Usage
		E = r * D
 	Inputs
 		r: QQ
 		D: RDiv
 	Outputs
 		E: RDiv
 	Description
 	 Text
 	 	Multiply a real divisor by a rational number
 	 Example
 	  R = ZZ/101[x, y, z]
 	  D = divisor(x * y * z^2)
 	  (3/4) * D
///

doc ///
	Key
		(symbol *, RR, RDiv)
	Headline
		Multiply a real divisor by a real number
	Usage
		E = x * D
 	Inputs
 		x: RR
 		D: RDiv
 	Outputs
 		E: RDiv
 	Description
 	 Text
 	 	Multiply a real divisor by a real number 
 	 Example
 	    R = ZZ/31[x, y, z] / ideal(x * y - z^2 )
 	    D = divisor({1.2, -3.4}, {ideal(x, z), ideal(y, z)}, CoeffType=>RR)
 	    (7.8) * D
///

doc ///
  Key
    (symbol +, BasicDiv, BasicDiv)
  Headline
    Sum two divisors.
  Usage
    C = A + B
  Inputs
    A:BasicDiv
    B:BasicDiv
  Outputs
    C:BasicDiv
  Description
    Text
      Returns the sum of two divisors. For example it can add Weil divisors
    Example
      R = QQ[x, y, z]
      D1 = divisor({1, 2, 1, 3, 8}, {ideal(x), ideal(y), ideal(z), ideal(y), ideal(y)})
      D2 = divisor({-2, 3, -5}, {ideal(z), ideal(y), ideal(x)})
      D1 + D2
    Text
      or it can add divisors of different types
    Example
      R = QQ[x]
      D1 = divisor({3}, {ideal(x)})
      D2 = divisor({3/2}, {ideal(x)}, CoeffType=>QQ)
      D3 = divisor({1.333}, {ideal(x)}, CoeffType=>RR)
      D1+D2
      D1+D3
      D2+D3
      
///

doc ///
  Key
    (symbol -, BasicDiv, BasicDiv)
  Headline
    Subtract two divisors.
  Usage
    C = A - B
  Inputs
    A:BasicDiv
    B:BasicDiv
  Outputs
    C:BasicDiv
  Description
    Text
      Subtract two divisors.
    Example
      R = ZZ/11[x, y]
      D1 = divisor({3.4, -5.6}, {ideal(x), ideal(y)}, CoeffType => RR)
      D2 = divisor({3/4, -8/5}, {ideal(y), ideal(x)}, CoeffType => QQ)
      D1 - D2
///

doc ///
	Key
	 (symbol -, BasicDiv)
	Headline
	  Negation of a divisor
	Usage
	  E = -D
	Inputs
	  D: BasicDiv
	Outputs
	  E: BasicDiv
	Description
	 Text
	  Given a divisor, we return a divisor whose coefficients are the negative of the given one
	 Example
	  R = QQ[x, y, z] / ideal(x * y - z^2 )
	  D1 = divisor({1, 2}, {ideal(x, z), ideal(y, z)})
	  -D1
///

doc ///
 	Key
 	 divisorToModule
 	 (divisorToModule, WDiv)
 	 (divisorToModule, QDiv)
 	 (divisorToModule, RDiv)
 	Headline
 	 Calculate the corresponding module of a given divisor
 	Usage
 	 M1 = divisorToModule( D1 )
 	 M2 = divisorToModule( D2 )
 	 M3 = divisorToModule( D3 )
 	Inputs
 	 D1: WDiv
 	 D2: QDiv
 	 D3: RDiv
 	Outputs
 	 M1: Module
 	 M2: Module
 	 M3: Module
 	Description
 	 Text
 	  Get the associated module O(D) of a given Weil Divisor D.
 	 Example
 	  R = QQ[x, y, u, v] / ideal(x * y - u * v)
 	  D1 = divisor({1, -2, 3, -4}, {ideal(x, u), ideal(x, v), ideal(y, u), ideal(y, v)})
 	  divisorToModule( D1 )
 	 Text
 	  To get the associated module O(D) for a rational/real divisor D, we first obtain a new divisor D' whose coefficients are the floor of the coefficients of D, and take O(D') as O(D)
 	 Example
 	  R = QQ[x, y, u, v] / ideal(x * y - u * v)
 	  D2 = divisor({3/5, -4/7, 9/4, -7/8}, {ideal(x, u), ideal(x, v), ideal(y, u), ideal(y, v)}, CoeffType=>QQ)
 	  divisorToModule( D2 )
 	SeeAlso
 	 divisorToIdeal
///

doc ///
 	Key
 	 idealPower
 	 (idealPower, ZZ, Ideal)
 	Headline
 	 Compute the ideal generated by the generators of the given ideal raised to a power
 	Usage
 	 J = idealPower(n, I)
 	Inputs
 	 n: ZZ
 	 I: Ideal
 	Outputs
 	 J: Ideal
 	Description
 	 Text
 	  If I is generated by (x1, ..., xk) then idealPower(n, I) is the ideal generated by (x1^n, ..., xk^n).  This is relevant because idealPower(n, I) and I^n have the same reflexification, but idealPower(n, I) can be much faster to compute with since it has fewer generators typically.
 	 Example
 	  R = QQ[x, y, u, v] / ideal(x * y - u * v)
 	  I = ideal(x, u)
 	  idealPower(5, I)
 	  I^5
 	SeeAlso
 	 reflexifyIdeal
///

doc ///
 	Key
 	 divisorToIdeal
 	 (divisorToIdeal, WDiv)
 	 (divisorToIdeal, QDiv)
 	 (divisorToIdeal, RDiv)
 	Headline
 	 Calculate the corresponding module of a given divisor and represent it as an ideal
 	Usage
 	 M1 = divisorToIdeal( D1 )
 	 M2 = divisorToIdeal( D2 )
 	 M3 = divisorToIdeal( D3 )
 	Inputs
 	 D1: WDiv
 	 D2: QDiv
 	 D3: RDiv
 	Outputs
 	 M1: Module
 	 M2: Module
 	 M3: Module
 	Description
 	 Text
 	  Get the associated module O(D) of a given Weil Divisor D.  Then express it as an ideal.
 	 Example
 	  R = QQ[x, y, u, v] / ideal(x * y - u * v)
 	  D1 = divisor({1, -2, 3, -4}, {ideal(x, u), ideal(x, v), ideal(y, u), ideal(y, v)})
 	  divisorToIdeal( D1 )
 	SeeAlso
 	 divisorToModule
///

doc ///
	Key
	 mapToProjectiveSpace
	 (mapToProjectiveSpace, WDiv)
	 [mapToProjectiveSpace, KnownCartier]
	Headline
	 Compute the map to projective space associated with the global sections of a Cartier divisor
	Usage
	 h = mapToProjectiveSpace( D, KnownNormal=>b)
	Inputs
	 D: WDiv
	 b: Boolean
	Outputs
	 h: RingMap
	Description
	 Text
	  Given a Cartier divisor D on a projective variety (represented by a divisor on a normal graded ring), this function returns the map to projective space induced by the global sections of O(D).  If KnownCartier is set to false (default is true), the function will also check to make sure the divisor is Cartier away from the irrelevant ideal.
	 Example
	  R = QQ[x,y,u,v]/ideal(x*y-u*v)
	  D = divisor( ideal(x, u) )
	  mapToProjectiveSpace(D)
	 Example 
	  R = ZZ/7[x,y,z]
	  D = divisor(x*y)
	  mapToProjectiveSpace(D)	  
	SeeAlso
	 isCartier
///

doc ///
	Key
	 baseLocus
	 (baseLocus, WDiv)
	 (baseLocus, Module)
	Headline
	 Computes the locus where a graded module (or O(D) Weil divisor) is not globally generated.
	Usage
	 I = baseLocus(D)
	 I = baseLocus(M)
	Inputs
	 D: WDiv
	 M: Module
	Outputs
	 I: Ideal
	Description
	 Text
	  Given a module M with global sections s1, ..., sd, this computes the locus where the is do not generate M.  Given a Weil divisor D, this computes the base locus of O(D).  For example, consider the rulings on P^1 cross P^1.
	 Example
	  R = QQ[x,y,u,v]/ideal(x*y-u*v);
	  D = divisor( ideal(x,u) )
	  baseLocus(D)
	 Text
	  Or a point on an eliptic curve
	 Example
	  R = QQ[x,y,z]/ideal(y^2*z-x*(x+z)*(x-z));
	  D = divisor(ideal(y, x))
	  baseLocus(D)
	  baseLocus(2*D)
///


doc ///
	Key
	 reflexifyIdeal
	 (reflexifyIdeal, Ideal)
	 [reflexifyIdeal, KnownNormal]
	Headline
	 Calculate the double dual of an ideal
	Usage
	 I2 = reflexifyIdeal( I1, KnownNormal=>b)
	Inputs
	 I1: Ideal
	 b: Boolean
	Outputs
	 I2: Ideal
	Description
	 Text
	  Get the double dual (S2 - identification) of an ideal.  If KnownNormal is false (default value is true), then the function will check whether it is normal, if not it will compute using a slower method which will give the correct answer.
	 Example
	  R = QQ[x,y,z]/ideal(x^2-y*z)
	  m = ideal(x,y,z)
	  reflexifyIdeal(m)
	  I = ideal(x,y)
	  reflexifyIdeal(I)
	  reflexifyIdeal(I^2)
	  reflexifyIdeal(I^3)
	SeeAlso
	 reflexifyModule
	 isReflexive
	 idealToDivisor
///

doc ///
	Key
	 dualizeIdeal
	 (dualizeIdeal, Ideal)
	 [dualizeIdeal, KnownNormal]
	Headline
	 Finds an ideal isomorphic to Hom(I, R)
	Usage
	 I2 = dualizeIdeal( I1, KnownNormal=>b)
	Inputs
	 I1: Ideal
	 b: Boolean
	Outputs
	 I2: Ideal
	Description
	 Text
	  Get the double dual (S2 - identification) of an ideal.  If KnownNormal is false (default is true), then the computer will first check whether the ambient ring is normal, if it is not then it will perform a (possibly) slower check that will definitely give the right answer.
	 Example
	  R = QQ[x,y,z]/ideal(x^2-y*z)
	  m = ideal(x,y,z)
	  dualizeIdeal(m)
	  I = ideal(x,y)
	  dualizeIdeal(I)
	  dualizeIdeal(I^2)
	  dualizeIdeal(I^3)
	SeeAlso
	 reflexifyIdeal
	 reflexifyModule
	 isReflexive
	 idealToDivisor
///

doc ///
	Key
	 reflexivePower
	 (reflexivePower, ZZ, Ideal)
	Headline
	 Computes a reflexive power of an ideal
	Usage
	 I2 = reflexivePower( n1, I1 )
	Inputs
	 I1: Ideal
	 n1: ZZ
	Outputs
	 I2: Ideal
	Description
	 Text
	  Returns the n1'th reflexive power of I1.
	 Example
	  R = QQ[x,y,z]/ideal(x^2-y*z)
	  J = ideal(x,y)
	  reflexivePower(5, J)
	  reflexivePower(6, J)
	SeeAlso
	 reflexifyIdeal
	 isReflexive
///


doc ///
	Key
	 KnownNormal
	Headline
	 An option used to specify to certain functions that we know that the ambient ring is normal.
	Description
	 Text
	  If true, then some functions will not check whether or not an ambient ring is normal, they will assume it and proceed.
///

doc ///
	Key
	 KnownCartier
	Headline
	 An option used to specify to certain functions that we know that the divisor is Cartier.
	Description
	 Text
	  If true, then some functions will not check whether or not the divisor is Cartier, they will assume it is and proceed.
///

doc ///
	Key
	 Primes
	Headline
	 A value for the option Strategy for the divPullBack method
	Description
	 Text
	  If Strategy=>Primes then the divPullBack method will pull back each prime individually.
	SeeAlso
	 divPullBack
	 Sheaves
///

doc ///
	Key
	 Sheaves
	Headline
	 A value for the option Strategy for the divPullBack method
	Description
	 Text
	  If Strategy=>Sheaves then the divPullBack method will pull back the sheaf O(D).
	SeeAlso
	 divPullBack
	 Primes
///

doc ///
	Key
	 ReturnMap
	Headline
	 An option for moduleToIdeal and moduleWithSectionToIdeal
	Description
	 Text
	  If moduleToIdeal is set to true, then instead of just converting a module into an isomorphic ideal, this also returns the map from the module to the ring.
	SeeAlso
	 moduleToIdeal
	 moduleWithSectionToIdeal
///

doc ///
 	Key 
 	 reflexifyModule
 	 (reflexifyModule, Module)
 	Headline
 	  Calculate the double dual of a module 
 	Usage
 	 M2 = reflexifyModule( M1 )
 	Inputs
 	 M1: Module
 	Outputs
 	 M2: Module
 	Description
 	 Text
 	  Get the double-dual (S2 - identification) of a module
 	 Example
 	  R = QQ[x,y,z]/ideal(x^2-y*z)
	  m = ideal(x,y,z)
	  prune reflexifyModule(m*R^2)
	  I = ideal(x,y)
	  prune reflexifyModule(I*R^1)
	  prune reflexifyModule(I^2*R^1)
 	SeeAlso
 	 reflexifyIdeal
 	 reflexifyModuleWithMap
 	 isReflexive
///

doc ///
 	Key 
 	 isDomain
 	 (isDomain, Ring)
 	Headline
 	  Checks if a ring is a domain
 	Usage
 	 b = isDomain(R)
 	Inputs
 	 R: Ring
 	Outputs
 	 b: Boolean
 	Description
 	 Text
 	  Returns true if R is an integral domain, otherwise it returns false
 	 Example
 	  R = QQ[x,y,z]/ideal(x^2-y*z)
	  isDomain(R)
	  S = ZZ/5[x,y]/ideal(x^2*y^3)
	  isDomain(S)
///


doc ///
 	Key 
 	 reflexifyModuleWithMap
 	 (reflexifyModuleWithMap, Module)
 	Headline
 	  Compute the canonical map from a module to its double-dual
 	Usage
 	 M2 = reflexifyModuleWithMap( M1 )
 	Inputs
 	 M1: Module
 	Outputs
 	 M2: Module
 	Description
 	 Text
 	  Get the double-dual (S2 - identification) M1^** of a module M1 and return the canonical map M1 -> M1^**
 	 Example
 	  R = QQ[x,y]
 	  m = ideal(x,y)
 	  M = m*R^1
 	  f = reflexifyModuleWithMap( M )
 	  source f
 	  target f
 	SeeAlso
 	 reflexifyIdeal
 	 reflexifyModule
 	 isReflexive
///

doc ///
	Key
	 isReflexive
	 (isReflexive, Ideal)
	 (isReflexive, Module)
	Headline
	 Checks whether an ideal or module is reflexive
	Usage
	 b = isReflexive( I1 )
	 b = isReflexive( M1 )
	Inputs
	 I1: Ideal
	 M1: Module
	Outputs
	 b: Boolean
	Description
	 Text
	  Returns true if the module or ideal is reflexive, otherwise it returns false.  
	 Example
	  R = QQ[x,y,z]/ideal(x^2-y*z)
	  m = ideal(x,y,z)
	  isReflexive(m)
	  I = ideal(x,y)
	  isReflexive(I)
	SeeAlso
	 reflexifyModule
	 reflexifyIdeal
///


doc ///
 	Key 
 	 torsionSubmodule
 	 (torsionSubmodule, Module)
 	Headline
 	  Finds the torsion submodule of a given module
 	Usage
 	 M2 = torsionSubmodule( M1 )
 	Inputs
 	 M1: Module
 	Outputs
 	 M2: Module
 	Description
 	 Text
 	  Finds the torsion submodule of a given module.  It does this by computing the kernel of the map from M1 to its double dual
 	 Example
 	  R = QQ[x,y]
 	  m = ideal(x,y)
 	  M = (R^1/m) ++ R^1
 	  prune torsionSubmodule M
 	SeeAlso
 	 reflexifyModuleWithMap
///

doc ///
  	Key
  	 idealToDivisor
  	 (idealToDivisor, Ideal)
  	Headline
  	 Calculate the divisor D so that O_X(D) = I
  	Usage
  	 D = idealToDivisor( I )
  	Inputs
  	 I: Ideal
  	Outputs
  	 D: WDiv
  	Description
  	 Text
  	  Finds a divisor D such that O(D) is equal to the reflexification of I.  Note such a D will always be anti-effective.
  	 Example
  	  R = ZZ/7[x,y,z]/ideal(x^3+y^3+z^3)
  	  idealToDivisor( ideal(x) )
  	  idealToDivisor( ideal(x, y+z) )
  	SeeAlso
  	 (divisor, Ideal)
  	 moduleToDivisor
///

doc ///
  	Key
  	 idealWithSectionToDivisor
  	 (idealWithSectionToDivisor, RingElement, Ideal)
  	Headline
  	 Calculate the divisor D so that D corresponds to the section f of I
  	Usage
  	 D = idealWithSectionToDivisor( f, I )
  	Inputs
  	 f: RingElement 
  	 I: Ideal
  	Outputs
  	 D: WDiv
  	Description
  	 Text
  	  Finds the effective divisor D such that O(D) is isomorphic to I and so that f in I corresponds to 1 in O(D).
  	 Example
  	  R = ZZ/7[x]
  	  idealWithSectionToDivisor(x^2, ideal(x))
  	SeeAlso
  	 (divisor, Ideal)
  	 moduleToDivisor
///

doc ///
 	Key
 	 moduleToIdeal
 	 (moduleToIdeal, Ring, Module)
 	 (moduleToIdeal, Module)
 	 [moduleToIdeal, MTries]
 	 [moduleToIdeal, IsGraded]
 	 [moduleToIdeal, ReturnMap]
 	Headline
 	 Turn a module to an ideal of a ring
 	Usage
 	 I = moduleToIdeal(R, M, MTries=>n, IsGraded=>false)
 	 L = moduleToIdeal(R, M, MTries=>n, IsGraded=>true)
 	 L = moduleToIdeal(R, M, MTries=>n, ReturnMap=>true)
 	 L = moduleToIdeal(R, M, MTries=>n, ReturnMap=>true, IsGraded=>true)
 	Inputs
 	 M: Module
 	 R: Ring
 	 n: ZZ
 	Outputs
 	 I: Ideal
 	 L: List
 	Description
 	 Text
 	  Tries to embed the module as an ideal in R.  It will make several automatic tries followed by MTries=>n (the default n value is 10).  If IsGraded is set to true, then it returns a list, the first entry denoting the ideal, the second denoting the degree shift (the default value for the option IsGraded is false).  Parts of this function were based on code originally written in the Macaulay2 Divisor tutorial and also based on code by Mordechai Katzman, see the canonicalIdeal function in http://katzman.staff.shef.ac.uk/FSplitting/ParameterTestIdeals.m2
 	 Example
 	  R = QQ[x,y]
 	  M = (ideal(x^2,x*y))*R^1
 	  moduleToIdeal(M)
 	 Text
 	  It also works for non-domains
 	 Example
 	  R = QQ[x,y]/ideal(x*y);
 	  M = (ideal(x^3, y^5))*R^1;
 	  moduleToIdeal(M)
 	  N = (ideal(x,y))*R^1;
 	  moduleToIdeal(N)
 	 Text
 	  Note that the answer is right even if you don't recognize it at first.  Next, consider the IsGraded option. If this is set to true, then the system returns the degree as well (as you can see in the example below).
 	 Example
 	  R = QQ[x,y];
 	  M = R^{-3};
 	  moduleToIdeal(M, IsGraded=>true)
 	 Text
 	  In conclusion, we consider the ReturnMap option.  What this does is also return the map from M to R^1 of which the map is based upon.  Note that if both IsGraded and ReturnMap are enabled, then the map comes after the degree.
 	 Example
 	  R = QQ[x,y];
 	  M = ideal(x^2, x*y)*R^1;
 	  L = moduleToIdeal(M, ReturnMap=>true)
 	  target L#1
 	  source L#1
 	SeeAlso
 	 moduleToDivisor
 	 moduleWithSectionToIdeal
///

doc ///
 	Key
 	 moduleWithSectionToIdeal
 	 (moduleWithSectionToIdeal, Matrix)
 	 [moduleWithSectionToIdeal, MTries]
 	 [moduleWithSectionToIdeal, ReturnMap]
 	Headline
 	 Turn a module to an ideal of a ring and keep track of a module element
 	Usage
 	 L = moduleWithSectionToIdeal(mat, MTries=>n, ReturnMap=>false)
 	Inputs
 	 mat: Matrix
 	 R: Ring
 	 n: ZZ
 	Outputs
 	 L: List
 	Description
 	 Text
 	  Tries to embed the target of the module map as an ideal in R, it will also return the image of 1 under the module map.  These are returned as a list, the element first, and then the ideal.  It uses MTries=>n (the default n value is 10) in the same way as moduleToIdeal.  
 	 Example
 	  R = QQ[x,y];
 	  M = (ideal(x^2,x*y))*R^1;
 	  mat = map(M, R^1, {{1}, {1}});
 	  moduleWithSectionToIdeal(mat)
 	 Text
 	  Like moduleToIdeal, if ReturnMap is set to true, then the method will also return the map from M to R^1.
 	 Example
 	  R = QQ[x,y];
 	  M = (ideal(x^2,x*y))*R^1;
 	  mat = map(M, R^1, {{1}, {1}});
 	  L = moduleWithSectionToIdeal(mat, ReturnMap=>true)
 	  target L#2
 	  source L#2
 	SeeAlso
 	 moduleToIdeal
///

doc ///
	Key
	 moduleToDivisor
	 (moduleToDivisor, Ring, Module)
	 (moduleToDivisor, Module)
	 [moduleToDivisor, IsGraded]
	Headline
	 Compute a divisor associated to a module in a ring
	Usage
	 D = moduleToDivisor(R, M, IsGraded=>b)
	 D = moduleToDivisor(M, IsGraded=>b)
	Inputs
	 M: Module
	 R: Ring
	 b: Boolean
	Outputs
	 D: WDiv
	Description 
	 Text
	  Given a rank 1 reflexive module M, this finds a divisor D such that O(D) is isomorphic to M.  If IsGraded is true (it is false by default) this assumes we are working on the Proj of the ambient ring.
	 Example
	  R = QQ[x,y,z]/ideal(x^2-y*z)
	  M = (ideal(y*x,y*z))*R^1
	  moduleToDivisor(M)
	  moduleToDivisor(M, IsGraded=>true)
	SeeAlso
	 moduleToIdeal
	 idealToDivisor
	 moduleWithSectionToDivisor
///

doc ///
	Key
	 moduleWithSectionToDivisor
	 (moduleWithSectionToDivisor, Matrix)
	Headline
	 Compute the effective divisor associated to the section of a module
	Usage
	 D = moduleToDivisor(f1)
	Inputs
	 f1: Matrix
	Outputs
	 D: WDiv
	Description 
	 Text
	  Given map from a rank 1 free module to a rank 1 reflexive module M, this finds the unique divisor D corresponding to the section.
	  In the example below, we consider the divisor corresponding to the inclusion x*R^1 -> (x,y)*R^1
	 Example
	  R = QQ[x,y,z]/ideal(x^2-y*z)
	  M = (ideal(x,y))*R^1
	  mat = map(M, R^1, {{1},{0}})
	  moduleToDivisor(M)
	SeeAlso
	 moduleToIdeal
	 moduleToDivisor
	 idealWithSectionToDivisor
///


doc ///
	Key
	 divPullBack
	 (divPullBack, RingMap, RDiv)
	 [divPullBack, Strategy]
	Headline
	 Compute the pullback of a divisor under a ring map
	Usage
	 D2 = divPullBack(f, D1, Strategy=>b)
	Inputs
	 f: RingMap
	 D1: RDiv
	 b: Symbol
	Outputs
	 D2: RDiv
	Description
	 Text
	  This function computes the pullback of a divisor under a ring map.  There are two potential strategies, Primes and Sheaves (Primes is the default strategy).  The Primes strategy pulls back each prime individually.  It can be faster but it only works for ring maps that are either finite or flat (unless each prime is also Cartier).  For more general maps, it can give incorrect results.  The other option for Strategy is Sheaves.  .  This can be slower especially for divisors with large coefficients, but it will successfully pull back any Cartier divisor.  Sheaves also requires the divisor passed to be a WDiv.
	 Example
	  R = QQ[x,y,z,w]/ideal(z^2-y*w,y*z-x*w,y^2-x*z);
	  T = QQ[a,b];
	  f = map(T, R, {a^3, a^2*b, a*b^2, b^3});
 	  D = divisor(y*z)
	  divPullBack(f, D, Strategy=>Primes)
	  divPullBack(f, D, Strategy=>Sheaves)
	 Text
	  Let us also consider pulling back a divisor under a blowup map
	 Example
	  R = QQ[x,y];
	  S = QQ[a,b];
          f = map(S, R, {a*b, b});
          D = divisor(x*y*(x+y));
          D1 = divPullBack(f, D)
	SeeAlso
	 Primes
	 Sheaves
///

doc /// 
	Key
	 findElementOfDegree
	 (findElementOfDegree, ZZ, Ring)
	 (findElementOfDegree, BasicList, Ring)
	Headline
	 Find an element of a specified degree
	Usage
	 x = findElementOfDegree( n, R )
	 x = findElementOfDegree( l, R )
	Inputs
	 R: Ring
	 n: ZZ
	 l: BasicList
	Outputs
	 x: BasicList
	Description
	 Text
	  Given a singly graded ring and an integer n, this function tries to find an element of degree n.  If successful, it returns a list with two elements {a,b} such that a/b has degree n.  If it is impossible, it gives an error.  If instead of an integer, you pass it a basic list corresponding to a multi-degree, it still tries to find a, b in R such that a/b has degree n.  It only works on rings with flattened variables (ie, no Rees algebras for now).
	  First we do an example without multidegrees.
	 Example
	  R = ZZ/7[x,y,Degrees=>{3, 5}]
	  output = findElementOfDegree(1, R)
	  output#0/output#1
	  findElementOfDegree(-2, R)
	 Text
	  We also do an example with multidegrees
	 Example
	  R = QQ[x,y,Degrees=>{{1,2}, {3, 5}}]
	  output = findElementOfDegree({1, 3}, R)
	  output#0/output#1
	SeeAlso
	 [moduleToDivisor, IsGraded] 
	 getLinearDiophantineSolution
///

doc ///
	Key
	 getLinearDiophantineSolution
	 (getLinearDiophantineSolution, BasicList, BasicList)
	 (getLinearDiophantineSolution, BasicList, Matrix)
	 [getLinearDiophantineSolution, Unsafe]
	Headline
	 Find a solution of the linear Diophantine equation Ax = b
	Usage
	 x = getLinearDiophantineSolution(l, L)
	 x = getLinearDiophantineSolution(l, A)
	Inputs
	 L: BasicList
	 l: BasicList
	 A: Matrix
	Outputs
	 x: List
	Description
	 Text
	  Given a linear Diophantine equation Ax = b (i.e. an integer matrix with target vector also integer), we want to find a solution of this equation.
	 Example
	   colList = {{1,3,7}, {2,4,-31}, {1,6,101}, {3,-2,47}, {8,9,1}};
	   A = transpose matrix colList;
	   b = {1, 2, 3}
	   getLinearDiophantineSolution(b, A)
	   sol = getLinearDiophantineSolution(b, colList )
	   sum apply(#sol, i->(sol#i)*(colList#i) )
	 Text
	  When the context is clear, set the Unsafe option to true in order to avoid routine checks.
	 Example
		A = matrix{ {1, 0, 0, 0, 0}, {0, 2, 0, 0, 0}, {3, 4, 5, 6, 8} }
	   	b = {1, 2, 3}
	   	getLinearDiophantineSolution(b, A, Unsafe => true)
	SeeAlso
	 findElementOfDegree
///

doc ///
   	Key
   	 applyToCoefficients
   	 (applyToCoefficients, BasicDiv, Function)
   	 [applyToCoefficients, CoeffType]
   	 [applyToCoefficients, Unsafe]
   	Headline
   	 Applies a function to the coefficients of a divisor
   	Usage
   	 D = applyToCoefficients(D1, h, CoeffType=>nn, Unsafe=>b)
   	Inputs
   	 D1: BasicDiv
   	 h: Function
   	 nn: Number
   	 b: Boolean
   	Outputs
   	 D: WDiv
   	Description
   	 Text
	  applyToCoefficients applies the function h to the coefficients of the divisor of D.  Specifying the CoeffType=>ZZ, CoeffType=>QQ, CoeffType=>RR, will force the output divisor to be of a certain form (WDiv, QDiv, RDiv respectively), otherwise the class of the output D is the same as the class of the input D1 (WDiv, QDiv, RDiv, or BasicDiv).  If Unsafe is set to false (the default is true), then the function will check to make sure the output is really a valid divisor.  
	 Example
	  R = QQ[x, y, z];
	  D = divisor(x*y^2/z)
	  applyToCoefficients(D, z->5*z)
	SeeAlso
	 floorDiv
	 ceilingDiv
///



doc /// 
	Key
	 canonicalDivisor
	 (canonicalDivisor, Ring)
	 [canonicalDivisor, IsGraded]
	Headline
	 Compute the canonical divisor of a ring
	Usage
	 D = canonicalDivisor( R, IsGraded=>b )
	Inputs
	 R: Ring
	 b: Boolean
	Outputs
	 D: WDiv
	Description
	 Text
	  Compute the canonical divisor of a ring (warning, the canonical divisor is not unique, but only unique up to linear equivalence).  If the IsGraded option is set to true (default false), then it will return a canonical divisor for the Proj of R, otherwise it will return one for only the Spec.  The graded version only works reliably for graded rings over a field (for instance, if you have a Rees algebra you will need to flatten the variables).
	 Example
	  R = QQ[x,y,z]
	  canonicalDivisor(R)
	  canonicalDivisor(R, IsGraded=>true)
	 Text
	  Note the IsGraded option makes a difference.  Consider now a non-Gorenstein singularity.
	 Example
	  R = QQ[a,b,c,d]/ideal(c^2-b*d, b*c-a*d, b^2-a*c)
	  canonicalDivisor(R)
///

doc /// 
	Key
	 ramificationDivisor
	 (ramificationDivisor, RingMap)
	 [ramificationDivisor, IsGraded]
	Headline
	 Compute the ramification divisor of a finite inclusion of normal domains
	Usage
	 D = ramficationDivisor( f, IsGraded=>b)
	Inputs
	 f: RingMap
	 b: Boolean
	Outputs
	 D: WDiv
	Description
	 Text
	  Compute the ramification divisor corresponding the finite inclusion of normal domains.  If you pass it a non-finite map, it will compute the divisorial part of the locus where the map is not smooth.  If IsGraded is set to false (the default value), then the coefficient ring of both the source and target of f must be equal.  If the IsGraded is set to true, then the function will assume that the source of f is affine, and the target is projective over the source.  In this case, the coefficient ring of the target must be equal to the source ring.  This can be useful when computing things like relative canonical divisors over regular bases (it may not give the expected answer over non-regular bases).
	 Example
	  R = QQ[x];
	  S = QQ[y];
	  f = map(S, R, {y^3});
	  ramificationDivisor(f)
	 Text
	  The next example is a Veronese which is etale in codimension 1.
	 Example
	  R = QQ[x,y];
	  T = QQ[a,b,c,d];
	  h = map(R, T, {x^3, x^2*y, x*y^2, y^3});
	  S = T/ker h;
	  f = map(R, S, {x^3, x^2*y, x*y^2, y^3});
	  ramificationDivisor(f)
	 Text
	  Here is an example with wild ramification.
	 Example
	  R = ZZ/2[t];
	  S = ZZ/2[x];
	  f = map(S, R, {x^2*(1+x)});
	  ramificationDivisor(f)
	 Text
	  Next we demonstrate how to compute the relative canonical divisor of a blowup over a smooth base.
	 Example
	  R = QQ[x,y];
	  S = reesAlgebra(ideal(x,y^2));
	  f = map(S, R);
	  ramificationDivisor(f,IsGraded=>true)
///

doc ///
	Key
	 isWDiv
	 (isWDiv, RDiv)
	Headline
		Check if a rational/real divisor is a Weil divisor
	Usage
		b = isWDiv( D2 )
	Inputs
		D2: RDiv
	Outputs
		b: Boolean
	Description
	 Text
	  Check if a rational/real divisor is a Weil divisor
	 Example
	  R = QQ[x, y, z]
	  D1 = divisor({1/1, 2/2, -6/3}, {ideal(x), ideal(y), ideal(z)}, CoeffType=>QQ)
	  D2 = divisor({1/2, 3/4, 5/6}, {ideal(y), ideal(z), ideal(x)}, CoeffType=>QQ)
	  isWDiv( D1 )
	  isWDiv( D2 )
	SeeAlso
		toWDiv
///

doc ///
	Key
		isEffective
		(isEffective, BasicDiv)
	Headline
		Check if a divisor is effective
	Usage
		b = isEffective( D1 )
	Inputs
		D1: WDiv
	Outputs
		b: Boolean
	Description
	 Text
	  Returns true if the divisor is effective, otherwise it returns false
	 Example
	  R = ZZ/31[x, y, u, v] / ideal(x * y - u * v)
	  D1 = divisor({1, -2, 3, -4}, {ideal(x, u), ideal(x, v), ideal(y, u), ideal(y, v)})
	  D2 = divisor({1, 39, 5, 27}, {ideal(x, v), ideal(y, v), ideal(x, u), ideal(x, u)})
	  isEffective( D1 )
	  isEffective( D2 )
///

doc ///
	Key
	 isDivPrime
	 (isDivPrime, BasicDiv)
	Headline
	 Check if a divisor is prime
	Usage
		b = isDivPrime( D1 )
	Inputs
		D1: WDiv
	Outputs
		b: Boolean
	Description
	 Text
	  Returns true if the divisor is prime (with coefficient 1), otherwise it returns false
	 Example
	  R = QQ[x, y]
	  D1 = divisor(x^2 * y)
	  D2 = divisor(x^2)
	  D3 = divisor(y)
	  isDivPrime( D1 )
	  isDivPrime( D2 )
	  isDivPrime( D3 )
///

doc ///
	Key
	 isDivReduced
	 (isDivReduced, BasicDiv)
	Headline
	 Check if a divisor is reduced
	Usage
		b = isDivReduced( D1 )
	Inputs
		D1: WDiv
	Outputs
		b: Boolean
	Description
	 Text
	  Returns true if the divisor is reduced (all coefficients equal to 1), otherwise it returns false
	 Example
	  R = QQ[x, y, z]
	  D1 = divisor(x^2 * y^3 * z)
	  D2 = divisor(x * y * z)
	  isDivReduced( D1 )
	  isDivReduced( D2 )
///

doc ///
  	Key 
  	 isDivPrincipal
  	 (isDivPrincipal, WDiv)
  	 [isDivPrincipal, IsGraded]
  	Headline
  	 Check if a Weil divisor is globally principal
  	Usage
  	 flag = isDivPrincipal( D, IsGraded => b )
  	Inputs
  	 D: WDiv
	 b: Boolean
  	Outputs
  	 flag: Boolean
  	Description
  	 Text
  	  Returns true if the Weil divisor D is principal, otherwise false.  If IsGraded is set to true, then this checks whether the divisor corresponds to a principal divisor on the Proj of the ambient ring.  Note that this function may return a false negative if the defining equations of the divisor are not homogeneous (it warns the user if this occurs).
  	 Example
  	  R = QQ[x, y, z]
  	  D = divisor(x)
  	  isDivPrincipal(D, IsGraded => true)
  	 Text  
  	  By default, IsGraded is set to false.  Regardless of the format, the check is done by determining whether or not O(D) is free.  
  	 Example
  	  R = QQ[x, y, z]
  	  D = divisor(x)
  	  E = divisor(x^2 * y)
  	  isDivPrincipal( D )
  	  isDivPrincipal( E )
  	SeeAlso
  	 divisorToModule
///

doc ///
	Key
	 isCartier
	 (isCartier, WDiv)
	 [isCartier, IsGraded]
	Headline
	 Check if a Weil divisor is Cartier
	Usage
	 flag = isCartier( D, IsGraded => b )
	Inputs
	 D: WDiv
	 b: Boolean
	Outputs
	 flag: Boolean
	Description
	 Text
	  Check if a Weil divisor is Cartier.  For example, the following divisor is not Cartier
	 Example
	  R = QQ[x, y, u, v] / ideal(x * y - u * v)
	  D = divisor({2, -3}, {ideal(x, u), ideal(y, v)})
	  isCartier( D )
	 Text
	  Neither is this divisor.
	 Example
	  R = QQ[x, y, z] / ideal(x * y - z^2 )
	  D = divisor({1, 2}, {ideal(x, z), ideal(y, z)})
	  isCartier( D )
	 Text
	  Of course the next divisor is Cartier.
	 Example
	  R = QQ[x, y, z]
	  D = divisor({1, 2}, {ideal(x), ideal(y)})
	  isCartier( D )
	 Text
	  If IsGraded == true (it is false by default), this will check as if D is a divisor on the Proj of the ambient graded ring.
	 Example
	  R = QQ[x, y, u, v] / ideal(x * y - u * v)
	  D = divisor({2, -3}, {ideal(x, u), ideal(y, v)})
	  isCartier(D, IsGraded => true)
	 Example
	  R = QQ[x, y, z] / ideal(x * y - z^2)
	  D = divisor({1, 2}, {ideal(x, z), ideal(y, z)})
	  isCartier(D, IsGraded => true)
	SeeAlso
	 divisorToModule
	 isQCartier
///

doc ///
	Key
	 nonCartierLocus
	 (nonCartierLocus, WDiv)
	 [nonCartierLocus, IsGraded]
	Headline
	 Returns the non-Cartier locus of a Weil divisor
	Usage
	 J = nonCartierLocus( D, IsGraded=>b)
	Inputs
	 D: WDiv
	 b: Boolean
	Outputs
	 J: Ideal
	Description
	 Text
	  Returns an ideal which vanishes on the locus where D is not Cartier.  
	 Example
	  R = QQ[x, y, u, v]/ideal(x * y  - u * v)
	  D = divisor({1, -3, -5, 8}, {ideal(x, u), ideal(y, v), ideal(x, v), ideal(y, u)})
	  nonCartierLocus( D )
	 Text
	  If IsGraded=>true (by default it is false), it saturates with respect to the homogeneous maximal ideal.
	 Example
	  R = QQ[x, y, u, v]/ideal(x * y  - u * v)
	  D = divisor({1, -3, -5, 8}, {ideal(x, u), ideal(y, v), ideal(x, v), ideal(y, u)})
	  nonCartierLocus( D, IsGraded => true )
	SeeAlso
	 isCartier
	 isQCartier
///

doc ///
 	Key
 	 isLinearEquivalent
 	 (isLinearEquivalent, WDiv, WDiv)
 	 [isLinearEquivalent, IsGraded]
 	Headline
 	 Check if two Weil divisor are linearly equivalent
 	Usage
 	 flag = isLinearEquivalent(D1, D2, IsGraded=>b)
 	Inputs
 	 D1: WDiv
 	 D2: WDiv
 	 b: Boolean
 	Outputs
 	 flag: Boolean
 	Description
 	 Text
 	  Given two Weil divisors, this method checks if they are linearly equivalent or not.  
 	 Example
 	  R = QQ[x, y, z]/ ideal(x * y - z^2)
 	  D1 = divisor({3, 8}, {ideal(x, z), ideal(y, z)})
 	  D2 = divisor({8, 1}, {ideal(y, z), ideal(x, z)})
 	  isLinearEquivalent(D1, D2)
 	 Text
 	  If IsGraded is set to true (by default it is false), then it treats the divisors as divisors on the Proj of their ambient ring. 
 	 Example 
 	  R = QQ[x, y, z]/ ideal(x * y - z^2)
 	  D1 = divisor({3, 8}, {ideal(x, z), ideal(y, z)})
 	  D2 = divisor({8, 1}, {ideal(y, z), ideal(x, z)})
 	  isLinearEquivalent(D1, D2, IsGraded => true)
 	SeeAlso
 	 divisorToModule
 	 isQLinearEquivalent
///

doc ///
 	Key
 	 isQCartier
 	 (isQCartier, ZZ, WDiv)
 	 (isQCartier, ZZ, QDiv)
 	 [isQCartier, IsGraded]
 	Headline
 	 Check whether m times a divisor is Cartier for any m from 1 to a fixed positive integer n1. 
 	Usage
 	 b = isQCartier(n1, D1, IsGraded=>b)
 	 b = isQCartier(n1, D2, IsGraded=>b)
 	Inputs
 	 D1: WDiv
 	 D2: QDiv
 	 n1: ZZ
 	 b: Boolean
 	Outputs
 	 b: Boolean
 	Description
 	 Text
 	  Check whether m times a Weil or Q-divisor is Cartier for each m from 1 to a fixed positive integer n1 (if the divisor is a QDiv, it can search slightly higher than n1).  If m * D1 is Cartier, it returns m.  If it fails to find an m, it returns 0.  
 	 Example
 	  R = QQ[x, y, z] / ideal(x * y - z^2 )
 	  D1 = divisor({1, 2}, {ideal(x, z), ideal(y, z)})
 	  D2 = divisor({1/2, 3/4}, {ideal(y, z), ideal(x, z)}, CoeffType => QQ)
 	  isQCartier(10, D1)
 	  isQCartier(10, D2)
 	 Example
 	  R = QQ[x, y, u, v] / ideal(x * y - u * v)
 	  D1 = divisor({1, 2}, {ideal(x, u), ideal(y, v)})
 	  D2 = divisor({1/2, -3/4}, {ideal(y, u), ideal(x, v)}, CoeffType => QQ)
 	  isQCartier(10, D1)
 	  isQCartier(10, D2)
 	 Text
 	  If IsGraded is set to true (by default it is false), then it treats the divisor as a divisor on the Proj of their ambient ring.
 	 Example
 	  R = QQ[x, y, z] / ideal(x * y - z^2 )
 	  D1 = divisor({1, 2}, {ideal(x, z), ideal(y, z)})
 	  D2 = divisor({1/2, 3/4}, {ideal(y, z), ideal(x, z)}, CoeffType => QQ)
 	  isQCartier(10, D1, IsGraded => true)
 	  isQCartier(10, D2, IsGraded => true) 
 	SeeAlso
 	 isCartier
///

doc ///
   	Key
   	 isQLinearEquivalent
   	 (isQLinearEquivalent, QDiv, QDiv)
   	 [isQLinearEquivalent, IsGraded]
   	Headline
   	 Check if two rational divisors are linearly equivalent
   	Usage
   	 flag = isQLinearEquivalent(D1, D2, IsGraded=>b)
   	Inputs
   	 D1: QDiv
   	 D2: QDiv
   	 b: Boolean
   	Outputs
   	 flag: Boolean
   	Description
   	 Text
   	  Given two rational divisors, this method returns if they are Q-linearly equivalent.  Otherwise it returns false.  
   	 Example
   	  R = QQ[x, y, z] / ideal(x * y - z^2)
   	  D = divisor({1/2, 3/4}, {ideal(x, z), ideal(y, z)}, CoeffType => QQ)
   	  E = divisor({3/4, 5/2}, {ideal(y, z), ideal(x, z)}, CoeffType => QQ)
   	  isQLinearEquivalent(D, E)
   	 Text
   	  In the above ring, every pair of divisors is Q-linearly equivalent because the Weil divisor class group is isomorphic to Z/2.
   	 Text
   	  If IsGraded=>true (the default is false), then it treats the divisors as if they are divisors on the Proj of their ambient ring.
   	 Example
   	  R = QQ[x, y, z] / ideal(x * y - z^2)
   	  D = divisor({1/2, 3/4}, {ideal(x, z), ideal(y, z)}, CoeffType => QQ)
   	  E = divisor({3/4, 5/2}, {ideal(y, z), ideal(x, z)}, CoeffType => QQ)
   	  isQLinearEquivalent(D, E, IsGraded => true)
   	 Text
   	  This is a more restrictive condition, and now the two divisors are not Q-linearly equivalent as they have different degrees on the corresponding projective line.
   	SeeAlso
   	 divisorToModule
   	 isLinearEquivalent
///

doc ///
   	Key
   	 isDivGraded
   	 (isDivGraded, BasicDiv)
   	Headline
   	 Checks to see if the divisor is graded (homogeneous)
   	Usage
   	 flag = isDivGraded(D)
   	Inputs
   	 D: BasicDiv
   	Outputs
   	 flag: Boolean
   	Description
   	 Text
   	  Returns true if the divisor is graded (homogeneous), otherwise it returns false
   	 Example
   	  R = QQ[x, y, z]
   	  D = divisor({1, 2, 3}, {ideal(x * y - z^2), ideal(y * z - x^2), ideal(x * z - y^2)})
   	  isDivGraded( D )
   	 Example
   	  R = QQ[x, y, z]
   	  D = divisor({1, 2}, {ideal(x * y - z^2), ideal(y^2 - z^3)})
   	  isDivGraded( D )
///

doc /// 
   	Key
   	 isRegular
   	 (isRegular, Ideal)
   	 [isRegular, IsGraded]
   	Headline
   	 Checks to see if R mod the given ideal is regular
   	Usage
   	 flag = isRegular(I, IsGraded=>b)
   	Inputs
   	 I: Ideal
   	 b: Boolean
   	Outputs
   	 flag: Boolean
   	Description
   	 Text
   	  Returns true if R/I is regular where R is the ambient ring of I, otherwise it sets to false.  
   	 Example
   	  R = QQ[x, y, z]
   	  I = ideal(x * y - z^2 )
   	  isRegular( I )
   	 Example
   	  R = QQ[x, y, u, v]
   	  I = ideal(x * y - u * v)
   	  isRegular( I )
   	 Example
   	  R = QQ[x, y, z]
   	  J = ideal( x )
   	  isRegular( J )
   	 Text
   	  If IsGraded is set to true (default false) then it treats I as an ideal on Proj R.  In particular, singularities at the origin (corresponding to the irrelevant ideal) are ignored.
   	 Example
   	  R = QQ[x, y, z]
   	  I = ideal(x * y - z^2 )
   	  isRegular(I, IsGraded => true)
   	 Example
   	  R = QQ[x, y, u, v]
   	  I = ideal(x * y - u * v)
   	  isRegular(I, IsGraded => true)
///

doc ///
   	Key
   	 isSNC
   	 (isSNC, BasicDiv)
   	 [isSNC, IsGraded]
   	Headline
   	 Checks to see if the divisor is simple normal crossings
   	Usage
   	 flag = isSNC(D, IsGraded=>b)
   	Inputs
   	 D: BasicDiv
   	 b: Boolean
   	Outputs
   	 flag: Boolean
   	Description
   	 Text
   	  Returns true if the divisor is simple normal crossings, this includes checking that the ambient ring is regular.
   	 Example
   	  R = QQ[x, y, z] / ideal(x * y - z^2 )
   	  D = divisor({1, -2}, {ideal(x, z), ideal(y, z)})
   	  isSNC( D )
   	 Example 
   	  R = QQ[x, y]
   	  D = divisor(x*y*(x+y))
   	  isSNC( D )
   	 Example 
   	  R = QQ[x, y]
   	  D = divisor(x*y*(x+1))
   	  isSNC( D )
   	 Text 
   	  If IsGraded is set to true (default false), then the divisor is treated as if it is on the Proj of the ambient ring.  In particular, non-SNC behavior at the origin in ignored.  This can make it easier to be simple normal crossings.
   	 Example
   	  R = QQ[x, y, z] / ideal(x * y - z^2 )
   	  D = divisor({1, -2}, {ideal(x, z), ideal(y, z)})
   	  isSNC( D, IsGraded => true )
   	 Example
   	  R = QQ[x, y]
   	  D = divisor(x*y*(x+y))
   	  isSNC( D, IsGraded => true )
   	 Example
   	  R = QQ[x,y,z]
   	  D = divisor(x*y*(x+y))
   	  isSNC( D, IsGraded => true)
///

doc ///
   	Key
   	 isZeroDivisor
   	 (isZeroDivisor, BasicDiv)
   	Headline
   	 Checks to see if the divisor is the zero divisor
   	Usage
   	 flag = isZeroDiviosr(D)
   	Inputs
   	 D: BasicDiv
   	Outputs
   	 flag: Boolean
   	Description
   	 Text
   	  Returns true if the divisor is zero, otherwise it returns false.
   	 Example
   	  R = QQ[x, y, z]
   	  D = divisor({1, 2, -3, 4}, {ideal(x), ideal(y), ideal(z), ideal(y)}) 	 
   	  isZeroDivisor( D ) 	  
   	 Example
   	  R = QQ[x, y, z]
   	  E = divisor({1, 2, -3, 4, 5, -9, 13, 2, -15}, {ideal(x), ideal(x), ideal(x), ideal(y), ideal(y), ideal(y), ideal(z), ideal(z), ideal(z)})
   	  isZeroDivisor( E )  	 
///

doc ///
   	Key
   	 zeroDivisor
   	 (zeroDivisor, Ring)
   	Headline
   	 Constructs the zero Weil divisor for the given ring
   	Usage
   	 D = zeroDiviosr(R)
   	Inputs
   	 R: Ring
   	Outputs
   	 D: WDiv
   	Description
   	 Text
	  Constructs the zero Weil divisor for the given ring
	 Example
	  R = QQ[x, y, z] / ideal(x * y - z^2)
	  D = zeroDivisor( R )
///

TEST ///
---check constructors and verify equality of them. this also checks some comparison
R = QQ[x,y,z]/ideal(x^2-y*z);
D = divisor(x^2);
E = divisor({2,2}, {ideal(x,y), ideal(x,z)});
F = 2*divisor(ideal(x));
G = divisor({{2, ideal(x,z)}, {2, ideal(x,y)}});
assert( (D == E) and (E == F) and (D == F) and (D == G) and (E == G) and (F == G))
///

TEST ///
---check constructors and verify that they don't produce the same value with different inputs
R = QQ[x,y,z]/ideal(x^2-y*z);
D = divisor(x);
E = divisor({1,2}, {ideal(x,y), ideal(x,z)});
F = 2*divisor(ideal(x));
G = divisor({{3, ideal(x,z)}, {1, ideal(x,y)}});
assert( not ((D == E) or (E == F) or (D == F) or (D == G) or (E == G) or (F == G)) )
///

TEST ///
--more construction testing, and coeff testing
R = ZZ/5[x,y,z]/ideal(x^2-y*z);
D = divisor(y^5); 
assert( coeff(ideal(y,x), D) == 10)
///


TEST ///
---check a canonical divisor and verify it is Cartier
R = QQ[x,y,z]/ideal(x^2-y*z);
assert(isCartier(canonicalDivisor(R)) == true)
///

TEST ///
--- check a canonical divisor and verify it is not Cartier
R = QQ[a,b,c,d]/ideal(c^2-b*d, b*c-a*d, b^2-a*c);
assert((isCartier(canonicalDivisor(R)) == false) and (isQCartier(10, canonicalDivisor(R)) == 3))
///

TEST ///
--- verify idealToDivisor and coeff
R = QQ[x,y,z]/ideal(x^2-y*z);
D = idealToDivisor(ideal(y));
assert( coeff(ideal(x, y), D) == -2)
///

TEST ///
--- verify ramification divisor in a simple case
R = ZZ/5[x]
S = ZZ/5[y]
f = map(S, R, {y^3});
D = ramificationDivisor(f);
assert(D == divisor(y^2))
///

TEST ///
--- verify the ramificationDivisor in one case where both source and target are nonsmooth
T = QQ[x,y];
R = QQ[a,b,c];
g = map(T, R, {x^2,x*y,y^2});
R = R/ker g;
S = QQ[m,n,o,p,q]
h = map(T, S, {x^4,x^3*y,x^2*y^2,x*y^3,y^4});
S = S/ker h;
f = map(R, S, {a^2,a*b,b^2,b*c,c^2});
D = ramificationDivisor(f);
assert(isZeroDivisor(D))
///

TEST ///
---checking idealToDivisor
R = QQ[x,y,z]/ideal(x^2-y*z);
J = ideal(x,y,z);
assert(isZeroDivisor(idealToDivisor(ideal(x,y,z))))
///


TEST ///
---checking moduleToDivisor and isCartier
 R = QQ[x,y,z]/ideal(x^2-y*z);
 M = ideal(x^2,x*y,x*z)*R^1;
 D = moduleToDivisor(M);
 assert(isCartier(D))
///

TEST ///
--- checking moduleToDivisor and linearEquivalence
R = QQ[x,y,z]/ideal(x^2-y*z);
M = ideal(x, y)*R^1;
D = moduleToDivisor(M);
E = divisor({-1, 2}, {ideal(x,y), ideal(x, z)});
assert(isLinearEquivalent(D, E))
///

TEST ///--check sums a+b==b+a 
R = ZZ/7[x,y,u,v]/ideal(x*y-u*v);
D = divisor(x);
E = divisor({1,2}, {ideal(x,u), ideal(y,u)});
assert(D+E == E+D)
///

TEST ///--check sums 3*a==a+a+a but not 2*a = a+a+a 
R = ZZ/7[x,y,u,v]/ideal(x*y-u*v);
D = divisor(x);
assert((3*D == D + D+ D) and (not(2*D == D + D + D)))
///

TEST ///--check some conversion functions
R = ZZ/7[x,y,u,v]/ideal(x*y-u*v);
D = divisor(x*u);
E = (2/2)*D;
F = (1.0)*E;
G = toWDiv(E);
H = toWDiv(F);
assert((D == G) and (D == H) and (not(2*D == G)) )
///

TEST /// --test functoriality for a finite map
R = QQ[x,y,z,w]/ideal(z^2-y*w,y*z-x*w,y^2-x*z);
T = QQ[a,b];
h = map(T, R, {a^3, a^2*b, a*b^2, b^3}); --this is the natural inclusion map
D = divisor(y*z);
E = divisor(x*w);
H = 3*divisor(a*b);
assert( (divPullBack(h, D) == H) and (divPullBack(h, E) == H) and (divPullBack(h, zeroDivisor(R)) == zeroDivisor(T))  )
///

TEST /// --test functoriality for localization (ie, a flat but not finite map)
R = QQ[x,y];
S = QQ[a,b,c]/ideal(a*c-1);
h = map(S, R, {a,b});
D = divisor((x)*(y));
E = divisor(b);
assert (  (divPullBack(h, D) == E) and (not (divPullBack(h, D) == zeroDivisor(S)) )  )
///

TEST /// --test functoriality for the sheaf strategy for a blowup and check the isSNC function
R = QQ[x,y];
S = QQ[a,b];
h = map(S, R, {a*b, b});
D = divisor(x*y*(x+y));
E = divisor(y^3-x^2);
D1 = divPullBack(h, D);
E1 = divPullBack(h, E);
assert( (isSNC(D1) == true) and (isSNC(E1) == false) and (coeff(ideal(b), D1) == 3) and (coeff(ideal(b), E1) == 2) )
///

--we also do some tests with IsGraded=>true 
TEST ///
---checking and isCartier in the graded setting
 R = QQ[x,y,z]/ideal(x^2-y*z);
 D = divisor(ideal(x,z));
 assert(isCartier(D, IsGraded=>true) and (not isCartier(D, IsGraded=>false)) and (isQCartier(5, D, IsGraded=>true) == 1) and (isQCartier(5, D, IsGraded=>false) == 2) )
///

TEST ///
---checking and isCartier in the graded setting again
 R = QQ[x,y,u,v]/ideal(x*y-u*v);
 D = divisor(ideal(x,u));
 assert(isCartier(D, IsGraded=>true) and (not isCartier(D, IsGraded=>false)) and (isQCartier(5, D, IsGraded=>true) == 1) and (isQCartier(10, D, IsGraded=>false) == 0) )
///

TEST ///
---some linear equivalence tests
 R = QQ[x,y,z];
 K = canonicalDivisor(R, IsGraded=>true);
 Z = zeroDivisor(R);
 D = -divisor(x*y*z);
 assert(isLinearEquivalent(K, D, IsGraded=>true) and (not isLinearEquivalent(K, Z, IsGraded=>true)) and isLinearEquivalent(K, Z, IsGraded=>false) )
///

TEST ///
---some nonCartierLocus tests
R = QQ[x,y,z]/ideal(x^2-y*z);
m = ideal(x,y,z);
D = divisor(ideal(x,y));
assert( (radical(nonCartierLocus(D)) == m) and (nonCartierLocus(D, IsGraded=>true) == ideal(sub(1, R))) )
///

--Q linear equivalence
TEST ///
R = ZZ/7[x,y,z];
Z = zeroDivisor(R);
D = 1/3*divisor(x^3+y^3+z^3);
E = divisor(x+y+z);
assert( (isQLinearEquivalent(D, E, IsGraded=>true) == true) and (isQLinearEquivalent(Z, D, IsGraded=>true) == false) and (isQLinearEquivalent(D, Z, IsGraded=>false) == true) )
///

TEST /// --some random checks on a determinantal variety
R =  QQ[a,b,c,d,e,f]/ideal(a*d-b*c, a*f-b*e, c*f-d*e);
K1 = canonicalDivisor(R);
K2 = canonicalDivisor(R, IsGraded=>true);
Z = zeroDivisor(R);
assert( (isQCartier(10, K1) == 0) and (isLinearEquivalent(K1, K2) == true) and (isLinearEquivalent(Z, K1) == false) and (isQCartier(10, K2, IsGraded=>true) == 1) )
///

--simple normal crossings tests
TEST ///
R = QQ[x,y,z];
D = divisor(x*y*(x+1)*z*(z-1));
E = divisor(x*y*z);
F = divisor(x*y*z*(x+y+z));
G = divisor(x*y-z^2);
assert( (isSNC(D) == true) and (isSNC(E) == true) and (isSNC(F) == false) and (isSNC(G) == false) )
///

--various checks for the zero divisor (ie, make sure it behaves well and doesn't break various functions)
TEST ///
R = QQ[x,y,z];
D = 0*divisor(x);
E = zeroDivisor(R);
assert( (D == E) and (isCartier(D) == true) and (isQCartier(5, D) == 1) and (dim source mapToProjectiveSpace(D) == 1) and (isFreeModule divisorToModule(D) == true) and (isSNC(D) == true) and (D == floorDiv(D)) )
///

--checks for very ample divisors #1 (divisors on elliptic curves)
TEST ///
R = QQ[x,y,z]/ideal(x^3+y^3-z^3);
D = divisor(ideal(x, y-z));
assert( (isVeryAmple(0*D) == false) and (isVeryAmple(1*D) == false) and (isVeryAmple(2*D) == false) and (isVeryAmple(3*D) == true) )
///

--checks for very ample divisors #2 (divisors on P^1 x P^1)
TEST ///
R = QQ[x,y,u,v]/ideal(x*y-u*v);
D = divisor(ideal(x,u));
E = divisor(ideal(x, v));
assert( (isVeryAmple(D) == false) and (isVeryAmple(E) == false) and (isVeryAmple(D+E) == true) )
///
end

---***************************
---*******CHANGELOG***********
---***************************
--changes 0.1s
------Made isVeryAmple not crash if you passed it a divisor with an empty linear system

--changes 0.1s
------Added the command isVeryAmple

--changes 0.1r
------Added the command ramificationDivisor

--changes 0.1o
------Added quotes to the exports for compatibility with 1.8

--changes 0.1q
------modified moduleToIdeal to allow the user to also output the map via the ReturnMap flag.
------fixed a bug in moduleToIdeal which caused it to sometimes not produce the right output for domains.

--changes 0.1p
------added verifyDivisor
------added applyToCoefficients
------fixed some typos in documentation
------renamed module2Ideal to moduleToIdeal for consistency
------added additional testing (in particular checking things for the zero divisor)


--changes 0.1m
------added getPrimeDiviors
------renamed divAmbientRing to getAmbientRing for consistency
------substantial speed improvement for moduleToDivisor in the graded case via a change of moduleToIdeal (adding an IsGraded option)
------fixed bug in moduleToDivisor which would sometimes provide the wrong shift
------added mapToProjectiveSpace 
------made divisorToIdeal work slightly better for anti-effective divisors
------added a second algorithm to divPullBack which works for Cartier divisors even in the map is not flat or finite
------added getLinearDiophantineSolution which makes findElementOfDegree work in the multigraded setting


----FUTURE PLANS------
--speed up the divisor stuff by doing some simultaneous dimension computations for checking for the zero divisor (seems to be faster frequently)
--refine the ability to compute relative canonical divisors.  Right now it should handle things pretty well, but there are some ways it can be improved (can we do canonical bundle formulas for fibrations I wonder?)
--for not necessarily S2 graded rings, handle things (this should be pretty easy, it just takes some re-coding)
--add a very ample check
--can we check ampleness, is there a better way to do this than to check if some power is very ample?  Hm, how do we prove that something is *not* ample...
--can we check semi-ampleness, is there a better way to do this than to check if some power induces a base point free morphism?  Hm, how do we prove something is *not* semi-ample
--we ought to be able to do bigness by seeing if nD - (ample) has a section for large n (or at least give an affirmative answer)
--do lots of optimization when the IsGraded flag is true (I'm sure things can be speeded up)
--compare the current nonCartierLocus with the approach that David suggested, using minors of a presentation matrix, probably what's there now is faster but...
--making checking principalness and checking linearEquivalence work better for non-homogeneous rings and ideals.  Sometimes it can give false negatives now, although the user is warned about before a (false?) negative is provided, this might be unavoidable but maybe it can give fewer false negatives.
