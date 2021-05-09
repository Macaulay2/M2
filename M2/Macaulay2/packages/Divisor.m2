--this file is in the public domain

newPackage( "Divisor",
     Version => "0.3", 
     Date => "May 30th, 2018",
     Authors => {
	  {Name => "Karl Schwede", Email=> "kschwede@gmail.com", HomePage=> "http://www.math.utah.edu/~schwede"},
     	  {Name=> "Zhaoning Yang", Email=> "zyy5054@gmail.com", HomePage => "http://sites.psu.edu/zhaoningyang"}},
     Headline => "Weil divisors",
     Keywords => {"Commutative Algebra"},
     PackageImports => { "IntegralClosure", "RationalMaps" },
     Certification => {
	  "journal name" => "The Journal of Software for Algebra and Geometry",
	  "journal URI" => "http://j-sag.org/",
	  "article title" => "Divisor Package for Macaulay2",
	  "acceptance date" => "31 August 2018",
	  "published article URI" => "https://msp.org/jsag/2018/8-1/p09.xhtml",
	  "published article DOI" => "10.2140/jsag.2018.8.87",
	  "published code URI" => "https://msp.org/jsag/2018/8-1/jsag-v8-n1-x09-Divisor.m2",
	  "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/Divisor.m2",
	  "release at publication" => "0e40b423ff375d6eb0a98d6fbbe7be8b2db95a98",	    -- git commit number in hex
	  "version at publication" => "0.3",
	  "volume number" => "8",
	  "volume URI" => "https://msp.org/jsag/2018/8-1/"
	  }
     )
export{
    --objects
	"BasicDivisor",
	"WeilDivisor",
	"QWeilDivisor",
	"RWeilDivisor",
    --methods for defining divisors and related operations
	"divisor", --checks verified
	"zeroDivisor", --added checks
    --accessing data
	"primes", --added checks
	"getPrimeCount", --added checks
	"gbs", --added checks
	"cleanSupport", --added checks 
	"clearCache", --NEED TO CHECK, NEEDS DOCUMENTATION
	"getPrimeDivisors", --added checks
     --simple operations
	"positivePart", --added checks
	"negativePart", --added checks
	"applyToCoefficients", --added checks
    --conversion
	"toWeilDivisor",  --added checks
	"toQWeilDivisor", --added checks
	"toRWeilDivisor", --added checks
    --divisors to modules and functorial properties
	"pullback", --added checks
	"findElementOfDegree", --added checks
	"getLinearDiophantineSolution",		--added checks --has Safe option
	"canonicalDivisor", --added checks --has IsGraded option
	"ramificationDivisor", --added checks
    --tests and related constructions
    --ideal, --cached
    --OO, --cached
    "isWeilDivisor", --added checks
	"isEffective", --added checks
	"isPrincipal", --added checks, has IsGraded option, cached
    "isReduced", --added checks
    "isCartier", --added checks, has IsGraded option, cached
    "isLinearEquivalent", --added checks has IsGraded option
    "isQCartier", --added checks, has IsGraded option, cached
    "isQLinearEquivalent", --added checks, has IsGraded option
    "nonCartierLocus", --added checks, has IsGraded option, cached
    "isSNC", --added checks, has IsGraded option, cached
    "isZeroDivisor", --added checks
    "isVeryAmple", --added checks, 
    --functions for getting maps to projective space from divisors (graded only)
	"baseLocus", --added checks
	"mapToProjectiveSpace", --added checks
    --general useful functions not directly related to divisors
    "idealPower", --added checks
    "reflexify", --added checks
	"isReflexive", --added checks
	"reflexivePower", --added checks
	"torsionSubmodule", --added checks
	"dualize", --added checks
	"embedAsIdeal", --added checks, has IsGraded option
	"isDomain", --added checks
	"isSmooth", --added checks, has IsGraded option
    --options
    "Safe", --an option, if set true then the above commands avoid doing any checks
	"CoefficientType", --an option, one can set the coefficient type
	"AmbientRing", --an option, one can specify the ambient ring during divisor construction
    "MTries", --an option, used to try to embed a module into a ring as an ideal in a random way
    --"keyPlus",
	"KnownCartier", --an option, used to specify that the divisor is known to be Cartier
	"KnownDomain", --an option, used to specify that the ring is known to be a integral domain
	"IsGraded", --an option, if you specify it in several arguments it assumes we are working on a projective variety
	"ReturnMap", --an option, for embedAsIdeal which returns the map from the module to R^1
	"Primes", --a potential value for the pullback Strategy option
	"Sheaves", --a potential value for the pullback Strategy option	
	"ModuleStrategy", --a strategy option for dualizing & reflexifying
	"IdealStrategy", --a strategy option for dualizing & reflexifying
	"NoStrategy", -- no strategy specified
--	"ReturnMap" --an option used to return the map instead of just a module
	"Section", --an option for specifying a section when find a divisor corresponding to a module
    --hashtable keys
    "ideals"

}

----------------------------------------------------------------
--************************************************************--
--Structure of our divisor objects and their display------------
--************************************************************--
----------------------------------------------------------------

----------------------------------------------------------------
---The divisor object is a hashtable
---Some keys are groebner bases of height 1 prime ideals
-----These evaluate to a list with the coefficient of the ideal.
-----It is ASSUMED that the only keys which evaluate to basic lists are gbs
---
---Another key is the ambient ring
---
---Another key is the cache
-----The cache always has a key (symbol ideals)
-------This key evaluates to a HashTable, the keys of which are Groebner bases, 
-------the values are the ideals the user entered
-----Other common cache values include:

	
BasicDivisor = new Type of HashTable;
RWeilDivisor = new Type of BasicDivisor;
QWeilDivisor = new Type of RWeilDivisor;
WeilDivisor = new Type of QWeilDivisor;

--Divisors have keys equal to Grobner bases of prime height-1 ideals.  They have values which are pairs.  The first entry in the value is the coefficient, the second is an ideal which hopefully the user will recognize.

--we can also control how they display
--they should display as something like
--**************************************
-- 5*Div(x,y) + -2*Div(y,z) of QQ[x,y,z]/(y^2-x*z)
--*************************************

---TODO: Change if we change internal structure
net BasicDivisor := t -> (
	valList := coefficients(t);
	primeList := primes(t);
	myStr := "";
	i := 0;
	j := 0;
	genList := {};
	if (#valList > 0) then (
		while (i < #valList) do(
			if (i > 0) then myStr = myStr | " + ";
			if (valList#i == 1) then (
			    myStr = myStr | "Div(";
			)
			else if (valList#i == -1) then (
			    myStr = myStr | "-Div(";
			)
			else (			
			    myStr = myStr | toString(valList#i) | "*Div(";
			);
			
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
	myStr
)

BasicDivisor#{Standard,AfterPrint} = BasicDivisor#{Standard,AfterNoPrint} = (D) -> (
     << endl;				  -- double space
     << concatenate(interpreterDepth:"o") << lineNumber << " : " << toString(class D) << " on " << toString(ring(D)) << endl;
     )


----------------------------------------------------------------
--************************************************************--
--Divisor construction -----------------------------------------
--************************************************************--
----------------------------------------------------------------



--the following is an internal function for divisors, it's basically the collision function
keyPlus = (l1, l2) ->( --there can be two kinds of inputs, ordinary divisor pairs and ambient rings, 
                        --the cache is thrown away
                        --TODO change if we change internal structure
	if ((instance(l1, BasicList) ) and (instance(l2, BasicList))) then (
		return {l1#0 + l2#0} --add the coefficients
	)
	else if ((instance(l1, Ring) ) and (instance(l2, Ring))) then (
		return l1 --keep the ring
	)
	else if ((instance(l1, CacheTable) ) and (instance(l2, CacheTable) )) then (
	    return mergeDivisorCache(l1, l2);
	);
	null
)

mergeDivisorCache = (c1, c2) -> (--this takes two caches of a divisor, and merges them
    C1 := new HashTable from c1#(symbol ideals);
    C2 := new HashTable from c2#(symbol ideals);
    L := {symbol ideals => new MutableHashTable from merge(C1, C2, (t1, t2) -> t1) };
--    print L;
--    1/0;
    return new CacheTable from L;
)



--the following is the basic construction function for the divisor
--it is passed a list of coefficients and a list of prime height one ideals
--several options are available

divisor = method(Options => {CoefficientType => ZZ, AmbientRing => null, Section => null, IsGraded => false});  

divisor(BasicList, BasicList) := BasicDivisor => o ->(l1, l2) -> 
(
	divList := new List from {}; 		--the Weil Divisor
	coeffList := l1; 					--list of coefficient
	idealList := l2; 				--list of height one prime ideals	
	N := #coeffList;
	RTest := o.AmbientRing;
	
	--initial specification of the ambient ring
	--and some basic checks
	if (N > 0) then ( --if there are ideals to compare
		RTest = ring ( idealList#0 );
		if (( not (o.AmbientRing === null)) and (not (RTest === o.AmbientRing) ) ) then (
			error "divisor: Specified ambient ring does not match the ideals given."; 
		);
	)
	else ( --otherwise use the users ambient ring
		RTest = o.AmbientRing; 
		if (RTest === null) then RTest = ZZ; --or specify ZZ if the user didn't use one
	);
	
	--next we check to make sure the user passed something reasonable (ie, the lists have the same length)
	M := #idealList;
	if (N != M) then
	(
		error "divisor: lists should have the same length";
	);
		
	--check that all the ideals are really ideals
	if ( not all(idealList, z->instance(z, Ideal)) ) then (error "divisor: all ideals should actually be ideals"; );
				


	divList = toList(apply(0..(N-1), i -> ( (first entries gens (gb idealList#i) ) => {coeffList#i} ) ));
	divList2 := toList(apply(0..(N-1), i -> ( (first entries gens (gb idealList#i) ) => idealList#i ) ));
	idealHash := new MutableHashTable from divList2;
	divList = append(divList, symbol ring => RTest);
	divList = append(divList, symbol cache => new CacheTable from {symbol ideals => idealHash}); --the list of ideal descriptions is stored in the cache table
        		--print hashTable(divList);
		--if we have a common coefficient ring type
		--TODO change if we change internal structure
	if (o.CoefficientType === ZZ) then 
		new WeilDivisor from ( hashTable(keyPlus, divList) ) 
	else if (o.CoefficientType === QQ) then
		new QWeilDivisor from ( hashTable(keyPlus, divList) )  
	else if (o.CoefficientType === RR) then
		new RWeilDivisor from ( hashTable(keyPlus, divList) )  
	else 
		new BasicDivisor from ( hashTable(keyPlus, divList) ) 
	
);

--the user may also pass in a single list consisting of pairs {n, P} where n is a coefficient and P is a height 1 prime ideal
divisor(BasicList) := BasicDivisor => o ->(myList) -> (
	myList2 := transpose myList;
	divisor(myList2#0, myList2#1,  CoefficientType=>o.CoefficientType, AmbientRing=>o.AmbientRing)
);

--gives an effective divisor corresponding to the ideal, i.e. V(I)
--however, if Section=> is specified, and non-null, the divisor(Ideal, Section=>f) finds a divisor D such that O(D) is isomorphic to the Ideal and such that the section corresponds 

divisor(Ideal) := WeilDivisor => o ->(I1) -> ( 
    if  (I1 == 0*I1) then (error "(divisor, Ideal): cannot form divisor from the zero ideal";);
	--question, is it faster to reflexify the ideal before computing minimal primes?  It seems so, but I'm not sure.
	I2 := reflexify(I1);
	L2 := {}; --the list of minimal primes
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
			flag = isSubset(I2, reflexify(idealPower(top1, (L2#i1)))); --ideal power gives us a huge speedup again
		);
		--now we do the binary search
		flag = true;
		bottom1 = 0;
		while (top1 > bottom1+1) do(
			tmp = floor((top1 + bottom1)/2);
			flag = isSubset(I2, reflexify(idealPower(tmp, (L2#i1))));
			if (flag == false) then top1 = tmp else bottom1 = tmp;
		);
		L0 = join(L0, {bottom1}); 
	);
	--for the users display purposes, if the ideal was prime we need to handle it differently
	if (#L0 == 1) then (
	    if (L0#0 == 1) then (
	        --the problem is minimalPrimes(I2) or even reflexify(I1) can change the displayed generators of the ideal.  This might not be optimal.
	        if (I2 == I1) then (
	            L2 = {I1};
	        );
	    )
	);
	
	if (instance(o.Section, RingElement)) then (
	    --NOTE the flipping of the sign in this case.
	    D1 := -divisor(L0, L2, AmbientRing=>(ring I1), CoefficientType=>o.CoefficientType);
	    D2 := divisor(o.Section);
	    D2 + D1
	)
	else (
	    --NOTE the sign is not flipped unlike when Section is specified.
	    divisor(L0, L2, AmbientRing=>(ring I1), CoefficientType=>o.CoefficientType)
	)
);

--gives an effective divisor corresponding to a ring element

divisor(RingElement) := WeilDivisor => o ->(f1) -> ( 
	if (instance(ring f1, FractionField) ) then (
		divisor(ideal(numerator(f1)), CoefficientType => o.CoefficientType) - divisor(ideal(denominator(f1)), CoefficientType => o.CoefficientType))
	else (
		divisor(ideal(f1), CoefficientType=>o.CoefficientType)
    )
);



divisor( Matrix ) := WeilDivisor => o -> (mat) ->
(
    L := embedAsIdeal(mat);
    divisor(L#1, Section => L#0)
);

--the following function was previously called moduleToDivisor
--moduleToDivisor = method(Options => {IsGraded => false});
--it creates a divisor D such that OO(D) is isomorphic to the given module M.
--if IsGraded is set to true, then it works as if we are on a projective variety


divisor( Module ) := WeilDivisor => o -> ( M ) ->
(
	I := 0;
	R1 := ring M;
	
	if (instance(o.Section, Matrix)) then (
	    if (not (target(o.Section) == M)) then error "(divisor, Module, Section=>Matrix): target of matrix is not the given module.";
	    return divisor(o.Section);
	);
	if (o.IsGraded == false) then ( 
		I = embedAsIdeal(R1, M);	
		-divisor( I )
	)
	else(
		L1 := embedAsIdeal(R1, M, IsGraded => true);
		I = L1#0;
		l := (-1)*L1#1;
		D1 := -divisor(I);
		--next we find an element of degree l
		els := findElementOfDegree(l, R1);
		D1 - divisor(els#0) + divisor(els#1)
	) 
);	



zeroDivisor = method();

zeroDivisor(Ring) := BasicDivisor => (R1) -> (
	divisor(sub(1, R1))
);





----------------------------------------------------------------
--************************************************************--
--Verifying divisor data----------------------------------------
--************************************************************--
----------------------------------------------------------------

--the following function is used to verify that a divisor is valid, it checks to make sure the coefficients are the right type, that the ideals are prime and height 1, etc.  It was formerly called verifyDivisor

isWellDefined(BasicDivisor) := Boolean => (D1) -> (
	myType := class D1;
	myList := coefficients(D1);
	flag := true;
	curFlag := true;
	--first we check the coefficients
	if (myType === WeilDivisor) then (
		curFlag = all(myList, z->instance(z, ZZ));
		if ((curFlag == false) and (debugLevel > 0)) then (print "(isWellDefined, BasicDivisor): Not all coefficients are integers");
		flag = flag and curFlag;
	);
	if (myType === QWeilDivisor) then (
		curFlag = all(myList, z->instance(z, QQ));
		if ((curFlag == false) and (debugLevel > 0)) then (print "(isWellDefined, BasicDivisor): Not all coefficients are rational numbers");
		flag = flag and curFlag;
	);
	if (myType === RWeilDivisor) then (
		curFlag = all(myList, z->instance(z, RR));
		if ((curFlag == false) and (debugLevel > 0)) then (print "(isWellDefined, BasicDivisor): Not all coefficients are real numbers");
		flag = flag and curFlag;
	);
	
	--now check to see if the ideals are prime and height one and in the same ambient ring
	--it only does these checks if the others were successful since they are time consuming
	if (flag == true) then(
		myList = primes(D1);
		myAmb := ring D1;
		--first we check the ambient ring
		flag = all(myList, z -> (myAmb === ring z));
		if ((flag == false) and (debugLevel > 0)) then (		    
		    print "(isWellDefined, BasicDivisor): Not all ideals have the same ambient ring";
		);
		--next we check primality
		if (flag == true) then ( 
			flag = all(myList, z->(isPrime(z)));
			if ((flag == false) and (debugLevel > 0)) then (print "(isWellDefined, BasicDivisor): Not all ideals are prime");
		);
		
		--finally we check dimension
		if (flag == true) then (
			d1 := dim myAmb;
			flag = all(myList, z->(d1 - dim(z) == 1));
			if ((flag == false) and (debugLevel > 0)) then (print "(isWellDefined, BasicDivisor): Not all ideals are height one");
		);
	);
	
	flag
);

----------------------------------------------------------------
--************************************************************--
--Accessing divisor data----------------------------------------
--************************************************************--
----------------------------------------------------------------

--Get the list of height one prime ideals of a divisor.  This returns the primes as the user entered them.

primes = method();
--TODO change if we change internal structure
primes( BasicDivisor ) := List => ( D ) -> 
(
	--we don't want the ambient ring in our prime list do we
	--D1 := select(D, z -> instance(z, BasicList));
	--Dkeys := keys D;
	gbKeys := gbs D; --select(keys D, z -> instance(z, GroebnerBasis)); --get the set of keys from the divisor that are Groebner bases
	return apply(gbKeys, g -> ( if ((D#cache#(symbol ideals))#?g) then ( (D#cache#(symbol ideals))#g ) else (return ideal g)));
--	D1 := select((D#cache), z->instance(z, BasicList));
--	valList := values D1;
--	if (#valList > 0) then (
--		valList2 := transpose valList;
--		return valList2#1;
--	)
--	else (
--	    return 
--	)
);

--get the number of primes
--TODO change if we change internal structure
getPrimeCount = method();

getPrimeCount( BasicDivisor ) := ZZ => ( D ) -> 
(
    gbKeys := gbs(D);
--	D1 := select(D, z -> instance(z, BasicList));
	#gbKeys
);

--we can also get the list of Grobner bases
--TODO change if we change internal structure
gbs = method();

gbs(BasicDivisor) := List => (D) -> ( 
--	D1 := select(D, z -> instance(z, BasicList));
--	keys D1 
    gbKeys := select(keys D, z->instance(z, BasicList));    
    gbKeys
);

--Get the list of coefficients of a divisor
--TODO change if we change internal structure
coefficients( BasicDivisor ) := List => o-> ( DD ) -> ( 	
    gbKeys := gbs(DD);
    return apply(gbKeys, z -> DD#z#0);
--	D1 := select(DD, z -> instance(z, BasicList));
--	valList := values D1;
--	if (#valList > 0) then (
--		valList2 := transpose valList;
--		valList2#0
--	)
--	else {}
);

--Given a divisor D and a irreducible codimensional one subspace C
--we would like to know what is the coefficient of this component inside this divisor
--it does NOT check whether or not P is prime


--TODO change if we change internal structure
coefficient(Ideal, BasicDivisor) := Number => (P, D) ->
(
	n := 0;
	if (instance(D, WeilDivisor)) then ( n = 0; )
	else if (instance(D, QWeilDivisor)) then ( n = 0/1; )
	else if (instance(D, RWeilDivisor)) then ( n = 0.0; );
--	flag := D #? (first entries gens (gb P) );
--	if( flag == true ) then ( n = (D # (first entries gens (gb P) ))#0 );
    gbListP := first entries gens (gb P);
    if (D #? gbListP) then ( n = (D#gbListP)#0 );
	n
);

--TODO change if we change internal structure
--in this version, the second argument is a list of generators of the appropriate Groebner basis
coefficient(BasicList, BasicDivisor) := Number => (l1, D) ->
(
	n := 0;
	if (instance(D, WeilDivisor)) then ( n = 0; )
	else if (instance(D, QWeilDivisor)) then ( n = 0/1; )
	else if (instance(D, RWeilDivisor)) then ( n = 0.0; );
--	flag := D #? l1;
	if( D #? l1 ) then ( n = (D # l1)#0 );
	n
);



--Given a divisor D, we want to know what is the ambient ring.
--TODO change if we change internal structure
ring(BasicDivisor) := Ring => (D1) ->
(
    return D1#(symbol ring);
);

--The next function gets a list of prime divisors of a given divisor
--warning, it accesses the underlying structure of the HashTable, 

getPrimeDivisors = method();
--TODO change if we change internal structure
getPrimeDivisors( BasicDivisor ) := List => (D) ->
(
	gbList := gbs(D);
	ambRing := ring D;

	myList := apply(gbList, z -> {z => {1}, symbol ring => ambRing, symbol cache => new CacheTable from {symbol ideals => new MutableHashTable from {z => ((D#cache)#(symbol ideals))#z }}});

	apply(myList, z -> new WeilDivisor from z)
);


----------------------------------------------------------------
--************************************************************--
--Other basic common methods for divisors.----------------------
--************************************************************--
----------------------------------------------------------------

--cleanSupport simply removes prime divisors with coefficient 0 (it also keeps the flag specifying the ambient ring of course)

cleanSupport = method();
--TODO change if we change internal structure 
cleanSupport( BasicDivisor ) := BasicDivisor => (D)  -> ( 
    cleaned := select(D, x -> ( if (instance(x, Ring)) then true else if (instance(x, CacheTable)) then false else (x#0 != 0) )); --this cleans the divisor itself
    newCleaned := new (class D) from ((pairs cleaned) | {(symbol cache, new CacheTable from D#cache)});
        --now to clean the cache
    scan(keys ((newCleaned#cache)#(symbol ideals)), t -> if (not (newCleaned#?t)) then remove((newCleaned#cache)#(symbol ideals), t) );
    return newCleaned;
);	

--clearCache removes all entries in the cache except ideals (in particular, it removes all computed entries)

clearCache = method(); 
clearCache( BasicDivisor ) := BasicDivisor => (D) -> (
    --D#cache = new CacheTable from {cache => D#cache#(symbol ideals)}; --doesn't work divisor is immutable hash table derived
    newCache := new CacheTable from pairs (D#cache);
    scan(keys newCache, t -> if (not (t === (symbol ideals))) then remove(newCache, t)); --this is a cache with all the computed values removed
    D1 := new (class D) from (pairs( select(D, t -> not (t === cache))) | {{ symbol cache, newCache}});
    return D1;
);

--applyFunctionToDivisorCoefficients applies the function to the coefficients of the divisor

applyToCoefficients = method(Options => {CoefficientType => null, Safe => false});
--TODO change if we change internal structure
applyToCoefficients( BasicDivisor, Function) := BasicDivisor => o -> (D, hhh) -> (
	myClass := class D;
	if (o.CoefficientType === ZZ) then (myClass = WeilDivisor)
	else if (o.CoefficientType === QQ) then (myClass = QWeilDivisor)
	else if (o.CoefficientType === RR) then (myClass = RWeilDivisor);

	tempDiv := cleanSupport(new myClass from applyValues(D, x -> (if (instance(x, Ring)) then x else if (instance(x, CacheTable)) then new CacheTable from {symbol ideals => x#(symbol ideals)} else {hhh(x#0)} )) );
	if (o.Safe == true) then (
		if (not (isWellDefined(tempDiv))) then (error "applyToCoefficients: the output of this function is not a valid divisor, did you set the CoefficientType option properly?";);
	);
	tempDiv	
);


---------------------------------
--this function cleans the support of the divisor
--and trims the ideals that are displayed
---------------------------------
--TODO change if we change internal structure
trim( BasicDivisor ) := BasicDivisor => o -> (DD) -> (
	myClass := class DD;
    tempDiv := cleanSupport(DD);
    scan(keys ((DD#cache)#(symbol ideals)), t -> ((DD#cache)#(symbol ideals)#t = trim(((DD#cache)#(symbol ideals)#t))));
    tempDiv
);

--Given a rational/real divisor, we return a Weil Divisor, such that new coefficients are obtained 
--from taking ceilings from the given one


ceiling( RWeilDivisor ) := WeilDivisor => ( D ) -> ( applyToCoefficients(D, ceiling, CoefficientType=>ZZ) );
--new WeilDivisor from applyValues(D, x -> (if (instance(x, Ring)) then x else {ceiling (x#0), x#1} )) );

--Given a rational/real divisor, we return a Weil divisor for which new coefficients are obtained
--from taking floors from the given one

floor( RWeilDivisor ) := WeilDivisor => ( D ) -> ( applyToCoefficients(D, floor, CoefficientType=>ZZ) );

--Given a divisor D, we want to return positive/negative part of D

positivePart = method();
--TODO change if we change internal structure
positivePart( RWeilDivisor ) := RWeilDivisor => (D) -> ( 
    cacheless := select(D, x -> (if (instance(x, Ring)) then true else if (instance(x, CacheTable)) then false else (x#0) > 0) ); --first make a divisor without its cache
    L := (pairs cacheless) | {{symbol cache, new CacheTable}};--grab the pairs, and make a clean cache
    cached := new (class D) from L;
    (cached#cache)#(symbol ideals) = new MutableHashTable from (D#cache)#(symbol ideals);
--    1/0;
    cleanSupport(cached)
);	

negativePart = method();
--TODO change if we change internal structure
negativePart( RWeilDivisor ) := RWeilDivisor => (D) ->
(
	-- := select(D, x -> (if (instance(x, Ring)) then true else if (instance(x, CacheTable)) then false else ((x#0) < 0)) );
--	applyValues(E, x -> (if (instance(x, Ring)) then x else {(-1) * (x#0), x#1}))
    positivePart(-D)
);



----------------------------------------------------------------
--************************************************************--
--Conversion and Comparing method among divisors.---------------
--************************************************************--
----------------------------------------------------------------


--Comparing Divisors


--Given two real Divisors, we want to test whether or not they are equal.

RWeilDivisor == RWeilDivisor := Boolean => (D, E) ->
(
    if (not (ring D === ring E)) then error "Expected divisors to have the same ring.";
	divDiff := cleanSupport( D - E );
	isZeroDivisor(divDiff)
);

--Conversion among divisors
--Given a Weil Divisor, it is naturally a rational divisor.
--since Weil divisors are Q-divisors

toQWeilDivisor = method();

toQWeilDivisor( WeilDivisor ) := QWeilDivisor => (D) -> 
(
	E := cleanSupport( D );
	coeffList := apply(coefficients E, x -> (1/1) * x );
	divisor(coeffList, primes E, AmbientRing=>ring(D), CoefficientType=>QQ )
);

toQWeilDivisor( QWeilDivisor ) := QWeilDivisor => (D) -> ( D ); --do nothing to an honest Q-divisor

--Given a Weil Divisor, it is naturally a real divisor.

toRWeilDivisor = method();

toRWeilDivisor( WeilDivisor ) := RWeilDivisor => (D) -> 
(
	E := cleanSupport( D );
	coeffList := apply(coefficients E, x -> (1.0) * x );
	divisor(coeffList, primes E, AmbientRing=>ring(D), CoefficientType=>RR)
);


--Given a rational Divisor, it is naturally a real divisor
--since QWeilDivisor is a subclass of RWeilDivisor

toRWeilDivisor( QWeilDivisor ) := RWeilDivisor => (D) -> 
(
	E := cleanSupport( D );
	coeffList := apply(coefficients E, x -> (1.0) * x );
    divisor(coeffList, primes E, AmbientRing=>ring(D), CoefficientType=>RR)
);

toRWeilDivisor( RWeilDivisor ) := RWeilDivisor => (D) -> ( D ); --do nothing to an honest R-divisor

--Given a rational/real divisor, if all its coefficients are of the integer type, we want to turn it to a Weil Divisor.

toWeilDivisor = method();
--TODO change if we change internal structure
toWeilDivisor( RWeilDivisor ) := WeilDivisor => ( D ) ->
(
	coeffList := new List from {};
	if ( isWeilDivisor( D ) != true ) then
	(
		error "toWeilDivisor: this is not a Weil divisor"
	)
	else
	(
		coeffList = apply(coefficients D, x -> floor x);
		divisor(coeffList, primes D, AmbientRing=>ring(D))
	)	
);

----------------------------------------------------------------
--************************************************************--
--Group operations on divisors.---------------------------------
--************************************************************--
----------------------------------------------------------------

--Multiplication of a divisor by scalar (integer, rational, real numbers).
--TODO change if we change internal structure
Number * BasicDivisor := BasicDivisor=> (n, D) ->
(
    D1 := clearCache D;    
	cleanSupport( applyValues(D1, x -> if (instance(x, Ring)) then x else if (instance(x, CacheTable)) then x else {n * (x#0)}) )
);	

QQ * WeilDivisor := QWeilDivisor => (r, D) ->
(
	E := toQWeilDivisor( D );
	r * E
);

--TODO change if we change internal structure
QQ * RWeilDivisor := RWeilDivisor => (r, D) ->
(
    D1 := clearCache D;  
	clearCache(cleanSupport( applyValues(D1, x -> if (instance(x, Ring)) then x else if (instance(x, CacheTable)) then x else {r * (x#0)}) ))
);

RR * QWeilDivisor := RWeilDivisor => (x, D) ->
(
	E := toRWeilDivisor( D );
	x * E
);

--TODO change if we change internal structure
RR * RWeilDivisor := RWeilDivisor => (y, D) -> 
( 
    D1 := clearCache D;
	clearCache(cleanSupport( applyValues(D1, x -> if (instance(x, Ring)) then x else if (instance(x, CacheTable)) then x else {y * (x#0)}) ) )
);

--Addition
--TODO change if we change internal structure
BasicDivisor + BasicDivisor := BasicDivisor => (D, E) -> 
(	
	myType := Nothing;
		
	if ( not (ring D === ring E) ) then 
	(
		error "(BasicDivisor + BasicDivisor): the two divisors should have the same ambient ring"
	)
	else
	(
		myType = BasicDivisor;
		if (instance(D, WeilDivisor)) then myType = class E
		else if (instance(E, WeilDivisor)) then myType = class D
		else if (instance(D, QWeilDivisor)) then myType = class E
		else if (instance(E, QWeilDivisor)) then myType = class D
		else if (instance(D, RWeilDivisor)) then myType = class E
		else if (instance(E, RWeilDivisor)) then myType = class D;

        cleanSupport( new myType from merge(D, E, keyPlus) )
	)
);	



--Subtraction between two divisors

BasicDivisor - BasicDivisor := BasicDivisor => (D, E) -> (D + (-E) );

--Negative of a divisor

-BasicDivisor := BasicDivisor => (D) -> ( (-1) * D );


----------------------------------------------------------------
--************************************************************--
--Functorial Properties and divisor module identifications------
--************************************************************--
----------------------------------------------------------------

--Given a divisor D, we want to construct its corresponding sheaf O(D).
--The internal version is called divisorToModule.  
--The external version is now called OO, so you can use it like OO(D)

divisorToModule = method ();

divisorToModule( WeilDivisor ) := Module => (D) ->
(
    if (D#cache#?divisorToModule == true) then (
        return D#cache#divisorToModule;
    );
	R := ring( D );
	E := positivePart( D );
	F := negativePart( D );
	E1 := apply(getPrimeCount(E), i -> (  idealPower( (coefficients E)#i,  (primes E)#i) )  ); --idealPower here yields a massive speedup
	F1 := apply(getPrimeCount(F), i -> (  idealPower( (coefficients F)#i,  (primes F)#i) )  ); 
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
--	prodE = reflexify(prodE); --these lines seem to speed things up, sometimes... (and more often they slow things down)
--	prodF = reflexify(prodF); --but when they speed things up it seems like a huge benefit, and when it slows things down it's only ~3 times slower
	                               --maybe eventually this could be something that is done with multiple threads... (I'm leaving them commented out for now)
	dual := (prodE*R^1) ** ( Hom(prodF*R^1, R^1) );
	M := Hom(dual, R^1);
	D#cache#divisorToModule = M;
	M
);   


divisorToModule( QWeilDivisor ) := Module => (D) ->
(
	divisorToModule( floor(D) )
);  

divisorToModule( RWeilDivisor ) := Module => (D) ->
(
	divisorToModule( floor(D) )
);

installMethod( symbol SPACE, OO, RWeilDivisor, (OO, D1) ->(divisorToModule(D1)) );

--Given a divisor D, 
--ideal D 
--produces an ideal isomorphic to to the sheaf O(-D).  If D is effective, this will be the honest ideal corresponding to O(-D), 
--otherwise it will just produce some ideal abstractly isomorphic to O(-D) (as a module)



ideal(WeilDivisor) := Ideal=> (D) -> (
    if (D#cache#?ideal == true) then (
        return D#cache#ideal;
    );    
	R := ring( D );
	E := positivePart( D );
	F := negativePart( D );
	J := null;
	E1 := apply(getPrimeCount(E), i -> (  idealPower( (coefficients E)#i,  (primes E)#i) )  ); --this seems to result in a huge speedup
	F1 := apply(getPrimeCount(F), i -> (  idealPower( (coefficients F)#i,  (primes F)#i) )  );
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
	if (#(F1) != 0) then (
		mydual := (prodF) * ( dualize(prodE) );  
		J = dualize(mydual);
		D#cache#ideal = J;
		return J;
	)
	else (
		J = reflexify(prodE);
		D#cache#ideal = J;
		return J;
	)
);

ideal( QWeilDivisor ) := Ideal=> (D) ->
(
	ideal( ceiling(D) )
);  

ideal( RWeilDivisor ) := Ideal=> (D) ->
(
	ideal( ceiling(D) )
);







--Give a ring map f: R -> S for which we assume is finite or flat, we want to construct its pullback from Div X to Div Y
--where Div X = Spec S and Div Y = Spec R.  If the map is neither finite or flat, then this method can produce unexpected results unless the divisor is Cartier (which the function checks for).  

pullback = method(Options => {Strategy => Primes});

pullback(RingMap, RWeilDivisor) := BasicDivisor => o->(f, D) ->
(		
	if ( not (ring D === source f) ) then error "(pullback, WeilDivisor): Expected the Divisor and the source of the map to have the same ambient ring.";
	
	if (o.Strategy === Primes) then (--we pull back individual prime ideals
		E := divisor({}, {}, AmbientRing => (target f));
		L := coefficients D;
		PL := primes D;
		for i when (i < #L ) do
		(
			E = E + L#i * divisor( f( PL#i ) )		
		);
		E
	)
	else if (o.Strategy === Sheaves) then ( --we pullback a sheaf
		if (isWeilDivisor(D) == false) then ( error "(pullback, WeilDivisor):  If you use the sheaf strategy, you must pullback a WeilDivisor"; );
		toWeilDivisor(D);
		DM := negativePart(D);
		IM := ideal(DM);
		g1 := 0;
		i := 0;
		genList := first entries gens IM;
		myFlag := false;
		while (myFlag == false) do (
			g1 = genList#i;
			i = i+1;
			myFlag = (f(g1) != 0);
		);
		if (myFlag == false) then (error "(pullback, WeilDivisor): this divisor cannot pull back, it has a component which vanishes on the image of the map (this error will only occur for terms with negative coefficients)";);
		--the point of all that is that D + divisor(g1) is effective
		J := ideal(D + divisor(g1));
		J = f(J);
		divisor(J) - divisor(f(g1))
	)
);

--one can also call pullback via the following method

installMethod(symbol ^*, RingMap, f1-> (D1 -> pullback(f1, D1)));

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

findElementOfDegree(ZZ, Ring) := List => (n1, R1) ->  (
	varList := first entries vars R1;
	degList := flatten (degrees R1); --apply(varList, q -> (degree(q))#0);
	myGCD := gcd(degList);
	if (not (n1%(myGCD) == 0)) then error "findElementOfDegree: No element of the specified degree can be obtained";
	bezoutList := floor(n1/myGCD)*bezoutNumbers(degList);
	bezoutPositive := apply(bezoutList, z->max(0, z));
	bezoutNegative := (-1)*apply(bezoutList, z->min(0, z));
	{sub(product(  apply(#varList, i -> (varList#i)^(bezoutPositive#i) )  ), R1), sub(product(  apply(#varList, i -> (varList#i)^(bezoutNegative#i) )  ), R1)}
);

--sometimes elements are given by multidegrees
findElementOfDegree(BasicList, Ring) := List => (l1, R1) ->  ( 
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

getLinearDiophantineSolution = method(Options => {Safe => true});

getLinearDiophantineSolution(BasicList, BasicList) := BasicList => o ->(l1, l2) -> (	--the first entry is the target vector because that is simpler
	rowList := transpose l2;							--the list of integer rows that forms the integer matrix A
	bEntries := l1;									--making the entries of target vector into a list
	b := vector( bEntries );						--the target vector in the LDE Ax = b
	flag := true;									--a boolean value used for checking
	m := #rowList;									--the number of rows of integer matrix A
	testNumber := 0; 									
	
	if (o.Safe == true ) then (
		--We need to check all vectors have integer entry. And the length of the row list is equal to the length of vector b
		if( #bEntries != #rowList ) then (
			error "Number of rows in the matrix should match the length of target vector"
		);
		if ( not( all(bEntries, i -> ( instance(i, ZZ) ) ) ) ) then (
			error "Each entry of the target vector should be an integer"
		);
		for i when (i < #rowList ) do (
			if ( not( all (rowList#i, j ->( instance(j, ZZ) ) ) ) ) then (
				error "Each entry of the ith row should be an integer"
			)
		);
		if ( m == 0) then (
			error "The number of rows in the integer matrix of the linear Diophantine equation should be nonzero"
		) else (
			testNumber = #(rowList#0);
			if ( not( all(rowList, x -> (#x == testNumber) ) ) ) then (
				error "The length of each row vector should be equal"
			)
		)
	);
    n := #(rowList#0); --the number of columns of the integer matrix A
	if ( n == 0 ) then (
		error "The number of columns in the integer matrix of the linear Diophantine equation should be nonzero"
	); 
	A := matrix( rowList );	--the integer matrix A in the matrix equation Ax = b						
	smithList := toList( smithNormalForm( A ) );		--smith normal form (D = L * A * R )
	D := smithList#0;		--the diagonal matrix in smith normal form						
	L := smithList#1;		--matrix multiply A on the left which corresponds to column operations on A						
	R := smithList#2;		--matrix multiply A on the right which corresponds to row operations on A						
	c := L * b;										--important vector for checking the existence of equation Ax = b
	cEntry := entries c;
				
	--The principle is here: the matrix solution Ax = b is equivalent to LAx = Lb = c. Now if we let y = R^(-1)x then 
	--the equation becomes (LAR)y = c which is Dy = c. So to check existence of Ax = b, it's enough to check the existence
	--of equation Dy = c which means we have to check is ci divisible by di for the first r entries.
			
	diagMatrixEntry := entries D;--the row lists of the diagonal matrix D from SNF of A
	diagList := apply(min{m, n}, i -> ( diagMatrixEntry#i )#i );	--the list of diagonal entries of D in SNF of A
	diagList = select( diagList, x -> ( x != 0 ) );
	r := #(diagList); --the number of nonzero elements in D			
				
	--To check if the system D y = c has a solution is very easy, we just need to check if each ci is divisible by di
				
	if ( not( all(r, i -> ( (cEntry#i)%(diagList#i) == 0 ) ) ) ) then (
		error "The linear Diophantine equation does not have a solution";
	);

	y := vector ( apply(n, i -> ( if (i > r - 1) then ( 0 ) else( (cEntry#i)/(diagList#i) ) ) ) ); --the solution of equation Dy = c		
	--As we said previously, y = R^(-1)x. So to get the solution of equation Ax = b, we only need to apply R to the vector y.			
	--output := flatten entries ( (inverse( R ) ) * (matrix y) );		
    flatten entries  ( R  * (matrix y) )
);

--this solves A*x = l1
getLinearDiophantineSolution(BasicList, Matrix) := BasicList => o-> (l1, A) -> (
	L := entries transpose A;
	getLinearDiophantineSolution(l1, L, Safe => o.Safe)
);


--used for construction of canonical divisors
canonicalDivisor = method(Options => {IsGraded => false});

canonicalDivisor(Ring) := WeilDivisor => o->(R1) -> (
	S1 := ambient R1;
	I1 := ideal R1;
	dR := dim R1;
	dS := dim S1;
	varList := first entries vars S1;
	degList := {};
	if (#varList > 0) then ( --then there are no variables
        if (#(degree(varList#0)) == 1) then (
		    degList = apply(varList, q -> (degree(q))#0); )
	    else (
		    degList = apply(varList, q -> (degree(q))); );
	);
	M1 := Ext^(dS - dR)(S1^1/I1, S1^{-(sum degList)});
	divisor(M1**R1, IsGraded=>o.IsGraded)
);


--computes a ramification divisor of a finite map Y -> X of normal varieties.  It can also compute the relative canonical divisor of things like blowups (in which case, make sure the IsGraded flag is set to true)
--warning, the IsGraded functionality may not work properly if X is not smooth (it is experimental)
ramificationDivisor = method(Options => {IsGraded => false});
--pass it an injective map between normal rings f1 : R1 -> S1 such that S1 is a finite R1 module.  The function assumes the two rings use the same coefficientRing.
ramificationDivisor(RingMap) := WeilDivisor => o->( f1 ) ->
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
	
	sourceList := first entries vars ambient R1;
	targetList := first entries vars ambient S1;
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

isWeilDivisor = method();

isWeilDivisor( RWeilDivisor ) := Boolean => ( D ) ->
(
	coeffList := coefficients ( cleanSupport( D ) );
	all(coeffList, x -> (ceiling x == floor x) )
);

--Given a divisor, we want to test is this divisor is effective or not.

isEffective = method();

isEffective(BasicDivisor) :=  Boolean => (D) -> (
	coeffList := coefficients ( cleanSupport( D ) );
	all(coeffList, x->(x >= 0))
);


--Given a divisor D, we want to know is this divisor is prime or not.

isPrime(BasicDivisor) := {} >> o -> (D1) -> (
	(coefficients (cleanSupport D1) ) == {1} 
);

--Given a divisor D, we want to know is this divisor is reduced or not.

isReduced = method();

isReduced( BasicDivisor ) := Boolean => (D) -> 
( 
	coeffList := coefficients ( cleanSupport( D ) );
	all(coeffList, x->(x == 1))
);

--Given a divisor, we want to check if the corresponding module is globally principal or not

isPrincipal = method(Options => {IsGraded => false});

isPrincipal( WeilDivisor ):= Boolean => o -> (D) ->
(
    if (D#cache#?isPrincipal == true) then (
        if (D#cache#isPrincipal#0 == o.IsGraded) then (
            return D#cache#isPrincipal#1;
        );
    );
	M := prune OO(D); 
	flag := false;
	if (o.IsGraded == false) then(
		flag = isFreeModule ( M );
		if ((flag == false) and (isHomogeneous(D) == false)) then ( --let's try some other tricks to see if we can make it principal
			J1 := embedAsIdeal(M);
			if (#(first entries gens J1) == 1) then (flag = true;);
			if (flag == false) then (
				J1 = trim (J1 : (J1^0)); 
				if (#(first entries gens J1) == 1) then (flag = true;); 
			);
			if (flag == false) then ( flag = (1 == #(first entries gens gb J1)); );
			if (flag == false) then print "Warning, isPrincipal may give a false negative for a divisor defined by non-homogeneous ideals";
		)

	) 
	else ( --TODO:  Perhaps It would be faster to check whether or not M has a section that doesn't vanish anywhere instead of pruning M.
		if (isHomogeneous(D) == false) then error "isPrincipal: Expected argument to be homogeneous if the IsGraded option is set to true.";
		if (isFreeModule ( M ) ) then(
			flag = (degrees M == {{0}})
		);
	);
	D#cache#isPrincipal = {o.IsGraded, flag};
	flag
);	

--Given a divisor, we want to check if the corresponding module is Cartier

isCartier = method(Options => {IsGraded => false});

isCartier( WeilDivisor ) := Boolean => o -> (D) ->
( --we rely on the fact that an ideal corresponds to a Cartier divisor if and only if I*I^{-1} is reflexive
  --David Eisenbud pointed out another option would be to compute a bunch of minors and see if they generate the unit ideal... I'll try this in some examples and see which is faster (but I haven't done so yet)
    if ((D#cache#?isCartier == true)) then (
        if ((D#cache#isCartier)#0 == o.IsGraded) then (
            return D#cache#isCartier#1;
        );
    );
	flag := false;
	R := ring( D );
	if (o.IsGraded == false) then (
		ID := ideal( D );
		IDminus := dualize(ID); 
		myProduct := ID*IDminus;
		flag = (myProduct == reflexify(myProduct))
	)
	else (
		if (isHomogeneous(D) == false) then error "isCartier: Expected argument to be homogeneous if the IsGraded option is set to true.";
		myMax := getIrrelevantIdeal(R);
		J1 := nonCartierLocus(D);
		L := saturate(J1, myMax);
		flag = isSubset(ideal(sub(1, R)), L)
	);
	D#cache#isCartier = {o.IsGraded, flag};
	flag
);	

--Get the non-Cartier locus


nonCartierLocus = method(Options => {IsGraded => false});
--TODO:  Compare this with computing minors of a presentation, I'm not sure if this will be faster or slower, there can be a lot of minors... (David Eisenbud suggested this)
nonCartierLocus( WeilDivisor ) := Ideal => o -> (D) ->
(
    if ((D#cache#?nonCartierLocus) == true) then (
        if ((D#cache#nonCartierLocus)#0 == o.IsGraded) then (
            return (D#cache#nonCartierLocus#1);
        );
    );
	R := ring( D );
	OD := ideal( D ); --I woulder if it would be better to saturate this first in the graded case... (or if we have multiple threads, do it at the same time we do the next command).
	ODminus:= dualize(OD);
	I := OD*ODminus;
	J := annihilator ((reflexify(I)*R^1) / (I*R^1));
	if (o.IsGraded == true) then (
		if (isHomogeneous(D) == false) then error "nonCartierLocus: Expected argument to be homogeneous if the IsGraded option is set to true.";		
		J = saturate(J, getIrrelevantIdeal(R));
	);
	D#cache#nonCartierLocus = {o.IsGraded, J};
	J
);	

--Given two divisors D and E, we want to know whether they are linear equivalent or not.

isLinearEquivalent = method(Options => {IsGraded => false});

isLinearEquivalent( WeilDivisor, WeilDivisor ):= Boolean => o->(D, E) ->
(
	isPrincipal(D-E, IsGraded=>o.IsGraded)
);		

--Given two rational divisor, we want to see if they are linearly equivalent after removing denominators

isQLinearEquivalent = method(Options => {IsGraded => false});

isQLinearEquivalent(ZZ, QWeilDivisor, QWeilDivisor ):= Boolean => o->(nn, D1, D2) ->
(
	if (not (ring D1 === ring D2) ) then error "isQLinearEquivalent: Expected the two divisors to have the same ambient ring";
	D1 = toQWeilDivisor(D1);
	D2 = toQWeilDivisor(D2);
	La := coefficients(D1);
	L1 := apply(#La, i -> denominator( La#i ) ); --list of denominators of coefficients of D1
	La = coefficients(D2);
	L2 := apply(#La, i -> denominator( La#i ) ); --list of denominators of coefficients of D2
	m1 := 1; if (#L1 > 0) then m1 = lcm( toSequence L1  );
	m2 := 1; if (#L2 > 0) then m2 = lcm( toSequence L2  );
	m := lcm(m1, m2);
	--first we check whether the two divisors are already linearly equivalent after clearing denominators
	E1 := toWeilDivisor( m * D1);
	E2 := toWeilDivisor( m * D2);
	returnVal := isLinearEquivalent( E1, E2, IsGraded=>o.IsGraded );
	i := 2;
	while( (i <= nn) and (returnVal == false)) do(
	    returnVal = isLinearEquivalent(i*E1, i*E2, IsGraded=>o.IsGraded);
	    i = i+1;
	);
	return returnVal;
);


--checks whether mD1 is Cartier for any m from 1 to n1, if it is Cartier, it returns the Cartier-index.  If it is not Q-Cartier or if the Q-Cartier index is greater than n1, then it returns 0.  This can be quite slow for large values of n1.

--it would be good to compare this with a function that also uses the idealPower function
isQCartier = method(Options => {IsGraded => false});

isQCartier( ZZ, WeilDivisor ):= Boolean => o->(n1, D1) -> (
	if (n1 < 1) then error "isQCartier: Expected the first argument to be a positive integer";
	i := 1;
	if ((D1#cache#?isQCartier) == true) then (
        if ((D1#cache#isQCartier)#0 == o.IsGraded) then (
            if ((D1#cache#isQCartier)#1 > 0) then (return ((D1#cache#isQCartier)#1));-- else (i = (D1#cache#isQCartier#2));
            if ((D1#cache#isQCartier)#2 > n1) then (return 0); --we've already computed this far           
        );
    );
--	M1 := ideal(i*D1);
--	curModule := M1;
--	S1 := ring M1;
--	minusCurModule := dualize(curModule);
--	tempModule := trim (curModule*minusCurModule);
    S1 := ring D1;
	J := 0;
	if (o.IsGraded==false) then (
    	M1 := ideal(i*D1);
	    curModule := M1;
	    minusCurModule := dualize(curModule);
    	tempModule := trim (curModule*minusCurModule);
		flag := isReflexive(tempModule);
		while ((flag == false) and (i <= n1)) do(
			curModule = M1*curModule;
			minusCurModule = dualize(curModule);
			curModule = dualize(minusCurModule);
			tempModule = trim (curModule*minusCurModule);
			flag = isReflexive(tempModule);
			i = i+1;
		);
		if (flag == false) then ( --store what we've done in the cache
		    D1#cache#isQCartier={o.IsGraded, 0, i}; --we aren't currently using the i value
		    i = 0;
		)
		else (
		    D1#cache#isQCartier={o.IsGraded, i, i};
		);
	)
	else ( --homogeneous case
		if (isHomogeneous(D1) == false) then error "isQCartier: Expected second argument to be homogeneous if the IsGraded option is set to true.";
		myMax := getIrrelevantIdeal(S1);
		J = nonCartierLocus(D1);
		L := saturate(J, myMax);
		gflag := isSubset(ideal(sub(1, S1)), L);
		while ((gflag == false) and (i <= n1)) do(
			J = nonCartierLocus(i*D1);
			L = saturate(J, myMax);
			gflag = isSubset(ideal(sub(1, S1)), L);
			i = i+1;
		);
		if (gflag == false) then ( --store what we've done in the cache
		    D1#cache#isQCartier={o.IsGraded, 0, i};		--we aren't currently using the i value
		    i = 0;
		)
		else (
		    D1#cache#isQCartier={o.IsGraded, i, i};
		);

	);
	i
);

--a function to get the irrelevant ideal, it is internal only
getIrrelevantIdeal = method();

getIrrelevantIdeal(Ring) := Ideal => (R1) -> (
    ideal select(first entries vars R1, zz -> (degree(zz)) > degree(sub(1,R1)))
);

--we also can check it for Q-divisors
isQCartier(ZZ, QWeilDivisor) := Boolean => o->(n1, D1) -> (
	La := coefficients(D1);
	L1 := apply(#La, i -> denominator( La#i ) ); --list of denominators of coefficients of D1
	m1 := lcm( toSequence L1  ); --this number clears the denominators
	m1*isQCartier(ceiling(n1/m1), IsGraded=>o.IsGraded, toWeilDivisor(m1*D1))
);

isHomogeneous (BasicDivisor) := Boolean => (D1) -> (
	pList := primes(D1);
	all(pList, isHomogeneous)
);

--checks whether a divisor is a SNC divisor
isSNC = method(Options => {IsGraded => false});

isSNC(BasicDivisor) := Boolean => o->(D1) -> (
    if ((D1#cache#?isSNC) == true) then (
        if ((D1#cache#isSNC)#0 == o.IsGraded) then (
            return (D1#cache#isSNC#1);
        );
    );
	D1 = cleanSupport(D1);
	j := 0;
	R1 := ring(D1);
	d1 := dim R1;
	pList := primes(D1);
	idealSubsets := subsets pList;
	nonemptySubsets := select(idealSubsets, z->(#z > 0));
	toModOutBy := apply(nonemptySubsets, z -> sum(z));
	flag := isSmooth(ideal(sub(0, ring(D1))), IsGraded=>o.IsGraded);
	irrIdeal := getIrrelevantIdeal(R1);

	while ( (j < #toModOutBy) and flag ) do (
		if (o.IsGraded == false) then (
			flag = ((d1 - #(nonemptySubsets#j) == dim(R1/toModOutBy#j)) or (dim(R1/toModOutBy#j) < 0) );
			if (flag == true) then flag = isSmooth(toModOutBy#j);
		)
		else (	
			if (isHomogeneous(toModOutBy#j) == false) then error "isSNC: Expected a homogeneous ideal";
			flag = ((d1 - #(nonemptySubsets#j) == dim(R1/toModOutBy#j)) or (saturate(toModOutBy#j, irrIdeal) == ideal(sub(1, R1)) ) );
			if (flag == true) then flag = isSmooth(toModOutBy#j, IsGraded=>o.IsGraded);
		);
		j = j+1
	);
	D1#cache#isSNC = {o.IsGraded, flag};
	flag 
);

isZeroDivisor = method();

isZeroDivisor(BasicDivisor) := Boolean => (D1) -> (
	D1 = cleanSupport(D1);
	(#(primes(D1)) == 0)
);

----------------------------------------------------------------
--************************************************************--
--Global sections for divisors (base point free, etc)-----------
--************************************************************--
----------------------------------------------------------------

--given a Cartier divisor, we can find the map to projective space from the corresponding module
mapToProjectiveSpace = method(Options => {KnownCartier=>true, Variable=>"YY"});

mapToProjectiveSpace(WeilDivisor) := RingMap => o->(D1) -> (
	if (isHomogeneous(D1) == false) then (error "mapToProjectiveSpace: Expected a graded/homogeneous divisor.";);
	if (o.KnownCartier == false) then (if (isCartier(D1, IsGraded=>true) == false) then (error "mapToProjectiveSpace: Expected a Cartier divisor."); );
	--this might be slower than the method done in the tutorial, say calling 
	--ideal(positivePart(D1)) and ideal(negativePart(D1)) 
	--and then proceeding as they did might be faster
	newVar := null;
	if ( instance(o.Variable, Symbol) ) then (
	    newVar = o.Variable;
	)
	else if (instance(o.Variable, String) ) then (
	    newVar = getSymbol o.Variable;
	)
	else(
	    error "mapToProjectiveSpace: expected option Variable to be a string or a symbol.";
	);
	R1 := ring(D1);
	M1 := prune OO(D1);
	L1 := embedAsIdeal(M1, IsGraded=>true);
	d1 := L1#1;
	M1 = L1#0*R1^{d1};
	b1 := super ((basis(degree sub(1,R1), M1))**R1);
	n1 := #(first entries b1);
	K1 := coefficientRing R1;
	myMon := monoid[toList(newVar_1..newVar_n1)];
	S1 := K1 myMon;
	varTargetList := first entries b1;
	--do some defense against degree zero stuff
	--this defense should be better than it is...
	--basically, by default if D = 0, then the map produced by the above corresponds to kk[newVar] -> R1
	--which sends newVar to 1.  The kernel is not homogeneous in that case, which doesn't make sense of course.
	--and this can break other functions.  Thus, in this case, we fix it by turning the map into an equivalent map
	if ( instance(d1, List) ) then(
	    if (#d1 >= 1) then (
	        if (instance(d1#0, Number)) then (
	            if (d1#0 == 0) then (
	                R1varList := first entries vars R1;
	                if (#R1varList > 0) then (
    	                vv := (first entries vars R1)#0;
	                    varTargetList = apply(varTargetList, ss -> ss*vv);
	                );
    	        );
    	    );
	    );
	);
	map(R1, S1, varTargetList)
);

--finds the base locus of a module or divisor

baseLocus = method();

baseLocus(Module) := Ideal => (M1) -> (
	b1 := basis(0, M1);
	saturate ann coker b1
);

baseLocus(WeilDivisor) := Ideal => (D1) -> (
	M1 := OO(D1);
	baseLocus(M1)
);

isVeryAmple = method(Options => {Verbose=>false});

isVeryAmple(WeilDivisor) := Boolean => o->(D1) -> (    
    if (D1#cache#?isVeryAmple == true) then (
        return D1#cache#isVeryAmple;
    );    
    mapFromD1 := mapToProjectiveSpace(D1);
    if (#(first entries vars source mapFromD1) == 0) then (
        D1#cache#isVeryAmple = false;
        false)
    else (
        flag := isEmbedding(mapFromD1, Verbose=>o.Verbose);
        D1#cache#isVeryAmple = flag;
        flag
    )
);
 	 	
----------------------------------------------------------------
--************************************************************--
--Useful functions which don't interact with the divisor class--
--************************************************************--
----------------------------------------------------------------

idealPower = method(); -- it seems to be massively faster to reflexify ideals with few generators than ideals with many generators, at least some of the time...

idealPower(ZZ, Ideal) := Ideal => (n, J) -> (
	genList := first entries gens J;
	ideal( apply(genList, z->z^n))
);


dualize = method(Options => {KnownDomain=>true, Strategy =>NoStrategy});

dualize(Ideal) := Ideal => o->(I1) -> ( 
    if (o.Strategy == ModuleStrategy) then (
        S1 := ring I1;
        mydual := Hom(I1*S1^1, S1^1);
        embedAsIdeal(mydual)
    )
    else  (
        dualizeIdeal(I1, KnownDomain=>o.KnownDomain)
    )
);

dualize(Module) := Module => o->(M1) -> (
    S1 := ring M1;
    if (o.Strategy == IdealStrategy) then (
        I1 := embedAsIdeal(M1);
        (dualizeIdeal(I1, KnownDomain=>o.KnownDomain))*(S1)^1
    )
    else (
        Hom(M1, S1^1)
    )
);

--the following is an internal function for dualizing an ideal (finding an ideal isomorphic to Hom(I, R))
dualizeIdeal = method(Options => {KnownDomain=>true});

dualizeIdeal(Ideal) := Ideal => o->(I1) -> (
	S1 := ring I1;
	assumeNormal := false;
	if (o.KnownDomain == true) then (assumeNormal = true;) else (assumeNormal = isNormal(S1););
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
--		inc := inducedMap(S1^1, I1*(S1^1));
		mydual := Hom(I1*S1^1, S1^1);
		embedAsIdeal(mydual)
	)

);

--Given an ideal, we will frequently want to double-dualize / find the S2-ification.  The following function does this.

reflexify = method(Options => {Strategy => NoStrategy, KnownDomain=>true, ReturnMap => false});

--the first variant simply reflexifies an ideal

reflexify(Ideal) := Ideal => o->(I1) -> (
    if (o.Strategy == ModuleStrategy) then (
        --the user specified we use the ModuleStrategy
        S1 := ring I1; 
        inc := inducedMap(S1^1, I1*(S1^1));
        ddual := Hom(Hom(inc, S1^1), S1^1);
		annihilator(coker ddual)
    )
    else ( --otherwise we use the default ideal strategy
        reflexifyIdeal(I1, KnownDomain=>o.KnownDomain)
    )
);

--an internal function that reflexifies an ideal

reflexifyIdeal = method(Options => {KnownDomain=>true});

reflexifyIdeal(Ideal) := Ideal => o->(I1) -> (
	S1 := ring I1;
	assumeDomain := false;
	if (o.KnownDomain == true) then (assumeDomain = true;) else (assumeDomain = isDomain(S1););
	if (assumeDomain) then ( 
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

reflexify(Module) := Module => o-> (M1) -> (
	S1 := ring M1;
	if (o.Strategy == IdealStrategy) then (
	    --the user specified we use the ideal strategy, this only works if the module can be embedded as an ideal
	    I1 := embedAsIdeal(M1);
	    I2 := reflexifyIdeal(I1, KnownDomain => o.KnownDomain);
	    if (o.ReturnMap == true) then (
	        inducedMap(I2*S1^1, I1*S1^1)
	    )
	    else (
	        I2*S1^1
	    )
	)
	else (
	    reflexifyModule(M1, ReturnMap => o.ReturnMap)
	)
);

reflexifyModule = method(Options=>{ReturnMap=>false});

reflexifyModule(Module) := Module => o-> (M1) -> (
	S1 := ring M1;
	if (o.ReturnMap == true) then (
	    gensMatrix := gens M1;
	    h := map(M1, source gensMatrix, id_(source gensMatrix));
	    ddh := Hom(Hom(h, S1^1), S1^1);
	    map(target ddh, M1, matrix ddh)
	)
	else (
	    (Hom(Hom(M1, S1^1), S1^1))
	)
);

isReflexive = method(Options => {Strategy => NoStrategy, KnownDomain=>true});

isReflexive(Module) := Boolean => o -> (M1) ->(
	g := reflexify(M1, ReturnMap => true, Strategy => o.Strategy, KnownDomain=>o.KnownDomain);
	(-1 == dim coker g)
);

isReflexive(Ideal) := Boolean => o-> (I1) ->(
	J1 := reflexify(I1, Strategy => o.Strategy, KnownDomain=>o.KnownDomain);
	(J1 == I1)
);

--we can also grab the torsion submodule since we are here
torsionSubmodule = method(Options => {Strategy => NoStrategy, KnownDomain=>false});

torsionSubmodule(Module) := Module => o -> (M1) -> (
	ker reflexify(M1, ReturnMap => true, Strategy=>o.Strategy, KnownDomain=>o.KnownDomain)
);

--this method embeds a rank 1 module as a divisorial ideal
--this method is based on and inspired by code originally written by Moty Katzman, earlier versions can be found in 
-- http://katzman.staff.shef.ac.uk/FSplitting/ParameterTestIdeals.m2
--under canonicalIdeal

embedAsIdeal = method(Options => {MTries =>10, IsGraded=>false, ReturnMap=>false, Section=>null});

embedAsIdeal(Module) := Ideal => o -> (M1) -> (
    S1 := ring M1;
	embedAsIdeal(S1, M1, MTries=>o.MTries, IsGraded=>o.IsGraded, ReturnMap=>o.ReturnMap, Section=>o.Section)
)

embedAsIdeal(Matrix) := Ideal => o -> (Mat1) -> (
    S1 := ring Mat1;
	embedAsIdeal(S1, Mat1, MTries=>o.MTries, IsGraded=>o.IsGraded, ReturnMap=>o.ReturnMap)
)

embedAsIdeal(Ring, Module) := Ideal => o ->(R1, M2) ->(
    if (instance(o.Section, Matrix)) then ( --if we are passing a section
        if (target o.Section == M2) then (
            embedAsIdeal(R1, o.Section, MTries=>o.MTries, IsGraded=>o.IsGraded, ReturnMap=>o.ReturnMap)
        )
        else (
            error "embedAsIdeal: the target of the section is not equal to the given module.";
        )
    )
    else(
        internalModuleToIdeal(R1, M2, MTries=>o.MTries, IsGraded=>o.IsGraded, ReturnMap=>o.ReturnMap)
    )
)

embedAsIdeal(Ring, Matrix) := Ideal => o->(R1, Mat2) -> (
    internalModuleWithSectionToIdeal(R1, Mat2, MTries=>o.MTries, IsGraded=>o.IsGraded, ReturnMap=>o.ReturnMap)
)

internalModuleToIdeal = method(Options => {MTries=>10, IsGraded=>false, ReturnMap=>false});

internalModuleToIdeal(Ring, Module) := Ideal => o ->(R1, M2) -> 
(--turns a module to an ideal of a ring
--	S1 := ambient R1;
	flag := false;
	answer:=0;
	if (M2 == 0) then ( --don't work for the zero module	    
	    answer = ideal(sub(0, R1));
	    if (o.IsGraded==true) then (		    
			answer = {answer, degree (sub(1,R1))};
		);
		if (o.ReturnMap==true) then (
		    if (#entries gens M2 == 0) then (
		        answer = flatten {answer, map(R1^1, M2, sub(matrix{{}}, R1))};
		    )
		    else (
			    answer = flatten {answer, map(R1^1, M2, {apply(#(first entries gens M2), st -> sub(0, R1))})};
			);
		);

	    return answer;
	);
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
		if (isWellDefined(h) == false) then error "internalModuleToIdeal: Something went wrong, the map is not well defined.";
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
		if (isWellDefined(h) == false) then error "internalModuleToIdeal: Something went wrong, the map is not well defined.";
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
	if (flag == false) then error "internalModuleToIdeal: No way found to embed the module into the ring as an ideal, are you sure it can be embedded as an ideal?";
	answer
);


--this variant takes a map from a free module of rank 1 and maps to another rank 1 module.  The function returns the second module as an ideal combined with the element 

internalModuleWithSectionToIdeal = method(Options => {MTries=>10, ReturnMap=>false, IsGraded=>false});

internalModuleWithSectionToIdeal(Ring, Matrix) := Ideal => o->(R1, f1)->
(
	M1 := source f1;
	M2 := target f1;
	if ((isFreeModule M1 == false) or (not (rank M1 == 1))) then error ("internalModuleWithSectionToIdeal: Error, source is not a rank-1 free module";);
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
		if (isWellDefined(h) == false) then error "internalModuleWithSectionToIdeal: Something went wrong, the map is not well defined.";
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
			);
		);
		i = i+1;
	);
	-- if that doesn't work, then try a random combination/embedding
	while ((flag == false) and (i < o.MTries) ) do (
		coeffRing := coefficientRing(R1);
		d := sum(#s2, z -> random(coeffRing, Height=>100000)*(s2#z));
		h = map(R1^1, M2**R1, {d});
		if (isWellDefined(h) == false) then error "internalModuleWithSectionToIdeal: Something went wrong, the map is not well defined.";
		if (isInjective(h) == true) then (
			flag = true;
			answer = trim ideal(d);
			if (o.IsGraded==true) then (
				d1 = degree(d#0) - (degrees M2)#0;
				answer = {answer, d1};
			);
			if (o.ReturnMap==true) then (
				answer = flatten {answer, h};
			);
		);
	);
	
	if (flag == false) then error "internalModuleWithSectionToIdeal: No way found to embed the module into the ring as an ideal, are you sure it can be embedded as an ideal?";
	newMatrix := h*f1;
	flatten {first first entries newMatrix, answer}
);


isDomain = method();

isDomain(Ring) := Boolean => (R1) -> (
	isPrime( ideal(sub(0, R1)))
);

--checks whether R/J1 is regular
isSmooth  = method(Options => {IsGraded => false});

isSmooth(Ideal) := Boolean => o->J1 -> (
	--empty schemes are smooth (which is why we are first check whether ideals are the whole ring or contain the irrelevant ideal
	flag := false;
	if (o.IsGraded == true) then (
		if (isHomogeneous(J1) == false) then (error "isSmooth: Expected a homogeneous ideal");
		if (not isField(coefficientRing ring J1)) then (error "isSmooth: expected a standard graded ring over a field if IsGraded=>true");
		myMax := getIrrelevantIdeal(ring J1);
		if (isSubset(myMax, J1)) then (flag = true) else (flag = (saturate((ideal singularLocus J1)*(ring J1), myMax) == ideal(sub(1, ring J1))));
		--(isSubset(myMax, (ideal singularLocus J1)*(ring J1)))
	)
	else ( 
		if (isSubset(ideal(sub(1, ring J1)), J1)) then (flag = true) else (flag = (dim singularLocus J1 < 0));
	);

	flag
);

reflexivePower = method(Options=>{Strategy=>IdealStrategy});

reflexivePower(ZZ, Ideal) := Ideal => o -> (n1, I1) -> (
	reflexify(idealPower(n1, I1), Strategy=>o.Strategy)
);

--****************************************************--
--*****************Documentation**********************--
--****************************************************--

beginDocumentation();

document {
    Key => Divisor,
    Headline => "divisors",
    EM "Divisor", " is a package for working with (Q/R)-Weil divisors on ", EM "normal", " affine and projective varieties (equivalently, on commutative, normal and graded rings).", 
    BR{},BR{},
    "This package introduces a type ", TO "WeilDivisor", " which lets the user work with Weil divisors similar to the way one might in algebraic geometry.  We highlight a few important functions below.",
    BR{},BR{},
    BOLD "Useful functions:",BR{},
    UL {
	  {TO "isCartier", " or ", TO "isQCartier", " can let you determine if a divisor is Cartier or if a power is Cartier."},
	  {TO "isVeryAmple", " lets you check if a divisor is very ample." },
	  {TO "baseLocus", " lets you compute the base locus of the complete linear system corresponding to a divisor on a projective variety." },
	  {TO "mapToProjectiveSpace", " returns the map to projective space determined by the complete linear system determined by the divisor." },
	  {TO "canonicalDivisor", " lets you compute the canonical divisor on some affine or projective variety." },
	  {TO "ramificationDivisor", " lets you compute the relative canonical divisor of a finite map varieties." },
	},
	BR{},
	"This package also includes some functions for interacting with ideals and modules which might be independently useful.", BR{},
	UL {
	  {TO "embedAsIdeal", " embeds a rank one module as an ideal."},
	  {TO "reflexify", " computes the reflexification, Hom(Hom(M, R), R) of a module M or ideal."},
	  {TO "reflexivePower", " computes the reflexification of a power of an ideal quickly."},
	  {TO "torsionSubmodule", " find the torsion submodule of a module."},
	}, BR{},
	"We emphasize once more that the functions in this package might produce unexpected results on non-normal rings.",BR{},BR{},
	BOLD "Acknowledgements:",BR{},BR{},
	"The authors would like to thank Tommaso de Fernex, David Eisenbud, Daniel Grayson, Anurag Singh, Greg Smith, Mike Stillman and the referee for useful conversations and comments on the development of this package.",BR{}
}

doc ///
	Key
	 BasicDivisor
	 RWeilDivisor
	 QWeilDivisor
	 WeilDivisor
	Headline
	 the Types of divisors
	Description
	 Text
	  The BasicDivisor is the class of divisors whose coefficients are unspecified, a base class.  Not typically for use.  All subtypes have the same essential structure.  
	 Text
	  RWeilDivisor is a subclass which has real coefficients.  
	 Text
	  QWeilDivisor is a further subclass with rational coefficients. 
	 Text
	  WeilDivisor is a subclass with integer coefficients. 	    
	 Text
	  The basic structure is a HashTable.  There is one key which has a value which specifies the ambient ring.  Another key is cache which points to a CacheTable.  The remaining keys are a Groebner basis $L$ for each prime ideal $P$ in the support with corresponding value a list with one entry {$n$} where $n$ is the coefficient of the height one prime.  
	 Example
	  R = QQ[x,y,z];
	  D = divisor(x*y^2*z^3)
	  H = new HashTable from D
	  (2/3)*D
	  0.6*D
///

--doc ///
--	Key
--	 (net, BasicDivisor)
--	Headline
--	 controls how divisors are displayed to the user
--  Description
--	 Text
--	  Currently divisors are listed as sums with the ambient ring stated at the end.
--///



doc ///
	Key
	 Safe
	Headline
	 an option used to tell functions whether not to do checks.
	Description
	 Text
	  If set to {\tt true}, then certain functions will perform checks to make sure the user didn't pass something unreasonable.  You can use {\tt isWellDefined} to ensure that a constructed divisor is constructed correctly.
	SeeAlso
	 isWellDefined
///

doc ///
	Key
	 CoefficientType
	Headline
	 an option used to tell divisor construction that a particular type of coefficients are expected.
	Description
	 Text
	  Can be set to {\tt ZZ}, {\tt QQ} or {\tt RR} (or any other {\tt Thing}) when divisor is called, it checks whether the coefficient list is a list of instances of this type.  If it is set to {\tt ZZ}, {\tt QQ} or {\tt RR} then a {\tt WeilDivisor}, {\tt QWeilDivisor} or {\tt RWeilDivisor} are created.  Default value is {\tt ZZ}.
///

doc ///
	Key
	 AmbientRing
	Headline
	 an option used to tell divisor construction that a particular ambient ring is expected.
	Description
	 Text
	  If set to a ring, then when calling divisor, the primes are all checked whether they are ideals in that ring. 
///

doc ///
	Key
	 MTries
	Headline
	 an option used by embedAsIdeal how many times to try embedding the module as an ideal in a random way.
	Description
	 Text
	  After making some canonical attempts, embedAsIdeal tries to embed the module into a ring as an ideal in a random way.  The value of this option is how many times that random embedding is attempted.  The default value is 10.
///

doc ///
	Key
	 IsGraded
	Headline
	 an option used by numerous functions which tells it to treat the divisors as if we were working on the Proj of the ambient ring.
	Description
	 Text
	  An option used by numerous functions which tells it to treat the divisors as if we were working on the Proj of the ambient ring.  In other words, setting it to {\tt true} tells the function to ignore behavior at the irrelevant ideal (the ideal generated by vars Ring).  Default value is {\tt false}.
///

doc /// 
	 Key
		divisor
		(divisor, BasicList, BasicList)
		(divisor, BasicList)
		(divisor, Ideal)
		(divisor, Module)
		(divisor, Matrix)
		(divisor, RingElement)
		[divisor, CoefficientType]
		[divisor, AmbientRing]
		[divisor, Section]
		[divisor, IsGraded]
     Headline
     	constructor for (Weil/Q/R)-divisors
     Usage
     	divisor(l1, l2)
     	divisor( l3)
     	divisor( I )
     	divisor( f )
     	divisor( M )
     	divisor( Mat )
     Inputs
      	l1: BasicList
      		which describes the list of coefficients in integers
      	l2: BasicList
      		which describes the list of height one prime ideals that corresponds to codimension one irreducible subspaces
      	l3: BasicList
      		a list of pairs {c, P} where c is a coefficient and P is a prime ideal
      	I: Ideal
      		construct a divisor out of the vanishing locus of I
      	f: RingElement
      		construct a divisor out of the vanishing locus of ideal(f)
      	M: Module
      		construct a divisor such that O(D) is isomorphic to M
      	Mat: Matrix      	
      		a matrix, construct an effective divisor such that O(D) is isomorphic to the target of Mat based on the section
      	CoefficientType => ZZ
      		specify the coefficients of your divisor 
      	AmbientRing => Ring
      		specify the ambient ring in the divisor constructor
      	Section => RingElement
      		specify a global section of the ideal I used to construct an effective divisor
      	Section => Matrix
      	    specify a global section of a module M used to construct an effective divisor
      	IsGraded => Boolean
      	    specify a that a divisor constructed from a module should view the module as a graded object
     Outputs
      	 : BasicDivisor
     Description
      Text
		This is the general function for constructing divisors.  There are many ways to call it.  In our first example, we construct divisors on $A^3$ (which can also be viewed as divisors on $P^2$ since the ideals are homogeneous).  The following creates the same Weil divisor with coefficients 1, 2 and 3 in five different ways.
      Example
       R = QQ[x,y,z];
       D = divisor({1,2,3}, {ideal(x), ideal(y), ideal(z)})
       E = divisor(x*y^2*z^3)
       F = divisor(ideal(x*y^2*z^3))
       G = divisor({{1, ideal(x)}, {2, ideal(y)}, {3, ideal(z)}})
       H = divisor(x) + 2*divisor(y) + 3*divisor(z)
      Text
       Next we construct the same divisor in two different ways.  We are working on the quadric cone, and we are working with a divisor of a ruling of the cone.  This divisor is not Cartier, but 2 times it is.
      Example
       R = QQ[x,y,z]/ideal(x^2-y*z);
       D = divisor({2}, {ideal(x,y)})
       E = divisor(y)
      Text
       Here is a similar example in a slightly more complicated Veronese ring.
      Example
       R = QQ[x,y,z]; 
       S = QQ[x3,x2y, x2z, xy2, xyz, xz2, y3, y2z, yz2, z3];
       f = map(R, S, {x^3, x^2*y, x^2*z, x*y^2, x*y*z, x*z^2, y^3, y^2*z, y*z^2, z^3});
       A = S/(ker f);
       D = divisor(x3)
       E = divisor(y2z)
      Text
       We can construct a Q-divisor as well.  Here are two ways to do it (we work in $A^2$ this time).
      Example
       R = ZZ/7[x,y];
       D = divisor({-1/2, 2/1}, {ideal(y^2-x^3), ideal(x)}, CoefficientType=>QQ)
       D = (-1/2)*divisor(y^2-x^3) + (2/1)*divisor(x)
      Text
       Or an R-divisor.  This time we work in the cone over $P^1 \times P^1$.
      Example
       R = ZZ/11[x,y,u,v]/ideal(x*y-u*v);
       D = divisor({1.1, -3.14159}, {ideal(x,u), ideal(x, v)}, CoefficientType=>RR)
       D = 1.1*divisor(ideal(x,u)) - 3.14159*divisor(ideal(x,v))
      Text
       You can also pass it an element of the ring or even the fraction field.  
      Example
       R = QQ[x,y];
       divisor(x)
       divisor(x/y)
      Text
       Given a rank 1 reflexive module {\tt M}, {\tt divisor(M)} finds a divisor $D$ such that $O(D)$ is isomorphic to {\tt M}.  If {\tt IsGraded} is {\tt true} (it is {\tt false} by default) this assumes we are working on the Proj of the ambient ring.
      Example
       R = QQ[x,y,z]/ideal(x^2-y*z);
       M = (ideal(y*x,y*z))*R^1;
       divisor(M)
       divisor(M, IsGraded=>true)
      Text
       Finally, {\tt divisor(Matrix)} assumes that the matrix is a map from a rank-1 free module to the module corresponding to $O(D)$.  In that case, this function returns the effective divisor corresponding to that section.  The same behavior can also be obtained by calling {\tt divisor(Module, Section=>Matrix)} where the {\tt Matrix} is a map from a rank-1 free module to {\tt M}.  In the following example, we demonstrate this by considering a rank-1 module (on the cone of $P^1 \times P^1$), and considering the map from $R^1$ mapping to the first generator of the module.
      Example
       R = QQ[x,y,u,v]/ideal(x*y-u*v);
       M = (ideal(x,u))*R^1;
       matr = map(M, R^1, {{1},{0}});
       divisor(matr)
       divisor(M, Section=>matr)
      Text
       One can also obtain the same behavior (as {\tt divisor(Matrix)}) by passing the divisor either an ideal or a module and then specifying a global section of that object (which will produce the corresponding effective divisor).  In particular, if the main argument in the divisor is an Ideal, the option {\tt Section=>f} specifies that we should find the unique effective divisor $D$ such that I is isomorphic to $O(D)$ and such that {\tt f} maps to 1 under that isomorphism.
      Example
       R = QQ[x,y,u,v]/ideal(x*y-u*v)
       D = divisor(ideal(x,u), Section=>x)
      Text
       Note if the section is not in $I$, then it is interpreted as a rational section and the produced divisor $D$ may not be effective.
      Text
       If the main argument in the divisor is a module, then the Matrix {\tt Mat} should be a matrix mapping a free module to {\tt M}.  In this case {\tt divisor} constructs the unique effective divisor $D$ such that {\tt M} is isomorphic to $O(D)$ and so that $1$ in the matrix map is mapped to $1$ in $O(D)$.
      Example
       R = QQ[x];
       D = divisor(R^1, Section=>matrix{{x^2}})       
///




doc ///
    Key
        (isWellDefined, BasicDivisor)
    Headline
        whether a divisor is valid
    Usage
        isWellDefined( D1 )
    Inputs
        D1: BasicDivisor
    Outputs
        : Boolean
    Description
        Text
            This function tries to verify that this is a valid divisor.  It checks that the coefficients are from the right ring (in the {\tt WeilDivisor/QWeilDivisor/RWeilDivisor} cases at least).  It also checks to make sure all the ideals are from the same ring, are prime, and have height one.  If debugLevel > 0, the function will print an message explaining why the divisor was not valid.
        Example
            debugLevel = 1;
            R = QQ[x,y];
            isWellDefined(divisor({1}, {ideal(x)} ))
            isWellDefined(divisor({1/2}, {ideal(x)} ))
            isWellDefined(divisor({1/2}, {ideal(x)}, CoefficientType=>QQ))
            isWellDefined(divisor({1}, {ideal(x,y)}))
            isWellDefined(divisor({1}, {ideal(x^2)}))
            S = QQ[a,b];
            isWellDefined(divisor({1,2}, {ideal(x), ideal(a)}))
    SeeAlso
        divisor
///


doc ///
	Key
	 primes
	 (primes, BasicDivisor)
	Headline
	 get the list of height-one primes in the support of a divisor
	Usage
	 primes( D1 )
	Inputs
	 D1: BasicDivisor
	Outputs
	 : List
	Description
	 Text
	  This function returns the list of height-one prime ideals corresponding to the components of a {\tt BasicDivisor}.  Note that if you don't call {\tt cleanSupport}, this can return primes with coefficient equal to zero.
	 Example
	  R = QQ[x,y,u,v]/ideal(x*y-u*v);
	  D = divisor(x)
	  primes(D)
	  E = divisor(x*u)
	  primes(E)
	  F = divisor({0}, {ideal(x,u)})
	  primes(F)
	  primes(cleanSupport F)
	  primes(1*F)
	SeeAlso
	 (coefficients, BasicDivisor)
	 gbs
///

doc ///
	Key
	 getPrimeCount
	 (getPrimeCount, BasicDivisor)
	Headline
	 get the number of height-one primes in the support of the divisor
	Usage
	 getPrimeCount( D1 )
	Inputs
	 D1: BasicDivisor
	Outputs
	 : List
	Description
	 Text
	  This function returns the number of height one prime ideals corresponding to the components of a {\tt BasicDivisor}.  Note that if you don't call {\tt cleanSupport}, this can return primes with coefficient equal to zero.
	 Example
	  R = QQ[x,y,u,v]/ideal(x*y-u*v);
	  D = divisor(x)
	  getPrimeCount(D)
	  E = divisor(x*u)
	  getPrimeCount(E)
	  F = divisor({0}, {ideal(x,u)})	  
	  getPrimeCount(F)
	  getPrimeCount(cleanSupport F)
	  getPrimeCount(1*F)	  
	 Text
	  This is equivalent to {\tt #primes}.
	SeeAlso
	 (coefficients, BasicDivisor)
	 gbs
///

doc ///
	Key
	 gbs
	 (gbs, BasicDivisor)
	Headline
	 get the list of Groebner bases corresponding to the height-one primes in the support of a divisor
	Usage
	 gbs( D1 )
	Inputs
	 D1: BasicDivisor
	Outputs
	 : List
	Description
	 Text
	   This function returns the list of Groebner bases associated to the height-one prime ideals corresponding to the components of a {\tt BasicDivisor} (or a {\tt WeilDivisor}, {\tt QWeilDivisor} or {\tt RWeilDivisor}).  Note that this list of Groebner bases is made when the divisor is constructed.
	 Example
	  R = ZZ/7[x,y,u,v]/ideal(x*y-u*v);
	  D = divisor(x)
	  gbs(D)
	 Text
	  Note, the Grobner basis can be different from a minimal set of generators the user provides.
	 Example
	  R = ZZ/2[x,y,z]/ideal(z^2+x*y*z+x^2*y+x*y^2);
	  J = ideal(x+y, x^2+z);
	  D = divisor({2}, {J})
	  gbs(D)
	  primes(D)
	SeeAlso
	 (coefficients, BasicDivisor)
	 primes
///
	
doc ///
	Key
	 (coefficients, BasicDivisor)
	Headline
	 get the list of coefficients of a divisor
	Usage
	 coefficients( D1 )
	Inputs
	 D1: BasicDivisor
	Outputs
	 : List
	Description
	 Text
	  Get the list of coefficients of a {\tt BasicDivisor} (or a {\tt WeilDivisor}, {\tt QWeilDivisor} or {\tt RWeilDivisor}).
	 Example
	  R = QQ[x,y,u,v]/ideal(x*y-u*v);
	  D = divisor(x);
	  coefficients(D)
	  E = divisor(x*u);
	  coefficients(E)
	  F = divisor({0, 1/2, -2/3}, {ideal(y, u), ideal(x,u), ideal(x,v)}, CoefficientType => QQ)
	  coefficients(F)
	  G = divisor({0.5, -0.667}, {ideal(x,u), ideal(x,v)}, CoefficientType => RR)
	  coefficients(G)
	SeeAlso
	 (coefficient, Ideal, BasicDivisor)
	 primes
	 gbs
///

doc /// 
	Key
	 cleanSupport
	 (cleanSupport, BasicDivisor)
  	Headline
   	 removes primes with coefficient zero from a divisor
   	Usage
   	 cleanSupport( D1 )
     	Inputs
      	 D1: BasicDivisor
     	Outputs
      	 : BasicDivisor   
     	Description  	
     	 Text
     	  This function returns a divisor where all entries with coefficient zero are removed. 
     	 Example
     	  R = QQ[x,y,z];
     	  D = divisor({1,0,-2}, {ideal(x), ideal(y), ideal(z)})
     	  cleanSupport(D)
///

doc /// 
	Key
	 clearCache
	 (clearCache, BasicDivisor)
  	Headline
   	 creates a new divisor with most entries from the cache removed
   	Usage
   	 clearCache( D1 )
     	Inputs
      	 D1: BasicDivisor
     	Outputs
      	 : BasicDivisor   
     	Description  	
     	 Text
     	  This function returns a divisor where the only entries left in the cache is the key ideals (which points to how the ideals should be displayed to the user).
     	 Example
     	  R = QQ[x,y,z];
     	  D = divisor(x);
     	  isPrincipal(D)
     	  peek (D#cache)
     	  E = clearCache(D);
     	  peek (E#cache)
///

doc /// 
	Key
	 (trim, BasicDivisor)
  	Headline
   	 trims the ideals displayed to the user and removes primes with coefficient zero
   	Usage
   	 trim( D1 )
     	Inputs
      	 D1: BasicDivisor
     	Outputs
      	 : BasicDivisor   
     	Description  	
     	 Text
     	  This function returns a divisor where all entries with coefficient zero are removed and where the ideals displayed to the user are trimmed.
     	 Example
     	  R = QQ[x,y,z]/ideal(x*y-z^2);
     	  D = divisor({1,0,-2}, {ideal(x, z), ideal(x-z,y-z), ideal(y+z, z)});
     	  cleanSupport(D)
     	  trim(D)
     	  D == trim(D)
///


doc ///
	Key 
	 (coefficient, Ideal, BasicDivisor)
	Headline
	 get the coefficient of an ideal for a fixed divisor
	Usage
	 coefficient(P, D)
	Inputs
	 P: Ideal
	    the height one prime ideal that we want to find the coefficient of
	 D: BasicDivisor
	    the divisor in question		 
	Outputs	
	 : Number
	Description
	 Text
	  This function returns the coefficient of $D$ along the prime divisor associated to $P$.
	 Example
	  R = QQ[x,y,u,v]/ideal(x*y-u*v);
	  D = divisor(x)
	  coefficient(ideal(x,u), D)
	  E = divisor(x*u)
	  coefficient(ideal(x,u), E)
	SeeAlso
	 (coefficient, BasicList, BasicDivisor)
	 (coefficients, BasicDivisor)
///

doc ///
	Key 
	 (coefficient, BasicList, BasicDivisor)
	Headline
	 get the coefficient of an ideal for a fixed divisor
	Usage
	 coefficient(L, D)
	Inputs
	 L: BasicList
	    the list of elements in a Grobner basis of a height one prime ideal that we want to find the coefficient of
	 D: BasicDivisor
	    the divisor in question		 
	Outputs	
	 : Number
	Description
	 Text
	  This function returns the coefficient of $D$ along the prime divisor generated by $L$ assuming $L$ is the ordered list of an element 
	 Example
	  R = QQ[x,y,u,v]/ideal(x*y-u*v);
	  E = divisor(x*u)
	  coefficient({u, x}, E)
	  coefficient({x, u}, E)
	SeeAlso
	 (coefficient, Ideal, BasicDivisor)	  
	 (coefficients, BasicDivisor)
///



doc ///
	 Key		
		(ring, BasicDivisor)
	Headline
		get the ambient ring of a divisor
	Usage
		ring( D1 )
	Inputs
		D1: BasicDivisor
	Outputs
		: Ring
	Description
	  Text
	   This function returns the ambient ring of a divisor.
	  Example
	   R = QQ[x, y, z] / ideal(x * y - z^2 );
	   D = divisor({1, 2}, {ideal(x, z), ideal(y, z)})
	   ring( D )
///

doc ///
	 Key
		isVeryAmple
		(isVeryAmple, WeilDivisor)
		[isVeryAmple, Verbose]
	Headline
		whether a divisor is very ample.
	Usage
		isVeryAmple( D1 )
	Inputs
		D1: WeilDivisor		
		Verbose => Boolean
		  pass the verbose option to the called isEmbedding method
	Outputs
		: Boolean
	Description
	  Text
	   This function returns {\tt true} if the divisor is very ample, otherwise it returns {\tt false}.  It works by calling {\tt isEmbedding} from the {\tt RationalMaps} package.  If {\tt Verbose} is set to {\tt true}, it will print {\tt Verbose} output from {\tt isEmbedding}.
	  Example
	   R = QQ[x, y, z]/ideal(x^3 + y^3 - z^3);
	   D = divisor(ideal(x, y-z));
	   isVeryAmple(D)
	   isVeryAmple(2*D)
	   isVeryAmple(3*D)	 
	  Text
	   The output value of this function is stored in the divisor's cache.  
///


doc ///
	 Key
		getPrimeDivisors
		(getPrimeDivisors, BasicDivisor)
	Headline
		get the list of prime divisors of a given divisor
	Usage
		getPrimeDivisors( D1 )
	Inputs
		D1: BasicDivisor
	Outputs
		: List
	Description
	  Text
	   This function returns the list of prime divisors of a given divisor.  The prime divisors are all of the class {\tt WeilDivisor}.  If you do not call {\tt cleanSupport}, you may obtain divisors with zero coefficients.
	  Example
	   R = QQ[x, y, z];
	   D = divisor({-8, 2, 0}, {ideal(x), ideal(y), ideal(x^2+z)})
	   getPrimeDivisors( D )
	   getPrimeDivisors( cleanSupport D )
///

doc ///
	Key 
		(ceiling, RWeilDivisor)
		(floor, RWeilDivisor)
	Headline
		produce a WeilDivisor whose coefficients are ceilings or floors of the divisor
	Usage
		ceiling( E1 )
		floor( E1 )
	Inputs
		E1: RWeilDivisor
	Outputs
		: WeilDivisor
	Description
	 Text
	  Start with a rational or real Weil divisor.  We form a new divisor whose coefficients are obtained by applying the {\tt ceiling} or {\tt floor function} to them.
	 Example
	  R = QQ[x, y, z] / ideal(x *y - z^2);
	  D = divisor({1/2, 4/3}, {ideal(x, z), ideal(y, z)}, CoefficientType => QQ)
	  ceiling( D )
	  floor( D )
	  E = divisor({0.3, -0.7}, {ideal(x, z), ideal(y,z)}, CoefficientType => RR)
	  ceiling( E )
	  floor( E )
///

doc ///
	Key
	 positivePart
	 (positivePart, RWeilDivisor)
	 negativePart
	 (negativePart, RWeilDivisor)
	Headline
	 get the effective part or anti-effective part of a divisor
	Usage
	 positivePart( F1 )
	 negativePart( F1 )
	Inputs
	 F1: RWeilDivisor
	Outputs
	 : RWeilDivisor
	  an effective divisor
	Description
	 Text
	  This function returns the positive part of a divisor
	 Example
	  R = QQ[x, y, u, v] / ideal(x * y - u * v);
	  D = divisor({1, -2, 3, -4}, {ideal(x, u), ideal(y, u), ideal(x, v), ideal(y, v)})
	  positivePart( D )
	  negativePart( D )
	  D == positivePart(D) - negativePart(D)
	  E = divisor({0, 1}, {ideal(x,u), ideal(y,u)})
	  positivePart(E)
	  negativePart(E)
	  E == positivePart(E) - negativePart(E)
///


doc ///
	 Key
	  (symbol ==, RWeilDivisor, RWeilDivisor)
	 Headline
	  whether two divisors are equal
	 Usage
	  (D == E)
	 Inputs
	 	D: RWeilDivisor
	 	E: RWeilDivisor
	 Outputs
	 	: Boolean
	 Description
	 	Text
	 	 This function returns {\tt true} if the two divisors are equal
	 	Example 
	 	 R = QQ[x,y];
	 	 D = divisor(x*y);
	 	 E = divisor(x);
	 	 F = divisor(y);
	 	 D == E
	 	 D == E+F
	 	Text
	 	 Here is an example with rational coefficients compared with integer coefficients.
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
	 	toQWeilDivisor
	 	(toQWeilDivisor, WeilDivisor)
	 	(toQWeilDivisor, QWeilDivisor)
	 Headline
	 	create a Q-Weil divisor from a Weil divisor
	 Usage
	 	toQWeilDivisor( D )
	 Inputs
	 	D: QWeilDivisor
	 Outputs
	 	: QWeilDivisor
	 Description
	  Text
	   Turn a Weil divisor into a Q-divisor (or do nothing to a Q-divisor).
	  Example
	   R = ZZ/5[x, y];
	   D = divisor(x);
	   E = toQWeilDivisor(D)
	   toQWeilDivisor(E)
	   F = divisor({3, 0, -2}, {ideal(x), ideal(y), ideal(x+y)})
	 SeeAlso
	 	toWeilDivisor
	 	toRWeilDivisor
///

doc ///
	Key
		toRWeilDivisor
		(toRWeilDivisor, WeilDivisor)
		(toRWeilDivisor, QWeilDivisor)
		(toRWeilDivisor, RWeilDivisor)
	Headline
		create a R-divisor from a Q or Weil divisor
	Usage
		toRWeilDivisor( D1 )
		toRWeilDivisor( E1 )
	Inputs
		D1: WeilDivisor
		E1: QWeilDivisor
		E1: RWeilDivisor
	Outputs
		: RWeilDivisor
	Description
	  Text
	   Turn a Weil divisor or a Q-divisor into a R-divisor (or do nothing to a R-divisor).
	  Example
	   R = ZZ/5[x,y];
	   D = divisor({2, 0, -4}, {ideal(x), ideal(y), ideal(x-y)})
	   E = (1/2)*D
	   F = toRWeilDivisor(D)
	   G = toRWeilDivisor(E)
	   F == 2*G
	SeeAlso
		toWeilDivisor
		toQWeilDivisor		
///

doc ///
	Key
	 	toWeilDivisor
	 	(toWeilDivisor, RWeilDivisor)
	Headline
		create a Weil divisor from a Q or R-divisor
	Usage
		toWeilDivisor( E1 )
	Inputs
		E1: RWeilDivisor
	Outputs
		: WeilDivisor
	Description
	 Text
	  Given a divisor with rational or real coefficients, but whose coefficients are actually integers, we first check if all coefficients are integers.
	  If so we make this Weil divisor.  Otherwise, an error is thrown.
	 Example
	  R=QQ[x];
	  D=divisor({3/2}, {ideal(x)}, CoefficientType=>QQ)
	  E=divisor({1.5}, {ideal(x)}, CoefficientType=>RR)
	  toWeilDivisor(2*D)
	  toWeilDivisor(2*E)
	  isWeilDivisor(D)
	  try toWeilDivisor(D) then print "converted to a WeilDivisor" else print "can't be converted to a WeilDivisor"
	 Text
	  Notice in the final computation, {\tt D} cannot be converted into a Weil divisor since $D$ has non-integer coefficients, but {\tt 2*D} can be converted into a Weil divisor.
	SeeAlso
		toQWeilDivisor
		toRWeilDivisor
		isWeilDivisor
///

doc ///
	Key
		(symbol *, Number, BasicDivisor)			
		(symbol *, QQ, WeilDivisor)		
		(symbol *, RR, QWeilDivisor)
		(symbol *, QQ, RWeilDivisor)		
		(symbol *, RR, RWeilDivisor)			    
	Headline
		multiply a divisor by a number
	Usage
		n * D
 	Inputs
 		n: Number
 		D: BasicDivisor
 	Outputs
 		: BasicDivisor
 	Description
 	 Text
 	 	Multiply a divisor by an integer or a real or rational number.
 	 Example
 	  R = QQ[x,y];
 	  D = divisor(x^2*y/(x+y));
 	  E = divisor({1/2, -5/3}, {ideal(x), ideal(y)}, CoefficientType=>QQ)
 	  F = divisor({1.5, 0, -3.2}, {ideal(x), ideal(y), ideal(x^2-y^3)}, CoefficientType=>RR)
 	  8*D
 	  (-2/3)*D
 	  0.0*D
 	  (3/2)*E
 	  (-1.414)*E
 	  6*F
 	  (-3/2)*F
///

doc ///
  Key
    (symbol +, BasicDivisor, BasicDivisor)
    (symbol -, BasicDivisor, BasicDivisor)
    (symbol -, BasicDivisor)
  Headline
    add or subtract two divisors, or negate a divisor
  Usage
    A + B
    A - B
    -A
  Inputs
    A:BasicDivisor
    B:BasicDivisor
  Outputs
    :BasicDivisor
  Description
    Text
      We can add or subtract two divisors:
    Example
      R = QQ[x, y, z];
      D1 = divisor({1, 3, 2}, {ideal(x), ideal(y), ideal(z)})
      D2 = divisor({-2, 3, -5}, {ideal(z), ideal(y), ideal(x)})
      D1 + D2
      D1 - D2
    Text
      We can also add or subtract divisors with different coefficients.
    Example
      R = QQ[x,y];
      D1 = divisor({3, 1}, {ideal(x), ideal(y)})
      D2 = divisor({3/2, -1}, {ideal(x), ideal(y)}, CoefficientType=>QQ)
      D3 = divisor({1.25}, {ideal(x)}, CoefficientType=>RR)
      D1+D2
      D1+D3
      D2+D3
    Text
      Finally, we can negate a divisor.
    Example
      R = ZZ/3[x,y,z]/ideal(x^2-y*z);
      D = divisor({3, 0, -1}, {ideal(x,z), ideal(y,z), ideal(x-y, x-z)})
      -D
      E = divisor({3/2, -2/3}, {ideal(x, z), ideal(y, z)})
      -E
///

doc ///
 	Key
 	 (symbol SPACE, OO, RWeilDivisor)
 	Headline
 	 calculate module corresponding to divisor
 	Usage
 	 OO( D1 )
 	Inputs
 	 D1: RWeilDivisor
 	Outputs
 	 : Module
 	Description
 	 Text
 	  Get the associated module $O(D)$ of a Weil Divisor $D$.  In the affine case, $O(D)$ is by definition the set of elements $f$ of the fraction field such that $D + Div(f) \geq 0$.  We represent this as a module.  In the projective case, $O(D)$ is the coherent sheaf of such elements, and hence we represent it as a graded module.  For example, consider the following modules on $P^2$.
 	 Example
 	  R = ZZ/7[x,y,z];
 	  D = divisor(x);
 	  OO(D)
 	  OO(2*D) 	  
 	  OO(0*D)
 	  OO(-3*D)
 	 Text
 	  Next, consider an example on $P^1 \times P^1$.  
 	 Example
 	  R = QQ[x, y, u, v] / ideal(x * y - u * v);
 	  D1 = divisor(ideal(x, u))
 	  D2 = divisor(ideal(x, v))
 	  OO( D1 )
 	  OO(D1 + D2)
 	 Text
 	  To get the associated module $O(D)$ for a rational/real divisor $D$, we first obtain a new divisor $D'$ whose coefficients are the floor of the coefficients of $D$, and take $O(D')$ as $O(D)$.
 	 Example
 	  R = QQ[x, y, u, v] / ideal(x * y - u * v);
 	  D2 = divisor({3/5, -4/7, 9/4, -7/8}, {ideal(x, u), ideal(x, v), ideal(y, u), ideal(y, v)}, CoefficientType=>QQ)
 	  OO( D2 )
 	  OO( floor(D2) )
 	 Text
 	  Note that you can call the {\tt divisor} constructor on the module you construct, but it will only produce a divisor up to linear equivalence (which can mean different things depending on whether or not you are keeping track of the grading).
 	 Example
 	  R = ZZ/11[x,y];
 	  D = divisor(x*y/(x+y))
 	  divisor(OO(D))
 	  divisor(OO(D), IsGraded=>true)
 	 Text
 	  The output value of this function is stored in the divisor's cache. 	  
 	SeeAlso
 	 (ideal, RWeilDivisor)
///

doc ///
 	Key
 	 idealPower
 	 (idealPower, ZZ, Ideal)
 	Headline
 	 compute the ideal generated by the generators of the ideal raised to a power
 	Usage
 	 idealPower(n, I)
 	Inputs
 	 n: ZZ
 	 I: Ideal
 	Outputs
 	 : Ideal
 	Description
 	 Text
 	  If {\tt I} is generated by $(f1, ..., fk)$ then {\tt idealPower(n, I)} is the ideal generated by $(f1^n, ..., fk^n)$.  This is relevant because {\tt idealPower(n, I)} and {\tt I^n} have the same reflexification, but {\tt idealPower(n, I)} can be much faster to compute with since it has fewer generators typically.
 	 Example
 	  R = QQ[x, y, u, v] / ideal(x * y - u * v);
 	  I = ideal(x, u);
 	  idealPower(5, I)
 	  I^5
 	SeeAlso
 	 reflexify
///

doc ///
 	Key
 	 (ideal, RWeilDivisor)
 	 (ideal, QWeilDivisor)
 	 (ideal, WeilDivisor)
 	Headline
 	 calculate the corresponding module of a divisor and represent it as an ideal
 	Usage
 	 ideal( D1 )
 	Inputs
 	 D1: RWeilDivisor
 	Outputs
 	 : Ideal
 	Description
 	 Text
 	  Get an ideal associated to $O(-D)$ of a Weil Divisor $D$.  Recall that on an affine scheme, $O(-D)$ is by definition the subset of the fraction field made up of elements such that $Div(f) - D \geq 0$.  If $D$ is effective, this will produce the ideal corresponding to $O(-D)$.  Otherwise, it will produce some ideal isomorphic to a module corresponding to $O(-D)$. 
 	 Example
 	  R = QQ[x, y, u, v] / ideal(x * y - u * v);
 	  D1 = divisor({1, -2, 3, -4}, {ideal(x, u), ideal(x, v), ideal(y, u), ideal(y, v)})
 	  ideal( D1 )
 	  D2 = divisor(ideal(x,u))
 	  ideal(D2)
 	 Text
 	  Note, if the divisor has non-integer coefficients, their ceilings will be taken, since $O(-D) = O({\tt floor}(-D)) = O(-{\tt ceiling}(D))$.
 	 Example
 	  R = QQ[x,y,z]/(ideal(x^3 + y^3 - z^3));
 	  D1 = 1.3*divisor(ideal(x, y-z))
 	  ideal D1
 	  I1 = ideal (ceiling(D1))
 	  I2 = ideal (-ceiling(D1))
 	  reflexify(I1*I2)
 	 Text
 	  The output value of this function is stored in the divisor's cache.
 	SeeAlso
 	 (symbol SPACE, OO, RWeilDivisor)
///

doc ///
	Key
	 mapToProjectiveSpace
	 (mapToProjectiveSpace, WeilDivisor)
	 [mapToProjectiveSpace, KnownCartier]
	 [mapToProjectiveSpace, Variable]
	Headline
	 compute the map to projective space associated with the global sections of a Cartier divisor
	Usage
	 mapToProjectiveSpace( D )
	Inputs
	 D: WeilDivisor
	 KnownCartier => Boolean
	   specify whether the divisor is known to be Cartier
	 Variable => Symbol
	 Variable => String
	   specify the variable to use in the construction of the target projective space 
	Outputs
	 : RingMap
	Description
	 Text
	  Given a Cartier divisor $D$ on a projective variety (represented by a divisor on a normal standard graded ring), this function returns the map to projective space induced by the global sections of $O(D)$.  If {\tt KnownCartier} is set to {\tt false} (default is {\tt true}), the function will also check to make sure the divisor is Cartier away from the irrelevant ideal.
	 Example
	  R = QQ[x,y,u,v]/ideal(x*y-u*v);
	  D = divisor( ideal(x, u) )
	  mapToProjectiveSpace(D)
	 Text
	  The user may also specify the variable name of the new projective space.
	 Example 
	  R = ZZ/7[x,y,z];
	  D = divisor(x*y)
	  mapToProjectiveSpace(D, Variable=>"Z")	  
	SeeAlso
	 isCartier
///

doc ///
	Key
	 baseLocus
	 (baseLocus, WeilDivisor)
	 (baseLocus, Module)
	Headline
	 compute the locus where a graded module (or O(D) of a Weil divisor) is not globally generated
	Usage
	 I = baseLocus(D)
	 I = baseLocus(M)
	Inputs
	 D: WeilDivisor
	 M: Module
	Outputs
	 I: Ideal
	Description
	 Text
	  Given a graded module $M$ with degree 0 global sections $s1, ..., sd$, this computes the locus where the $si$ do not generate $M$.  Given a Weil divisor $D$, this computes the base locus of $O(D)$.  For example, consider the rulings on $P^1 \times P^1$.
	 Example
	  R = QQ[x,y,u,v]/ideal(x*y-u*v);
	  D = divisor( ideal(x,u) )
	  baseLocus(D)
	 Text
	  Next we consider an example of a point on an elliptic curve.
	 Example
	  R = QQ[x,y,z]/ideal(y^2*z-x*(x+z)*(x-z));
	  D = divisor(ideal(y, x))
	  baseLocus(D)
	  baseLocus(2*D)
///


doc ///
	Key
	 reflexify
	 (reflexify, Ideal)
	 (reflexify, Module)
	 [reflexify, Strategy]
	 [reflexify, ReturnMap]
	 [reflexify, KnownDomain]
	Headline
	 calculate the double dual of an ideal or module Hom(Hom(M, R), R)
	Usage
	 reflexify( I1 )
	 reflexify( M1 )
	Inputs
	 I1: Ideal
	 M1: Module
	 b: Boolean
	 Strategy => Symbol
	   specify a strategy for reflexify
	 ReturnMap => Boolean
	   tell reflexify to return a map from the original module to the reflexification
	 KnownDomain => Boolean
	   make reflexify more efficient if the ambient ring is a domain
	Outputs
	 : Ideal
	 : Module
	Description
	 Text
	  Get the reflexification or double dual (in the case of a normal ring, S2-ification) of an ideal $I$ or module $M$. Recall the double dual is defined to be $Hom(Hom(M, R), R)$. 
	 Example
	  R = QQ[x,y,z]/ideal(x^2-y*z);
	  m = ideal(x,y,z);
	  reflexify(m)
	  I = ideal(x,y);
	  reflexify(I)
	  reflexify(I^2)
	  reflexify(I^3)
	 Text
	  We also have an example of reflexifying a module.
	 Example
	  R = QQ[x,y,z]/ideal(x^2-y*z);
	  m = ideal(x,y,z);
	  prune reflexify(m*R^2)
	  I = ideal(x,y);
	  prune reflexify(I*R^1)
	  prune reflexify(I^2*R^1)
	 Text
	  There is a canonical map from a module $M$ to its reflexification, $Hom(Hom(M, R), R)$.  If reflexify is passed the option {\tt ReturnMap => true}, then instead of returning a module, reflexify returns that map.  This is not necessary for ideals since an ideal is canonically a subsetset of its reflexification.
	 Example
 	  R = QQ[x,y];
 	  m = ideal(x,y);
 	  M = m*R^1;
 	  f = reflexify( M, ReturnMap => true )
 	  source f
 	  target f
	 Text
	  Generally speaking, it is faster to reflexify ideals as opposed to modules.  Consider the following example of a point on an elliptic curve.
	 Example
	   R = QQ[x,y,z]/ideal(-y^2*z +x^3 + x^2*z + x*z^2+z^3);
	   I = ideal(x-z,y-2*z);
	   J = I^21;
	   time reflexify(J);
	   time reflexify(J*R^1);
	 Text
	  Because of this, there are two strategies for computing a reflexification (at least if the module embeds as an ideal).  
	 Text
	  {\tt IdealStrategy}.  In the case that $R$ is a domain, and our module is isomorphic to an ideal $I$, then one can compute the reflexification by computing colons.
	 Text
	  {\tt ModuleStrategy}.  This computes the reflexification simply by computing $Hom$ twice.
	 Text
	  {\tt ModuleStrategy} is the default strategy for modules, {\tt IdealStrategy} is the default strategy for ideals.  In our experience, {\tt IdealStrategy} is faster on average.  Note that calling {\tt ModuleStrategy} for ideals or {\tt IdealStrategy} for modules creates overhead which can slow things down substantially (since we must embed various modules as ideals).
	 Example
	  R = ZZ/13[x,y,z]/ideal(x^3 + y^3-z^11*x*y);
	  I = ideal(x-4*y, z);
	  J = I^20;
	  M = J*R^1;
 	  J1 = time reflexify( J, Strategy=>IdealStrategy )
 	  J2 = time reflexify( J, Strategy=>ModuleStrategy )
 	  J1 == J2
 	  time reflexify( M, Strategy=>IdealStrategy );
 	  time reflexify( M, Strategy=>ModuleStrategy );	 
	 Text
	  However, sometimes {\tt ModuleStrategy} is faster, especially for Monomial ideals.
	 Example
 	  R = QQ[x,y,u,v]/ideal(x*y-u*v);
 	  I = ideal(x,u);
 	  J = I^20;
 	  M = I^20*R^1;
 	  time reflexify( J, Strategy=>IdealStrategy )
 	  time reflexify( J, Strategy=>ModuleStrategy )
 	  time reflexify( M, Strategy=>IdealStrategy );
 	  time reflexify( M, Strategy=>ModuleStrategy );	  
	 Text
	  For ideals, if {\tt KnownDomain} is false (default value is {\tt true}), then the function will check whether it is a domain.  If it is a domain (or assumed to be a domain), it will reflexify using a strategy which can speed up computation, if not it will compute using a sometimes slower method which is essentially reflexifying it as a module. 
	 Text	 
	  Consider the following example showing the importance of making the correct assumption about the ring being a domain.
	 Example
	  R = QQ[x,y]/ideal(x*y);
	  I = ideal(x,y);
	  reflexify(I, KnownDomain=>false)
	  reflexify(I, KnownDomain=>true)
	  J = ideal(x-y, x+y);
	  reflexify(J, KnownDomain=>false)
	  reflexify(I, KnownDomain=>true)
	 Text
	  In the above, when KnownDomain=>true (an incorrect assumption), this function returns the incorrect answer for $I$.
	SeeAlso
	 isReflexive
	 dualize
///





doc ///
	Key
	 dualize
	 (dualize, Ideal)
	 (dualize, Module)
	 [dualize, KnownDomain]
	 [dualize, Strategy]
	Headline
	 finds an ideal or module isomorphic to Hom(M, R)
	Usage
	 dualize( I1 )
	 dualize( M1 )
	Inputs
	 I1: Ideal
	 M1: Module
	 KnownDomain => Boolean
	  assume the ambient ring is a domain
	 Strategy => Symbol
	  specify the strategy of dualize
	Outputs
	 : Ideal
	 : Module
	Description
	 Text
	  This computes $Hom_R(M, R)$.  
	 Example
	  R = QQ[x,y,z]/ideal(x^2-y*z);
	  m = ideal(x,y,z);
	  dualize(m)
	  I = ideal(x,y);
	  dualize(I)
	  dualize(I^2)
	  dualize(I^3)
	 Text
	  If {\tt Strategy => IdealStrategy}, then dualize assume the module is isomorphic to an ideal, embeds it as an ideal, and computes the dual by forming a colon.  ModuleStrategy simply computes the Hom.  The default Strategy for modules is ModuleStrategy, and the default Strategy for ideals is IdealStrategy.  This is because there is overhead using the opposite strategy (involving embedding modules as ideals).  Frequently {\tt IdealStrategy} is faster, but not always.	Consider first a D4 singularity in characteristic 2.
	 Example
	  R = ZZ/2[x,y,z]/ideal(z^2-x*y*z-x^2*y-x*y^2);
	  m = ideal(x,y,z);
	  J = m^9;
	  M = J*R^1;
	  time dualize(J, Strategy=>IdealStrategy);
	  time dualize(J, Strategy=>ModuleStrategy);
	  time dualize(M, Strategy=>IdealStrategy);
	  time dualize(M, Strategy=>ModuleStrategy);
	  time embedAsIdeal dualize(M, Strategy=>ModuleStrategy);
	 Text
	  For monomial ideals in toric rings, frequently ModuleStrategy appears faster.
	 Example
	  R = ZZ/7[x,y,u,v]/ideal(x*y-u*v);
	  I = ideal(x,u);
	  J = I^15;
	  time dualize(J, Strategy=>IdealStrategy);
	  time dualize(J, Strategy=>ModuleStrategy);
	 Text
	  {\tt KnownDomain} is an option for {\tt dualize}.  If it is {\tt false} (default is {\tt true}), then the computer will first check whether the ring is a domain, if it is not then it will revert to {\tt ModuleStrategy}.  If {\tt KnownDomain} is set to {\tt true} for a non-domain, then the function can return an incorrect answer.
	 Example
	  R = QQ[x,y]/ideal(x*y);
	  J = ideal(x,y);
	  dualize(J, KnownDomain=>true)
	  dualize(J, KnownDomain=>false)       
	SeeAlso
	 reflexify
///


doc ///
	Key
	 reflexivePower
	 (reflexivePower, ZZ, Ideal)
	 [reflexivePower, Strategy]
	Headline
	 computes a reflexive power of an ideal in a normal domain
	Usage
	 reflexivePower( n, I )
	Inputs
	 I: Ideal
	 n: ZZ
	 Strategy => Symbol
	  specify the strategy passed to reflexify
	Outputs
	 : Ideal
	Description
	 Text
	  This function returns the $n$-th reflexive power of $I$.  By definition this is the reflexification of $I^n$, or in other words, $Hom(Hom(I^n, R), R)$. 
	 Example
	  R = QQ[x,y,z]/ideal(x^2-y*z);
	  J = ideal(x,y);
	  reflexivePower(5, J)
	  reflexivePower(6, J)
	 Text
	  This function is typically much faster than reflexifying $I^n$ however.  We can obtain this speedup, because in a normal domain, the reflexification of $I^n$ is the same as the reflexification of the ideal generated by the $n$-th powers of the generators of $I$.  Consider the example of a cone over a point on an elliptic curve.
	 Example
	  R = QQ[x,y,z]/ideal(-y^2*z +x^3 + x^2*z + x*z^2+z^3);
	  I = ideal(x-z,y-2*z);
	  time J20a = reflexivePower(20, I);
	  I20 = I^20;
	  time J20b = reflexify(I20);
	  J20a == J20b
	 Text
	  This passes the {\tt Strategy} option to a {\tt reflexify} call.  Valid options are {\tt IdealStrategy} and {\tt ModuleStrategy}.
	 Example
	  R = QQ[x,y,z]/ideal(-y^2*z +x^3 + x^2*z + x*z^2+z^3);
	  I = ideal(x-z,y-2*z);
	  time J1 = reflexivePower(20, I, Strategy=>IdealStrategy);
	  time J2 = reflexivePower(20, I, Strategy=>ModuleStrategy);
	  J1 == J2
	SeeAlso
	 reflexify
	 isReflexive
///



doc ///
	Key
	 IdealStrategy
	 ModuleStrategy
	 NoStrategy
	Headline
	 a valid value for the Strategy option in dualize or reflexify
	Description
	 Text
	  This is a valid input for the {\tt Strategy} option of dualize or reflexify.
	SeeAlso
	 dualize
	 reflexify
///



doc ///
	Key
	 KnownDomain
	Headline
	 an option used to specify to certain functions that we know that the ring is a domain
	Description
	 Text
	  If true, then some functions will not check whether or not the ring is domain, they will assume it and proceed.
///

doc ///
	Key
	 KnownCartier
	Headline
	 an option used to specify to certain functions that we know that the divisor is Cartier
	Description
	 Text
	  If true, then some functions will not check whether or not the divisor is Cartier, they will assume it is and proceed.
///

doc ///
	Key
	 Section
	Headline
	 an option used in a number of functions
	Description
	 Text
	  Used to specify a (global) section of an ideal or module, in order to construct an effective divisor.
///

doc ///
	Key
	 Primes
	Headline
	 a value for the option Strategy for the pullback method
	Description
	 Text
	  If {\tt Strategy=>Primes} then the pullback method will pull back each prime individually.
	SeeAlso
	 pullback
	 Sheaves
///

doc ///
	Key
	 Sheaves
	Headline
	 a value for the option Strategy for the pullback method
	Description
	 Text
	  If {\tt Strategy => Sheaves} then the pullback method will pull back the sheaf $O(D)$.
	SeeAlso
	 pullback
	 Primes
///

doc ///
	Key
	 ReturnMap
	Headline
	 an option for embedAsIdeal
	Description
	 Text
	  If {\tt ReturnMap} is set to {\tt true}, then instead of {\tt embedAsIdeal} converting a module into an isomorphic ideal, this also returns the map from the module to the ring.
	SeeAlso
	 embedAsIdeal
///

doc ///
 	Key 
 	 isDomain
 	 (isDomain, Ring)
 	Headline
 	  whether a ring is a domain
 	Usage
 	 isDomain(R)
 	Inputs
 	 R: Ring
 	Outputs
 	 : Boolean
 	Description
 	 Text
 	  This function returns {\tt true} if {\tt R} is an integral domain, otherwise it returns {\tt false}.  It simply checks whether the zero ideal is prime.
 	 Example
 	  R = QQ[x,y,z]/ideal(x^2-y*z)
	  isDomain(R)
	  S = ZZ/5[x,y]/ideal(x^2*y^3)
	  isDomain(S)
///

doc ///
	Key
	 isReflexive
	 (isReflexive, Ideal)
	 (isReflexive, Module)
	 [isReflexive, Strategy]
	 [isReflexive, KnownDomain]
	Headline
	 whether an ideal or module is reflexive
	Usage
	 isReflexive( I1 )
	 isReflexive( M1 )
	Inputs
	 I1: Ideal
	 M1: Module
	 Strategy => Symbol
	   specify a strategy for the internal call to reflexify
	 KnownDomain => Boolean
	   assume the ambient ring is a domain for the internal call to reflexify
	Outputs
	 : Boolean
	Description
	 Text
	  This function returns {\tt true} if the module or ideal is reflexive, otherwise it returns {\tt false}.  In other words this checks if $M \cong Hom(Hom(M, R))$.  This function calls {\tt reflexify} and passes the options {\tt Strategy} and {\tt KnownDomain} specified in its call.
	 Example
	  R = QQ[x,y,z]/ideal(x^2-y*z);
	  m = ideal(x,y,z);
	  isReflexive(m)
	  isReflexive(m*R^1)
	  I = ideal(x,y);
	  isReflexive(I)
	  isReflexive(I*R^1)	  
	SeeAlso
	 reflexify
	 Strategy
	 KnownDomain
///



doc ///
 	Key 
 	 torsionSubmodule
 	 (torsionSubmodule, Module)
 	 [torsionSubmodule, Strategy]
 	 [torsionSubmodule, KnownDomain]
 	Headline
 	  create the torsion submodule of a module
 	Usage
 	 torsionSubmodule( M1 )
 	Inputs
 	 M1: Module
 	 Strategy => Symbol
	   specify a strategy for the internal call to reflexify
	 KnownDomain => Boolean
	   assume the ambient ring is a domain for the internal call to reflexify
 	Outputs
 	 : Module
 	Description
 	 Text
 	  Finds the torsion submodule of a module.  It does this by computing the kernel of the map from {\tt M1} to its reflexification.  The options {\tt Strategy} and {\tt KnownDomain} are passed along when the function {\tt reflexify} is called.
 	 Example
 	  R = QQ[x,y];
 	  m = ideal(x,y);
 	  M = (R^1/m) ++ R^1
 	  prune torsionSubmodule M
 	SeeAlso
 	 reflexify
///


doc ///
 	Key
 	 embedAsIdeal
 	 (embedAsIdeal, Ring, Module)
 	 (embedAsIdeal, Module)
 	 (embedAsIdeal, Ring, Matrix)
 	 (embedAsIdeal, Matrix)
 	 [embedAsIdeal, MTries]
 	 [embedAsIdeal, IsGraded]
 	 [embedAsIdeal, ReturnMap]
 	 [embedAsIdeal, Section]
 	Headline
 	 embed a module as an ideal of a ring
 	Usage
 	 I = embedAsIdeal(R, M, MTries=>n, IsGraded=>false)
 	 L = embedAsIdeal(R, M, MTries=>n, IsGraded=>true)
 	 L = embedAsIdeal(R, M, MTries=>n, ReturnMap=>true)
 	 L = embedAsIdeal(R, M, MTries=>n, ReturnMap=>true, IsGraded=>true)
 	Inputs
 	 M: Module
 	 R: Ring
 	 MTries => ZZ
 	   how many times random attempts to embed the module should be attempted if the first attempt fails
 	 IsGraded => Boolean
 	   specify that the module is graded and instruct the function to return the degree shift of the embedding
 	 ReturnMap => Boolean
 	   specify that the function should also return a map from the module to R^1, identifying the embedding
 	 Section => RingElement
 	   used to specify that the function will keep track of a certain element
 	Outputs
 	 I: Ideal
 	   the module embedded as an ideal
 	 L: List
 	   if IsGraded is true, then this function returns the degree shift of the embedding as a second entry in a list.  Further if ReturnMap is true, then a map from the module to the ring is also returned as another entry in this list.  Finally, if a matrix defining is passed to the function, then the first entry in the list is this ring element. 
 	Description
 	 Text
 	  Tries to embed the module $M$ as an ideal in $R$.  It will make several automatic tries followed by {\tt MTries => n} attempts (the default {\tt n} value is 10).  Parts of this function were based on code originally written in the Macaulay2 Divisor tutorial and also based on code by Mordechai Katzman, see the {\tt canonicalIdeal} function in http://katzman.staff.shef.ac.uk/FSplitting/ParameterTestIdeals.m2
 	 Example
 	  R = QQ[x,y]
 	  M = (ideal(x^2,x*y))*R^1
 	  embedAsIdeal(M)
 	 Text
 	  It also works for non-domains
 	 Example
 	  R = QQ[x,y]/ideal(x*y);
 	  M = (ideal(x^3, y^5))*R^1;
 	  embedAsIdeal(M)
 	  N = (ideal(x,y))*R^1;
 	  embedAsIdeal(N)
 	 Text
 	  Note that the answer is right even if you don't recognize it at first.  Next, consider the {\tt IsGraded} option. If this is set to {\tt true}, then the system returns the degree as well (as you can see in the example below).   The default value for the option {\tt IsGraded} is {\tt false}.
 	 Example
 	  R = QQ[x,y];
 	  M = R^{-3};
 	  embedAsIdeal(M, IsGraded=>true)
 	 Text
 	  Next consider the {\tt ReturnMap} option.  What this does is also return the map from {\tt M} to {\tt R^1} of which the map is based upon.  Note that if both {\tt IsGraded} and {\tt ReturnMap} are enabled, then the map comes after the degree.
 	 Example
 	  R = QQ[x,y];
 	  M = ideal(x^2, x*y)*R^1;
 	  L = embedAsIdeal(M, ReturnMap=>true)
 	  target L#1
 	  source L#1
 	 Text
 	  Alternately, instead of passing an ideal you can pass {\tt embedAsIdeal} a {\tt Matrix}, where the source is a free module of rank one and the target is the module you wish to embed.  (This can also be accomplished by passing the same matrix via the Section option).  In this case, the first output will be a ring element corresponding to the section.
 	 Example
 	  R = QQ[x,y];
 	  M = (ideal(x^2,x*y))*R^1;
 	  mat = map(M, R^1, {{1}, {1}});
 	  embedAsIdeal(mat)
 	  embedAsIdeal(M, Section=>mat)
///

doc ///
    Key
     pullback
     (pullback, RingMap, RWeilDivisor)
     [pullback, Strategy]
    Headline
     pullback a divisor under a ring map
    Usage
     pullback(f, D1)
    Inputs
     f: RingMap
     D1: RWeilDivisor
     Strategy => Symbol
       specify the strategy used by pullback
    Outputs
     : RWeilDivisor
    Description
     Text
      This function computes the pullback of a divisor under a ring map.  There are two potential strategies, {\tt Primes} and {\tt Sheaves} ({\tt Primes} is the default strategy).  The {\tt Primes} strategy pulls back each prime individually.  It can be faster, but it only works for ring maps that are either finite or flat (unless each prime is also Cartier).  For more general maps, it can give incorrect results.  The other option for {\tt Strategy} is {\tt Sheaves}.  This can be slower, especially for divisors with large coefficients, but it will successfully pull back any Cartier divisor.  The option {\tt Sheaves} also requires the divisor passed to be a {\tt WeilDivisor}.
     Example
      R = QQ[x,y,z,w]/ideal(z^2-y*w,y*z-x*w,y^2-x*z);
      T = QQ[a,b];
      f = map(T, R, {a^3, a^2*b, a*b^2, b^3});
      D = divisor(y*z)
      pullback(f, D, Strategy=>Primes)
      pullback(f, D, Strategy=>Sheaves)
     Text
      Let us also consider pulling back a divisor under a blowup map.
     Example
      R = QQ[x,y];
      S = QQ[a,b];
      f = map(S, R, {a*b, b});
      D = divisor(x*y*(x+y));
      D1 = pullback(f, D)
      f^* D
     Text
      As illustrated by the previous example, the same functionality can also be accomplished by {\tt f^*} (which creates a function which sends a divisor $D$ to $f^* D$).
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
	 find an element of a specified degree
	Usage
	 findElementOfDegree( n, R )
	 findElementOfDegree( l, R )
	Inputs
	 R: Ring
	 n: ZZ
	 l: BasicList
	Outputs
	 : BasicList
	Description
	 Text
	  Given a singly graded ring and an integer $n$, this function tries to find an element of degree $n$.  If successful, it returns a list with two elements {$a,b$} such that $a/b$ has degree $n$.  If it is impossible, it gives an error.  If instead of an integer, you pass it a basic list corresponding to a multi-degree, it still tries to find $a, b$ in R such that $a/b$ has the provided multidegree.  It only works on rings with flattened variables (ie, no Rees algebras).
	  First we do an example without multidegrees.
	 Example
	  R = ZZ/7[x,y,Degrees=>{3, 5}];
	  output = findElementOfDegree(1, R)
	  output#0/output#1
	  findElementOfDegree(-2, R)
	 Text
	  We also do an example with multidegrees
	 Example
	  R = QQ[x,y,Degrees=>{{1,2}, {3, 5}}];
	  output = findElementOfDegree({1, 3}, R)
	  output#0/output#1
	SeeAlso
	 [divisor, IsGraded] 
	 getLinearDiophantineSolution
///

doc ///
	Key
	 getLinearDiophantineSolution
	 (getLinearDiophantineSolution, BasicList, BasicList)
	 (getLinearDiophantineSolution, BasicList, Matrix)
	 [getLinearDiophantineSolution, Safe]
	Headline
	 find a solution of the linear Diophantine equation Ax = b
	Usage
	 getLinearDiophantineSolution(l, L)
	 getLinearDiophantineSolution(l, A)
	Inputs
	 L: BasicList
	 l: BasicList
	 A: Matrix
	 Safe => Boolean
	   turns on or off routine checks in the function 
	Outputs
	 : List
	Description
	 Text
	  Given a linear Diophantine equation $Ax = b$ (i.e. an integer matrix with target vector also integer), we want to find a solution of this equation.
	 Example
	   colList = {{1,3,7}, {2,4,-31}, {1,6,101}, {3,-2,47}, {8,9,1}};
	   A = transpose matrix colList;
	   b = {1, 2, 3}
	   getLinearDiophantineSolution(b, A)
	   sol = getLinearDiophantineSolution(b, colList )
	   sum apply(#sol, i->(sol#i)*(colList#i) )
	 Text
	  When the context is clear, set the {\tt Safe} option to {\tt false} in order to avoid routine checks.
	 Example
		A = matrix{ {1, 0, 0, 0, 0}, {0, 2, 0, 0, 0}, {3, 4, 5, 6, 8} }
	   	b = {1, 2, 3}
	   	getLinearDiophantineSolution(b, A, Safe => false)
	SeeAlso
	 findElementOfDegree
///

doc ///
   	Key
   	 applyToCoefficients
   	 (applyToCoefficients, BasicDivisor, Function)
   	 [applyToCoefficients, CoefficientType]
   	 [applyToCoefficients, Safe]
   	Headline
   	 apply a function to the coefficients of a divisor
   	Usage
   	 applyToCoefficients(D1, h)
   	Inputs
   	 D1: BasicDivisor
   	 h: Function
   	 CoefficientType => Number
   	   specifies what the coefficients of the output divisor should be
   	 Safe => Boolean
   	   specifies that the system checks whether the output is a valid divisor
   	Outputs
   	 : WeilDivisor
   	Description
   	 Text
	  applyToCoefficients applies the function {\tt h} to the coefficients of the divisor of {\tt D1}.  Specifying the {\tt CoefficientType=>ZZ}, {\tt CoefficientType=>QQ}, {\tt CoefficientType=>RR}, will force the returned divisor to be of a certain form ({\tt WeilDivisor, QWeilDivisor, RWeilDivisor} respectively), otherwise the class of the output {\tt D} is the same as the class of the input {\tt D1} ({\tt WeilDivisor, QWeilDivisor, RWeilDivisor, BasicDivisor}).  If {\tt Safe} is set to {\tt true} (the default is {\tt false}), then the function will check to make sure the output is a valid divisor.  
	 Example
	  R = QQ[x, y, z];
	  D = divisor(x*y^2/z)
	  applyToCoefficients(D, u->5*u)
	SeeAlso
	 (floor, RWeilDivisor)
	 (ceiling, RWeilDivisor)
///



doc /// 
	Key
	 canonicalDivisor
	 (canonicalDivisor, Ring)
	 [canonicalDivisor, IsGraded]
	Headline
	 compute a canonical divisor of a ring
	Usage
	 canonicalDivisor( R )
	Inputs
	 R: Ring
	 IsGraded => Boolean
	   specify that the returned canonical divisor should reflect the grading of the ring
	Outputs
	 : WeilDivisor
	Description
	 Text
	  Compute the canonical divisor of a ring (warning, the canonical divisor is not unique, but only unique up to linear equivalence).  If the {\tt IsGraded} option is set to {\tt true} (default {\tt false}), then it will return a canonical divisor for the $Proj$ of $R$, otherwise it will return one for only the $Spec$.  The graded version only works reliably for graded rings over a field (for instance, if you have a Rees algebra you will need to flatten the variables).
	 Example
	  R = QQ[x,y,z];
	  canonicalDivisor(R)
	  canonicalDivisor(R, IsGraded=>true)
	 Text
	  Note the {\tt IsGraded} option makes a difference.  Consider now a non-Gorenstein singularity.
	 Example
	  R = QQ[a,b,c,d]/ideal(c^2-b*d, b*c-a*d, b^2-a*c);
	  canonicalDivisor(R)
	Caveat
	 Text
	  This function assumes that the {\tt coefficientRing} of the ambient ring is a field (or at least Gorenstein).  If the {\tt coefficientRing} is a more general ring, this function will  produce a relative canonical divisor of the ring over its {\tt coefficientRing}.
///

doc /// 
	Key
	 ramificationDivisor
	 (ramificationDivisor, RingMap)
	 [ramificationDivisor, IsGraded]
	Headline
	 compute the ramification divisor of a finite inclusion of normal domains or a blowup over a smooth base
	Usage
	 ramficationDivisor( f )
	Inputs
	 f: RingMap
	 b: Boolean
	 IsGraded => Boolean
	   specify true to compute the relative canonical divisor of blowup over a smooth base
	Outputs
	 : WeilDivisor
	Description
	 Text
	  Compute the ramification (relevative canonical) divisor corresponding the finite inclusion of normal domains.  If you pass it a non-finite map, it will compute the divisorial part of the locus where the map is not smooth.  If {\tt IsGraded} is set to {\tt false} (the default value), then the coefficient ring of both the source and target of $f$ must be equal.  
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
	  If the option {\tt IsGraded} is set to {\tt true}, then the function will assume that the source of {\tt f} is affine, and the target is projective over the source.  In this case, the coefficient ring of the target must be equal to the source ring.  This can be useful when computing things like relative canonical divisors over regular bases (it may not give the expected answer over non-regular bases).  For example, this is useful if you want to compute the relative canonical divisor of a blowup.
	 Example
	  R = QQ[x,y];
	  S = reesAlgebra(ideal(x,y^2));
	  f = map(S, R);
	  ramificationDivisor(f,IsGraded=>true)
	SeeAlso
	 canonicalDivisor
///

doc ///
	Key
	 isWeilDivisor
	 (isWeilDivisor, RWeilDivisor)
	Headline
		whether a rational/real divisor is in actuality a Weil divisor
	Usage
		isWeilDivisor( D2 )
	Inputs
		D2: RWeilDivisor
	Outputs
		: Boolean
	Description
	 Text
	  Check if a rational/real divisor is a Weil divisor
	 Example
	  R = QQ[x, y, z];
	  D1 = divisor({1/1, 2/2, -6/3}, {ideal(x), ideal(y), ideal(z)}, CoefficientType=>QQ)
	  D2 = divisor({1/2, 3/4, 5/6}, {ideal(y), ideal(z), ideal(x)}, CoefficientType=>QQ)
	  isWeilDivisor( D1 )
	  isWeilDivisor( D2 )
	SeeAlso
		toWeilDivisor
///

doc ///
	Key
		isEffective
		(isEffective, BasicDivisor)
	Headline
		whether a divisor is effective
	Usage
		isEffective( D1 )
	Inputs
		D1: WeilDivisor
	Outputs
		: Boolean
	Description
	 Text
	  This function returns {\tt true} if the divisor is effective (all coefficients nonnegative), otherwise it returns {\tt false}.
	 Example
	  R = ZZ/31[x, y, u, v] / ideal(x * y - u * v);
	  D1 = divisor({1, -2, 3, -4}, {ideal(x, u), ideal(x, v), ideal(y, u), ideal(y, v)})
	  D2 = divisor({1, 39, 5, 27}, {ideal(x, v), ideal(y, v), ideal(x, u), ideal(x, u)})
	  isEffective( D1 )
	  isEffective( D2 )
///

doc ///
	Key
	 (isPrime, BasicDivisor)
	Headline
	 whether a divisor is prime
	Usage
		isPrime( D1 )
	Inputs
		D1: BasicDivisor
	Outputs
		: Boolean
	Description
	 Text
	  This function returns {\tt true} if the divisor is prime (with coefficient 1), otherwise it returns {\tt false}.
	 Example
	  R = QQ[x, y];
	  D1 = divisor(x^2 * y)
	  D2 = divisor(x^2)
	  D3 = divisor(y)
	  isPrime( D1 )
	  isPrime( D2 )
	  isPrime( D3 )
///

doc ///
	Key
	 isReduced
	 (isReduced, BasicDivisor)
	Headline
	 whether a divisor is reduced
	Usage
		isReduced( D1 )
	Inputs
		D1: WeilDivisor
	Outputs
		: Boolean
	Description
	 Text
	  This function returns {\tt true} if the divisor is reduced (all coefficients equal to 1), otherwise it returns {\tt false}.
	 Example
	  R = QQ[x, y, z];
	  D1 = divisor(x^2 * y^3 * z)
	  D2 = divisor(x * y * z)
	  isReduced( D1 )
	  isReduced( D2 )
///

doc ///
  	Key 
  	 isPrincipal
  	 (isPrincipal, WeilDivisor)
  	 [isPrincipal, IsGraded]
  	Headline
  	 whether a Weil divisor is globally principal
  	Usage
  	 isPrincipal( D, IsGraded => b )
  	Inputs
  	 D: WeilDivisor
	 b: Boolean
  	Outputs
  	 : Boolean
  	Description
  	 Text
  	  This function returns {\tt true} if the Weil divisor {\tt D} is principal, otherwise {\tt false}.  If {\tt IsGraded} is set to {\tt true}, then this checks whether the divisor corresponds to a principal divisor on the Proj of the ambient ring.  Note that this function may return a false negative if the defining equations of the divisor are not homogeneous (it warns the user if this occurs).
  	 Example
  	  R = QQ[x, y, z];
  	  D = divisor(x);
  	  isPrincipal(D, IsGraded => true)
  	 Text  
  	  By default, {\tt IsGraded} is set to {\tt false}.  Regardless of the format, the check is done by determining whether or not $O(D)$ is free.  
  	 Example
  	  R = QQ[x, y, z]/ideal(x^2 - y*z);
  	  D = divisor(x);
  	  E = divisor(ideal(x,z));
  	  isPrincipal( D )
  	  isPrincipal( E )
  	 Text
  	  The output value of this function is stored in the divisor's cache with the value of the last {\tt IsGraded} option.  If you change the {\tt IsGraded} option, the value will be recomputed.
  	SeeAlso
  	 (symbol SPACE, OO, RWeilDivisor)
///

doc ///
	Key
	 isCartier
	 (isCartier, WeilDivisor)
	 [isCartier, IsGraded]
	Headline
	 whether a Weil divisor is Cartier
	Usage
	 isCartier( D )
	Inputs
	 D: WeilDivisor
	 IsGraded => Boolean
	   set to true to assume that we are doing this check on a projective variety
	Outputs
	 : Boolean
	Description
	 Text
	  Check if a Weil divisor is Cartier.  For example, the following divisor is not Cartier
	 Example
	  R = QQ[x, y, u, v] / ideal(x * y - u * v);
	  D = divisor({2, -3}, {ideal(x, u), ideal(y, v)})
	  isCartier( D )
	 Text
	  Neither is this divisor.
	 Example
	  R = QQ[x, y, z] / ideal(x * y - z^2 );
	  D = divisor({1, 2}, {ideal(x, z), ideal(y, z)})
	  isCartier( D )
	 Text
	  Of course the next divisor is Cartier.
	 Example
	  R = QQ[x, y, z];
	  D = divisor({1, 2}, {ideal(x), ideal(y)})
	  isCartier( D )
	 Text
	  If the option {\tt IsGraded} is set to {\tt true} (it is {\tt false} by default), this will check as if {\tt D} is a divisor on the $Proj$ of the ambient graded ring.
	 Example
	  R = QQ[x, y, u, v] / ideal(x * y - u * v);
	  D = divisor({2, -3}, {ideal(x, u), ideal(y, v)})
	  isCartier(D, IsGraded => true)
	 Example
	  R = QQ[x, y, z] / ideal(x * y - z^2);
	  D = divisor({1, 2}, {ideal(x, z), ideal(y, z)})
	  isCartier(D, IsGraded => true)
	 Text	  
	  The output value of this function is stored in the divisor's cache with the value of the last {\tt IsGraded} option.  If you change the {\tt IsGraded} option, the value will be recomputed.
	SeeAlso
	 (symbol SPACE, OO, RWeilDivisor)
	 isQCartier
///

doc ///
	Key
	 nonCartierLocus
	 (nonCartierLocus, WeilDivisor)
	 [nonCartierLocus, IsGraded]
	Headline
	 the non-Cartier locus of a Weil divisor
	Usage
	 nonCartierLocus( D, IsGraded=>b)
	Inputs
	 D: WeilDivisor
	 IsGraded => Boolean
	   specify that we are computing this locus on a projective varietys
	Outputs
	 : Ideal
	Description
	 Text
	  This function returns an ideal which vanishes on the locus where {\tt D} is not Cartier.  
	 Example
	  R = QQ[x, y, u, v]/ideal(x * y  - u * v);
	  D = divisor({1, -3, -5, 8}, {ideal(x, u), ideal(y, v), ideal(x, v), ideal(y, u)})
	  nonCartierLocus( D )
	 Text
	  If the option {\tt IsGraded} is set to {\tt true} (by default it is {\tt false}), it saturates with respect to the homogeneous maximal ideal.
	 Example
	  R = QQ[x, y, u, v]/ideal(x * y  - u * v);
	  D = divisor({1, -3, -5, 8}, {ideal(x, u), ideal(y, v), ideal(x, v), ideal(y, u)})
	  nonCartierLocus( D, IsGraded => true )
	 Text
	  The output value of this function is stored in the divisor's cache with the value of the last {\tt IsGraded} option.  If you change the {\tt IsGraded} option, the value will be recomputed.
	SeeAlso
	 isCartier
	 isQCartier
///

doc ///
 	Key
 	 isLinearEquivalent
 	 (isLinearEquivalent, WeilDivisor, WeilDivisor)
 	 [isLinearEquivalent, IsGraded]
 	Headline
 	 whether two Weil divisors are linearly equivalent
 	Usage
 	 flag = isLinearEquivalent(D1, D2)
 	Inputs
 	 D1: WeilDivisor
 	 D2: WeilDivisor
 	 IsGraded => Boolean
 	   specify that we are doing this computation on a projective algebraic variety
 	Outputs
 	 flag: Boolean
 	Description
 	 Text
 	  Given two Weil divisors, this method checks whether they are linearly equivalent.  
 	 Example
 	  R = QQ[x, y, z]/ ideal(x * y - z^2);
 	  D1 = divisor({3, 8}, {ideal(x, z), ideal(y, z)})
 	  D2 = divisor({8, 1}, {ideal(y, z), ideal(x, z)})
 	  isLinearEquivalent(D1, D2)
 	 Text
 	  If {\tt IsGraded} is set to {\tt true} (by default it is {\tt false}), then it treats the divisors as divisors on the $Proj$ of their ambient ring. 
 	 Example 
 	  R = QQ[x, y, z]/ ideal(x * y - z^2);
 	  D1 = divisor({3, 8}, {ideal(x, z), ideal(y, z)})
 	  D2 = divisor({8, 1}, {ideal(y, z), ideal(x, z)})
 	  isLinearEquivalent(D1, D2, IsGraded => true)
 	SeeAlso
 	 (symbol SPACE, OO, RWeilDivisor)
 	 isQLinearEquivalent
///

doc ///
 	Key
 	 isQCartier
 	 (isQCartier, ZZ, WeilDivisor)
 	 (isQCartier, ZZ, QWeilDivisor)
 	 [isQCartier, IsGraded]
 	Headline
 	 whether m times a divisor is Cartier for any m from 1 to a fixed positive integer n1. 
 	Usage
 	 isQCartier(n1, D1 )
 	 isQCartier(n1, D2 )
 	Inputs
 	 D1: WeilDivisor
 	 D2: QWeilDivisor
 	 n1: ZZ
 	 IsGraded => Boolean
 	   specify that we should do this computation on a projective algebraic variety
 	Outputs
 	 b: Boolean
 	Description
 	 Text
 	  Check whether $m$ times a Weil or Q-divisor $D$ is Cartier for each $m$ from {\tt 1} to a fixed positive integer {\tt n1} (if the divisor is a {\tt QWeilDivisor}, it can search slightly higher than n1).  If {\tt m * D1} is Cartier, it returns {\tt m}.  If it fails to find an {\tt m}, it returns {\tt 0}.  
 	 Example
 	  R = QQ[x, y, z] / ideal(x * y - z^2 );
 	  D1 = divisor({1, 2}, {ideal(x, z), ideal(y, z)})
 	  D2 = divisor({1/2, 3/4}, {ideal(y, z), ideal(x, z)}, CoefficientType => QQ)
 	  isQCartier(10, D1)
 	  isQCartier(10, D2)
 	 Example
 	  R = QQ[x, y, u, v] / ideal(x * y - u * v);
 	  D1 = divisor({1, 2}, {ideal(x, u), ideal(y, v)})
 	  D2 = divisor({1/2, -3/4}, {ideal(y, u), ideal(x, v)}, CoefficientType => QQ)
 	  isQCartier(10, D1)
 	  isQCartier(10, D2)
 	 Text
 	  If the option {\tt IsGraded} is set to {\tt true} (by default it is {\tt false}), then it treats the divisor as a divisor on the $Proj$ of their ambient ring.
 	 Example
 	  R = QQ[x, y, z] / ideal(x * y - z^2 );
 	  D1 = divisor({1, 2}, {ideal(x, z), ideal(y, z)})
 	  D2 = divisor({1/2, 3/4}, {ideal(y, z), ideal(x, z)}, CoefficientType => QQ)
 	  isQCartier(10, D1, IsGraded => true)
 	  isQCartier(10, D2, IsGraded => true) 
 	 Text
 	  The output value of this function is stored in the divisor's cache with the value of the last {\tt IsGraded} option.  If you change the {\tt IsGraded} option, the value will be recomputed.
 	SeeAlso
 	 isCartier
///

doc ///
   	Key
   	 isQLinearEquivalent
   	 (isQLinearEquivalent, ZZ, QWeilDivisor, QWeilDivisor)
   	 [isQLinearEquivalent, IsGraded]
   	Headline
   	 whether two Q-divisors are linearly equivalent
   	Usage
   	 isQLinearEquivalent(n, D1, D2)
   	Inputs
   	 n: ZZ
   	 D1: QWeilDivisor
   	 D2: QWeilDivisor
   	 IsGraded => Boolean
   	   specify that we should do this computation on a projective algebraic variety
   	Outputs
   	 : Boolean
   	Description
   	 Text
   	  Given two rational divisors, this method returns {\tt true} if they linearly equivalent after clearing denominators or if some further multiple up to {\tt n} makes them linearly equivalent.  Otherwise it returns {\tt false}.  
   	 Example
   	  R = QQ[x, y, z] / ideal(x * y - z^2);
   	  D = divisor({1/2, 3/4}, {ideal(x, z), ideal(y, z)}, CoefficientType => QQ)
   	  E = divisor({3/4, 5/2}, {ideal(y, z), ideal(x, z)}, CoefficientType => QQ)
   	  isQLinearEquivalent(10, D, E)
   	 Text
   	  In the above ring, every pair of divisors is Q-linearly equivalent because the Weil divisor class group is isomorphic to Z/2.  However, if we don't set {\tt n} high enough, the function will return {\tt false}.  
   	 Example
   	  R = QQ[x,y,z] / ideal(x * y - z^2);
   	  D = divisor(x);
   	  E = divisor(ideal(x,z));
   	  isQLinearEquivalent(1, D, E)
   	  isQLinearEquivalent(3, D, E) 
   	 Text
   	  If {\tt IsGraded=>true} (the default is {\tt false}), then it treats the divisors as if they are divisors on the $Proj$ of their ambient ring.
   	 Example
   	  R = QQ[x, y, z] / ideal(x * y - z^2);
   	  D = divisor({1/2, 3/4}, {ideal(x, z), ideal(y, z)}, CoefficientType => QQ)
   	  E = divisor({3/2, -1/4}, {ideal(y, z), ideal(x, z)}, CoefficientType => QQ)
   	  isQLinearEquivalent(10, D, E, IsGraded => true)
   	  isQLinearEquivalent(10, 3*D, E, IsGraded => true)
   	 Text
   	SeeAlso
   	 (symbol SPACE, OO, RWeilDivisor)
   	 isLinearEquivalent
///

doc ///
   	Key
   	 (isHomogeneous, BasicDivisor)
   	Headline
   	 whether the divisor is graded (homogeneous)
   	Usage
   	 isHomogeneous(D)
   	Inputs
   	 D: BasicDivisor
   	Outputs
   	 : Boolean
   	Description
   	 Text
   	  This function returns {\tt true} if the divisor is graded (homogeneous), otherwise it returns {\tt false}.
   	 Example
   	  R = QQ[x, y, z];
   	  D = divisor({1, 2, 3}, {ideal(x * y - z^2), ideal(y * z - x^2), ideal(x * z - y^2)})
   	  isHomogeneous( D )
   	 Example
   	  R = QQ[x, y, z];
   	  D = divisor({1, 2}, {ideal(x * y - z^2), ideal(y^2 - z^3)})
   	  isHomogeneous( D )
///

doc ///
    Key
     ideals
    Headline
     a symbol used as a key within the divisor cache
    Description
     Text
      Each divisor has a cache (a {\tt CacheTable}).  One of the keys of this cache is always {\tt (symbol ideals)}.  This points to a {\tt MutableHashTable} whose keys are lists of elements in a Groebner basis and which themselves point to how the ideal should be displayed.  Note the {\tt trim} command trims these ideals.
    SeeAlso
     (trim, BasicDivisor)
///

doc /// 
   	Key
   	 isSmooth
   	 (isSmooth, Ideal)
   	 [isSmooth, IsGraded]
   	Headline
   	 whether R mod the ideal is smooth
   	Usage
   	 isSmooth( I )
   	Inputs
   	 I: Ideal
   	 IsGraded => Boolean
   	   specify that we should do this computation on a projective algebraic variety
   	Outputs
   	 flag: Boolean
   	Description
   	 Text
   	  This function returns {\tt true} if $R/I$ is regular where $R$ is the ambient ring of $I$, otherwise it sets to {\tt false}.  
   	 Example
   	  R = QQ[x, y, z];
   	  I = ideal(x * y - z^2 )
   	  isSmooth( I )
   	 Example
   	  R = QQ[x, y, u, v];
   	  I = ideal(x * y - u * v)
   	  isSmooth( I )
   	 Example
   	  R = QQ[x, y, z];
   	  J = ideal( x )
   	  isSmooth( J )
   	 Text
   	  If {\tt IsGraded} is set to {\tt true} (default {\tt false}) then it treats $I$ as an ideal on $Proj R$ (and it assumes $R$ is standard graded over a field).  In particular, singularities at the origin (corresponding to the irrelevant ideal) are ignored.
   	 Example
   	  R = QQ[x, y, z];
   	  I = ideal(x * y - z^2 )
   	  isSmooth(I)
   	  isSmooth(I, IsGraded => true)
   	 Example
   	  R = QQ[x, y, u, v];
   	  I = ideal(x * y - u * v)
   	  isSmooth(I)
   	  isSmooth(I, IsGraded => true)
///

doc ///
   	Key
   	 isSNC
   	 (isSNC, BasicDivisor)
   	 [isSNC, IsGraded]
   	Headline
   	 whether the divisor is simple normal crossings
   	Usage
   	 isSNC( D )
   	Inputs
   	 D: BasicDivisor
   	 IsGraded => Boolean
   	   specify that we should do this computation on a projective algebraic variety
   	Outputs
   	 : Boolean
   	Description
   	 Text
   	  This function returns {\tt true} if the divisor is simple normal crossings, this includes checking that the ambient ring is regular.
   	 Example
   	  R = QQ[x, y, z] / ideal(x * y - z^2 );
   	  D = divisor({1, -2}, {ideal(x, z), ideal(y, z)})
   	  isSNC( D )
   	 Example 
   	  R = QQ[x, y];
   	  D = divisor(x*y*(x+y))
   	  isSNC( D )
   	 Example 
   	  R = QQ[x, y];
   	  D = divisor(x*y*(x+1))
   	  isSNC( D )
   	 Text 
   	  If {\tt IsGraded} is set to {\tt true} (default {\tt false}), then the divisor is treated as if it is on the $Proj$ of the ambient ring.  In particular, non-SNC behavior at the origin is ignored.  
   	 Example
   	  R = QQ[x, y, z] / ideal(x * y - z^2 );
   	  D = divisor({1, -2}, {ideal(x, z), ideal(y, z)})
   	  isSNC( D, IsGraded => true )
   	 Example
   	  R = QQ[x, y];
   	  D = divisor(x*y*(x+y))
   	  isSNC( D, IsGraded => true )
   	 Example
   	  R = QQ[x,y,z];
   	  D = divisor(x*y*(x+y))
   	  isSNC( D, IsGraded => true)
   	 Text 
   	  The output value of this function is stored in the divisor's cache with the value of the last {\tt IsGraded} option.  If you change the {\tt IsGraded} option, the value will be recomputed.   	  
///

doc ///
   	Key
   	 isZeroDivisor
   	 (isZeroDivisor, BasicDivisor)
   	Headline
   	 whether the divisor is the zero divisor
   	Usage
   	 isZeroDivisor(D)
   	Inputs
   	 D: BasicDivisor
   	Outputs
   	 : Boolean
   	Description
   	 Text
   	  This function returns {\tt true} if the divisor is zero, otherwise it returns {\tt false}.
   	 Example
   	  R = QQ[x, y, z];
   	  D = divisor({1, 2, -3, 4}, {ideal(x), ideal(y), ideal(z), ideal(y)}); 
   	  isZeroDivisor( D ) 	  
   	 Example
   	  R = QQ[x, y, z];
   	  E = divisor({1, 2, -3, 4, 5, -9, 13, 2, -15}, {ideal(x), ideal(x), ideal(x), ideal(y), ideal(y), ideal(y), ideal(z), ideal(z), ideal(z)});
   	  isZeroDivisor( E )  	 
///

doc ///
   	Key
   	 zeroDivisor
   	 (zeroDivisor, Ring)
   	Headline
   	 constructs the zero Weil divisor for the ring
   	Usage
   	 zeroDivisor(R)
   	Inputs
   	 R: Ring
   	Outputs
   	 : WeilDivisor
   	Description
   	 Text
	  Constructs the zero Weil divisor for the input ring
	 Example
	  R = QQ[x, y, z] / ideal(x * y - z^2);
	  D = zeroDivisor( R )
///

TEST /// --check #0 (divisor, zeroDivisor)
---check constructors and verify equality of them. this also checks some comparison
R = QQ[x,y,z]/ideal(x^2-y*z);
D = divisor(x^2);
E = divisor({2,2}, {ideal(x,y), ideal(x,z)});
F = 2*divisor(ideal(x));
G = divisor({{2, ideal(x,z)}, {2, ideal(x,y)}});
--now do a check with a divisor with a zero coefficient
H = divisor({2, 2, 0}, {ideal(x,y), ideal(x,z), ideal(x-y, x-z)});
assert( (D == E) and (E == F) and (D == F) and (D == G) and (E == G) and (F == G) and (H == D) and (H == E) and (H == F) and (H == G) );
assert( (zeroDivisor(R) == 0*D) and (zeroDivisor(R) == D-E) )
///

TEST /// --check #1 (divisor)
---check constructors and verify that they don't produce the same value with different inputs
R = QQ[x,y,z]/ideal(x^2-y*z);
D = divisor(x);
E = divisor({1,2}, {ideal(x,y), ideal(x,z)});
F = 2*divisor(ideal(x));
G = divisor({{3, ideal(x,z)}, {1, ideal(x,y)}});
assert( not ((D == E) or (E == F) or (D == F) or (D == G) or (E == G) or (F == G)) )
///

TEST /// --check #2 (coefficient, divisor)
--more construction testing, and coeff testing
R = ZZ/5[x,y,z]/ideal(x^2-y*z);
D = divisor(y^5); 
assert( coefficient(ideal(y,x), D) == 10)
///

TEST /// --check #3 (primes)
R = ZZ/5[x,y,u,v]/ideal(x*y-u*v);
D = divisor(x);
L = primes D;
assert ( (L == {ideal(x,u), ideal(x,v)}) or (L == {ideal(x,v), ideal(x,u)}) )
///

TEST /// --check #4 (getPrimeCount, cleanSupport)
R = ZZ/7[x,y,u,v]/ideal(x*y-u*v);
D = divisor(x);
assert ( (getPrimeCount(D) == 2) and (getPrimeCount(-2*D) == 2) and (getPrimeCount(0/1*D) == 0) );
E = divisor({-2, 0, 1}, {ideal(x,u), ideal(y,u), ideal(x,v)});
assert ( (getPrimeCount(E) == 3) and (getPrimeCount(cleanSupport(E)) == 2) );
F = divisor({0, 0}, {ideal(x,u), ideal(x,v)});
assert ( (getPrimeCount(cleanSupport(F)) == 0) and (getPrimeCount(cleanSupport(zeroDivisor(R))) == 0) and (getPrimeCount(0*D) == 0) );
///

TEST /// --check #5 (trim)
R = QQ[x,y,z]/(x*y-z^2);
D = divisor({2}, {ideal(x+z, z)});
I1 = (primes(D))#0;
I2 = (primes(trim(D)))#0;
s1 = set(first entries gens I1);
s2 = set(first entries gens I2);
assert( (isSubset(set{x+z,z}, s1)) and (isSubset(s1, set{x+z,z})) and (isSubset(set{x,z}, s2)) and (isSubset(s2, set{x,z})) );
assert( D == trim(D));
///

TEST /// --check #6 (gbs)
S = QQ[a,b,c,d];
T = QQ[x,y];
f = map(T, S, {x^3, x^2*y, x*y^2, y^3});
I = ker f;
R = S/I;
J = ideal(sub(b, R));
S1 = set{first entries gens gb sub(ideal(a,b,c), R), first entries gens gb sub(ideal(b,c,d), R) };
D = divisor(J);
S2 = gbs(D);
assert(isSubset(S1, S2) and isSubset(S2, S1));
assert(#(gbs(zeroDivisor(S))) == 0);
///

TEST /// --check #7 (getPrimeDivisors)
R = QQ[x,y];
D = 2*divisor(x) - 3*divisor(y) + 5*divisor(x^2 - y);
S1 = set{divisor(x), divisor(y), divisor(x^2-y)};
S2 = set(getPrimeDivisors(D));
assert( isSubset(S1, S2) and isSubset(S2, S1) and (#getPrimeDivisors(zeroDivisor(R)) == 0) );
///

TEST /// --check #8 (positivePart, negativePart)
R = QQ[x,y];
D = divisor({1, 0, -2}, {ideal(x), ideal(y), ideal(x+y)});
E = divisor({1/2, 0, -5/3}, {ideal(x^2 - y), ideal(y^2-x^3), ideal(y^2 - x^3 - x)}, CoefficientType => QQ);
F = divisor({1, 2}, {ideal(x), ideal(y)});
assert( (positivePart(D) == divisor({1}, {ideal(x)})) and (negativePart(D) == divisor({2}, {ideal(x+y)})) );
assert( (positivePart(E) == divisor({1/2}, {ideal(x^2-y)})) and (negativePart(E) == (5/3)*divisor(y^2-x^3-x)) );
assert( (not(positivePart(D) == positivePart(E))) and (not(positivePart(D) == positivePart(E))) );
assert( (not(positivePart(F) == zeroDivisor(R))) and (negativePart(F) == zeroDivisor(R)) );
///

TEST /// --check #9 (applyToCoefficients, coefficient)
R = QQ[x,y];
D = divisor({1, 0, -4}, {ideal(x), ideal(y), ideal(x^2-y^3)});
E = applyToCoefficients(D, t -> t^2);
assert( E == divisor({1, 0, 16}, {ideal(x), ideal(y), ideal(x^2-y^3)}) );
assert(applyToCoefficients(zeroDivisor(R), t->1/t) == zeroDivisor(R));
assert(applyToCoefficients(cleanSupport(D), t -> 0) == zeroDivisor(R));
assert(not ( applyToCoefficients(D, t -> -3*t) == zeroDivisor(R)) );
///

TEST ///--check #10  (toWeilDivisor, toQWeilDivisor, toRWeilDivisor)
R = ZZ/7[x,y,u,v]/ideal(x*y-u*v);
D = divisor(x*u);
E = (2/2)*D;
F = (1.0)*E;
G = toWeilDivisor(E);
H = toWeilDivisor(F);
assert((D == G) and (D == H) and (not(2*D == G)) );
assert( (not instance(toQWeilDivisor(D), WeilDivisor)) and (instance(toQWeilDivisor(D), QWeilDivisor)) );
assert( (not instance(toRWeilDivisor(D), QWeilDivisor)) and (instance(toRWeilDivisor(D), RWeilDivisor)) );
assert( (not instance(toRWeilDivisor(zeroDivisor(R)), WeilDivisor)) and (instance(toRWeilDivisor(zeroDivisor(R)), RWeilDivisor)) );
///

TEST /// --check #11, test functoriality for a finite map
R = QQ[x,y,z,w]/ideal(z^2-y*w,y*z-x*w,y^2-x*z);
T = QQ[a,b];
h = map(T, R, {a^3, a^2*b, a*b^2, b^3}); --this is the natural inclusion map
D = divisor(y*z);
E = divisor(x*w);
H = 3*divisor(a*b);
assert( (pullback(h, D, Strategy=>Primes) == H) and (pullback(h, E, Strategy=>Primes) == H) and (pullback(h, zeroDivisor(R), Strategy=>Primes) == zeroDivisor(T)) );
assert( (pullback(h, D, Strategy=>Sheaves) == H) and (pullback(h, E, Strategy=>Sheaves) == H) and (pullback(h, zeroDivisor(R), Strategy=>Sheaves) == zeroDivisor(T)) );
///

TEST /// --check #12, test functoriality for pullback under localization (ie, a flat but not finite map)
R = QQ[x,y];
S = QQ[a,b,c]/ideal(a*c-1);
h = map(S, R, {a,b});
D = divisor((x)*(y^2));
E = divisor(b^2);
assert (  (pullback(h, D, Strategy=>Primes) == E) and (not (pullback(h, D, Strategy=>Primes) == zeroDivisor(S)) ) and (pullback(h, 3*divisor(x), Strategy=>Primes) == zeroDivisor(S)) );
assert (  (pullback(h, D, Strategy=>Sheaves) == E) and (not (pullback(h, D, Strategy=>Sheaves) == zeroDivisor(S)) ) and (pullback(h, 3*divisor(x), Strategy=>Sheaves) == zeroDivisor(S)) );
///

TEST /// --check #13, test functoriality for the sheaf strategy for a blowup and check the isSNC function
R = QQ[x,y];
S = QQ[a,b];
h = map(S, R, {a*b, b});
D = divisor(x*y*(x+y));
E = divisor(y^3-x^2);
D1 = pullback(h, D, Strategy=>Sheaves);
E1 = pullback(h, E, Strategy=>Sheaves);
assert( (isSNC(D1) == true) and (isSNC(E1) == false) and (coefficient(ideal(b), D1) == 3) and (coefficient(ideal(b), E1) == 2) );
assert( (isSNC(D1) == true) and (isSNC(E1) == false)); --check cache
///

TEST /// --check #14, (findElementOfDegree)
R = QQ[x,y,z, Degrees=>{6, 10, 15}];
tt = findElementOfDegree(1, R);
ss = findElementOfDegree(7, R);
rr = findElementOfDegree(-3, R);
uu = findElementOfDegree(0, R);
vv = findElementOfDegree(-11, R);
assert( degree( (tt#0)/(tt#1) ) == {1});
assert( degree( (ss#0)/(ss#1) ) == {7});
assert( degree( (rr#0)/(rr#1) ) == {-3});
assert( degree( (uu#0)/(uu#1) ) == {0});
assert( degree( (vv#0)/(vv#1) ) == {-11});
///

TEST /// --check #15 (linear diophantine solution)
A = matrix{{2,1,4},{-5,2,6}};
b = {17, -13};
x = getLinearDiophantineSolution(b, A);
assert(entries(A*vector(x)) == b);
///

TEST /// --check #16 (canonicalDivisor, isCartier)
---check a canonical divisor and verify it is Cartier
R = QQ[x,y,z]/ideal(x^2-y*z);
K = canonicalDivisor(R);
assert(isCartier(K) == true);
assert(isCartier(K) == true) --verifying cache
///

TEST /// --check #17(canonicalDivisor, isCartier, isQCartier)
--- check a canonical divisor and verify it is not Cartier
R = QQ[a,b,c,d]/ideal(c^2-b*d, b*c-a*d, b^2-a*c);
K = canonicalDivisor(R);
assert((isCartier(K) == false) and (isQCartier(10, K) == 3));
assert((isCartier(K) == false) and (isQCartier(10, K) == 3)) --verifying cache
///

TEST /// --check #18 ([canonicalDivisor, IsGraded], isLinearEquivalent)
---some linear equivalence tests
 R = QQ[x,y,z];
 K = canonicalDivisor(R, IsGraded=>true);
 Z = zeroDivisor(R);
 D = -divisor(x*y*z);
 assert(isLinearEquivalent(K, D, IsGraded=>true) and (not isLinearEquivalent(K, Z, IsGraded=>true)) and isLinearEquivalent(K, Z, IsGraded=>false) )
///

TEST /// --check #19 ([canonicalDivisor, IsGraded], isLinearEquivalent, [isQCartier, IsGraded]), some random checks on a determinantal variety
R =  QQ[a,b,c,d,e,f]/ideal(a*d-b*c, a*f-b*e, c*f-d*e);
K1 = canonicalDivisor(R);
K2 = canonicalDivisor(R, IsGraded=>true);
Z = zeroDivisor(R);
assert( (isQCartier(10, K1) == 0) and (isLinearEquivalent(K1, K2) == true) and (isLinearEquivalent(Z, K1) == false));
assert( (isQCartier(10, K1) == 0)); --check cache
assert(isQCartier(10, K2, IsGraded=>true) == 1);
assert(isQCartier(10, K2, IsGraded=>true) == 1); --check cache
///

TEST /// --check #20 (canonicalDivisor), no variable case
R = QQ[];
assert(canonicalDivisor(R) == zeroDivisor(R))
///

TEST /// --check #21 (ramificationDivisor)
--- verify ramification divisor in a simple (tamely ramified) case
R = ZZ/5[x];
S = ZZ/5[y];
f = map(S, R, {y^3});
D = ramificationDivisor(f);
assert(D == divisor(y^2))
///

TEST /// --check #22 (ramificationDivisor)
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

TEST /// --check #23 (ramificationDivisor)
--- do some wild ramification (due p dividing powers)
R = ZZ/3[x];
S = ZZ/3[y];
f = map(S, R, {y^3*(y+1)});
g = map(S, R, {y^3*(y^2+2)});
assert( (ramificationDivisor(f) == 3*divisor(y)) and (ramificationDivisor(g) == 4*divisor(y)) )
///

TEST /// --check #24 (ramificationDivisor,pullback)
--- do some wild ramification (due to inseparable residue field extension)
R = ZZ/3[a,b];
S = ZZ/3[x,y,z]/ideal(z^3-x*z-y^2);
f = map(S, R, {x,y});
assert( (ramificationDivisor(f) == divisor(x)) and (pullback(f, divisor(a)) == divisor(x)) )
///

TEST /// --check #25 [ramificationDivisor, IsGraded=>true] (some blowup examples)
R = QQ[x,y];
A = reesAlgebra(ideal(x,y));
B = reesAlgebra(ideal(x^2,y));
C = reesAlgebra((ideal(x,y))*(ideal(x^2,y)));
f = map(A, R);
g = map(B, R);
h = map(C, R);
D = ramificationDivisor(f, IsGraded=>true);
E = ramificationDivisor(g, IsGraded=>true);
F = ramificationDivisor(h, IsGraded=>true);
assert( (coefficients(D) == {1}) and (coefficients(E) == {2}) and ((coefficients(F) == {1,2}) or (coefficients(F) == {2,1})) )
///

TEST /// --check #26 [ramificationDivisor, IsGraded=>true] (some blowup examples in higher dimension)
R = QQ[x,y,z];
A = reesAlgebra(ideal(x,y,z));
f = map(A, R);
D = ramificationDivisor(f, IsGraded=>true);
assert( coefficients(D) == {2} )
///

TEST /// --check #27 (divisor, coefficient)
R = QQ[x,y,z]/ideal(x^2-y*z);
D = divisor(ideal(y));
assert( coefficient(ideal(x, y), D) == 2)
///

TEST /// --check #28 (divisor)
---checking divisor
R = QQ[x,y,z]/ideal(x^2-y*z);
J = ideal(x,y,z);
assert(isZeroDivisor(divisor(ideal(x,y,z))))
///

TEST /// --check #29 (isWeilDivisor)
R = QQ[x,y];
D = divisor({1/2, 0, -3/2}, {ideal(x), ideal(x+y), ideal(y)}, CoefficientType => QQ);
assert( (not isWeilDivisor(D)) and (isWeilDivisor(2*D)) )
///

TEST /// --check #30 (isWellDefined)
R = ZZ/101[x,y];
assert( isWellDefined(divisor({1}, {ideal(x)} )) );
assert( not isWellDefined(divisor({1/2}, {ideal(x)} )) );
assert( isWellDefined(divisor({1/2}, {ideal(x)}, CoefficientType=>QQ)) );
assert( not isWellDefined(divisor({1}, {ideal(x,y)})) );
assert( not isWellDefined(divisor({1}, {ideal(x^2)})) );
assert( not isWellDefined(divisor({-1.333}, {ideal(x+y)})) );
assert( not isWellDefined(divisor({-1.333}, {ideal(x+y)}, CoefficientType => QQ)) );
assert( isWellDefined(divisor({-1.333}, {ideal(x+y)}, CoefficientType => RR)) );
assert( isWellDefined(zeroDivisor(R)) );
assert( isWellDefined(divisor({1, 0}, {ideal(x), ideal(y)})) );
///


TEST /// --check #31 (isPrincipal)
R = ZZ/13[x, y, z]/ideal(x^2 - y*z);
assert( isPrincipal(zeroDivisor(R)) );
assert( isPrincipal(divisor({2, 0}, {ideal(x,y), ideal(x,z)})) );
D = 2*divisor(ideal(x,y));
assert( isPrincipal(D) );
assert( isPrincipal(D) ); --checking the cache
E = divisor(ideal(x,y));
assert( not isPrincipal(E) );
assert( not isPrincipal(E) ); --checking the cache
///

TEST /// --check #32 ([isPrincipal, isGraded=>true])
R = ZZ/5[x, y, z]/ideal(y^2*z - x^3 - x*z^2);
D = divisor(ideal(x-2*z, y)); --a point of order 2 on the elliptic curve
O = divisor(ideal(x,z));
assert( not isPrincipal(D, IsGraded=>true) );
assert( not isPrincipal(D, IsGraded=>true) ); --checking the cache
assert( not isPrincipal(2*D, IsGraded=>true) );
assert( isPrincipal(2*D-2*O, IsGraded=>true) );
///

TEST /// --check #33 (isReduced)
R = QQ[x,y,z];
assert( not isReduced(divisor(x^2)) );
assert( isReduced(divisor(x*y*(x-y^2))) );
assert( not isReduced(divisor(x/y)) );
assert( isReduced(divisor({1/1, 0}, {ideal(x), ideal(y)}, CoefficientType=>QQ)) );
assert( isReduced(divisor({1.0, 2.0/2.0}, {ideal(x), ideal(y)}, CoefficientType=>QQ)) );
assert( isReduced(zeroDivisor(R)) );
///

TEST /// --check #34 (isCartier)
---checking (divisor, Module) and isCartier
 R = QQ[x,y,z]/ideal(x^2-y*z);
 M = ideal(x^2,x*y,x*z)*R^1;
 D = divisor(M);
 E = divisor(ideal(x,y));
 assert( (isCartier(D)) and (not isCartier(E)) );
 assert( (isCartier(D)) and (not isCartier(E)) ) --verifying cache
///

--we also do some tests with IsGraded=>true 
TEST /// --check #35 [isCartier, IsGraded=>true]
---checking and isCartier in the graded setting
 R = QQ[x,y,z]/ideal(x^2-y*z);
 D = divisor(ideal(x,z));
 assert(isCartier(D, IsGraded=>true) and (not isCartier(D, IsGraded=>false)));
 assert(isQCartier(5, D, IsGraded=>true) == 1);
 assert(isQCartier(5, D, IsGraded=>true) == 1); --check cache
 assert(isQCartier(5, D, IsGraded=>false) == 2);
 assert(isQCartier(5, D, IsGraded=>false) == 2); --check cache
 S = ZZ/5[x,y,z]/ideal(x^3+y^3-z^3); --elliptic curve
 E = divisor(ideal(x,y-z));
 assert(isCartier(E, IsGraded=>true));
 assert(isCartier(E, IsGraded=>true)); --verifying cache
 assert((not isCartier(E)) );
 assert((not isCartier(E)) ); --verifying cache
///

TEST /// --check #36 [isCartier, IsGraded=>true]
---checking and isCartier in the graded setting again
 R = QQ[x,y,u,v]/ideal(x*y-u*v);
 D = divisor(ideal(x,u));
 assert(isCartier(D, IsGraded=>true) and (not isCartier(D, IsGraded=>false)) and (isQCartier(5, D, IsGraded=>true) == 1) and (isQCartier(10, D, IsGraded=>false) == 0) );
///

TEST /// --check #37 (isLinearEquivalent)
--- checking (divisor, Module) and linearEquivalence
R = QQ[x,y,z]/ideal(x^2-y*z);
M = ideal(x, y)*R^1;
D = divisor(M);
E = divisor({-1, 2}, {ideal(x,y), ideal(x, z)});
assert(isLinearEquivalent(D, E))
///

TEST /// --check #38 [isLinearEquivalent, IsGraded=>true]
R = QQ[x,y,z];
D = 2*divisor(x);
E = divisor(y^2-x*z);
F = divisor(y^2*z - x^3 - x*z^2);
G = divisor(x-y+z);
assert(isLinearEquivalent(D,E, IsGraded=>true));
assert(isLinearEquivalent(F-G, E, IsGraded=>true));
assert(not isLinearEquivalent(G, D, IsGraded=>true));
assert(not isLinearEquivalent(E, -F, IsGraded=>true)); 
assert(isLinearEquivalent(zeroDivisor(R), F - G-D));
///

TEST ///--check #39 (sums a+b==b+a)
R = ZZ/7[x,y,u,v]/ideal(x*y-u*v);
D = divisor(x);
E = divisor({1,2}, {ideal(x,u), ideal(y,u)});
assert(D+E == E+D)
///

TEST ///--check #40 (sums 3*a==a+a+a but not 2*a = a+a+a)
R = ZZ/7[x,y,u,v]/ideal(x*y-u*v);
D = divisor(x);
assert((3*D == D + D+ D) and (not(2*D == D + D + D)))
///

TEST ///--check #41 (isQCartier)
R = QQ[x,y,z]/ideal(x*y-z^2);
D = divisor(ideal(x,z));
assert( isQCartier(10, D) == 2);
assert( isQCartier(10, 1/3*D) == 6);
assert( isQCartier(10, zeroDivisor(R)) == 1);
///

TEST ///--check #41 (isQCartier)
R = QQ[x,y,u,v]/ideal(x*y-u*v);
D = divisor(ideal(x,u));
assert( isQCartier(10, D) == 0);
assert( isQCartier(10, D, IsGraded=>true) == 1);
///

TEST ///--check #42 (isQCartier)
R= ZZ/13[x,y,z]/(z*y^2 - x^3 + z^2*x);
D = divisor(ideal(x-5*z, y-4*z)); --should be a point of order 4
O = divisor(ideal(z, x)); --point at infinity, cone over has order 3 in divisor class group since inflection point
assert(isQCartier(20, D-O) == 4); 
assert(isQCartier(20, D) == 12); --gcd(3,4) = 12
assert(isQCartier(20, D) == 12); --check cache
assert(isQCartier(20, D, IsGraded=>true) == 1);
assert(isQCartier(20, D, IsGraded=>true) == 1); --check cache
///

TEST ///--check #43 (isQCartier)
R = ZZ/13[x,y,z]/(z*y^2-x^3-2*x*z^2);
D = divisor(ideal(x-11*z, y-1*z)); --point such that P-O has order 10
O = divisor(ideal(x,z)); --point at infinity, order 3
assert(isQCartier(20, D-O) == 10);
assert(isQCartier(20, D-O) == 10); --check cache
///

TEST ///--check #44 (isQLinearEquivalent)
R = QQ[x,y,z]/ideal(x*y-z^2);
D = divisor(ideal(x,z));
assert(not isQLinearEquivalent(1, D, zeroDivisor(R)));
assert(isQLinearEquivalent(2, D, zeroDivisor(R)));
///


TEST /// --check #45 [isQLinearEquivalent, IsGraded=>true]
R = ZZ/7[x,y,z];
Z = zeroDivisor(R);
D = 1/3*divisor(x^3+y^3+z^3);
E = divisor(x+y+z);
assert( (isQLinearEquivalent(1, D, E, IsGraded=>true) == true) and (isQLinearEquivalent(1, Z, D, IsGraded=>true) == false) and (isQLinearEquivalent(1, D, Z, IsGraded=>false) == true) )
///

TEST /// --check #46 (nonCartierLocus)
---some nonCartierLocus tests
R = QQ[x,y,z]/ideal(x^2-y*z);
m = ideal(x,y,z);
D = divisor(ideal(x,y));
assert( (radical(nonCartierLocus(D)) == m));
assert( (radical(nonCartierLocus(D)) == m)); --checking the cache
assert( (nonCartierLocus(D, IsGraded=>true) == ideal(sub(1, R))) );
assert( (nonCartierLocus(D, IsGraded=>true) == ideal(sub(1, R))) ); --checking the cache
assert( (nonCartierLocus(zeroDivisor(R)) == ideal(sub(1,R))) );
///

TEST /// --check #47 (isSNC)
R = QQ[x,y,z];
D = divisor(x*y*(x+1)*z*(z-1));
E = divisor(x*y*z);
F = divisor(x*y*z*(x+y+z));
G = divisor(x*y-z^2);
assert( (isSNC(D) == true) and (isSNC(E) == true) and (isSNC(F) == false) and (isSNC(G) == false) );
assert( (isSNC(D) == true) and (isSNC(E) == true) and (isSNC(F) == false) and (isSNC(G) == false) );--check cache
///

TEST /// --check #48 (zeroDivisor, isSNC, isQCartier, mapToProjectiveSpace)
R = QQ[x,y,z];
D = 0*divisor(x);
E = zeroDivisor(R);
assert( (D == E) and (isCartier(D) == true) and (isQCartier(5, D) == 1) and (dim source mapToProjectiveSpace(D) == 1) and (isSNC(D) == true) and (D == floor(D)) );
assert(isFreeModule OO(D) == true);
assert(isFreeModule OO(D) == true) --verifying the cache
///

TEST /// --check #49 (isZeroDivisor)
R = QQ[x,y,z];
D = divisor({0, 0}, {ideal(x), ideal(y)});
assert(isZeroDivisor(D));
assert(isZeroDivisor(zeroDivisor(R)));
assert(not isZeroDivisor(2*divisor(x)));
assert(not isZeroDivisor(-3/2*divisor(y)));
assert(not isZeroDivisor(-11.111*divisor(z)));
///


TEST /// --check #50 (isVeryAmple) checks for very ample divisors #1 (divisors on elliptic curves)
R = QQ[x,y,z]/ideal(x^3+y^3-z^3);
D = divisor(ideal(x, y-z));
D2 = 2*D;
D3 = 3*D;
assert( (isVeryAmple(0*D) == false) and (isVeryAmple(1*D) == false) and (isVeryAmple(D2) == false)); 
assert (isVeryAmple(D2) == false); --check cache
assert (isVeryAmple(D3) == true);
assert (isVeryAmple(D3) == true); --check cache
///


TEST /// --check #51 (isVeryAmple) checks for very ample divisors #2 (divisors on P^1 x P^1)
R = QQ[x,y,u,v]/ideal(x*y-u*v);
D = divisor(ideal(x,u));
E = divisor(ideal(x, v));
DE = D+E;
assert( (isVeryAmple(D) == false) and (isVeryAmple(E) == false) and (isVeryAmple(DE) == true) );
assert(isVeryAmple(DE) == true); --check cache
assert(isVeryAmple(zeroDivisor(R)) == false);
Dn = -D;
assert(isVeryAmple(Dn) == false);
assert(isVeryAmple(Dn) == false); --check cache
///



TEST /// --check #52 (mapToProjectiveSpace)
R = QQ[x,y];
D = 3*divisor(x);
h = mapToProjectiveSpace(D);
assert(#(first entries vars source h) == 4);
g = mapToProjectiveSpace(zeroDivisor(R));
assert(#(first entries vars source g) == 1);
f = mapToProjectiveSpace(-D);
assert(#(first entries vars source f) == 0);
///

TEST /// --check #53 (idealPower)
R = QQ[x,y];
I = ideal(x,y);
Z = ideal(sub(0, R));
W = ideal(sub(1, R));
assert( (idealPower(5, I) == ideal(x^5, y^5)) and (idealPower(3, Z) == Z) and (idealPower(11, W) == W) );
///

TEST /// --check #54 (reflexify, Ideal) and isReflexive
    R = QQ[x,y];
    I = ideal(x*y, x^2);
    assert(reflexify(I) == ideal(x));
    assert(not isReflexive(I));
    assert(isReflexive(reflexify I));
    J = ideal(x,y);
    assert(reflexify(J) == ideal(sub(1,R)));
///

TEST /// --check #55 (reflexify, Module) and isReflexive
R = ZZ/5[x,y,z];
M = (ideal(x,y,z)*R^1)++R^1;
assert(isFreeModule(reflexify M) and (rank reflexify(M) == 2));
assert(not isReflexive(M));
assert(isReflexive(reflexify M));
///

TEST /// --check #56 (reflexivePower)
R = QQ[x,y,u,v]/ideal(x*y-u*v);
I = ideal(x,u);
Istar = reflexivePower(5, I);
assert(isSubset(set{Istar}, set primaryDecomposition(I^5)));
///

TEST /// --check #57 (torsionSubmodule)
R = QQ[x,y];
M = R^1 ++ (R^1/ideal(x^2*y));
N = R^1 ++ (R^1/ideal(x,y));
assert(ann(torsionSubmodule(M)) == ideal(x^2*y));
assert(ann(torsionSubmodule(N)) == ideal(x,y));
assert(ann(torsionSubmodule(R^0)) == ideal(sub(1, R)));
assert(ann(torsionSubmodule(R^1)) == ideal(sub(1, R)));
///

TEST /// --check #58 (dualize, Ideal)
R = QQ[x,y,u,v]/ideal(x*y-u*v);
I = ideal(x,u);
assert( isPrincipal(divisor(I) + divisor(dualize(I))) );
///

TEST /// --check #59 (dualize, Module)
R = QQ[x];
M = R^1/ideal(x^3);
assert( ann(dualize(M)) == ideal(sub(1, R)) );
S = QQ[a,b]/ideal(a*b);
N = S^1/ideal(a);
assert( ann(dualize(N)) == ideal(a) );
///

TEST /// --check #60 (isDomain)
 	  R = QQ[x,y,z]/ideal(x^2-y*z)
	  assert(isDomain(R))
	  S = ZZ/5[x,y]/ideal(x^2*y^3)
	  assert(not isDomain(S))
///

TEST /// --check #61 (isSmooth)
       	  R = QQ[x, y, u, v];
       	  I = ideal(x * y - u * v);
       	  assert(not isSmooth(I));
       	  assert(isSmooth(I, IsGraded => true));
///

TEST /// --check #62 (baseLocus, mapToProjectiveSpace)
R = QQ[x,y,z]/ideal(x^3+y^3-z^3);
P = divisor(ideal(x, y- z)); --point on an elliptic curve
assert(baseLocus(P) == ideal(x,y-z)); 
assert(baseLocus(2*P) == ideal(sub(1,R)) );
g = mapToProjectiveSpace(2*P);
assert(instance(source(g), PolynomialRing));
assert(#(first entries vars source g) == 2); --we are mapping to P^1
///


TEST /// --check #63 (isEffective)
R = QQ[x,y];
assert( isEffective(divisor(x)) );
assert( isEffective(zeroDivisor(R)) );
assert( isEffective(0.001*divisor(x^2)) );
assert( not isEffective(divisor(x/y)) );
assert( not isEffective(divisor({2, -1}, {ideal(x), ideal(y)})) );
assert( not isEffective(divisor({2, 0, -1}, {ideal(x), ideal(x-y), ideal(y)})) );
assert( isEffective(divisor({2, 0}, {ideal(x), ideal(y)})) );
///

TEST ///--check #64 (isPrime)
R = QQ[x,y];
assert(isPrime(divisor(x)));
assert(isPrime(divisor(y^2-x^3)));
assert(not isPrime(2*divisor(y^2-x^3)));
assert(not isPrime(-3*divisor(y^2-x^3)));
assert(not isPrime(zeroDivisor(R)));
assert(not isPrime(divisor(x*y)));
///

TEST ///--check #64 (isHomogeneous)
R = QQ[x,y,z];
assert(isHomogeneous(divisor(x)));
assert(isHomogeneous(divisor(x) + 3*divisor(x^3+y^3-z^3) - 2*divisor(y^2-x*y)));
assert(isHomogeneous(zeroDivisor(R)));
assert(not isHomogeneous(divisor({1,0,2}, {ideal(x), ideal(y-x^2), ideal(z)})));
assert(isHomogeneous(cleanSupport divisor({1,0,2}, {ideal(x), ideal(y-x^2), ideal(z)})));
assert(not isHomogeneous(divisor(y^2+x^3-z^5)));
///

TEST /// --check #65 (embedAsIdeal)
R = ZZ/5[x,y];
M = ideal(x^2,x*y)*R^1;
J = embedAsIdeal(M, ReturnMap=>true);
assert(ann( (reflexify(J#0)*R^1)/((J#0)*R^1) ) == ideal(x,y));
assert(dim (ker(J#1)) <= -1);
///

TEST /// --check #66 (embedAsIdeal) (test extreme situations)
R = ZZ/5[x,y]/ideal(x*y);
M = R^1/ideal(x); --should embed as x
J = trim embedAsIdeal(M, MTries=>100);
assert(ann(R^1/J) == ideal(y));
assert(embedAsIdeal(R^0) == ideal(sub(0, R)));
///

TEST /// --check #67 (isSNC) (IsGraded=>true)
R = QQ[x,y,z]/ideal(x^3+y^3-z^3);
D = divisor(ideal(x, y-z)) + divisor(ideal(y, x-z));
assert(isSNC(D, IsGraded=>true));
assert(isSNC(D, IsGraded=>true)); --check cache
assert(not isSNC(D, IsGraded=>false));
assert(not isSNC(D, IsGraded=>false)); --check cache
assert(isSNC(zeroDivisor(R), IsGraded=>true));
///

TEST /// --check #68 (ideal(Divisor))
R = ZZ/5[x,y,z]/ideal(x^2-y*z);
D = divisor(ideal(x,z));
E = 2*D;
assert (ideal(D) == ideal(x,z));
assert (ideal(D) == ideal(x,z));--checking the cache
assert (ideal(E) == ideal(z));
assert (ideal(E) == ideal(z));--checking the cache
///

end

---***************************
---*******CHANGELOG***********
---***************************
--changes 0.3
------Addressed referee comments and made other changes
------  In particular
------------          AmbRing -> AmbientRing (throughout)
------------          CoeffType -> CoefficientType (throughout)
------------          Changed isQLinearEquivalent to take an index for which to check Q-linear equivalence up to.
------------          Added the ideal names/generators to the cache
------------          Added nonCartierLocus to the cache and added checking (updated documentation to list this as being cached)
------------          Added isPrincipal to the cache and added checking (updated documentation to list this as being cached)
------------          Added ideal(Divisor) to the cache and added checking (updated documentation to list this as being cached)
------------          Added OO(Divisor) to the cache and added checking (updated documentation to list this as being cached)
------------          Added isCartier to the cache and added checking (updated documentation to list this as being cached)
------------          Added isQCartier to the cache and added checking (updated documentation to list this as being cached)
------------          Added isSNC to the cache and added checking (updated documentation to list this as being cached)
------------          Added isVeryAmple to the cache and added checking (updated documentation to list this as being cached)
------------          The Unsafe option has been changed to Safe
------------          Numerous improvements to the documentation.
------------          The internal structure of the divisor has changed.


--changes 0.2
------Addressed referee comments and made other changes
------  In particular:
------------          removed rationalDivisor and realDivisor 
------------          added cache to BasicDivisor
------------          changes to (net, BasicDivisor) changing how divisors are displayed
------------          various functions simplified
------------          various functions now do not crash when passing them boundary case input
------------          functions now specify the Type of their output
------------          verifyDivisor is now isWellDefined (also removed some checking from the constructor)
------------          coeff is now coefficient
------------          getCoeffList is now coefficients
------------          getGBList is now gbs
------------          getAmbientRing(BasicDivisor) is now ring(BasicDivisor)
------------          the key specifying the ambient ring in a BasicDivisor is now a symbol, not a string
------------          simplifyDiv is now cleanSupport, 
------------          trim is now a function that calls cleanSupport and also trims the ideals displayed to the user
------------          divPlus / divMinus are now positivePart / negativePart
------------          removed isDivAmbient and sameDivAmbient
------------          renamed toQDiv -> toQWeilDivisor and toRDiv -> toRWeilDivisor and toWDiv -> toWeilDivisor
------------          renamed divisorToModule(D) -> OO(D)
------------          renamed divisorToIdeal(D) -> ideal(D)
------------          merged the WithSection versions to include a option Section=> instead
------------          idealToDivisor(Ideal) is now just divisor(Ideal)
------------          moduleToDivisor(Module) is now just divisor(Module)
------------          add support for pullback using the notation f^* D
------------          renamed moduleToIdeal -> embedAsIdeal
------------          isWDiv -> isWeilDivisor
------------          divPullBack -> pullback
------------          isDivPrime -> isPrime
------------          isDivReduced -> isReduced
------------          isDivPrincipal -> isPrincipal
------------          isDivGraded -> isHomogeneous
------------          reflexifyIdeal -> reflexify and reflexifyModule -> reflexify
------------          dualizeIdeal -> dualize and dualizeModule -> dualize
------------          added Strategy options to reflexify, dualize, isReflexive, torsionSubmodule
------------          many improvements to the documentation
------------          renamed getPrimesList -> primes
------------          added Option to choose the variable in mapToProjectiveSpace Variable=> ...
------------          substantially expanded tests


--changes 0.1sa
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
------added a second algorithm to pullback which works for Cartier divisors even in the map is not flat or finite
------added getLinearDiophantineSolution which makes findElementOfDegree work in the multigraded setting


----FUTURE PLANS------
--cache various computations for later use
--speed up the divisor stuff by doing some simultaneous dimension computations for checking for the zero divisor (seems to be faster frequently)
--refine the ability to compute relative canonical divisors.  Right now it should handle things pretty well, but there are some ways it can be improved (can we do canonical bundle formulas for fibrations I wonder?)
--for not necessarily S2 graded rings, handle things (this should be pretty easy, it just takes some re-coding)
--can we check ampleness, is there a better way to do this than to check if some power is very ample?  Hm, how do we prove that something is *not* ample...
--can we check semi-ampleness, is there a better way to do this than to check if some power induces a base point free morphism?  Hm, how do we prove something is *not* semi-ample
--we ought to be able to do bigness by seeing if nD - (ample) has a section for large n (or at least give an affirmative answer)
--do lots of optimization when the IsGraded flag is true (I'm sure things can be speeded up)
--compare the current nonCartierLocus with the approach that David suggested, using minors of a presentation matrix, probably what's there now is faster but...
--making checking principalness and checking linearEquivalence work better for non-homogeneous rings and ideals.  Sometimes it can give false negatives now, although the user is warned about before a (false?) negative is provided, this might be unavoidable but maybe it can give fewer false negatives.
