newPackage ( "TorAlgebra",
    Version => "2.1",
    Date => "5 November 2020",
    Authors => {
	{ Name => "Lars Winther Christensen",
	  Email => "lars.w.christensen@ttu.edu",
	  HomePage => "http://www.math.ttu.edu/~lchriste/index.html" },
	{ Name => "Oana Veliche", 
	  Email => "o.veliche@northeastern.edu",
	  HomePage => "https://web.northeastern.edu/oveliche/index.html" }
	},
    Headline => "classification of local rings based on multiplication in homology",
    Keywords => {"Homological Algebra"},
    PackageImports => { "OldChainComplexes", "LocalRings" },
    Certification => { -- this package was certified under its old name, "CodepthThree"
	 "journal name" => "The Journal of Software for Algebra and Geometry",
	 "journal URI" => "https://msp.org/jsag/",
	 "article title" => "Local rings of embedding codepth 3: A classification algorithm",
	 "acceptance date" => "2014-07-11",
         "published article DOI" => "10.2140/jsag.2014.6.1",
	 "published article URI" => "https://msp.org/jsag/2014/6-1/p01.xhtml",
	 "published code URI" => "https://msp.org/jsag/2014/6-1/jsag-v6-n1-x01-code.zip",
	 "release at publication" => "4b2e83cd591e7dca954bc0dd9badbb23f61595c0",
	 "legacy name" => "CodepthThree",
	 "version at publication" => "1.0",
	 "volume number" => "6",
	 "volume URI" => "https://msp.org/jsag/2014/6-1/"
	 },
    Reload => false,
    DebuggingMode => false
    )

export { 
    "torAlgData", 
    "torAlgDataPrint", 
    "torAlgDataList", 
    "torAlgClass", 
    "isCI",
    "isGorenstein",
    "isGolod",
    "attemptsAtGenericReduction", 
    "setAttemptsAtGenericReduction" }

if version#"VERSION" < "1.4" then error "This package was written for Macaulay2 ver. 1.4 or higher";

-- Workaround because Macaulay2 ver. 1.6 has a minor bug --

if version#"VERSION" == "1.6" then ( complete GradedModule := (M) -> M );



--==========================================================================
-- EXPORTED FUNCTIONS
--==========================================================================

----------------------------------------------------------------------------
-- Implementation of the classification algorithm
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- torAlgData

-- R a quotient of a polynomial algebra

-- Returns a hash table with the following data of the local ring
-- obtained by localizing R at the irrelevant maximal ideal:

-- c: codepth
-- e: embedding dimension
-- h: Cohen-Macaulay defect
-- m: minimal number of generators the defining ideal
-- n: type
-- Class: class (B ,C ,G ,GS, GT, GH, H ,S, T) in the classification due to 
--   Weyman and to Avramov Kustin and Miller; see [L.L. Avramov, 
--   A cohomological study of local rings of embedding codepth 3, 
--   J. Pure Appl. Algebra, 216, 2489--2506 (2012)] and [L.L. Avramov, Homological
--   asymptotics  of modules over local rings, Commutative algebra (Berkeley, CA, 1987), 
--   Math. Sci. Res. Inst.  Publ., vol. 15, Springer, New York, 1989, pp.~33--62] 
--   for an overview
-- p: classification parameter
-- q: classification parameter
-- r: classification parameter
-- isCI: boolean, TRUE if R is complete intersection, otherwise FALSE
-- isGolod: boolean, TRUE if R is Golod, otherwise FALSE
-- isGorenstein: boolean, TRUE if R is Gorenstein, otherwise FALSE
-- PoincareSeries: rational function, Poincare series in closed form
-- BassSeries: rational function, Bass series in closed form

T := degreesRing {1};
t := T_0;

toralgdata = R -> (
    limit := 4;
    if not R.?attemptsAtGenericReduction then (
	tries := 25
	)
    else (
	tries = R.attemptsAtGenericReduction
	);

    isGorenstein := false;
    isCI := false;
    isGolod := false;

    c := "UNDETERMINED";
    e := "UNDETERMINED";
    h := "UNDETERMINED";
    m := "UNDETERMINED";
    n := "UNDETERMINED";
    p := "UNDETERMINED";
    q := "UNDETERMINED";
    r := "UNDETERMINED";
    cls:= "UNDETERMINED";
    Bas := "UNDETERMINED";
    Poi := "UNDETERMINED";


-- Step 1: Classify R (to the extent possible based on the presentation):
--         if it is the zero ring or defined by the zero ideal.

    if R == 0 then (
	cls = "zero ring";
	m = 1;
	)
    else (
	if ideal R == 0 then (
	    isCI = true;
	    isGolod = true;	    
	    isGorenstein = true;
	    cls = "C";
	    R = ring zeroIdeal R;
	    c = 0;
	    e = numgens ideal vars R;
	    h = 0;
	    m = 0;
	    n = 1;
	    p = 0;
	    q = 0;
	    r = 0;
	    Poi = computePoincareC (e, c);
	    Bas = t^e;
	    )
    	else (
	    R = prune R;
	    Q := ambient R;
	    if ( not isSubset( ideal R, ideal vars Q ) ) then (
		cls = "zero ring";
		m = 1;
		)
	    else (
		if isHomogeneous ideal R then (
		    I := ideal R;
		    F := res I;
		    )
		else (
		    setMaxIdeal ideal vars Q;		
		    I = ideal localMingens (localResolution ideal R).dd_1;
		    if not isSubset(I, (ideal vars Q)^2) then error "Please provide presentation without linear terms." else (
		    F = localResolution I;
		    R = Q/I;
		    );
		);
		c = length F;
		e = numgens ideal vars R;
		h = c - codim R;
		m = rank F_1;
		n = rank F_c;
		l := m-1;
    	    	if e == 1 then poi := 1;
    	    	if e == 2 then poi = 1+t;
		if e >= 3 then poi = new Power from { 1+t, e-1 };
		);
	    );
	);
    
-- Step 2: Classify R (to the extent possible based on the free resolution of R over Q):
--         if c = 0, 1, 2
--         if c = 3 and [ [h = 0 and n = 1] or h = 2 ]
--         if c = 4 = m
--    	   if c >= 5 and h = 0 and n = 1
    
    if cls == "UNDETERMINED" then (

	if c <= 1 then (
    	    isCI = true;
    	    isGolod = true;	    
	    isGorenstein = true;	     
	    cls = "C"; 
	    p = 0;
	    q = 0;
	    r = 0;
	    Poi = computePoincareC (e, c);
	    Bas = t^(e-c);
 	    );

	if c == 2 then (
		q = 0;
		r = 0;
	    if h == 0 and n == 1 then (
		isCI = true;
		isGorenstein = true;		
		cls = "C";
		p = 1;
	    	Poi = computePoincareC (e, c);
		Bas = t^(e-c);
		)
	    else (
		isGolod = true;
		cls = "S";
		p = 0;
		den := 1 - t - l*t^2;
	    	Poi = new Divide from { poi , den };
	    	Bas = adjustBass (1 + t - t^2, den, e, c);
		);
	    );

	if c == 3 then (
    	    if h == 0 and n == 1 then (
		isGorenstein = true;
	    	q = 1;
		r = m;
		Bas = t^(e-c);
    		if r == 3 then (
		    isCI = true;
    		    cls = "C";
    		    p = 3;
		    Poi = computePoincareC (e, c);
		    )
		else (
    		    cls = "G";
    		    p = 0;
		    Poi = new Divide from { poi,  1 - t - l*t^2 - t^3 + t^4 };
		    );
		)
	    else (
	    	if h == 2 then (
		    isGolod = true;
		    cls = "H";
		    p = 0;
		    q = 0;
		    r = 0;
		    den = 1 - t - l*t^2 - n*t^3;
		    Poi = new Divide from { poi, den };
		    Bas = adjustBass (n + l*t + t^2 - t^3, den, e, c);
		    );
		);
	    );

	if c >= 4 then (
	    r = "-";
	    if h == 0 and n == 1 then (
		isGorenstein = true;
		r = m;
		Bas = t^(e-c);
		if m == c then (
		    isCI = true;
		    cls = "C";
		    p = binomial(c,2);
		    q = binomial(c,3);
		    Poi = computePoincareC (e, c);		    
		    )
		else (
		   if c >= 5 then (
		   	cls = "Gorenstein";
			);
		    );
		)
	    else (
		num := 0;
		limit = max{c+1,e};
		);
	    );

	);

-- Step 3: Classify R (to the extent that it is possible 
--         based on the free resolution of the residue field):
--    	   if c = 3 and [p = 2 or p > 3 or q > 1 ]
--    	   if c >= 4
    
    if cls == "UNDETERMINED" then (
	if isHomogeneous ideal R then (
	    L := res( ideal vars R, LengthLimit => limit ); 
	    )
	else (
	    setMaxIdeal ideal vars R;
    	    L = localResolution( R^1/(ideal vars R), LengthLimit => limit ); 
	    );
	b := for i from 0 to limit list rank L_i;
	
    	if c == 3 then (    
	    p = n + l*b#1 + b#2 - b#3 + binomial(e-1,3);
	    q = (n-p)*b#1 + l*b#2 + b#3 - b#4 + binomial(e-1,4);
	    den = 1 - t - l*t^2 - (n-p)*t^3 + q*t^4;
	    if p == 2 or p > 3 or q > 1 then (
	    	cls = "H";
	    	r = q;
	    	num = n + (l-r)*t - p*t^2 - t^3 + t^4;
    	    	Poi = new Divide from { poi , den };
    	    	Bas = adjustBass (num , den, e, c);
	    	);
	    );
	
    	if c == 4 and isGorenstein then (
	    p = 2 + (l-2)*b#1 + 2*b#2 - b#3 + binomial(e-2,3);
	    q = p - 1 - (p-2)*b#1 + (l-2)*b#2 + 2*b#3 - b#4 + binomial(e-2,4);
	    poi = new Power from { 1+t, e-2 };
	    den = 1 - 2*t - (l-2)*t^2 + (p-2)*t^3 + (q-p+1)*t^4;
	    if p == 0 then (
	    	cls = "GS";
	    	Poi = new Divide from { poi, den };
	    	)
	    else (
	    	Poi = new Divide from { poi, den - t^5};	    
	    	if p == q then (
		    cls = "GT";
		    )
	    	else (
		    cls = "GH";
		    );
	    	);
	    );
	
    	if c >= 4 and not isGorenstein then (	
	    f := for i from 0 to c list rank F_i;
    	    f = f|for i from 1 to limit - c - 1 list 0;
	    j := 2;
	    while cls == "UNDETERMINED" and j <= limit do (
		if b#j != binomial(e,j) +  sum for i from 0 to j-2 list b#i*f#(j-1-i) then (
		    cls = "no class";
		    )
		else (
		    if j == limit then (
			isGolod = true;
			cls = "Golod";
	    		p = 0;
	    		q = 0;
		    	den = 1 + sum for j from 1 to c list (
			    (-1)^j*(sum for i from 0 to j-1 list (-1)^i*f#i)*t^j
			    );
		    	Poi = new Divide from { poi, den };
		    	num = -t^c + t^(c-1) + sum for j from 0 to c-2 list ( 
			    (sum for i from 0 to j list (-1)^i*rank F_(c-j+i))*t^j
			    );
		    	Bas = adjustBass(num, den, e, c);
			);
		    );
		j = j+1;
		);
	    );
	);

-- Step 4: Classify R (based on Bass numbers):
--    	   if c = 3 

    if cls == "UNDETERMINED" and c == 3 then (

	if p == 0 then (
	    if q == 0 then (
		isGolod = true;
		cls = "H";
		r = q;
		num = n + l*t + t^2 - t^3;
		)
	    else (
		if isHomogeneous ideal R then (
		    data := computeBass1 (Q, R, I, e, chainComplex(L.dd_1,L.dd_2), tries)
		    )
		else (
    	    	    data = computeBass2 (Q, R, I, e, chainComplex(L.dd_1,L.dd_2), tries);
		    );
		mu := data#"bass";
		c':= data#"codepth";
		d':= data#"codim";
		e':= data#"edim";	  
		if (  c' != 3 or d' != codim R or e' != 3 or mu#0 != n ) then (
	    	    error "Failed to compute Bass numbers. You may raise the 
	    	    number of attempts to compute Bass numbers via a generic reduction 
	    	    with the function setAttemptsAtGenericReduction and try again.";	    
	    	    )
		else (
	    	    r = l + n - mu#1;
		    if q == r then (
			cls = "H";
			num = n + (l-r)*t - t^3 + t^4;
			)
		    else (
		    	cls="G";
		    	num = n + (l-r)*t - (r-1)*t^2 - t^3 + t^4;
		    	);
		    );
	    	);
	    );     

	if p == 1 then (
	    if q == 0 then (
		cls = "H";
		r = q;
		num = n + l*t - t^2 - t^3 + t^4;
		)
	    else (
		if isHomogeneous ideal R then (
		    data = computeBass1 (Q, R, I, e, chainComplex(L.dd_1,L.dd_2), tries)
		    )
		else (
    	    	    data = computeBass2 (Q, R, I, e, chainComplex(L.dd_1,L.dd_2), tries);
		    );
		mu = data#"bass";
		c'= data#"codepth";
		d'= data#"codim";
		e'= data#"edim";	  
		if (  c' != 3 or d' != codim R or e' != 3 or mu#0 != n ) then (
	    	    error "Failed to compute Bass numbers. You may raise the 
	    	    number of attempts to compute Bass numbers via a generic reduction 
	    	    with the function setAttemptsAtGenericReduction and try again.";	    
	    	    )
		else (
	    	    r = l + n - mu#1;
		    if q == r then (
			cls = "H";
			num = n + (l-r)*t - t^2 - t^3 + t^4;
			)
		    else (
		    	cls="B";
		    	num = n + (l-r)*t - t^2 + t^4;
		    	);
		    );
	    	);
	    );     

	if p == 3 then (
	    if q == 1 then (
		cls = "H";
		r = q;
		num = n + (l-1)*t - 3*t^2 - t^3 + t^4;
		)
	    else (
		if isHomogeneous ideal R then (
		    data = computeBass1 (Q, R, I, e, chainComplex(L.dd_1,L.dd_2,L.dd_3), tries)
		    )
		else (
    	    	    data = computeBass2 (Q, R, I, e, chainComplex(L.dd_1,L.dd_2,L.dd_3), tries);
		    );
		mu = data#"bass";
		c'= data#"codepth";
		d'= data#"codim";
		e'= data#"edim";	  
		if (  c' != 3 or d' != codim R or e' != 3 or mu#0 != n ) then (
	    	    error "Failed to compute Bass numbers. You may raise the 
	    	    number of attempts to compute Bass numbers via a generic reduction 
	    	    with the function setAttemptsAtGenericReduction and try again.";	    
	    	    )
		else (
		    if mu#2 == mu#1 + l*n - 3 then (
			cls = "H";
			r = q;
			num = n + l*t - 3*t^2 - t^3 + t^4;
			)
		    else (
		    	cls="T";
			r = q;
			num = n + l*t - 2*t^2 - t^3 + t^4;
			den = den - t^5;			
		    	);
		    );
	    	);

	    );
    
        Poi = new Divide from { poi , den };
    	Bas = adjustBass(num, den, e, c);
     	);
 
 
-- Step 5: Create error if R not classified

    if cls == "UNDETERMINED" then ( 
	error "Internal error: computed invariants not consistent"
	);
    

-- Step 6: Prepare data to be returned

    torAlg := hashTable { 
	"c" => c,
	"e" => e,
	"h" => h,
	"m" => m,
	"n" => n,	  
	"p" => p,
	"q" => q,
	"r" => r,
	"Class" => cls,
	"isCI" => isCI,
	"isGorenstein" => isGorenstein,
	"isGolod" => isGolod,
	"PoincareSeries" => Poi,
	"BassSeries" => Bas,
	}
    )

torAlgData = method()

torAlgData( QuotientRing ) := R -> (
    if R.cache#?"torAlg" then result := R.cache#"torAlg" else (
	if (ideal R).cache#?"torAlg" then result = (ideal R).cache#"torAlg" else (
	    result = toralgdata R; 
    	    R.cache#"torAlg" = result;
    	    (ideal R).cache#"torAlg" = result;	    
	    );
    );
    result
    )
    
torAlgData( Ideal ) := I -> (
    if I.cache#?"torAlg" then result := I.cache#"torAlg" else (
	result = toralgdata ((ring I)/I);
	I.cache#"torAlg" = result;
	);
    result
    )


----------------------------------------------------------------------------
-- setAttemptsAtGenericReduction
--
-- R a quotient of a polynomial algebra
-- n a positive integer
--
-- Sets R.attemptsAtGenericReduction = n

setAttemptsAtGenericReduction = (R,n) -> (
    R.attemptsAtGenericReduction = n;
    toString(n^2)|" attempt(s) will be made to compute the Bass numbers via a generic reduction"
    )


----------------------------------------------------------------------------
-- Functions for presenting classification data
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- torAlgClass
--
-- R a quotient of a polynomial algebra
--
-- Returns the (parametrized) class of the ring ring obtained by localizing 
-- R at the irrelevant maximal ideal

torAlgClass = method()

torAlgClass (QuotientRing) := R -> (
    torAlg := torAlgData R;
    cls := torAlg#"Class";
    if cls == "zero ring" or cls == "S" 
    or cls == "B" or cls == "T" 
    or cls == "GS" or cls == "GT"then ( 	
    	S := cls;
	);
    if cls == "C" then (
	S = cls|"("|toString torAlg#"c"|")";
	);
    if cls == "G" then (
	if isGorenstein R then (
	    S = cls|"("|toString torAlg#"r"|")"|", Gorenstein"
	    )
	else (
	    S = cls|"("|toString torAlg#"r"|")"
	    );
	);    
    if cls == "H" then (
	S = cls|"("|toString torAlg#"p"|","|toString torAlg#"q"|")";
	);
    if cls == "GH" then (
	S = cls|"("|toString torAlg#"p"|")";
	);    
    if cls == "Gorenstein" or cls == "Golod" or cls == "no class" then (
	S = "codepth "|toString torAlg#"c"|" "|cls;
	);        
    S )        

torAlgClass( Ideal ) := I -> torAlgClass((ring I)/I)

----------------------------------------------------------------------------
-- torAlgDataPrint
--
-- R a quotient of a polynomial algebra
-- L a list of keys for the hash table returned by torAlgData
--
-- Returns a string of keys and their values

torAlgDataPrint = method()

torAlgDataPrint( QuotientRing, List) := (R,L) -> (
    torAlg := torAlgData R;
    fn := temporaryFileName();
    for x in L do (
	fn << toString x << "=" << toString(torAlg#(toString x)) << " "
	);
    fn << endl << close;
    get fn
    )

torAlgDataPrint( Ideal, List) := (I,L) -> torAlgDataPrint((ring I)/I, L)

----------------------------------------------------------------------------
-- torAlgDataList
--
-- R a quotient of a polynomial algebra
-- L a list of keys for the hash table returned by torAlgClass
--
-- Returns a list of the values of the specified keys


torAlgDataList = method()

torAlgDataList( QuotientRing, List) := (R,L) -> (
    torAlg := torAlgData R;
    for x in L list torAlg#(toString x)
    )

torAlgDataList( Ideal, List) := (I,L) -> torAlgDataList((ring I)/I, L)

----------------------------------------------------------------------------
-- isCI
--
-- R a quotient of a polynomial algebra
--
-- Returns TRUE if R is complete intersection and FALSE otherwise

isCI = method()

isCI( QuotientRing ) := R -> ( 
    (torAlgData R)#"isCI"
    )

isCI( Ideal ) := I -> isCI((ring I)/I)

----------------------------------------------------------------------------
-- isGorenstein
--
-- R a quotient of a polynomial algebra
--
-- Returns TRUE if R is Gorenstein and FALSE otherwise

isGorenstein = method()

isGorenstein( QuotientRing) := R -> (
    (torAlgData R)#"isGorenstein"
    )

isGorenstein( Ideal ) := I -> isGorenstein((ring I)/I)

----------------------------------------------------------------------------
-- isGolod
--
-- R a quotient of a polynomial algebra
--
-- Returns TRUE if R is Golod and FALSE otherwise

isGolod = method()

isGolod( QuotientRing) := R -> (
    (torAlgData R)#"isGolod"
    )

isGolod( Ideal ) := I -> isGolod((ring I)/I)

--==========================================================================
-- INTERNAL ROUTINES
--==========================================================================

----------------------------------------------------------------------------
-- Routines used by torAlgData
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- computeBass1

-- Q is a polynomial algebra
-- I is a homogeneous ideal of Q contained in the irrelevant maximal ideal
-- R is the quotient Q/I
-- e is the embedding dimension of R

-- Returns a hash table with Bass numbers of the local ring
-- obtained by localizing R at the irrelevant maximal ideal
-- plus data for verification that computations went OK

computeBass1 = (Q, R, I, e, L, tries) -> ( 
    ll := length L;
    data := new MutableHashTable;
    if e == 3 then (
    	E := dual L;
    	data#"edim" = 3;
    	data#"codepth" = 3;
	data#"codim" = codim R;
	)
    else (
	R' := Q;
	c' := 0;
	e' := 0;
	i := 0;
	while (	( c' != 3 or e' != 3 or codim R' != codim R ) 
	    and i < tries ) do (
	    i = i+1;
	    X := zeroIdeal Q;
	    j := 0;
	    while grade X != e - 3 and j < tries do (
	    	j = j+1; 
	    	X = zeroIdeal Q;
	    	for k from 4 to e do (
    		    X = X + ideal random(1,Q);
		    );
	    	);
	    Q' := Q/X;
	    R' = prune( Q'/promote(I,Q') );
	    c' = length res ideal R';
	    e' = numgens ideal vars R';
	    );
    	data#"edim" = e';
    	data#"codepth" = c';
    	data#"codim" = codim R';
	E = dual res( ideal vars R', LengthLimit => ll ); 
	);
    data#"bass" = for i from 0 to ll-1 list degree HH^i(E); 
    data
    )

----------------------------------------------------------------------------
-- computeBass2
-- serves same purpose as computeBass1 but handles non-homogeneous
-- rings using the LocalRings package

-- Q is a polynomial algebra
-- I is an ideal of Q contained in the irrelevant maximal ideal
-- R is the quotient Q/I
-- e is the embedding dimension of R

-- Returns a hash table with Bass numbers of the local ring
-- obtained by localizing R at the irrelevant maximal ideal
-- plus data for verification that computations went OK

computeBass2 = (Q, R, I, e, L, tries) -> ( 
    ll := length L;
    data := new MutableHashTable;
    if e == 3 then (
    	E := dual L;
    	data#"edim" = 3;
    	data#"codepth" = 3;
	data#"codim" = codim R;
	)
    else (
	R' := Q;
	c' := 0;
	e' := 0; 
	i := 0;
	while (	( c' != 3 or e' != 3 or codim R' != codim R ) 
	    and i < tries ) do (
	    i = i+1;
	    X := zeroIdeal Q;
	    j := 0;
	    while grade X != e - 3 and j < tries do (
	    	j = j+1;
	    	X = zeroIdeal Q;
	    	for k from 4 to e do (
       		    X = X + ideal random(1,Q);
		    );
	    	);
	    Q' := Q/X;
	    R' = prune( Q'/promote(I,Q') );
	    setMaxIdeal ideal vars ambient R';
	    c' = length localResolution ideal R';
	    e' = numgens ideal vars R';
	    );
    	data#"edim" = e';
    	data#"codepth" = c';
    	data#"codim" = codim R';
	setMaxIdeal ideal vars R';
	E = dual localResolution( R'^1/(ideal vars R'), LengthLimit => ll ); 
	);
    data#"bass" = for i from 0 to ll-1 list degree HH^i(E); 
    data
    )

----------------------------------------------------------------------------
-- computePoincareC

-- e is the embedding dimension of a complete intersection local ring 
-- c is the codepth of the ring 

-- Returns the Poincare series of the local ring in closed form

computePoincareC = (e, c) -> (
    if c == 0 then (
	if e == 0 then Ser:= 1;
	if e == 1 then Ser = 1+t;
	if e >= 2 then Ser = new Power from { 1+t, e };
	)
    else (
	if e == c then Ser = new Divide from { 1 , new Power from { 1-t, c } };
	if e == c+1 then Ser = new Divide from { 1+t , new Power from { 1-t, c } };
	if e >= c+2 then Ser = new Divide from { new Power from { 1+t, e-c }, new Power from { 1-t, c } };
	);
    Ser
    )

----------------------------------------------------------------------------
-- adjustBass

-- e is the embedding dimension of a local ring 
-- c is the codepth of the ring 
-- num is the numerator of the closed form of the Bass series of an 
--    artinian reduction of the ring
-- den is the denominator of the same series

-- Returns the Bass series of the local ring in closed form

adjustBass = (num, den, e, c) -> (
    if e == c then (
	Ser := new Divide from { num , den }
	)
    else (
	Ser = ( new Power from { t^(e-c), 1 } )*( new Divide from { num, den } )
	);
    Ser
    )


----------------------------------------------------------------------------
-- Auxiliary routines
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- sup
--
-- C a chain complex
--
-- Returns the supremum of C: the highest degree of a non-zero module

sup = C -> (
    j := max C;
    while true do (
        if j < min C then (
	    break -infinity 
	    )
	else (
	    if C_j != 0 then (
		break j 
		)
	    else (
		j = j-1
		)
	    )
	)
    )


----------------------------------------------------------------------------
-- grade
--
-- I a homogeneous ideal in a polynomial algebra
--
-- Returns the grade of I

grade = I -> (
    - sup prune HH(dual res I)
    )

----------------------------------------------------------------------------
-- zeroIdeal
--
-- R a ring
--
-- Returns the zero ideal of R

zeroIdeal = R -> ideal (map(R^1,R^0,0)) 


--==========================================================================
-- DOCUMENTATION
--==========================================================================

beginDocumentation()

doc ///
  Key
    TorAlgebra
  Headline
    Classification of local rings based on multiplication in homology
  Description

    Text 
      Let $I$ be an ideal of a regular local ring $Q$ with residue
      field $k$. The length of the minimal free resolution of $R=Q/I$
      is called the codepth of $R$; if it is at most $3$, then the
      resolution carries a structure of a differential graded
      algebra. While the DG algebra structure may not be unique, the
      induced algebra structure on Tor$^Q$ ($R,k$) is unique and
      provides for a classification of such local rings.
      
      According to the multiplicative structure on Tor$^Q$ ($R,k$), a
      non-zero local ring $R$ of codepth at most 3 belongs to exactly one of
      the (parametrized) classes designated {\bf B}, {\bf C}(c), {\bf G}(r), {\bf
      H}(p,q), {\bf S}, or {\bf T}. An overview of the theory can be
      found in L.L. Avramov,  @HREF("https://doi.org/10.1016/j.jpaa.2012.03.012","A cohomological study of local rings
      of embedding codepth 3")@.

      There is a similar classification of Gorenstein local rings of codepth
      4, due to A.R. Kustin and M. Miller. There are four classes,
      which in the original paper
      @HREF("https://doi.org/10.1007/BF01215134", "Classification of the
      Tor-Algebras of Codimension Four Gorenstein Local rings")@, are called A, B, C,
      and D, while in the survey 
      @HREF("https://doi.org/10.1007/978-1-4612-3660-3_3","Homological asymptotics of
      modules over local rings")@ by
      L.L. Avramov, they are called CI, GGO, GTE, and GH(p),
      respectively. Here we denote these classes {\bf C}(c), {\bf GS},
      {\bf GT}, and {\bf GH}(p), respectively.
  
      The package implements an algorithm for classification of local
      rings in the sense discussed above. For rings of codepth at most
      3 it is described in L.W. Christensen and O. Veliche, 
      @HREF("http://dx.doi.org/10.2140/jsag.2014.6.1","Local
      rings of embedding codepth 3: A classification algorithm")@. The classification of
      Gorenstein rings of codepth 4 is analogous. 
      
      The package also recognizes Golod rings, Gorenstein rings, and
      complete intersection rings of any codepth. To recognize Golod
      rings the package implements a test found in J. Burke,
      @HREF("https://arxiv.org/abs/1508.03782","Higher
      homotopies and Golod rings")@. ///


doc ///
  Key
    torAlgData
    (torAlgData, QuotientRing)
  Headline
    invariants of a local ring and its class (w.r.t. multiplication in homology)
  Usage
    torAlgData R or torAlgData I
  Inputs
    R : QuotientRing
        of a polynomial algebra by an ideal contained in the irrelevant maximal ideal
  Outputs
      : HashTable
        with invariants of the local ring obtained by
  	localizing {\tt R} at the irrelevant maximal ideal
  Description
  
    Text 
      Computes invariants of the local ring obtained by localizing {\tt
      R} at the irrelevant maximal ideal and, provided that it has
      codepth at most 3, classifies it as belonging to one of the
      (parametrized) classes {\bf B}, {\bf C}(c), {\bf G}(r), {\bf
      H}(p,q), {\bf S}, or {\bf T}. Rings of higher codepth are
      classified as {\bf C}(c) (complete intersection), {\bf
      Gorenstein}, {\bf Golod}, or {\bf no class}. Gorenstein rings of
      codepth 4 are further classified as belonging to one of the
      (parametrized) classes {\bf C}(4), {\bf GS}, {\bf GT}, or {\bf
      GH}(p). It is also possible to call the function on the defining
      ideal of {\tt R}; see @TO (torAlgData,Ideal)@.
      
      Returns a hash table with the following data of the local ring:
  
      "c": codepth
      
      "e": embedding dimension
      
      "h": Cohen-Macaulay defect
      
      "m": minimal number of generators of defining ideal
      
      "n": type
      
      "Class": class ('B', 'C', 'G', 'GH', 'GS', 'GT', 'H', 'S', 'T',
      'Golod', 'Gorenstein' `zero ring', or 'no class')
      
      "p": classification parameter
      
      "q": classification parameter
      
      "r": classification parameter
      
      "isCI": boolean
      
      "isGorenstein": boolean
      
      "isGolod": boolean
      
      "PoincareSeries": Poincar\'e series in closed from (rational function)
      
      "BassSeries": Bass series in closed from (rational function)
      
    Example
      Q = QQ[x,y,z];
      data = torAlgData (Q/ideal(x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3))
      data#"PoincareSeries"

    Example
      Q = QQ[w,x,y,z];
      torAlgData (Q/ideal(w^2-x*y*z,x^3,y^3,x^2*z,y^2*z,z^3-x*y*w,x*z*w,y*z*w,z^2*w-x^2*y^2))

    Example
      Q = QQ[v,w,x,y,z];
      torAlgData (Q/(ideal(v^2-w^3)*ideal(v,w,x,y,z)))

    Example
      Q = QQ[u,v,w,x,y,z];
      torAlgData (Q/ideal(u^2,v^2,w^2-y^4,x^2,x*y^15))

    Text  
      To extract data from the hash table returned by the function one may use  
      @TO torAlgDataList@ and @TO torAlgDataPrint@.
       
  Caveat
      If the embedding dimension of {\tt R} is large, then the response time
      may be longer, in particular if {\tt R} is a quotient of a polynomial
      algebra over a small field. The reason is that the function attempts to
      reduce {\tt R} modulo a generic regular sequence of generators of the irrelevant
      maximal ideal. The total number of attempts made can be controlled with
      @TO setAttemptsAtGenericReduction@.
      
      If {\tt R} is a quotient of a polynomial algebra by a
      homogeneous ideal, then it is graded and the relevant invariants
      of the local ring obtained by localizing {\tt R} at the
      irrelevant maximal ideal can be determined directly from {\tt R}.
      If {\tt R} is a quotient of a polynomial algebra by a
      non-homogeneous ideal, then the function uses the package @TO
      LocalRings@ to compute some of the invariants.
///

doc ///
  Key
    (torAlgData, Ideal)
  Headline
    invariants of a local ring and its class (w.r.t. multiplication in homology)
  Usage
    torAlgData R or torAlgData I
  Inputs
    I : Ideal
        of a polynomial algebra contained in the irrelevant maximal ideal
  Outputs
      : HashTable
        with invariants of the local ring obtained by
  	localizing the quotient by {\tt I} at the irrelevant maximal ideal
  Description
  
    Text 
      See @TO (torAlgData,QuotientRing)@. 
      
    Example
      Q = QQ[x,y,z];
      data = torAlgData (ideal(x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3))
      data#"PoincareSeries"
///

doc ///
  Key
    torAlgClass
    (torAlgClass, QuotientRing)
  Headline
    the class (w.r.t. multiplication in homology) of a local ring
  Usage
    torAlgClass R or torAlgClass I
  Inputs
    R : QuotientRing
        of a polynomial algebra by an ideal contained in the irrelevant maximal ideal
  Outputs
      : String
        the (parametrized) class of the local ring obtained by
        localizing {\tt R} at the irrelevant maximal ideal, provided
        that this ring is non-zero and of codepth at most 3 or Gorenstein 
	or Golod; otherwise "no class"
  Description
  
    Text 
      Classifies the local ring obtained by localizing {\tt R} at the
      irrelevant maximal ideal; it is also possible to call the function on the
      defining ideal of {\tt R}; see @TO (torAlgClass,Ideal)@.  
      
    Text  
      If the local ring has codepth at most 3, then it is classified
      as belonging to one of the (parametrized) classes {\bf B}, {\bf
      C}(c), {\bf G}(r), {\bf H}(p,q), {\bf S}, or {\bf T}.
            
    Example
      Q = QQ[x,y,z];
      torAlgClass (Q/ideal(x))
      torAlgClass (Q/ideal(x*y))
      torAlgClass (Q/ideal(x^2,y^2))
      torAlgClass (Q/ideal(x^2,y^2,x*y))
      torAlgClass (Q/ideal(x^2,x*y,y*z,z^2))
      torAlgClass (Q/ideal(x^2,y^2,z^2))      
      torAlgClass (Q/ideal(x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3))
      torAlgClass (Q/ideal(x*z+y*z,x*y+y*z,x^2-y*z,y*z^2+z^3,y^3-z^3))
      torAlgClass (Q/ideal(x^2,y^2,z^2,x*y))
      torAlgClass (Q/ideal(x^2,y^2,z^2,x*y*z))
      
    Text  
      If the local ring is Gorenstein or Golod of codepth 4, then it is classified
      as belonging to one of the (parametrized) classes {\bf C}(4), {\bf GH}(p), 
      {\bf GS}, {\bf GT}, or {\bf codepth 4 Golod}.
      
    Example
      Q = QQ[w,x,y,z];
      torAlgClass (Q/ideal(w^2,x^2,y^2,z^2))
      torAlgClass (Q/ideal(y*z,x*z,x*y+z^2,x^2,w*x+y^2+z^2,w^2+w*y+y^2+z^2))
      torAlgClass (Q/ideal(z^2,x*z,w*z+y*z,y^2,x*y,w*y,x^2,w*x+y*z,w^2+y*z))
      torAlgClass (Q/ideal(x^2,y^2,z^2,x*w,y*w,z*w,w^3-x*y*z))
      torAlgClass (Q/(ideal(w,x,y,z))^2)

    Text	  
      If the local ring has codepth at least 5, then it is classified as belonging
      to one of the classes {\bf C}(c), if it is complete intersection, {\bf codepth c Gorenstein}, 
      if it is Gorenstein and not complete intersection, {\bf codepth c Golod}, if it is Golod,
      and {\bf no class} otherwise.
            
    Example
      Q = QQ[u,v,w,x,y,z];
      torAlgClass (Q/ideal(u^2,v^2,w^2,x^2+y^2, x^2+z^2))
      torAlgClass (Q/ideal(w^2,v*w,z*w,y*w,v^2,z*v+x*w,y*v,x*v,z^2+x*w,y*z,x*z,y^2+x*w,x*y,x^2))
      torAlgClass (Q/ideal(x^2*y^2,x^2*z,y^2*z,u^2*z,v^2*z,w^2*z))
      torAlgClass (Q/ideal(u^2,v^2,w^2,x^2,z^2,x*y^15))
      
    Text  
      If the defining ideal of {\tt R} is not contained in the irrelevant maximal ideal, 
      then the resulting local ring is zero, and the function returns {\bf zero ring}.
      
    Example
      Q = QQ[x,y,z];
      torAlgClass (Q/ideal(x^2-1))
///

doc ///
  Key
    (torAlgClass, Ideal)
  Headline
    the class (w.r.t. multiplication in homology) of a local ring
  Usage
    torAlgClass R or torAlgClass I
  Inputs
    I : Ideal
        of a polynomial algebra contained in the irrelevant maximal ideal
  Outputs
      : String
        the (parametrized) class of the local ring obtained by
        localizing the quotient by {\tt I} at the irrelevant maximal ideal, provided
        that this ring is non-zero and of codepth at most 3 or Gorenstein 
	or Golod; otherwise "no class"
  Description
  
    Text 
      See @TO (torAlgClass,QuotientRing)@.	
            
    Example
      Q = QQ[x,y,z];
      torAlgClass (ideal(x^2,y^2,z^2))      
///

doc ///
  Key
    isCI
    (isCI, QuotientRing)
  Headline
    whether the ring is complete intersection
  Usage
    isCI R or isCI I
  Inputs
    R : QuotientRing
        of a polynomial algebra by an ideal contained in the irrelevant maximal ideal
  Outputs
      : Boolean
        whether the local ring obtained by localizing {\tt R} at the irrelevant maximal ideal is complete intersection

  Description
  
    Text 
      Checks if the local ring obtained by localizing {\tt R} at the
      irrelevant maximal ideal is complete intersection. It is also possible to call the function on the defining
      ideal of {\tt R}; see @TO (isCI,Ideal)@.
      
    Example
      Q = QQ[x,y,z];
      isCI (Q/ideal(x^2,x*y,y*z,z^2))
      isCI (Q/ideal(x^2,y^2))
///

doc ///
  Key
    (isCI, Ideal)
  Headline
    whether the ring is complete intersection
  Usage
    isCI R or isCI I
  Inputs
    I : Ideal
        of a polynomial algebra contained in the irrelevant maximal ideal
  Outputs
      : Boolean
        whether the local ring obtained by localizing the quotient by {\tt I} at the irrelevant maximal ideal is complete intersection 

  Description
  
    Text 
      Checks if the local ring obtained by localizing the quotient by {\tt I} at the
      irrelevant maximal ideal is complete intersection. 
      
    Example
      Q = QQ[x,y,z];
      isCI (ideal(x^2,x*y,y*z,z^2))
      isCI (ideal(x^2,y^2))
///

doc ///
  Key
    isGorenstein
    (isGorenstein, QuotientRing)
  Headline
    whether the ring is Gorenstein
  Usage
    isGorenstein R or isGorenstein I
  Inputs
    R : QuotientRing
        of a polynomial algebra by an ideal contained in the irrelevant maximal ideal
  Outputs
      : Boolean
        whether the local ring obtained by localizing {\tt R} at the irrelevant maximal ideal is Gorenstein

  Description
  
    Text 
      Checks if the local ring obtained by localizing {\tt R} at the
      irrelevant maximal ideal is Gorenstein. It is also possible to call the function on the defining
      ideal of {\tt R}; see @TO (isGorenstein,Ideal)@.
      
    Example
      Q = QQ[x,y,z];
      isGorenstein (Q/ideal(x^2,x*y,y*z,z^2))
      isGorenstein (Q/ideal(x^2,y^2))
      isGorenstein (Q/ideal(x*z+y*z,x*y+y*z,x^2-y*z,y*z^2+z^3,y^3-z^3))
///

doc ///
  Key
    (isGorenstein, Ideal)
  Headline
    whether the ring is Gorenstein
  Usage
    isGorenstein R or isGorenstein I
  Inputs
    I : Ideal
        of a polynomial algebra contained in the irrelevant maximal ideal
  Outputs
      : Boolean
        whether the local ring obtained by localizing the quotient by {\tt I} at the irrelevant maximal ideal is Gorenstein

  Description
  
    Text 
      Checks if the local ring obtained by localizing the quotient by {\tt I} at the
      irrelevant maximal ideal is Gorenstein.
      
    Example
      Q = QQ[x,y,z];
      isGorenstein (ideal(x^2,x*y,y*z,z^2))
      isGorenstein (ideal(x^2,y^2))
      isGorenstein (ideal(x*z+y*z,x*y+y*z,x^2-y*z,y*z^2+z^3,y^3-z^3))
///

doc ///
  Key
    isGolod
    (isGolod, QuotientRing)
  Headline
    whether the ring is Golod
  Usage
    isGolod R or isGolod I
  Inputs
    R : QuotientRing
        of a polynomial algebra by an ideal contained in the irrelevant maximal ideal
  Outputs
      : Boolean
        whether the local ring obtained by localizing {\tt R} at the irrelevant maximal ideal is Golod

  Description
  
    Text 
      Checks if the local ring obtained by localizing {\tt R} at the
      irrelevant maximal ideal is Golod. It is also possible to call the function on the defining
      ideal of {\tt R}; see @TO (isGolod,Ideal)@.
      
    Example
      Q = QQ[x,y,z];
      isGolod (Q/ideal(x^2,x*y,y*z,z^2))
      isGolod (Q/ideal(x^2))
      isGolod (Q/(ideal(x,y,z))^2)      
///

doc ///
  Key
    (isGolod, Ideal)
  Headline
    whether the ring is Golod
  Usage
    isGolod R or isGolod I
  Inputs
    I : Ideal
        of a polynomial algebra contained in the irrelevant maximal ideal
  Outputs
      : Boolean
        whether the local ring obtained by localizing the quotient by {\tt I} at the irrelevant maximal ideal is Golod

  Description
  
    Text 
      Checks if the local ring obtained by localizing the quotient by {\tt I} at the
      irrelevant maximal ideal is Golod. 
      
    Example
       Q = QQ[x,y,z];
      isGolod (ideal(x^2,x*y,y*z,z^2))
      isGolod (ideal(x^2))
      isGolod ((ideal(x,y,z))^2)      
///

doc ///
  Key
    torAlgDataList
    (torAlgDataList, QuotientRing, List)
  Headline
    list invariants of a local ring
  Usage
    torAlgDataList(R,L) or torAlgDataList(I,L)
  Inputs
    R : QuotientRing
        of a polynomial algebra  by an ideal contained in the irrelevant maximal ideal
    L : List
        of keys from the hash table returned by @TO torAlgData@
	
  Outputs
      : List
        of values corresponding to the keys specified in {\tt L}
	
  Description
    Text 
      Extracts data from the hash table returned by @TO torAlgData@.  It is also possible to call the function on the
      defining ideal of {\tt R}; see @TO (torAlgDataList,Ideal,List)@.

    Example
      Q = QQ[x,y,z];
      R = Q/ideal(x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3);
      torAlgDataList( R, {m, n, Class, p, q, r, PoincareSeries, BassSeries} )            
///      

doc ///
  Key
    (torAlgDataList, Ideal, List)
  Headline
    list invariants of a local ring
  Usage
    torAlgDataList(R,L) or torAlgDataList(I,L)
  Inputs
    I : Ideal
        of a polynomial algebra contained in the irrelevant maximal ideal
    L : List
        of keys from the hash table returned by @TO torAlgData@
	
  Outputs
      : List
        of values corresponding to the keys specified in {\tt L}
	
  Description
    Text 
      Extracts data from the hash table returned by @TO torAlgData@.

    Example
      Q = QQ[x,y,z];
      I = ideal(x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3);
      torAlgDataList( I, {m, n, Class, p, q, r, PoincareSeries, BassSeries} )            
///      
      
doc ///
  Key
    torAlgDataPrint
    (torAlgDataPrint, QuotientRing, List)
  Headline
    print invariants of a local ring
  Usage
    torAlgDataPrint (R,L) or torAlgDataPrint(I,L)
  Inputs
    R : QuotientRing
        of a polynomial algebra  by an ideal contained in the irrelevant maximal ideal
    L : List 
        of keys from the hash table returned by @TO torAlgData@    
  Outputs
      : String
        of keys specified in {\tt L} together with their values
  Description
    Text 
      Extracts data from the hash table returned by @TO
      torAlgData@. It is also possible to call the function on the
      defining ideal of {\tt R}; see @TO (torAlgDataPrint,Ideal,List)@.

    Example
      Q = QQ[x,y,z];
      R = Q/ideal(x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3);
      torAlgDataPrint( R, {c, e, h, m, n, Class, p, q, r} )      
///

doc ///
  Key
    (torAlgDataPrint, Ideal, List)
  Headline
    print invariants of a local ring
  Usage
    torAlgDataPrint (R,L) or torAlgDataPrint(I,L)
  Inputs
    I : Ideal
        of a polynomial algebra contained in the irrelevant maximal ideal
    L : List 
        of keys from the hash table returned by @TO torAlgData@    
  Outputs
      : String
        of keys specified in {\tt L} together with their values
  Description
    Text 
      Extracts data from the hash table returned by @TO
      torAlgData@. 

    Example
      Q = QQ[x,y,z];
      I = ideal(x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3);
      torAlgDataPrint( I, {c, e, h, m, n, Class, p, q, r} )      
///

doc ///
  Key
    setAttemptsAtGenericReduction
  Headline
    control the number of attempts to compute Bass numbers via a generic reduction
  Usage
    setAttemptsAtGenericReduction(R,n)
  Inputs
    R : QuotientRing
        of a polynomial algebra by an ideal contained in the irrelevant maximal ideal
    n : ZZ
        must be non-negative
  Outputs
      : ZZ
        the number of attempts that will be made to perform a generic reduction to compute the Bass 
	numbers of the local ring obtained by localizing {\tt R} at the irrelevant maximal ideal
  Description
  
    Text 
      Changes the number of attempts made to reduce {\tt R} modulo
      a generic regular sequence of generators of the irrelevant
      maximal ideal in order to compute the Bass numbers of the local
      ring obtained by localizing {\tt R} at the irrelevant maximal
      ideal. The function has the effect of setting {\tt
      R.attemptsAtGenericReduction = n}, and the number of attempts
      made is at most {\tt n^2}. The default value is 25, so if {\tt
      R.attemptsAtGenericReduction} is not set, then at most 625
      attempts are made.
      
    Example
      Q = ZZ/2[u,v,w,x,y,z];
      R = Q/ideal(x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3);
      R.?attemptsAtGenericReduction
      setAttemptsAtGenericReduction(R,100)
      R.attemptsAtGenericReduction
      
    Text 
      If the value of {\tt R.attemptsAtGenericReduction} is too small, then the computation of Bass
      numbers may fail resulting in an error message. Notice, though, that if the local ring obtained     
      by localizing {\tt R} at the irrelevant maximal ideal has embedding dimension at most 3, then the 
      Bass numbers are computed without any attempt to reduce the ring, and {\tt R.attemptsAtGenericReduction}
      has no significance.

    Example
      Q = ZZ/2[x,y,z];
      R = Q/ideal(x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3);
      setAttemptsAtGenericReduction(R,0)
      torAlgClass R
///

doc ///
  Key
    attemptsAtGenericReduction
  Headline
    see setAttemptsAtGenericReduction
  Description
  
    Text 
      See @TO setAttemptsAtGenericReduction@

///


--===================================================================================================
-- TESTS
--===================================================================================================

-- #0 zero ring, graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( promote(1,Q) )
assert( torAlgClass(Q/I) === "zero ring" )
assert( torAlgClass(I) === "zero ring" )
///

-- #1 zero ring, local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( x^2-1 )
assert( torAlgClass(Q/I) === "zero ring" )
assert( torAlgClass(I) === "zero ring" )
///

-- #2 C(0), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(u+v+w+x+y+z)
assert( torAlgClass(Q/I) === "C(0)" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 0, 0, 0, 1, "C", 0, 0, 0} )
///

-- #3 C(0), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x-y^2-z^7+u+v+w)
assert( torAlgClass(Q/I) === "C(0)" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert(L === {5, 0, 0, 0, 1, "C", 0, 0, 0} )
///

-- #4 C(1), graded
TEST ///
Q = QQ[x]
I = ideal(x^2)
assert( torAlgClass(Q/I) === "C(1)" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L === {1, 1, 0, 1, 1, "C", 0, 0, 0} )
///

-- #5 C(1), local
TEST ///
Q = QQ[x]
I = ideal(x^2-x^3)
assert( torAlgClass(I) === "C(1)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert(L === {1, 1, 0, 1, 1, "C", 0, 0, 0} )
///

-- #6 C(1), graded
TEST ///
Q = ZZ/53[x,y]
I = ideal(x*y)
assert( torAlgClass(I) === "C(1)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {2, 1, 0, 1, 1, "C", 0, 0, 0} )
///

-- #7 C(1), local
TEST ///
Q = ZZ/53[x,y]
I = ideal(x*y-x^3)
assert( torAlgClass(Q/I) === "C(1)" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert(L === {2, 1, 0, 1, 1, "C", 0, 0, 0} )
///

-- #8 C(1), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x*y,x+y+z,u+v+w)
assert( torAlgClass(Q/I) === "C(1)" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 1, 0, 1, 1, "C", 0, 0, 0} )
///

-- #9 C(1), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2-y^3+z^3)
assert( torAlgClass(I) === "C(1)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 1, 0, 1, 1, "C", 0, 0, 0} )
///

-- #10 C(2), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x*y,u*v,x+y+z+u+v+w)
assert( torAlgClass(Q/I) === "C(2)" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 2, 0, 2, 1, "C", 1, 0, 0} )
///

-- #11 C(2), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x*y,u*v,z^2-w)
assert( torAlgClass(I) === "C(2)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 2, 0, 2, 1, "C", 1, 0, 0} ) 
///

-- #12 S, graded
TEST ///
Q = QQ[x,y]
I = ideal(x^2,x*y)
assert( torAlgClass(Q/I) === "S" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L === {2, 2, 1, 2, 1, "S", 0, 0, 0} )
///

-- #13 S, local
TEST ///
Q = QQ[x,y]
I = ideal((x+y^2)^2,(x+y^2)*y)
assert( torAlgClass(I) === "S" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert(L === {2, 2, 1, 2, 1, "S", 0, 0, 0} )
///

-- #14 S, graded
TEST ///
Q = ZZ/53[u,v,w,x,y,z]
I = ideal(x^2,x*y,y*z,x+y+z+u+v+w)
assert( torAlgClass(I) === "S" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 2, 0, 3, 2, "S", 0, 0, 0} )
///

-- #15 S, local
TEST ///
Q = ZZ/53[u,v,w,x,y,z]
I = ideal(x^2*y-y^2,x^3-x*y)
assert( torAlgClass(Q/I) === "S" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 2, 1, 2, 1, "S", 0, 0, 0} ) 
///

-- #16 B, graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,x*y,z^2,y*z,x+y+z+u+v+w)
assert( torAlgClass(I) === "B" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 3, 1, 4, 1, "B", 1, 1, 2} )
///

-- #17 B, local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^4-z*w^2,y^3-x^2*w,z^3-x*y,w^3-x*y^2*z^2,z^2*x^3-y*w^2,u,v)
assert( torAlgClass(I) === "B" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 3, 0, 5, 2, "B", 1, 1, 2} ) 
///

-- #18 C(3), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal (u*v,w*x,y*z)
assert( torAlgClass(Q/I) === "C(3)" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 3, 0, 3, 1, "C", 3, 1, 3} ) 
///

-- #19 C(3), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(u*v,w*(x+w^2),y*z)
assert( torAlgClass(Q/I) === "C(3)" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 3, 0, 3, 1, "C", 3, 1, 3} )
///

-- #20 G(7) Gorenstein, graded
TEST ///
Q = QQ[x,y,z]
I = ideal(x^3,x^2*z,x*(z^2+x*y),z^3-2*x*y*z,y*(z^2+x*y),y^2*z,y^3)
assert( torAlgClass(I) === "G(7), Gorenstein" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {3, 3, 0, 7, 1, "G", 0, 1, 7} ) 
///

-- #21 G(7) Gorenstein, local
TEST ///
Q = QQ[x,y,z]
I = ideal(x^6,x^4*z,x^2*(z^2+x^2*y),z^3-2*x^2*y*z,y*(z^2+x^2*y),y^2*z,y^3)
assert( torAlgClass(I) === "G(7), Gorenstein" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {3, 3, 0, 7, 1, "G", 0, 1, 7} )
///

-- #22 G(2), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x*y^2,x*y*z,y*z^2,x^4-y^3*z,x*z^3-y^4)
assert( torAlgClass(Q/I) === "G(2)" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 3, 1, 5, 2, "G", 0, 1, 2} ) 
///

-- #23 G(4), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal (x^6,x^2*(z^2+x^2*y),z^3-2*x^2*y*z,y*(z^2+x^2*y),y^2*z,y^3)+x^4*z*ideal(x,y,z)
assert( torAlgClass(Q/I) === "G(4)" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L ===  {6, 3, 0, 7, 2, "G", 0, 1, 4} )
///

-- #24 H(0,0), graded
TEST ///
Q = QQ[x,y,z]
I = ideal(x^2,x*y^2)*ideal(y*z,x*z,z^2)  
assert( torAlgClass(Q/I) === "H(0,0)" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L === {3, 3, 2, 5, 2, "H", 0, 0, 0} ) 
///

-- #25 H(0,0), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,x*(y+v^2)^2)*ideal((y+v^2)*z,x*z,z^2)
assert( torAlgClass(Q/I) === "H(0,0)" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L ===  {6, 3, 2, 5, 2, "H", 0, 0, 0} )
///

-- #26 H(3,2), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*y,x+u+z,y+v+w) 
assert( torAlgClass(I) === "H(3,2)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 3, 0, 4, 2, "H", 3, 2, 2} ) 
///

-- #27 H(3,2), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,(y+w^2)^2,z^2,x*(y+w^2))
assert( torAlgClass(I) === "H(3,2)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L ===  {6, 3, 0, 4, 2, "H", 3, 2, 2} )
///

-- #28 T, graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^3,y^3,z^3,x^2*y*z,x+y+x+u+v+w)
assert( torAlgClass(Q/I) === "T" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L ===  {5, 3, 0, 4, 3, "T", 3, 0, 0} )
///

-- #29 T, local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal((x+u^2)^2,y^2,z^3,(x+u^2)*y*z^2) 
assert( torAlgClass(Q/I) === "T" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 3, 0, 4, 3, "T", 3, 0, 0} ) 
///

-- #30 C(4), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( u^2,v^2,w^2,x^2,y+z )
assert( torAlgClass(I) === "C(4)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 4, 0, 4, 1, "C", 6, 4, 4} ) 
///

-- #31 C(4), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( u^2-v^3,z^6,w^2,x^2,y )
assert( torAlgClass(Q/I) === "C(4)" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 4, 0, 4, 1, "C", 6, 4, 4} ) 
///

-- #32 GT, graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*w,y*w,z*w,u*w^2-x*y*z)
assert( torAlgClass(Q/I) === "GT" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 4, 0, 7, 1, "GT", 3, 3, 7} ) 
///

-- #33 GT, local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*w,y*w,z*w,w^2-x*y*z)
assert( torAlgClass(I) === "GT" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 4, 0, 7, 1, "GT", 3, 3, 7} ) 
///

-- #34, GH(5) graded
TEST ///
Q = QQ[w,x,y,z]
I = ideal fromDual(matrix random(3,Q))
assert( torAlgClass(Q/I) === "GH(5)" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 4, 0, 6, 1, "GH", 5, 6, 6} ) 
///

-- #35 GH(2), local,
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(w^2-x*y*z,x^3,y^3,x^2*z,y^2*z,z^3-x*y*w,x*z*w,y*z*w,z^2*w-x^2*y^2)
assert( torAlgClass(I) === "GH(2)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 4, 0, 9, 1, "GH", 2, 3, 9} ) 
///

-- #36 GS, graded
TEST ///
Q = QQ[w,x,y,z]
I = ideal fromDual(matrix random(2,Q))
assert( torAlgClass(I) === "GS" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 4, 0, 9, 1, "GS", 0, 0, 9} ) 
///

-- #37 codepth 4 Golod, graded
TEST ///
Q = QQ[w,x,y,z]
I = ideal(w^2,x^2,y^2)*ideal(y^2,z^2)
assert( torAlgClass(Q/I) === "codepth 4 Golod" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 4, 2, 6, 1, "Golod", 0, 0, "-"} ) 
///

-- #38 codepth 4 Golod, local
TEST ///
Q = QQ[w,x,y,z]
I = ideal(w^2-y^3,x^2,y^2)*ideal(y^2,z^2)
assert( torAlgClass(I) === "codepth 4 Golod" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 4, 2, 6, 1, "Golod", 0, 0, "-"} ) 
///

-- #39 codepth 4 no class, graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*w,y*w,z*w,w^2)
assert( torAlgClass(I) === "codepth 4 no class" )
assert( torAlgClass(Q/I) === "codepth 4 no class" )
///

-- #40 codepth 4 no class, local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,u+x*w,y*w,z*w,w^2)
assert( torAlgClass(I) === "codepth 4 no class" )
assert( torAlgClass(Q/I) === "codepth 4 no class" )
///

-- #41 C(5), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( u^2,v^2,w^2,x^2,y^2 )
assert( torAlgClass(Q/I) === "C(5)" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 5, 0, 5, 1, "C", 10, 10, 5} ) 
///

-- #42 C(5), local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( u^2-v^3,z^6,w^2,x^2,y^3 )
assert( torAlgClass(Q/I) === "C(5)" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 5, 0, 5, 1, "C", 10, 10, 5} ) 
///

-- #43 codepth 5 Gorenstein, graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*w,y*w,z*w,w^3-x*y*z,v^2)
assert( torAlgClass(Q/I) === "codepth 5 Gorenstein" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 5, 0, 8, 1, "Gorenstein", "UNDETERMINED", "UNDETERMINED", 8} ) 
///

-- #44 codepth 5 Gorenstein, local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*w,y*w,z*w,w^2-x*y*z,v^2)
assert( torAlgClass(I) === "codepth 5 Gorenstein" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 5, 0, 8, 1, "Gorenstein", "UNDETERMINED", "UNDETERMINED", 8} ) 
///

-- #45 codepth 5 Golod, graded
TEST ///
Q = QQ[v,w,x,y,z]
I = ideal(w^2,x^2,y^2)*ideal(y^2,z^2,v^3-x*y*z)
assert( torAlgClass(I) === "codepth 5 Golod" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 5, 2, 9, 1, "Golod", 0, 0, "-"} ) 
///

-- #46 codepth 5 Golod, local
TEST ///
Q = QQ[v,w,x,y,z]
I = ideal(v,w,x,y,z)*ideal(z^2-x*y*v)
assert( torAlgClass(Q/I) === "codepth 5 Golod" )
L = torAlgDataList(I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 5, 4, 5, 1, "Golod", 0, 0, "-"} ) 
///

-- #47 codepth 5 no class, graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*w,y*w,z*w,w^2,v^2)
assert( torAlgClass(I) === "codepth 5 no class" )
assert( torAlgClass(Q/I) === "codepth 5 no class" )
///

-- #48 codepth 5 no class, local
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*w,y*w,z*w,w^2-x*y*z*v,v^2)
assert( torAlgClass(I) === "codepth 5 no class" )
assert( torAlgClass(Q/I) === "codepth 5 no class" )
///

-- #49 codepth 6 no class, graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal (u^2,v^2,w^2,x^2,x*y^15,w*z^4)
assert( torAlgClass(I) === "codepth 6 no class" )
assert( torAlgClass(Q/I) === "codepth 6 no class" )
///

-- #50 C(6), graded
TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal (u^2,v^2,w^2,x^2,y^2,x*y+z^2)
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 6, 0, 6, 1, "C", 15, 20, 6} )
///

-- #51 Codepth 6 Gorenstein, graded
TEST ///
Q = ZZ/53[u,v,w,x,y,z]
I = ideal fromDual(matrix random(3,Q))
L = torAlgDataList(Q/I,{e, c, h, m, n, Class})
assert( L === {6, 6, 0, 15, 1, "Gorenstein"} )
///

-- #52 Codepth 6 Golod, graded
TEST ///
Q = ZZ/53[u,v,w,x,y,z]
I = ideal(x^2*y^2,x^2*z,y^2*z,u^2*z,v^2*z,w^2*z,z^2)
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 6, 4, 7, 1, "Golod", 0, 0, "-"} )
///

end


--==========================================================================
-- end of package code
--==========================================================================

uninstallPackage "TorAlgebra"
restart
installPackage "TorAlgebra"
loadPackage "TorAlgebra"
check "TorAlgebra"
