newPackage ( "CodepthThree",
    Version => "1.0",
    Date => "25 June 2014",
    Authors => {
	{ Name => "Lars Winther Christensen",
	  Email => "lars.w.christensen@ttu.edu",
	  HomePage => "http://www.math.ttu.edu/~lchriste/index.html" },
	{ Name => "Oana Veliche", 
	  Email => "o.veliche@neu.edu",
	  HomePage => "http://www.math.neu.edu/people/profile/oana-veliche/" }
	},
    Headline => "Classification of codepth 3 local rings based on multiplication in homology",
    PackageImports => {"LocalRings"},
    Certification => {
	 "journal name" => "The Journal of Software for Algebra and Geometry",
	 "journal URI" => "http://j-sag.org/",
	 "article title" => "Local rings of embedding codepth 3: A classification algorithm",
	 "acceptance date" => "2014-07-11",
         "published article DOI" => "http://dx.doi.org/10.2140/jsag.2014.6.1",
	 "published article URI" => "http://msp.org/jsag/2014/6-1/jsag-v6-n1-p01-s.pdf",
	 "published code URI" => "http://msp.org/jsag/2014/6-1/jsag-v6-n1-x01-code.zip",
	 "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/CodepthThree.m2",
	 "release at publication" => "4b2e83cd591e7dca954bc0dd9badbb23f61595c0",
	 "version at publication" => "1.0",
	 "volume number" => "6",
	 "volume URI" => "http://msp.org/jsag/2014/6-1/"
	 },
    Reload => false,
    DebuggingMode => false
    )

export { 
    "torAlgData", 
    "torAlgDataPrint", 
    "torAlgDataList", 
    "torAlgClass", 
    "attemptsAtGenericReduction", 
    "setAttemptsAtGenericReduction" }

if version#"VERSION" < "1.4" then error "This package was written for Macaulay2 ver. 1.4 or higher";

-- Workaround because Macaulay2 ver. 1.6 has a minor bug --

if version#"VERSION" == "1.6" then ( complete GradedModule := (M) -> M );



--===================================================================================================
-- EXPORTED FUNCTIONS
--===================================================================================================

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
-- Class: class (B ,C ,G ,H ,S, T) in the classification due to Weyman 
--   and to Avramov Kustin and Miller; see [L.L. Avramov, 
--   A cohomological study of local rings of embedding codepth 3, 
--   J. Pure Appl. Algebra, 216, 2489--2506 (2012)] for an overview
-- p: classification parameter
-- q: classification parameter
-- r: classification parameter
-- PoincareSeries: Poincare series
-- BassSeries: Bass series

torAlgdata = R -> (
    if not R.?attemptsAtGenericReduction then (
	tries := 25
	)
    else (
	tries = R.attemptsAtGenericReduction
	);

    cls:= "UNDEFINED";
    c := "UNDEFINED";
    e := "UNDEFINED";
    h := "UNDEFINED";
    m := "UNDEFINED";
    n := "UNDEFINED";
    p := "UNDEFINED";
    q := "UNDEFINED";
    r := "UNDEFINED";
    Bas := "UNDEFINED";
    Poi := "UNDEFINED";

    T := degreesRing 1;
    T = newRing(T, Degrees => {1});
    t := T_0;

-- Step 1: Classify R if it is the zero ring or defined by the zero ideal.
--         If not, compute c, e, h, l, m(=l+1), n 

    if R == 0 then (
	cls = "zero ring";
	m = 1;
	)
    else (
	if ideal R == 0 then (
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
	    Poi = new Power from {1+t,e};
	    Bas = t^e;
	    )
    	else (
	    R = prune R;
	    Q := ambient R;
	    if ( not isSubset( ideal R, ideal vars Q )) then (
		cls = "zero ring";
		m = 1;
		)
	    else (
		if isHomogeneous R then (
		    I := ideal R;
		    F := res I;
		    )
		else (
		    setMaxIdeal ideal vars Q;		
		    I = ideal localMingens (localResolution ideal R).dd_1;
		    F = localResolution I;
		    R = Q/I;
		    );
		c = length F;
		e = numgens ideal vars R;
		h = c - codim R;
		m = rank F_1;
		n = rank F_c;
		l := m-1;
		poi := new Power from { 1+t, e-1 };
		bas := t^(e-c);
		);
	    );
	);
    
-- Step 2: Classify R if c is not 3
--         If c = 3 classify R if h=0 and n=1, or if h=2
    
    if cls == "UNDEFINED" then (
	if c <= 1 then ( 
	    cls = "C"; 
	    p = 0;
	    q = 0;
	    r = 0;
	    Bas = bas;
	    if c == 0 then (
		Poi = new Power from { 1+t, e } 
		)
	    else (
		Poi = new Divide from { poi, 1-t }

		);
 	    );
	if c == 2 then (
		q = 0;
		r = 0;
	    if h == 0 and n == 1 then (
		cls = "C";
		p = 1;
		Bas = bas;
		Poi = new Divide from { new Power from { 1+t, e-2 }, new Power from { 1-t, 2} };
		)
	    else (
		cls = "S";
		p = 0;
		den := 1 - t - l*t^2;
	    	Poi = new Divide from { poi , den };
		if c == e then (
		    Bas = new Divide from { 1 + t - t^2 , den }
		    )
		else (
		    Bas = ( new Power from { bas, 1 } )*( new Divide from { 1 + t - t^2 , den } );
		    );
		);
	    );
	if c == 3 then (
    	    if h == 0 and n == 1 then (
	    	q = 1;
		r = m;
    		if r == 3 then (
    		    cls = "C";
    		    p = 3;
		    Bas = bas;
		    Poi = new Divide from { new Power from { 1+t, e-3 }, new Power from { 1-t, 3} };
		    )
		else (
    		    cls = "G";
    		    p = 0;
		    Bas = bas;
		    Poi = new Divide from { poi,  1 - t - l*t^2 - t^3 + t^4 };
		    );
		)
	    else (
	    	if h == 2 then (
		    cls = "H";
		    p = 0;
		    q = 0;
		    r = 0;
		    den = 1 - t - l*t^2 - n*t^3;
		    Poi = new Divide from { poi, den };
		    if c == e then (
			Bas = new Divide from { n + l*t + t^2 - t^3, den }
			)
		    else (
		    	Bas = ( new Power from { bas, 1 } ) * new Divide from { n + l*t + t^2 - t^3, den };
			);
		    );
		);
	    );
    	if c >= 4 then (
    	    cls = "codepth > 3";	    
    	    );
    	);
    
-- Step 3: Compute p and q and classify R if p=2, p > 3, or q > 1
    
    if cls == "UNDEFINED" then (
	if isHomogeneous R then (
	    L := res( ideal vars R, LengthLimit => 4 ); 
	    )
	else (
	    setMaxIdeal ideal vars R;
    	    L = localResolution( R^1/(ideal vars R), LengthLimit => 4 ); 
	    );
	b := for i from 0 to 4 list rank L_i;
	p = n + l*b#1 + b#2 - b#3 + binomial(e-1,3);
	q = (n-p)*e + l*b#2 + b#3 - b#4 + binomial(e-1,4);
	den = 1 - t - l*t^2 - (n-p)*t^3 + q*t^4;

	if p == 2 or p > 3 or q > 1 then (
	    cls = "H";
	    r = q;
	    enu := n + (l-r)*t - p*t^2 - t^3 + t^4;
    	    Poi = new Divide from { poi , den };
	    if c == e then (
	    	Bas = new Divide from { enu , den }
		)
	    else (
		Bas = ( new Power from {bas,1} )*( new Divide from { enu , den } )
		);
	    );
	);

-- Step 4: If p <= 1 or p = 3 compute r, if necessary, and classify

    if cls == "UNDEFINED" then (
	if p == 0 then (
	    if q == 0 then (
		cls = "H";
		r = q;
		enu = n + l*t + t^2 - t^3;
		)
	    else (
		if isHomogeneous R then (
		    data := computeBass1 (Q,R,I,e,chainComplex(L.dd_1,L.dd_2),tries)
		    )
		else (
    	    	    data = computeBass2 (Q,R,I,e,chainComplex(L.dd_1,L.dd_2),tries);
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
			enu = n + (l-r)*t - t^3 + t^4;
			)
		    else (
		    	cls="G";
		    	enu = n + (l-r)*t - (r-1)*t^2 - t^3 + t^4;
		    );
		);
	    );
	);     

	if p == 1 then (
	    if q == 0 then (
		cls = "H";
		r = q;
		enu = n + l*t - t^2 - t^3 + t^4;
		)
	    else (
		if isHomogeneous R then (
		    data = computeBass1 (Q,R,I,e,chainComplex(L.dd_1,L.dd_2),tries)
		    )
		else (
    	    	    data = computeBass2 (Q,R,I,e,chainComplex(L.dd_1,L.dd_2),tries);
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
			enu = n + (l-r)*t - t^2 - t^3 + t^4;
			)
		    else (
		    	cls="B";
		    	enu = n + (l-r)*t - t^2 + t^4;
		    );
		);
	    );
	);     

	if p == 3 then (
	    if q == 1 then (
		cls = "H";
		r = q;
		enu = n + (l-1)*t - 3*t^2 - t^3 + t^4;
		)
	    else (
		if isHomogeneous R then (
		    data = computeBass1 (Q,R,I,e,chainComplex(L.dd_1,L.dd_2,L.dd_3),tries)
		    )
		else (
    	    	    data = computeBass2 (Q,R,I,e,chainComplex(L.dd_1,L.dd_2,L.dd_3),tries);
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
			enu = n + l*t - 3*t^2 - t^3 + t^4;
			)
		    else (
		    	cls="T";
			r = q;
			enu = n + l*t - 2*t^2 - t^3 + t^4;
			den = den - t^5;			
		    );
		);
	    );
	);

    Poi = new Divide from { poi , den };
    if c == e then (
	Bas = new Divide from { enu , den }
	)
     else (
	 Bas = ( new Power from {bas,1} )*( new Divide from { enu , den } )
	 );
     );
 
    if cls == "UNDEFINED" then ( 
	error "Internal error: computed values of p,q,r not consistent"
	);

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
	"PoincareSeries" => Poi,
	"BassSeries" => Bas,
	}
    )


torAlgData = ( cacheValue "torAlg" ) torAlgdata


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
-- bcghtClass
--
-- R a quotient of a polynomial algebra
--
-- Returns the (parametrized) class of the ring ring obtained by localizing 
-- R at the irrelevant maximal ideal

torAlgClass = R -> (
    torAlg := torAlgData R;
    cls := torAlg#"Class";
    if cls == "B" or cls == "T" or cls == "S" or cls == "undetermined" 
    or cls == "zero ring" or cls == "codepth > 3" then ( 
    	S := cls;
	);
    if cls == "C" then (
	S = cls|"("|toString torAlg#"c"|")";
	);
    if cls == "G" then (
	S = cls|"("|toString torAlg#"r"|")";
	);    
    if cls == "H" then (
	S = cls|"("|toString torAlg#"p"|","|toString torAlg#"q"|")";
	);
    S )        


----------------------------------------------------------------------------
-- torAlgDataPrint
--
-- R a quotient of a polynomial algebra
-- L a list of keys for the hash table returned by torAlgData
--
-- Returns a string of keys and their values

torAlgDataPrint = (R,L) -> (
    torAlg := torAlgData R;
    fn := temporaryFileName();
    for x in L do (
	fn << toString x << "=" << toString(torAlg#(toString x)) << " "
	);
    fn << endl << close;
    get fn
    )

----------------------------------------------------------------------------
-- torAlgDataList
--
-- R a quotient of a polynomial algebra
-- L a list of keys for the hash table returned by torAlgClass
--
-- Returns a list of the values of the specified keys

torAlgDataList = (R,L) -> (
    torAlg := torAlgData R;
    for x in L list torAlg#(toString x)
    )


--===================================================================================================
-- INTERNAL ROUTINES
--===================================================================================================

----------------------------------------------------------------------------
-- Routines used by torAlgData
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- computeBass1

-- Q is a polynomial algebra
-- I is a homogeneous ideal of Q contained in the irrelevant maximal ideal
-- R is the quotient Q/I
-- e is the embedding dimension of R

-- Returns a hash table with Betti and Bass numbers of the local ring
-- obtained by localizing R at the irrelevant maximal ideal
-- plus data for verification that computations went OK

computeBass1 = (Q,R,I,e,L,tries) -> ( 
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

-- Returns a hash table with Betti and Bass numbers of the local ring
-- obtained by localizing R at the irrelevant maximal ideal
-- plus data for verification that computations went OK

computeBass2 = (Q,R,I,e,L,tries) -> ( 
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



--===================================================================================================
-- DOCUMENTATION
--===================================================================================================

beginDocumentation()

doc ///
  Key
    CodepthThree
  Headline
    Classification of local rings of codepth at most 3 based on multiplication in homology
  Description

    Text 
      Let $I$ be an ideal of a regular local ring $Q$ with residue
      field $k$. The minimal free resolution of $R=Q/I$ carries a
      structure of a differential graded algebra. If the length of the
      resolution, which is called the codepth of $R$, is at most $3$,
      then the induced algebra structure on Tor$_Q*$ ($R,k$) is unique
      and provides for a classification of such local rings.
      
      According to the multiplicative structure on Tor$_Q*$ ($R,k$), a
      non-zero local ring $R$ of codepth at most 3 belongs to exactly one of
      the (parametrized) classes designated {\bf B}, {\bf C}(c), {\bf G}(r), {\bf
      H}(p,q), {\bf S}, or {\bf T}. An overview of the theory can be
      found in L.L. Avramov, {\it A cohomological study of local rings
      of embedding codepth 3}, @HREF"http://arxiv.org/abs/1105.3991"@.
  
      The package implements an algorithm for classification of local
      rings in the sense discussed above; it is described in
      L.W. Christensen and O. Veliche, {\it Local rings of embedding codepth
      3. A classification algorithm}, @HREF"http://arxiv.org/abs/1402.4052"@.  
///


doc ///
  Key
    torAlgData
  Headline
    invariants of a local ring and its class (w.r.t. multiplication in homology)
  Usage
    torAlgData R
  Inputs
    R : QuotientRing
        a quotient of a polynomial algebra  by an ideal contained in the irrelevant maximal ideal
  Outputs
      : HashTable
        a hash table with invariants of the local ring obtained by
  	localizing {\tt R} at the irrelevant maximal ideal
  Description
  
    Text 
      Computes invariants of the local ring obtained by localizing
      {\tt R} at the irrelevant maximal ideal and, provided that it
      has codepth at most 3, classifies it as belonging to one of the
      (parametrized) classes {\bf B}, {\bf C}(c), {\bf G}(r), {\bf
      H}(p,q), {\bf S}, or {\bf T}. Returns a hash table with the
      following data of the local ring:
  
      "c": codepth
      
      "e": embedding dimension
      
      "h": Cohen-Macaulay defect
      
      "m": minimal number of generators of defining ideal
      
      "n": type
      
      "Class": class ('B', 'C', 'G', 'H', 'S', 'T', `zero ring', or 'codepth >3')
      
      "p": classification parameter
      
      "q": classification parameter
      
      "r": classification parameter
      
      "PoincareSeries": Poincar\'e series
      
      "BassSeries": Bass series
      
    Example
      Q = QQ[x,y,z];
      data = torAlgData (Q/ideal (x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3))
      data#"PoincareSeries"
  
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
    torAlgClass
  Headline
    the class (w.r.t. multiplication in homology) of a local ring
  Usage
    torAlgClass R
  Inputs
    R : QuotientRing
        a quotient of a polynomial algebra by an ideal contained in the irrelevant maximal ideal
  Outputs
      : String
        the (parametrized) class of the local ring obtained by
        localizing {\tt R} at the irrelevant maximal ideal, provided
        that this ring is non-zero and of codepth at most 3; otherwise
	a message that it is not
  Description
  
    Text 
      Classifies the local ring obtained by localizing {\tt R} at the
      irrelevant maximal ideal as belonging to one of the
      (parametrized) classes {\bf B}, {\bf C}(c), {\bf G}(r), {\bf
      H}(p,q), {\bf S}, or {\bf T}, provided that it is codepth at
      most 3
      
    Example
      Q = QQ[x,y,z];
      torAlgClass (Q/ideal (x^2,x*y,y*z,z^2))
      torAlgClass (Q/ideal (x^2,y^2))
      torAlgClass (Q/ideal (x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3))
      torAlgClass (Q/ideal (x^2,y^2,z^2,x*y))
      torAlgClass (Q/ideal (x^2,y^2,x*y))
      torAlgClass (Q/ideal (x^2,y^2,z^2,x*y*z))
      
    Text  
      If the local ring has codepth more than 3, then the function returns {\tt "codepth > 3"}.
            
    Example
      Q = QQ[w,x,y,z];
      torAlgClass (Q/ideal (w^4,x^2,y^2,z^2))
      
    Text  
      If the defining ideal of {\tt R} is not contained in the irrelevant maximal ideal, 
      then the resulting local ring is zero, and the function returns {\tt "zero ring"}.
      
    Example
      Q = QQ[x,y,z];
      torAlgClass (Q/ideal (x^2-1))
///

doc ///
  Key
    torAlgDataList
  Headline
    list invariants of a local ring
  Usage
    torAlgDataList(R,L)
  Inputs
    R : QuotientRing
        a quotient of a polynomial algebra  by an ideal contained in the irrelevant maximal ideal
    L : List
        a list of keys from the hash table returned by @TO torAlgData@
	
  Outputs
      : List
        the list of values corresponding to the keys specified in {\tt L}
	
  Description
    Text 
      Extracts data from the hash table returned by @TO torAlgData@.

    Example
      Q = QQ[x,y,z];
      R = Q/ideal (x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3);
      torAlgDataList( R, {m, n, Class, p, q, r, PoincareSeries, BassSeries} )            
///      
      
doc ///
  Key
    torAlgDataPrint
  Headline
    print invariants of a local ring
  Usage
    torAlgDataPrint (R,L)
  Inputs
    R : QuotientRing
        a quotient of a polynomial algebra  by an ideal contained in the irrelevant maximal ideal
    L : List 
        a list of keys from the hash table returned by @TO torAlgData@    
  Outputs
      : String
        the string of keys specified in {\tt L} together with their values.
  Description
    Text 
       Extracts data from the hash table returned by @TO torAlgData@.

    Example
      Q = QQ[x,y,z];
      R = Q/ideal (x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3);
      torAlgDataPrint( R, {c, e, h, m, n, Class, p, q, r} )      
     
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
        a quotient of a polynomial algebra by an ideal contained in the irrelevant maximal ideal
    n : ZZ
        a positive integer
  Outputs
      : ZZ
        the number of attempts that will be made to perform a generic reduction to compute the Bass 
	numbers of the local ring obtained by localizing {\tt R} at the irrelevant maximal ideal.
  Description
  
    Text 
      Changes the number of attempts made to reduce {\tt R} modulo a generic regular sequence of
      generators of the irrelevant maximal ideal in order to compute the Bass numbers of the
      local ring obtained by localizing {\tt R} at the irrelevant maximal ideal. The function has
      the effect of setting {\tt R.attemptsAtGenericReduction = n}, and the number of attempts made is 
      at most {\tt n^2}. If {\tt R.attemptsAtGenericReduction} is not set, then at most 625 attempts
      are made.
      
    Example
      Q = ZZ/2[u,v,w,x,y,z];
      R = Q/ideal (x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3);
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
      R = Q/ideal (x*y,y*z,x^3,x^2*z,x*z^2-y^3,z^3);
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

TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( promote(1,Q) )
assert( torAlgClass(Q/I) === "zero ring" )
///

TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( x^2-1 )
assert( torAlgClass(Q/I) === "zero ring" )
///

TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( u^2,v^2,w^2,x^2,y,z )
assert( torAlgClass(Q/I) === "codepth > 3" )
///

TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal( u^2-v^3,z^6,w^2,x^2,y )
assert( torAlgClass(Q/I) === "codepth > 3" )
///

TEST ///
Q = QQ[x]
I = ideal(x^2)
assert( torAlgClass(Q/I) === "C(1)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {1, 1, 0, 1, 1, "C", 0, 0, 0} )
///

TEST ///
Q = QQ[x]
I = ideal(x^2-x^3)
assert( torAlgClass(Q/I) === "C(1)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert(L === {1, 1, 0, 1, 1, "C", 0, 0, 0} )
///

TEST ///
Q = QQ[x,y]
I = ideal(x*y)
assert( torAlgClass(Q/I) === "C(1)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {2, 1, 0, 1, 1, "C", 0, 0, 0} )
///

TEST ///
Q = QQ[x,y]
I = ideal(x*y-x^3)
assert( torAlgClass(Q/I) === "C(1)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert(L === {2, 1, 0, 1, 1, "C", 0, 0, 0} )
///

TEST ///
Q = QQ[x,y]
I = ideal(x^2,x*y)
assert( torAlgClass(Q/I) === "S" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {2, 2, 1, 2, 1, "S", 0, 0, 0} )
///

TEST ///
Q = QQ[x,y]
I = ideal((x+y^2)^2,(x+y^2)*y)
assert( torAlgClass(Q/I) === "S" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert(L === {2, 2, 1, 2, 1, "S", 0, 0, 0} )
///

TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(u+v+w+x+y+z)
assert( torAlgClass(Q/I) === "C(0)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 0, 0, 0, 1, "C", 0, 0, 0} )
///

TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x-y^2-z^7+u+v+w)
assert( torAlgClass(Q/I) === "C(0)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert(L === {5, 0, 0, 0, 1, "C", 0, 0, 0} )
///

TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x*y,x+y+z,u+v+w)
assert( torAlgClass(Q/I) === "C(1)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 1, 0, 1, 1, "C", 0, 0, 0} )
///

TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2-y^3+z^3)
assert( torAlgClass(Q/I) === "C(1)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 1, 0, 1, 1, "C", 0, 0, 0} )
///

TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x*y,u*v,x+y+z+u+v+w)
assert( torAlgClass(Q/I) === "C(2)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 2, 0, 2, 1, "C", 1, 0, 0} )
///

TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x*y,u*v,z^2-w)
assert( torAlgClass(Q/I) === "C(2)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 2, 0, 2, 1, "C", 1, 0, 0} ) 
///

TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,x*y,y*z,x+y+z+u+v+w)
assert( torAlgClass(Q/I) === "S" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 2, 0, 3, 2, "S", 0, 0, 0} )
///

TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2*y-y^2,x^3-x*y)
assert( torAlgClass(Q/I) === "S" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 2, 1, 2, 1, "S", 0, 0, 0} ) 
///

TEST ///
setRandomSeed "codepthThree";
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,x*y,z^2,y*z,x+y+z+u+v+w)
assert( torAlgClass(Q/I) === "B" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {5, 3, 1, 4, 1, "B", 1, 1, 2} )
///

TEST ///
setRandomSeed "codepthThree";
Q = QQ[u,v,w,x,y,z]
I = ideal(x^4-z*w^2,y^3-x^2*w,z^3-x*y,w^3-x*y^2*z^2,z^2*x^3-y*w^2,u,v)
assert( torAlgClass(Q/I) === "B" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 3, 0, 5, 2, "B", 1, 1, 2} ) 
///

TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal (u*v,w*x,y*z)
assert( torAlgClass(Q/I) === "C(3)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 3, 0, 3, 1, "C", 3, 1, 3} ) 
///

TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(u*v,w*(x+w^2),y*z)
assert( torAlgClass(Q/I) === "C(3)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 3, 0, 3, 1, "C", 3, 1, 3} )
///

TEST ///
Q = QQ[x,y,z]
I = ideal(x^3,x^2*z,x*(z^2+x*y),z^3-2*x*y*z,y*(z^2+x*y),y^2*z,y^3)
assert( torAlgClass(Q/I) === "G(7)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {3, 3, 0, 7, 1, "G", 0, 1, 7} ) 
///

TEST ///
Q = QQ[x,y,z]
I = ideal(x^6,x^4*z,x^2*(z^2+x^2*y),z^3-2*x^2*y*z,y*(z^2+x^2*y),y^2*z,y^3)
assert( torAlgClass(Q/I) === "G(7)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {3, 3, 0, 7, 1, "G", 0, 1, 7} )
///

TEST ///
setRandomSeed "codepthThree";
Q = QQ[u,v,w,x,y,z]
I = ideal(x*y^2,x*y*z,y*z^2,x^4-y^3*z,x*z^3-y^4)
assert( torAlgClass(Q/I) === "G(2)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 3, 1, 5, 2, "G", 0, 1, 2} ) 
///

TEST ///
setRandomSeed "codepthThree";
Q = QQ[u,v,w,x,y,z]
I = ideal (x^6,x^2*(z^2+x^2*y),z^3-2*x^2*y*z,y*(z^2+x^2*y),y^2*z,y^3)+x^4*z*ideal(x,y,z)
assert( torAlgClass(Q/I) === "G(4)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L ===  {6, 3, 0, 7, 2, "G", 0, 1, 4} )
///

TEST ///
Q = QQ[x,y,z]
I = ideal(x^2,x*y^2)*ideal(y*z,x*z,z^2)  
assert( torAlgClass(Q/I) === "H(0,0)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {3, 3, 2, 5, 2, "H", 0, 0, 0} ) 
///

TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,x*(y+v^2)^2)*ideal((y+v^2)*z,x*z,z^2)
assert( torAlgClass(Q/I) === "H(0,0)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L ===  {6, 3, 2, 5, 2, "H", 0, 0, 0} )
///

TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,y^2,z^2,x*y,x+u+z,y+v+w) 
assert( torAlgClass(Q/I) === "H(3,2)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {4, 3, 0, 4, 2, "H", 3, 2, 2} ) 
///

TEST ///
Q = QQ[u,v,w,x,y,z]
I = ideal(x^2,(y+w^2)^2,z^2,x*(y+w^2))
assert( torAlgClass(Q/I) === "H(3,2)" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L ===  {6, 3, 0, 4, 2, "H", 3, 2, 2} )
///

TEST ///
setRandomSeed "codepthThree";
Q = QQ[u,v,w,x,y,z]
I = ideal(x^3,y^3,z^3,x^2*y*z,x+y+x+u+v+w)
assert( torAlgClass(Q/I) === "T" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L ===  {5, 3, 0, 4, 3, "T", 3, 0, 0} )
///

TEST ///
setRandomSeed "codepthThree";
Q = QQ[u,v,w,x,y,z]
I = ideal((x+u^2)^2,y^2,z^3,(x+u^2)*y*z^2) 
assert( torAlgClass(Q/I) === "T" )
L = torAlgDataList(Q/I,{e, c, h, m, n, Class, p, q, r})
assert( L === {6, 3, 0, 4, 3, "T", 3, 0, 0} ) 
///

end

--===================================================================================================
-- end of package code
--===================================================================================================

uninstallPackage "CodepthThree"
restart
installPackage "CodepthThree"
check "CodepthThree"

