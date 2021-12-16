--RealRoots.m2
newPackage(
    "RealRoots",
    Version=>"0.1",
    Date=>"Oct 9, 2020",
    Authors=>{
     	{Name=>"Jordy Lopez",
	 Email=>"jordy.lopez@tamu.edu",
	 HomePage=>""},
    	{Name=>"Kelly Maluccio",
	 Email=>"kmaluccio@tamu.edu",
	 HomePage=>"https://www.math.tamu.edu/~kmaluccio"},
    	{Name=>"Frank Sottile",
	 Email=>"sottile@tamu.edu",
	 HomePage=>"https://www.math.tamu.edu/~frank.sottile/"},
	{Name=>"Thomas Yahl",
	 Email=>"Thomasjyahl@tamu.edu",
	 HomePage=>"https://math.tamu.edu/~thomasjyahl"}
	},
    Headline=>"Package for symbolically exploring, counting, and locating real solutions to polynomial systems",
    PackageImports=>{},
    PackageExports=>{},
    DebuggingMode=>true
    )


export{
    --methods
    "eliminant",
    "regularRep",
    "charPoly",
    "variations",
    "SylvesterSequence",
    "numSylvester",
    "SturmSequence",
    "numSturm",
    "realRootIsolation",
    "BudanFourierBound",
    "traceForm",
    "numTrace",
    --options
    "Multiplicity"
    }


----------------------------
--METHODS FOR INTERNAL USE--
----------------------------

--Check that a ring is a univariate polynomial ring over a field of characteristic zero
----worry about more interesting fields?
----make warning optional?
isUnivariate = method()
isUnivariate (Ring) := Boolean => R->(
    K := coefficientRing R;
    if instance(K,InexactField) then print "Warning: Computations over inexact field";
    (isPolynomialRing R) and (numgens R === 1) and (isField K) and (char K === 0)
    )


--Check that a ring is Artinian over a field of characteristic zero
----check if warning is printed more than once
isArtinian = method()
isArtinian (Ring) := Boolean => R->(
    K := coefficientRing R;
    if instance(K,InexactField) then print "Warning: Computations over inexact field";
    (isField K) and (char K===0) and (dim R===0)
    )


--Computes the sign of a real number
sign = method()
sign (Number) := ZZ => n ->(
     if n < 0 then -1 
     else if n == 0 then 0
     else if n > 0 then 1
     )


--Computes the sign of a real univariate polynomial at a given real number
signAt = method()
signAt (RingElement,Number) := ZZ => (f,r)->(
    sign(substitute(f,(ring f)_0=>r))
    )

signAt (RingElement,InfiniteNumber) := ZZ => (f,r)->(
    if (r>0) then (
	sign(leadCoefficient f)
	) else (
	sign((if odd first degree f then -1 else 1)*(leadCoefficient f))
	)
    )


--Computes the sequence of derivatives of f
derivSequence = method()
derivSequence (RingElement) := List => f->(
    R := ring f;
    if not isUnivariate(R) then error "Error: Expected univariate polynomial";
    if (f == 0) then error "Error: Expected nonzero polynomial";
    
    t := R_0;
    d := first degree f;
    apply(d+1, i -> diff(t^i,f))
    )

--------------------
--EXPORTED METHODS--
--------------------

--Compute the eliminant of 'f' in the quotient ideal defined by 'I'
----better naming for strategies?
----fix second option to use minimal polynomial/give better error
eliminant = method(Options=>{Strategy=>0})
eliminant (RingElement,Ideal) := RingElement => opts-> (f,I)->(
    R := ring f;
    if not (ring I === R) then error "Error: Expected polynomial and ideal of the same ring";
    eliminant(sub(f,R/I))
    )
    
eliminant (RingElement) := RingElement => opts-> f->(
    R := ring f;
    if not isArtinian(R) then error "Error: Expected element of Artinian ring";
    
    if (opts.Strategy === 1) then (
	--This strategy computes the eliminant as the kernel of the multiplication map
	K := coefficientRing R;
    	Z := getSymbol "Z";
    	S := K(monoid [Z]);
    	phi := map(R,S,{f});
    	(ker phi)_0
        
	) else if (opts.Strategy === 2) then (
	--This strategy computes the eliminant as a characteristic polynomial when possible
	n := numgens source basis R;
    	g := charPoly(f);
    	if (first degree g === n) then g else error "Error: Eliminant not computed"
    	
	) else (
	--This strategy computes the eliminant by finding a minimal linear combination in powers of f
    	B := basis R;
    	n = numgens source B;
    	K = coefficientRing R;
	
    	Z = getSymbol "Z";
    	S = K(monoid [Z]);
    	
    	P := map(R^1,R^(n+1),(i,j)->f^j);
    	M := last coefficients(P, Monomials=>B);
    	coeffs := sub(gens ker M,K);
    	(map(S^1,S^(n+1),(i,j)->S_0^j) * coeffs)_(0,0)
	) 
    )


--Computes a matrix representation of the multiplication map determined by f
regularRep = method()
regularRep (RingElement,Ideal) := Matrix => (f,I)->(
    R := ring f;
    if not (ring I === R) then error "Error: Expected polynomial ring and ideal of the same ring";
    regularRep(sub(f,R/I))
    )

regularRep (RingElement) := Matrix => f->(
    R := ring f;
    if not isArtinian(R) then error "Error: Expected element of Artinian ring";
    K := coefficientRing R;
    
    B := basis R;
    C := last coefficients(f*B,Monomials=>B);
    (B,sub(C,K))
    )


--Computes the characteristic polynomial of a matrix
charPoly = method()
charPoly (Matrix) := RingElement => M->(
    n := numgens source M;
    if not (numgens target M === n) then error "Error: Expected a square matrix";
    
    K := ring M;
    if not (isField K and char K === 0) then error "Error: Expected a field of characteristic zero";
    
    Z := getSymbol "Z";
    S := K(monoid [Z]);

    IdZ := S_0*id_(S^n);
    det(IdZ - M)
    )

--Computes the characteristic polynomial of the regular representation of f
charPoly (RingElement,Ideal) := RingElement => (f,I)->(
    R := ring f;
    if not (ring I === R) then error "Error: Expected polynomial ring and ideal of the same ring";
    charPoly(sub(f,R/I))
    )

charPoly (RingElement) := RingElement => f->(
    (B,mf) := regularRep(f);
    charPoly(mf)
    )


--Computes the number of sign changes in a list of real numbers
variations = method()
variations (List) := ZZ => l->(
    n := 0;
    last := 0;
    scan(l, x -> if x =!=0 then (
	    if last < 0 and x > 0 or last > 0 and x < 0 then n = n+1;
	    last = x;
	    )
	);
    n
    )


--Computes the difference in variations of the derivative sequence at specified values
BudanFourierBound = method()
for A in {Number,InfiniteNumber} do 
for B in {Number,InfiniteNumber} do
BudanFourierBound (RingElement,A,B) := ZZ => (f,a,b)->(
    if not (a<b) then error "Error: Expected non-empty interval";
    l := derivSequence f;
    variations apply(l,g->signAt(g,a)) - variations apply(l,g->signAt(g,b))
    )

BudanFourierBound (RingElement) := ZZ => f->( 
    BudanFourierBound(f,-infinity,infinity)
    )


--Computes the Sylvester sequence of a pair (f,g)
----This isn't actually the Sylvester sequence since we divide by gcd(f,g)
SylvesterSequence = method()
SylvesterSequence (RingElement, RingElement) := List => (f,g)->(
    R := ring f;
    if not (ring g === R) then error "Error: Polynomials should be in the same ring";
    if not isUnivariate(R) then error "Error: Expected univariate polynomials";
    
    --dividing out common factors
    h := gcd(f,g);
    f = sub(f/h,R);
    g = sub(g/h,R);
        
    --'d' is a bound for the length of the Sylvester sequence:
    m := if f == 0 then 0 else first degree f;
    n := if g == 0 then 0 else first degree g;
    d := 2 + min {m,n};
    
    Syl := new MutableList from toList(0..d);
    Syl#0 = f;
    Syl#1 = g;
    scan(2..d, i -> Syl#i = -Syl#(i-2) % Syl#(i-1));
    	    
    toList Syl
    )


--Computes the difference in the number of roots of f where g is positive and where g is negative
----letting g = 1 gives the number of real roots from the Sturm sequence
numSylvester = method()
for A in {Number,InfiniteNumber} do 
for B in {Number,InfiniteNumber} do
numSylvester (RingElement,RingElement,A,B) := ZZ => (f, g, a, b)->(
    if not (a<b) then error "Error: Expected non-empty interval";
    l := SylvesterSequence(f,diff((ring f)_0,f)*g);
    variations apply(l,h->signAt(h,a)) - variations apply(l,h->signAt(h,b))
    )

numSylvester (RingElement,RingElement) := ZZ => (f,g)->(
    numSylvester(f,g,-infinity,infinity)
    )


--Computes the Sturm sequence of f via a Sylvester sequence
SturmSequence = method()
SturmSequence (RingElement) := List => f->(
    if (f == 0) then error "Error: Expected nonzero polynomial";
    R := ring f;
    SylvesterSequence(f,diff(R_0,f))
    )


--Computes the difference in variations of the Sturm sequence at specified values
numSturm = method(Options=>{Multiplicity=>false})
for A in {Number,InfiniteNumber} do
for B in {Number,InfiniteNumber} do
numSturm (RingElement,A,B) := ZZ => opts->(f,a,b)->(
    R := ring f;
    h := gcd(f,diff(R_0,f));
    f = sub(f/h,R);
    n := numSylvester(f,1_R,a,b);
    if (opts.Multiplicity==true) then (
	while(first degree h > 0) do (
	    n = n + numSturm(h,a,b,opts);
	    h = gcd(h,diff(R_0,h));
	    )
	);
    n
    )

numSturm (RingElement) := ZZ => opts-> f->( 
    numSturm(f,-infinity,infinity,opts)
    )


--Uses Sturm sequence and a bisection method to isolate real solutions to a real univariate polynomial within a tolerance
realRootIsolation = method()
realRootIsolation (RingElement,RR) := List => (f,eps)->(
    R := ring f;
    if not isUnivariate(R) then error "Error: Expected univariate polynomial";
    
    f = sub(f/gcd(f,diff(R_0,f)),R);
    
    if (numSturm(f)>0) then (
	l := SturmSequence(f);
	
	--bound for real roots
    	C := flatten entries sub(last coefficients f, coefficientRing R);
    	M := (sum(C,abs))/(leadCoefficient f);
	
	L := {{-M,M}};
	midp := 0;
	v := new MutableHashTable from {M=>variations apply(l,g->signAt(g,M)),-M=>variations apply(l,g->signAt(g,-M))};
	
	while (max apply(L,I-> I#1-I#0) > eps) or (max apply(L,I-> v#(I#0)-v#(I#1)) > 1) do (
	    for I in L do (
		midp = (sum I)/2;
		v#midp = variations apply(l,g->signAt(g,midp));
		L = drop(L,1);
		if (v#(I#0)-v#midp > 0) then (
		    L = append(L,{I#0,midp})
		    );
		if (v#midp-v#(I#1) > 0) then (
		    L = append(L,{midp,I#1})
		    )
		)
	    );
	L
	) else (
	{}
	)
    )
        
    
    
--Computes the trace form of f in an Artinian ring 
----change names? mm? tr?
traceForm = method()
traceForm (RingElement,Ideal) := Matrix => (f,I)->(
    R := ring f;
    traceForm(sub(f,R/I))
    )

traceForm (RingElement) := Matrix => f->(
    R := ring f;
    if not isArtinian R then error "Error: Expected Artinian ring";    
    B := basis R;
    K := coefficientRing R;

    mm := sub(last coefficients(f*B**B,Monomials=>B),K);
    tr := matrix {apply(first entries B, x -> trace last regularRep x)};
    adjoint(tr*mm, source tr, source tr)
    )


--Compute the number of real points of a scheme/real univariate polynomial/real polynomial system using the trace form.
--Use numSylvester for this
numTrace = method()
numTrace (RingElement) := ZZ => f->(
    R := ring f;
    numTrace(R/f)
    )

numTrace (List) := ZZ => F->(
    I := ideal F;
    R := ring I;
    numTrace(R/I)
    )

numTrace (Ideal) := ZZ=> I->(
    R := ring I;
    numTrace(R/I)
    )

numTrace (QuotientRing) := ZZ=> R->(
    if not isArtinian R then error "Expected Artinian ring";
    K := coefficientRing R;
    
    ch := charPoly(traceForm(1_R));
    chNeg := sub(ch,(ring ch)_0=>-(ring ch)_0);
    numSturm(ch,0,infinity,Multiplicity=>true) - numSturm(chNeg,0,infinity,Multiplicity=>true)
    )


--------------------
---DOCUMENTATION----
--------------------

beginDocumentation()
document {
	Key => RealRoots,
	Headline =>"Package for exploring counting and locating real solutions to polynomial systems",
	"The purpose of this package is to provide tools for elimination and solving, with a particular emphasis
	on counting and isolating real zeros of ideals in QQ[X].",
	}

document {
	Key => {(eliminant, RingElement),(eliminant,RingElement,Ideal),eliminant},
	Usage => "eliminant(f)",
	Inputs => {"f"},
	Outputs => { RingElement => { "the eliminant of", TT "f", "with respect to the polynomial ring in one variable", TT "Z"}},
	PARA {"This computes the eliminant of an element ", TT "f", " of an Artinian ring ", TT "R", " and returns a polynomial in ",TT "Z"},
	EXAMPLE lines ///
	    	R = QQ[x,y]
		F = {y^2-x^2-1,x-y^2+4*y-2}
		I = ideal F
		S = R/I
		eliminant(x)
	       	eliminant(y)	      
	 	 ///,
     	}

document {
	Key => {(regularRep, RingElement, Ideal), (regularRep, RingElement), regularRep},
	Usage => "regularRep(f,I)",
	Inputs => {"f", "I"},
	Outputs => { Matrix => { "the matrix of the linear map defined by multiplication by", TT "f", "in terms of the standard basis of a finite-dimensional k-vector space", TT "I" }},
	PARA {"This command gives the matrix of the linear map defined by multiplication by ", TT "f", " in terms of the standard basis of a finite-dimensional k-vector space ", TT "I" },
	EXAMPLE lines ///
		 R = QQ[x,y]
		 F = {y^2-x^2-1,x-y^2+4*y-2}
		 I = ideal F
		 regularRep(y,I)
		 S = R/I
		 regularRep(y)
	 	 ///,
     	}

document {
	Key => {(charPoly, Matrix),charPoly},
	Usage => "charPoly(M)",
	Inputs => {"M"},
	Outputs => { RingElement => { "the characteristic polynomial of", TT "M"}},
	PARA {"This computes the characteristic polynomial of ", TT "M"},
	EXAMPLE lines ///
	         R = QQ[x,y]
		 F = {y^2-x^2-1,x-y^2+4*y-2}
		 I = ideal F
		 S = R/I
		 M = last regularRep(y)
		 charPoly(M)
	 	 ///,
     	}

 document {
	Key => {(SylvesterSequence, RingElement, RingElement),SylvesterSequence},
	Usage => "SylvesterSequence(f,g)",
	Inputs => {"f","g"},
	Outputs => { List => { "the Sylvester sequence of ", TT "f", " and ",TT "g"}},
	PARA {"This computes the Sylvester sequence of two univariate polynomials ", TT "f", " and ", TT "g", " in the same ring"},
	EXAMPLE lines ///
	         R = QQ[t]
		 f = (t+1)*(t+2)
		 g = (t+2)
		 SylvesterSequence(f,g)
	 	 ///,
	SeeAlso => {"numSylvester"}
     	}
    
document {
	Key => {(numSylvester, RingElement, RingElement, Number,Number),(numSylvester, RingElement, RingElement, InfiniteNumber,InfiniteNumber),(numSylvester, RingElement, RingElement, InfiniteNumber,Number),(numSylvester, RingElement, RingElement, Number,InfiniteNumber),numSylvester},
	Usage => "numSylvester(f,g,a,b)",
	Inputs => {"f","g","a","b"},
	Outputs => { ZZ => {"the difference between number of roots of ",TT "f"," when ",TT "g",
		"is positive and when g is negative"}},
	PARA {"This computes the difference in variations of the Sylvester sequence of ", TT "f"," and ",TT "f'g"," at the values", TT "a"," and ", TT "b"},
	EXAMPLE lines ///
	    	 R = QQ[t]
		 f = (t-2)*(t-1)*(t+3)
		 g = t+1
		 a = -5
		 b = 4
		 numSylvester(f,g,a,b)
	 	 ///,
	SeeAlso => {"SylvesterSequence"}
     	}

document {
	Key => {(SturmSequence, RingElement),SturmSequence},
	Usage => "SturmSequence(f)",
	Inputs => {"f"},
	Outputs => { List => { "the Sturm sequence of", TT "f"}},
	PARA {"This computes the Sturm Sequence of a univariate polynomial ", TT "f"},
	EXAMPLE lines ///
	 	 R = QQ[t]
		 f = 45 - 39*t - 34*t^2+38*t^3-11*t^4+t^5
		 roots f
		 SturmSequence(f)
	 	 ///,
	SeeAlso => {"numSturm"}
     	}

document {
    	Key =>{"Multiplicity(RealRoots)", [numSturm, Multiplicity]},
	PARA {"This is an optional input for counting roots with multiplicity."}
    }


document {
	Key => {(numSturm, RingElement, Number,Number), (numSturm,RingElement,Number,InfiniteNumber), (numSturm, RingElement,InfiniteNumber,Number), (numSturm, RingElement,InfiniteNumber,InfiniteNumber),numSturm},
	Usage => "numSturm(f,a,b)",
	Inputs => {"f, a univariate polynomial", "a, a lower bound of the interval", "b, an upper bound of the interval"},
	Outputs => { ZZ => { "the number of real roots of a univariate polynomial ", TT "f"," not counting multiplicity in the interval ", TT "(a,b]"}},
	PARA {"This computes the difference in variation of the Sturm sequence of ", TT "f", ". If ", TT "a", " and ", TT "b"," are not specified, the interval will be taken from negative infinity to infinity."},
	EXAMPLE lines ///
	    	 R = QQ[t]
		 f = (t-5)*(t-3)^2*(t-1)*(t+1)
		 roots f
		 numSturm(f)
		 numSturm(f,0,5)
		 numSturm(f,-2,2)
		 numSturm(f,-1,5)
	 	 ///,
	PARA {"In the above example, multiplicity is not included so to include this we can make the multiplicity option true in the below example."},
	EXAMPLE lines ///
		numSturm(f,Multiplicity=>true)
		numSturm(f,0,5,Multiplicity=>true)
		numSturm(f,0,3,Multiplicity=>true)
		///,
	PARA {"If ", TT "a"," is an ", TT "InfiniteNumber", ", then the lower bound will be negative infinity and if ", TT "b"," is an ", TT "InfiniteNumber", ", then the upper bound is infinity."},
	EXAMPLE lines ///
	    	numSturm(f,-infinity, 0)
		numSturm(f,0,infinity)
		numSturm(f,-infinity,infinity)
		///,
	SeeAlso => {"SturmSequence"}
     	}
    
document {
    	Key => {(variations, List),variations},
	Usage => "variations(l)",
	Inputs => {"l"},
	Outputs => { ZZ => { "the number of sign changes in a sequence ", TT "l" }},
	PARA {"This computes the number of changes of sign in a sequence ", TT "l"},
	EXAMPLE lines ///
		 L = for i to 10 list random(-50,50)
		 variations(L)
	 	 ///,
     	}
    
document {
        Key => {(realRootIsolation, RingElement,RR),realRootIsolation},
	Usage => "realRootIsolation(f,eps)",
	Inputs => {"f", "eps"},
	Outputs => { List => { "the number of real roots of a univariate polynomial", TT "f"," not counting multiplicity"}},
	PARA {"This method uses a Sturm sequence and a bisection method to isolate real solutions of a polynomial", TT "f"," to a real univariate polynomial and it lists an interval for which each real solution is located"},
	EXAMPLE lines ///
	    	 R = QQ[t]
		 f = 45 - 39*t - 34*t^2+38*t^3-11*t^4+t^5
		 realRootIsolation(f,0.5)
	 	 ///,
	SeeAlso => {"SturmSequence"}
     	}
    
document {
	Key => {(BudanFourierBound, RingElement,Number,Number), (BudanFourierBound, RingElement, Number, InfiniteNumber), (BudanFourierBound, RingElement, InfiniteNumber, Number),(BudanFourierBound, RingElement, InfiniteNumber, InfiniteNumber),BudanFourierBound}, --maybe we can call it BFbound (Budan-Fourier bound?)
	Usage => "BudanFourierBound(f, a, b)",
	Inputs => {"f, a univariate polynomial", "a, a lower bound of the interval", "b, an upper bound of the interval"},
	Outputs => { ZZ => { "a sharp upper  bound for the number of real roots of a univariate polynomial", TT "f", " in the interval ", TT "(a,b)"}},
	PARA {"This computes a sharp upper bound for the number of real roots of a univariate polynomial ", TT "f", " from ", TT "a", " to ", TT "b", ". If interval is not specified, it computes a bound for the real roots of the function from negative infinity to infinity."},
	EXAMPLE lines ///
	         R = QQ[t]
		 f = 45 - 39*t - 34*t^2+38*t^3-11*t^4+t^5
		 BudanFourierBound(f)
		 g = (t-4)*(t-1)^2*(t+1)*(t+3)*(t+5)*(t-6)
		 a = -6
		 BudanFourierBound(g,a,infinity)
		 BudanFourierBound(g,-1,5)
	 	 ///,
     	}
    
document {
	Key => {(traceForm, RingElement),traceForm},
	Usage => "traceForm(f)",
	Inputs => {"f"},
	Outputs => { Matrix => { "the trace quadratic form of", TT "f" }},
	PARA {"This computes the trace quadratic form of an element ", TT "f", " in an Artinian ring"},
	EXAMPLE lines ///
	         R = QQ[x,y]
		 F = {y^2 - x^2 - 1, x-y^2+4*y-2}
		 I = ideal F
		 S = R/I
		 f = y^2 - x^2 - x*y + 4
		 traceForm(f)
	 	 ///,
	SeeAlso => {"numTrace"}
     	}
    

document {
	Key => {(numTrace, QuotientRing), (numTrace, RingElement), (numTrace, List),(numTrace,Ideal), numTrace},
	Usage => "numRealTrace(R)",
	Inputs => {"R"},
	Outputs => { ZZ => { "the number of real points of Spec", TT "R" }},
	PARA {"This computes the number of real points of Spec", TT "R", " where ", TT "R", " is an Artinian ring with characteristic zero"},
	EXAMPLE lines ///
	         R = QQ[x,y]
		 F = {y^2-x^2-1,x-y^2+4*y-2}
		 I = ideal F
		 S = R/I
		 numTrace(S)
		 ///,
	EXAMPLE lines ///
		 R = QQ[x,y]
		 I = ideal(1 - x^2*y + 2*x*y^2, y - 2*x - x*y + x^2)
		 numTrace(I)
	 	 ///,
	SeeAlso => {"traceForm"}
     	}

TEST ///
    R = QQ[x,y];
    F = {y^2-x^2-1,x-y^2+4*y-2};
    I = ideal F;
    S = R/I;
    a = eliminant(x);
    T = ring a;
    assert(flatten entries last coefficients(eliminant(x)) == {1,-2,-9,-6,-7});
    assert(flatten entries last regularRep(y) == {0, 0, -3, -2, 0, 0, -1, 1, 0, 1, 4, 0, 1, 0, 4, 4});
    M = last regularRep(y);
    pol = charPoly(M);
    G = ring pol;
    ans = Z^4 - 8*Z^3 + 19*Z^2 - 16*Z + 5;
    assert(pol == ans); 
    ///

TEST ///
    c1 = {4, 5, -1, -11, 13, -9, 8};
    c2 = {9, 0, 1, 0, -1, -2, 11, 0, 14};
    assert(variations(c1) == 4);
    assert(variations(c2) == 2);
    ///

TEST ///
    R = QQ[t];
    f = (t-1)*(t+1);
    g = (t+1);
    assert(SylvesterSequence(f,g) == {t-1, 1, 0});
    assert(SturmSequence(f) == {t^2-1, 2*t, 1, 0});
    ///

TEST ///
    R = QQ[t];
    f = (t-4)*(t-1)^2*(t+1)*(t+3)*(t+5)*(t-6);--roots at 6, 4, 1 (mult 2), -1, -3, -5
    g = (2*t-1)*(3*t+8)*(t-9)*(6*t+1);--rational roots at -8/3, -1/6, 1/2, 9
    p = (t-5)*(t-2)*(t+3)*(t+4)*(t-8)*(t+13);--roots at -13, -4, -3, 2, 5, 8
    assert(BudanFourierBound(f) == 7);
    assert(BudanFourierBound(g) == 4);
    assert(BudanFourierBound(p) == 6);
    
    assert(numSturm(f)== 6);
    assert(numSturm(f,-6,0) == 3);
    assert(numSturm(f,-1,10) == 3);
    assert(numSturm(f,Multiplicity=>true) == 7);
    assert(numSturm(f,-10,5,Multiplicity=>true) == 6);
    assert(numSturm(f,0,6,Multiplicity=>true) == 4);
    
    
    assert(numSturm(g) == 4);
    assert(numSturm(g,-3,1) == 3);
    assert(numSturm(g,0,10) == 2);
    
    assert(numSturm(p) == 6);
    assert(numSturm(p,-15,0) == 3);
    assert(numSturm(p,2,10) == 2);
    ///
    
TEST ///
    R = QQ[t];
    f = (t-2)*(t-1)*(t+3);
    g = t+1;
    assert(numSylvester(f,g,-5,4) == 1);
    h = (t-4)*(t-1)^2*(t+1)*(t+3)*(t+5)*(t-6);
    p = t+5;
    assert(numSylvester(h,p,-10,10) == 5);
    assert(numSylvester(h,p,0,10) == 3);
    ///
    
TEST ///
    R = QQ[t];
    f = (t-1)^2*(t+3)*(t+5)*(t-6);
    assert(realRootIsolation(f,0.5) == {{-161/32, -299/64}, {-207/64, -23/8}, {23/32, 69/64}, {23/4, 391/64}});
    ///    
    
TEST ///
    R = QQ[x,y];
    F = {y^2 - x^2 - 1, x-y^2+4*y-2};
    I = ideal F;
    S = R/I;
    f = y^2 - x^2 - x*y + 4;
    assert(flatten entries traceForm(f) == {4, -86, -340, -42, -86, -266, -1262, -340, -340, -1262, -5884, -1454, -42, -340, -1454, -262});
    ///
    
TEST ///
     R = QQ[x,y];
     I = ideal(1 - x^2*y + 2*x*y^2, y - 2*x - x*y + x^2);
     assert(numTrace(I) == 3);
     F = {y^2-x^2-1,x-y^2+4*y-2};
     assert(numTrace(F) == 2);
     I = ideal F;
     S = R/I;
     assert(numTrace(S) == 2);
    ///
    
end

--Computes the rank and signature of the trace form of f
----change name
----change output
traceFormInfo = method()
traceFormInfo (RingElement,Ideal) := Sequence => (f,I)->(
    R := ring f;
    traceFormInfo(sub(f,R/I))
    )

traceFormInfo (RingElement) := Sequence => f->(
    R := ring f;
    if not isArtinian R then error "Expected Artinian ring";
    
    trf := traceForm f;
    ch := charPoly(trf);
    chNeg := sub(ch,(ring ch)_0=>-(ring ch)_0);
    sig := numSturm(ch,0,infinity,Multiplicity=>true) - numSturm(chNeg,0,infinity,Multiplicity=>true);
    (rank(trf),sig)
    )

--document {
--	Key => {(traceFormInfo, RingElement),traceFormInfo},
--	Usage => "traceFormInfo(f)",
--	Inputs => {"f"},
--	Outputs => { Sequence => { "the rank and signature of the trace quadratic form of", TT "f" }},
--	PARA {"This computes the rank and signature of the trace quadratic form of an element ", TT "f", " in an Artinian ring of characteristic zero"},
--	EXAMPLE lines ///
--	         R = QQ[x,y]
--		 I = ideal(1 - x^2*y + 2*x*y^2, y - 2*x - x*y + x^2)
--		 A = R/I
--		 traceFormInfo(x*y)
--		 traceFormInfo(x - 2)
--		 traceFormInfo(x + y - 3)
--	 	 ///,
--	SeeAlso => {"traceForm", "numTrace"}
  --   	}