--RealRoots.m2
newPackage(
    "RealRoots",
    Version=>"0.1",
    Date=>"Oct 9, 2020",
    Authors=>{
     	{Name=>"Jordy Lopez Garcia",
	 Email=>"jordy.lopez@tamu.edu",
	 HomePage=>"https://www.github.com/jordylopez27"},
    	{Name=>"Kelly Maluccio",
	 Email=>"keleburke@aggienetwork.com",
	 HomePage=>"https://www.github.com/kmaluccio"},
    	{Name=>"Frank Sottile",
	 Email=>"sottile@tamu.edu",
	 HomePage=>"https://www.math.tamu.edu/~frank.sottile/"},
	{Name=>"Thomas Yahl",
	 Email=>"thomasjyahl@tamu.edu",
	 HomePage=>"https://www.github.com/tjyahl"}
	},
    Headline=>"Package for symbolically exploring, counting, and locating real solutions to general polynomial systems",
    Keywords=>{"Real Algebraic Geometry"},
    PackageImports=>{},
    PackageExports=>{},
    DebuggingMode=>true
    )


export{
    --methods
    "minimalPolynomial",
    "univariateEliminant",
    "regularRepresentation",
    "characteristicPolynomial",
    "variations",
    "SylvesterSequence",
    "SylvesterCount",
    "SturmSequence",
    "SturmCount",
    "realRootIsolation",
    "BudanFourierBound",
    "traceForm",
    "traceCount",
    "rationalUnivariateRepresentation",
    "HurwitzMatrix",
    "isHurwitzStable",
    --options
    "Multiplicity"
    }


----------------------------
--METHODS FOR INTERNAL USE--
----------------------------

--Check that a polynomial is univariate
isUnivariatePolynomial = method()
isUnivariatePolynomial (RingElement) := Boolean => f->(
    S := select(support f,x->index x < numgens ring f);
    #S <= 1
    )

--Determines the variable of a univariate polynomial
variable = method()
variable (RingElement) := RingElement => f->(
    if not isUnivariatePolynomial(f) then error "Error: Expected univariate polynomial";
    S := select(support f,x->index x < numgens ring f);
    if S === {} then (ring f)_0 else S#0
    )

variable (Ideal) := RingElement => I->(
    S := support I;
    if S === {} then (ring I)_0 else S#0
    )


--Check that a ring is zero-dimensional
----isArtinian does NOT work over fields with parameters.
isArtinian = method()
isArtinian (Ring) := Boolean => R->(
    if instance(coefficientRing R,InexactField) then print "Warning: Computations over inexact field";
    dim R === 0
    )

        
--Computes the sign of a real number
sign = method()
for A in {ZZ,QQ,RR} do
sign (A) := ZZ => n->(
     if n < 0 then -1 
     else if n == 0 then 0
     else if n > 0 then 1
     )


--Computes the sign of a real univariate polynomial at a given real number
signAt = method()
for A in {ZZ,QQ,RR} do
signAt (RingElement,A) := ZZ => (f,r)->(
    sign(sub(f,{variable f => r}))
    )

signAt (RingElement,InfiniteNumber) := ZZ => (f,r)->(
    if (r>0) then (
	sign(leadCoefficient f)
	) else (
	sign((if odd first degree f then -1 else 1)*(leadCoefficient f))
	)
    )


--Computes the sequence of Horner polynomials associated to f
HornerSequence = method()
HornerSequence (RingElement) := RingElement => f ->(
    d := first degree f;
    x := variable(f);
    a := apply(d+1,i->coefficient(x^(d-i),f));
    H := new MutableList from {sub(a#0,ring f)};
    for i from 1 to d-1 do (
	H#i = x*H#(i-1) + a#i);
    toList H
    )


--computes the signature of a matrix
----can also use SylvesterCount(ch,variable ch,Multiplicity=>true)
signature = method()
signature (Matrix) := ZZ => M->(
    ch := characteristicPolynomial M;
    coeffs := flatten entries sub(last coefficients ch,ring M);
    2*(variations coeffs) - rank M
    )


--Computes the sequence of derivatives of f
derivSequence = method()
derivSequence (RingElement) := List => f->(
    if not isUnivariatePolynomial(f) then error "Error: Expected univariate polynomial.";
    if (f == 0) then error "Error: Expected nonzero polynomial";
    
    t := variable f;
    d := first degree f;
    apply(d+1, i -> diff(t^i,f))
    )

--------------------
--EXPORTED METHODS--
--------------------

--Compute the minimalPolynomial of 'f' in the quotient ideal defined by 'I'
----better naming for strategies?
minimalPolynomial = method(Options=>{Strategy=>0})
minimalPolynomial (RingElement,Ideal) := RingElement => opts->(f,I)->(
    R := ring f;
    if not (ring I === R) then error "Error: Expected polynomial and ideal of the same ring";
    minimalPolynomial(sub(f,R/I))
    )
    
minimalPolynomial (RingElement) := RingElement => opts->f->(
    R := ring f;
    if not isArtinian(R) then error "Error: Expected element of Artinian ring";

    K := coefficientRing R;
    Z := getSymbol "Z";
    S := K(monoid [Z]);
    
    if (opts.Strategy === 0) then (
	--This strategy computes the minimalPolynomial as the kernel of the multiplication map
    	phi := map(R,S,{f});
    	(ker phi)_0
        
	) else if (opts.Strategy === 1) then (
      	--This strategy computes the minimalPolynomial by finding a minimal linear combination in powers of f
    	B := basis R;
    	n := numgens source B;
    	
    	P := map(R^1,R^(n+1),(i,j)->f^j);
    	M := last coefficients(P, Monomials=>B);
    	coeffs := sub(gens ker M,K);
    	(map(S^1,S^(n+1),(i,j)->S_0^j) * coeffs)_(0,0)
	) 
    )


--Function alias
univariateEliminant = method(Options=>{Strategy=>0})
univariateEliminant (RingElement) := o-> f-> minimalPolynomial(f,o)
univariateEliminant (RingElement,Ideal) := o-> (g,I)-> minimalPolynomial(g,I,o)


--Computes a matrix representation of the multiplication map determined by f
regularRepresentation = method()
regularRepresentation (RingElement,Ideal) := Matrix => (f,I)->(
    R := ring f;
    if not (ring I === R) then error "Error: Expected polynomial ring and ideal of the same ring";
    regularRepresentation(sub(f,R/I))
    )

regularRepresentation (RingElement) := Matrix => f->(
    R := ring f;
    if not isArtinian(R) then error "Error: Expected element of Artinian ring";
    K := coefficientRing R;
    
    B := basis R;
    C := last coefficients(f*B,Monomials=>B);
    (B,sub(C,K))
    )


--Computes the characteristic polynomial of a matrix.
characteristicPolynomial = method(Options => {Variable => "Z"})
characteristicPolynomial (Matrix) := RingElement => opts->M->(
    n := numgens source M;
    if not (numgens target M === n) then error "Error: Expected a square matrix";
    
    K := ring M; 
    Z := opts.Variable;
    S := K(monoid [Z]);
    
    if (n < 30) then (
    	--characteristic polynomial via determinants
	IdZ := S_0*id_(S^n);
    	det(IdZ - M)
	
	) else (
	
	--characteristic polynomial via elementary symmetric polynomials and traces
	A := id_(ZZ^n);
    	traces := {n}|apply(n,k->(A = M*A; trace A));
    	coeffs := new MutableList from {1};
    	for k from 1 to n do (
	    coeffs#k = -sum(k,i->coeffs#(k-i-1)*traces#(i+1))/k
	    );
    	sum(n+1,i->coeffs#i*S_0^(n-i))
	)
    )


--characteristicPolynomial (RingElement,Ideal) := RingElement => opts->(f,I)->(
--    R := ring f;
--    if not (ring(I) === R) then error "Error: Expected polynomial ring and ideal of the same ring";
--    characteristicPolynomial(sub(f,R/I))
--    )



--Computes the characteristic polynomial of the regular representation of f
characteristicPolynomial (RingElement,Ideal) := RingElement => opts->(f,I)->(
    R := ring f;
    Z := opts.Variable;
    if not (ring(I) === R) then error "Error: Expected polynomial ring and ideal of the same ring";
    characteristicPolynomial(sub(f,R/I))
    )

characteristicPolynomial (RingElement) := RingElement => opts->f->(
    (B,mf) := regularRepresentation(f);
    characteristicPolynomial(mf)
    )

--check this
characteristicPolynomial (RingElement) := RingElement => opts->t ->(
 
    R := ring t; 
    Z := opts.Variable;
    K := coefficientRing R;
    S := K(monoid [Z]);
    
    if not isArtinian(R) then error "Error: Expected element of Artinian ring";
    B := basis R;
    D := numgens source B;
    v := transpose(matrix{flatten append({1},toList apply(D-1,i -> 0))}); 
    
    Vtr := matrix{toList apply(D, i-> trace last regularRepresentation(B_(0,i)))};
    Mt := last regularRepresentation(t);
    traces := {D}|apply(D,k->(v = Mt*v;(Vtr*v)_(0,0)));
    coeffs := new MutableList from {1};
        for k from 1 to D do (
	coeffs#k = -sum(k,i->(coeffs#(k-i-1)*traces#(i+1)))/k
	);
    sum(D+1,i->coeffs#i*S_0^(D-i))
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
for A in {ZZ,QQ,RR,InfiniteNumber} do 
for B in {ZZ,QQ,RR,InfiniteNumber} do
BudanFourierBound (RingElement,A,B) := ZZ => (f,a,b)->(
    if not isUnivariatePolynomial(f) then error "Error: Expected univariate polynomial.";
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
    if (f==0 or g==0) then error "Error: Expected nonzero polynomials";
    if not (isUnivariatePolynomial(f)) then error "Error: Expected univariate polynomials";
    R := ring f;
    if not isField coefficientRing R then error "Error: Expected polynomials over a field";
    if not (ring g === R) then error "Error: Polynomials should be in the same ring";
    
    --dividing out common factors
    h := gcd(f,g);
    f = sub(f/h,R);
    g = sub(g/h,R);

    Syl := new MutableList from {f,g};
    
    i := 1;
    while (Syl#i != 0) do (
	i = i+1;
	Syl#i = -Syl#(i-2) % Syl#(i-1)
    );
    	    
    toList Syl
    )


--Computes the difference in the number of roots of f where g is positive and where g is negative
----letting g = 1 gives the number of real roots from the Sturm sequence
SylvesterCount = method(Options=>{Multiplicity=>false})
for A in {ZZ,QQ,RR,InfiniteNumber} do 
for B in {ZZ,QQ,RR,InfiniteNumber} do
SylvesterCount (RingElement,RingElement,A,B) := ZZ => opts->(f, g, a, b)->(
    if not (a<b) then error "Error: Expected non-empty interval";
    l := SylvesterSequence(f,diff(variable f,f)*g);
    n := variations apply(l,h->signAt(h,a)) - variations apply(l,h->signAt(h,b));
    if opts.Multiplicity then (
	h := gcd(f,diff(variable f,f));
	while (first degree h > 0) do (
	    n = n + SylvesterCount(h,g,a,b);
	    h = gcd(h,diff(variable h,h))
	    )
	);
    n
    )

SylvesterCount (RingElement,RingElement) := ZZ => opts->(f,g)->(
    SylvesterCount(f,g,-infinity,infinity)
    )


--Computes the Sturm sequence of f via a Sylvester sequence
SturmSequence = method()
SturmSequence (RingElement) := List => f->(
    if (f == 0) then error "Error: Expected nonzero polynomial";
    SylvesterSequence(f,diff(variable f,f))
    )


--Computes the difference in variations of the Sturm sequence at specified values
SturmCount = method(Options=>{Multiplicity=>false})
for A in {ZZ,QQ,RR,InfiniteNumber} do
for B in {ZZ,QQ,RR,InfiniteNumber} do
SturmCount (RingElement,A,B) := ZZ => opts->(f,a,b)->(
    R := ring f;
    SylvesterCount(f,1_R,a,b,opts)
    )

SturmCount (RingElement) := ZZ => opts->f->( 
    SturmCount(f,-infinity,infinity,opts)
    )


--Uses Sturm sequence and a bisection method to isolate real solutions to a real univariate polynomial within a tolerance
realRootIsolation = method()
for A in {ZZ,QQ,RR} do
realRootIsolation (RingElement,A) := List => (f,r)->(
    if not r > 0 then error "Error: Expected positive integer or positive rational number";
    
    if not isUnivariatePolynomial(f) then error "Error: Expected univariate polynomial";
    R := ring f;
    
    f = sub(f/gcd(f,diff(variable f,f)),R);
    
    if (SturmCount(f)>0) then (
	l := SturmSequence(f);
	
	--bound for real roots
	C := (listForm f)/last;
    	M := (sum(C,abs))/(leadCoefficient f);
	
	L := {{-M,M}};
	midp := 0;
	v := new MutableHashTable from {M=>variations apply(l,g->signAt(g,M)),-M=>variations apply(l,g->signAt(g,-M))};
	
	while (max apply(L,I-> I#1-I#0) > r) or (max apply(L,I-> v#(I#0)-v#(I#1)) > 1) do (
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
traceForm = method()
traceForm (RingElement,Ideal) := Matrix => (f,I)->(
    R := ring f;
    if not (ring I === R) then error "Error: Expected RingElement and Ideal in same Ring";
    traceForm(sub(f,R/I))
    )

traceForm (RingElement) := Matrix => f->(
    R := ring f;
    if not isArtinian(R) then error "Error: Expected zero-dimensional ring";
    B := basis R;
    K := coefficientRing R;

    mm := sub(last coefficients(f*B**B,Monomials=>B),K);
    tr := matrix {apply(first entries B, x -> trace last regularRepresentation x)};
    adjoint(tr*mm, source tr, source tr)
    )


--Compute the number of real points of a scheme/real univariate polynomial/real polynomial system using the trace form.
traceCount = method()
traceCount (RingElement) := ZZ => f->(
    R := ring f;
    traceCount(R/f)
    )

traceCount (List) := ZZ => F->(
    I := ideal F;
    R := ring I;
    traceCount(R/I)
    )

traceCount (Ideal) := ZZ=> I->(
    R := ring I;
    traceCount(R/I)
    )

traceCount (QuotientRing) := ZZ=> R->(
    signature(traceForm(1_R))
    )


--Computes the Rational Univariate Representation of a zero-dimensional ideal
----output is:
------a linear functional l that separates the points of I
------a polynomial ch defining the image of the points of V(I) under the map defined by l
------a list of rational polynomials that consitite a rational inverse (on V(I)) of the map defined by l
rationalUnivariateRepresentation = method()
rationalUnivariateRepresentation (Ideal) := Sequence => I->(
    R := ring I;
    S := R/I;
    if not isArtinian(S) then error "Error: Expected I to be a zero-dimensional ideal";
    d := rank traceForm(1_S);
    
    i := 1;
    X := gens R;
    n := #X;
    while (i < n*(binomial(d,2))) do (
    	l := sum(X,apply(n,k->i^k),(a,b)->a*b);
	(B,m) := regularRepresentation(sub(l,S));
	ch := characteristicPolynomial(m);
	
	chbar := ch/gcd(ch,diff((support ch)_0,ch));
	chbar = sub(chbar,ring ch);
	print(first degree chbar);
	if (first degree chbar === d) then break;
	i = i+1
	);
    
    T := ring ch;
    phi := map(S,T,{l});
    H := phi matrix{HornerSequence(chbar)};
    M := sub(matrix {gens R},S);        
    tr := matrix{apply(first entries B,x-> trace last regularRepresentation x)};

    gvCoeffs := sub(adjoint(tr*(last coefficients(M**H,Monomials=>B)),source M,source H),coefficientRing R);
    g1 := sub(diff(variable ch,ch)/gcd(diff(variable ch,ch),ch),ring ch);
    Z := matrix {apply(d,i->(ring ch)_0^(d-1-i))};
    gv := first entries ((1/g1)*sub(Z*gvCoeffs,frac ring ch));
        
    return(l,ch,gv)
    )


--Computes the Hurwitz matrix of f (of order k)
HurwitzMatrix = method()
HurwitzMatrix (RingElement) := Matrix => f->(
    d := first degree f;
    if not (d>0) then error "Error: Expected polynomial of positive degree";
    
    x := variable f;
    a := apply(d+1,i->coefficient(x^i,f));
    
    M := matrix table(d,d,(i,j)->if (0 <= d-1+i-2*j and d-1+i-2*j <= d) then a#(d-1+i-2*j) else 0);
    M
    )

HurwitzMatrix (RingElement,ZZ) := Matrix => (f,k)->(
    M := HurwitzMatrix f;
    if (k==0) then matrix{{1}} else submatrix(M,toList(0..k-1),toList(0..k-1))
    )


--Determines whether or not a univariate polynomial of degree >=1 is Hurwitz stable.
----criterion requires lead coefficient of f to be positive
isHurwitzStable = method()
isHurwitzStable (RingElement) := Boolean => f->(
    if (leadCoefficient f < 0) then f = -f;
    d := first degree f;
    all(d+1,k->det HurwitzMatrix(f,k) > 0)
    )

--------------------
---DOCUMENTATION----
--------------------

beginDocumentation()
document {
	Key => RealRoots,
	Headline => "Package for exploring, counting and locating real solutions to polynomial systems",
	"The purpose of this package is to provide general tools for elimination and solving systems of polynomial equations."
	}

document {
	Key => {minimalPolynomial,(minimalPolynomial, RingElement),(minimalPolynomial,RingElement,Ideal),[minimalPolynomial,Strategy]},
	Headline => "the minimal polynomial of an element of an Artinian ring",
	Usage => "minimalPolynomial(f)
	          minimalPolynomial(g,I)",
	Inputs => {
	    RingElement => "f" => {"an element of an Artinian ring"},
	    RingElement => "g" => {"a polynomial"},
	    Ideal => "I" => {"a zero-dimensional ideal"},
	    Strategy => {"set method for computing the minimal polynomial"}
	    },
	Outputs => { RingElement => {"the desired minimal polynomial. See description"}},
	PARA {"This computes the minimal polynomial of a ring element ", TT "f", " in the Artinian ring ", TT "ring f", ", or the minimal polynomial of a polynomial ", TT "g", " in the Artinian ring ", TT "(ring g)/I", ".
	    When ",TT "f"," is a variable in ", TT "ring f", ", this is the eliminant with respect to that variable."},
	EXAMPLE lines ///
	    	R = QQ[x,y]
		I = ideal(y^2 - x^2 - 1,x - y^2 + 4*y - 2)
		minimalPolynomial(y,I)
		S = R/I
		minimalPolynomial(y)
	 	///,
	PARA {"We provide two examples to compute minimal polynomials given by ",TT "Strategy => 0"," (computes the kernel of ",TEX///$k[T]\to$///,TT " ring f"," by sending ", TEX///$T$///," to ",TT "f",") and ",TT "Strategy => 1", " (a minimal linear combination of powers of the input)."},
	EXAMPLE lines ///
		minimalPolynomial(x,Strategy => 0)
	    	minimalPolynomial(x,Strategy => 1)
	        ///
	}
    
document {
        Key => {univariateEliminant,(univariateEliminant,RingElement),(univariateEliminant,RingElement,Ideal),[univariateEliminant,Strategy]},
        Headline => "the univariate eliminant of an element of an Artinian ring",
	Usage => "univariateEliminant(f)
	          univariateEliminant(g,I)",
	Inputs => {
	    RingElement => "f" => {"an element of an Artinian ring"},
	    RingElement => "g" => {"a polynomial"},
	    Ideal => "I" => {"a zero-dimensional ideal"},
	    Strategy => {"set method for computing the univariate eliminant"}
	    },
	Outputs => { RingElement => {"the desired univariate polynomial. See description"}},
	PARA {"This computes the univariate eliminant of a ring element ", TT "f", " in the Artinian ring ", TT "ring f", ", or the univariate eliminant of a polynomial ", TT "g", " in the Artinian ring ", TT "(ring g)/I", ".
	    When ",TT "f"," is a variable in ", TT "ring f", ", this is the eliminant with respect to that variable. This is computed by finding the minimal polynomial of the corresponding multiplication matrix."},
	EXAMPLE lines ///
	    	R = QQ[x,y]
		I = ideal(y^2 - x^2 - 1,x - y^2 + 4*y - 2)
		univariateEliminant(y,I)
		S = R/I
		univariateEliminant(y)
	 	///,
	PARA {"We provide two examples to compute minimal polynomials given by ",TT "Strategy => 0"," (computes the kernel of ",TEX///$k[T]\to$///,TT " ring f"," by sending ", TEX///$T$///," to ",TT "f",") and ",TT "Strategy => 1", " (a minimal linear combination of powers of the input)."},
	EXAMPLE lines ///
		univariateEliminant(x,Strategy => 0)
	    	univariateEliminant(x,Strategy => 1)
	        ///,
	SeeAlso => {"minimalPolynomial"}
	}

document {
	Key => {regularRepresentation,(regularRepresentation, RingElement, Ideal), (regularRepresentation, RingElement)},
	Headline => "the regular representation of a rational polynomial",
	Usage => "regularRepresentation(f)
	          regularRepresentation(g,I)",
	Inputs => {
	    RingElement => "f"=> {"an element of an Artinian ring"},
	    RingElement => "g"=> {"a rational polynomial"},
	    Ideal => "I" => {"a zero-dimensional ideal in the same ring as ", TT "g"},
	    },
	Outputs => {
	    Matrix => {"the standard basis of ",TT "ring f", " (resp. ",TT "(ring g)/I",")"},
	    Matrix => {"the matrix of the linear map defined by multiplication by ", TT "f"," (resp. ",TT "g",") in ",TT "ring f", " (resp. ",TT "(ring g)/I",")"}},
	PARA {"This command gives the matrix of the linear map defined by multiplication by ", TT "f", " (resp. ",TT "g",") in terms of the standard basis of the finite-dimensional vector space ", TT "ring f"," (resp. ",TT "(ring g)/I",")."},
	EXAMPLE lines ///
		 R = QQ[x,y]
		 I = ideal(y^2 - x^2 - 1,x - y^2 + 4*y - 2)
		 regularRepresentation(y,I)
		 S = R/I
		 regularRepresentation(y)
	 	 ///,
     	}

document {
	Key => {characteristicPolynomial, (characteristicPolynomial, Matrix),(characteristicPolynomial,RingElement),(characteristicPolynomial,RingElement,Ideal),[characteristicPolynomial,Variable]},
	Headline => "the characteristic polynomial of a matrix or the characteristic polynomial of the regular representation of a polynomial",
	Usage => "characteristicPolynomial(M)
	          characteristicPolynomial(f)
		  characteristicPolynomial(g,I)",
	Inputs => {
	    Matrix => "M" => {"a square matrix"},
	    RingElement => "f" => {"an element of an Artinian ring"},
	    RingElement => "g" => {"a polynomial"},
	    Ideal => "I"  => {"a zero-dimensional ideal"},
	    },
	Outputs => {RingElement => {"the desired characteristic polynomial. See description."}},
	   
	PARA  {"This computes the characteristic polynomial of the matrix ", TT "M", ", or the characteristic polynomial of the regular representation of ", TT "f"," on the Artinian ring ",TT "ring f", ", or the 
		characteristic polynomial of the regular representation of ", TT "g"," on the Artinian ring ",TT "(ring g)/I", "." },
	EXAMPLE lines ///
	         R = QQ[x,y]
		 M = matrix{{2,1},{1,-1}}
		 characteristicPolynomial(M)
		 ///,
    	PARA {"We can also change the variable name, as we show below."},
	EXAMPLE lines ///
	         characteristicPolynomial(M,Variable => "x")
		 ///,
	PARA {"We show the last two methods."},
       	EXAMPLE lines ///
		 I = ideal(y^2 - x^2 - 1,x - y^2 + 4*y - 2)
		 characteristicPolynomial(y,I)
		 S = R/I
		 characteristicPolynomial(y)
		 ///,
     	}


 document {
	Key => {SylvesterSequence,(SylvesterSequence, RingElement, RingElement)},
	Headline => "the Sylvester sequence of two rational univariate polynomials",
	Usage => "SylvesterSequence(f,g)",
	Inputs => {
	    RingElement => "f" => {"a rational univariate polynomial"},
	    RingElement => "g" => {"a rational univariate polynomial in the same variable as ", TT"f"},
	    },
	Outputs => { List => { "the Sylvester sequence of ", TT "f", " and ",TT "g"}},
	PARA {"This computes the Sylvester sequence of two rational univariate polynomials ", TT "f", " and ", TT "g", " in the same ring."},
	EXAMPLE lines ///
	         R = QQ[t]
		 f = (t + 1)*(t + 2)
		 g = t + 2
		 SylvesterSequence(f,g)
	 	 ///,
	SeeAlso => {"SylvesterCount"}
     	}
    
document {
	Key => {SylvesterCount,(SylvesterCount,RingElement,RingElement)}|(flatten table({ZZ,QQ,RR,InfiniteNumber},{ZZ,QQ,RR,InfiniteNumber},(a,b)->(SylvesterCount,RingElement,RingElement,a,b))),
	Headline => "the difference in variations of the Sylvester sequence of two rational univariate polynomials",
	Usage => "SylvesterCount(f,g,a,b)
	          SylvesterCount(f,g)",
	Inputs => {
	    RingElement => "f" => {"a rational univariate polynomial"},
	    RingElement => "g" => {"a rational univariate polynomial"},
	    RR => "a" => {"(optional) the lower bound of the interval"},
	    RR => "b" => {"(optional) the upper bound of the interval"},
	    Multiplicity => {"option for computing roots with multiplicity"}
	    },
	Outputs => { ZZ => {"the difference between the number of roots of ",TT "f"," in the interval ",TEX///$(a,b]$///," where ",TT "g",
		" is positive and where ",TT "g"," is negative"}},
	PARA {"This computes the difference in variations of the Sylvester sequence of ", TT "f"," and ",TT "f'g"," on the interval ",TEX///$(a,b]$///,"."},
	EXAMPLE lines ///
	    	 R = QQ[t]
		 f = (t - 2)*(t - 1)*(t + 3)
		 g = t + 1
		 a = -5
		 b = 4
		 SylvesterCount(f,g,a,b)
	 	 ///,
	SeeAlso => {"SylvesterSequence"}
     	}

document {
	Key => {SturmSequence,(SturmSequence, RingElement)},
	Headline => "the Sturm sequence of a rational univariate polynomial",
	Usage => "SturmSequence(f)",
	Inputs => {
	    RingElement => "f" => {"a rational univariate polynomial"},
	    },
	Outputs => { List => {"the Sturm sequence of ", TT "f"}},
	PARA {"This computes the Sturm sequence of the square-free part of a univariate polynomial ", TT "f","."},
	EXAMPLE lines ///
	 	 R = QQ[t]
		 f = 45 - 39*t - 34*t^2 + 38*t^3 - 11*t^4 + t^5
		 roots f
		 SturmSequence(f)
	 	 ///,
	SeeAlso => {"SturmCount"}
     	}

document {
    	Key => {"Multiplicity(RealRoots)", [SylvesterCount, Multiplicity], [SturmCount, Multiplicity]},
	PARA {"This is an optional input for counting roots with multiplicity."}
    }

document {
    	Key => {"Multiplicity"},
	PARA {"This is a symbol for counting roots with multiplicity."}
    }

document {
	Key => {SturmCount,(SturmCount,RingElement)}|(flatten table({ZZ,QQ,RR,InfiniteNumber},{ZZ,QQ,RR,InfiniteNumber},(a,b)->(SturmCount,RingElement,a,b))),
	Headline => "the number of real roots of a rational univariate polynomial",
	Usage => "SturmCount(f,a,b)
	          SturmCount(f)",
	Inputs => {
	    RingElement => "f" => {"a rational univariate polynomial"},
	    RR => "a" => {"a lower bound of the interval"},
	    RR => "b" => {"an upper bound of the interval"},
	    Multiplicity => {"option for computing roots with multiplicity"}
	    },
	Outputs => { ZZ => {"the number of real roots of ", TT "f"," in the interval ",TEX///$(a,b]$///}},
	PARA {"This computes the difference in variation of the Sturm sequence of ", TT "f"," on the interval ",TEX///$(a,b]$///,". If ", TT "a", " and ", TT "b"," are not specified,
	     the interval will be taken from ",TEX///$-\infty$///," to ",TEX///$\infty$///,". If the coefficients of ",TT "f"," are inexact, then the computations may be unreliable."},
	EXAMPLE lines ///
	    	 R = QQ[t]
		 f = (t - 5)*(t - 3)^2*(t - 1)*(t + 1)
		 roots f
		 SturmCount(f)
		 SturmCount(f,0,5)
		 SturmCount(f,-2,2)
		 SturmCount(f,-1,5)	       
	 	 ///,
	PARA {"In the above example, multiplicity is not counted. To include it, make the multiplicity option ",TT "true","."},
	EXAMPLE lines ///
		SturmCount(f,Multiplicity=>true)
		SturmCount(f,0,5,Multiplicity=>true)
		SturmCount(f,0,3,Multiplicity=>true)
		///,
	PARA {"If ", TT "a"," is an ", TT "InfiniteNumber", ", then the lower bound will be ",TEX///$-\infty$///,", and if ", TT "b"," is an ", TT "InfiniteNumber", 
	    ", then the upper bound is ",TEX///$\infty$///,"."},
	EXAMPLE lines ///
	    	SturmCount(f,-infinity, 0)
		SturmCount(f,0,infinity)
		SturmCount(f,-infinity,infinity)
		///,
	SeeAlso => {"SturmSequence"}
     	}
    
document {
    	Key => {variations,(variations, List)},
	Headline => "the number of sign changes of an ordered list of numbers",
	Usage => "variations(l)",
	Inputs => {
	    List => "l" => {" of ordered numbers"},
	    },
	Outputs => {ZZ => { "the number of sign changes in the ordered list ", TT "l" }},
	PARA {"This computes the number of sign changes in the ordered list ", TT "l","."},
	EXAMPLE lines ///
		 L = for i to 10 list random(-50,50)
		 variations(L)
	 	 ///,
     	}
    
document {
        Key => {realRootIsolation,(realRootIsolation, RingElement,ZZ),(realRootIsolation, RingElement,QQ),(realRootIsolation, RingElement,RR)},
	Headline => "a list that isolates the real roots of a rational univariate polynomial",
	Usage => "realRootIsolation(f,r)",
	Inputs => {
	    RingElement => "f" => {"a rational univariate polynomial"},
	    RR => "r" => {"a positive rational number"},
	    },
	Outputs => {List => {"of intervals that contain all the real roots of ", TT "f"}},
	PARA {"This method uses a Sturm sequence and a bisection method to isolate real solutions of ", TT "f",
	       " in intervals of length at most ", TT "r","."},
	EXAMPLE lines ///
	    	 R = QQ[t]
		 f = 45 - 39*t - 34*t^2 + 38*t^3 - 11*t^4 + t^5
		 realRootIsolation(f,1/2)
		 realRootIsolation(f,.2)
	 	 ///,
	SeeAlso => {"SturmSequence"}
     	}
    
document {
	Key => {BudanFourierBound,(BudanFourierBound,RingElement)}|(flatten table({ZZ,QQ,RR,InfiniteNumber},{ZZ,QQ,RR,InfiniteNumber},(a,b)->(BudanFourierBound,RingElement,a,b))),
	Headline => "a bound for the number of real roots of a univariate polynomial with rational coefficients",
	Usage => "BudanFourierBound(f, a, b)
	          BudanFourierBound(f)",
	Inputs => {
	    RingElement => "f" => {"a univariate polynomial with rational coefficients, where", TT " ring f ", "is not necessarily univariate"},
	    RR => "a" => {"(optional) the lower bound of the interval"},
	    InfiniteNumber => "a" => {"(optional) the lower bound of the interval"},
	    RR => "b" => {"(optional) the upper bound of the interval"},
	    InfiniteNumber => "b" => {"(optional) the upper bound of the interval"},
	    },
	Outputs => { ZZ => { "the bound for the number of real roots of a rational univariate polynomial", TT " f ", "in the interval ", TT "(a,b]"}},
	PARA {"This computes the bound from the Budan Fourier Theorem for the number of real roots of a rational univariate polynomial", TT " f ", "in the interval", TT "(a,b]",
	      ", counted with multiplicity. If the interval is not specified, it 
	      computes such bound on ", TEX///$(-\infty, \infty)$///,". Moreover,", TT " ring f ", "is allowed to be multivariate."},
	EXAMPLE lines ///
	         R = QQ[t]
		 f = 45 - 39*t - 34*t^2 + 38*t^3 - 11*t^4 + t^5
		 BudanFourierBound(f)
		 g = (t + 5)*(t + 3)*(t + 1)*(t - 1)^2*(t - 4)*(t - 6)
		 BudanFourierBound(g,-6,infinity)
		 BudanFourierBound(g,-1,5)
	 	 ///,
	PARA {"We also provide examples when the interval includes ", TEX///$-\infty$///," or ", TEX///$\infty$///, "."},
	EXAMPLE lines ///
	         BudanFourierBound(g,-infinity,0)
	         BudanFourierBound(g,3,infinity)
		 BudanFourierBound(g,-infinity,infinity)
		 ///
     	}
    
    
document {
	Key => {traceForm,(traceForm, RingElement),(traceForm,RingElement,Ideal)},
	Headline => "the trace quadratic form of a rational polynomial in an Artinian ring",
	Usage => "traceForm(f)
	          traceForm(f,I),",
	Inputs => {
	    RingElement => "f" => {"a rational polynomial in an Artinian Ring"},
	    Ideal => "I" => {"the ideal generated by ", TT "f"},
	    },
	Outputs => {Matrix => {"a symmetric matrix representing the trace quadratic form of ", TT "f" , " in the standard basis of the Artinian ring"}},
	PARA {"This computes the trace quadratic form of a polynomial ", TT "f", " in an Artinian ring."},
	EXAMPLE lines ///
	         R = QQ[x,y]
		 F = {y^2 - x^2 - 1, x - y^2 + 4*y - 2}
		 I = ideal F
		 S = R/I
		 f = y^2 - x^2 - x*y + 4
		 traceForm(f)
	 	 ///,
	SeeAlso => {"traceCount"}
     	}
    

document {
	Key => {traceCount,(traceCount, QuotientRing), (traceCount, RingElement), (traceCount, List),(traceCount,Ideal)},
        Headline => "the number of real points of the spectrum of an Artinian ring (of characteristic 0)",
	Usage => "numRealTrace(R)",
	Inputs => {
	    QuotientRing => "S" => {"an Artinian ring"},
	    RingElement => "f" => {"a rational univariate polynomial"},
	    Ideal => "I" => {"the ideal generated by ", TT "f"},
	    List => "l" => {"a system of rational univariate polynomials"},
	    },
	Outputs => { ZZ => {"the number of real points of Spec", TT "R" }},
	PARA {"This computes the number of real points of Spec", TT "R", ", where ", TT "R", " is an Artinian ring with characteristic zero"},
	EXAMPLE lines ///
	         R = QQ[x,y]
		 F = {y^2 - x^2 - 1,x - y^2 + 4*y - 2}
		 I = ideal F
		 S = R/I
		 traceCount(S)
		 ///,
	EXAMPLE lines ///
		 R = QQ[x,y]
		 I = ideal(1 - x^2*y + 2*x*y^2, y - 2*x - x*y + x^2)
		 traceCount(I)
	 	 ///,
	SeeAlso => {"traceForm"}
     	}
    
document {
        Key => {rationalUnivariateRepresentation, (rationalUnivariateRepresentation, Ideal)},
	Headline => "the rational univariate representation of a zero-dimensional ideal",
	Usage => "rationalUnivariateRepresentation(I)",
	Inputs => {
	    Ideal => "I" => {"a zero-dimensional ideal"},
	    },
	Outputs => {List => {"the rational univariate representation of ",TT "I"}},
	PARA{"This computes the rational univariate representation of a zero-dimensional ideal."},
	
	EXAMPLE lines ///
	R = QQ[x,y]
	I = ideal(x*y - 1,2*x - y + 3)
	rationalUnivariateRepresentation(I)
	///
    }
    
document {
	Key => {HurwitzMatrix,(HurwitzMatrix,RingElement),(HurwitzMatrix, RingElement, ZZ)},
	Headline => "a specified principle submatrix of the Hurwitz matrix of a univariate polynomial",
	Usage => "HurwitzMatrix(f,k)",
	Inputs => {
	    RingElement => "f" => {"a rational univariate polynomial of degree n"},
	    ZZ => "k" => {"a nonnegative integer that determines the dimensions of a square submatrix of the ",TEX///$n\times n$///," Hurwitz matrix of ",TT "f","."},
	    },
	Outputs => {Matrix => {"the ",TEX///$k\times k$///, " submatrix ",TEX///$H_{k}$///," generated by the corresponding leading principal minor of the ",TEX///$n\times n$///," Hurwitz matrix ",TEX///$H$///," of ", TT "f","."}},
	PARA{"This computes the ",TEX///$k\times k$///, " submatrix ",TEX///$H_{k}$///," of the corresponding leading principal minor of the ",TEX///$n\times n$///," Hurwitz matrix ",TEX///$H$///," of a rational univariate polynomial ", TT "f"," of degree n with positive leading coefficient and degree at least 1.
	    The polynomial, however, is not necessarily from a univariate polynomial ring."},
	
	EXAMPLE lines ///
	    	R = QQ[x]
	        f = 3*x^4 - 7*x^3 + 5*x - 7
		HurwitzMatrix(f) 
		HurwitzMatrix(f,4)
	        HurwitzMatrix(f,3)	      
	 	 ///,
	PARA{"We can also use mutliple variables to represent unknown coefficients. Note that we create another ring ",TT "S"," so 
	    that ", TT "x", " and ", TT "y"," are not considered variables in the same ring and so confuse the monomials ", TEX///$x$///, " or ",TEX///$y$///,
	    " with ",TEX///$xy$///,"."},
	EXAMPLE lines ///
	        S = R[y]
		g = y^3 + 2*y^2 + y - x + 1
		HurwitzMatrix(g,3)
		HurwitzMatrix(g,2)
		HurwitzMatrix(g,1)
		 ///,
	SeeAlso => {"isHurwitzStable"} 
	}   
    
document {
	Key => {isHurwitzStable,(isHurwitzStable, RingElement)},
	Headline => "determines whether or not a rational univariate polynomial is Hurwitz stable",
	Usage => "isHurwitzStable(f)",
	Inputs => {RingElement => "f" => {"a rational univariate polynomial"}},
	Outputs => { Boolean => { "the Hurwitz stability of a rational univariate polynomial ", TT "f"}},
	PARA {"Recall that a univariate polynomial is Hurwitz stable if all its roots have negative real parts. This method determines the Hurwitz stability of a rational univariate polynomial ", TT "f", "with positive leading coefficient and degree at least 1. 
	    The polynomial, however, is not necessarily from a univariate polynomial ring."},
	EXAMPLE lines ///
	    	R = QQ[x]
            	f = 3*x^4 - 7*x^3 + 5*x - 7 
		g = x^2 + 10*x + 21
		isHurwitzStable(f)
		isHurwitzStable(g)	      
	 	 ///,
	SeeAlso => {"HurwitzMatrix"}	 
     	}

TEST ///
    R = QQ[x,y];
    F = {y^2-x^2-1,x-y^2+4*y-2};
    I = ideal F;
    S = R/I;
    a = minimalPolynomial(x);
    T = ring a;
    assert(flatten entries last coefficients(minimalPolynomial(x)) == {1,-2,-9,-6,-7});
    assert(flatten entries last regularRepresentation(y) == {0, 0, -3, -2, 0, 0, -1, 1, 0, 1, 4, 0, 1, 0, 4, 4});
    M = last regularRepresentation(y);
    pol = characteristicPolynomial(M);
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
    assert(BudanFourierBound(g,-infinity,0) == 2);
    assert(BudanFourierBound(g,-1,infinity) == 3);
    
    assert(SturmCount(f)== 6);
    assert(SturmCount(f,-6,0) == 3);
    assert(SturmCount(f,-1,10) == 3);
    assert(SturmCount(f,Multiplicity=>true) == 7);
    assert(SturmCount(f,-10,5,Multiplicity=>true) == 6);
    assert(SturmCount(f,0,6,Multiplicity=>true) == 4);
    
    
    assert(SturmCount(g) == 4);
    assert(SturmCount(g,-3,1) == 3);
    assert(SturmCount(g,0,10) == 2);
    
    assert(SturmCount(p) == 6);
    assert(SturmCount(p,-15,0) == 3);
    assert(SturmCount(p,2,10) == 2);
    ///
    
TEST ///
    R = QQ[t];
    f = (t-2)*(t-1)*(t+3);
    g = t+1;
    assert(SylvesterCount(f,g,-5,4) == 1);
    h = (t-4)*(t-1)^2*(t+1)*(t+3)*(t+5)*(t-6);
    p = t+5;
    assert(SylvesterCount(h,p,-10,10,Multiplicity=>true) == 6);
    assert(SylvesterCount(h,p,0,10) == 3);
    ///
    
TEST ///
    R = QQ[t];
    f = (t-1)^2*(t+3)*(t+5)*(t-6);
    assert(realRootIsolation(f,1/2) == {{-161/32, -299/64}, {-207/64, -23/8}, {23/32, 69/64}, {23/4, 391/64}});
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
     assert(traceCount(I) == 3);
     F = {y^2-x^2-1,x-y^2+4*y-2};
     assert(traceCount(F) == 2);
     I = ideal F;
     S = R/I;
     assert(traceCount(S) == 2);
    ///
    
TEST ///
     R = QQ[x];
     f = 3*x^4 - 7*x^3 + 5*x - 7;
     assert(HurwitzMatrix(f,4) == sub(matrix{{-7,5,0,0},{3,0,-7,0},{0,-7,5,0},{0,3,0,-7}},QQ));
     assert(HurwitzMatrix(f,3) == sub( matrix{{-7,5,0},{3,0,-7},{0,-7,5}},QQ));
     assert(isHurwitzStable(f) == false);
     g = x^2 + 10*x + 21;
     assert(isHurwitzStable(g) == true);
     ///

--TEST ///
  --   R = QQ[x,y];
  --   I = ideal(x*y - 1,2*x - y + 3);
    -- Z = (support I)_0; --little trick to compute Z not being symbol
    -- assert(rationalUnivariateRepresentationresentation(I) == {Z^2 - (3/2)*Z - 9, x + y});
    -- ///	 
    
end






