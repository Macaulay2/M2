--RealRoots.m2
newPackage(
    "RealRoots",
    Version=>"1.1",
    --updates/corrections to realRootIsolation by Corin Lee (cel34@bath.ac.uk) 2025/05/25
    Date=>"Oct 9, 2020",
    Authors=>{
     	{Name=>"Jordy Lopez Garcia",
	 Email=>"jordy.lopez@tamu.edu",
	 HomePage=>"https://jordylopez27.github.io/"},
    	{Name=>"Kelly Maluccio",
	 Email=>"kmaluccio@gmail.com",
	 HomePage=>"https://www.github.com/kmaluccio/"},
    	{Name=>"Frank Sottile",
	 Email=>"sottile@tamu.edu",
	 HomePage=>"https://franksottile.github.io/"},
	{Name=>"Thomas Yahl",
	 Email=>"tyahl@wisc.edu",
	 HomePage=>"https://tjyahl.github.io/"}
	},
    Headline=>"symbolically exploring, counting, and locating real solutions to general polynomial systems",
    Keywords=>{"Real Algebraic Geometry"},
    PackageImports=>{},
    PackageExports=>{},
    DebuggingMode=>false,
    Certification => {
	"journal name" => "Journal of Software for Algebra and Geometry",
	"journal URI" => "https://msp.org/jsag/",
	"article title" => "Real solutions to systems of polynomial equations in Macaulay2",
	"acceptance date" => "2024-03-18",
	"published article URI" => "https://msp.org/jsag/2024/14-1/p10.xhtml",
	"published article DOI" => "10.2140/jsag.2024.14.87",
	"published code URI" => "https://msp.org/jsag/2024/14-1/jsag-v14-n1-x10-RealRoots.m2",
	"release at publication" => "8280f904f702534a4a01a727e86c8ac56c64f25b",
	"version at publication" => "1.0",
	"volume number" => "14",
	"volume URI" => "https://msp.org/jsag/2024/14-1/"
	}
    )


export{
    --methods
    "signature",
    "minimalPolynomial",
    "univariateEliminant",
    "regularRepresentation",
    "characteristicPolynomial",
    "variations",
    "sylvesterSequence",
    "SylvesterSequence"=>"sylvesterSequence",
    "sylvesterCount",
    "SylvesterCount"=>"sylvesterCount",
    "sturmSequence",
    "SturmSequence"=>"sturmSequence",
    "sturmCount",
    "SturmCount"=>"sturmCount",
    "realRootIsolation",
    "budanFourierBound",
    "BudanFourierBound"=>"budanFourierBound",
    "traceForm",
    "traceCount",
    "realCount",
    "rationalUnivariateRepresentation",
    "hurwitzMatrix",
    "HurwitzMatrix"=>"hurwitzMatrix",
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
    S := select(support I,x->index x < numgens ring I);
    if S === {} then (ring I)_0 else S#0
    )


--Check that a ring is zero-dimensional
----isArtinian does NOT work over fields with parameters.
isArtinian = method()
isArtinian (Ring) := Boolean => S->(
    if not (isPolynomialRing ambient S and isField coefficientRing S) then error "Error: Expected quotient of polynomial ring over a field";
    dim S === 0
    )

--Computes the sign of a real univariate polynomial at a given real number
signAt = method()
for A in {ZZ,QQ} do
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
hornerSequence = method()
hornerSequence (RingElement) := List => f ->(
    d := first degree f;
    x := variable(f);
    a := apply(d+1,i->coefficient(x^(d-i),f));
    H := new MutableList from {sub(a#0,ring f)};
    for i from 1 to d-1 do (
	H#i = x*H#(i-1) + a#i
	);
    toList H
    )

--Computes the sequence of derivatives of f
derivSequence = method()
derivSequence (RingElement) := List => f->(
    if not isUnivariatePolynomial(f) then error "Error: Expected univariate polynomial.";
    if (f == 0) then error "Error: Expected nonzero polynomial.";
    
    t := variable f;
    d := first degree f;
    apply(d+1, i -> diff(t^i,f))
    )

--------------------
--EXPORTED METHODS--
--------------------

--Compute the minimalPolynomial of 'f' in the quotient ideal defined by 'I'
----better naming for strategies?
minimalPolynomial = method(Options=>{Strategy=>0,Variable=>"Z"})
minimalPolynomial (RingElement,Ideal) := RingElement => opts->(f,I)->(
    R := ring f;
    if not (ring I === R) then error "Error: Expected polynomial and ideal of the same ring";
    minimalPolynomial(sub(f,R/I))
    )
    
minimalPolynomial (RingElement) := RingElement => opts->f->(
    R := ring f;
    if not isArtinian(R) then error "Error: Expected element of Artinian ring";

    K := coefficientRing R;
    Z := opts.Variable;
    S := K(monoid [Z]);
    
    if (opts.Strategy === 0) then (
	--This strategy computes the minimalPolynomial by finding a minimal linear combination in powers of f
    	B := basis R;
    	n := numgens source B;
    	
    	P := map(R^1,R^(n+1),(i,j)->f^j);
    	M := last coefficients(P, Monomials=>B);
    	coeffs := sub(gens ker M,K);
   	g := (map(S^1,S^(n+1),(i,j)->S_0^j) * coeffs)_(0,0);
	
	) else if (opts.Strategy === 1) then (
      	--This strategy computes the minimalPolynomial as the kernel of the multiplication map
    	phi := map(R,S,{f});
    	g = (ker phi)_0;
	
	);
    
    if (K === QQ) then (
	d := gcd flatten entries sub(last coefficients g,QQ);
	g/d
	) else if (isField K) then (
	g/(leadCoefficient g)
	) else (
	g
	)
    )


--Function alias
univariateEliminant = method(Options=>{Strategy=>0,Variable=>"Z"})
univariateEliminant (RingElement,Ideal) := o->(g,I)->(
    minimalPolynomial(g,I,o)
    )


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
    	g := sum(n+1,i->coeffs#i*S_0^(n-i));
	g/(leadCoefficient g)
	)
    )

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
    g := sum(D+1,i->coeffs#i*S_0^(D-i));
    g/(leadCoefficient g)
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
budanFourierBound = method()
for A in {ZZ,QQ,InfiniteNumber} do 
for B in {ZZ,QQ,InfiniteNumber} do
budanFourierBound (RingElement,A,B) := ZZ => (f,a,b)->(
    if not isUnivariatePolynomial(f) then error "Error: Expected univariate polynomial.";
    if not (a<b) then error "Error: Expected non-empty interval";
    l := derivSequence f;
    variations apply(l,g->signAt(g,a)) - variations apply(l,g->signAt(g,b))
    )

budanFourierBound (RingElement) := ZZ => f->(
    budanFourierBound(f,-infinity,infinity)
    )


--Computes the Sylvester sequence of a pair (f,g)
----This isn't actually the Sylvester sequence since we divide by gcd(f,g)
sylvesterSequence = method()
sylvesterSequence (RingElement, RingElement) := List => (f,g)->(
    if (f==0 or g==0) then error "Error: Expected nonzero polynomials";
    if not (isUnivariatePolynomial(f) and isUnivariatePolynomial(g)) then error "Error: Expected univariate polynomials";

    R := ring f;
    if not isField coefficientRing R then error "Error: Expected polynomials over a field";
    if not (ring g === R) then error "Error: Polynomials should be in the same ring";
    
    --dividing out common factors
    h := gcd(f,diff(variable f,f)*g);
    f0 := sub(f/h,R);
    f1 := sub(diff(variable f,f)*g/h,R);

    Syl := new MutableList from {f0,f1};
    
    i := 1;
    while (Syl#i != 0) do (
	i = i+1;
	Syl#i = -Syl#(i-2) % Syl#(i-1)
    );
    	    
    toList Syl
    )


--Computes the difference in the number of roots of f where g is positive and where g is negative
----letting g = 1 gives the number of real roots from the Sturm sequence
sylvesterCount = method(Options=>{Multiplicity=>false})
for A in {ZZ,QQ,InfiniteNumber} do 
for B in {ZZ,QQ,InfiniteNumber} do
sylvesterCount (RingElement,RingElement,A,B) := ZZ => opts->(f, g, a, b)->(
    if not (a<b) then error "Error: Expected non-empty interval";
    l := sylvesterSequence(f,g);
    n := variations apply(l,h->signAt(h,a)) - variations apply(l,h->signAt(h,b));
    if opts.Multiplicity then (
	h := gcd(f,diff(variable f,f));
	while (first degree h > 0) do (
	    n = n + sylvesterCount(h,g,a,b);
	    h = gcd(h,diff(variable h,h))
	    )
	);
    n
    )

sylvesterCount (RingElement,RingElement) := ZZ => opts->(f,g)->(
    sylvesterCount(f,g,-infinity,infinity)
    )


--Computes the Sturm sequence of f via a Sylvester sequence
sturmSequence = method()
sturmSequence (RingElement) := List => f->(
    if (f == 0) then error "Error: Expected nonzero polynomial";
    R := ring f;
    sylvesterSequence(f,1_R)
    )


--Computes the difference in variations of the Sturm sequence at specified values
sturmCount = method(Options=>{Multiplicity=>false})
for A in {ZZ,QQ,InfiniteNumber} do
for B in {ZZ,QQ,InfiniteNumber} do
sturmCount (RingElement,A,B) := ZZ => opts->(f,a,b)->(
    R := ring f;
    sylvesterCount(f,1_R,a,b,opts)
    )

sturmCount (RingElement) := ZZ => opts->f->( 
    sturmCount(f,-infinity,infinity,opts)
    )


--Uses Sturm sequence and a bisection method to isolate real solutions to a real univariate polynomial within a tolerance
realRootIsolation = method()
for A in {ZZ,QQ} do
realRootIsolation (RingElement,A) := List => (f,r)->(
    if not r > 0 then error "Error: Expected positive integer or positive rational number";
    
    if not isUnivariatePolynomial(f) then error "Error: Expected univariate polynomial";
    R := ring f;
    
    f = sub(f/gcd(f,diff(variable f,f)),R); --makes polynomial squarefree
    
    if (sturmCount(f)>0) then (
	l := sturmSequence(f);
	
	--bound for real roots
	--Cauchy bound is usually good, but Lagrange is better in cases of coefficient blowup.
		
	(E0,C0) := coefficients f; -- get exponents and coefficients of polynomial as matrices

	C := flatten(entries(C0)); -- convert coefficients to list
	if #C == 1 then (
	    M := 0; -- if polynomial is only one term (only root is 0), return 0.
	) else (
	    C = apply(C,i -> lift(i,QQ)); --lift coefficients to QQ
	    C1 := apply(C,i -> abs(lift(i/C#0,QQ))); --divide coeffs by leading coeff, take abs, with values in QQ.
	    E1 := flatten(apply(flatten(entries(E0)), i -> degree(i))); -- convert exponents to list
	    MC := 1 + max(apply(drop(C1,1), abs)); -- Cauchy bound
	    CL := apply(drop(C1,1),drop(E1,1), (c,e) -> 2*(c^(1/(E1#0-e)))); -- Lagrange (Fujiwara) bound
	    ML := max CL;
	    if ring ML === QQ or ring ML === ZZ then ( -- want to keep result in QQ
	        ML = ML_QQ;
		) else (
	        if abs(C#0) > 1 then  ( -- if suitable, keep bound in form similar to other bound 
		        ML = ceiling(abs(C#0)*ML)/abs(C#0); -- (round up to nearest 1/leadcoeff, this is a bad rational approximation if ML was very small)
		    ) else (
		        ML = ceiling ML; -- if leading term is less than 1, the above approximation is less accurate than just taking the ceiling.
	        );
	    );
	    M = min(MC,ML); -- take the smaller of the two bounds.
	);

	L := {{-M,M}};
	midp := 0;
	v := new MutableHashTable from {M=>variations apply(l,g->signAt(g,M)),-M=>variations apply(l,g->signAt(g,-M))};
	
	while (max apply(L,I-> I#1-I#0) > r) or (max apply(L,I-> v#(I#0)-v#(I#1)) > 1) do (
	    for I in L do (
		if ((v#(I#0)-v#(I#1) == 1) and (I#1-I#0 <= r)) then (
	            L = take(L,{1,#L})|{L#0}; -- skip bisection if root is identified and bound is within interval size.
		)
		else (
		    midp = (sum I)/2;
		    v#midp = variations apply(l,g->signAt(g,midp));
		    L = drop(L,1);
		    if (v#(I#0)-v#midp > 0) then (
		        L = append(L,{I#0,midp})
		        );
	  	    if (v#midp-v#(I#1) > 0) then (
		        L = append(L,{midp,I#1})
		        );
		    )
		)
	    );
	sort L --list is ordered from smallest to largest interval, in case the while look stops after a larger interval
	) else (
	{}
	)
    )

--computes the signature of a matrix
----can also use sylvesterCount(ch,variable ch,Multiplicity=>true)
signature = method()
signature (Matrix) := ZZ => M->(
    if not (ring M === ZZ or ring M === QQ) then error "Error: Expected rational matrix";
    if M != transpose M then error "Error: Expected symmetric matrix.";
    ch := characteristicPolynomial M;
    coeffs := flatten entries sub(last coefficients ch,ring M);
    2*(variations coeffs) - rank M
    ) 
    
--Computes the trace form of f in an Artinian ring 
traceForm = method()
traceForm (RingElement,Ideal) := Matrix => (f,I)->(
    R := ring f;
    if not (ring I === R) then error "Error: Expected RingElement and Ideal in same Ring.";
    traceForm(sub(f,R/I))
    )

traceForm (RingElement) := Matrix => f->(
    R := ring f;
    if not isArtinian(R) then error "Error: Expected zero-dimensional ring.";
    B := basis R;
    K := coefficientRing R;

    mm := sub(last coefficients(f*B**B,Monomials=>B),K);
    tr := matrix {apply(first entries B, x -> trace last regularRepresentation x)};
    adjoint(tr*mm, source tr, source tr)
    )

	
--Compute the number of points of a scheme/real univariate polynomial/real polynomial system using the rank of the trace form.
traceCount = method()
traceCount (RingElement) := ZZ => f->(
    traceCount {f}
    )

traceCount (List) := ZZ => F->(
    traceCount ideal F
    )

traceCount (Ideal) := ZZ => I->(
    supp := new Array from support I;
    C := coefficientRing ring I;
    R := C(monoid supp);
    traceCount(R/sub(I,R))
    )

traceCount (QuotientRing) := ZZ => S->(
    rank traceForm(1_S)
    )


--Compute the number of real points of a scheme/real univariate polynomial/real polynomial system using the signature of the trace form.
realCount = method()
realCount (RingElement) := ZZ => f->(
    realCount {f}
    )

realCount (List) := ZZ => F->(
    realCount ideal F
    )

realCount (Ideal) := ZZ => I->(
    supp := new Array from support I;
    C := coefficientRing ring I;
    R := C(monoid supp);
    realCount(R/sub(I,R))
    )

realCount (QuotientRing) := ZZ => S->(
    signature traceForm(1_S)
    )


--Computes the Rational Univariate Representation of a zero-dimensional ideal in a polynomial ring
----output is:
------a linear functional l that separates the points of I
------a polynomial ch defining the image of the points of V(I) under the map defined by l
------a list of rational polynomials that consitite a rational inverse (on V(I)) of the map defined by l
rationalUnivariateRepresentation = method()
rationalUnivariateRepresentation (QuotientRing) := Sequence => S->(
    R := ambient S;
    I := ideal S;
    if not (isArtinian S and isPolynomialRing R) then error "Error: Expected Artinian ring as quotient of a polynomial ring";
    
    rationalUnivariateRepresentation(I)
    )

rationalUnivariateRepresentation (Ideal) := Sequence => I->(
    R := ring I;
    S := R/I;
    if not (isArtinian S and isPolynomialRing R) then error "Error: Expected I to be a zero-dimensional ideal in a polynomial ring";
    d := traceCount(S);
    
    i := 1;
    X := gens R;
    n := #X;
    while (i < n*(binomial(d,2))+2) do (
    	l := sum(X,apply(n,k->i^k),(a,b)->a*b);
	(B,m) := regularRepresentation(sub(l,S));
	ch := characteristicPolynomial(m);
	
	chbar := ch/gcd(ch,diff((support ch)_0,ch));
	chbar = sub(chbar,ring ch);
	if (first degree chbar === d) then break;
	i = i+1
	);
    
    T := ring ch;
    phi := map(S,T,{l});
    H := phi matrix{hornerSequence(chbar)};
    M := sub(matrix {gens R},S);        
    tr := matrix{apply(first entries B,x-> trace last regularRepresentation x)};

    gvCoeffs := sub(adjoint(tr*(last coefficients(M**H,Monomials=>B)),source M,source H),coefficientRing R);
    g1 := sub(diff(variable ch,ch)/gcd(diff(variable ch,ch),ch),ring ch);
    Z := matrix {apply(d,i->(ring ch)_0^(d-1-i))};
    gv := first entries ((1/g1)*sub(Z*gvCoeffs,frac ring ch));
        
    return(l,ch,gv)
    )


--Computes the Hurwitz matrix of f (of order k)
hurwitzMatrix = method()
hurwitzMatrix (RingElement) := Matrix => f->(
    d := first degree f;
    if not (d>0) then error "Error: Expected polynomial of positive degree";
    
    x := variable f;
    a := apply(d+1,i->coefficient(x^i,f));
    
    M := matrix table(d,d,(i,j)->if (0 <= d-1+i-2*j and d-1+i-2*j <= d) then a#(d-1+i-2*j) else 0);
    M
    )

hurwitzMatrix (RingElement,ZZ) := Matrix => (f,k)->(
    M := hurwitzMatrix f;
    if (k==0) then matrix{{1}} else submatrix(M,toList(0..k-1),toList(0..k-1))
    )


--Determines whether or not a univariate polynomial of degree >=1 is Hurwitz stable.
----criterion requires lead coefficient of f to be positive
isHurwitzStable = method()
isHurwitzStable (RingElement) := Boolean => f->(
    if (leadCoefficient f < 0) then f = -f;
    d := first degree f;
    all(d+1,k->det hurwitzMatrix(f,k) > 0)
    )

--------------------
---DOCUMENTATION----
--------------------

beginDocumentation()

undocumented {
    delete((sylvesterCount,RingElement,RingElement,QQ,QQ),flatten table({ZZ,QQ,InfiniteNumber},{ZZ,QQ,InfiniteNumber},(a,b)->(sylvesterCount,RingElement,RingElement,a,b))),
    delete((sturmCount,RingElement,QQ,QQ),flatten table({ZZ,QQ,InfiniteNumber},{ZZ,QQ,InfiniteNumber},(a,b)->(sturmCount,RingElement,a,b))),
    delete((budanFourierBound,RingElement,QQ,QQ),flatten table({ZZ,QQ,InfiniteNumber},{ZZ,QQ,InfiniteNumber},(a,b)->(budanFourierBound,RingElement,a,b))),
    (realRootIsolation, RingElement,ZZ)
    }

document {
	Key => RealRoots,
	Headline => "Package for exploring, counting and locating real solutions to polynomial systems",
	Acknowledgement => {"This package is an updated version the ",TT "realroots"," package by Grayson and Sottile."},
	"This package provides general tools for elimination and solving systems of polynomial equations, with a particular emphasis on real solutions."
	}

document {
	Key => {minimalPolynomial,(minimalPolynomial, RingElement),(minimalPolynomial,RingElement,Ideal),[minimalPolynomial,Strategy],[minimalPolynomial,Variable]},
	Headline => "the minimal polynomial of an element of an Artinian ring",
	Usage => "minimalPolynomial(f)
	          minimalPolynomial(g,I)",
	Inputs => {
	    RingElement => "f" => {"an element of an Artinian ring"},
	    RingElement => "g" => {"a polynomial"},
	    Ideal => "I" => {"a zero-dimensional ideal in a polynomial ring"},
	    Strategy => {"set method for computing the minimal polynomial"},
	    Variable => {"allows user to change the variable of the resulting polynomial"},
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
	        ///,
	SeeAlso => {"univariateEliminant"}
	}

document {
        Key => {univariateEliminant,(univariateEliminant,RingElement,Ideal),[univariateEliminant,Strategy],[univariateEliminant,Variable]},
        Headline => "the univariate eliminant of an element of an Artinian ring",
	Usage => "univariateEliminant(f)
	          univariateEliminant(g,I)",
	Inputs => {
	    RingElement => "f" => {"an element of an Artinian ring"},
	    RingElement => "g" => {"a polynomial"},
	    Ideal => "I" => {"a zero-dimensional ideal in the polynomial ring containing ",TT "g"},
	    Strategy => {"set method for computing the univariate eliminant"},
	    Variable => {"allows user to change the variable of the resulting polynomial"}
	    },
	Outputs => { RingElement => {"the desired univariate polynomial. See description"}},
	PARA {"This computes the univariate eliminant of a ring element ", TT "f", " in the Artinian ring ", TT "ring f", ", or the univariate eliminant of a polynomial ", TT "g", " in the Artinian ring ", TT "(ring g)/I", ".
	    When ",TT "f"," is a variable in ", TT "ring f", ", this is the eliminant with respect to that variable. This is computed by finding the minimal polynomial of the corresponding multiplication matrix."},
	EXAMPLE lines ///
	    	R = QQ[x,y]
		I = ideal(y^2 - x^2 - 1,x - y^2 + 4*y - 2)
		univariateEliminant(y,I)
	 	///,
	PARA {"We provide two examples to compute minimal polynomials given by ",TT "Strategy => 0"," (computes the kernel of ",TEX///$k[T]\to$///,TT "ring f"," by sending ", TEX///$T$///," to ",TT "f",") and ",TT "Strategy => 1", " (a minimal linear combination of powers of the input)."},
	EXAMPLE lines ///
		univariateEliminant(x,I,Strategy => 0)
	    	univariateEliminant(x,I,Strategy => 1)
	        ///,
	SeeAlso => {"minimalPolynomial"}
	}

document {
	Key => {regularRepresentation,(regularRepresentation, RingElement, Ideal), (regularRepresentation, RingElement)},
	Headline => "the regular representation of an element of an Artinian ring",
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
	PARA {"This computes the matrix of the linear map defined by multiplication by ", TT "f", " (resp. ",TT "g",") in terms of the standard basis of the finite-dimensional vector space ", TT "ring f"," (resp. ",TT "(ring g)/I",")."},
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
	    Ideal => "I"  => {"a zero-dimensional ideal in the polynomial ring containing ", TT "g"},
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
	Key => {sylvesterSequence,(sylvesterSequence, RingElement, RingElement)},
	Headline => "the Sylvester sequence of two univariate polynomials with rational coefficients",
	Usage => "sylvesterSequence(f,g)",
	Inputs => {
	    RingElement => "f" => {"a univariate polynomial with rational coefficients"},
	    RingElement => "g" => {"a univariate polynomial with rational coefficients in the same variable as ", TT"f"},
	    },
	Outputs => { List => { "the reduced Sylvester sequence of ", TT "f", " and ",TT "g"}},
	PARA {"This computes the reduced Sylvester sequence of two univariate polynomials with rational coefficients ", TT "f", " and ", TT "g", " in the same ring.
	    This begins with the Sylvester sequence ",TEX///$(f_{0},f_{1},\dots,f_{k})$///,", where ",TEX///$f_{0} = f, f_{1} = f'\cdot g$///," and for ",
	    TEX///$i\geq 1, f_{i+1} = -1\cdot$///,"remainder ",TEX///$(f_{i-1},f_{i})$///,". The last nonzero remainder ",TEX///$f_{k}$///," 
	    is a greatest common divisor of ",TEX///$f$///," and ",TEX///$g$///,". The reduced Sylvester sequence is obtained by dividing each term of 
	    the Sylvester sequence by ",TEX///$f_{k}$///,"."},
	EXAMPLE lines ///
	         R = QQ[t]
		 f = (t + 1)*(t + 2)
		 g = t + 2
		 sylvesterSequence(f,g)
	 	 ///,
	SeeAlso => {"sylvesterCount","sturmSequence"}
     	}

document {
	Key => {sylvesterCount,(sylvesterCount,RingElement,RingElement),(sylvesterCount,RingElement,RingElement,QQ,QQ)},
	Headline => "a signed count of the real roots of a univariate polynomial with rational coefficients",
	Usage => "sylvesterCount(f,g,a,b)
	          sylvesterCount(f,g)",
	Inputs => {
	    RingElement => "f" => {"a univariate polynomial with rational coefficients"},
	    RingElement => "g" => {"a univariate polynomial with rational coefficients in the same variable as ", TT"f"},
	    QQ => "a" => {"(optional) the left endpoint of the interval"},
	    QQ => "b" => {"(optional) the right endpoint of the interval"},
	    Multiplicity => {"option for computing roots with multiplicity"}
	    },
	Outputs => { ZZ => {"the difference between the number of real roots of ",TT "f"," in the interval ",TEX///$(a,b]$///," where ",TT "g","
	     is positive, and the number of real roots of ",TT "f"," in the interval ",TEX///$[a,b)$///," where ",TT "g"," is negative"}},
	PARA {"This computes the difference between the number of real roots of  ",TT "f"," in the interval ",TEX///$(a,b]$///," where ",TT "g","
	     is positive, and the number of real roots of ",TT "f"," in the interval ",TEX///$[a,b)$///," where ",TT "g"," is negative.
	     This is computed by taking the difference in variations of the reduced Sylvester sequence of ", TT "f"," and ",TT "g","."},
	EXAMPLE lines ///
	    	 R = QQ[t]
		 f = (t - 2)*(t - 1)*(t + 3)
		 g = t + 1
		 a = -5
		 b = 4
		 sylvesterCount(f,g,a,b)
	 	 ///,
	SeeAlso => {"sylvesterSequence","sturmCount"}
     	}

document {
	Key => {sturmSequence,(sturmSequence, RingElement)},
	Headline => "the Sturm sequence of a univariate polynomial with rational coefficients",
	Usage => "sturmSequence(f)",
	Inputs => {
	    RingElement => "f" => {"a univariate polynomial with rational coefficients"},
	    },
	Outputs => { List => {"the Sturm sequence of ", TT "f"}},
	PARA {"This computes the Sturm sequence of the square-free part of a univariate polynomial with rational coefficients ", TT "f","."},
	EXAMPLE lines ///
	 	 R = QQ[t]
		 f = 45 - 39*t - 34*t^2 + 38*t^3 - 11*t^4 + t^5
		 roots f
		 sturmSequence(f)
	 	 ///,
	SeeAlso => {"sturmCount","sylvesterSequence"}
     	}

document {
    	Key => {"Multiplicity(RealRoots)", [sylvesterCount, Multiplicity], [sturmCount, Multiplicity]},
	PARA {"This is an optional input for counting roots with multiplicity."}
    }

document {
    	Key => {"Multiplicity"},
	PARA {"This is a symbol for counting roots with multiplicity."}
    }

document {
	Key => {sturmCount,(sturmCount,RingElement),(sturmCount,RingElement,QQ,QQ)},
	Headline => "the number of real roots of a univariate polynomial with rational coefficients",
	Usage => "sturmCount(f,a,b)
	          sturmCount(f)",
	Inputs => {
	    RingElement => "f" => {"a univariate polynomial with rational coefficients"},
	    QQ => "a" => {"the left endpoint of the interval"},
	    QQ => "b" => {"the right endpoint of the interval"},
	    Multiplicity => {"option for computing roots with multiplicity"}
	    },
	Outputs => { ZZ => {"the number of real roots of ", TT "f"," in the interval ",TEX///$(a,b]$///}},
	PARA {"This computes the number of real roots in the interval ",TEX///$(a,b]$///,"  of a univariate polynomial ",TT "f"," with rational coefficients.
	     If ", TT "a", " and ", TT "b"," are not specified, this computes the number of real roots of ",TT "f",". This is computed by taking
	     the difference in variation of the Sturm sequence of ", TT "f","."},
	EXAMPLE lines ///
	    	 R = QQ[t]
		 f = (t - 5)*(t - 3)^2*(t - 1)*(t + 1)
		 roots f
		 sturmCount(f)
		 sturmCount(f,0,5)
		 sturmCount(f,-2,2)
		 sturmCount(f,-1,5)	       
	 	 ///,
	PARA {"In the above example, multiplicity is not counted. To include it, make the multiplicity option ",TT "true","."},
	EXAMPLE lines ///
		sturmCount(f,Multiplicity => true)
		sturmCount(f,0,5,Multiplicity => true)
		sturmCount(f,0,3,Multiplicity => true)
		///,
	PARA {"We give examples with infinite intervals."},
	EXAMPLE lines ///
	    	sturmCount(f,-infinity, 0)
		sturmCount(f,0,infinity)
		sturmCount(f,-infinity,infinity)
		///,
	SeeAlso => {"sturmSequence","sylvesterCount"}
     	}
    
document {
    	Key => {variations,(variations, List)},
	Headline => "the number of sign changes of an ordered list of numbers",
	Usage => "variations(l)",
	Inputs => {
	    List => "l" => {" of numbers"},
	    },
	Outputs => {ZZ => {"the number of sign changes in the ordered list ", TT "l" }},
	PARA {"This computes the number of sign changes in the ordered list ", TT "l","."},
	EXAMPLE lines ///
		 L = for i to 10 list random(-50,50)
		 variations(L)
	 	 ///,
     	}

document {
        Key => {realRootIsolation,(realRootIsolation, RingElement,QQ)},
	Headline => "a list that isolates the real roots of a univariate polynomial with rational coefficients",
	Usage => "realRootIsolation(f,r)",
	Inputs => {
	    RingElement => "f" => {"a univariate polynomial with rational coefficients"},
	    QQ => "r" => {"a positive rational number"},
	    },
	Outputs => {List => {"of disjoint intervals"}},
	PARA {"This computes disjoint intervals of length at most ",TT "r",", each containing exactly one real root of ",TT "f","."},
	EXAMPLE lines ///
	    	 R = QQ[t]
		 f = 45 - 39*t - 34*t^2 + 38*t^3 - 11*t^4 + t^5
		 realRootIsolation(f,1/2)
	 	 ///,
	SeeAlso => {"sturmSequence"}
     	}
    
document {
	Key => {budanFourierBound,(budanFourierBound,RingElement),(budanFourierBound,RingElement,QQ,QQ)},
	Headline => "a bound for the number of real roots with multiplicity of a univariate polynomial with rational coefficients",
	Usage => "budanFourierBound(f, a, b)
	          budanFourierBound(f)",
	Inputs => {
	    RingElement => "f" => {"a univariate polynomial with rational coefficients, where", TT " ring f ", "is not necessarily univariate"},
	    QQ => "a" => {"the left endpoint of the interval"},
	    QQ => "b" => {"the right endpoint of the interval"},
	    },
	Outputs => { ZZ => { "the bound for the number of real roots in the interval ", TT "(a,b]", ", counted with multiplicity, of a univariate polynomial ", TT "f", " with rational coefficients"}},
	PARA {"This computes the bound from the Budan-Fourier Theorem for the number of real roots in the interval ", TT "(a,b]",", counted with multiplicity, of a univariate polynomial ", TT "f", " with rational coefficients. It assumes an unspecified interval is
	      ",TEX///$(-\infty, \infty)$///,". Note that ", TT "ring f", " is allowed to be multivariate."},
	EXAMPLE lines ///
	         R = QQ[t]
		 f = 45 - 39*t - 34*t^2 + 38*t^3 - 11*t^4 + t^5
		 budanFourierBound(f)
		 g = (t + 5)*(t + 3)*(t + 1)*(t - 1)^2*(t - 4)*(t - 6)
		 budanFourierBound(g,-6,infinity)
		 budanFourierBound(g,-1,5)
	 	 ///,
	PARA {"We also provide examples when the interval includes ", TEX///$-\infty$///," or ", TEX///$\infty$///, "."},
	EXAMPLE lines ///
	         budanFourierBound(g,-infinity,0)
	         budanFourierBound(g,3,infinity)
		 budanFourierBound(g,-infinity,infinity)
		 ///
     	}
    
    
document {
	Key => {traceForm,(traceForm, RingElement),(traceForm,RingElement,Ideal)},
	Headline => "the trace symmetric form of a polynomial in an Artinian ring",
	Usage => "traceForm(f)
	          traceForm(g,I)",
	Inputs => {
	    RingElement => "f" => {"a polynomial in an Artinian ring"},
	    RingElement => "g" => {"a polynomial"},
	    Ideal => "I" => {"a zero-dimensional ideal in the polynomial ring containing ", TT "g",},
	    },
	Outputs => {Matrix => {"the symmetric matrix representing the trace symmetric form of ",TT "f"," (resp. ",TT "g",") in ",TT "ring f",
		" (resp. ",TT "(ring g)/I",")"}},
	PARA {"This computes the symmetric matrix of the trace symmetric form of ", TT "f", " (resp. ",TT "g",")
	       in terms of the standard basis of the finite-dimensional vector space ", TT "ring f"," (resp. ",TT "(ring g)/I",")."},
	EXAMPLE lines ///
		 R = QQ[x,y]
		 I = ideal(y^2 - x^2 - 1, x - y^2 + 4*y - 2)
		 S = R/I
		 f = y^2 - x^2 - x*y + 4
		 traceForm(f)
	 	 ///,
	PARA {"We can also compute the signature of the trace form of ", TT "g", ", which is the number of real points in ",TEX///$V(J)$///," where ",TT "g"," 
	   is positive minus the number of real points in ",TEX///$V(J)$///," where ",TT "g"," is negative."},
	EXAMPLE lines ///
    	    	 R = QQ[x,y]
		 J = ideal(y^2 - x^2 - 1,x - y^2 + 4*y - 2)
		 g = x + y
		 signature(traceForm(g,J))	 
	 	 ///,
	PARA {"Additionally, we show an example computing the number of points in ",TEX///$V(I)$///," using the rank of the trace form."},
	EXAMPLE lines ///
		 rank traceForm(1_R,J)	 
	 	 ///,	 
	SeeAlso => {"traceCount","realCount","signature"}
     	}
    
document {
	Key => {signature,(signature, Matrix)},
	Headline => "the signature of a symmetric matrix with rational entries",
	Usage => "signature(M)",
	Inputs => {
	    Matrix => "M" => {"a symmetric matrix with rational entries"},
	    },
	Outputs => { ZZ => {"the signature of ", TT "M"}},
	PARA {"This computes the signature of the symmetric matrix ", TT "M"," with rational entries."},
	EXAMPLE lines ///
		 signature(matrix{{1,-2,3/5},{-2,-16,9},{3/5,9,13}})
		 ///
     	}    

document {
	Key => {traceCount,(traceCount, QuotientRing), (traceCount, RingElement), (traceCount, Ideal),(traceCount, List)},
        Headline => "the degree of the reduced scheme of an Artinian ring",
	Usage => "traceCount(S)
	          traceCount(f)
		  traceCount(I)
		  traceCount(l)",
	Inputs => {
	    QuotientRing => "S" => {"an Artinian ring over any field"},
	    RingElement => "f" => {"a univariate polynomial"},
	    Ideal => "I" => {"a zero-dimensional ideal in a polynomial ring"},
	    List => "l" => {"a system of polynomials with a finite number of solutions"},
	    },
	Outputs => { ZZ => {"the degree of the reduced scheme of an Artinian ring"}},
	PARA {"This computes the degree of the reduced scheme of an Artinian ring ",TT "S" ," over any field, which is the
	    number of distinct points of Spec ", TT "S", ", after a base change to an algebraically-closed field."},
	PARA {"This counts the number of roots of a univariate polynomial ",TT "f","."},
	EXAMPLE lines ///
 	         R = QQ[x,y]
		 f = (x^2 + 1)*(x + 1)*(x - 2)^2
		 traceCount(f)
		 ///,
	PARA {"If ",TT "I"," is a zero-dimensional ideal of a polynomial ring, this computes the number of distinct points of Spec ",TT "(ring I)/I","."},
	EXAMPLE lines ///
		 I = ideal(5 - 3*x^2 - 3*y^2 + x^2*y^2, 1 + 2*x*y - 4*x*y^2 + 3*x^2*y)
		 traceCount(I)
		 ///,
	PARA {"If ", TT "l", " is a system of rational polynomials, then this computes the number of distinct solutions."},
	EXAMPLE lines ///
	    	 l = {y^2 - x^2 - 1,x - y^2 + 4*y - 2}
		 traceCount(l)
	    	///,
--	EXAMPLE lines ///
--		 J = ideal(y^2 - x^2 - 1,x - y^2 + 4*y - 2)
--		 S = R/J
--		 traceCount(S)
--	 	 ///,
	SeeAlso => {"traceForm","realCount"}
     	}
    
document {
	Key => {realCount,(realCount, QuotientRing), (realCount, RingElement), (realCount, Ideal), (realCount, List)},
        Headline => "the number of real points of the spectrum of an Artinian ring (of characteristic 0)",
	Usage => "realCount(S)",
	Inputs => {
	    QuotientRing => "S" => {"an Artinian ring"},
	    RingElement => "f" => {"a univariate polynomial"},
	    Ideal => "I" => {"a zero-dimensional ideal in the polynomial ring given by its variables"},
	    List => "l" => {"generators of a zero-dimensional ideal"},
	    },
	Outputs => { ZZ => {"the number of distinct real points of Spec ", TT "S",", not counting multiplicity"}},
	PARA {"This computes the number of distinct real points of Spec ", TT "S", ", not counting multiplicity.
	       If ", TT "f", " is a univariate polynomial (resp. if ",TT "I"," is a zero-dimensional ideal), this computes the number of real points of Spec ",TT "R/(f)", " (resp. ",TT "R/I","),
	       where ",TT "R"," is the ring generated by the variables that appear in ",TT "f"," (resp. in ",TT "I",") to allow for elimination of variables. If ", TT "l", " is a list of generators of an ideal ",TT "I",", then this computes the number of real points of Spec ",TT "(R/I)"," as before."},
    	PARA {"If ",TT "f"," is a univariate polynomial, then this counts its number of real roots."},
	EXAMPLE lines ///
 	         R = QQ[x,y]
		 f = (x^2 + 1)*(x + 1)*(x - 2)^2
		 realCount(f)
		 ///,
	PARA {"If ",TT "I"," is a zero-dimensional ideal, this computes the number of distinct real points of Spec ",TT "R/I","."},
	EXAMPLE lines ///
		 I = ideal(5 - 3*x^2 - 3*y^2 + x^2*y^2, 1 + 2*x*y - 4*x*y^2 + 3*x^2*y)
		 realCount(I)
		 ///,
	PARA {"If ", TT "l", " is a system of rational polynomials, then this computes the number of distinct real solutions."},
	EXAMPLE lines ///
	    	 l = {y^2 - x^2 - 1,x - y^2 + 4*y - 2}
		 realCount(l)
	    	///,
--	EXAMPLE lines ///
--		 J = ideal(y^2 - x^2 - 1,x - y^2 + 4*y - 2)
--		 S = R/J
--		 realCount(S)
--	 	 ///,
	SeeAlso => {"traceForm","traceCount"}
     	}
    
document {
        Key => {rationalUnivariateRepresentation, (rationalUnivariateRepresentation, Ideal),(rationalUnivariateRepresentation, QuotientRing)},
	Headline => "the rational univariate representation of a zero-dimensional ideal in a polynomial ring",
	Usage => "rationalUnivariateRepresentation(I)
	          rationalUnivariateRepresentation(S)",
	Inputs => {
	    Ideal => "I" => {"a zero-dimensional ideal in a polynomial ring"},
	    QuotientRing => "S" => {"an Artinian ring as a quotient of a polynomial ring"},
	    },
	Outputs => {List => {"the rational univariate representation of ",TT "I", " or the rational univariate representation of ", TT "S"}},
	PARA{"This computes the rational univariate representation of a zero-dimensional ideal in a polynomial ring."},
	
	EXAMPLE lines ///
	R = QQ[x,y]
	I = ideal(x*y - 1,2*x - y + 3)
	rationalUnivariateRepresentation(I)
	rationalUnivariateRepresentation(R/I)
	///
    }

document {
	Key => {hurwitzMatrix,(hurwitzMatrix,RingElement),(hurwitzMatrix, RingElement, ZZ)},
	Headline => "the principal submatrix of the Hurwitz matrix of a univariate polynomial",
	Usage => "hurwitzMatrix(f,k)",
	Inputs => {
	    RingElement => "f" => {"a univariate polynomial of degree ",TEX///$n$///," with rational coefficients"},
	    ZZ => "k" => {"a nonnegative integer at most ",TEX///$n$///},
	    },
	Outputs => {Matrix => {"the principal ",TEX///$k\times k$///, " submatrix ",TEX///$H_{k}$///}},
	PARA{"This computes the principal ",TEX///$k\times k$///, " submatrix ",TEX///$H_{k}$///, " of the Hurwitz matrix ",TEX///$H$///," of a univariate polynomial ", TT "f"," of degree ",TEX///$n > 0$///," with rational coefficients whose leading coefficient is positive.
	    Note that ",TT "ring f" ," is allowed to be multivariate."},
	
	EXAMPLE lines ///
	    	R = QQ[x]
	        f = 3*x^4 - 7*x^3 + 5*x - 7
		hurwitzMatrix(f) 
		hurwitzMatrix(f,4)
	        hurwitzMatrix(f,3)	      
	 	 ///,
	PARA{"We can also use multiple variables to represent unknown coefficients. Note that we create another ring ",TT "S"," so 
	    that ", TT "x", " and ", TT "y"," are not considered variables in the same ring and so confuse the monomials ", TEX///$x$///, " or ",TEX///$y$///,
	    " with ",TEX///$xy$///,"."},
	EXAMPLE lines ///
	        S = R[y]
		g = y^3 + 2*y^2 + y - x + 1
		hurwitzMatrix(g,3)
		hurwitzMatrix(g,2)
		hurwitzMatrix(g,1)
		 ///,
	SeeAlso => {"isHurwitzStable"} 
	}   

document {
	Key => {isHurwitzStable,(isHurwitzStable, RingElement)},
	Headline => "determines if a univariate polynomial with rational coefficients is Hurwitz-stable",
	Usage => "isHurwitzStable(f)",
	Inputs => {RingElement => "f" => {"a univariate polynomial with rational coefficients"}},
	Outputs => { Boolean => { "the Hurwitz stability of the polynomial ", TT "f"}},
	PARA {"A real univariate polynomial is Hurwitz-stable if all its roots have negative real parts. This method determines the Hurwitz stability of a univariate polynomial ", TT "f", " of degree ",TEX///$n>0$///," with rational coefficients whose leading coefficient is positive. 
	    Note that ", TT "ring f"," is allowed to be multivariate."},
	EXAMPLE lines ///
	    	R = QQ[x]
            	f = 3*x^4 - 7*x^3 + 5*x - 7 
		g = x^2 + 10*x + 21
		isHurwitzStable(f)
		isHurwitzStable(g)	      
	 	 ///,
	SeeAlso => {"hurwitzMatrix"}	 
     	}

TEST ///
    R = QQ[x,y];
    F = {y^2 - x^2 - 1,x - y^2 + 4*y - 2};
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
    R = QQ[x,y];
    M = matrix{{2,1},{1,-1}};
    c1 =  characteristicPolynomial(M);
    G = ring c1;
    ans = Z^2 - Z - 3;
    assert(c1 == ans);
    
    I = ideal(y^2 - x^2 - 1,x - y^2 + 4*y - 2);
    c2 = characteristicPolynomial(y,I);
    G = ring c2;
    assert(c2 == Z^4 - 8*Z^3 + 19*Z^2 - 16*Z + 5);
    
    S = R/I;
    c3 = characteristicPolynomial(y)
    G = ring c3;
    assert(c3 == Z^4 - 8*Z^3 + 19*Z^2 - 16*Z + 5);
    
    c4 = characteristicPolynomial(M,Variable => "x");
    G = ring c4;
    assert(c4 == x^2 - x - 3);
    /// 
   
TEST ///
    c1 = {4, 5, -1, -11, 13, -9, 8};
    c2 = {9, 0, 1, 0, -1, -2, 11, 0, 14};
    assert(variations(c1) == 4);
    assert(variations(c2) == 2);
    ///

TEST ///
    R = QQ[t];
    f = (t + 1)*(t + 2);
    g = (t + 2);
    assert(sylvesterSequence(f,g) == {t + 1, 2*t+3,1/2,0});
    
    f =  45 - 39*t - 34*t^2 + 38*t^3 - 11*t^4 + t^5;
    assert(sturmSequence(f) == {t^4 - 8*t^3 + 14*t^2 + 8*t - 15, 5*t^3 - 29*t^2 + 27*t + 13, 104/25*t^2 - 432/25*t + 232/25, 3100/169*t - 5300/169, 194688/24025, 0});
    ///

TEST ///
    R = QQ[t];
    f = (t - 4)*(t - 1)^2*(t + 1)*(t + 3)*(t + 5)*(t - 6);--roots at 6, 4, 1 (mult 2), -1, -3, -5
    g = (2*t - 1)*(3*t + 8)*(t - 9)*(6*t + 1);--rational roots at -8/3, -1/6, 1/2, 9
    p = (t - 5)*(t - 2)*(t + 3)*(t + 4)*(t - 8)*(t + 13);--roots at -13, -4, -3, 2, 5, 8
    assert(budanFourierBound(f) == 7);
    assert(budanFourierBound(g) == 4);
    assert(budanFourierBound(p) == 6);
    assert(budanFourierBound(g,-infinity,0) == 2);
    assert(budanFourierBound(g,-1,infinity) == 3);
   
    f' = 45 - 39*t - 34*t^2 + 38*t^3 - 11*t^4 + t^5;
    assert(budanFourierBound(f') == 5);
    g' = (t + 5)*(t + 3)*(t + 1)*(t - 1)^2*(t - 4)*(t - 6);
    assert(budanFourierBound(g',-6,infinity) == 7);
    assert(budanFourierBound(g',-1,5) == 3);
    assert(budanFourierBound(g',-infinity,0) == 3);
    assert(budanFourierBound(g',3,infinity) == 2);
    assert(budanFourierBound(g',-infinity,infinity) == 7);
    
    assert(sturmCount(f) == 6);
    assert(sturmCount(f,-6,0) == 3);
    assert(sturmCount(f,-1,10) == 3);
    assert(sturmCount(f,Multiplicity => true) == 7);
    assert(sturmCount(f,-10,5,Multiplicity => true) == 6);
    assert(sturmCount(f,0,6,Multiplicity => true) == 4);
    
    assert(sturmCount(g) == 4);
    assert(sturmCount(g,-3,1) == 3);
    assert(sturmCount(g,0,10) == 2);
    
    assert(sturmCount(p) == 6);
    assert(sturmCount(p,-15,0) == 3);
    assert(sturmCount(p,2,10) == 2);
    
    h = (t - 5)*(t - 3)^2*(t - 1)*(t + 1);
    assert(sturmCount(h) == 4);
    assert(sturmCount(h,0,5) == 3);
    assert(sturmCount(h,-2,2) == 2);
    assert(sturmCount(h,-1,5) == 3);
    assert(sturmCount(h,Multiplicity => true) == 5);
    assert(sturmCount(h,0,5,Multiplicity => true) == 4);
    assert(sturmCount(h,0,3,Multiplicity => true) == 3);
    assert(sturmCount(h,-infinity, 0) == 1);
    assert(sturmCount(h,0,infinity) == 3);
    assert(sturmCount(h,-infinity,infinity) == 4);
    ///
    
TEST ///
    R = QQ[t];
    f = (t - 2)*(t - 1)*(t + 3);
    g = t + 1;
    assert(sylvesterCount(f,g,-5,4) == 1);
    h = (t - 4)*(t - 1)^2*(t + 1)*(t + 3)*(t + 5)*(t - 6);
    p = t + 5;
    assert(sylvesterCount(h,p,-10,10,Multiplicity=>true) == 6);
    assert(sylvesterCount(h,p,0,10) == 3);
    ///
    
TEST ///
    R = QQ[t];
    f = (t - 1)^2*(t + 3)*(t + 5)*(t - 6);
    assert(realRootIsolation(f,1/2) == {{-21/4, -39/8},{-27/8, -3},{3/4, 9/8},{45/8, 6}});
	
    g = 45 - 39*t - 34*t^2 + 38*t^3 - 11*t^4 + t^5;
    assert(realRootIsolation(g,1/2) == {{-3/2, -1}, {1/2, 1}, {5/2, 3}, {9/2, 5}});
    ///    
    
TEST ///
    R = QQ[x,y];
    I = ideal(y^2 - x^2 - 1, x - y^2 + 4*y - 2);
    S = R/I;
    f = y^2 - x^2 - x*y + 4;
    assert(flatten entries traceForm(f) == {4, -86, -340, -42, -86, -266, -1262, -340, -340, -1262, -5884, -1454, -42, -340, -1454, -262});
    
    R = QQ[x,y];
    J = ideal(y^2 - x^2 - 1,x - y^2 + 4*y - 2);
    g = x + y;
    assert(signature(traceForm(g, J)) == 2);
    assert(rank traceForm(1_R,J) == 4);
    ///
    
TEST ///
    assert(signature(matrix{{1,-2,3/5},{-2,-16,9},{3/5,9,13}}) == 1);
    ///

TEST ///
    R = QQ[x,y];
    f = (x^2 + 1)*(x + 1)*(x - 2)^2;
    assert(traceCount(f) == 4); 
    I = ideal(5 - 3*x^2 - 3*y^2 + x^2*y^2, 1 + 2*x*y - 4*x*y^2 + 3*x^2*y);
    assert(traceCount(I) == 8);
    l = {y^2 - x^2 - 1,x - y^2 + 4*y - 2};
    assert(traceCount(l) == 4);
    ///
    
TEST ///
     R = QQ[x,y];
     f = (x^2 + 1)*(x + 1)*(x - 2)^2;
     assert(realCount(f) == 2); 
     I = ideal(5 - 3*x^2 - 3*y^2 + x^2*y^2, 1 + 2*x*y - 4*x*y^2 + 3*x^2*y)
     assert(realCount(I) == 4);
     l = {y^2 - x^2 - 1,x - y^2 + 4*y - 2};
     assert(realCount(l) == 2);
    ///
    
TEST ///
     R = QQ[x];
     f = 3*x^4 - 7*x^3 + 5*x - 7;
     assert(hurwitzMatrix(f,4) == sub(matrix{{-7,5,0,0},{3,0,-7,0},{0,-7,5,0},{0,3,0,-7}},QQ));
     assert(hurwitzMatrix(f,3) == sub( matrix{{-7,5,0},{3,0,-7},{0,-7,5}},QQ));
     assert(isHurwitzStable(f) == false);
     g = x^2 + 10*x + 21;
     assert(isHurwitzStable(g) == true);
     
     S = R[y];
     h = y^3 + 2*y^2 + y - x + 1;
     assert(hurwitzMatrix(h,3) == matrix{{2, -x + 1, 0},{1, 1, 0},{0, 2, -x + 1}});
     assert(hurwitzMatrix(h,2) == matrix{{2, -x + 1}, {1, 1}});
     assert(hurwitzMatrix(h,1) == matrix{{2_R}});
     ///

TEST ///
     R = QQ[x,y];
     I = ideal(x*y - 1,2*x - y + 3);
     S = R/I
     d = traceCount(S)
          
     (l,ch,ph) = rationalUnivariateRepresentation(I);
     use ring ch;
     assert((l,ch,ph) == (x + y, Z^2 - 3/2*Z - 9, {(-3*Z + 15)/(4*Z - 3), (6*Z + 21)/(4*Z - 3)}));
     ///	 
    
end






