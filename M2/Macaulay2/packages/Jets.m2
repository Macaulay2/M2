--------------------------------------------------------------------------------
-- Copyright 2021  Federico Galetto, Nicholas Iammarino
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
-- details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program.  If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------

newPackage(
     "Jets",
     Version => "1.0",
     Date => "June 29, 2021",
     AuxiliaryFiles => true,
     Authors => {
	 {
	     Name => "Federico Galetto",
	     Email => "galetto.federico@gmail.com",
	     HomePage => "http://math.galetto.org"
	     },
	 {
	     Name=> "Nicholas Iammarino",
	     Email=> "nickiammarino@gmail.com"
	     }
	   },
     Headline => "compute jets of various algebraic, geometric and combinatorial objects",
     PackageImports => {"SimpleDoc","EdgeIdeals"},
     PackageExports => {"EdgeIdeals"},
     DebuggingMode => true
     )



importFrom(MinimalPrimes, {"radical"});

export {
    "JJ",
    "jets",
    "jetsMaxOrder",
    "jetsBase",
    "jetsRing",
    "projet",
    "jet",
    "jetsMatrix",
    "jetsRadical",
    "jetsProjection",
    "jetsInfo",
    "principalComponent",
    "Saturate"
    }

jetsOptions= {
    Projective=> false
-- these are set up in case one needs to pass these options
-- to jets of a RingMap
--    DegreeMap=> null,
--    DegreeLift=> null
    };

---------------------------------------------------------------------------
--helpers------------------------------------------------------------------
---------------------------------------------------------------------------
--create new-tier variables for jets ring
--by appending the order n as a string to the variable names
jetsVariables= (n,R) -> (
    symList:= apply(gens R, baseName);
    nString:= toString n;
    varNames:=
        for s in symList list (
	    if instance(s,IndexedVariable) then (
	        name:= separate("_", toString s);
	        name#0|nString|"_"|name#1
            ) else (
	        toString s|nString
	    )
        );
    varNames= apply(varNames,value)
    )

--generate degree list for order n jets variables
--this is used to create the rings of projective jets
degGenerator= (n,R) -> (
    for d in degrees R list (
	for l in d list n
	)
    )

--generate degrees/map for truncation ring in ideal calculation
jetsDegrees= jetsOptions >> o -> R -> (
    Tdegrees:= null;
    degreeMap:= null;
    
    if o.Projective then (
	Tdegrees= -1* {degree R_0};
	degreeMap= d -> degree 1_R;
	) else (
	Tdegrees= {degree 1_R};
	degreeMap= identity;
	);
    return (Tdegrees, degreeMap);
    ) 


--------------------------------------------------------------------------
--method functions--------------------------------------------------------
--------------------------------------------------------------------------


--Jets (Main Method)------------------------------------------------------

jets= method(Options=>jetsOptions);

jets(ZZ,PolynomialRing):= PolynomialRing => o -> (n,R) -> (
    if n<0 then error("jets order must be a non-negative integer");
    if not isCommutative R then error("jets method does not support noncommutative rings");
    
    --name to assign "storage" hashtable to be cached in the base ring
    typeName:= if o.Projective then (projet) else (jet);
    jetDegs:= null;--initialize degree list for jets variables

    if not R#? typeName then (
	jetDegs= if o.Projective then degGenerator(0, R) else degrees R;
	R#typeName= new CacheTable from {
	    (symbol jetsMaxOrder)=> 0,
	    (symbol jetsRing)=> coefficientRing R[jetsVariables(0,R), 
		                                 Join=> false,
					 	 Degrees=> jetDegs],
	    }
	);
    m:= R#typeName#jetsMaxOrder;
    S:= R#typeName#jetsRing;
    
    --build jet ring tower incrementally up to order n
    if n>m then (
	for i from m+1 to n do(
    	    jetDegs= if o.Projective then degGenerator(i,R) else degrees R; 
	    S= S[jetsVariables(i,R), 
		Join=> false, 
		Degrees=> jetDegs];
            );
     	R#typeName#jetsMaxOrder= n;
	R#typeName#jetsRing= S;
	) else if m>n then (
	for i from 0 to m-n-1 do (
	    S= coefficientRing S;
	    )
	);
    
    S#jetsInfo= new CacheTable from {
	(symbol jetsBase)=> R,
	(symbol Projective)=> o.Projective
	}; 
    return S;
    )

jets(ZZ,Ideal):= Ideal => o -> (n,I) -> (
    if n<0 then error("jets order must be a non-negative integer");

    R:= ring I;
    S:= null;--initializes jets ring
    t:= local t;--initializes truncation variable
    
    typeName:= if o.Projective then (projet) else (jet);
    
    if not I.cache#? typeName then (
	S= jets(0,R, Projective=> o.Projective);
	I.cache#typeName= new CacheTable from {
	    (symbol jetsMaxOrder)=> 0,
	    (symbol jetsMatrix)=> (map(S,R,vars S)) gens I
	    };
    	);
   
    m:= I.cache#typeName#jetsMaxOrder;
    
    --calculate higher order entries if needed
    if n>m then (
	S= jets(n,R, Projective=> o.Projective);
    	(Tdegrees, degreeMap):= jetsDegrees (R, Projective=> o.Projective);
	T:= S[t, Degrees=> Tdegrees, Join=> false]/(ideal(t^(n+1)));

	--a row matrix of substitution polynomials in t with coefficients
	--in the jets ring. Calculated incrementally from variables of each
	--level of the tower.
	tempS:= S;
	Tpolys:= sum join(
	    (for i from 0 to n-1 list(
		    promote(matrix t^(n-i),T) * vars tempS
		    ) do (
		    tempS= coefficientRing tempS)),
	    {promote (matrix t^0,T) * vars tempS}
	    );
	
    	phi:= map(T,R,Tpolys,DegreeMap=> degreeMap);

	--a list of generators for I is obtained to avoid dropping/repeating
	geners:= I_*;
    	--condition determining if all generators of the ideal are constants
	constCond:= all(geners,isConstant);
    	--add dummy generator to avoid loss of zeros
	gensI:= if constCond then matrix{geners|{R_0}} else matrix{geners};
    	(d,c):= coefficients(phi gensI);
    	--remove dummy generators if necessary
	if constCond then (
		L:= entries c;
		c= matrix (for l in L list drop(l,-1));
		);

	resultMatrix:= lift(c,S);

	--update value in ideal cache
	I.cache#typeName#jetsMatrix= resultMatrix;
	I.cache#typeName#jetsMaxOrder= n;
	m=n;
	);
   
    --retrieve ideal of appropriate order
    JMatrix:= I.cache#typeName#jetsMatrix; 
    if zero JMatrix then return ideal(0_(jets(n,R)));
    f:= map(jets(n,R,Projective=> o.Projective),jets(m,R, Projective=> o.Projective));
    J:= f ideal (JMatrix^{m-n..m});

    J.cache#jetsInfo= new CacheTable from {
	jetsBase=> I,
	Projective=> o.Projective
	};
    
    return J;
    )

jets(ZZ,QuotientRing):= QuotientRing => o -> (n,R) -> (
    if n<0 then error("jets order must be a non-negative integer");
    splitQuotient:= presentation R;
    ambientRing:= ring splitQuotient;
    base:= null; --jets ring to be used in quotient
    modI:= null; --jets ideal to be used in quotient
    Q:= null; --variable to store quotient ring
    
    typeName:= if o.Projective then (projet) else (jet);
    if not R#? typeName then (
	base= jets(0, ambientRing, Projective=> o.Projective);
	modI= jets(0, ideal(splitQuotient), Projective=> o.Projective);
	R#typeName= new CacheTable from {
	    (symbol jetsRing)=> new CacheTable from {
		0 => base/modI
		},
	    };
	);
    
    --form the jets of a quotient ring by taking the quotients of a jets
    --ring and a jets ideal.  Each order of the quotient is stored in a
    --cache table with the integer value of the order as the key    
    if R#typeName#jetsRing#? n then (
	Q= R#typeName#jetsRing#n;
	) else (
	base= jets(n, ambientRing, Projective=> o.Projective);
	modI= jets(n, ideal(splitQuotient), Projective=> o.Projective);
	Q= base/modI;
	R#typeName#jetsRing#n= Q;
	Q#jetsInfo= new CacheTable from {
    	    jetsBase=> R,
       	    Projective=> o.Projective
	    }
	);
    
    return Q;
    )


jets(ZZ,RingMap):= RingMap => o -> (n,phi) -> (
    if n<0 then error("jets order must be a non-negative integer");
    I:= ideal(phi.matrix);
    typeName:= if o.Projective then (projet) else (jet);
    
    -- check whether jets have been calculated for this map
    if (not phi.cache#? typeName) then (
	jets(0,I, Projective=> o.Projective);
	phi.cache#typeName= new CacheTable from {
	    (symbol jetsMaxOrder)=> 0,
    	    (symbol jetsMatrix)=> (map(jets(0,phi.target, Projective=> o.Projective),
		    	    	       jets(0,phi.source, Projective=> o.Projective),
		                       I.cache#typeName#jetsMatrix)).matrix
	    };
	);

    JR:= jets(n,phi.source, Projective=> o.Projective);
    JS:= jets(n,phi.target, Projective=> o.Projective);
    targets:= null;
    
    --check whether lower order jets have already been calculated
    m:= phi.cache#typeName#jetsMaxOrder;
    if m < n then (
	jets(n,I, Projective=> o.Projective);
    	targets= (I.cache#typeName#jetsMatrix);	
	phi.cache#typeName#jetsMaxOrder= n;
	phi.cache#typeName#jetsMatrix= targets;
    	) else (
    	targets= phi.cache#typeName#jetsMatrix^{m-n..m};
	--need to lift 'targets' to jets of order m-n
	targets=lift(targets,JS);
	);

--DegreeMap and DegreeLift options are set up but may not work as expected
    psi:= map(JS,JR,
	flatten transpose targets
--	,DegreeLift=> degreeLift,
--	DegreeMap=> degreeMap
    );

    psi.cache#jetsInfo= new CacheTable from {
    	jetsBase=> phi,
    	Projective=> o.Projective
    	};	  
    
    return psi;
   )

jets(ZZ,Graph):= Graph => o -> (n,G) -> (
    if n<0 then error("jets order must be a non-negative integer");
    --get the list of edges of the jets of the (hyper)graph
    --ring is flattened because graphs don't play well with towers of rings
    E := (flattenRing(jetsRadical(n,edgeIdeal G),Result=>1)) / support;
    --create graph
    graph E
    )

jets(ZZ,HyperGraph):= HyperGraph => o -> (n,G) -> (
    if n<0 then error("jets order must be a non-negative integer");
    --get the list of edges of the jets of the (hyper)graph
    --ring is flattened because graphs don't play well with towers of rings
    E := (flattenRing(jetsRadical(n,edgeIdeal G),Result=>1)) / support;
    --create hypergraph
    hyperGraph E
    )

jets(ZZ,AffineVariety):= o -> (n,V) -> (
    if n<0 then error("jets order must be a non-negative integer");
    R:= ring V;
    JR:= jets(n,R,Projective=> o.Projective);
    if o.Projective then return Proj JR else return Spec JR;
    )

---Secondary Methods--------------------------------------------------

--to potentially reduce computation time for monomial jet ideals  
--(see documentation)
jetsRadical= method(TypicalValue=>Ideal);

jetsRadical(ZZ,Ideal):= (n,I) -> (

    if n<0 then error("jets order must be a non-negative integer");
    
    if isMonomialIdeal I then (
	baseIdeal:= jets(n,I);
	R:= ring I;
	gensList:= flatten entries gens baseIdeal;
	termList:= apply(gensList, t-> terms(coefficientRing R, t));
	squarefreeGens:= apply(apply(flatten termList, support),product);
	ideal(squarefreeGens)
	) else (
	radical jets(n,I)
	)
    )


--to create a map sending elements of a jets ring to a jets ring of
--higher order
jetsProjection= method(Options=>jetsOptions,TypicalValue=>RingMap);

jetsProjection(ZZ,ZZ,PolynomialRing):=
jetsProjection(ZZ,ZZ,QuotientRing):= o -> (t,s,R) -> (

    if t < s then error("first argument must be less than or equal to the second");    
    if t<0 or s<0 then error("jets orders must be non-negative integers");

    (map(jets(t,R,Projective=> o.Projective),jets(s,R,Projective=> o.Projective)))
    ) 

--scripted functor for jets
--this modeled after the code for Tor
--if new jets methods are added, this will automatically work
JJ = new ScriptedFunctor from {
     subscript => (
	  i -> new ScriptedFunctor from {
	       argument => (X -> (
	       	    	 jetsOptions >> o -> Y -> (
		    	      f := lookup(jets,class i,class Y);
		    	      if f === null then error "no method available"
		    	      else (f o)(i,Y)
			      )
	       	    	 ) X
	       	    )
	       }
	  )
     }
 
--compute an ideal whose vanishing locus is the
--principal component of the jets of an ideal
principalComponent = method(Options=>{Saturate=>true},TypicalValue=>Ideal)
principalComponent(ZZ,Ideal) := o -> (n,I) -> (
    if n<0 then error("jets order must be a non-negative integer");
    -- compute jets of I
    JI := jets(n,I);
    -- get the jets projection
    R := ring I;
    p := jetsProjection(n,0,R);
    -- identify original ambient ring with 0-jets
    i := map(source p,R,vars source p);
    --compute the singular locus of I
    --map it to the zero jets via the map i
    --then to the n jets via the map p
    sing := p(i(ideal singularLocus I));
    --default is to saturate JI wrt sing
    if o.Saturate then (
    	saturate(JI,sing)
	)
    --if JI is radical, colon is enough
    else (
	JI:sing
	)
    )

beginDocumentation()
----------------------------------------------------------------------
-- TESTS
----------------------------------------------------------------------
TEST ///
    R= QQ[x,y,z];
    assert(degrees jets(2,R)==={{1}, {1}, {1}})
    assert(degrees jets(2,R,Projective=> true) === {{2}, {2}, {2}})
    I=ideal(y-x^2,z-x^3);
    assert(not(isHomogeneous jets(2,I)))
    assert(isHomogeneous jets(2,I,Projective=>true))
///

--for non uniform degrees
TEST ///
    R= QQ[x,y,z, Degrees=> {2,3,1}];
    assert(degrees jets(2,R)==={{2}, {3}, {1}})
    assert(degrees jets(2,R,Projective=> true) === {{2}, {2}, {2}})
    I= ideal(x*y, x*z^2);
    J= ideal(x^3-y*z^3, y+x*z);
    assert(isHomogeneous jets(2,I))
    assert(isHomogeneous jets(2,I,Projective=>true))
    assert(isHomogeneous jets(2,J))
    assert(isHomogeneous jets(2,J,Projective=>true))
    X= radical jets(2,I);
    Y= jetsRadical(2,I);
    assert(X == Y)
    assert(mingens X === mingens Y);
///

TEST ///
    R=QQ[x,y, Degrees=> {2,3}];
    S=QQ[a,b,c, Degrees=> {1,1,2}]
    phi= map(S,R, {a^2 + c, b*c});
    f= jets(2,phi);
    testx= c2+2*a0*a2+a1^2;
    testy= b0*c2+c0*b2+b1*c1;
    assert(f x2===testx)
    assert(f y2===testy)
    assert(isHomogeneous jets(3,phi))
    assert(isHomogeneous jets(3,phi,Projective=>true))
///

--for ideals with constant generators
TEST ///
    R=QQ[x]
    I0= ideal(2_R)
    Ftest0=jets(2,I0)
    assert(Ftest0==jets(2,R))
    I1= ideal(2_R,x)
    Ftest1=jets(2,I1)
    assert(Ftest1== jets(2,R))
    S=ZZ[x]
    J0= ideal(2_S)
    Ztest0= jets(2,J0)
    assert(Ztest0!=jets(2,S))
    J1= ideal(2_S,x) 
    Ztest1=jets(2,J1)
    assert(Ztest1!=jets(2,S))
///

--for principal component
TEST ///
    R=QQ[x,y]
    I=ideal(y^2-x^3)
    PC=principalComponent(2,I)
    P=primaryDecomposition jets(2,I)
    C=first select(P,c -> degree c==6)
    assert(PC==C)
///

--for quotients and varieties
TEST ///
    R=QQ[x,y]
    I= ideal(y^2,x^3)
    Q=R/I
    JR= jets(2,R)
    JI= jets(2,I)
    JQ= jets(2,Q)
    assert(JR===ambient JQ)
    assert(JI===ideal JQ)
    assert(presentation (JR/JI)===presentation JQ)
    V= Spec Q
    JV= jets(2,V)
    assert(ring JV===JQ)   
///

--for graphs
TEST ///
    R=QQ[x,y,z]
    G= graph(R,{{x,y},{y,z},{x,z}})
    JG= jets(1,G)
    JR= jets(1,R)
    use ring JG
    test= {{x0,y0},{x0,z0},{y0,z0},{x1,y0},{x1,z0},{y1,x0},{y1,z0},{z1,x0},{z1,y0}}
    assert((set edges JG) === (set test))
///

--for projections
TEST ///
    R=QQ[x,y,z]
    I= ideal(y-x^2,z-x^3)
    JI= jets(1,I)
    p= jetsProjection(3,1,R)
    assert(ring p JI === jets(3,R))
///
----------------------------------------------------------------------
-- Documentation
----------------------------------------------------------------------
doc ///

Node
    Key
    	Jets
    Headline
    	compute jets of various algebraic, geometric and combinatorial objects
    Description
    	Text
	    This package enables computations with jet functors.
	    It introduces the @TO jets@ method to compute jets of
	    polynomial rings, ideals, quotients, ring homomorphisms,
	    and varieties.
	    The construction of jets follows an algebraic procedure
	    discussed in several sources, including the first three
	    references below.
	    
	    Additional features include an alternative algorithm to compute
	    the radical of jets of monomial ideals, a function
	    to construct jets of graphs, a method for principal components of jets,
	    and an option for jets with "projective" gradings.
    References
    	@arXiv("math/0612862","L. Ein and M. Mustaţă,
    		Jet schemes and singularities.")@
		
    	@arXiv("2104.08933","F. Galetto, E. Helmick, and M. Walsh,
    		Jet graphs.")@
		
    	@HREF("https://doi.org/10.1080/00927870500454927",
	    "R.A. Goward and K.E. Smith,
	    The jet scheme of a monomial scheme.")@

    	@arXiv("math/0407113","P. Vojta,
	    	Jets via Hasse-Schmidt Derivations.")@
    Subnodes
    	:Package methods
    	jets
	jetsProjection
	jetsRadical
	principalComponent
	:Examples from the literature
	"Example 1"
	"Example 2"
	"Example 3"
	:Technical information
	"Storing Computations"

Node
    Key
    	jets
    Headline
    	compute the jets of an object
    Subnodes	
	(jets,ZZ,PolynomialRing)
	(jets,ZZ,Ideal)
	(jets,ZZ,QuotientRing)
	(jets,ZZ,RingMap)
	(jets,ZZ,Graph)
    	(jets,ZZ,AffineVariety)
	[jets,Projective]
	JJ
	
Node
    Key
    	"Storing Computations"
    Headline
    	caching scheme for jets computations
    Description
    	Text
	    In many cases, the @TO jets@ method will store its results inside
	    a @TO CacheTable@ in the base object. When the method is called
	    again with the same or a lower jets order, the result is pulled
	    from the cache.
	    
	    For polynomial rings, data is stored under @TT "*.jet"@.
    	Example
	    R= QQ[x,y]
	    R.?jet
	    jets(3,R)
	    R.?jet
	    peek R.jet
    	Text
	    Note also that rings of jets are built as towers from lower to
	    higher jets orders. Therefore it is possible to store a single
	    ring of the highest order computed thus far.
	    
	    For ideals, data is stored under @TT "*.cache.jet"@.
	    A single matrix is stored containing generators for the
	    highest order of jets computed thus far.
	    Generators for lower orders are recovered from this matrix
	    without additional computations.
    	Example
	    I= ideal (x^2 - y)
	    I.cache.?jet
	    elapsedTime jets(3,I)
	    I.cache.?jet
	    peek I.cache.jet
	    elapsedTime jets(3,I)
	    elapsedTime jets(2,I)
    	Text
	    For quotient rings, data is stored under @TT "*.jet"@.
	    Each jets order gives rise to a different quotient
	    that is stored separately under @TT "*.jet.jetsRing"@
	    (order zero jets are always included by default).
    	Example
	    Q= R/I
	    Q.?jet
	    jets(3,Q)
	    Q.?jet
	    peek Q.jet.jetsRing
	    jets(2,Q)
	    peek Q.jet.jetsRing
    	Text
	    For ring homomorphisms, data is stored under @TT "*.cache.jet"@.
	    A single matrix is stored describing the map for the
	    highest order of jets computed thus far.
	    Lower orders map are recovered from this matrix
	    without additional computations.
    	Example
	    S= QQ[t]
	    f= map(S,Q,{t,t^2})
	    isWellDefined f
	    f.cache.?jet
	    elapsedTime jets(3,f)
	    f.cache.?jet
	    peek f.cache.jet
	    elapsedTime jets(2,f)
    	Text
	    Projective jets data is stored separately under @TT "*.projet"@
	    or @TT "*.cache.projet"@ to accommodate for the different grading.
    	Example
	    jets(2,I,Projective=>true)
	    peek I.cache.projet
	    peek R.projet
    Caveat
    	No data is cached when computing jets of affine varieties and (hyper)graphs,
	radicals, or principal components.
    Subnodes
	jet
	projet
	jetsRing
	jetsMaxOrder
	jetsMatrix
	jetsBase
    	jetsInfo    


Node
    Key
	(jets,ZZ,PolynomialRing)
    Headline
    	compute jets of a polynomial ring
    Usage
    	jets (n,R)
    Inputs
    	n:ZZ
    	R:PolynomialRing
    Outputs
    	:PolynomialRing
       	 of jets order @TT "n"@.
    Description
    	Text
	    This function is provided by the package @TO Jets@.  Rings are 
	    constructed incrementally as towers.  The function returns the 
	    ring with variables in the jets order requested, and coefficients 
	    in all lower orders.  The grading or multigrading of the jets ring 
	    follows from that of the base ring.
    	Example	    
	    R= QQ[x,y,z,Degrees=>{2,1,3}]
	    JR= jets(2,R)
	    describe JR
    	    degrees (flattenRing JR)_0	    
    	Text
	    When the @TO [jets,Projective]@ option is set to true, the degree 
	    of each jets variable matches the jets order, in accordance with
	    Proposition 6.6 (c) of @arXiv("math/0407113","P. Vojta,
	    	Jets via Hasse-Schmidt Derivations")@.
    	Example
	    R=QQ[x,y,z,Degrees=>{2,1,3}]
	    JR= jets(2,R,Projective=>true)
	    degrees (flattenRing JR)_0
    	Text
	    The convention for labeling variables in the jets of polynomial ring
	    is to append the order of the jets to name of the variables in the
	    base ring. Existing subscripts are preserved.
    	Example
	    A=QQ[a_1..a_3]
	    JA= jets(1,A)
	    describe JA
    	Text
	    Note that the coefficient ring of the polynomial ring does not need
	    to be a field. The jets of the input polynomial ring will be a
	    polynomial ring with the same coefficient ring as the input.
    	Example
	    Zi=ZZ[i]/ideal(i^2+1)
	    B=Zi[b_1..b_3]
	    JB= jets(1,B)
	    describe JB
    Caveat
    	With @TT "Projective=>true"@ the jet variables of order zero have degree 0,
	therefore no heft vector exist for the ambient ring of the jets.
	As a result, certain computations will not be supported, and others may run more slowly.
	See @TO "Macaulay2Doc::heft vectors"@ for more information.
	    	  
Node
    Key
	(jets,ZZ,Ideal)
    Headline
    	compute jets of a an ideal in a polynomial ring
    Usage
    	jets (n,I)
    Inputs
    	n:ZZ
	I:Ideal
    Outputs
        :Ideal
	 generated by the jets of the generators of @TT "I"@
    Description
    	Text
	    This function is provided by the package
	    @TO Jets@.
    	Example	    
	    R= QQ[x,y]
	    I= ideal (x^3 + y^3 - 3*x*y)
    	    J= jets(3,I);
	    netList J_*
	Text
	    When the @TO [jets,Projective]@ option is set to true, the degree
	    of each jets variable matches its order, in accordance with
	    Proposition 6.6 (c) of @arXiv("math/0407113","P. Vojta,
	    	Jets via Hasse-Schmidt Derivations")@.
	    As a result, the jets of any ideal will be homogeneous regardless
	    of the homogeneity of the base ideal, or that of its affine jets.
	Example
	    R= QQ[x,y,z]
	    I= ideal (y-x^2, z-x^3)
	    JI= jets(2,I)
	    isHomogeneous JI
	    JIproj= jets(2,I,Projective=>true)
	    isHomogeneous JIproj
    Caveat
    	With @TT "Projective=>true"@ the jet variables of order zero have degree 0,
	therefore no heft vector exist for the ambient ring of the jets.
	As a result, certain computations will not be supported, and others may run more slowly.
	See @TO "Macaulay2Doc::heft vectors"@ for more information.

Node
    Key
	(jets,ZZ,QuotientRing)
    Headline
    	the jets of an affine algebra
    Usage
    	jets (n,Q)
    Inputs
	n:ZZ
	Q:QuotientRing
    Outputs
        :QuotientRing
    Description
    	Text
    	    This function is provided by the package @TO Jets@.  Forms the jets of a @TO QuotientRing@ by forming the quotient of
	    @TO (jets,ZZ,PolynomialRing)@ of the ambient ring of @TT "Q"@ with 
	    @TO (jets,ZZ,Ideal)@ of the ideal defining @TT "Q"@
    	Example	    
	    R= QQ[x,y];
	    I= ideal(y^2-x^3);
    	    Q= R/I;
	    JQ= jets(2,Q);
	    describe JQ
    Caveat
    	Forming quotients triggers a Groebner basis computation, which may be time consuming.
	
Node
    Key
	(jets,ZZ,RingMap)
    Headline
    	the jets of a homomorphism of rings
    Usage
    	jets (n,f)
    Inputs
	n:ZZ
	f:RingMap
    Outputs
        :RingMap
	    obtained by applying the @TT "n"@-th jets functor to @TT "f"@
    Description
    	Text
	    This function is provided by the package
	    @TO Jets@.
    	Example	    
	    R= QQ[x,y,z]
	    S= QQ[t]
	    f= map(S,R,{t,t^2,t^3})
	    Jf= jets(2,f);
	    matrix Jf
    	Text
	    This function can also be applied when the source and/or the target
	    of the ring homomorphism are quotients of a polynomial ring
    	Example
	    I= ideal(y-x^2,z-x^3)
	    Q= R/I
	    g= map(S,Q,{t,t^2,t^3})
	    isWellDefined g
	    Jg= jets(2,g);
	    isWellDefined Jg

Node
    Key
	(jets,ZZ,Graph)
	(jets,ZZ,HyperGraph)
    Headline
    	the jets of a graph
    Usage
    	jets (n,G)
    Inputs
	n:ZZ
	G:Graph
	    undirected, finite, and simple graph or hypergraph
    Outputs
        :Graph
	 the (hyper)graph of @TT "n"@-jets of @TT "G"@
    Description
    	Text
	    This function is provided by the package
	    @TO Jets@.
	    
	    Jets of graphs are defined in § 2 of
	    @arXiv("2104.08933","F. Galetto, E. Helmick, and M. Walsh,
    		Jet graphs")@.
	    The input is of type @TO "EdgeIdeals::Graph"@ as defined by
	    the @TO EdgeIdeals@ package, which is automatically exported
	    when loading @TO Jets@.
    	Example	    
	    R= QQ[x,y,z]
	    G= graph(R,{{x,y},{y,z}})
	    JG= jets(2,G)
	    vertexCovers JG
	Text
	    We can also calculate the jets of a  @TO "EdgeIdeals::HyperGraph"@.
	Example
	    R= QQ[u,v,w,x,y,z]
	    H= hyperGraph(R,{{u},{v,w},{x,y,z}})
    	    jets(1,H)
    Caveat
        Rings of jets are usually constructed as towers of rings with
    	tiers corresponding to jets of different orders. However, the
    	tower is flattened out before constructing the edge ideal of
    	the jets of a (hyper)graph. This is done in order to ensure
    	compatibility with the @TO "EdgeIdeals::EdgeIdeals"@ package.
	    

Node
    Key
    	(jets,ZZ,AffineVariety)
    Headline
    	the jets of an affine variety
    Usage
    	jets (n,V)
    Inputs
    	n:ZZ
	V:AffineVariety
    Outputs
    	:Variety
    	 an @TO AffineVariety@ or a @TO ProjectiveVariety@
    Description
    	Text
	    Returns the jets of an @TO AffineVariety@ as an @TO AffineVariety@. 
	Example
	    R=QQ[x,y]
	    I= ideal(y^2-x^2*(x+1))
	    A= Spec(R/I)
	    jets(2,A)
    	Text
	    If @TO [jets,Projective]@ is set to true, then jets are computed
	    with the grading introduced in Proposition 6.6 (c) of
	    @arXiv("math/0407113","P. Vojta, Jets via Hasse-Schmidt Derivations")@,
	    and the function returns a @TO ProjectiveVariety@.
	Example
	    jets(2,A,Projective=>true)
    Caveat
    	With @TT "Projective=>true"@ the jet variables of order zero have degree 0,
	therefore no heft vector exist for the ambient ring of the jets.
	As a result, certain computations will not be supported, and others may run more slowly.
	See @TO "Macaulay2Doc::heft vectors"@ for more information.
	
	Note: jets of projective varieties are currently not implemented.

Node
    Key
    	jet
    Headline
    	hashtable key
    Description
    	Text
	    @TO CacheTable@ for storing information on jets constructed
	    from a base object @TT "x"@.
	    For @TO PolynomialRing@ and @TO QuotientRing@, stored as @TT "x.*"@.
	    For @TO RingMap@ and @TO Ideal@ stored as @TT "x.cache.*"@.
    SeeAlso
	projet
	jetsRing
	jetsMaxOrder
	jetsMatrix
	jetsBase
    	jetsInfo

Node
    Key
    	projet
    Headline
    	hashtable key
    Description
    	Text
	    @TO CacheTable@ for storing information on the projective jets
	    of the base object @TT "x"@.
	    For @TO PolynomialRing@ and @TO QuotientRing@, stored as @TT "x.*"@.
	    For @TO RingMap@ and @TO Ideal@ stored as @TT "x.cache.*"@.
    SeeAlso
    	jet
	jetsRing
	jetsMaxOrder
	jetsMatrix
	jetsBase
    	jetsInfo

Node
    Key
    	jetsRing
    Headline
    	hashtable value
    Description
    	Text
    	    The @TO (jets,ZZ,PolynomialRing)@ of highest order computed thus far 
	    for a particular base ring.  Stored in @TO jet@ or @TO projet@ 
	    of the base.
    SeeAlso
    	jet
	projet
	jetsMaxOrder
	jetsMatrix
	jetsBase
    	jetsInfo	

Node
    Key
    	jetsMatrix
    Headline
    	hashtable value
    Description
    	Text
	    A matrix of jets elements which generate a @TO (jets,ZZ,Ideal)@
	    or serve as targets for a @TO (jets,ZZ,RingMap)@.  Stored in 
	    @TO jet@ or @TO projet@ of the base.
    SeeAlso
    	jet
	projet
	jetsRing
	jetsMaxOrder
	jetsBase
    	jetsInfo
		
Node
    Key
    	jetsMaxOrder
    Headline
    	hashtable value
    Description
    	Text
	    The highest order of jets computed thus far for a particular 
	    object.  Stored in @TO jet@ or @TO projet@ of the base object.
    SeeAlso
    	jet
	projet
	jetsRing
	jetsMatrix
	jetsBase
    	jetsInfo	

Node
    Key
    	jetsBase
    Headline
    	hashtable value
    Description
    	Text
    	    The base ring of a @TO (jets,ZZ,PolynomialRing)@.  Stored in a jets object 
	    for reference.
    SeeAlso
    	jet
	projet
	jetsRing
	jetsMaxOrder
	jetsMatrix
    	jetsInfo
	
Node
    Key
    	jetsInfo
    Headline
    	hashtable key
    Description
    	Text
    	    @TO CacheTable@ for storing information on the base object within
	    the jets object.
    SeeAlso
    	jet
	projet
	jetsRing
	jetsMatrix
	jetsBase
    	jetsMaxOrder

Node
    Key
    	[jets,Projective]
   	[jetsProjection,Projective]
    Headline
    	Option for jets
    Description
    	Text
	    Set the degree of each jet variable to match its order,
	    as in Proposition 6.6 (c) of
	    @arXiv("math/0407113","P. Vojta, Jets via Hasse-Schmidt Derivations")@.
	    This guarantees that the output of @TO jets@ is homogeneous.
    Caveat
    	With @TT "Projective=>true"@ the jet variables of order zero have degree 0,
	therefore no heft vector exist for the ambient ring of the jets.
	As a result, certain computations will not be supported, and others may run more slowly.
	See @TO "Macaulay2Doc::heft vectors"@ for more information.
    SeeAlso
    	(jets,ZZ,PolynomialRing)
	(jets,ZZ,Ideal)
	    
Node
    Key
    	jetsRadical
    	(jetsRadical,ZZ,Ideal)
    Headline
    	compute radicals of jets ideals
    Usage
    	jetsRadical(n,I)
    Inputs
    	n:ZZ
	I:Ideal
    Outputs
        :Ideal
    	    radical of the nth jets of @TT "I"@
    Description
   	Text
	    This function is provided by the package @TO Jets@. It returns the radical
	    of the ideal of jets of the input ideal.
	    
	    If the input is not a monomial ideal, this function uses the @TO radical@ function from
	    the @TO MinimalPrimes@ package.

	    If the input is a monomial ideal, it uses an algorithm 
	    based on the proof of Theorem 3.2 in @HREF("https://doi.org/10.1080/00927870500454927",
	    "R.A. Goward and K.E. Smith, The jet scheme of a monomial scheme")@.
	    This has the potential to speed up the computation, especially for large jet orders.
	    Note that the generating set of the output may not be minimal,
	    unless the generators of the input are squarefree monomials.
	    
	    An ideal generated by squarefree monomials:
	Example
	    R= QQ[x,y,z]
	    I= ideal (x*z, y*z)
	    J= jets(1,I); 
	    MP= radical J;
	    GS= jetsRadical(1,I);
	    netList sort MP_* | netList sort GS_*
    	Text
	    An ideal with genereators which are not squarefree:
	Example
	    R= QQ[x,y,z]
	    I= ideal(x*y^2, z*x, x^3)
	    J= jets(1,I); 
	    MP= radical J;
	    GS= jetsRadical(1,I);
	    netList sort MP_* | netList sort GS_*
	    MP == GS

Node
    Key
    	jetsProjection
    	(jetsProjection,ZZ,ZZ,PolynomialRing)
	(jetsProjection,ZZ,ZZ,QuotientRing)
    Headline
    	canonical map between jets rings
    Usage
    	jets(t,s,R)
	jets(t,s,Q)
    Inputs
    	t:ZZ
	s:ZZ
	R:PolynomialRing
	    or a quotient of
    Outputs
    	:RingMap
	 between jets orders
    Description
    	Text
	    This function is provided by the package @TO Jets@.  Generates an 
	    inclusion map from the order @TT "s"@ into the order @TT "t"@ jets
	    of a (quotient of a) polynomial ring.
	    Throws an error if @TT "t<s"@.
	    
    	Example
	    R=QQ[x,y]
	    f= jetsProjection(5,2,R)
	    use jets(2,R)
	    p= (x2 + 2*x1*y1 + x0*y2^2)
	    f p

Node
    Key
    	principalComponent
    	(principalComponent,ZZ,Ideal)
    Headline
    	compute principal component of jets
    Usage
    	principalComponent(s,I)
    Inputs
	s:ZZ
	I:Ideal
    Outputs
    	:Ideal
	 whose vanishing locus is the principal component of the @TT "s"@-jets of @TT "I"@
    Description
    	Text
	    This function is provided by the package @TO Jets@.
	    
	    Consider an affine variety $X \subseteq \mathbb{A}^n_\Bbbk$.
	    The principal (or dominant) component of the $s$-jets of $X$
	    is the Zariski closure of the $s$-jets of the smooth locus of $X$.
	    Let $X_{\mathrm{reg}}$ and $X_{\mathrm{sing}}$ denote respectively
	    the smooth and singular locus of $X$. If $\mathcal{J}_s$ denotes
	    the $s$-jets functor, then there is a natural embedding
	    $$X_\mathrm{sing} \subset X \subseteq \mathbb{A}^n_\Bbbk \subset
	    \mathcal{J}_s (\mathbb{A}^n_\Bbbk) \cong \mathbb{A}^{n(s+1)}_\Bbbk.$$
	    Let $I$ denote the ideal of $X_\mathrm{sing}$ in this embedding,
	    and let $J$ denote the ideal of $\mathcal{J}_s (X)$; both ideals
	    live in the polynomial ring $\Bbbk [\mathbb{A}^{n(s+1)}_\Bbbk]$.
	    We have an equality of sets $$\mathcal{J}_s (X_\mathrm{reg}) =
	    \mathcal{J}_s (X) \setminus X_\mathrm{sing} = \mathbf{V} (J)
	    \setminus \mathbf{V} (I).$$ By Theorem 10 in Chapter 4, §4 of
	    @HREF("https://doi.org/10.1007/978-3-319-16721-3",
		"D.A. Cox, J. Little, D. O'Shea - Ideals, Varieties, and Algorithms")@,
	    if $\Bbbk$ is algebraically closed, then there is an equality
	    $$\mathbf{V} (J\colon I^\infty) = \overline{\mathbf{V} (J)
	    \setminus \mathbf{V} (I)} = \overline{\mathcal{J}_s (X_\mathrm{reg})}.$$
	    This function returns the ideal $J\colon I^\infty$.
	    
	    If $J$ is known to be a radical ideal, then
	    $\mathbf{V} (J\colon I) = \overline{\mathbf{V} (J)
	    \setminus \mathbf{V} (I)}$ by Corollary 11 in Chapter 4, §4 of
	    @HREF("https://doi.org/10.1007/978-3-319-16721-3",
		"D.A. Cox, J. Little, D. O'Shea - Ideals, Varieties, and Algorithms")@.
	    In this case, the user may pass the option @TT "Saturate=>false"@
	    to return the ideal $J\colon I$, which can speed up computations.
	    
	    As an example, consider the union of three non parallel lines
	    in the affine plane. We compute the principal component of the
	    jets of order two.
    	Example
	    R=QQ[x,y]
	    I=ideal(x*y*(x+y-1))
	    PC=principalComponent(2,I)
    	Text
	    Despite the name, the principal component need not be a component
	    of the jet scheme (i.e., it need not be irreducible). In this example,
	    the principal component has degree 3 and is the union of three components
	    of degree 1.
    	Example
	    P=primaryDecomposition jets(2,I)
	    any(P,c -> c==PC)
	    PC==intersect(select(P,c -> degree c==1))
    Caveat
    	This function requires computation of a singular locus,
	a saturation (or quotient), and jets, with each step being
	potentially quite time consuming.
    Subnodes
    	Saturate
	
Node
    Key
    	Saturate
    	[principalComponent,Saturate]
    Headline
    	option for principal components
    Description
    	Text
	    Strategy for computing principal components of jet schemes
    SeeAlso
    	principalComponent
		    
Node
    Key
    	JJ
    Headline
    	scripted functor associated with jets
    Usage
    	JJ_n X
    Inputs
    	n:ZZ
    Description
    	Text
	    Shorthand for @TO jets@
	Example
	    R= QQ[x,y]
	    I= ideal(y^2-x^3)
	    JJ_2 R
	    JJ_2 I

Node
    Key
    	"Example 1"
    Headline
    	jets of monomial ideals
    Description
    	Text
	    As observed in @HREF("https://doi.org/10.1080/00927870500454927",
	    "R.A. Goward and K.E. Smith, The jet scheme of a monomial scheme")@ [GS06],
	    the ideal of jets of a monomial ideal is typically not a monomial ideal.
	Example
	    R=QQ[x,y,z]
	    I=ideal(x*y*z)
	    J2I=jets(2,I)
	Text
	    However, by [GS06, Theorem 3.1], the radical is always a (squarefree) monomial ideal. In
	    fact, the proof of [GS06, Theorem 3.2] shows that the radical is generated by the terms of the
	    jet equations constructed as in the introduction. This observation provides an alternative
	    algorithm for computing radicals of jets of monomial ideals, which can be faster than the
	    default radical computation in Macaulay2.
    	Example
	    elapsedTime jetsRadical(2,I)
	    elapsedTime radical J2I
	Text
	    For a monomial hypersurface, [GS06, Theorem 3.2] describes the minimal primes of the
	    ideal of jets. Moreover, the main theorem in @arXiv("math/0607638",
	    "C. Yuen, Multiplicity of jet schemes of monomial schemes")@ counts the multiplicity of the jet scheme
	    of a monomial hypersurface along its minimal primes (see also @HREF("https://doi.org/10.1080/00927870701512168",
	    "C. Yuen, The multiplicity of jet schemes of a simple normal crossing divisor")@). We compute the
	    minimal primes, then use the @TO "LocalRings::LocalRings"@ package to compute their multiplicities in
	    the second jet scheme of the example above. Note that we need to flatten the polynomial ring
	    of jets because the @TT "LocalRings"@ package does not allow towers of rings.
	Example
	    P=minimalPrimes J2I
	    (A,f)=flattenRing ring J2I
	    needsPackage "LocalRings"
	    M=cokernel gens f J2I
	    mult=for p in P list (
		Rp := localRing(A,f p);
		length(M ** Rp)
		);
	    netList(pack(4,mingle{P,mult}),HorizontalSpace=>1)

Node
    Key
    	"Example 2"
    Headline
    	jets of graphs
    Description
    	Text
	    Jets of graphs were introduced in @arXiv("2104.08933","F. Galetto, E. Helmick, and M. Walsh,
    		Jet graphs")@ [GHW21]. Starting with a
	    finite, simple graph $G$, one may construct a quadratic squarefree
	    monomial ideal $I(G)$ (known as the \emph{edge ideal} of the graph) by
	    converting edges to monomials.
	    One may then consider the radical of the ideal of $s$-jets of $I(G)$,
	    which is again a quadratic squarefree monomial ideal. The graph
	    corresponding to this ideal is the graph of $s$-jets of $G$, denoted
	    $\mathcal{J}_s (G)$.
	    
	    Jets of graphs and hypergraphs can be obtained by applying the
	    @TO jets@ method to objects of type @TO "EdgeIdeals::Graph"@ and
	    @TO "EdgeIdeals::HyperGraph"@ from the Macaulay2 @TO "EdgeIdeals::EdgeIdeals"@ package
	    (which is automatically loaded by the @TO Jets@ package.
	    Consider, for example, the graph in the figure below.
    	Code
      	    IMG ("src" => replace("PKG", "Jets", currentLayout#"package") | "graph.png",
		"alt" => "a graph on 5 vertices", "height" => "300")
	Example
	    R=QQ[a..e]
	    G=graph({{a,c},{a,d},{a,e},{b,c},{b,d},{b,e},{c,e}})
	Text
	    We compute the first and second order jets, and list their edges.
    	Example
	    J1G=jets(1,G); netList pack(7,edges J1G)
	    J2G=jets(2,G); netList pack(7,edges J2G)
	Text
	    As predicted in [GHW21, Theorem 3.1], all jets have the same
	    chromatic number.
	Example
	    apply({G,J1G,J2G},chromaticNumber)
    	Text
	    By contrast, jets may not preserve the property of being co-chordal.
    	Example
	    apply({G,J1G,J2G},x -> isChordal complementGraph x)	    
    	Text
	    Using Fröberg's Theorem (cf. R. Fröberg, On Stanley-Reisner rings),
	    we deduce that although
	    the edge ideal of a graph may have a linear free resolution, the edge
	    ideals of its jets may not have linear resolutions.
        	
	    Finally, we compare minimal vertex covers of the graph and of its
	    second order jets.
    	Example
    	    vertexCovers G
	    netList pack(2,vertexCovers J2G)
    	Text
	    With the exception of the second row, many vertex covers arise as
	    indicated in [GHW21, Proposition 5.2, 5.3].

Node
    Key
    	"Example 3"
    Headline
    	jets of determinantal varieties
    Description
    	Text
	    Consider the determinantal varieties $X_r$ of
	    $3\times 3$ matrices of rank at most $r$, which are defined by the
	    vanishing of minors of size $r+1$.  We illustrate computationally some
	    of the known results about jets.
	Example
	    R=QQ[x_(1,1)..x_(3,3)]
	    G=genericMatrix(R,3,3)
	Text
	    Since $X_0$ is a single point, its first jet scheme consists of a
	    single (smooth) point.
    	Example
	    I1=minors(1,G)
	    JI1=jets(1,I1)
	    dim JI1, isPrime JI1
	Text
	    The jets of $X_2$ (the variety of maximal minors) are known to be
	    irreducible (see Theorem 3.1 in @HREF("https://doi.org/10.1016/j.jpaa.2004.06.001",
		    "T. Košir, B.A. Sethuraman, Determinantal varieties over truncated polynomial rings")@ [KS05],
		or Corollary 4.13 in @HREF("https://doi.org/10.1090/S0002-9947-2012-05564-4",
		    "R. Docampo, Arcs on determinantal varieties")@ [Doc13]).
	Example
	    I3=minors(3,G)
	    JI3=jets(1,I3)
	    isPrime JI3
    	Text
    	    As for the case of $2\times 2$ minors, Theorem 5.1 in [KS05], Theorem 5.1 in
	    @arXiv("math/0608632","C. Yuen, Jet schemes of determinantal varieties")@,
	    and Corollary 4.13 in [Doc13] all count the number of components;
	    the first two references describe
	    the components further. As expected, the first jet scheme of $X_1$ has
	    two components, one of them an affine space.
    	Example
	    I2=minors(2,G)
	    JI2=jets(1,I2)
	    P=primaryDecomposition JI2; #P
	    P_1
    	Text
	    The other component is the so-called principal component of the jet
	    scheme, i.e., the Zariski closure of the first jets of the smooth
	    locus of $X_1$. To check this, we first establish that the first jet
	    scheme is reduced (i.e. its ideal is radical), then use the
	    @TO principalComponent@ method with the option
	    @TO [principalComponent,Saturate]@ set to @TT "false"@ to speed up computations.
    	Example
	    radical JI2==JI2
	    P_0 == principalComponent(1,I2,Saturate=>false)
	    P_0
    	Text
	    Finally, as observed in Theorem 18 of @HREF("http://dx.doi.org/10.2140/pjm.2014.272.147",
		"S.R. Ghorpade, B. Jonov and B.A. Sethuraman,
		Hilbert series of certain jet schemes of determinantal varieties")@ the Hilbert
	    series of the principal component of the first jet scheme of $X_1$ is
	    the square of the Hilbert series of $X_1$.
    	Example
	    apply({P_0,I2}, X -> hilbertSeries(X,Reduce=>true))
	    numerator (first oo) == (numerator last oo)^2
///
end
