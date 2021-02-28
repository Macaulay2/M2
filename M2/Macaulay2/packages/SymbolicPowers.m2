newPackage(
        "SymbolicPowers",
	Version => "2.0", 
	Date => "May 20, 2019",
	Authors => {
	    {Name => "Eloisa Grifo", Email => "grifo@umich.edu", HomePage => "http://www-personal.umich.edu/~grifo/"}
	    },
	Headline => "symbolic powers",
	Keywords => {"Commutative Algebra"},
	DebuggingMode => false,
	Certification => {
	     "journal name" => "The Journal of Software for Algebra and Geometry",
	     "journal URI" => "http://j-sag.org/",
	     "article title" => "Calculations involving symbolic powers",
	     "acceptance date" => "20 May 2019",
	     "published article URI" => "https://msp.org/jsag/2019/9-1/p09.xhtml",
	     "published article DOI" => "10.2140/jsag.2019.9.71",
	     "published code URI" => "https://msp.org/jsag/2019/9-1/jsag-v9-n1-x09-SymbolicPowers.m2",
	     "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/SymbolicPowers.m2",
	     "release at publication" => "fe3eea250b0c2c9a0ebbbd84cf44b7a52da63fc0",	    -- git commit number in hex
	     "version at publication" => "2.0",
	     "volume number" => "9",
	     "volume URI" => "https://msp.org/jsag/2019/9-1/"
	     }
        )


export {
    -- Options
    "InSymbolic",
    "SampleSize",
    "UseMinimalPrimes",
    "UseWaldschmidt",
    "CIPrimes", 
    
    -- Methods
    "asymptoticRegularity",
    "assPrimesHeight",
    "bigHeight",
    "containmentProblem",     
    "joinIdeals",    
    "isKonig", 
    "isPacked",
    "isSymbolicEqualOrdinary",
    "isSymbPowerContainedinPower", 
    "lowerBoundResurgence",
    "minDegreeSymbPower",
    "minimalPart",
    "noPackedSub", 
    "noPackedAllSubs",
    "squarefreeGens", 
    "squarefreeInCodim",    
    "symbolicDefect",
    "symbolicPower",
    "symbolicPowerJoin", 
    "symbPowerPrimePosChar",
    "symbolicPolyhedron", 
    "waldschmidt"
    }


needsPackage "Polyhedra";
needsPackage "Depth";
needsPackage "PrimaryDecomposition";


---------------------------------------------------------------
---------------------------------------------------------------
-- Auxiliary functions: faster powers, big height, minimal part
-- isMonomial
---------------------------------------------------------------
---------------------------------------------------------------

fastPower = method(TypicalValue => Ideal)
fastPower(Ideal,ZZ) := Ideal => (I,n) -> (
    J := I;
    for i from 2 to n do J = J*I;
    return J
    )


--old version of big height
--bigHeight = method(TypicalValue => ZZ)
--bigHeight(Ideal) := ZZ => I -> (if isPrimary(I) then codim(I) else 
--    (R := ring I; d := dim R; c := codim I; M := R^1/I; 
--	if codim Ext^d(M,R) == d then d else
--	(l := toList (c .. d);
--	w := apply(l, i->Ext^i(M,R)); v := apply(w,codim); 
--	d-position(reverse(v-l),i->i==0))))


bigHeight = method(TypicalValue => ZZ)
bigHeight(Ideal) := ZZ => I -> (
    if isPrimary(I) then return codim(I) else (
	R := ring I; 
	d := dim R; 
	c := codim I; 
	M := R^1/I; 
	ans := d;
	scan(reverse toList (c .. d), i -> (if codim Ext^i(M,R) == i then (ans = i; break)));
	return ans
	)
    )


assPrimesHeight = method(TypicalValue => List)
assPrimesHeight(Ideal) := List => I -> (
    if isPrimary(I) then {codim(I)} else (
	R := ring I; 
	d := dim R; 
	c := codim I; 
	M := R^1/I; 
	l := toList (c .. d);
	w := apply(l, i->Ext^i(M,R)); 
	v := apply(w,codim);
	reverse apply(positions(reverse(v-l),i->i==0), j -> d-j)
	)
    )



minimalPart = method()
minimalPart(Ideal) := Ideal => I -> (
    minPrimes := minimalPrimes (I);
    primDec := primaryDecomposition(I);
    minComponents := {};
    scan(primDec, i -> (
    	rad := radical(i); 
	scan(minPrimes, a -> if rad == a then (minComponents = append(minComponents,i); break))
	));
    intersect(minComponents)
    )


isMonomial = method()
isMonomial(RingElement) := r -> # terms r === 1 --(terms(r) == {r})
isMonomial(MonomialIdeal) := I -> true
isMonomial(Ideal) := I -> all(flatten entries mingens I,a -> isMonomial(a))


minDegreeSymbPower = method(TypicalValue => ZZ)
minDegreeSymbPower(Ideal,ZZ) := ZZ => (I,n) -> min flatten degrees symbolicPower(I,n)

--Given an ideal I and q=p^e, computes the e-th Frobenius power of I
frobeniusPower = method(TypicalValue => Ideal)
frobeniusPower(Ideal,ZZ) := Ideal => (I,q) -> (
    ideal(apply(flatten entries gens I, i -> i^q))
    )



-----------------------------------------------------------
-----------------------------------------------------------
-- Main symbolic powers function
-----------------------------------------------------------
-----------------------------------------------------------



symbPowerMon = method(TypicalValue => Ideal)
symbPowerMon(Ideal,ZZ) := Ideal => (I,n) -> (
    if isSquareFree I 
    then (
	assP := associatedPrimes(I); 
	intersect apply(assP, i -> fastPower(i,n))
	)
    else (
	--If I is simply monomial, one can collect the primary components in a decomposition
	--of I and intersect the powers of the *maximal* ones
	Pd:=primaryDecomposition I;
    	P:=apply(Pd, a-> radical a);
    	maxP:={};
    	apply(P, a-> if #select(P, b-> isSubset(a,b))==1 then maxP=maxP|{a});
    	Q:=for p in maxP list (intersect select(Pd, a-> isSubset(a,p)));
    	intersect apply(Q,i -> fastPower(i,n))
	)
    )


-- symbPowerPrime = method()
-- symbPowerPrime(Ideal,ZZ) := Ideal => (I,n) -> (
--         primaryList := primaryDecomposition(fastPower(I,n)); 
-- 	local result;
-- 	scan(primaryList, i -> if radical(i)==I then (result = i; break));
-- 	result
-- 	)
    
-- symbPowerPrimary = method()
-- symbPowerPrimary(Ideal, ZZ) := Ideal => (I,n) -> (
--         rad := radical(I);
-- 	local result;
-- 	primaryList := primaryDecomposition(fastPower(I,n)); 
-- 	scan(primaryList,i->(if radical(i)==rad then result := i; break));
-- 	result
-- 	)

  
   
symbPowerSat = method(TypicalValue => Ideal)
symbPowerSat(Ideal,ZZ) := Ideal => (I,n) -> (
    R := ring I; 
    m := ideal vars R; 
    saturate(fastPower(I,n),m)
    )

--Takes a primary decomposition of I^n, picks the components corresponding to the 
--minimal primes of I and intersects them
symbPowerSlow = method(TypicalValue => Ideal)
symbPowerSlow(Ideal,ZZ) := Ideal => (I,n) -> (
    assI := associatedPrimes(I);
    decomp := primaryDecomposition fastPower(I,n);
    intersect select(decomp, a -> any(assI, i -> radical a==i))
    )


symbolicPower = method(TypicalValue => Ideal, Options => {UseMinimalPrimes => false,CIPrimes => false})
symbolicPower(Ideal,ZZ) := Ideal => opts -> (I,n) -> (
    R := ring I;
        
    if opts.UseMinimalPrimes then return (minimalPart fastPower(I,n));
    
    if opts.CIPrimes then (
        if (
	    Pd :=primaryDecomposition I;
	    all(Pd,i->(dim(R)-dim(I))==#flatten(entries(mingens i)))
	    ) 
	then (
	    return intersect apply(Pd, i->fastPower(i,n)) 
	    ) 
	else (
	    error "Associated primes are not complete intersections of the same height."
	    )
	);
		    
        
    if not opts.UseMinimalPrimes 
    then (    
    	if (isPolynomialRing R and isMonomial I) 
	then (
	    return symbPowerMon(monomialIdeal(I),n)
	    ) 
	else (
	    if (codim I == dim R - 1 and isHomogeneous(I)) 
	    then (
		if depth (R/I) == 0 then return fastPower(I,n) else return symbPowerSat(I,n) 
		) 
	    else (
		if (isPolynomialRing R and bigHeight I == codim I) 
		then return topComponents(fastPower(I,n)) 
		else return symbPowerSlow(I,n)
		)
	    )
	)
    )


-----------------------------------------------------------
-----------------------------------------------------------
-- Containment Problem and Equality functions
-----------------------------------------------------------
-----------------------------------------------------------


isSymbolicEqualOrdinary = method(TypicalValue => Boolean)
isSymbolicEqualOrdinary(Ideal,ZZ) := (P,n) -> (
    Q := fastPower(P,n); 
    h := bigHeight(P);
    if bigHeight(Q) > h then false else (
	if h==codim(P) then true else symbolicPower(P,n)==Q
	)
    )
    



isSymbPowerContainedinPower = method(TypicalValue => Boolean, Options => {UseMinimalPrimes => false, CIPrimes => false})
isSymbPowerContainedinPower(Ideal,ZZ,ZZ) := Boolean => opts -> (I,m,n) -> (
    if isPolynomialRing(ring(I)) 
    then (
	h := bigHeight I; 
	if m<n then false else (
	    if m>= h*n then true else (
		symb := symbolicPower(I,m, UseMinimalPrimes => opts.UseMinimalPrimes, CIPrimes => opts.CIPrimes); 
		pow := fastPower(I,n); 
		isSubset(symb,pow))))
    else (
	sym := symbolicPower(I,m, UseMinimalPrimes => opts.UseMinimalPrimes, CIPrimes => opts.CIPrimes); 
	po := fastPower(I,n); 
	isSubset(sym,po)))
	    




containmentProblem = method(TypicalValue => ZZ, Options => {UseMinimalPrimes => false,InSymbolic => false,CIPrimes => false})
containmentProblem(Ideal,ZZ) := ZZ => opts -> (I,n) -> (

    if not(opts.InSymbolic) then (
	m := n; 
	while not(isSymbPowerContainedinPower(I,m,n, UseMinimalPrimes => opts.UseMinimalPrimes, CIPrimes => opts.CIPrimes)) 
	do m = m+1; 
	return(m)
	);
    
    if opts.InSymbolic then (
	h := bigHeight(I);
	e := (n-n%h)/h; 
	l := lift(e,ZZ);
	while isSymbPowerContainedinPower(I,n,l+1,UseMinimalPrimes => opts.UseMinimalPrimes, CIPrimes => opts.CIPrimes) 
	do l = l+1;
	return l
	)
    )





-----------------------------------------------------------
-----------------------------------------------------------
--Other ways to compute symbolic powers: 
--for primes in char p
--for prime ideals in a polynomial ring, using join
-----------------------------------------------------------
-----------------------------------------------------------



--Given integers a and p, finds the largest power of p such that p^k<=a
--auxiliary for symbPowerPrimePosChar
powersdivision = method(TypicalValue => ZZ)
powersdivision(ZZ,ZZ,ZZ) := ZZ => (a,p,k) -> ( if p^k>a then 1 else 1+(powersdivision(a,p,k+1)) )
powersdivision(ZZ,ZZ) := ZZ => (a,p) -> powersdivision(a,p,1)


--Computes the symbolic power of a prime ideal in characteristic p
--The method is as follows: to compute $I^{(a)}$, find the largest value k with 
-- $q=p^k \leqslant a$
--$I^{(a)} = (I^{[q]} : I^{a-q+1})$
symbPowerPrimePosChar = method(TypicalValue => Ideal)
symbPowerPrimePosChar(Ideal,ZZ) := Ideal => (I,n) -> (
    R := ring I; 
    p := char R;
    if not(isPrime(I)) then "Not a prime ideal" else (
	h := codim I;
	if p==0 then "The characteristic must be positive" else	(
	    e := powersdivision(n,p); 
	    q := p^e; 
	    c := q-1; 
	    d := h*c-n+1; 
	    J:= I^d; --fastPower(I,d);
	    (frobeniusPower(I,q)):J
	    )
	)
    )


joinIdeals = method(TypicalValue => Ideal)
joinIdeals(Ideal,Ideal) := Ideal => (I,J) -> (
    R := ring I; 
    k := coefficientRing(R);
    d := dim(R);
    x := getSymbol "T";
    S := k[x_1 .. x_(3*d)];
    i := map(S, R, {(x_(d+1))_S .. (x_(2*d))_S});
    j := map(S, R, {(x_(2*d+1))_S .. (x_(3*d))_S});
    use S;
    aux := i -> (x_i)_S - (x_(d+i))_S - (x_(2*d+i))_S;
    extra := apply(toList(1 .. d), aux);
    bigideal := ideal(i(I),j(J), ideal(extra));
    inc := map(S,R,{(x_1)_S .. (x_d)_S});
    preimage(inc,bigideal)
    )
    
    
symbolicPowerJoin = method(TypicalValue => Ideal);
symbolicPowerJoin(Ideal,ZZ) := Ideal => (p,n) -> (
    m := ideal(generators ring p);
    joinIdeals(p,m^n)  --fastPower(m,n)))
    )




-----------------------------------------------------------
-----------------------------------------------------------
--Packing Problem
-----------------------------------------------------------
-----------------------------------------------------------

--Given a monomial ideal, finds a minimal generating set, 
--and then returns the exponents of the monomials in that set
--Given a monomial, returns the exponents

exponentsMonomialGens = method(TypicalValue => List)
exponentsMonomialGens(Ideal) := List => I -> (
    apply(flatten entries mingens I, l -> flatten exponents l)    
    )

squarefreeGens = method()
squarefreeGens(Ideal) := List => I -> (
    v := select(exponentsMonomialGens(I),i -> all(i,o -> o<2));
    l := flatten entries vars ring I;
    apply(v,o->product(apply(toList pairs(o),(i,j)->(l_i)^j)))
    )


--Finds squarefree monomials generating I^c, where c=codim I
squarefreeInCodim = method()
squarefreeInCodim(Ideal) := List => I -> (
    squarefreeGens(fastPower(I,codim I))
    )


isKonig = method(TypicalValue => Boolean)
isKonig(Ideal) := Boolean => I -> (
    R := ring I;
    if I == ideal 1_R then true else (
	if I == ideal(0_R) then true else (
	    not(squarefreeGens(fastPower(I,codim I))=={})
	    )
	)
    )


-- Input: Ideal and List of variables to evaluate to 1
-- Output: Ideal with vars from list evaluated to 1
replaceVarsBy1 = method(TypicalValue => Ideal)
replaceVarsBy1(Ideal,List) := Ideal => (I,L) -> (
    w := flatten entries vars ring I;
    v := fold((i,o) -> replace(o,1,i),w,L);
    promote(substitute(I,matrix{v}),ring I)
    )


-- Input: Ideal and List of variables to evaluate to 0
-- Output: Ideal with vars from list evaluated to 0
replaceVarsBy0 = method(TypicalValue => Ideal)
replaceVarsBy0(Ideal,List) := Ideal => (I,L) -> (
    w := flatten entries vars ring I;
    v := fold((i,o) -> replace(o,0,i),w,L);
    promote(substitute(I,matrix{v}),ring I)
    )
    
-- Input: Monomial Ideal
-- Output: Boolean value determining if packed or not
isPacked = method(TypicalValue => Boolean)
isPacked(Ideal) := Boolean => I -> (
    d := # flatten entries vars ring I; 
    s := subsets(d);
    w := flatten(table(s,s,(a,b) -> {a,b}));
    w = select(w, i -> unique(join(i_0,i_1))==join(i_0,i_1));
    all(w,x -> isKonig(replaceVarsBy1(replaceVarsBy0(I,x_0),x_1)))
    )


noPackedSub = method(TypicalValue => List)
noPackedSub(Ideal) := List => I -> (
    if not(isKonig(I)) then "The ideal itself is not Konig!" else (
	var := flatten entries vars ring I; d := # var;
    	s := delete({},subsets(d));
    	w := flatten(table(s,s,(a,b) -> {a,b}));
    	w = select(w, i -> unique(join(i_0,i_1))==join(i_0,i_1));
    	firstFailure := select(1,w,x -> not(isKonig(replaceVarsBy1(replaceVarsBy0(I,x_0),x_1))));
    	firstFailure = flatten firstFailure;
    	varsReplacedBy0 := firstFailure_0;
    	varsReplacedBy0 = var_(varsReplacedBy0);
    	varsReplacedBy1 := firstFailure_1;
    	varsReplacedBy1 = var_(varsReplacedBy1);
    	varsReplacedBy0 = apply(apply(varsReplacedBy0,toString),i -> concatenate(i,"=>0"));
    	varsReplacedBy1 = apply(apply(varsReplacedBy1,toString),i -> concatenate(i,"=>1"));
    	varsReplacedBy0 | varsReplacedBy1
	)
    )


noPackedAllSubs = method(TypicalValue => List)
noPackedAllSubs(Ideal) := List => I -> (
    var := flatten entries vars ring I; 
    s := delete({},subsets(# var));
    w := flatten(table(s,s,(a,b) -> {a,b}));
    w = select(w, i -> unique(join(i_0,i_1))==join(i_0,i_1));
    allFailures := select(w,x -> not(isKonig(replaceVarsBy1(replaceVarsBy0(I,x_0),x_1))));
    allSubs := apply(allFailures, o -> (
	    apply(var_(o_0),i->concatenate(toString i,"=>0")) | apply(var_(o_1),i->concatenate(toString i,"=>1"))
	    ));
    if allSubs == {} then "Only I is not Konig -- all proper substitutions are Konig." else
    allSubs
    )
    


---------------------------------
---Symbolic Defect
---------------------------------

symbolicDefect = method(TypicalValue => ZZ, Options => {UseMinimalPrimes => false,CIPrimes => false})
symbolicDefect(Ideal,ZZ) := opts -> (I,n) -> (
    R := ring I;
    Y := fastPower(I,n);
    S := R/Y;
    F := map(S,R);
    X := symbolicPower(I,n, UseMinimalPrimes => opts.UseMinimalPrimes, CIPrimes => opts.CIPrimes);
    # flatten entries mingens F(X)
    )



-----------------------------------------------------------
-----------------------------------------------------------
-- Functions for asymptotic invariants
-----------------------------------------------------------
-----------------------------------------------------------

-- Computes the symbolic polyhedron for a monomial ideal
-- Input: an ideal or a  monomial ideal 
-- Output: a Polyhedron

symbolicPolyhedron = method();

symbolicPolyhedron Ideal := Polyhedron => I -> (
    if not isMonomial(I) then ( 
	error "symbolicPolyhedron cannot be applied for an ideal that is not monomial"; 
	return
	);
    
    return symbolicPolyhedron monomialIdeal I
    )


symbolicPolyhedron MonomialIdeal := Polyhedron => I -> (
    Pd:=primaryDecomposition I;
    P:=apply(Pd, a-> radical a);
    maxP:={};
    apply(P, a-> if #select(P, b-> isSubset(a,b))==1 then maxP=maxP|{a});
    Q:=for p in maxP list (intersect select(Pd, a-> isSubset(a,p)));
    PI:=apply(Q, a-> newtonPolytope sum flatten entries gens a);
    C := posHull id_(ZZ^(dim ring I));
    QI :=apply(PI, p-> p+C);
    N :=intersection QI;
    return N
    )

alpha = I -> min apply(flatten entries gens I, f-> (degree f)_0) 

-- Computes the Waldschmidt constant for a given ideal
-- Input: an ideal or a  monomial ideal 
-- Output: a rational number

waldschmidt = method(Options=>{SampleSize=>5});
waldschmidt Ideal := opts -> I -> (
    if isMonomial I then ( 
--    	print "Ideal is monomial, the Waldschmidt constant is computed exactly";   
    	N:=symbolicPolyhedron I;
    	return min flatten entries ((transpose vertices N) * (matrix degrees ring I) )
    	)
    else (
--    	print ("Ideal is not monomial, the  Waldschmidt constant is approximated using first "| opts#SampleSize |" powers.");
    	return min for i from 1 to opts#SampleSize  list alpha(symbolicPower(I,i))/i
    	)
    )

waldschmidt MonomialIdeal := opts -> I -> (  
    N:=symbolicPolyhedron I;
    return min apply (entries transpose vertices N, a-> sum  a)
    )

lowerBoundResurgence = method(TypicalValue => QQ, Options =>{SampleSize=>5,UseWaldschmidt=>false})
lowerBoundResurgence(Ideal) := opts  -> (I) -> (
    l := max append(apply(toList(2 .. opts#SampleSize),o -> (containmentProblem(I,o)-1)/o),1);
    if opts#UseWaldschmidt == false 
    then return l
    else return max {l, alpha(I)/waldschmidt(I)}
    )


-- for this function I am assuming that the asymptotic regularity is an infimum
-- this is more specific than being a limit
-- I need to think if this is true (did not find it written in the literature)

asymptoticRegularity = method(Options=>{SampleSize=>10});
asymptoticRegularity Ideal := opts -> I -> (
--    print ("The asymptotic regularity is approximated using first "| opts#SampleSize |" powers.");
    return min for i from 1 to opts#SampleSize  list regularity(symbolicPower(I,i))/i
    ) 





-----------------------------------------------------------
-----------------------------------------------------------
--Documentation
-----------------------------------------------------------
-----------------------------------------------------------

beginDocumentation()

document { 
  Key => SymbolicPowers,
  Headline => "symbolic powers of ideals",
   PARA {
       "This package gives the ability to compute symbolic powers, and related invariants,
       of ideals in a polynomial ring or a quotient of a polynomial ring. For example, 
       in the context of the default behavior, ", TO "symbolicPower", " assumes the 
       following definition of the symbolic power of an ideal ", TEX /// $I$, ///, 
       TEX /// $$I^{(n)} = \cap_{p \in Ass(R/I)}(I^nR_p \cap R ),$$ ///,
       "as defined by M. Hochster and C. Huneke."},

   PARA {"Alternatively, as defined in Villarreal, ", TO "symbolicPower", 
       " has the option to restrict to minimal primes versus use all associated 
       primes with ", TO "UseMinimalPrimes", ".", " In particular, the 
       symbolic power of an ideal ", TEX ///$I$ ///, " is defined as", 
       TEX /// $$I^{(n)} = \cap_{p \in Min(R/I)}(I^nR_p \cap R ),$$ ///, 
       "where ", TEX /// $Min(R/I)$///, " is the set of minimal primes in ", 
       TEX /// $I$,///},
   
   UL { 
       {"M. Hochster and C. Huneke.", EM " Comparison of symbolic and ordinary powers of ideals.", 
	   " Invent. Math. 147 (2002), no. 2, 349–369."}, 
       {"R. Villarreal.", EM " Monomial algebras.", " Second edition. Monographs and Research Notes 
	   in Mathematics. CRC Press, Boca Raton, FL, 2015. xviii+686 pp. ISBN: 978-1-4822-3469-5."}, 
       {"Hailong Dao, Alessandro De Stefani, Eloísa Grifo, Craig Huneke, and Luis Núñez-Betancourt. ", 
	   EM "Symbolic powers of ideals.", "in Singularities and foliations. Geometry, topology and applications, pp. 387-432, Springer Proc. Math. Stat., 222, Springer, Cham, 2018. See ", HREF("https://arxiv.org/abs/1708.03010","https://arxiv.org/abs/1708.03010"), "."} 
       },
  
   SUBSECTION "Contributors", "The following people have generously
   contributed code or worked on our code at various Macaulay2
   workshops.",
     
     UL {
	 "Ben Drabkin",
	 "Andrew Conner",
	 "Alexandra Seceleanu",
	 "Branden Stone",
	 "Xuehua (Diana) Zhong"
	},

   SUBSECTION "A Quick Introduction",
   UL {
    TO "Computing symbolic powers of an ideal",
    TO "Alternative algorithm to compute the symbolic powers of a prime ideal in positive characteristic"
    },
    
 
  SUBSECTION "Other Related Examples",
  UL {
    TO "The Containment Problem",
    TO "Sullivant's algorithm for primes in a polynomial ring",
    TO "The Packing Problem"    
  }

}  



doc ///
     Key
     	  "A quick introduction to this package"
     Headline
     	  How to use this package
     Description
     	  Text
	       Here is a list of some examples which illustrate various parts of this package.
	       
	       {\bf First examples which show how to use this package}
    	       
	       $\bullet$ @TO"Computing symbolic powers of an ideal"@
	       
	       $\bullet$ @TO"Alternative algorithm to compute the symbolic powers of a prime ideal in positive characteristic"@
 
               {\bf Other examples which illustrate this package}

               $\bullet$ @TO"The Containment Problem"@
	       
	       $\bullet$ @TO"Sullivant's algorithm for primes in a polynomial ring"@
    	       	     
	       {\bf The Packing Problem}
	       
	       $\bullet$ @TO"The Packing Problem"@
	       
 	  	  
///

doc ///
     Key
     	  "Computing symbolic powers of an ideal"
     Description
     	 Text
	      Given an ideal, symbolicPower computes a given symbolic power.  
	 Example     
	      B = QQ[x,y,z];
	      I = ideal(x*(y^3-z^3),y*(z^3-y^3),z*(x^3-y^3));
	      J = symbolicPower(I,3)
     Description
         Text
              Various algorithms are used, in the following order:     
	      
	      1. If $I$ is squarefree monomial ideal, intersects the powers of the associated primes of $I$;
	      
	      2. If $I$ is monomial ideal, but not squarefree, takes an irredundant primary decomposition of $I$ and intersects the powers of those ideals;
	      
	      3. If $I$ is a saturated homogeneous ideal in a polynomial ring whose height is one less than the dimension of the ring, returns the saturation of $I^n$;
	      	      
	      4. If all the associated primes of $I$ have the same height, computes a primary decomposition of $I^n$ and intersects the components with radical $I$;
	      
	      5. If all else fails, compares the radicals of a primary decomposition of $I^n$ with the associated primes of $I$, and intersects the components corresponding to minimal primes.
///



doc ///
     Key
     	  "Alternative algorithm to compute the symbolic powers of a prime ideal in positive characteristic"
     Description
     	 Text
	      Given a prime ideal P in a regular ring of positive characteristic, symbPowerPrimePosChar computes its symbolic powers. Unfortunately, this algorithm is slower than others.  
	      
	 Example     
	      R = ZZ/7[x,y,z]
	      P = ker map(ZZ/7[t],R,{t^3,t^4,t^5})
	      J = symbPowerPrimePosChar(P,2)
	      
	 Text
	      The symbolic powers of P do not coincide with its powers.
	      
	 Example     
	      J == P^2
	      
	 Text
	      We can also test it a bit faster, without computing the symbolic powers of $P$.
	      
	 Example
	      isSymbolicEqualOrdinary(P,2)

///



doc ///
     Key
     	  "The Containment Problem"
     Description
     	 Text
	      Given an ideal $I$, we can determine if $I^{(m)} \subseteq I^n$. For example, here is an ideal that fails the containment $I^{(3)} \subseteq I^2$:
	      
	 Example     
	      B = ZZ/101[x,y,z];
	      I = ideal(x*(y^3-z^3),y*(z^3-x^3),z*(x^3-y^3));
	      isSymbPowerContainedinPower(I,3,2)
	      
     	 Text
	      We can also determine the smallest symbolic power contained in a given power.
	      
     	 Text
	      In our example, $I^{(4)}$ is the smallest symbolic power contained in $I^2$:
	      
	 Example
	      containmentProblem(I,2)
	      
     	 Text
	      We can ask the same question backwards: what is the largest power of I that contains $I^{(4)}$?
	      
	 Example
	      containmentProblem(I,4,InSymbolic => true)     
///



doc ///
     Key
     	  "Sullivant's algorithm for primes in a polynomial ring"
     Description
     	 Text
	      Given ideals I and J in a polynomial ring, we compute their join I*J:  
	      
	 Example     
	      S = QQ[x,y,z];
	      I = ideal(x^3,x^2*y^2,x*z^3,y^4,y^2*z);
	      J = joinIdeals(I,I)
	      
     	 Text
	      Following Seth Sullivant's "Combinatorial symbolic powers", J. Algebra 319 (2008), no. 1, 115--142, we can compute symbolic powers of prime ideals using join:
	      
	 Example     
	      A = QQ[x,y,z];
	      symbolicPowerJoin(ideal(x,y),2)  
	      
       	 Example
	      f = map(QQ[t],A,{t^3,t^4,t^5})
	      P = ker f;
	      symbolicPowerJoin(P,2)
///




doc ///
     Key
     	  "The Packing Problem"
     Description
     	 Text
	      Given a square-free monomial ideal $I$ of codimension $c$, $I$ is Konig if it contains a regular sequence of monomials of length $c$.
     	 
	      We can test if a given ideal is Konig:
	      
	 Example     
	      R = QQ[x,y,z]
	      I = ideal(x*y,z*y,x*z)
	      isKonig(I)
	      
     	 Text
	      $I$ is said to have the packing property if any ideal obtained from $I$ by setting any number of variables equal to 0 is Konig.
	      
	 Example     
	      isPacked(I)

     	 Text
	      Given an ideal that is not packed, we can determine which variable substitutions lead to ideals that are not Konig.

	 Example     
	      noPackedAllSubs(I)

     	 Text
	      We can obtained just one substitution leading to an ideal that is not Konig, or all such substitutions:

	 Example     
	      R = QQ[a,b,c,d,A,B,C,D]
	      J = ideal"ABCD,cdAB,abcD,bcABD,abcBD,abcdA,abcAD,abdAC,acdBC,acBCD,abcdC,bcdAC,bcACD,bcdAD,acdBD,adBCD,acABD,bdABC,adABC"
	      isPacked(J)
	      noPackedSub(J)

     	 Text
	      These can easily be tested:

	 Example     
	      L = substitute(J,matrix{{1,0,c,d,A,1,C,D}})
	      isKonig(L)

///




doc ///
   Key
       bigHeight
       (bigHeight, Ideal)
   Headline
       computes the big height of an ideal
   Usage
       bigHeight(I)
   Inputs
        I:Ideal
   Outputs
       :ZZ
           the largest height of an associated prime of I
   Description
       Text  
       	   Big height of an ideal: the largest height of an associated prime. 
           The algorithm is based on the following result by Eisenbud-Huneke-Vasconcelos, 
	   in their 1993 Inventiones Mathematicae paper:
	   
	   $\bullet$ codim $Ext^d(M,R) \geq d$ for all $d$
	   
	   $\bullet$ If $P$ is an associated prime of $M$ of codimension $d :=$ codim $P > $ codim $M$, 
	   then codim $Ext^d(M,R) = d$ and the annihilator of $Ext^d(M,R)$ is contained in $P$
	   
	   $\bullet$ If codim $Ext^d(M,R) = d$, then there really is an associated prime of codimension $d$.

       Example
           R = QQ[x,y,z,a,b]
     	   J = intersect(ideal(x,y,z),ideal(a,b))
    	   bigHeight(J)
   SeeAlso
       codim
       assPrimesHeight
   Caveat
       bigHeight works faster than assPrimesHeight
///


doc ///
   Key
       assPrimesHeight
       (assPrimesHeight, Ideal)
   Headline
       The heights of all associated primes
   Usage
       assPrimesHeight(I)
   Inputs
        I:Ideal
   Outputs
       :List
           the heights of all associated primes of I
   Description
       Text  
           The algorithm is based on the following result by Eisenbud-Huneke-Vasconcelos, 
	   in their 1993 Inventiones Mathematicae paper:
	   
	   $\bullet$ codim $Ext^d(M,R) \geq d$ for all $d$
	   
	   $\bullet$ If $P$ is an associated prime of $M$ of codimension $d :=$ codim $P > $ codim $M$, 
	   then codim $Ext^d(M,R) = d$ and the annihilator of $Ext^d(M,R)$ is contained in $P$
	   
	   $\bullet$ If codim $Ext^d(M,R) = d$, then there really is an associated prime of codimension $d$.	   

       Example
           R = QQ[x,y,z,a,b]
     	   J = intersect(ideal(x,y,z),ideal(a,b))
    	   assPrimesHeight(J)
   SeeAlso
       bigHeight
       codim
   Caveat
       bigHeight works faster than using assPrimesHeight and then taking the maximum
///


doc ///
   Key
       isSymbPowerContainedinPower
       (isSymbPowerContainedinPower, Ideal, ZZ, ZZ)
   Headline
       tests if the m-th symbolic power an ideal is contained the n-th power
   Usage
       isSymbPowerContainedinPower(I,m,n)
   Inputs
        I:Ideal
    	m:ZZ
    	n:ZZ
   Outputs
       :Boolean
           whether the m-th symbolic power of $I$ is contained in the n-th power
   Description
       Example
           R = QQ[x,y,z]
     	   J = ideal(x,y)
    	   isSymbPowerContainedinPower(J,3,2)
   SeeAlso
       containmentProblem
///


doc ///
   Key
       minimalPart
       (minimalPart, Ideal)
   Headline
       intersection of the minimal components
   Usage
       minimalPart(I)
   Inputs
        I:Ideal
   Outputs
       :Ideal
           the intersection of the components of I corresponding to minimal primes
   Description
       Text  
       	   Eliminates embedded components of a given ideal

       Example
           R = QQ[x,y,z]
     	   J = intersect(ideal(x^2,y,z^3),ideal(x,z))
    	   minimalPart(J)
   SeeAlso
       assPrimesHeight
       bigHeight
       symbolicPower
///




doc ///
   Key
       containmentProblem
       (containmentProblem, Ideal, ZZ)
   Headline
       computes the smallest symbolic power contained in a power of an ideal.
   Usage
       containmentProblem(I,n)
   Inputs
	I:Ideal
	n:ZZ
   Outputs
       :ZZ
           the minimum value m such that the m-th symbolic power of I is contained in I^n
   Description
       Text
       	   Given an ideal $I$ and an integer $n$, containementProblem returns the order of the smallest symbolic power of $I$ contained in $I^n$.

       Example
	   B = QQ[x,y,z];
	   f = map(QQ[t],B,{t^3,t^4,t^5})
	   I = ker f;
	   m = containmentProblem(I,2)
   SeeAlso
       isSymbPowerContainedinPower
///




doc ///
   Key
       symbPowerPrimePosChar
       (symbPowerPrimePosChar, Ideal, ZZ)
   Headline
       
   Usage
       	symbPowerPrimePosChar(I,n)
   Inputs
        I:Ideal
	n:ZZ
   Outputs
       :Ideal
           the n-th symbolic power of I
   Description
       Text  
           Given a prime ideal $I$ in a polynomial ring over a field of positive characteristic, and an integer $n$, 
	   this method returns the $n$-th symbolic power of $I$.  To compute $I^{(a)}$, find the largest value $k$ with 
	   $q = p^k \leq a$. Then $I^{(a)} = (I^{[q]} : I^{a-q+1})$.

       Example 
           B = ZZ/7[x,y,z];
	   f = map(ZZ/7[t],B,{t^3,t^4,t^5})
	   I = ker f;
	   symbPowerPrimePosChar(I,2)
   Caveat
       The ideal must be prime.
   SeeAlso
       symbolicPower
///



doc ///
   Key
       symbolicPower
       (symbolicPower, Ideal, ZZ)
   Headline
       computes the symbolic power of an ideal.
   Usage
       	symbolicPower(I,n)
   Inputs
        I:Ideal
	n:ZZ
   Outputs
       :Ideal
           the n-th symbolic power of I
   Description
       Text
              Given an ideal $I$ and an integer $n$, this method returns the $n$-th symbolic power of $I$. Various algorithms are used, in the following order:     
	      
	      1. If $I$ is squarefree monomial ideal, intersects the powers of the associated primes of $I$;
	      
	      2. If $I$ is monomial ideal, but not squarefree, takes an irredundant primary decomposition of $I$ and intersects the powers of those ideals;
	      
	      3. If $I$ is a saturated homogeneous ideal in a polynomial ring whose height is one less than the dimension of the ring, returns the saturation of $I^n$;
	      
	      4. If $I$ is an ideal with only degree one primary components, intersects the powers of the primary components of I.
	      
	      5. If all the associated primes of $I$ have the same height, computes a primary decomposition of $I^n$ and intersects the components with radical $I$;
	      
	      6. If all else fails, compares the radicals oyf a primary decomposition of $I^n$ with the associated primes of $I$, and intersects the components corresponding to minimal primes.
 
       Example
              B = QQ[x,y,z];
	      f = map(QQ[t],B,{t^3,t^4,t^5})
	      I = ker f;
	      symbolicPower(I,2)
	      
       Text
       	   When computing symbolic powers of a quasi-homogeneous ideal, the method runs faster if the ideal is changed to be homogeneous.
	   
       Example
       	   P = ker map(QQ[t],QQ[x,y,z],{t^3,t^4,t^5})
	   isHomogeneous P
	   time symbolicPower(P,4);
	   Q = ker map(QQ[t],QQ[x,y,z, Degrees => {3,4,5}],{t^3,t^4,t^5})
	   isHomogeneous Q
	   time symbolicPower(Q,4);

   SeeAlso
      symbPowerPrimePosChar
///



doc ///
   Key
       isSymbolicEqualOrdinary
       (isSymbolicEqualOrdinary, Ideal, ZZ)
   Headline
    	tests if symbolic power is equal to ordinary power       
   Usage
       	isSymbolicEqualOrdinary(I,n)
   Inputs
        I:Ideal
	n:ZZ
   Outputs
       :Boolean
           the truth value of $I^n==I^{(n)}$
   Description
       Text
           Given a radical ideal I and an integer $n$, this method returns true if and only if $I^n=I^{(n)}$. 
	   This method circumvents computing the symbolic powers in most cases, by first testing the @TO bigHeight@ of $I^n$

       Example
              B = QQ[x,y,z];
	      f = map(QQ[t],B,{t^3,t^4,t^5})
	      I = ker f;
	      isSymbolicEqualOrdinary(I,2)
   SeeAlso
      isSymbPowerContainedinPower
///



doc ///
   Key
       joinIdeals
       (joinIdeals,Ideal,Ideal)
   Headline
       Computes the join of the given ideals
   Usage
       joinIdeals(Ideal,Ideal)
   Inputs
        I:Ideal
	J:Ideal
   Outputs
       :Ideal
       	   the join of I and J
   Description
       Text    
           This method uses Seth Sullivant's results found in "Combinatorial symbolic powers", J. Algebra 319 (2008), no. 1, 115-142.

       Example
           S = QQ[x,y,z];
	   I = ideal(x^3,x^2*y^2,x*z^3,y^4,y^2*z);
	   J = joinIdeals(I,I)
///


doc ///
     Key 
         symbolicPowerJoin
	 (symbolicPowerJoin,Ideal,ZZ)
     Headline 
         computes the symbolic power of the prime ideal using join of ideals.
     Usage 
         symbolicPowerJoin(P,n)
     Inputs 
	  P:Ideal
	  n:ZZ
     Outputs
          :Ideal
	      the n-th symbolic power of P
     Description	  
       Text
	   Computes the $n$-th symbolic power of the prime ideal $P$, using join of ideals.
	   
	   This is the algorithm in Seth Sullivant's "Combinatorial symbolic powers", J. Algebra 319 (2008), no. 1, 115-142.

       Example 
	   A = QQ[x,y,z];
	   symbolicPowerJoin(ideal(x,y),2) 
     SeeAlso 
	  joinIdeals
///




doc ///
     Key 
         squarefreeGens
	 (squarefreeGens,Ideal)
     Headline 
         returns all square-free monomials in a minimal generating set of the given ideal.
     Usage 
         squarefreeGens(I)
     Inputs 
     	  I:Ideal
     Outputs
          :List
     Description	  
       Text
	   Given a monomial ideal $I$, returns all square-free monomials in a minimal generating set of $I$.

       Example 
	   R = QQ[x,y,z];
	   I = ideal(x*y,y*z,x^2);
	   squarefreeGens(I) 
     SeeAlso 
	  squarefreeInCodim
/// 

doc ///
     Key 
         squarefreeInCodim
	 (squarefreeInCodim,Ideal)
     Headline 
         finds square-fee monomials in ideal raised to the power of the codimension.
     Usage 
         squarefreeInCodim(I)
     Inputs 
     	  I:Ideal
     Outputs
          :List
     Description	  
       Text
	   Given a monomial ideal $I$, returns all square-free monomials in a minimal generating set of $I^c$ where $c$ is the codimension of $I$.

       Example 
	   R = QQ[x,y,z];
	   I = ideal(x*y,y*z,x*z);
	   squarefreeInCodim(I) 
     SeeAlso 
	  squarefreeGens
///



doc ///
     Key 
         isKonig
	 (isKonig,Ideal)
     Headline 
         determines if a given square-free ideal is Konig.
     Usage 
         isKonig(I)
     Inputs 
     	  I:Ideal
     Outputs
          :Boolean
     Description	  
       Text
	   Given a square-free monomial ideal $I$, determines if the ideal is Konig. 
	   A square-free monomial ideal $I$ of codimension $c$ is Konig if it contains a regular sequence of monomials of length $c$.

       Example 
	   R = QQ[x,y,z];
	   I = ideal(x*y,y*z,x*z);
	   isKonig(I) 
     SeeAlso 
	  squarefreeGens
///

doc ///
     Key 
         isPacked
	 (isPacked,Ideal)
     Headline 
         determines if a given square-free ideal is packed.
     Usage 
         isPacked(I)
     Inputs 
     	  I:Ideal
     Outputs
          :Boolean
     Description	  
       Text
	   Given a square-free monomial ideal $I$, determines if the ideal is Konig.
	   A square-free monomial ideal $I$ of codimension $c$ is packed if every ideal obtained from it by replacing any number of variables by 1 or 0 is Konig.

       Example 
	   R = QQ[x,y,z];
	   I = ideal(x*y,y*z,x*z);
	   isPacked(I) 
     SeeAlso 
	  squarefreeGens
///


doc ///
     Key 
         noPackedSub
	 (noPackedSub,Ideal)
     Headline 
         finds a substitution of variables by 1 and/or 0 for which an ideal is not Konig.
     Usage 
         noPackedSub(I)
     Inputs 
     	  I:Ideal
     Outputs
          :List
     Description	  
       Text
	   Given an ideal that is not packed, returns a substitution of variables by 0 and/or 1 that produces an ideal that is not Konig.
	   Determines only one such substitutions, even though others may exist.

       Example 
	   R = QQ[x,y,z];
	   I = ideal(x*y,y*z,x*z);
	   noPackedSub(I) 
     SeeAlso 
	  isPacked	  
///
	  

doc ///
     Key 
         noPackedAllSubs
	 (noPackedAllSubs,Ideal)
     Headline 
         finds all substitutions of variables by 1 and/or 0 for which ideal is not Konig.
     Usage 
         noPackedAllSubs(I)
     Inputs 
     	  I:Ideal
     Outputs
          :List
     Description	  
       Text
	   Given an ideal that is not packed, returns a list with all substitution of variables by 0 and/or 1 that produces an ideal that is not Konig.

       Example 
	   R = QQ[x,y,z];
	   I = ideal(x*y,y*z,x*z);
	   noPackedAllSubs(I) 
     SeeAlso 
	  noPackedSub
	  isPacked	  
///

doc ///
     Key 
         minDegreeSymbPower
	 (minDegreeSymbPower,Ideal,ZZ)
     Headline 
         returns the minimal degree of a given symbolic power of an ideal.
     Usage 
         minDegreeSymbPower(Ideal,ZZ)
     Inputs 
     	  I:Ideal
	  n:ZZ
     Outputs
          :ZZ
     Description	  
       Text
	   Given an ideal $I$ and an integer $n$, returns the minimal degree of an element in $I^{(n)}$.

       Example 
	   T = QQ[x,y,z];
	   I = intersect(ideal"x,y",ideal"x,z",ideal"y,z")
	   minDegreeSymbPower(I,2)

///

doc ///
     Key 
         lowerBoundResurgence
	 (lowerBoundResurgence,Ideal)
     Headline 
         computes a lower bound for the resurgence of a given ideal.
     Description
       Text
           The resurgence of an ideal $I$, defined by Harbourne and Bocci, is given by
	 $\rho(I) :=$ sup $\lbrace a/b : I^{(a)}$ &nsub; $I^b \rbrace.$
     Usage 
         lowerBoundResurgence(Ideal)
     Inputs 
     	  I:Ideal
     Outputs
          :QQ
     Description	  
       Text
	   Given an ideal $I$, finds the maximum of the quotients $m/k$ that fail $I^{(m)} \subseteq I^k$ with $k \leq$ the optional input SampleSize.

       Example 
	   T = QQ[x,y,z];
	   I = intersect(ideal"x,y",ideal"x,z",ideal"y,z");
	   lowerBoundResurgence(I)

///

doc ///
     Key 
         UseWaldschmidt
     Headline 
         optional input for computing a lower bound for the resurgence of a given ideal.
     Usage 
         lowerBoundResurgence(Ideal,UseWaldschmidt=>true)
     Inputs 
     	  I:Ideal
     Outputs
          :QQ
     Description	  
       Text
	   Given an ideal $I$ and an integer $n$, returns the larger value between the 
	   maximum of the quotients $m/k$ that fail $I^{(m)} \subseteq I^k$ with $k \leq n$ 
	   and $\frac{\alpha(I)}{waldschmidt(I)}$. 

       Example 
	   T = QQ[x,y,z];
	   I = intersect(ideal"x,y",ideal"x,z",ideal"y,z");
	   lowerBoundResurgence(I,UseWaldschmidt=>true)

///


doc ///
     Key 
         [lowerBoundResurgence,SampleSize]
     Headline 
         optional parameter used for approximating asymptotic invariants that are defined as limits.
     Usage 
         lowerBoundResurgence(I,SampleSize=>ZZ)
     Description	  
         Text
       	   Given an ideal $I$ and an integer $n$, returns the larger of the two numbers $\frac{\alpha(I)}{waldschmidt(I)}$ 
	   and the maximum of the quotients $m/k$ that fail $I^{(m)} \subseteq I^k$ with $k \leq$ @TO SampleSize@.
     
         Example
           R = QQ[x,y,z];
	   J = ideal (x*(y^3-z^3),y*(z^3-x^3),z*(x^3-y^3));
	   lowerBoundResurgence(J, SampleSize=>5)
///


doc ///
     Key 
         [lowerBoundResurgence,UseWaldschmidt]
     Headline 
         optional input for computing a lower bound for the resurgence of a given ideal.
     Usage 
         lowerBoundResurgence(Ideal,UseWaldschmidt=>true)
     Inputs 
     	  I:Ideal
     Outputs
          :QQ
     Description	  
       Text
	   Given an ideal $I$ and an integer $n$, returns the larger value between the 
	   maximum of the quotients $m/k$ that fail $I^{(m)} \subseteq I^k$ with $k \leq$ @TO SampleSize@ 
	   and $\frac{\alpha(I)}{waldschmidt(I)}$. 

       Example 
	   T = QQ[x,y,z];
	   I = intersect(ideal"x,y",ideal"x,z",ideal"y,z");
	   lowerBoundResurgence(I,UseWaldschmidt=>true)

///



doc ///
     Key 
         symbolicPolyhedron
	 (symbolicPolyhedron,Ideal)
	 (symbolicPolyhedron,MonomialIdeal)
     Headline 
         computes the symbolic polyhedron for a monomial ideal. 
     Usage 
         symbolicPolyhedron(I)
     Inputs 
     	  I:Ideal
     Outputs
          :Polyhedron 
     Description	  
       Text
	   The symbolic polyhedron associated to a monomial ideal $I$ is defined in the paper "Symbolic Powers of Monomial Ideals" 
	   by S. M. Cooper, R. J. D. Embree, H. T. Ha, A. H. Hoefel. The symbolic polyhedron contains the exponent vector of any
	   monomial in $I^n$ scaled by $1/n$.
	  
       Text
       	   This function uses the Polyhedra package and returns an object of type Polyhedron.
       
       Example 
	   R = QQ[x,y,z];
	   I = ideal(x*y,y*z,x*z);
	   symbolicPolyhedron(I)
        
     SeeAlso
	  Polyhedra
///

doc ///
     Key 
         waldschmidt
	 (waldschmidt,Ideal)
	 (waldschmidt,MonomialIdeal)
     Headline 
         computes the Waldschmidt constant for a homogeneous ideal. 
     Usage 
         waldschmidt(I)
     Inputs 
     	  I:Ideal
     Outputs
          :QQ 
     Description	  
       Text
	   The Waldschmidt constant for a homogeneous ideal I is defined as $waldschmidt(I)=lim_{n\to\infty} \frac{\alpha(I^{(n)})}{n}$, 
	   where $\alpha(J)$ denotes the smallest degree of a nonzero element in a given homogeneous ideal $J$. The limit of the sequence 
	   $\frac{\alpha(I^{(n)})}{n}$ exists because of the subadditivity of $\alpha$ and is equal to the infimum of the sequence $\frac{\alpha(I^{(n)})}{n}$.
	  
       Text
       	   The Waldschmidt constant can be computed for monomial ideals as the smallest value of the sum of the coordinates over all the points of 
	   the symbolic polyhedron. The function uses this method to return an exact answer for the Waldschmidt constant of a monomial ideal.
	   
       Text
       	   For ideals that are not monomial, we give an approximation of the Waldschmidt constant by taking the minimum value of $\frac{\alpha(I^{(n)})}{n}$
	   over a finite number of exponents $n$, namely for $n$ from 1 to the optional parameter @TO SampleSize@.  
       
       Example 
	   R = QQ[x,y,z];
	   I = ideal(x*y,y*z,x*z);
	   waldschmidt(I)
	   
       Example 
	   R = QQ[x,y,z];
	   J = ideal (x*(y^3-z^3),y*(z^3-x^3),z*(x^3-y^3));
	   waldschmidt(J, SampleSize=>5)
        
     SeeAlso 
	  symbolicPolyhedron
///



doc ///
     Key 
         SampleSize
     Headline 
         optional parameter used for approximating asymptotic invariants that are defined as limits.
     SeeAlso
     	 waldschmidt
	 lowerBoundResurgence
	 asymptoticRegularity    
///	   

doc ///
     Key 
         [waldschmidt,SampleSize]
     Headline 
         optional parameter used for approximating asymptotic invariants that are defined as limits.
     Usage 
         waldschmidt(I,SampleSize=>ZZ)
     Description	  
         Text
       	   For ideals that are not monomial, we give an approximation of the Waldschmidt constant by taking the minimum value of $\frac{\alpha(I^{(n)})}{n}$
	   over a finite number of exponents $n$, namely for $n$ from 1 to the optional parameter @TO SampleSize@. Similarly the @TO SampleSize@ is used to give an
	   approximation for the asymptotic regularity by computing the smallest value of $\frac{reg(I^{(n)})}{n}$ for $n$ from
	   1 to the @TO SampleSize@.
     
         Example
           R = QQ[x,y,z];
	   J = ideal (x*(y^3-z^3),y*(z^3-x^3),z*(x^3-y^3));
	   waldschmidt(J, SampleSize=>5)
///	   

doc ///
     Key 
         [asymptoticRegularity,SampleSize]
     Headline 
         optional parameter used for approximating asymptotic invariants that are defined as limits.
     Usage 
         asymptoticRegularity(I,SampleSize=>ZZ)
     Description	  
         Text
       	   We give an approximation of the asymptotic regularity by taking the minimum value of $\frac{reg(I^{(n)})}{n}$
	   over a finite number of exponents $n$, namely for $n$ from 1 to the optional parameter @TO SampleSize@;
	   the default value for @TO SampleSize@ is 10. 
     
         Example
           R = QQ[x,y,z];
	   J = ideal (x*(y^3-z^3),y*(z^3-x^3),z*(x^3-y^3));
	   asymptoticRegularity(J, SampleSize=>5)
///	   


doc ///
     Key 
         InSymbolic
     Headline 
         an optional parameter used in containmentProblem.
     SeeAlso
     	 containmentProblem

///	   

doc ///
     Key 
       	 [containmentProblem,InSymbolic]
     Headline 
         an optional parameter used in containmentProblem.
     Usage 
         containmentProblem(I,n,InSymbolic => true)
     Description	  
         Text
       	   Given an ideal I and an integer n, @TO InSymbolic@ is used to ask the following question:
	   What is the largest power containing the symbolic power $I^{(n)}$?

         Example
           R = QQ[x,y,z];
	   J = ideal (x*(y^3-z^3),y*(z^3-x^3),z*(x^3-y^3));
	   containmentProblem(J,3,InSymbolic => true)
///	   
	   
doc ///
     Key 
         asymptoticRegularity
	 (asymptoticRegularity,Ideal)
     Headline 
         approximates the asymptotic regularity 
     Usage 
         asymptoticRegularity(I,SampleSize=>ZZ)
     Description
         Text	  
       	   We give an approximation of the asymptotic regularity by taking the minimum value of $\frac{reg(I^{(n)})}{n}$
	   over a finite number of exponents $n$, namely for $n$ from 1 to the optional parameter @TO SampleSize@;
	    the default value for @TO SampleSize@ is 10. 
     
         Example
           R = QQ[x,y,z];
	   J = ideal (x*(y^3-z^3),y*(z^3-x^3),z*(x^3-y^3));
	   asymptoticRegularity(J, SampleSize=>5)
///

doc ///
   Key
        symbolicDefect
        (symbolicDefect, Ideal, ZZ)
   Headline
    	computes the symbolic defect of an ideal
   Usage
         symbolicDefect(I,m)
   Inputs
         I:Ideal
         m:ZZ
   Outputs
          :ZZ
             the size of a minimal generating set of the m-th symbolic power of I modulo I^m.
   Description
       Text
       	   Given an ideal $I$ and integer $m$, this method returns the size of a minimal generating set for the $m$-th symbolic power of $I$ modulo $I^m$.

       Example
         R = QQ[x,y,z]    
         I = ideal(x*y,x*z,y*z);					      
	 symbolicDefect(I,2)
 ///

doc ///
    Key
    	UseMinimalPrimes
    	[symbolicPower,UseMinimalPrimes]
    	[isSymbPowerContainedinPower,UseMinimalPrimes]
    	[symbolicDefect,UseMinimalPrimes]	
    	[containmentProblem,UseMinimalPrimes]
    Headline	 
    	an option to only use minimal primes to calculate symbolic powers
    Description
    	Text
	    The default value is false. When defined to be true, the symbolic power is calculated as defined in Villarreal. 
	    In particular, @TO symbolicPower@ has the option to restrict to minimal primes 
	    versus use all associated primes with @TO UseMinimalPrimes@. With this option the
	    symbolic power of an ideal $I$ is defined as 
	    $$I^{(n)} = \cap_{p \in Min(R/I)}(I^nR_p \cap R ),$$
	    where $Min(R/I)$, is the set of minimal primes in $I$.
	    
       Text
       	   R. Villarreal. "Monomial algebras" Second edition. Monographs and Research Notes in Mathematics. CRC Press, Boca Raton, FL, 2015. xviii+686 pp. ISBN: 978-1-4822-3469-5.

    SeeAlso
    	containmentProblem
	isSymbPowerContainedinPower
	symbolicDefect	  
	symbolicPower
///

doc /// 
    Key
    	CIPrimes
	[symbolicPower,CIPrimes]
	[isSymbPowerContainedinPower,CIPrimes]
	[symbolicDefect,CIPrimes]
	[containmentProblem,CIPrimes]
    Headline
    	compute the symbolic power by taking the intersection of the powers of the primary components
    Description
    	Text
	    The default value is false.  When defined to be true, the @TO symbolicPower@ function tests whether the primary components are complete intersections having the same height.  If each component is, then the function takes the intersection of the powers of the components:
	     $$I^{(n)}=\cap_{p\in Ass(R/I)} p^n.$$
    SeeAlso
    	containmentProblem
	isSymbPowerContainedinPower
	symbolicDefect
	symbolicPower
///


-- tests

TEST ///
   S = QQ[x,y,z];
   I = ideal(x,y,z);
   assert(isSymbPowerContainedinPower(I,2,2) == true)
///

--bigHeight
TEST ///
R=ZZ/2[x,y,z]
I=ideal(x,y)
assert(bigHeight(I)==2)
///

TEST ///
R=QQ[x,y,z]
I=ideal(x,y^3,z^2)
assert(bigHeight I==3)
///


TEST ///
R=QQ[x,y,z]
I=ideal(x*(y^3-z^3),y*(z^3-x^3),z*(x^3-y^3))
assert(bigHeight(I)==2)
///


--symbolicPower
TEST ///
R=QQ[x,y,z]
I=ideal(y-z,x+z)
assert(symbolicPower(I,2)==ideal(y^2-2*y*z+z^2,x*y-x*z+y*z-z^2,x^2+2*x*z+z^2))
///

TEST ///
R=QQ[x,y,z]
I=ideal(x)
assert(symbolicPower(I,2)==ideal(x^2))
///

TEST ///
R=QQ[x,y,z]
I=ideal(x+1)
assert(symbolicPower(I,2)==ideal(x^2+2*x+1))
///

TEST ///
R=QQ[w,x,y,z]
I=ideal(x*y+1,w*y*z)
assert(symbolicPower(I,3)==ideal(w^3*z^3,w^2*x*y*z^2+w^2*z^2,w*x^2*y^2*z+2*w*x*y*z+w*z,x^3*y^3+3*x^2*y^2+3*x*y+1))
///

TEST ///
R = QQ[x,y,z]
I = ideal"x,y"
assert(symbolicPower(I,2)==I^2)
///


TEST ///
R=QQ[x,y,z]
I=ideal(x*y+x*z)
assert(symbolicPower(I,2)==ideal((x*y+x*z)^2))
///

TEST ///
R=QQ[x,y,z]
I=ideal(x*(y^5-z^5),y*(z^5-x^5),z*(x^5-y^5))
assert(symbolicPower(I,3,CIPrimes => true)==symbolicPower(I,3))
///

--isSymbPowerContainedinPower
TEST ///
R=QQ[x,y];

I=ideal(x);

assert(isSymbPowerContainedinPower(I,2,3)==false)
///

TEST ///
R=QQ[x,y];

I=ideal(x);

assert(isSymbPowerContainedinPower(I,2,2)==true)
///

TEST ///
R=QQ[x,y];

I=ideal(x);

assert(isSymbPowerContainedinPower(I,3,2)==true)
///

--ContainmentProblem

TEST ///
R=QQ[x,y,z];

I=ideal(x*y,x*z,y*z);

assert(containmentProblem(I,2)==3)
///

TEST ///
R=QQ[x,y,z]
I=ideal(x*(y^3-z^3),y*(z^3-x^3),z*(x^3-y^3))
assert(containmentProblem(I,2)==4)
///

--frobeniusPower
--TEST ///
--R=ZZ/3[x,y]
--I=ideal(x*y^2+1,x^2)
--assert(frobeniusPower(I,9)==ideal(x^9*y^(19)+1,x^(18)))
--///

--lowerBoundResurgence
TEST ///
R=QQ[x,y,z]
I=ideal(x*y,x*z,y*z)
assert(lowerBoundResurgence(I)==6/5)
///

----isSymbolicEqualOrdinary
TEST ///
R=QQ[x,y,z]
I=ideal(x*y,x*z,y*z)
assert(isSymbolicEqualOrdinary(I,2)==false)
///

TEST ///
R=ZZ/3[x,y]
I=ideal(x)
assert(isSymbolicEqualOrdinary(I,3)==true)
///

TEST ///
R=QQ[x,y,z]
I=ideal(x*z,y*z)
assert(isSymbolicEqualOrdinary(I,2)==true)
///

----joinIdeals
TEST ///
R=QQ[x,y,z]
I=ideal(x,y)
J=ideal(x,z)
assert(joinIdeals(I,J)==ideal(x))
///

----symbolicPowerJoin

----containmentProblem given Symbolic Power
TEST ///
R=QQ[x,y,z]
I=ideal(x*(y^3-z^3),y*(z^3-x^3),z*(x^3-y^3))
assert(containmentProblem(I,4,InSymbolic => true)==2)
///

----squarefreeGens
TEST ///
R=ZZ/5[w,x,y,z]
I=ideal(y^2*z,x*y*w,z*w^3)
assert(squarefreeGens(I)=={w*x*y})
///

TEST ///
R=QQ[x,y,z]
I=ideal(x^2*z,x*y^8,z^3)
assert(squarefreeGens(I)=={})
///

----squarefreeInCodim
TEST ///
R=QQ[x,y,z]
I=ideal(x,y^2)
assert(squarefreeInCodim I=={})
///

TEST ///
R=ZZ/2[x,y,z]
I=ideal(x,y)
assert(squarefreeInCodim I=={x*y})
///

-- symbolicPolyhedron
TEST ///
 needsPackage"Polyhedra"
 R = QQ[x,y,z];
 I = ideal(x*y,y*z,x*z);
 assert((vertices symbolicPolyhedron I)== matrix{{1,1,0,1/2},{1,0,1,1/2},{0,1,1,1/2}})
///

-- waldschmidt
TEST ///
 R = QQ[x,y,z];
 I = ideal(x*y,y*z,x*z);
 assert(waldschmidt(I)==3/2)
///

TEST ///
R=QQ[x,y,z,Degrees=>{9,11,13}];
S=QQ[t]
I=kernel(map(S,R,{t^9,t^11,t^13}))
J=ideal(apply(flatten entries gens gb I,leadTerm))
assert(waldschmidt(I)==22)
assert(waldschmidt(J)==22)
///

----isKonig
TEST ///
R=ZZ/17[x]
I=ideal(1_R)
assert(isKonig(I)==true)
///

TEST ///
R=QQ[y,z]
I=ideal(0_R)
assert(isKonig(I)==true)
///

TEST ///
R=QQ[x,y,z]
I=ideal(x*y,x*z,y*z)
assert(isKonig(I)==false)
///

----isPacked

TEST ///
R=QQ[x,y,z]
I=ideal(x*y,x*z,y*z)
assert(isPacked(I)==false)
///


TEST ///
R=QQ[x,y,z,a,b]
I=intersect(ideal(x,y),ideal(x,z),ideal(z,a),ideal(y,a),ideal(x,b))
assert(isPacked(I)==true)
///

----noPackedSub
TEST ///
R=QQ[x,y,z]
I=ideal(x*y,x*z,y*z)
assert(noPackedSub(I)=="The ideal itself is not Konig!")
///


----noPackedAllSubs
TEST ///
R=QQ[x,y,z]
I=ideal(x*y,x*z,y*z)
assert(noPackedAllSubs I=="Only I is not Konig -- all proper substitutions are Konig.")
///

TEST ///
R=QQ[x_1..x_6]
I=intersect(ideal(x_1,x_2),ideal(x_2,x_3),ideal(x_3,x_1),ideal(x_3,x_4),ideal(x_4,x_5),ideal(x_5,x_6),ideal(x_6,x_4))
assert(noPackedAllSubs I=={{"x_1=>0", "x_2=>1", "x_3=>1"}, {"x_2=>0", "x_1=>1", "x_3=>1"}, {"x_5=>0", "x_4=>1", "x_6=>1"}, {"x_6=>0", "x_4=>1", "x_5=>1"}})
///

end

restart
uninstallPackage"SymbolicPowers"
loadPackage"SymbolicPowers"

restart
uninstallPackage"SymbolicPowers"
restart
installPackage"SymbolicPowers"
viewHelp"SymbolicPowers"
check"SymbolicPowers"

needsPackage"SymbolicPowers"
R = QQ[x,y]
I = ideal"x+y"
symbolicPolyhedron I

restart
-- Paper Example Ideal of height dim R-1
loadPackage "SymbolicPowers";
R=QQ[x,y,z];
I=ideal(x*(y^3-z^3),y*(z^3-x^3),z*(x^3-y^3));
symbolicPower(I,3);
symbolicPower(I,3)==saturate(I^3)

-- Paper Example Primary ideals
restart
loadPackage "SymbolicPowers";
R=QQ[w,x,y,z]/(x*y-z^2);
I=ideal(x,z);
symbolicPower(I,2)

restart
-- Paper Example Monomial Ideal
loadPackage "SymbolicPowers";
R = QQ[x,y,z];
I = ideal(x*y,x*z,y*z)
symbolicPower(I,2)

restart
-- Paper Example Containment Problem
loadPackage "SymbolicPowers";
R=QQ[x,y,z];
I=ideal(x*(y^3-z^3),y*(z^3-x^3),z*(x^3-y^3));
containmentProblem(I,2)

restart
-- Paper Example Waldschmidt constants of monomial ideals
loadPackage "SymbolicPowers";
R=QQ[x,y,z];
I=ideal(x*y,x*z,y*z);
symbolicPolyhedron(I)
waldschmidt I

restart
-- Paper Example Waldschmidt constants of arbitrary ideals
loadPackage "SymbolicPowers";
R=QQ[x,y,z];
I=ideal(x*(y^3-z^3),y*(z^3-x^3),z*(x^3-y^3));
waldschmidt I


lowerBoundResurgence(I)



------testing CIPRimes
loadPackage "SymbolicPowers"
loadPackage "Points"
I=randomPoints(2,10);
time symbolicPower(I,6,CIPrimes=>true);
time symbolicPower(I,6);
