--add export command: 
--isTorusFixed
--distraction
--indicialIdeal
--cssExpts
--cssExptsMult
-- 
--CB 15 Dec 2022: What is our output command for solutions? Did we finish printing log solutions? 



--Input: J an ideal in a Weyl algebra
--Output: True if ideal is torus fixed, as in SST Lem. 2.3.1. False if not.
isTorusFixed = method();
isTorusFixed(Ideal) := Boolean => (J)->(
    n := numgens ring J//2;
    J' := ideal flatten apply(J_*,f->( 
	    apply(apbFactor(f
		    ),v->(		
		    a := take(v#0,n)|apply(n,i-> 0);
		    b := apply(n,i-> 0)|drop(v#0,n);
		    pTheta := sum apply(v#1,u->( u#0*((ring J)_((u#1)|(u#1)) )));
		    (ring J)_a*pTheta*(ring J)_b
		    ))));
    J == J'
    )

--Input: element in Weyl algebra in a torus-fixed ideal
--Output: list of the form {list a, list b, list of coefficients and exponents for poly p}, as in SST Lem. 2.3.1 
-- internal
apbFactor = method();
apbFactor(RingElement) := List => (f) -> (
    n := (numgens ring f)//2;
    E := exponents f;
    D := ring f;
    createDpairs D;
    if D_* != (flatten D.dpairVars) then error "The variables in this Weyl algebra are in the wrong order.";
    --Each exponent pair (a,b) for writing terms in f as x^a*p(theta)*\del^b, as in the proof of SST Lem. 2.3.1:
    abList := apply(E,e->(
	    thetaExp := apply(n,i-> min(e#(n+i),e#i)); --We already checked above that the Dpairs match up as coded here.
	    e-(thetaExp|thetaExp)
	    ));
    --List of unique exponent pairs (a,b) that occur in abList:
    UabList := unique abList;
    --The positions of each term in f that corresponds to an element in UabList:
    abPos := apply(UabList,u-> positions(abList,i-> i==u));
    C := flatten entries (coefficients f)#1;
    --Return list of lists of form {a,b,coeff of p(theta), expts of p(theta)}, corresponding to rearrangement of f:
    apply(#abPos,l->(
    	{ UabList#l, apply(abPos#l,j->( {C#j, drop(E#j-UabList#l,n)} )) }
    ))
)

--Input: b is a List of length n (= numVars of ambient space for D)
--       S is a ring for the output
--Output: [theta]_b as in SST p.68
-- internal
thetaBracketSub = method();
thetaBracketSub(List,Ring) := (RingElement) => (b,S)->( 
    n := length b;
    if n != numgens S then error "length of exponent vector does not match number of variables";
    product apply(n,i-> product apply(b#i,j-> S_i - j))
    )

--Input: element in D in a torus fixed ideal
--Output: List of corresponding gens from homogeneous pieces in the distraction, viewed in ring S
genToDistractionGens = method();
genToDistractionGens(RingElement,Ring) := List => (f,S) -> (
    n := (numgens ring f)//2;
    if n != numgens S then error "mismatched numbers of variables";
    apbF := apbFactor(f);
    sum apply(apbF,q->( 
         b:= drop(q#0,n);
	 --
	 pTheta := sum apply(q#1,v->( sub(v#0,S)*( 
		     product apply(length v#1,k->( product apply(v#1#k,i->( S_k-i))))
		     )));
	 phi := map(S,S,S_*-b);
	 pThetaMinusb := phi(pTheta);
	 thetaBracketSub(b,S)*pThetaMinusb
	 ))
)

--this was called thetaIdeal in the past		    
--Input: torus-fixed left D-ideal J
--Output: the distraction of J, viewed in ring S
distraction = method(); 
distraction(Ideal, Ring) := Ideal => (J,S) ->(
    n := numgens ring J//2;
    if n != numgens S then error "mismatched numbers of variables";
    ideal flatten apply(J_*,j-> genToDistractionGens(j,S))
)


--Input: holonomic ideal I, weight w in the form of a List
--Output: the indicial ideal of I with respect to w
indicialIdeal = method();
indicialIdeal(Ideal,List) := (Ideal) => (I,w) ->(
    distraction(inw(I,flatten{-w|w}))
    )

--Input: 0-dimensional primary ideal I
--Output: corresponding point, as a vector
-- internal
solveMax = method();
solveMax Ideal := List => I -> first entries lift(vars ring I % radical I, coefficientRing ring I)

--internal method
--Input: holonomic D-ideal H, weight vector w as List, half the number of variables in H
--Output: list of 0-dimensional ideals encoding css exponents and their multiplicities
-- internal
beginExptComp = method();
beginExptComp(Ideal,List,ZZ,Ring) := List => (H,w,n,S)->(
    	if not isHolonomic(H) then error "ideal is not holonomic";
	if #w != n then error "weight vector has wrong length";
	J := inw(H,(-w)|w);
    	if not isTorusFixed(J) then error "ideal is not torus-fixed"; 
        primaryDecomposition distraction(J,S)
	)

--Input: holonomic D-ideal H, weight vector w as List
--Output: list of starting monomial exponents for H wrt w
cssExpts = method();
cssExpts(Ideal,List) := List => (H,w)->(
	n:= (numgens ring H)//2;
	t := symbol t;
	S := QQ(monoid [t_1..t_n]);
    	L := beginExptComp(H,w,n,S);
    	apply(L,l-> solveMax(l))
	)

--Input: holonomic D-ideal H, weight vector w as List, 
--Output: list of starting monomial exponents for H wrt w, with multiplicities
cssExptsMult = method();
cssExptsMult(Ideal,List) := List => (H,w)->(
	n:= (numgens ring H)//2;
	t := symbol t;
	S := QQ(monoid [t_1..t_n]);
    	L := beginExptComp(H,w,n,S);
    	apply(L,l->( {degree l,solveMax(l)}))
	)    

-- making monomial expressions with arbitrary exponents
-- internal
makeMonomial = (R, L) -> Product apply(L,
    (i, e) -> if e == 1 then expression R_i else Power(expression R_i, expression e))
makeRationalMonomial = (R, p) -> (
    A := select(pairs  p,  (i, e) -> e > 0);
    B := select(pairs(-p), (i, e) -> e > 0);
    num := makeMonomial(R, A);
    if #B == 0 then num else
    num / makeMonomial(R, B))

makeLogTerm = f -> (
    W := ring f;
    thetas := matrix {product \ pack(2, mingle drop(W.dpairVars, -1))};
    (expression log) makeRationalMonomial(first W.dpairVars,
    first entries transpose lift(
	last coefficients(f, Monomials => thetas), ZZ)))
makeLogMonomial = g -> Product(makeLogTerm \ select(first \ toList g, f -> {0} < degree f))

factorial' = alpha -> (first exponents alpha) / (k -> k!) // product

-- TODO: move elsewhere
RingMap Sum :=
RingMap Product := (f, v) -> apply(v, e -> f e)
RingMap Power := (f, v) -> Power{f v#0, v#1}

-- Perform a lexographic breadth first search on monomials in k[x_1..x_n] \ S_< (I)
-- and compute c#(alpha, beta) as in Algorithm 2.3.14 of SST (pp. 74)
--Input:  an Ideal, zero-dimensional Frobenius m-primary ideal, and the Weyl algebra
--Output: a HashTable, { t^beta => f_beta } 
solvePrimaryFrobeniusIdeal = method();
solvePrimaryFrobeniusIdeal(Ideal, Ring) := List => (I, W) -> (
    R := ring I;
    n := # gens R;
    f := map(W, R, product \ pack(2, mingle drop(W.dpairVars, -1)));
    if dim I > 0 then error "expected zero-dimensional ideal";
    -- standard monomials S_<(I)
    S := new MutableHashTable from apply( first entries basis (R^1/I), elt -> (elt, elt) );
    B := sort keys S;
    -- the coefficients c#(alpha,beta)
    c := new MutableHashTable from {};
    -- non-standard monomials N_<(I)
    N := new MutableHashTable from flatten for i to n - 1 list for beta in B list R_i * beta => (i, beta);
    -- monomials that we have already visited
    M := new MutableHashTable from S;

    while #N > 0 do (
    	-- the vals in the hash table can be potentially used to speed up computation
	(alpha, vals) := min pairs N;
	remove(N, alpha);
	M#alpha = true;

	if S#?alpha then continue;

	lambda := alpha % I; -- computes the normal form
	if lambda == 0 then continue;

	coeffs := last coefficients(lambda, Monomials => B);
	for j to #B - 1 do c#(alpha, B_j) = coeffs_0_j;
	apply(B, beta -> if degree beta == degree alpha then S#beta = S#beta + c#(alpha, beta) * factorial' beta / factorial' alpha * alpha);

	-- Add the product of alpha and generators of R to N
	-- TODO: R_j * lambda is easier to reduce, so add that instead?
	for j to n - 1 do if not M#?(R_j * alpha) then N#(R_j * alpha) = (j, alpha);
	);
    makeLogMonomial \ f \ factor \ sort values S
    -- hashTable apply(pairs S, (k, v) -> (k, factor v))
    )

solveFrobeniusIdeal = method();
solveFrobeniusIdeal Ideal        := List =>  I     -> solveFrobeniusIdeal(I, makeWeylAlgebra ring I)
solveFrobeniusIdeal(Ideal, Ring) := List => (I, W) -> (
    R := ring I;
    n := # gens R;
    createDpairs W;
    flatten apply(primaryDecomposition I, C -> (
	if dim C > 0 then error "expected zero-dimensional components";
	(p, m) := (solveMax C, degree C); -- the point and its multiplicity
	--mon := makeRationalMonomial(first W.dpairVars, p);
	mon := makeMonomial(first W.dpairVars, select(pairs p, (i, e) -> e != 0));
	if m == 1 then return mon;
	psi := map(R, R, apply(n, i -> R_i + p_i));
	apply(solvePrimaryFrobeniusIdeal(psi C, W), ell -> mon * ell)
	))
    )

cssLeadTerm = method()
cssLeadTerm(Ideal, List) := List => (I, w) -> (
    createThetaRing(W := ring I);
    J := inw(I, flatten{-w|w});
    solveFrobeniusIdeal(distraction(J, W.ThetaRing), W))

--------------------
-- Tests section
--------------------

-- TODO: add assertions
TEST /// -- test solveFrobeniusIdeal
  R = QQ[t_1..t_5]
  I = ideal(R_0+R_1+R_2+R_3+R_4, R_0+R_1-R_3, R_1+R_2-R_3, R_0*R_2, R_1*R_3)
  F = solveFrobeniusIdeal I
///

end;
--------------------
--------------------

restart
path = prepend("~/Desktop/Workshop-2019-Minneapolis/M2/Macaulay2/packages/", path);
needsPackage "HolonomicSystems"
needsPackage "Polyhedra"
check HolonomicSystems
viewHelp cssExpts
viewHelp Dmodules



--Input: I regular holonomic ideal in a Weyl algebra on n vars, weight vector w in \ZZ^n as a List
--Output: list of generators of gbw(I) times monomial in variables, so that all inw terms have w-weight zero
nonpositiveWeightGens = method()
nonpositiveWeightGens(Ideal, List) := List => (I,w) ->( 
    
    )
S = QQ[x]
W = makeWeylAlgebra S;
I = ideal(x*dx*(x*dx-3)-x*(x*dx+101)*(x*dx+13))
w = {1}


--Input: I regular holonomic ideal in a Weyl algebra on n vars, weight vector w in \ZZ^n as a List
--Output: Cone containing support of the Nilsson cone for css of I with weight w 
nilssonSupportCone = method()
nilssonSupportCone(Ideal, List) := Cone => (I,w) ->(
    n := (numgens ring I)//2;
    if length w != n then error "Expected a weight vector of length (numgens ring I)//2";
    fw := flatten{-w,w};
    G := flatten entries gens gbw(I,fw);
    HS := 0;
    maxW := 0;
    scan(G,g->( 
	    --lead term of g:
	    LTg := inw(g,fw);
	    newWcond := (matrix{(exponents LTg)#0})*(transpose matrix{fw});
	    --remaining terms of g:	    
	    RTg := select(terms g, i-> i !=LTg);
	    scan(RTg, m->(
    	    	   newHSeq := matrix{-drop(flatten exponents m,-n)+drop(flatten exponents m,n)};
		   if HS == 0 then HS = newHSeq else HS = HS||newHSeq;
	    	   if maxW == 0 then maxW = newWcond else maxW = maxW||newWcond;
			));
    		));
	tailCone polar polyhedronFromHData(HS, maxW)
	)
    
    
A = matrix{{1,1,1,1,1},{1,1,0,-1,0},{0,1,1,-1,0}}
beta = {1,0,0}
I = gkz(A,beta)
w = {1,1,1,1,0}
--help gbw

C = nilssonSupportCone(I,w)
k=3


--Input: cone for support of Nilsson series, weight k
--Output: lattice points in cone of weight \leq k, as a List of Lists
nilssonSupportTruncated = method()
nilssonSupportTruncated(Cone, List, ZZ) := List => (C,w,k)->(
    P := polyhedronFromHData((halfspaces C)||matrix{w},map(target halfspaces C,ZZ^1,0)||matrix{{-k}},hyperplanes C, map(target hyperplanes C,ZZ^1,0));
    latticePoints P
    )

--Input: I regular holonomic ideal in a Weyl algebra on n vars, weight vector w in \ZZ^n as a List
--Output: starting monomials for the css for I for weight w, as a List of ring elements in vars for I and their logs
nilssonStart = method()
nilssonStart(Ideal, List) := List => (I,w)->( 
    
    )
