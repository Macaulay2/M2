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
makeLogMonomial = g -> Product(makeLogTerm \ select(first \ toList factor g, f -> {0} < degree f))

factorial' = alpha -> (first exponents alpha) / (k -> k!) // product

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
    makeLogMonomial \ f \ sort values S
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

-- FIXME
TEST /// -- test solveFrobeniusIdeal
  R = QQ[t_1..t_5]
  I = ideal(R_0+R_1+R_2+R_3+R_4, R_0+R_1-R_3, R_1+R_2-R_3, R_0*R_2, R_1*R_3)
  F = solveFrobeniusIdeal I
  end
  (A, B, C, D1, D2) = value \ (F_0, F_1#0#1, F_2#0#1, F_3#0#1, F_3#1#1)
  assert(A == 1)
  assert(B == (t_1^2*t_3^2)/(t_2^3*t_4))
  assert(C == (t_1*t_3)/(t_2*t_5))
  assert(D1 == (t_2*t_4)/t_5^2)
  assert(D2 == (t_1^2*t_3^2*t_4)/(t_2*t_5^4))
  assert(C^2 * B^-1 == D1)
  assert(C^4 * B^-1 == D2)
///

end;
--------------------
--------------------

restart; --
--uninstallPackage "Dmodules"
path = prepend("~/Desktop/Workshop-2019-Minneapolis/M2/Macaulay2/packages/", path);
needsPackage "Dmodules";
check Dmodules
viewHelp cssExpts
viewHelp Dmodules


