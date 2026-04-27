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
apbFactor RingElement := List => f -> (
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

distraction = method()
--Input: torus-fixed left D-ideal J
--Output: the distraction of J, viewed in ThetaRing
--Note: this was called thetaIdeal in the past
distraction(Ideal, Ring) := Ideal => (J, ThetaRing) -> sum(J_*, j -> ideal distraction(j, ThetaRing))
--Input: element in a torus-fixed left D-ideal
--Output: list of corresponding gens from homogeneous pieces in the distraction, viewed in ThetaRing
distraction(RingElement, Ring) := List => (f, ThetaRing) -> (
    if (n := numgens ring f // 2) != numgens ThetaRing then error "mismatched numbers of variables";
    sum(apbFactor f, q -> ( 
	    b := drop(q#0, n);
	    phi := map(ThetaRing, ThetaRing, ThetaRing_* - b);
	    pTheta := phi sum(q#1, v -> sub(v#0, ThetaRing) * product(#v#1, k -> product(v#1#k, i -> ThetaRing_k-i )));
	    thetaBracketSub(b, ThetaRing) * pTheta))
    )

--Input: holonomic ideal I, weight w in the form of a List
--Output: the indicial ideal of I with respect to w
indicialIdeal = method();
indicialIdeal(Ideal, List) := Ideal => (I, w) -> distraction(inw(I, -w|w), first createThetaRing ring I)

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

--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- TODO: move this to AssociatedAlgebras?
FreeAlgebraQuotient _ List := RingElement => (R, v) -> if #v === 0 then 1_R else product(
    if isListOfIntegers v then pairs v else v, (i, e) -> R_i^e )

-- TODO: move this to Core?
listForm Number := n -> {({}, n)}

protect NilssonRing
nilssonRing = method()
nilssonRing Ring := (cacheValue symbol NilssonRing) (W -> (
    n := numgens W // 2;
    X := local X;
    dX := local dX;
    logX := local logX;
    S := QQ<| dX_0..dX_(n-1), X_0..X_(n-1), logX_0..logX_(n-1) |> / flatten apply(n, i -> nonnull join(
	    flatten table(n, {X, logX, dX}, (j, sym) -> if i <  j then sym_i * sym_j - sym_j * sym_i ),
	    flatten table(n, {X, logX},     (j, sym) -> if i != j then  dX_i * sym_j - sym_j *  dX_i ),
	    -* FIXME: only missing dX*logX - 1/X - logX*dX, since 1/X is not allowed in this ring *-
	    apply(n, j -> X_i * logX_j - logX_j * X_i ), { dX_i*X_i-1-X_i*dX_i, X_i*dX_i*logX_i-1-X_i*logX_i*dX_i }));
    WtoS := f -> sum(listForm f, (m, c) -> c * S_(reverse toList pairs flatten reverse pack_n m));
    StoW := f -> sum(listForm f, (m, c) -> c * W_(reverse drop(m, -1)));
    S, WtoS, StoW))
--------------------------------------------------------------------------------

-- Perform a lexographic breadth first search on monomials in k[x_1..x_n] \ S_< (I)
-- and compute c#(alpha, beta) as in Algorithm 2.3.14 of SST (pp. 74)
--Input:  an Ideal, zero-dimensional Frobenius m-primary ideal, and the Weyl algebra
--Output: a HashTable, { t^beta => f_beta } 
solvePrimaryFrobeniusIdeal = method();
solvePrimaryFrobeniusIdeal(Ideal, Ring) := List => (I, W) -> (
    T := ring I;
    n := numgens T;
    if dim I > 0 then error "expected zero-dimensional ideal";
    -- standard monomials S_<(I)
    S := new MutableHashTable from apply( first entries basis (T^1/I), elt -> (elt, elt) );
    B := sort keys S;
    -- the coefficients c#(alpha,beta)
    c := new MutableHashTable from {};
    -- non-standard monomials N_<(I)
    N := new MutableHashTable from flatten for i to n - 1 list for beta in B list T_i * beta => (i, beta);
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

	-- Add the product of alpha and generators of T to N
	-- TODO: T_j * lambda is easier to reduce, so add that instead?
	for j to n - 1 do if not M#?(T_j * alpha) then N#(T_j * alpha) = (j, alpha);
	);
    -- hashTable apply(pairs S, (k, v) -> (k, factor v))
    -- f := map(W, T, product \ pack(2, mingle drop(W.dpairVars, -1)));
    -- makeLogMonomial \ f \ factor \ sort values S
    (R, f1, f2) := elapsedTime nilssonRing W;
    g := map(R, T, R_*_{2*n .. 3*n-1});
    g \ sort values S)

solveFrobeniusIdeal = method();
solveFrobeniusIdeal Ideal        := List =>  I     -> solveFrobeniusIdeal(I, makeWeylAlgebra ring I)
solveFrobeniusIdeal(Ideal, Ring) := List => (I, W) -> (
    R := ring I;
    n := numgens R;
    T := first nilssonRing W;
    createDpairs W;
    flatten apply(primaryDecomposition I, C -> (
	if dim C > 0 then error "expected zero-dimensional components";
	(p, m) := (solveMax C, degree C); -- the point and its multiplicity
	--mon := makeRationalMonomial(first W.dpairVars, p);
	mon := if any(p, i -> floor i != i or i < 0)
	then makeMonomial(first W.dpairVars, select(pairs p, (i, e) -> e != 0))
	--FIXME: version above works for negative or rational exponents, the one below doesn't
	else T_(flatten{toList(n:0), apply(p, i -> sub(i, ZZ)), toList(n:0)});
	if m == 1 then return mon;
	psi := map(R, R, apply(n, i -> R_i + p_i));
	apply(solvePrimaryFrobeniusIdeal(psi C, W), ell -> mon * ell)
	))
    )

--Input: I regular holonomic ideal in a Weyl algebra on n vars, weight vector w in \ZZ^n as a List
--Output: starting monomials for the css for I for weight w, as a List of ring elements in vars for I and their logs
cssLeadTerm = method()
cssLeadTerm(Ideal, List) := List => (I, w) -> solveFrobeniusIdeal(indicialIdeal(I, w), ring I)

--TODO: where should this go?
--Input: cone C, weight vector k
--Output: truncate cone containing points of weight \leq k
truncate(Cone, List, ZZ) := List => {} >> o -> (C, w, k) -> polyhedronFromHData(
    matrix  {w}  || -halfspaces C,
    matrix {{k}} || map(target halfspaces C, ZZ^1, 0),
    hyperplanes C, map(target hyperplanes C, ZZ^1, 0))

--Input: I regular holonomic ideal in a Weyl algebra on n vars, weight vector w in \ZZ^n as a List
--Output: Cone containing support of the Nilsson cone for css of I with weight w 
--NOTE: We are assuming that the ideal I is provided with LT of weight 0.
--We will adjust to this case using nonpositiveWeightGens
nilssonSupport = method()
nilssonSupport(Ideal, List) := Cone => (I, w) -> (
    -- See description before SST Thm 2.5.14
    n := length w;
    G := gbw(I, fw := -w|w);
    L := transpose flatten for g in G_* list (
	--lead term of g:
	LTg := inw(g, fw); -- FIXME: why is this not a single term?
	vec := matrix { first exponents LTg } * transpose matrix { fw };
	--loop over the remaining terms of g:
	apply(first \ exponents \ (terms g - set { LTg }),
	    m -> { matrix { take(m, -n) - take(m, n) }, vec }));
    tailCone polar polyhedronFromHData(concatRows first L, concatRows last L))
--Input: I regular holonomic ideal in a Weyl algebra on n vars, weight vector w in \ZZ^n as a List, weight k
--Output: lattice points in cone of weight \leq k, as a List of Lists
nilssonSupport(Ideal, List, ZZ) := List => (I, w, k) -> entries transpose concatCols latticePoints truncate(nilssonSupport(I, w), w, k)

--Input: I regular holonomic ideal in a Weyl algebra on n vars, weight vector w in \ZZ^n as a List
--Output: list of generators of gbw(I) times monomial in variables, so that all inw terms have w-weight zero
nonpositiveWeightGens = method()
nonpositiveWeightGens(Ideal, List) := List => (I, w) -> (
    W := ring I;
    n := length w;
    G := gbw(I, fw := -w|w);
    apply(G_*, g -> (
	    e1 := first exponents inw(g, fw);
	    e2 := take(e1, -n) - take(e1, n);
	    epos := apply(e2, i -> max(0, i));
	    eneg := apply(e2, i -> max(0, -i));
	    W_epos * g // W_eneg))
    )

truncatedCanonicalSeries = method()
truncatedCanonicalSeries(Ideal, List, ZZ) := List => (I, w, k) -> (
    W := ring I;
    n := numgens W // 2;
    r := holonomicRank I;
    G := ideal nonpositiveWeightGens(I, w);
    (S, WtoS, StoW) := nilssonRing W;
    -- FIXME: this step fails if any start terms have negative or rational exponents
    A := WtoS \ value \ cssLeadTerm(G, w);
    V := elapsedTime nilssonSupport(G, w, k);
    -- The variables of S are ordered as dX_i..., X_i..., logX_i...
    B := splice table(V, (n:0)..(n:r-1), (e, l) -> S_(toList join(n:0,e,l)));
    B  = B - set apply((n:0)..(n:r-1), l -> S_(toList join(n:0,n:0,l)));
    G' := WtoS \ G_*;
    G', apply(A, a -> (
	    B' := a * B - set A;
	    M := concatCols flatten table(G', B', (g, b) -> last coefficients(g * b, Monomials => B));
	    v := concatCols apply(G', g -> last coefficients(g * a, Monomials => B));
	    first first entries(a + matrix{B'} * solve(sub(M, QQ), sub(-v, QQ)))))
    )

--------------------
-- Tests section
--------------------

-- TODO: add assertions
TEST /// -- test solveFrobeniusIdeal
  S = QQ[x_1..x_5]
  W = makeWeylAlgebra S
  T = first createThetaRing W
  -- see SST Example 2.3.16
  w = {1,1,1,1,1}
  J = ideal(T_0+T_1+T_2+T_3+T_4, T_0+T_1-T_3, T_1+T_2-T_3, T_0*T_2, T_1*T_3)
  F = solveFrobeniusIdeal J
  g = map(W, T, apply(5, i -> W_i*W_(i+5)))
  cssLeadTerm(g J, w)
  -- FIXME
  --truncatedCanonicalSeries(g J, w, 5)
///

TEST ///
  W = makeWeylAlgebra(QQ[x]);
  I = ideal(x*dx*(x*dx-3)-x*(x*dx+101)*(x*dx+13))
  w = {1};
  nilssonSupport(I,w)
  nilssonSupport(I,w,3)
  cssLeadTerm(I, w)
  (G, sols) = truncatedCanonicalSeries(I, w, 4);
  table(G, sols, (g, s) -> (g * s)[0,x,0])

  -- A = matrix{{1,1,1,1,1},{1,1,0,-1,0},{0,1,1,-1,0}}
  -- TODO: do SST eq. (1.22)
  -- See SST pp. 26
  A = matrix{{1,0,0,-1},{0,1,0,1},{0,0,1,1}}
  beta = {1,0,0}
  I = gkz(A,beta)
  w = {1,1,1,0}
  nilssonSupport(I,w)
  nilssonSupport(I,w,3)
  cssLeadTerm(I, w)
  --(G, sols) = truncatedCanonicalSeries(I, w, 4);
///

TEST ///
  R = QQ[x]
  W = makeWeylAlgebra R
  w = {1}
  I = ideal(x*dx*(x*dx-3) - x*(x*dx+10)*(x*dx+20))
  -- FIXME: crashes when k = 3; see https://github.com/Macaulay2/M2/issues/2831
  (G, sols) = truncatedCanonicalSeries(I, w, 4)
  netList G
  netList sols
  -- error terms:
  table(G, sols, (g, s) -> (g * s)[0,x,0])
///

end--
restart
path = prepend("~/Desktop/Workshop-2019-Minneapolis/M2/Macaulay2/packages/", path);
installPackage "WeylAlgebras"
needsPackage "HolonomicSystems"
check HolonomicSystems
viewHelp HolonomicSystems

