-*
   Copyright 2020, Luigi Ferraro, Federico Galetto,
   Francesca Gandini, Hang Huang, Matthew Mastroeni, Xianglong Ni.

   You may redistribute this file under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 2 of
   the License, or any later version.
*-

-------------------------------------------
--- RingOfInvariants methods --------------
-------------------------------------------

RingOfInvariants = new Type of HashTable   

invariantRing = method(Options => {
	Strategy => UseNormaliz,
	UseLinearAlgebra => false,
	UseCoefficientRing => false,
	DegreeBound => infinity
	})

invariantRing GroupAction := RingOfInvariants => o -> G -> (

    new RingOfInvariants from {
	cache => new CacheTable from { (symbol generators) => invariants(G, o) },
	(symbol ambient) => ring G, 
	(symbol action) => G
	}
    )

PolynomialRing^GroupAction := RingOfInvariants => (R, G) -> (
    if ring G =!= R then (error "Expected the first argument to be the polynomial ring on which the actions acts.");
    invariantRing G
    )

QuotientRing^LinearlyReductiveAction := RingOfInvariants => (Q, L) -> (
    if ring L =!= Q then (error "Expected the first argument to be the ring on which the actions acts.");
    invariantRing L
    )

-------------------------------------------

net RingOfInvariants := S -> (
    comma := ", ";
    n := wrap(printWidth, "-", toString horizontalJoin deepSplice (
	net coefficientRing ambient S,
	"[",
	toSequence between(comma, apply(S.cache.generators, f -> net f)),
	"]"
	));
    if not zero ideal ambient S then (
	n = horizontalJoin(n," / ",net ideal ambient S);
	);
    return n;
    )

action = method()

action RingOfInvariants := GroupAction => S -> S.action

ambient RingOfInvariants := PolynomialRing => S -> S.ambient

generators RingOfInvariants := List => null -> S -> S.cache.generators


-------------------------------------------

definingIdeal = method(Options => {Variable => "u"})

definingIdeal RingOfInvariants := opts -> S -> (
    u := getSymbol opts.Variable;
    local J;
    if S.cache#?definingIdeal then (
	J = S.cache#definingIdeal;
	if first baseName first (ring J)_* == u then return J
	);
    R := ambient S;
    K := coefficientRing R;
    L := generators S;
    n := #L;
    gdegs := L / degree // flatten;
    U := K[u_1..u_n, Degrees => gdegs];
    J = ker map(R,U,L);
    S.cache#(symbol definingIdeal) = J;
    return J
    )


-------------------------------------------

hilbertSeries RingOfInvariants := Divide => opts -> S -> (
    hilbertSeries(definingIdeal S, Order => opts.Order, Reduce => opts.Reduce)
    )


-------------------------------------------

reynoldsOperator = method()

reynoldsOperator (RingElement, FiniteGroupAction) := RingElement => (f, G) -> (
    R := ring G;
    if not instance(f, R) then (error "reynoldsOperator: Expected an element from the ring on which 
	the group acts.");
    if #(group G)%(char coefficientRing R) == 0 then (error "reynoldsOperator: The Reynolds 
	operator is not defined when the characteristic of the coefficient field divides the 
	order of the group.");
    (1/#(group G))*(sum apply(group G, g -> sub(f, (vars R)*(transpose g) ) ) )
    )

reynoldsOperator (RingElement, DiagonalAction) := RingElement => (f, D) -> sum select(terms f, m -> isInvariant(m, D) )

-------------------------------------------

invariants = method(Options => {
	Strategy => UseNormaliz,
	UseLinearAlgebra => false,
	UseCoefficientRing => false,
	DegreeBound => infinity,
	DegreeLimit => {},
	SubringLimit => infinity
	})

invariants DiagonalAction := List => o -> D -> (
    (W1, W2) := weights D;
    R := ring D;
    kk := coefficientRing R;
    p := char kk;
    d := cyclicFactors D;
    r := rank D;
    if p > 0 and o.UseCoefficientRing then (
	q := kk#order;
	if any(d, j -> q%j =!= 1) then (
	    print "-- Diagonal action is not defined over the given coefficient ring. \n-- Returning invariants over an infinite extension field over which the action is defined."
	    )
	else (
	    D' := diagonalAction(W1||W2, apply(r, i -> q - 1)|d, R);
	    return invariants D'
	    )
    	);
    R = kk[R_*, MonomialOrder => GLex];
    g := numgens D;
    n := dim D;
    mons := R_*;
    local C, local S, local U;
    local v, local m, local v', local u;
    
    if g > 0 then (
	t := product d;
	
	reduceWeight := w -> vector apply(g, i -> w_i%d#i);
	
	C = apply(n, i -> reduceWeight W2_i);
	
	S = new MutableHashTable from apply(C, w -> w => {});
	scan(#mons, i -> S#(reduceWeight W2_i) = S#(reduceWeight W2_i)|{mons#i});
	U = R_*;
	
	while  #U > 0 do(
	    m = min U; 
	    v = first exponents m;
	    k := max positions(v, i -> i > 0);
	    v = reduceWeight(W2*(vector v));
	    
	    while k < n do(
	    	u = m*R_k;
	    	v' = reduceWeight(v + W2_k);
	    	if (not S#?v') then S#v' = {};
	    	if all(S#v', m' -> u%m' =!= 0_R) then (
		    S#v' = S#v'|{u};
		    if first degree u < t then U = U | {u}
		    );
	    	k = k + 1;
	    	);
	    U = delete(m, U);
	    );
    	if S#?(0_(ZZ^g)) then mons = S#(0_(ZZ^g)) else mons = {}
    	);
    if r == 0 then return apply(mons, m -> sub(m, ring D) );
    
    W1 = W1*(transpose matrix (mons/exponents/first));
    if o.Strategy == UsePolyhedra then (
	if r == 1 then C = convexHull W1 else C = convexHull( 2*r*W1|(-2*r*W1) );
	C = (latticePoints C)/vector;
	)
    else if o.Strategy == UseNormaliz then (
	if r == 1 then C = (normaliz(transpose W1, "polytope"))#"gen" 
	else C = (normaliz(transpose (2*r*W1|(-2*r*W1)), "polytope"))#"gen";
	C = transpose C_(apply(r, i -> i));
	C = apply(numColumns C, j -> C_j)
	);
    
    S = new MutableHashTable from apply(C, w -> w => {});
    scan(#mons, i -> S#(W1_i) = S#(W1_i)|{mons#i});
    U = new MutableHashTable from S;
    
    nonemptyU := select(keys U, w -> #(U#w) > 0);
    while  #nonemptyU > 0 do(
	v = first nonemptyU;
	m = first (U#v);
	
	scan(#mons, i -> (
		u := m*mons#i;
        	v' := v + W1_i;
        	if ((U#?v') and all(S#v', m' -> (
			    if u%m' =!= 0_R then true
			    else if g > 0 then (
				m'' := u//m';
			    	v'' := reduceWeight(W2*(vector first exponents m''));
			    	v'' =!= 0_(ZZ^g)
				)
			    else false
			    )
			)
		    ) 
		then( 
                    S#v' = S#v'|{u};
                    U#v' = U#v'|{u};
		    )
	    	)
	    );
	U#v = delete(m, U#v);
	nonemptyU = select(keys U, w -> #(U#w) > 0)
	);
    
    if S#?(0_(ZZ^r)) then mons = S#(0_(ZZ^r)) else mons = {};
    return apply(mons, m -> sub(m, ring D) )
    )


-------------------------------------------

manualTrim = method(TypicalValue => List)

manualTrim (List) := List => L -> (
    if not L#?0 then return L;
    L' := {0_(ring L#0)};
    
    scan(#L, i -> (
	if not (L#i % ideal(L') == 0) then L' = append(L', L#i)
    ));
    return drop(L',1)
)


-------------------------------------------
-- Computes an *additive* basis for the degree d part of the
-- invariant ring following Algorithm 4.5.1 of Derksen-Kemper.
invariants (LinearlyReductiveAction, List) := List => o -> (V,d) -> (
    M := actionMatrix V;
    Q := ring V;
    A := groupIdeal V;
    n := #(gens Q);
    K := coefficientRing ring groupIdeal V;
    x := local x, z := local z;
    
    l := #(gens ring M);
    S := Q**K[z_1..z_l];
    M' := sub(M, apply(l, i -> (ring M)_i => S_(n+i)));
    A' := sub(A, apply(l, i -> (ring M)_i => S_(n+i)));
    
    L := sub(basis(d,Q), S);
    if zero L then return {};
    if L == id_(S^1) then return {1_Q};
    r := numColumns L;
    NFDL := apply(r, i -> (sub(L_(0,i), apply(n, j -> S_j => sum(n, k -> M'_(k,j) * S_k))) - L_(0,i)) % A');
    monomialsNFDL := flatten entries monomials(matrix{NFDL});
    m := #monomialsNFDL;
    B := matrix(apply(m, i -> apply(r, j -> coefficient(monomialsNFDL#i, NFDL#j))));
    KB := gens kernel B;
    return flatten entries sub(L * KB, join(apply(n, i -> S_i => Q_i), apply(l, i -> S_(n+i) => 0)))
)

invariants (LinearlyReductiveAction, ZZ) := List => o -> (V,d) -> (
    invariants(V,{d})
    )

invariants (LinearlyReductiveAction) := List => o -> V -> (
    I := hilbertIdeal(V,DegreeLimit=>o.DegreeLimit,SubringLimit=>o.SubringLimit);
    Q := ring V;
    n := #(gens Q);
    K := coefficientRing ring groupIdeal V;
    x := local x;
    X := K[x_1..x_n];
    
    degreeList := sort unique apply(I_*, i -> degree i);
    generatorList := {};
    
    local d;
    while (#degreeList > 0) do(
	d = degreeList#0;
    	Id := select(I_*, i -> degree i == d);
	
	alreadyInv := true;
	j := 0;
	while alreadyInv and Id#?j do(
	    if not isInvariant(Id#j,V) then alreadyInv = false;
	    j = j+1
	);
    	if not alreadyInv then (
	    generatorList = join(generatorList, invariants(V,d))
	) else (
	    generatorList = join(generatorList, Id);
	);
    	degreeList = drop(degreeList,1)
    );
    return manualTrim generatorList
)

-------------------------------------------
-- invariants for finite groups
-------------------------------------------

-- Below is an implementation of King's algorithm following
-- Derksen-Kemper Algorithm 3.8.2 for the non-modular case
invariants FiniteGroupAction := List => o -> G -> (
    R := ring G; 
    S := {}; 
    b := #(group G); 
    if ( char(R) != 0 and b % char(R) == 0 ) then 
    error "Not implemented in the modular case";
    if unique degrees R =!= {{1}} then
    error "Only implemented for standard graded polynomial rings";
    if o.DegreeBound < b then b = o.DegreeBound;
    local M;
    for d from 1 to b do (
    	Gb := gb(promote(ideal S,R),DegreeLimit=>d);
	I := monomialIdeal leadTerm Gb;
	M = reverse select(flatten entries (basis(d,R)%I),m->not zero m);
	if M === {} then break else (
	    if o.UseLinearAlgebra then (
		for f in invariants(G,d) do (
	    	    g := f % Gb;
	    	    if not zero g then (
		    	S = S | {f};
		    	Gb = forceGB ( (gens Gb) | matrix{{g}} );
	    	    	);
		    );
	    	) else (
	    	for m in M do (
	    	    f := reynoldsOperator(m,G);
	    	    g := f % Gb;
	    	    if not zero g then (
		    	S = S | {f};
		    	Gb = forceGB ( (gens Gb) | matrix{{g}} );
	    	    	);
		    );
	    	);
    	    );
	);
    if M =!= {} then print"
Warning: stopping condition not met!
Output may not generate the entire ring of invariants.
Increase value of DegreeBound.
";
    if char(R) == 0 then (
	S = apply(S,s->(mingens ideal s)_(0,0));
	);
    return S;
    )

-- the following is an implementation of the linear algebra
-- method to compute invariants of a given degree for finite groups
-- following §3.1.1 of Derksen-Kemper
invariants(FiniteGroupAction, List) := List => o -> (G,d) -> (
    R := ring G;
    K := coefficientRing R;
    B := basis(d,R);
    L := apply(gens G, g -> sub(B,(vars R)*(transpose g)) - B);
    L = apply(L, l -> last coefficients l);
    M := sub(matrix pack(L,1),K);
    C := gens ker M;
    I := B*sub(C,R);
    flatten entries I
    )

invariants(FiniteGroupAction, ZZ) := List => o -> (G,d) -> (
    invariants(G,{d})
    )

-------------------------------------------

isInvariant = method()

isInvariant (RingElement, FiniteGroupAction) := Boolean => (f, G) -> reynoldsOperator(f, G) == f

isInvariant (RingElement, DiagonalAction) := Boolean => (f, D) -> (
    if not instance(f, ring D) then (
	error "isInvariant: Expected an element from the ring on which the group acts."
	);
    (W1, W2) := weights D;
    d := cyclicFactors D;
    torus := W1 * transpose(matrix (exponents f) ); 
    finite := W2 * transpose(matrix (exponents f) );
    return ( torus == 0 and all(#d, i -> finite_0_i%d#i == 0) )
    )

isInvariant (RingElement, LinearlyReductiveAction) := Boolean => (f, V) -> (
    A := groupIdeal V;
    M := actionMatrix V;
    R := ring(f);
    if (numColumns M =!= numRows M) or (numRows M =!= #(gens R)) then print "Matrix size does not match polynomial ring";
    x := local x, z := local z;
    n := numColumns M;
    K := coefficientRing(ring(f));
    l := #(gens ring M);
    
    S := K[x_1..x_n, z_1..z_l];
    M' := sub(M, apply(l, i -> (ring M)_i => z_(i+1)));
    A' := sub(A, apply(l, i -> (ring M)_i => z_(i+1)));
    f' := sub(f, apply(n, i -> (ring(f))_i => x_(i+1)));
    Gf' := sub(f, apply(n, i -> (ring(f))_i => sum(n, j -> M'_(j,i) * x_(j+1))));
    return ( (Gf' - f') % A' == 0 )
)





