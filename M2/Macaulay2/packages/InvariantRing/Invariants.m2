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
	Strategy => "Default",
	UseCoefficientRing => false,
	UsePolyhedra => false,
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

texMath RingOfInvariants := S -> (
    G := gens S;
    texString := (texMath coefficientRing ambient S);
    if #G != 0 then (
	texString |= "\\left[" |
	concatenate mingle(apply(G, g -> texMath g),toList(#G-1:", ")) | "\\right]";
	);
    if not zero ideal ambient S then (
	L := (ideal ambient S)_*;
	texString |= " / \\left(" |
	concatenate mingle(apply(L, l -> texMath l),toList(#L-1:", ")) | "\\right)";
	);
    texString
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

-------------------------------------------
--- NEW April 20th, 2026 ------------------
--- Elementary Invariants Methods ---------
-------------------------------------------

-*
-->-- Function: seedMinimal --<--
-- auxiliary unexported function for elementaryInvariants
-- Checks if a given candidate is minimal given the list of seeds, starting at the index, "startIndex"
--> INPUT:   
--    Seeds      : List[List[ZZ]] › A list of current seeds that are invariant
--    Candidate  : List[ZZ]       › A seed that may be added to the Seeds list
--    startIndex : ZZ             › The index in the Seeds list that you want to start minimizing from
--> OUTPUT: 
--    Returns ({-1} | candidate) if our candidate is minimal.
--    Returns {-2} if our candidate is not minimal.
--    Returns ({-3, index}) if a seed is not minimal, with the index of the seed. 
--    Returns ({-4, index} | newSeed) if our candidate helps reduce a seed. 
seedMinimal := (Seeds, candidate, startIndex) -> (
	i := startIndex;
	numSeeds := #Seeds;
	while i < numSeeds do (	-- Iterate through all seeds
		candMinimal := true;	-- We start by assuming our candidate & seed are divisible by each other
		seedIsMinimal := true;

		for k from 0 to #candidate - 1 do (
			if (candidate#k > Seeds#i#k) then (
				candMinimal = false; -- If our candidate ever has a greater power than the seed, its not minimal
			)
			else if (Seeds#i#k > candidate#k) then (
				seedIsMinimal = false; -- If our seed ever has a greater power than the candidate, our candidate is safe (for now)
			);
		);
		if seedIsMinimal then (		-- If our seed is exclusively less than our candidate, we:
			newCandidate := candidate - Seeds#i; -- Reduce our candidate by our seed, as that will still be invariant
			if (newCandidate === candidate) then (
				i = i + 1;				-- No change: move forward to avoid infinite loop
			)
			else (
				candidate = newCandidate;
				if (all (candidate, i -> i == 0)) then return {-2, 0}; -- If the candidate fully reduces, we return -2.
				i = startIndex;		-- Reduce our index by restarting so we can test candidate again. 
			);
		)
		else if candMinimal then (	-- If our candidate is minimal, we may need to discard our seed instead.
			removalSeed := Seeds#i;
			while (all(removalSeed - candidate, i -> i >= 0) and removalSeed =!= removalSeed - candidate) do (
				removalSeed = removalSeed - candidate;
			);
			if (all (removalSeed, i -> i == 0)) then return {-3, i}; -- If the seed fully reduces, we return -3
			return {-4, i} | removalSeed;						  -- Otherwise, we return -3, the index of the seed, and the updated seed.
		)
		else (
			i = i + 1;	-- Normal case: move forward
		);
	);
	return {-1, 0} | candidate ;
)
*-

-->--elementaryInvariants function--<--
-- unexported, called with invariants(D,Strategy=>"Elementary");
--> INPUT:  D (a diagonalAction)
--> OUTPUT: L (a list of invariants)
elementaryInvariants := D -> (
    ---------------------
    -- Seed Generation --
    ---------------------

    -->- Grab our variables W, R, Z from D -<--
    W := D.weights_1;
    R := ring D;
    Z := (D.cyclicFactors)#0;

    -->- Find our m and n from the weight matrix -<--
    n := numColumns W; m := numRows W;

    -->- STEP 1 -<--
    -->- Now, we find a n x n submatrix of W with maximal rank --<-
    -- compute RREF of weight matrix
    rref := reducedRowEchelonForm promote(W,QQ);
    -- find columns containing pivots, remove nulls from zero rows
    pivs := delete(null, apply(entries rref, r -> position(r, i -> i == 1)) );
    nonZeroSM := submatrix(W,pivs); -- the submatrix
    colList := toList( set(0..n-1) - set(pivs) ); -- columns without pivots

    -->- STEP 2 -<--
    -- Creates a list for the seed invariants in exponent vec form
    -- Iterates through all columns we didn't use for nonZeroSM
    seedList := for v in colList list (
	-- Matrix we extract the seed invariant from (where W_v is our additional vector)
	seedMatrix      := nonZeroSM | matrix(W_v);
	colsInSM        := pivs | {v};
	-- Current seed invariant we are calculating
	-- This loops lets us remove one of the columns from the matrix to calculate the plücker
	seedInvariant := for i from 0 to m list (
	    pluckerMatrix   := submatrix'(seedMatrix, {i});  -- Find plucker matrix
	    colInW          := colsInSM#i;                   -- W-column corresponding to this seedMatrix col
	    e               := for j from 0 to n-1 list (if j == colInW then 1 else 0);        
	    (-1)^i * determinant(pluckerMatrix) * e
	    );
	sum seedInvariant -- Adds the summed seed invariant vec to our list
	);
    -- Now, seedList contains our list of seed Invariants, so we move onto expansion.

    --------------------
    -- Seed Expansion --
    --------------------
    ringVars := gens R;

    -- our seeds are a Z basis 
    seedList = for l in seedList list apply(l, x -> ((x % Z) + Z) % Z); --mod p
    p := Z;                              
    t := #seedList;                      
    olsonBound := m * (p - 1) + 1;       
    divides := (b, a) -> all(#a, j -> b#j <= a#j);

    --> enumerate (c_1,...,c_t) in \ZZ/p\ZZ --
    -- I use flatten to remove the {} entries because the vec are stored in {} too so it prunes it
    candidates := flatten for i from 1 to p^t - 1 list (
	-- Turn the integer into a vector 
	c := for j from 0 to t - 1 list ((i // p^j) % p);
	cand := for k from 0 to n - 1 list (
	    -- sum over seeds component-wise with weights in c then mod p
	    (sum for j from 0 to t - 1 list (c#j) * (seedList#j#k)) % p
	    );
	deg := sum cand;
	-- check Olson's bound
	if deg > 0 and deg <= olsonBound then {cand} else {}
	);

    -- seeds might be above Olson's bound
    candidates = candidates | seedList;

    --> Then we add the pure powers to the list, checking if they are minimal via our purePowers list.
    candidates = candidates | for i from 0 to #ringVars - 1 list (
	for j from 0 to #ringVars - 1 list (if i == j then p else 0)
	);
    
    -- make minimal
    -- remove duplicates
    candidates = unique candidates;

    -- makes {2, 1, 4} into {7, {2, 1, 4}} so we can sort by degree sum
    candidates = apply(candidates, a -> {sum a, a});

    -- sorts it by degree sum
    candidates = sort candidates;

    -- {7, {2, 1, 4}} back into {2, 1, 4}
    candidates = apply(candidates, q -> q#1);

    -- Now we sorted by degree sum, we check if they divide (divides function checks an inequality)
    -- seed list is grown seeds
    seedList = new MutableList from {};
    for a in candidates do (
	-- if no seeds in the list divide our candidate then its a valid seed so we add it
	if not any(#seedList, i -> divides(seedList#i, a)) then seedList#(#seedList) = a;
	);

    -->-- Now, we turn each of the exponent vectors into their polynomials in the ring. --<--
    polyList := for i in toList seedList list (
	n := 1;
	for j to #i - 1 do (n = n * (((ringVars)#j)^(i#j)));
	n
	);
    
    return polyList; -- Return our list
)

-------------------------------------------
--- invariants for DiagonalAction ---------
-------------------------------------------

invariants = method(Options => {
	Strategy => "Default",
	UseCoefficientRing => false,
	UsePolyhedra => false,
	DegreeBound => infinity,
	DegreeLimit => {},
	SubringLimit => infinity
	}
)

invariants DiagonalAction := List => o -> D -> (
    d := cyclicFactors D;
    (W1, W2) := weights D;
    -- As of May 2026, the elementary generation method is default, as long as:
    -- i) there is no torus action: zero(W1)
    -- ii) there are cyclic factors: d =!= {}
    -- iii) all cyclic factors have the same order: all(d, i -> d#0 == i)
    -- iv) the weight matrix has maximal rank: rank W2 == min(numRows W2,numColumns W2)
    -- the old strategy can be used with the option Strategy=>"DerksenGandini"
    if (o.Strategy =!= "DerksenGandini") and
    zero(W1) and d =!= {} and isPrime d#0 and all(d, i -> d#0 == i)
    and rank W2 == min(numRows W2,numColumns W2)
    then (
	return elementaryInvariants D;
	);
    --*-* Otherwise, continue with Derksen-Gandini algorithm *-*--
    R := ring D;
    kk := coefficientRing R;
    p := char kk;
    r := rank D;
    if p > 0 and o.UseCoefficientRing then (
        q := kk#order;
        if any(d, j -> q%j =!= 1) then (
            print "-- Diagonal action is not defined over the given coefficient ring. \n-- Returning invariants over an infinite extension field over which the action is defined.";
        )
        else (
            D' := diagonalAction(W1||W2, apply(r, i -> q - 1)|d, R);
            return invariants D';
        )
    );
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
    if o.UsePolyhedra then (
        if r == 1 then C = convexHull W1 else C = convexHull( 2*r*W1|(-2*r*W1) );
        C = (latticePoints C)/vector;
    )
    else (
        if r == 1 then C = (normaliz(transpose W1, "polytope"))#"gen" 
        else C = (normaliz(transpose (2*r*W1|(-2*r*W1)), "polytope"))#"gen";
        C = transpose C_(apply(r, i -> i));
        C = apply(numColumns C, j -> C_j)
    );
    -- sort elements of convex hull to ensure consistency across sources
    -- vectors are turned to lists for sorting, then back to vectors
    C = apply(sort apply(C,entries), vector);
    -- begin Derksen's algorithm for tori
    S = new MutableHashTable from apply(C, w -> w => {});
    scan(#mons, i -> S#(W1_i) = S#(W1_i)|{mons#i});
    U = new MutableHashTable from S;
    while any(values U, u -> #u > 0) do(
	-- Derksen does not specify which key vector to pick
	-- for consistency, pick first available from convex hull
	v = first select(1,C, w -> #(U#w) > 0);
	-- Derksen does not specify which monomial to pick
	-- min seems to be the right one for minimal invariants
	m = min (U#v);
    
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
			    ))) then ( 
		    S#v' = S#v'|{u};
		    U#v' = U#v'|{u};
		    )
		));
	U#v = delete(m, U#v);
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
invariants (LinearlyReductiveAction, List) := List => o -> (V, d) -> (
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
    for d from 1 to b+1 do (
    	Gb := gb(promote(ideal S,R),DegreeLimit=>d);
	I := monomialIdeal leadTerm Gb;
	M = reverse select(flatten entries (basis(d,R)%I),m->not zero m);
	if d == b+1 then break;
	if M === {} then break else (
	    if o.Strategy == "LinearAlgebra" then (
		for f in invariants(G,d) do (
	    	    g := f % Gb;
	    	    if not zero g then (
		    	S = S | {f};
		    	Gb = forceGB ( (gens Gb) | matrix{{g}} );
	    	    	);
		    );
	    	) 
	    else (
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
    if (M =!= {} and b < #(group G)) then print"
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

--checking invariants with reynolds operator can be slow for large groups
--isInvariant (RingElement, FiniteGroupAction) := Boolean => (f, G) -> reynoldsOperator(f, G) == f
--instead it is enough to check invariance under group generators
isInvariant (RingElement, FiniteGroupAction) := Boolean => (f, G) ->
    all(gens G, g -> sub(f, (vars ring G)*(transpose g) ) == f ) 

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





