-- -*- coding: utf-8 -*-
-----------------------------------------------------------------------
-- Copyright 2008--2022 Graham Denham, Gregory G. Smith, Avi Steiner
--
-- You may redistribute this program under the terms of the GNU General
-- Public License as published by the Free Software Foundation, either
-- version 2 or the License, or any later version.
-----------------------------------------------------------------------
newPackage(
     "HyperplaneArrangements",
     Version => "2.0",
     Date => "4 May 2022",
     Authors => {
	  {Name => "Graham Denham", 
	   HomePage => "http://gdenham.math.uwo.ca/"},
	  {Name => "Gregory G. Smith", 
	   Email => "ggsmith@mast.queensu.ca", 
	   HomePage => "http://www.mast.queensu.ca/~ggsmith"},
	  {Name => "Avi Steiner", 
	   Email => "avi.steiner@gmail.com", 
	   HomePage => "https://sites.google.com/view/avi-steiner"}
	  },
     Headline => "manipulating finite sets of hyperplanes",
     Keywords => {"Algebraic Geometry", "Matroids"},
     DebuggingMode => false,
     PackageExports => {"Matroids"}
     )

export {
    "arrangementLibrary", 
    -- types
    "Arrangement", 
    "CentralArrangement",
    "Flat",     
    -- functions/methods
    "arrangement", 
    "arrangementSum",     
    "deCone",
    "der",    
    "EPY", 
    "eulerRestriction",     
    "flat",  
    "genericArrangement",    
    "graphic",
    "HS",     
    "isCentral",         
    "isDecomposable", 
    "lct",
    "logCanonicalThreshold",  
    "makeEssential",
    "meet", 
    "multIdeal", 
    "multiplierIdeal",      
    "orlikSolomon", 
    "orlikTerao",  
    "randomArrangement",  
    "subArrangement", 
    "typeA", 
    "typeB", 
    "typeD", 
    "vee", 
    -- Option names
    "HypAtInfinity",
    "NaiveAlgorithm", 
    "Popescu",
    "Validate"
    }

protect assertEdgesArePosInts
protect circuitMonomials
protect irreds
protect makeEdges
protect multiplicities
protect multipliers
protect pvtDual
protect stableExponent

------------------------------------------------------------------------------
-- CODE
------------------------------------------------------------------------------
Arrangement = new Type of HashTable
Arrangement.synonym = "hyperplane arrangement"
Arrangement.GlobalAssignHook = globalAssignFunction
Arrangement.GlobalReleaseHook = globalReleaseFunction
Arrangement#{Standard,AfterPrint} = A -> (
     << endl;
     << concatenate(interpreterDepth:"o") << lineNumber << " : Hyperplane Arrangement "
     << endl;
     )
 
ring Arrangement := Ring => A -> A.ring
hyperplanes Arrangement := 
toList Arrangement := List => A -> A.hyperplanes

matrix Arrangement := Matrix => opts -> A -> (
    if #hyperplanes A == 0 then map((ring A)^1, (ring A)^0, 0)
    else matrix {hyperplanes A})

CentralArrangement = new Type of Arrangement
CentralArrangement.synonym = "central hyperplane arrangement"
CentralArrangement.GlobalAssignHook = globalAssignFunction
CentralArrangement.GlobalReleaseHook = globalReleaseFunction

debug Core
-- we'll have a better way to do this later
net Arrangement := A -> if hasAttribute(A,ReverseDictionary) then toString getAttribute(A,ReverseDictionary) else net expression A
dictionaryPath = delete(Core#"private dictionary", dictionaryPath)

net Arrangement := A -> net expression A
expression Arrangement := A -> new RowExpression from { A.hyperplanes }

arrangement = method(TypicalValue => Arrangement, Options => {})
arrangement (List,Ring) := Arrangement => options -> (L,R) -> (
     if #L > 0 and ring L#0 =!= R then (
	  f := map(R, ring L#0);
	  A := L / f)
     else A = L;
     central := true;
     if #L > 0 then central = fold( (p,q) -> p and q, isHomogeneous\L); -- why not use `all`?
     central = central and isHomogeneous R;
     
     -- Check if all the forms are linear
     if not all(A, f -> all(exponents f, expon -> all(expon, i -> i>=0) and sum expon <= 1)) then
     error "expected linear forms";
	 
     data := {
	  symbol ring => R,
	  symbol hyperplanes => A,
	  symbol cache => new CacheTable
	  };     
     
     arr := if central then 
          new CentralArrangement from data 
	  else new Arrangement from data;
     
     arr
     )
 
arrangement List := Arrangement => opts -> L -> (
     if #L == 0 then error "Empty arrangement has no default ring"
     else arrangement(L, ring L#0, opts))
--arrangement (Arrangement, Ring) := Arrangement => opts -> (A, R) -> arrangement(A.hyperplanes, R, opts)

arrangement (Matrix, Ring) := Arrangement => opts -> (M,R) -> (
    if numgens R != numRows M then error (
	"The number of variables of the ring must equal the number of rows of the matrix");
    arrangement(flatten entries((vars R) * M), R, opts)
    )

arrangement Matrix := Arrangement => opts -> M -> (
    kk := ring M;
    x := symbol x;
    n := numrows M;
    R := kk[x_1..x_n];
    arrangement(M, R, opts)
    )

-- arrangement from a polynomial: if it's unreduced, have multiplicities
arrangement RingElement := Arrangement => opts -> Q -> (
     l := select(toList factor Q, p -> 0 < (degree p#0)_0);  -- kill scalar
     arrangement (flatten (l / (p->toList(p#1:p#0))), opts)
     );
         
-- look up a canned arrangement
arrangement String := Arrangement => opts -> name -> (
     if not arrangementLibrary#?name then 
         error "the given string does not correspond to any entry in the database";
     kk := ring arrangementLibrary#name;
     if kk === ZZ then kk = QQ;
     arrangement(kk ** arrangementLibrary#name, opts)
     )

arrangement (String, PolynomialRing) := Arrangement => opts -> (name, R) -> (
     arrangement(arrangementLibrary#name, R, opts));

arrangement (String, Ring) := Arrangement => opts -> (name, kk) -> (
     arrangement(kk ** arrangementLibrary#name, opts));

-- here is a database of "classic" arrangements 
arrangementLibrary = hashTable {
     "braid" => matrix {
	 {1, 0, 0,  1,  1,  0},
	 {0, 1, 0, -1,  0,  1},
	 {0, 0, 1,  0, -1, -1}},
     "X2" => matrix {
	 {1, 0, 0,  0,  1, 1,  1},
	 {0, 1, 0,  1,  0, 1,  1},
	 {0, 0, 1, -1, -1, 0, -2}},
     "X3" => matrix { 
	 {1, 0, 0, 1, 1, 0}, 
	 {0, 1, 0, 1, 0, 1}, 
	 {0, 0, 1, 0, 1, 1}},
     "Pappus" => matrix {
	 {1, 0, 0,  1,  0,  1, 2,  2,  2}, 
	 {0, 1, 0, -1,  1, -1, 1,  1, -5}, 
	 {0, 0, 1,  0, -1, -1, 1, -1,  1}},
     "(9_3)_2" => matrix {
	 {1, 0, 0, 1, 0, 1, 1, 1, 4}, 
	 {0, 1, 0, 1, 1, 0, 2, 2, 6}, 
	 {0, 0, 1, 0, 1, 3, 1, 3, 6}},
     "nonFano" => matrix {
	 {1, 0, 0,  0,  1,  1,  1}, 
	 {0, 1, 0,  1,  0, -1,  1}, 
	 {0, 0, 1, -1, -1,  0, -1}},
     "MacLane" => matrix(ZZ/31627, {
	 {1, 0, 0,  1,  1,     0,     1,     1}, 
	 {0, 1, 0, -1,  0,     1, -6420, -6420}, 
	 {0, 0, 1,  0, -1, -6420,    -1,  6419}}),
     "Hessian" => matrix(ZZ/31627, {
	     {1, 0, 0, 1,    1,     1,    1,    1,     1,     1,     1,     1}, 
	     {0, 1, 0, 1,    1,     1, 6419, 6419,  6419, -6420, -6420, -6420},
      	     {0, 0, 1, 1, 6419, -6420,    1, 6419, -6420,     1,  6419, -6420}}),
     "Ziegler1" => matrix {
	 {1, 0, 0, 1, 2, 2, 2, 3, 3}, 
	 {0, 1, 0, 1, 1, 3, 3, 0, 4}, 
	 {0, 0, 1, 1, 1, 1, 4, 5, 5}},
     "Ziegler2" => matrix { 
	 {1, 0, 0, 1, 2, 2, 2, 1, 1}, 
	 {0, 1, 0, 1, 1, 3, 3, 0, 2}, 
	 {0, 0, 1, 1, 1, 1, 4, 3, 3}},
     "prism" => matrix {
	 {1, 0, 0, 0, 1, 1}, 
	 {0, 1, 0, 0, 1, 0}, 
	 {0, 0, 1, 0, 0, 1}, 
	 {0, 0, 0, 1, 1, 1}},
     "notTame" => matrix {
	 {1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1}, 
	 {0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1}, 
	 {0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1}, 
	 {0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1}},
     "bracelet" => matrix {
	 {1, 0, 0, 1, 0, 0, 1, 1, 0}, 
	 {0, 1, 0, 0, 1, 0, 1, 0, 1}, 
	 {0, 0, 1, 0, 0, 1, 0, 1, 1}, 
	 {0, 0, 0, 1, 1, 1, 1, 1, 1}},
     "Desargues" => matrix {
	 {1, 0, 0, 1,  2,  2, -3, 1, 3, 2}, 
	 {0, 1, 0, 1,  0,  1, -2, 2, 2, 1}, 
	 {0, 0, 1, 1, -3, -3,  2, 1, 1, 0}}
     }

-- nonessential arrangements always have one row for each variable
coefficients Arrangement := Matrix => opts -> A -> (
    R := ring A;
    KK := coefficientRing R;
    n := numgens R;
    varCoeffs := if hyperplanes A === {} then map(KK^n, KK^0, 0)
    else if dim R === 0 then map(KK^0, KK^(# hyperplanes A), 0)
    else sub((coefficients(matrix A, Monomials=>basis(1,R)))#1, KK);
    
    if isCentral A then varCoeffs
    else (
	constCoeffs := sub((coefficients(matrix A, Monomials=>{1_R}))#1, KK);
	varCoeffs || constCoeffs
	)
    )

rank CentralArrangement := A -> (
    if hyperplanes A === {} then 0 
    else codim(ideal hyperplanes A, Generic => true)
    )

-- arrangements may usually be taken to be central without loss of generality:
-- however, sometimes noncentral arrangements are convenient
isCentral = method(TypicalValue => Boolean);
isCentral Arrangement := Boolean => A -> instance(A, CentralArrangement)

-- arrangements may sensibly be defined over quotients of polynomial rings by
-- affine-linear ideals.  However, sometimes this is a pain, so we provide
prune Arrangement := Arrangement => options -> A -> (
     R := ring A;
     if not instance(R, PolynomialRing) then (
	  S := prune R;
	  f := R.minimalPresentationMap;
	  arrangement(f \ hyperplanes A, S)) 
     else A
     )

-- local function
normal = h -> (1 / leadCoefficient h) * h -- representative of functional, mod scalars

-- reduce an arrangement with possibly repeated hyperplanes to a 
-- simple arrangement.  Cache the simple arrangement and multiplicities.
trim Arrangement := Arrangement => opts -> (cacheValue symbol trim)(A -> (
     	if hyperplanes A === {} then (
	    A.cache.trim = A; 
	    A.cache.multiplicities = {}; 
	    return A);
	count := new MutableHashTable;
	for h in hyperplanes A do (
	    if h != 0 then (    
		if not count#?(normal h) then count#(normal h) = 0;
		count#(normal h) = 1+count#(normal h)));
	(L, m) := (keys count , values count);	
    	if #L > 0 and all(m, i -> i === 1) then (
	    A.cache.trim = A;
	    A.cache.multiplicities = m;
	    return A
	    );	
	A' := arrangement(L, ring A);
	A.cache.multiplicities = m; 
	A.cache.trim = A'
	)
    )	   

-- make a central arrangement essential if it isn't already
-- this is naturally defined over a subring of the ring of definition.
-- since this isn't implemented, though, we have to pick a basis.
-- this function is idempotent.
makeEssential = method();
makeEssential CentralArrangement := CentralArrangement => A -> (
    R := ring A;
    if not isPolynomialRing R then error "arrangement must be defined over a polynomial ring";
    C := gens trim image transpose coefficients A;
    r := rank C; -- the rank of the arrangement
    newvars := flatten entries (vars (ring A))_{0..r-1};
    R' := (coefficientRing R)(monoid [newvars]); 
    C' := sub(C, R'); 
    if r == numgens R then A -- already essential
       else arrangement flatten entries (C'*transpose vars R')
    )

-- remove degenerate hyperplanes arising in restriction
compress Arrangement := Arrangement => A -> if (A.hyperplanes == {}) then A else (
    L := select(A.hyperplanes, h -> first degree(h) == 1);
    arrangement(L, ring A)
    )

-- The method `matroid` will return an error if the coefficient ring of the
-- arrangement is ZZ.
matroid CentralArrangement := Matroid => options -> arr -> (
    if arr.cache.?matroid then arr.cache.matroid 
    else (
	arr.cache.matroid = matroid coefficients arr;
	arr.cache.matroid
	)
    )

pvtDual := args -> (
    -- args should be a list of one or two elements.
    -- args#0 should be a CentralArrangement. If it exists, args#1 should be a Ring
    arr := args#0;
    if (hyperplanes arr == {}) then error "dual expects a nonempty arrangement";
    newCoeffs := transpose gens ker coefficients arr;
    if args#?1 then arrangement(newCoeffs, args#1)
    else arrangement newCoeffs
    )
dual (CentralArrangement, Ring) := CentralArrangement => new OptionTable >> options -> (arr, R) -> (
    pvtDual {arr, R}
    )
dual CentralArrangement := CentralArrangement => new OptionTable >> options -> arr -> (
    pvtDual {arr}
    )

Arrangement == Arrangement := Boolean => (A, B) -> (
    ring A === ring B and hyperplanes A == hyperplanes B
    )

-- 'deletion' method is defined in 'Matroids'
deletion(Arrangement, RingElement) := Arrangement => (A ,h) -> (
    normh := normal h;
    firstParallel := position(hyperplanes A, f -> normal f == normal h); -- first hyperplane parallel to h
    if firstParallel === null then error ("The given hyperplane is not in the arrangement.");
    arrangement(drop(hyperplanes A, {firstParallel, firstParallel}), ring A)
    )
deletion(Arrangement, Set) := Arrangement => (A, S) -> (
    l := hyperplanes A;
    n := #l; 
    keep := set(0..n-1) - S;
    arrangement(l_(toList keep))
    )
deletion(Arrangement, List) := Arrangement => (A, L) -> (
    deletion(A, set L)
    )
deletion(Arrangement, ZZ) := Arrangement => (A, i) -> (
    deletion(A, set{i})
    )


-- a non-central arrangement may be defined over an inhomogeneous quotient of
-- a polynomial ring, so we need to prune it
cone(Arrangement, RingElement) := CentralArrangement => (A, h) -> (
     prune arrangement ((apply(hyperplanes A, i-> homogenize(i, h)) ) | {h})
     )
cone(Arrangement, Symbol) := CentralArrangement => (A, h) -> (
     R := ring A;
     S := (coefficientRing R)[h];
     T := tensor(R, S, Degrees => toList((numgens(R)+numgens(S)):1));
     f := map(T, S);
     cone (sub(A, T), f S_0)
     )

deCone = method()
deCone (CentralArrangement,RingElement) := Arrangement => (A,h) -> (
     A' := deletion(A,h);
     sub(A', (ring A')/ideal(h-1))
     )
deCone (CentralArrangement,ZZ) := Arrangement => (A,i) -> (
     h := (hyperplanes A)_i;
     deCone(A,h));

partial := m -> (
     E := ring m;
     sum first entries compress diff(vars E,m)
     )
monomialSubIdeal := I -> (  -- note: add options (See SP's code)
     R := ring I;
     K := I;
     J := ideal(1_R);
     while (not isMonomialIdeal K) do (
	  J = ideal leadTerm gens gb K;
	  K = intersect(I,J));
     ideal mingens K
     )


-------------------------------------------------------     
-- Orlik--Solomon algebra
-------------------------------------------------------     
-- If orlikSolomon is given a central arrangement, it returns an ideal I with
-- OS = E/I, where E is the ring of I and OS is the (central) Orlik-Solomon
-- algebra.
-- 
-- If the input is not central, we cone (homogenize) and then dehomogenize.
--
-- in the central case, the same ideal defines the cohomology ring of the
-- projective complement, but in a subalgebra of E.
--
-- Since we can't construct this in M2, the option Projective returns a larger
-- ideal I' so that E/I' is the cohomology ring of the projective complement,
-- written in coordinates that put a hyperplane H_j at infinity.
--
-- not clear this is the best...
--
-- we also expect this method to cache the circuits of A, as a list of
-- exterior monomials, since this calculation is expensive.  bug fix in June
-- 2013: circuits are defined over the coefficient ring of the arrangement.
orlikSolomon = method(TypicalValue => Ideal, 
                      Options => {Projective    => false, 
			          HypAtInfinity => 0,
				  Strategy      => Matroids})
orlikSolomon (CentralArrangement, PolynomialRing) := Ideal => o -> (A,E) -> (
    if #hyperplanes A == 0 then (
	if o.Projective then error "Empty projective arrangement is not allowed."
	else return ideal(0_E); -- empty affine arrangement is contractible.
	);
    n := #A.hyperplanes;
    e := symbol e;
    circuitMonoms := new MutableHashTable;
    Ep := (coefficientRing ring A)[e_1..e_n, SkewCommutative=>true];
    if o.Strategy === Matroids then (
	circuitMonoms = (Ep, apply(circuits A, C -> product apply(C, i -> Ep_i)));
	)
    else if o.Strategy === Popescu then (
	if A.cache.?circuitMonomials then circuitMonoms = A.cache.circuitMonomials
	else (
	    C := substitute(syz coefficients A, Ep);
	    M := monomialSubIdeal( ideal( (vars Ep) * C));	    
	    A.cache.circuitMonomials = (Ep, flatten entries gens M);
	    circuitMonoms = A.cache.circuitMonomials;
	    );
     	);
    f := map(E,circuitMonoms_0,vars E); -- note: map changes coefficient ring
    I := ideal append( apply(circuitMonoms_1/f, r -> partial r),0_E);
    if o.Projective then trim I+ideal(E_(o.HypAtInfinity)) else trim I
    )

-- if the arrangement is not central, cone first, then project back
orlikSolomon (Arrangement, PolynomialRing) := Ideal => o -> (A, E) -> (
     h := symbol h;
     e := symbol e;
     cA := cone(A, h);
     k := coefficientRing E;
     cE := E**k[e,SkewCommutative=>true];
     proj := map(E, cE);
     proj orlikSolomon (cA, cE, o)
     )
orlikSolomon (Arrangement,Symbol) := Ideal => o -> (A, e) -> (
     n := #A.hyperplanes;
     E := coefficientRing(ring A)[e_1..e_n, SkewCommutative => true];
     orlikSolomon(A, E, o)
     )

-- one can just specify a coefficient ring
-- note that this no longer affects the arrangement: that was a bug in a
-- previous version
orlikSolomon (Arrangement, Ring) := Ideal => o -> (A, k) -> (
     e := symbol e;
     n := #A.hyperplanes;
     E := k[e_1..e_n,SkewCommutative=>true];
     orlikSolomon(A, E, o)
     )
orlikSolomon Arrangement := Ideal => o -> A -> (
     e := symbol e;
     orlikSolomon(A,e,o)
     )

--  can't forward options, since existing method doesn't have options.
poincare Arrangement := RingElement => A -> (
     I := orlikSolomon A;
     numerator reduceHilbert hilbertSeries ((ring I)/I)
     )
-- faster to use the matroids package
poincare CentralArrangement := RingElement => A -> (
     M := matroid A;
     r := rank M;
     T := tuttePolynomial M;
     R := ring T;
     t := symbol t;
     S := frac(ZZ[t]); -- we can't take frac of degreesRing
     p := S_0^r*sub(T,{R_0=>1+1/S_0,R_1=>0});
     D := degreesRing ring A;
     sub(sub(p,ZZ[t]), {t=>D_0}) -- first lift p from frac
     )

-- Euler characteristic of (proj) complement
-- complement of empty arrangement is CP^{n-1}
euler CentralArrangement := ZZ => A -> (
    if #hyperplanes A == 0 then dim ring A else (
	f := poincare A;
	R := ring f;
	sub(f // (1+R_0), {R_0 => -1}) 
	)
    )

-- the uniform matroid is realized by points on the monomial curve; pick n
-- points (1..n) on the monomial curve of degree r; the user is responsible for
-- anything unexpected that happens in small characteristic
genericArrangement = method(TypicalValue => Arrangement)
genericArrangement (ZZ,ZZ,Ring) := Arrangement => (r,n,K) -> (
    C := matrix table(r, n, (i,j) -> (j+1)^i);
    arrangement (C**K)
    )
genericArrangement (ZZ,ZZ) := Arrangement => (r,n) -> genericArrangement(r,n,QQ)


typeA = method()
typeA (ZZ, PolynomialRing) := Arrangement => (n, R) -> (
    if n < 1 then error "expected a positive integer";
    if numgens R < n+1 then 
        error ("expected the polynomial ring to have at least " | n+1 | " variables");
    arrangement flatten for i to n-1 list (
	for j from i+1 to n list R_i - R_j)
    )    
typeA (ZZ, Ring) := Arrangement => (n, kk) -> (
     x := symbol x;
     R := kk (monoid [x_1..x_(n+1)]);
     typeA(n, R) 
     )
typeA ZZ := Arrangement => n -> typeA(n, QQ)

typeD = method()
typeD (ZZ, PolynomialRing) := Arrangement => (n, R) -> (
    if n < 2 then error "expected an integer greater than 1";
    if numgens R < n then 
        error ("expected the polynomial ring to have at least " | n | " variables");    
     arrangement flatten flatten for i to n-2 list (
	 for j from i+1 to n-1 list {R_i - R_j, R_i + R_j}
	 )
     )
typeD (ZZ, Ring) := Arrangement => (n, kk) -> (
     x := symbol x;
     R := kk(monoid [x_1..x_n]);
     typeD(n, R)
     )
typeD ZZ := Arrangement => n -> typeD(n, QQ)

typeB = method()
typeB (ZZ, PolynomialRing) := Arrangement => (n, R) -> (
    if n < 1 then error "expected a positive integer";
    if numgens R < n then 
        error ("expected the polynomial ring to have at least " | n | " variables");    
     arrangement flatten flatten for i to n-1 list (
	 {R_i} | for j from i+1 to n-1 list {R_i - R_j, R_i + R_j}
	 )
     )     
typeB (ZZ, Ring) := Arrangement => (n, kk) -> (
     x := symbol x;
     R := kk (monoid [x_1..x_n]);
     typeB(n, R)
     )
typeB ZZ := Arrangement => n -> typeB(n, QQ)

-- construct a graphic arrangement, from a graph given by a list of edges.
-- Assume vertices are integers 1..n
makeEdges := (edges, verts) -> (
    -- We don't want duplicate vertices!
    if unique verts =!= verts then error "Vertices must be distinct!";
    -- Make a hash table with the vertices as keys and their indices (counted
    -- from 0) as values.
    vertsHash := hashTable toList (reverse \ pairs verts);
    -- Replace each edge {a,b} with {1 + index of a, 1 + index of b}
    applyTable(edges, ed -> 1 + vertsHash#ed)
    )
assertEdgesArePosInts := G -> (
    if not all(flatten G, v -> instance(v, ZZ) and v > 0) 
    then error "Expected edges to be pairs of positive integers"
    )

graphic = method()
graphic(List, PolynomialRing) := Arrangement => (G, R) -> (
    assertEdgesArePosInts G;
    arrangement (G/(e->(R_(e_1-1)-R_(e_0-1))))
    )
graphic(List, Ring) := Arrangement => (G, k) -> (
    assertEdgesArePosInts G;
    n := max flatten G;
    x := symbol x;
    R := k[x_1..x_n];
    graphic(G,R)
    )
graphic List := Arrangement => G -> graphic(G, QQ)
graphic(List, List, PolynomialRing) := (edges, verts, R) -> graphic (makeEdges (edges, verts), R)
graphic(List, List, Ring) := (edges, verts, k) -> graphic (makeEdges (edges, verts), k)
graphic(List, List) := (edges, verts) -> graphic (makeEdges (edges, verts))

-------------------------------------------------------     
-- Random arrangements
-------------------------------------------------------     
-- return a random arrangement of n hyperplanes in a polynomial ring of
-- dimension l.  For large enough N, this will tend to be the uniform matroid
-- Note that if N and n aren't large enough and Validate => true, the method
-- will never return.  
randomArrangement = method(Options => {Validate => false})
randomArrangement(ZZ, PolynomialRing, ZZ) := Arrangement => options -> (n, R, N) -> (
     k := coefficientRing R;        
     l := numgens R;
     m := k**matrix randomMutableMatrix(l,n,0.,N);
     A := arrangement (m,R);
     tryagain := options.Validate;
     while tryagain do (
	 m = QQ**matrix randomMutableMatrix(l,n,0.,N);
     	 A = arrangement m;     
	 U := uniformMatroid(l,n);
	 tryagain = not areIsomorphic(U,matroid A));
     A
     )	  
-- if the ring isn't specified, make one over QQ
randomArrangement (ZZ,ZZ,ZZ) := Arrangement => options -> (n,l,N) -> (
    x := symbol x;
    R := QQ[x_1..x_l];
    randomArrangement(n,R,N,options)
    )

-------------------------------------------------------     
-- Flats
-------------------------------------------------------     
Flat = new Type of HashTable
Flat.synonym = "flat in an hyperplane arrangement"
Flat#{Standard,AfterPrint} = F -> (
     << endl;
     <<  concatenate(interpreterDepth:"o") << lineNumber << " : Flat of " << F.arrangement
     << endl;
     )
toList Flat := List => F -> F.flat
arrangement Flat := Arrangement => opts -> F -> F.arrangement
net Flat := F -> net F.flat
expression Flat := (F) -> new Holder from { F.flat }

flat = method(Options => {Validate => true})
flat(Arrangement, List) := Flat => options -> (A,F) -> (
    if not all(F, i -> class i === ZZ and i >= 0 and i < #hyperplanes A) then (
	error "Expected a list of indices.");
    newF := new Flat from {
	 symbol flat => sort F,
	 symbol arrangement => A,
	 symbol cache => new CacheTable
    	 };
    if options.Validate then (
	if newF != closure(A, F) then error "not a flat";
	);
    newF
    )

euler Flat := ZZ => F -> euler subArrangement F 

Flat == Flat := (X,Y) -> (
    if arrangement X == arrangement Y then (
    	(toList X) == (toList Y)) 
    else false
    )
     
-- the 'closure' method is defined in 'Matroids'
closure(Arrangement, Ideal) := Flat => (A,I) -> (
     flat(A, positions(A.hyperplanes, h -> h % gb I == 0), Validate => false)
     )
closure(Arrangement, List) := Flat => (A, S) -> (
     closure(A,ideal (A.hyperplanes_S | {0_(ring A)}))  -- ugly hack for empty list
     )  

meet = method()
meet(Flat, Flat) := Flat => (F, G) -> (
     A := arrangement F;
     if (A =!= arrangement G) then error "need the same arrangement"; 
     flat(A, select((toList F), i -> member(i, toList G)))
     )
Flat ^ Flat := Flat => meet  -- ooh, cool.  But note L_1^L_2 isn't L_1^(L_2) !

vee = method()
vee(Flat, Flat) := Flat => (F, G) -> (
     A := arrangement F;
     if (A =!= arrangement G) then error "need the same arrangement"; 
     closure(A, (toList F) | (toList G)) 
     )
Flat | Flat := Flat => vee  


subArrangement = method(TypicalValue => Arrangement)
subArrangement Flat := Arrangement => F -> (
     A := arrangement F;
     arrangement(A.hyperplanes_(toList F), ring A)
     )

-- the next version is redundant, but I'm putting it in in case users want to
-- use the usual notation
subArrangement (Arrangement, Flat) := Arrangement => (A, F) -> (
     if (A =!= arrangement F) then error "not a flat of the arrangement";
     subArrangement F
     )
Arrangement _ Flat := Arrangement => subArrangement


-- restriction will return a (i) multiarrangement with (ii) natural coordinate
-- ring; maybe not what everyone expects; empty flat needs special treatment

-- the 'restriction' methods is defined in 'Matroids'
restriction(Arrangement, List) := Arrangement => (A, L) -> (
    R := ring A;
    compress sub(A,R/(ideal (((toList A)_L)|{0_R})))
    )
restriction Flat := Arrangement => F -> (
     A := arrangement F;
     R := ring A;
     restriction(A, toList F)
     )
--  compress arrangement(A,R/(ideal ((toList A)_(toList F) | {0_R}))))

restriction(Arrangement, Set) := Arrangement => (A, S) -> restriction(A,toList S)
restriction(Arrangement, Flat) := Arrangement => (A, F) -> (
     if (A =!= arrangement F) then error "not a flat of the arrangement";
     restriction F
     )
restriction(Arrangement, ZZ) := Arrangement => (A,i) -> (
     restriction(A, flat(A, {i}))
     )
Arrangement ^ Flat := Arrangement => restriction

restriction(Arrangement, RingElement) := Arrangement => (A,h) -> (
     compress sub(A, (ring A)/(ideal h))
     )
restriction(Arrangement,Ideal) := Arrangement => (A,I) -> (
     compress sub(A,(ring A)/I)
     )

-------------------------------------------------------     
-- restriction of a multiarrangement in the sense introduced by Abe,
-- Wakefield, Yoshinaga, JLMS 2008

-- compute the stable exponent: this is the one that stays the same
-- when we delete a non-coloop
-- assumption: A is rank 2 and not boolean.
stableExponent := (A,m) -> ( 
    n := #A.hyperplanes;
    i := 0;  -- find a non-coloop
    c := while i<n list (if 1 < m_i then break i; i = i+1;);
    if i == n then return 1; -- all multiplicities were 1
    m' := replace(c, m_c-1, m);
    D := set flatten degrees prune image der(A,m);
    D' := set flatten degrees prune image der(A,m');
    (elements(D*D'))_0
    )   

eulerRestriction = method()
eulerRestriction (CentralArrangement, List, ZZ) := Sequence => (A, m, i) -> (
    hyps := hyperplanes A;
    n := #hyps;
    A'' := trim restriction(A,i); -- underlying simple arrangement
    R := ring A;
    I := ideal ring A'';
    mstar := apply(hyperplanes A'', h-> (       -- multiplicity for h is stable exp.
	    H := lift(h,R);
	    F := select(n, i -> (hyps_i+I+H == I+H));
	    stableExponent(A_(flat(A,F)), m_F)));
    (A'',mstar)
    )
    	            
-- TODO: der for nonessential arrangements may fail, because coefficients
-- arrangement {x,y,x-y} over QQ[x,y,z] only gives a 2x3 matrix


rank Flat := ZZ => F -> rank subArrangement F

-- the 'flats' methods is defined in 'Matroids'
flats(ZZ, Arrangement) := List => (j,A) -> (
     I := orlikSolomon A;
     OS := (ring I)/I;
     L := flatten entries basis(j,OS);
     unique(L/indices/(S->closure(A,S)))
     )
flats(ZZ, CentralArrangement) := List => (j, A) -> (
    matFlats := flats(matroid A, j);
    apply(toList \ matFlats, flat_A)
    )
flats Arrangement := List => A -> apply(1+rank A, j-> flats(j,A))

-- return list of indices of hyperplanes in minimal dependent sets
circuits CentralArrangement := List => A -> toList \ circuits matroid A
     
     
-- should overload "directSum" when tensor product of a sequence of rings
-- becomes available
arrangementSum = method()
arrangementSum (Arrangement, Arrangement) := Arrangement => (A, B) -> (
     R := ring A; 
     S := ring B;
     RS := tensor(R, S, Degrees => toList ((numgens(R) + numgens(S)) : 1));
     f := map(RS, R); 
     g := map(RS, S);
     arrangement((hyperplanes A) / f | (hyperplanes B) / g, RS)
     )
Arrangement ++ Arrangement := Arrangement => arrangementSum


sub(Arrangement, RingMap) := Arrangement => (A, phi) -> arrangement (apply(hyperplanes A, f -> phi f), target phi)
sub(Arrangement, Ring) := Arrangement => (A, R) -> sub(A, map(R, ring A))

Arrangement ** RingMap := Arrangement => (A, phi) -> sub(A, phi)
Arrangement ** Ring := Arrangement => (A, k) -> sub(A, k ** (ring A))



-- Check if arrangement is decomposable in the sense of Papadima-Suciu. We need
-- to distinguish between the coefficients in A and the coefficients for I
isDecomposable = method(TypicalValue => Boolean)
isDecomposable (CentralArrangement, Ring) := Boolean => (A, k) -> (
     I := orlikSolomon (A, k);
     b := betti res(coker vars ((ring I)/I), LengthLimit => 3);
     phi3 := 3*b_(3,{3},3) - 3*b_(1,{1},1)*b_(2,{2},2) + b_(1,{1},1)^3 - b_(1,{1},1);
     multiplicities := apply(flats(2,A), i -> length toList i);
     sum(multiplicities, m -> m*(2-3*m+m^2)) == phi3
     )
isDecomposable (CentralArrangement) := Boolean => A -> (
     isDecomposable(A, QQ) -- changed from the coefficient ring of A, April 2022
     )


------------------------------------------------------------------------------  
symExt = (m,R) -> (
     if (not(isPolynomialRing(R))) then error "expected a polynomial ring or an exterior algebra";
     if (numgens R != numgens ring m) then error "the given ring has a wrong number of variables";
     ev := map(R,ring m,vars R);
     mt := transpose jacobian m;
     jn := gens kernel mt;
     q  := vars(ring m) ** id_(target m);
     n  := ev(q*jn)
     )
 
-- EPY module, formerly called FA
EPY = method()
EPY(Ideal, PolynomialRing) := Module => (J, R) -> (
    modT := (ring J)^1 / (J*(ring J^1));
    F := res(prune modT, LengthLimit => 3);
    g := transpose F.dd_2;
    G := res(coker g, LengthLimit => 4);
    FA := coker symExt(G.dd_4, R);
    d := first flatten degrees cover FA;
    FA ** (ring FA)^{d}  -- GD: I want this to be generated in degree 0
    )
EPY Ideal := Module => J -> (
    S := ring J;
    n := numgens S;
    f := symbol f;
    X := getSymbol "X";
    R := coefficientRing(S)[X_1..X_n];
    EPY(J, R)
    )
EPY Arrangement := Module => A -> EPY orlikSolomon A
EPY (Arrangement, PolynomialRing) := Module => (A, R) -> EPY(orlikSolomon A, R)

------------------------------------------------------------------------------
-- the Orlik-Terao algebra
orlikTeraoV1 := (A, S) -> (     
     hyps := hyperplanes A;    
     n := #hyps; 
     R := ring A;
     if n == 0 then return ideal(0_S);
     if (numgens S != n) then error "the given ring has a wrong number of variables";
     Q := product hyps;
     quotients := hyps/(h->Q//h);
     trim ker map(R,S, quotients));

-- construct the relation associated with a circuit
OTreln := (c, M, S) -> (  -- circuit, coeffs, ring of definition
     v := gens ker M_c;
     f := map(S, ring v);
     P := product(c/(i->S_i));  -- monomial
     (matrix {c / (i -> P//S_i)} * f v)_(0,0)
     )

-- this older version builds the ideal "manually": definitely slower, so kept
-- only to add a test.
orlikTeraoV2 := (A, S) -> (     
     n := #toList A;
     if n == 0 then return ideal(0_S);
     if (numgens S != n) then error "the given ring has a wrong number of variables";
     vlist := flatten entries vars S;
     M := coefficients A;
     trim ideal(circuits A/(c -> OTreln(c,M,S)))
     )
     
orlikTerao = method(Options => {NaiveAlgorithm => false})
orlikTerao(CentralArrangement, PolynomialRing) := Ideal => o -> (A,S) -> (
     if o.NaiveAlgorithm then orlikTeraoV2(A,S) else orlikTeraoV1(A,S)
     )
orlikTerao(CentralArrangement, Symbol) := Ideal => o -> (A, y) -> (
     n := #A.hyperplanes;
     S := coefficientRing(ring A)[y_1..y_n];
     orlikTerao(A, S, o)
     )
orlikTerao CentralArrangement := Ideal => o -> A -> (          
     y := symbol y; 
     orlikTerao(A, y, o)
     )


-- needs adjustment if ring of A is not polynomial.  
der = method(Options => {Strategy => null});
der (CentralArrangement) := Matrix => o -> A -> (
     Ap := prune A;  -- ring of A needs to be polynomial
     if o.Strategy === Popescu then der1(Ap) else (
	  if not Ap.cache.?trim then trim(Ap);
     	  der2(Ap.cache.trim, Ap.cache.multiplicities)
	  )
      )
-- it's a multiarrangement if multiplicities supplied
der (CentralArrangement, List) := Matrix => o -> (A,m) -> der2(prune A,m)   

-- Note: no removal of degree 0 part.
der1 = A -> (
     Q := product hyperplanes A;   -- defining polynomial
     J := jacobian ideal Q;
     m := gens ker map(transpose J | -Q, Degree => -1);
     l := rank A;
     submatrix(m,0..(l-1))
     )

-- simple arrangement with a vector of multiplicities; fixed 22 July 2021 to
-- ensure homogeneous results
der2 = (A, m) -> (
     hyps := hyperplanes A;
     R := ring A;
     n := #hyps;
     l := numgens R;
     P := R ** transpose coefficients A;
     D := diagonalMatrix apply(n, i-> hyps_i^(m_i));
     -- proj := map(R^n,,map(R^n,R^l,0) | map(R^n,R^n,1));
     -- proj * gens ker(map(target proj,, P|D)));
     M := gens ker map(R^n,, P|D);
     M^{l..(n+l-1)}
     )


-- compute multiplier ideals of an arrangement, via theorems of Mustata and
-- Teitler
weight := (F, m) -> sum((toList F) / (i -> m_i))

multiplierIdeal = method()
multIdeal = method()
-- it's expensive to recompute the list of irreducible flats, as well as
-- intersections of ideals.  So we cache a hash table whose keys are the lists
-- of exponents on each ideal, and whose values are the intersection.
multIdeal(QQ, CentralArrangement, List) :=
multiplierIdeal(QQ, CentralArrangement, List) := Ideal => (s,A,m) -> (
     if (#hyperplanes A != #m) then error "expected one weight for each hyperplane";
     R := ring A;
     if not A.cache.?irreds then
	  A.cache.irreds = select(flatten drop(flats(A),1), F->(0 != euler F));
     exps := A.cache.irreds/(F->max(0,floor(s*weight(F,m))-rank(F)+1));
     if not A.cache.?multipliers then A.cache.multipliers = new MutableHashTable;
     if not A.cache.multipliers#?exps then (
	  ideals := A.cache.irreds/(F-> trim ideal toList (A_F));
	  A.cache.multipliers#exps = intersect apply(#exps, i->(ideals_i)^(exps_i)))
     else
     	  A.cache.multipliers#exps
     )
multIdeal(QQ, CentralArrangement) :=
multiplierIdeal(QQ, CentralArrangement) := Ideal => (s,A) -> (
     if not A.cache.?trim then trim A;
     multiplierIdeal(s,A.cache.trim, A.cache.multiplicities)
     )

-- numeric argument might be an integer:
multIdeal(ZZ, CentralArrangement) :=
multiplierIdeal(ZZ, CentralArrangement) := Ideal => (s,A) -> multiplierIdeal(s*1/1, A)

multIdeal(ZZ, CentralArrangement, List) := 
multiplierIdeal(ZZ, CentralArrangement, List) := Ideal => (s,A,m) -> multiplierIdeal(s*1/1, A, m)


-- use the observation that the jumping numbers must be rationals with
-- denominators that divide the weight of one or more flats.
logCanonicalThreshold = method(TypicalValue => QQ)
lct = method(TypicalValue => QQ)

lct CentralArrangement :=
logCanonicalThreshold CentralArrangement := QQ => A -> (
     I0 := multiplierIdeal(0,A);  -- cache the irreducibles, make A a multiarrangement
     irreds := A.cache.trim.cache.irreds;
     N := lcm(irreds/(F->weight(F,A.cache.multiplicities)));
     s := 1;
     while I0 == multiplierIdeal(s/N,A) do s = s+1;
     s/N);     

HS = i -> reduceHilbert hilbertSeries i;

------------------------------------------------------------------------------
-- DOCUMENTATION
------------------------------------------------------------------------------
beginDocumentation()

undocumented {
    HS, 
    (expression, Arrangement), 
    (expression, Flat),
    (net, Flat), 
    (net, Arrangement), 
    HypAtInfinity,
    NaiveAlgorithm,     
    Validate
    }

doc ///
    Key
        HyperplaneArrangements
    Headline
        manipulating hyperplane arrangements
    Description
        Text
	    A hyperplane arrangement is a finite set of hyperplanes in an
     	    affine or projective space.  In this package, an arrangement is
     	    expressed as a list of (linear) defining equations for the
     	    hyperplanes.  The tools provided allow the user to create new
     	    arrangements from old, and to compute various algebraic invariants
     	    of arrangements.	
     	Text
     	    Introductions to the theory of hyperplane arrangements can be
     	    found in the following textbooks:
    	Text
	    @UL {
		{HREF("https://math.unice.fr/~dimca/", "Alexandru Dimca"), 
		 ", ", 
		HREF("https://doi.org/10.1007/978-3-319-56221-6",
		    "Hyperplane arrangements"), 
		", Universitext,", 
		"Springer, Cham, 2017. ",
		"ISBN: 978-3-319-56221-6" },
	    	{HREF("https://en.wikipedia.org/wiki/Peter_Orlik", "Peter Orlik"), 
		 " and ",
		 HREF("https://en.wikipedia.org/wiki/Hiroaki_Terao", "Hiroaki Terao"),
		 ", ", 
		HREF("https://doi.org/10.1007/978-3-662-02772-1",
		    "Arrangements of hyperplanes"), 
		", Grundlehren der mathematischen Wissenschaften 300,", 
		"Springer-Verlag, Berlin, 1992. ",
		"ISBN: 978-3-662-02772-1" },
	    {HREF("https://math.mit.edu/~rstan/", "Richard P. Stanley"), 
		 ", ", 
		HREF("https://doi.org/10.1090/pcms/013",
		    "An introduction to hyperplane arrangements"), 
		", in ",  EM "Geometric Combinatorics", ", 389-496, ", 
		"IAS/Park City Mathematics Series 13, American Mathematical Society, Providence, RI, 2007. ",
		"ISBN: 978-1-4704-3912-5" },	    
	    }@		    
///

doc ///
    Key
        Arrangement
    Headline
        the class of all hyperplane arrangements
    Description
        Text
	    A hyperplane is an affine-linear subspace of codimension one.  An
     	    arrangement is a finite set of hyperplanes.	
///


doc ///
    Key
        CentralArrangement
    Headline
        the class of all central hyperplane arrangements
    Description
        Text
	    A {\em central} arrangement is a finite set of linear hyperplanes.
	    In other words, each hyperplane passes through the origin.
///

doc ///
    Key
    	(arrangement, List, Ring)
	(arrangement, List)
	(arrangement, RingElement)
        arrangement	
    Headline
        make a hyperplane arrangement
    Usage
        arrangement(L, R) 
    	arrangement L	
    Inputs
        L : List
	    of affine-linear equations in the ring $R$ or 
	    @ofClass RingElement@ that is a product of linear forms
	R : Ring
	    a polynomial ring or linear quotient of a polynomial ring
    Outputs
        : Arrangement
	    determined by the input data
    Description
        Text 
	    A hyperplane is an affine-linear subspace of codimension one.  An
     	    arrangement is a finite set of hyperplanes.  When each hyperplane
     	    contains the origin, the arrangement is  
	    @TO2(CentralArrangement, "central")@.
    	Text
	    Probably the best-known hyperplane arrangement is the braid
     	    arrangement consisting of all the diagonal hyperplanes.  In
     	    $4$-space, it is constructed as follows.
     	Example
	    S = QQ[w,x,y,z];
	    A3 = arrangement {w-x, w-y, w-z, x-y, x-z, y-z}
	    assert isCentral A3
	Text
	    When a hyperplane arrangement is created from a product of linear
	    forms, the order of the factors is not preserved.
	Example
	    A3' = arrangement ((w-x)*(w-y)*(w-z)*(x-y)*(x-z)*(y-z))
	    assert(A3 != A3')
	    arrangement (x^2*y^2*(x^2-y^2)*(x^2-z^2))
	Text
	    The package can recognize that a polynomial splits into linear forms over 
	    the base field.
	Example
	    kk = toField(QQ[p]/(p^2+p+1)) -- toField is necessary so that M2 treats this as a field
	    R = kk[s,t]
	    arrangement (s^3-t^3)
	Text
	    If we project onto a linear subspace, then we obtain an essential
     	    arrangement, meaning that the rank of the arrangement is equal to
     	    the dimension of its ambient vector space.
	Example
	    R = S/ideal(w+x+y+z);
	    A3'' = arrangement({w-x,w-y,w-z,x-y,x-z,y-z}, R)
	    ring A3''
	    assert(rank A3'' === dim ring A3'')
	Text
	    The trivial arrangement has no equations.
	Example
	    trivial = arrangement({},S)
	    ring trivial
	    assert isCentral trivial
    Caveat
	If the entries in $L$ are not @TO2(RingElement, "ring elements")@ in
	$R$, then the induced identity map is used to map them from the ring
	of first element in $L$ into $R$.
    SeeAlso
        HyperplaneArrangements
	(arrangement, Matrix)
	(arrangement, String, PolynomialRing)
	(isCentral, Arrangement)    	
///

doc ///
    Key
    	(arrangement, Matrix, Ring)
	(arrangement, Matrix)
    Headline
        make a hyperplane arrangement
    Usage
        arrangement(M, R) 
    	arrangement M	
    Inputs
        M : Matrix
    	    a matrix whose columns represent linear forms defining hyperplanes
	R : Ring
	    a polynomial ring or linear quotient of a polynomial ring
    Outputs
        : Arrangement
	    determined by the input data
    Description
        Text 
	    A hyperplane is an affine-linear subspace of codimension one.  An
     	    arrangement is a finite set of hyperplanes.  When each hyperplane
     	    contains the origin, the arrangement is  
	    @TO2(CentralArrangement, "central")@.
    	Text
	    Probably the best-known hyperplane arrangement is the braid
     	    arrangement consisting of all the diagonal hyperplanes.  In
     	    $4$-space, it is constructed as follows.
     	Example
	    S = QQ[w,x,y,z];
	    A3 = arrangement(matrix{{1,1,1,0,0,0},{-1,0,0,1,1,0},{0,-1,0,-1,0,1},{0,0,-1,0,-1,-1}}, S)
	    assert isCentral A3
	Text
	    If we project along onto a subspace, then we obtain an essential
     	    arrangement, meaning that the rank of the arrangement is equal to
     	    the dimension of its ambient vector space.
	Example
	    R = S/ideal(w+x+y+z);
	    A3' = arrangement(matrix{{1,1,1,0,0,0},{-1,0,0,1,1,0},{0,-1,0,-1,0,1},{0,0,-1,0,-1,-1}}, R)
	    ring A3'
	    assert(rank A3' === dim ring A3')
	Text
	    The trivial arrangement has no equations.
	Example
	    trivial = arrangement(map(S^4,S^0,0),S)
	    ring trivial
	    assert isCentral trivial
    SeeAlso
        HyperplaneArrangements
	(arrangement, List)
	(arrangement, String, PolynomialRing)
	(isCentral, Arrangement)
///

doc ///
    Key
    	(arrangement, String, Ring)
	(arrangement, String, PolynomialRing)	
    	(arrangement, String)	
	symbol arrangementLibrary
    Headline
        access a database of classic hyperplane arrangements
    Usage
        arrangement(s, R) 
	arrangement s
    Inputs
        s : String
    	    corresponding to the name of a hyperplane arrangement in the database
	R : Ring
	    that determines the coefficient ring of the hyperplane arrangement
	    or @ofClass PolynomialRing@ that determines the 
	    @TO2((ring, Arrangement), "ambient ring")@
    Outputs
        : Arrangement
	    from the database
    Description
        Text 
	    A hyperplane is an affine-linear subspace of codimension one.  An
     	    arrangement is a finite set of hyperplanes.  This method allows
     	    convenient access to the hyperplane arrangements with the following
	    names
	Example
	    sort keys arrangementLibrary
	Text
	    We illustrate various ways to specify the ambient ring for some
	    classic hyperplane arrangements.
	Example
	    A0 = arrangement "(9_3)_2"
	    ring A0
	    A1 = arrangement("bracelet", ZZ)
	    ring A1
	    A2 = arrangement("braid", ZZ/101)
	    ring A2
	    A3 = arrangement("Desargues", ZZ[vars(0..2)])
    	    ring A3		
    	    A4 = arrangement("nonFano", QQ[a..c])	    
	    ring A4
	    A5 = arrangement("notTame", ZZ/32003[w,x,y,z])
    	    ring A5
	Text
	    Two of the entries in the database are defined over the finite
	    field with $31627$ elements where $6419$ is a cube root of unity.
	Example
	    A6 = arrangement "MacLane"
	    ring A6
	    A7 = arrangement("Hessian", ZZ/31627[a,b,c])
	    ring A7
	Text
	    Every entry in this database determines a central hyperplane arrangement.
	Example
	    assert all(keys arrangementLibrary, s -> isCentral arrangement s)
	Text	    
	    The following two examples have the property that the six triple
	    points lie on a conic in the one arrangement, but not in the
	    other.  The difference is not reflected in the matroid.  However,
	    Hal Schenck's and Ştefan O. Tohǎneanu's paper "The Orlik-Terao
	    algebra and 2-formality" {\em Mathematical Research Letters} 
	    {\bf 16} (2009) 171-182 
	    @HREF("https://arxiv.org/abs/0901.0253", "arXiv:0901.0253")@
	    observes a difference between their respective
	    @TO2(orlikTerao, "Orlik-Terao")@ algebras.
	Example
	    Z1 = arrangement "Ziegler1"
	    Z2 = arrangement "Ziegler2"
	    assert(matroid Z1 == matroid Z2) -- same underlying matroid
    	    I1 = orlikTerao Z1; 
	    I2 = orlikTerao Z2;
	    assert(hilbertPolynomial I1 == hilbertPolynomial I2) -- same Hilbert polynomial
    	    hilbertPolynomial ideal super basis(2,I1)
    	    hilbertPolynomial ideal super basis(2,I2) -- but not (graded) isomorphic	    
    SeeAlso
	(arrangement, List)
	typeA
	typeB
	typeD
	(isCentral, Arrangement)
///

doc ///
    Key
    	(arrangement, Flat)
    Headline
       	get the hyperplane arrangement to which a flat belongs
    Usage
    	arrangement F
    Inputs
    	F : Flat
    Outputs
    	: Arrangement
	    to which the flat belongs
    Description
    	Text
	    A flat is a set of hyperplanes that are maximal with respect to
	    the property that they contain a given affine subspace.  In this
	    package, flats are treated as lists of indices of hyperplanes in
	    the arrangement.  Given a flat, this method returns the underlying
	    hyperplane arrangement.
	Example
	    A3 = typeA 3
	    F = flat(A3,{3,4,5})
	    assert(arrangement F === A3)
    SeeAlso
    	(flat, Arrangement, List)
        (flats, Arrangement)
///

doc ///
    Key
    	(symbol ==, Arrangement, Arrangement)
    Headline
    	whether two hyperplane arrangements are equal
    Usage
    	A == B
    Inputs
    	A : Arrangement
	B : Arrangement
    Outputs
    	: Boolean
    	    that is true if the underlying rings are equal and the lists of
    	    hyperplanes are the same
    Description
    	Text
	    Two hyperplane arrangements are equal their underlying rings are
	    identical and their defining linear forms are listed in the same
	    order.
	Text
	    Although the following two arrangements have the same hyperplanes,
	    they are not equal because the linear forms are different.
	Example
	    R = QQ[x, y];
	    A = arrangement{x, y, x+y}
	    assert(A == A)
	    B = arrangement{2*x, y, x+y}
	    A == B
	    assert not (A == B)
	    assert( A != B )
        Text
    	    The order in which the hyperplanes are listed is also important.
	Example
	    A' = arrangement{y, x, x+y}
	    A == A'
	    assert( A != A' )
    SeeAlso
    	(ring, Arrangement)
        (hyperplanes, Arrangement)
///

doc ///
    Key
    	(ring, Arrangement)
    Headline
        get the underlying ring of a hyperplane arrangement
    Usage
    	ring A
    Inputs
        A : Arrangement
    Outputs
        : Ring
	    that contains the defining equations of the arrangement
    Description
        Text 
	    A hyperplane arrangement is defined by a list of affine-linear
	    equations in a ring, either a polynomial ring or the quotient of
	    polynomial ring by linear equations.  This methods returns this
	    ring.
    	Text
	    Probably the best-known hyperplane arrangement is the braid
     	    arrangement consisting of all the diagonal hyperplanes.  We
     	    illustrate two constructions of this hyperplane arrangement in
     	    $4$-space, using different polynomial rings.
     	Example
	    S = ZZ[w,x,y,z];
	    A = arrangement(matrix{{1,1,1,0,0,0},{-1,0,0,1,1,0},{0,-1,0,-1,0,1},{0,0,-1,0,-1,-1}}, S)
	    ring A
	    assert(ring A === S)
	    S' = ZZ/101[w,x,y,z];
	    A' = typeA(3, S') 
	    ring A'
	    assert(ring A' === S')
	    assert(A' =!= A)
	Text
	    Projecting onto an appropriate linear subspace, we obtain an
     	    essential arrangement, meaning that the rank of the arrangement is
     	    equal to the dimension of its ambient vector space. (See also 
	    @TO makeEssential@.)
	Example
	    R = S'/(w+x+y+z)
	    A'' = sub(A, R) -- this changes the coordinate ring of the arrangement
	    ring A''
	    assert(rank A'' == dim ring A'')
	Text
	    The trivial arrangement has no equations, so it is necessary to specify
	    a coordinate ring.
	Example
	    trivial = arrangement({}, S)
    	    assert(ring trivial === S)	    	    
	    trivial' = arrangement({},R)
    	    assert(ring trivial' === R)	    
    SeeAlso
	(arrangement, List)
///

doc ///
    Key
    	(matrix, Arrangement)
    Headline
        make a matrix from the defining equations
    Usage
    	matrix A
    Inputs
        A : Arrangement
	Degree => 
	    this optional input is ignored by this function
    Outputs
        : Matrix
	    having one row, whose entries are the defining equations
    Description
        Text 
	    A hyperplane arrangement is defined by a list of affine-linear
	    equations.  This methods creates a matrix, over the 
	    @TO2((ring, Arrangement), "underlying ring")@ of the hyperplane
	    arrangement, whose entries are the defining equations.
    	Text
            A few reflection arrangements yield the following matrices.
     	Example
	    A = typeA 3
	    R = ring A
	    matrix A
	    matrix typeB 2
	    matrix typeD 4
	Text
	    The trivial arrangement has no equations.
	Example
	    trivial = arrangement({},R)
	    matrix trivial
	    assert(matrix trivial == 0)
    SeeAlso
	(arrangement, List)
	(ring, Arrangement)
///

doc ///
    Key
    	(coefficients, Arrangement)
    Headline
        make a matrix from the coefficients of the defining equations
    Usage
    	coefficients A
    Inputs
        A : Arrangement
	Monomials => List
    	    which is ignored
	Variables => List
	    which is ignored
    Outputs
        : Matrix
    	    whose entries are the coefficients of the defining equations
    Description
        Text 
	    A hyperplane arrangement is defined by a list of affine-linear
	    equations.  This method creates a matrix whose rows correspond to
	    variables in the @TO2((ring, Arrangement), "underlying ring")@ and
	    whose columns correspond to the defining equations.  The entries
	    in this matrix are the coefficients of the defining equations.
	    
	    If the arrangement is affine (i.e. there are constant coefficients),
	    the last row of the output matrix is the constant coefficients.
    	Text
            A few reflection arrangements yield the following matrices.
     	Example
	    coefficients typeA 3
	    coefficients typeB 2
	    coefficients typeD 4
	Text
	    The coefficient ring need not be the rational numbers.
	Example
	    R = ZZ/101[x,y,z];
	    A = arrangement("Pappus", R)
	    coefficients A
	    H = arrangement("Hessian")
	    coefficients H
	Text
	    For non-central hyperplane arrangements, the last row of the coefficient matrix
	    records the constant terms.
	Example
	    B = arrangement(x*y*(x+y+1))
	    coefficients B
	    C = arrangement(x*y*z*(x+y+1)*(y+z-1))
	    coefficients C
	Text
	    The trivial arrangement has no equations, so its this method
	    returns the zero matrix.
	Example
	    R = ZZ[x,y,z];
	    trivial = arrangement(map(R^(numgens R),R^0,0),R)
	    coefficients trivial
	    assert(coefficients trivial == 0)
    SeeAlso
	(arrangement, List)
	(ring, Arrangement)
///

doc ///
    Key
    	(rank, CentralArrangement)
    Headline
        compute the rank of a central hyperplane arrangement
    Usage
    	rank A
    Inputs
        A : CentralArrangement
    Outputs
        : ZZ
	    the codimension of the intersection of the defining equations
    Description
        Text 
	    The {\em center} of a hyperplane arrangement is the intersection
	    of its defining affine-linear equations.  The {\em rank} of a
	    hyperplane arrangement is the codimension of its center.
    	Text
    	    We illustrate this method with some basic examples.
     	Example
	    R = QQ[x,y,z];
	    B = arrangement("braid", R)
	    rank B
    	    assert(rank B === rank matroid B)
	    rank typeA 4
	    M = arrangement("MacLane")
	    rank M
	Text
	    The trivial arrangement has no equations.
	Example
	    trivial = arrangement(map(R^(numgens R),R^0,0),R)
	    rank trivial
	    assert(rank trivial === 0)
    SeeAlso
	(arrangement, List)
	(ring, Arrangement)
///

doc ///
    Key
    	(rank, Flat)
    Headline
    	compute the rank of a flat
    Usage
    	rank F
    Inputs
    	F : Flat
    Outputs
    	: ZZ
	    the codimension of the intersection of the hyperplanes containing $F$
    Description
    	Text
	    The {\em rank} of a flat $F$ is the codimension of the intersection of 
	    the hyperplanes containing $F$ (i.e. whose indices are in $F$).
	Example
	    A3 = typeA 3
	    F = flat(A3, {3,4,5})
	    assert(rank F == 2)
    SeeAlso
    	(rank, CentralArrangement)
///

doc ///
    Key
	(makeEssential, CentralArrangement)
	makeEssential
    Headline
        make an essential arrangement out of an arbitrary one
    Usage
    	makeEssential A
    Inputs
        A : CentralArrangement
    Outputs
        : CentralArrangement
    	    a combinatorially equivalent essential arrangement
    Description
        Text 
	    A @TO2((CentralArrangement), "central arrangement")@ is {\em
	    essential} if the intersection of all of the hyperplanes equals
	    the origin.  If ${\mathcal A}$ is a hyperplane arrangement in an
	    affine space $V$ and $L$ is the intersection of all of the
	    hyperplanes, then the image of the hyperplanes of ${\mathcal A}$
	    in $V/L$ gives an equivalent essential arrangement.
	    
	    Since this essentialization is defined over a subring of the 
	    @TO2((ring, Arrangement), "underlying ring")@ of ${\mathcal A}$, it
	    cannot be implemented directly.  Instead, the method chooses a
	    splitting of the quotient $V\to V/L$ and returns an arrangement over
	    a polynomial ring on a subset of the original variables.
	    
	    If ${\mathcal A}$ is already essential, then the method returns the same
	    arrangement.
	Text
	    Deleting a hyperplane from an essential arrangement yields an
	    essential arrangement only if the hyperplane was not a coloop.
     	Example
	    R = QQ[x, y, z];
	    A = arrangement{x, y, x-y, z}
	    makeEssential A
	    assert(A == makeEssential A)
	    A' = deletion(A, z)
	    ring A'
	    makeEssential A'
	    ring makeEssential A'
	Text
	    Type-$A$ reflection arrangements are not essential.
	Example
    	    A = typeA 3
	    ring A
    	    A' = makeEssential A
	    ring A'
	Text
	    Type-$B$ reflection arrangements are essential.
	Example
	    B = typeB 3
	    assert(B == makeEssential B)
    SeeAlso
	(ring, Arrangement)
	(trim, Arrangement)
	(prune, Arrangement)
	
///
 
doc ///
    Key
	(trim, Arrangement)
	"make simple"
	"simplify"
    Headline
        make a simple hyperplane arrangement
    Usage
    	trim A
    Inputs
        A : Arrangement
    Outputs
        : Arrangement
    	    a simple arrangement
    Description
        Text
	    A hyperplane arrangement is {\em simple} if none of its linear
	    forms is identically $0$ and no hyperplane is cut out out by more
	    than one form. This method returns a simple arrangement by
	    reducing the multiplicities of the hyperplanes and eliminating the
	    zero equation (if necessary).
	Example
	    R = QQ[x, y];
	    A = arrangement{x,x,0_R,y,y,y,x+y,x+y,x+y,x+y,x+y}
	    A' = trim A
	    assert(ring A' === R)
	    assert(trim A' == A')
	    assert(trim A' == A')
	Text
	    Some natural operations produce non-simple hyperplane arrangements.
    	Example
    	    A'' = restriction(A, y)
	    trim A'' 
	    A''' = dual arrangement{x, y, x-y}
	    trim A'''
    SeeAlso
	(compress, Arrangement)
	(prune, Arrangement)
	(restriction, Arrangement, RingElement)
	(dual, CentralArrangement)
///

doc ///
    Key
	(compress, Arrangement)
	"make loopless"
    Headline
        extract nonzero equations
    Usage
    	compress A
    Inputs
        A : Arrangement
    Outputs
        : Arrangement
    	    a loopless arrangement
    Description
        Text
	    An arrangement is loopless if none of its forms are identically 0. This method returns
	    the arrangement defined by the non-identically-zero forms of A.
	Example
	    R = QQ[x,y,z]
	    A = dual arrangement {x,y,x-y,z} -- the last element of this arrangement is 0
	    compress A
    SeeAlso
	(trim, Arrangement)
///


doc ///
    Key
	(dual, CentralArrangement, Ring)
	(dual, CentralArrangement)
    Headline
        the Gale dual of an arrangement
    Usage
    	dual A or dual(A, R)
    Inputs
        A : CentralArrangement
	R : Ring
    Outputs
        : CentralArrangement
    	    the Gale dual of A, optionally over the polynomial ring R.
    Description
        Text 
    	    The dual of an arrangement of rank $r$ with $n$ hyperplanes is an
	    arrangement of rank $n-r$ with $n$ hyperplanes, given by a 
	    linear realization of the dual matroid to that of ${\mathcal A}$.
	    It is computed from a presentation of the kernel of the 
	    coefficient matrix of ${\mathcal A}$.  If ${\mathcal A}$ is the
	    @TO2((graphic,List),"arrangement of a planar graph")@ then
	    the dual of ${\mathcal A}$ is the arrangement of the dual graph.
    	Example
    	    A = arrangement "X2"
	    coefficients A
	    A' = dual A
	    coefficients dual A
	    assert (dual matroid A == matroid dual A)
    SeeAlso
 	(HyperplaneArrangements)
	(coefficients, Arrangement)
	(dual, Matroid)
///

doc ///
    Key
    	(genericArrangement, ZZ, ZZ, Ring)
	(genericArrangement, ZZ, ZZ)
	genericArrangement
    Headline
        realize the uniform matroid using points on the monomial curve
    Usage
        genericArrangement(r,n,K)
    	genericArrangement(r,n)
    Inputs
        r : ZZ
    	    the rank of the arrangement
	n : ZZ
	    the number of hyperplanes
	K : Ring
	    a coefficient ring: $\QQ$ by default
    Outputs
        : Arrangement
	    the arrangement with linear forms normal to 
	    $(1,j,j^2,\cdots,j^{r-1})$, for $1\leq j\leq n$.
    Description
        Text 
	    By definition, a generic arrangement is a realization of a uniform
	    matroid $U_{r,n}$, which is characterized by the property that all
	    subsets of the ground set of size at most $r$ are independent.
	    Points on the monomial curve have this property.
     	Example
	    poincare genericArrangement(3,5,QQ)
    SeeAlso
        randomArrangement
///

doc ///
    Key
    	(substitute, Arrangement, RingMap)
	(substitute, Arrangement, Ring)
	(sub, Arrangement, RingMap)
	(sub, Arrangement, Ring)
	(symbol **, Arrangement, RingMap)
    Headline
    	change the ring of an arrangement
    Usage
    	substitute(arr, f)
	sub(arr, f)
	arr ** f
    Inputs
    	arr : Arrangement
	f : RingMap
	    with source {\tt ring arr}, or @ofClass Ring@ for which {\tt map(f, ring arr)} makes sense
    Outputs
    	: Arrangement
	    the arrangement defined by applying {\tt f} (if {\tt f} is @ofClass RingMap@) or 
	    {\tt map(f, ring arr)} (if {\tt f} is @ofClass Ring@) to each defining linear form
    Description
    	Example
	    R = QQ[x,y]
	    arr = arrangement{x,y,x-y}
	    f = map(QQ[a,b], R, {a, a+b})
	    sub(arr, f)
	Text
	    Alternatively, you can use {\tt **}.
	Example
	    arr ** f === sub(arr, f)
	Text
	    Given @ofClass Ring@ {\tt S}, {\tt sub(arr, S)} is the same as {\tt sub(arr, map(S, ring arr))}.
	Example
	    S = QQ[x,y,z]
	    arr' = sub(arr, S)
	    ring arr' === S
	Text
	    Note that the underlying matroid of the arrangement may change as
	    a result of changing the ring.  For example, the Fano matroid
	    is realizable only in characteristic 2:
	Example
	    R = ZZ[x,y,z]
	    A = arrangement("nonFano",R)
	    f = map(ZZ/2[x,y,z],R);
	    B = A**f
	    flats(2,A)
	    flats(2,B)
    SeeAlso
    	(map, Ring, Ring)
	(symbol **, Arrangement, Ring)
///

doc ///
    Key
    	(symbol **, Arrangement, Ring)
    Headline
    	change the coefficient ring of an arrangement
    Usage
    	A ** K
    Inputs
    	A : Arrangement
	K : Ring
    Outputs
    	: Arrangement
	    the hyperplane arrangement defined by tensoring the 
	    @TO2((ring, Arrangement), "underlying ring")@ with $K$.	    
    Description
    	Text
	    This methods makes a new hyperplane arrangement by changing the
	    coefficient ring of the underlying ring.  
    	Example
	    R = ZZ[x,y];
	    A = arrangement{x,y,x-y}
	    A' = A ** QQ
	    ring A'
	    assert(R =!= ring A')
    SeeAlso
    	(sub, Arrangement, RingMap)
	(sub, Arrangement, Ring)
	(symbol **, Arrangement, RingMap)
///

doc ///
    Key
	(typeA, ZZ, Ring)    
    	(typeA, ZZ, PolynomialRing)
	(typeA, ZZ)
	typeA
    Headline
    	make the hyperplane arrangement defined by a type $A_n$ root system
    Usage
    	typeA(n, k)
	typeA(n, R)
	typeA n
    Inputs
    	n : ZZ
	    that is positive
	k : Ring
            that determines the coefficient ring of the hyperplane arrangement
	    or @ofClass PolynomialRing@ $R$ that determines the 
	    @TO2((ring, Arrangement), "ambient ring")@
    Outputs
    	: Arrangement
    Description
    	Text
	    Given a coefficient ring $k$, the {\em Coxeter arrangement} of
	    type $A_n$ is the hyperplane arrangement in $k^{n+1}$ defined by
	    $x_i - x_j$ for all $1 \leq i < j \leq n+1$.
	Example
	    A0 = typeA(3, ZZ)
	    ring A0
	    A1 = typeA(4, QQ)
	    ring A1
	    A3 = typeA(2, ZZ/2)
	    ring A3
    	Text
	    When the second input is a polynomial ring $R$, this ring determines the
	    ambient ring of the Coxeter arrangement.  The polynomial ring must
	    have at least $n+1$ variables.
    	Example
	    A4 = typeA(3, ZZ[a,b,c,d])
	    ring A4
	    A5 = typeA(2, ZZ[t][x,y,z])
	    ring A5
	Text
	    Omitting the ring (or second argument) is equivalent to setting $k := \mathbb{Q}$.
	Example
	    A6 = typeA 2
	    ring A6
    SeeAlso
    	(arrangement, List, Ring)
	(typeB, ZZ, Ring)
	(typeD, ZZ, Ring)
///

doc ///
    Key
	(typeB, ZZ, Ring)    
    	(typeB, ZZ, PolynomialRing)
	(typeB, ZZ)
	typeB
    Headline
    	make the hyperplane arrangement defined by a type $B_n$ root system
    Usage
    	typeB(n, k)
	typeB(n, R)
	typeB n
    Inputs
    	n : ZZ
	    that is positive
	k : Ring
            that determines the coefficient ring of the hyperplane arrangement
	    or @ofClass PolynomialRing@ $R$ that determines the 
	    @TO2((ring, Arrangement), "ambient ring")@
    Outputs
    	: Arrangement
    Description
    	Text
	    Given a coefficient ring $k$, the {\em Coxeter arrangement} of
	    type $B_n$ is the hyperplane arrangement in $k^{n}$ defined by
	    $x_i$ for all $1 \leq i \leq n$ and $x_i \pm x_j$ for all 
	    $1 \leq i < j \leq n$.
	Example
	    A0 = typeB(3, ZZ)
	    ring A0
	    A1 = typeB(4, QQ)
	    ring A1
	    A3 = typeB(2, ZZ/2)
	    trim A3
	    ring A3
    	Text
	    When the second input is a polynomial ring $R$, this ring determines the
	    ambient ring of the Coxeter arrangement.  The polynomial ring must
	    have at least $n$ variables.
    	Example
	    A4 = typeB(3, ZZ[a,b,c,d])
	    ring A4
	    A5 = typeB(2, ZZ[t][x,y,z])
	    ring A5
	Text
	    Omitting the ring (or second argument) is equivalent to setting $k := \mathbb{Q}$.
	Example
	    A6 = typeB 3
	    ring A6
	    A7 = typeB 1	
	    ring A7    
    SeeAlso
    	(arrangement, List, Ring)
	(typeA, ZZ, Ring)
	(typeD, ZZ, Ring)
///

doc ///
    Key
	(typeD, ZZ, Ring)    
    	(typeD, ZZ, PolynomialRing)
	(typeD, ZZ)
	typeD
    Headline
    	make the hyperplane arrangement defined by a type $D_n$ root system
    Usage
    	typeD(n, k)
	typeD(n, R)
	typeD n
    Inputs
    	n : ZZ
	    that is greater than $1$
	k : Ring
            that determines the coefficient ring of the hyperplane arrangement
	    or @ofClass PolynomialRing@ $R$ that determines the 
	    @TO2((ring, Arrangement), "ambient ring")@
    Outputs
    	: Arrangement
    Description
    	Text
	    Given a coefficient ring $k$, the {\em Coxeter arrangement} of
	    type $D_n$ is the hyperplane arrangement in $k^{n}$ defined by
	    $x_i \pm x_j$ for all $1 \leq i < j \leq n$.
	Example
	    A0 = typeD(3, ZZ)
	    ring A0
	    A1 = typeD(4, QQ)
	    ring A1
	    A3 = typeD(2, ZZ/2)
	    trim A3
	    ring A3
    	Text
	    When the second input is a polynomial ring $R$, this ring determines the
	    ambient ring of the Coxeter arrangement.  The polynomial ring must
	    have at least $n$ variables.
    	Example
	    A4 = typeD(3, ZZ[a,b,c,d])
	    ring A4
	    A5 = typeD(2, ZZ[t][x,y,z])
	    ring A5
	Text
	    Omitting the ring (or second argument) is equivalent to setting $k := \mathbb{Q}$.
	Example
	    A6 = typeD 3
	    ring A6
    SeeAlso
    	(arrangement, List, Ring)
	(typeA, ZZ, Ring)
	(typeB, ZZ, Ring)
///


doc ///
    Key
    	(randomArrangement,ZZ,PolynomialRing,ZZ)
	(randomArrangement,ZZ,ZZ,ZZ)
	randomArrangement
	[randomArrangement, Validate]
    Headline
        generate an arrangement at random
    Usage
    	randomArrangement(n,R,N)
    Inputs
        n : ZZ
	    number of hyperplanes
	R : PolynomialRing
	    a polynomial ring over which to define the arrangement, 
	    or a number of variables l instead
	N : ZZ
	    absolute value of upper bound on coefficients
	Validate => Boolean
	    if true, the method will attempt to return an arrangement whose
	    underlying matroid is uniform.
    Outputs
        : Arrangement
    	    a random rational arrangement of $n$ hyperplanes defined over $R$.
    Description
        Text
	    As $N$ increases, the random arrangement is a generic arrangement 
	    (i.e., a realization of the @TO2 {(uniformMatroid), "uniform matroid"}@
	     with probability tending to 1.  The user can require that the
	     arrangement generated is actually generic by using the option
	     {\tt Validate => true}.
	Example
	    randomArrangement(4,3,5)
	Text
	    If an arrangement has the @TO2 {(poincare, Arrangement), "poincare polynomial"}@
	    of a generic arrangement, then it is itself generic.
	Example
	    tally apply(12, i -> poincare randomArrangement(6,3,5))
    	    A = randomArrangement(6,3,5,Validate=>true)
	    U = uniformMatroid(3,6);
	    assert areIsomorphic(U, matroid A)
    Caveat
        If the user specifies {\tt Validate => true} and $N$ is too small,
    	the method may not halt.
    SeeAlso
    	genericArrangement
///

doc ///
    Key
    	(poincare, Arrangement)
	(poincare, CentralArrangement)
	poincare
    Headline
    	compute the Poincaré polynomial of an arrangement
    Usage
    	poincare A
    Inputs
    	A : Arrangement    	   		    
    Outputs
    	: RingElement
	    its Poincaré polynomial, an element of the degrees ring.
    Description
    	Text
	    The Poincaré polynomial $\pi({\mathcal A},t)$ of a central arrangement of rank $r$ equals
	    $t^r\,T(1+t^{-1},0)$, where $T(x,y)$ is the Tutte polynomial.
	    Alternatively,
	    \[
	    \pi({\mathcal A},t)=\sum_F\mu(\widehat{0},F)(-t)^{r(F)},
	    \]
    	    where the sum is over all flats $F$, the function $\mu$ denotes the Möbius
	    function of the intersection lattice, and $r(F)$ is the rank of the
	    flat $F$.  The characteristic polynomial of an (essential) 
	    arrangement is closely related and defined by
    	    \[
	    \chi({\mathcal A},t)=t^r\pi({\mathcal A},-t^{-1}).
	    \]
	Example
	    A = arrangement "MacLane";
	    poincare A
	    characteristicPolynomial matroid A
    	Text
	    If ${\mathcal A}$ is an arrangement defined over the complex
	    numbers, a classical theorem of Brieskorn-Orlik-Solomon asserts
	    that $\pi({\mathcal A},t)$ is also the Poincaré polynomial of 
	    the complement of the union of hyperplanes.  
	    In certain interesting cases, the Poincaré polynomial factors into 
	    linear factors.  This is the case if ${\mathcal A}$ is the
	    set of reflecting hyperplanes associated with a real or 
	    complex reflection group, in which case the (co)exponents of the
	    reflection group appear as the linear coefficients of the factors.
    	Example
	    factor poincare typeA 3
    	Text
    	    More generally (since reflection arrangements are free), if the
	    @TO2{der, "module of logarithmic derivations"}@ $D({\mathcal A})$ 
	    on $\mathcal A$ is free, Terao's Factorization Theorem states that
	    the Poincaré polynomial factors as a product 
	    $\prod_{i=1}^r(1+m_i t)$, where the $m_i$'s are the degrees of the
	    generators of the graded free module $D({\mathcal A})$.
	Example
	    A = arrangement "Hessian"; 
	    factor poincare A
	    prune image der A 
    	Text
	    The Poincaré polynomial appears in various enumerative contexts
	    as well.  If ${\mathcal A}$ is an arrangement defined over 
	    the real numbers, then $\pi({\mathcal A},1)$ equals the number
	    of connected components in the complement of the union of 
	    hyperplanes.  Similarly, $d/dt[\pi({\mathcal A},t)]$ evaluated at
	    $t=1$ counts the number of bounded components in the complement
	    of the @TO2(deCone, "decone")@ of ${\mathcal A}$.
	Text
	    If ${\mathcal A}$ is a non-central arrangement, the
	    Poincaré polynomial $\pi({\mathcal A},t)$ equals 
	    $\pi(c{\mathcal A},t)/(1+t)$, where $c{\mathcal A}$ denotes the
	    @TO2{cone, "cone"}@ of ${\mathcal A}$. 
    SeeAlso
    	(der, CentralArrangement)
    	(orlikSolomon, Arrangement)
	(characteristicPolynomial, Matroid)
///	

doc ///
    Key 
	(orlikSolomon, Arrangement, PolynomialRing)
	(orlikSolomon, CentralArrangement, PolynomialRing)
   	(orlikSolomon, Arrangement, Ring)
	(orlikSolomon, Arrangement, Symbol)
	(orlikSolomon, Arrangement)
	orlikSolomon
	[orlikSolomon, HypAtInfinity]
	[orlikSolomon, Projective]
	[orlikSolomon, Strategy]
	Popescu
    Headline
    	compute the defining ideal for the Orlik-Solomon algebra
    Usage
    	orlikSolomon(A,E)
	orlikSolomon(A,k)
	orlikSolomon(A,e)
	orlikSolomon(A)
    Inputs
    	A : Arrangement
	E: PolynomialRing
	    a skew-commutative polynomial ring with one variable for each hyperplane
	    with indexed variables, optionally, given by the symbol $e$.  
	    The user can also just specify a coefficient ring $k$.
    Outputs
    	: Ideal
	    the defining ideal of the Orlik-Solomon algebra of A
    Description
    	Text
	    The Orlik-Solomon algebra is the cohomology ring of the 
	    complement of the hyperplanes, either in complex projective 
	    or affine space. The optional Boolean argument Projective specifies 
	    which. 
	    
	    A fundamental property is that its Hilbert series is determined
	    by combinatorics: namely, up to a change of variables, it is the
	    characteristic polynomial of the matroid of the arrangement. 
	Example
	    A = typeA(3)
	    I = orlikSolomon(A,e)
	    reduceHilbert hilbertSeries I
	    characteristicPolynomial matroid A
	Text
	    The cohomology ring of the complement of an arrangement in 
	    projective space is most naturally described as 
	    the subalgebra of the Orlik-Solomon algebra
	    generated in degree $1$ by elements whose coefficients sum to $0$.
	    
	    This is inconvenient for Macaulay2; on the other hand, one can
	    choose a chart for projective space that places a hyperplane of
	    the arrangement at infinity.  This expresses the projective
	    Orlik-Solomon algebra as a quotient of a polynomial ring.
	    
	    By selecting the Projective option, the user can specify which
	    hyperplane is placed at infinity.  By default, the first one in
	    order is used.
    	Example	    
	    I' = orlikSolomon(A,Projective=>true,HypAtInfinity=>2)
	    reduceHilbert hilbertSeries I'
	Text
	    The method caches the list of @TO2{circuits, "circuits"}@ of the
	    arrangement.  By default, the method uses the @TO2(Matroids, 
		"Matroids")@ package to compute the Orlik-Solomon ideal.  The
	    option "Strategy=>Popescu" uses code by Sorin Popescu instead.
    Caveat
    	The coefficient rings of the Orlik-Solomon algebra and of the 
	arrangement, respectively, are unrelated.	    
    SeeAlso
        (poincare,Arrangement)
	(EPY,Arrangement)
///	    

doc ///
    Key
    	(orlikTerao, CentralArrangement, PolynomialRing)
	(orlikTerao, CentralArrangement, Symbol)
	(orlikTerao, CentralArrangement)
	orlikTerao
	[orlikTerao, NaiveAlgorithm]
    Headline
    	compute the defining ideal for the Orlik-Terao algebra
    Usage
    	orlikTerao(A,S)
	orlikTerao(A,x)
    	orlikTerao(A)
    Inputs
    	A: CentralArrangement
	    a hyperplane arrangement
	S: PolynomialRing
	    a polynomial ring with one variable for each hyperplane with 
	    indexed variables, optionally, given by the symbol $x$.
	NaiveAlgorithm => Boolean
    Outputs
    	: Ideal
	    the defining ideal of the Orlik-Terao algebra of A
    Description
    	Text
	    The Orlik-Terao algebra of an arrangement is the subalgebra of
	    rational functions $k[1/f_1,1/f_2,\ldots,1/f_n]$, where
	    the $f_i$'s are the defining forms for the hyperplanes.
	    The method produces an ideal presenting the Orlik-Terao algebra
	    as a quotient of a polynomial ring in $n$ variables.
	Example
	    R = QQ[x,y,z];
	    orlikTerao arrangement {x,y,z,x+y+z}
	Text
	    The defining ideal above has one generator given by the single
	    relation coming from the identity $x+y+z-(x+y+z)=0$.  In general,
	    the ideal is homogeneous with respect to the standard grading,
	    but its degrees of generation are not straightforward.  The 
	    projective variety cut out by this ideal is also called the
	    reciprocal plane.
	Example
	    I = orlikTerao arrangement "braid"
	    betti res I
	    OT := comodule I;
	    apply(1+dim OT, i-> 0 == Ext^i(OT, ring OT))
	Text
	    As the example above hints, the Orlik-Terao algebra is always
	    Cohen-Macaulay: see N. J. Proudfoot and D. E. Speyer, 
	    {\em A broken circuit ring}, Beitrage zur Algebra und Geometrie, 2006,
	    @HREF("https://arxiv.org/abs/math/0410069", "arXiv:math/0410069")@.
	    
	    Unlike the Orlik-Solomon
	    algebra, the isomorphism type of the Orlik-Terao algebra is not
	    a matroid invariant: see the example @TO2("arrangementLibrary", 
		"here.")@  However, Terao proved that the Hilbert series of
	    the Orlik-Terao algebra is a matroid invariant: it is given by
	    the @TO2("poincare","Poincaré polynomial")@:
	    \[
	    \sum_{i\geq 0}\dim (S/I)_it^i=\pi({\mathcal A},t/(1-t)).
	    \]
	Example
	    p = poincare arrangement "braid"
	    F = frac QQ[T]; f = map(F,ring p);
	    sub(f p, {T=>T/(1-T)})
	    reduceHilbert hilbertSeries I
    SeeAlso
    	(der,CentralArrangement)
///

doc ///
    Key
    	Flat
    Headline
    	intersection of hyperplanes
    Description
    	Text
	    A flat is a set of hyperplanes, maximal with respect to the property
	    that they contain a given subspace. In this package, flats are treated
	    as lists of indices of hyperplanes in the arrangement.
    SeeAlso
    	(flat, Arrangement, List)
	(flats, ZZ, Arrangement)
	(flats, Arrangement)
///

doc ///
    Key
    	(symbol ==, Flat, Flat)
    Headline
    	whether two flats are equal
    Usage
    	F == G
    Inputs
    	F : Flat
	G : Flat
    Outputs
    	: Boolean
    	    whether or not F and G are equal
    Description
    	Text
	    Two flats are equal if and only if they belong to the same 
	    @TO2{(arrangement, Flat), "arrangement"}@ and have the same
	    hyperplanes.
    SeeAlso
    	(symbol ==, Arrangement, Arrangement)
///

doc ///
    Key
    	(toList, Flat)
    Headline
    	the indices of a flat
    Usage
    	toList F
    Inputs
    	F : Flat
    Outputs
    	: List
	    the indices of the hyperplanes of a $F$
    Description
    	Text
	    As stated in @TO Flat@, flats are treated in this package as lists of indices
	    of hyperplanes in the arrangement. This method returns that list.
	Example
	    A3 = typeA 3
	    F = flat(A3, {3,4,5})
	    assert(toList F === {3,4,5})
	Text
	    Often one wants the corresponding linear forms. This can be accomplished
	    using subscripts:
	Example
	    (hyperplanes A3)_(toList F)
    SeeAlso
    	(toList, Arrangement)
///

doc ///
    Key
    	(flat, Arrangement, List)
	flat
	[flat, Validate]
    Headline
    	make a flat from a list of indices
    Usage
    	flat(A,L)
    Inputs
    	A : Arrangement
	    hyperplane arrangement
	L : List
	    list of indices in flat
	Validate => Boolean
	    whether or not to check if $L$ is indeed a flat of $A$ (default {\tt true})
    Outputs
    	: Flat
	    corresponding flat
    Description
    	Text
	    With the option {\tt Validate => true} (which is the case by default), 
	    {\tt flat(A,L)} checks to see whether $L$ is indeed the list of
	    indices of a flat of $A$.
	Example
	    A = typeA 2
	    flat(A, {0,1,2})
    SeeAlso
    	(flats,ZZ,Arrangement)
	(flats,Arrangement)
///

doc ///
    Key
    	(flats, ZZ, Arrangement)
	(flats, Arrangement)
	(flats, ZZ, CentralArrangement)
	flats
    Headline
    	list the flats of an arrangement of a given rank
    Usage
    	flats(n,A)
    Inputs
    	n : ZZ
	    rank
	A : Arrangement
	    hyperplane arrangement
    Outputs
    	: List
	    a list of @TO2{Flat, "flats"}@ of rank $n$
    Description
    	Text
	    If $A$ is a @TO(CentralArrangement)@, the flats are computed using the 
	    @TO2((flats, Matroid), "flats")@ method from the @TO Matroids@ package. Otherwise,
	    $A$ is computed using the @TO2(orlikSolomon, "Orlik--Solomon algebra")@.
    	Example
	    A = typeA(3)
	    flats(2,A)
	Text
	    If the rank is omitted, the @TO2{Flat, "flats"}@ of each rank are listed.
	Example
	    flats A
    SeeAlso
    	(circuits, CentralArrangement)
	(flats, Matroid)
///

doc ///
    Key
    	(circuits, CentralArrangement)
	circuits
    Headline
    	list the circuits of an arrangement
    Usage
    	circuits(A)
    Inputs
    	A : CentralArrangement
	    hyperplane arrangement
    Outputs
    	: List
	    a list of circuits of $A$, each one expressed as a list of indices
    Description
    	Text
	    A circuit is a minimal dependent set. More precisely, let $f_0,\ldots,f_{n-1}$ 
	    be the polynomials defining the hyperplanes of $A$. A circuit of $A$ is a subset 
	    $C\subseteq \{0,\ldots,n-1\}$ minimal among those for which $\{f_i : i\in C\}$ is 
	    linearly dependent.
	    
	    If $M$ is the @TO2{(matroid,CentralArrangement),"matroid of $A$"}@, then a circuit
	    of $A$ is the same as a circuit of $M$. In fact, {\tt circuits(A)} is defined as
	    {\tt toList \ circuits matroid A}.
	Example
	    A = typeA 3
	    circuits A
	    circuits matroid A
    SeeAlso
    	(flats, Arrangement)
	(circuits, Matroid)
///

doc ///
    Key
    	(closure, Arrangement, List)
	(closure, Arrangement, Ideal)
	closure
    Headline
    	closure operation in the intersection lattice
    Usage
    	closure(A,L) or closure(A,I)
    Inputs
    	A : Arrangement
	    hyperplane arrangement
	L : List
	    a list of indices of hyperplanes, or a linear ideal $I$ in the
	    ring of ${\mathcal A}$
    Outputs
    	: Flat
	    the flat of least codimension containing the hyperplanes $L$,
	    or the flat consisting of those hyperplanes of $\mathcal A$ whose
	    defining forms are also in $I$
    Description
    	Text
	    The closure of a set of indices $L$ consists of (indices of) all
	    hyperplanes that contain the intersection of the given ones.
	    
	    Equivalently, the closure of $L$ consists of all hyperplanes
	    whose defining linear forms are in the span of the linear forms
	    indexed by $L$.
    	Example
	    A = typeA 3
	    F = closure(A,{0,1})	    
	    A_F
    	    I = ideal((hyperplanes A)_{0,3}) -- one can also specify a linear ideal
   	    assert (F == closure(A,I))
	Text
	    The closure of a linear ideal $I$ is the flat consisting of all the
	    hyperplanes in $\mathcal A$ whose defining forms are also in $I$.
    SeeAlso
    	(meet, Flat, Flat)
	(vee, Flat, Flat)	    
	(closure, Matroid, Set)
///	    	    		

doc ///
    Key
	(meet, Flat, Flat)
	meet
	(symbol ^, Flat, Flat)
    Headline
        compute the meet operation in the intersection lattice
    Usage
    	meet(F, G)
	F ^ G
    Inputs
        F : Flat
	G : Flat
	    in the same arrangement as $F$
    Outputs
        : Flat
    	    having the greatest codimension among those contained in both $F$
    	    and $G$
    Description
        Text
	    In the geometric lattice of flats, the meet (also known as the
	    infimum or greatest lower bound) is the intersection of the flats.
	    Equivalently, identifying flats with subspaces, this operation is
	    the Minkowski sum of the subspaces.
	Text
    	    The meet operation is commutative, associative, and idempotent.
	Example
    	    A = typeA 6;
	    F = flat(A, {0, 1, 6, 15, 20})
	    G = flat(A, {0, 1, 2, 6, 7, 11})
	    H = flat(A, {0, 1, 2, 3, 6, 7, 8, 11, 12, 15})
	    F ^ G
	    G ^ H
	    F ^ H
	    assert(meet(F, G) === F ^ G)
	    assert(F ^ G === G ^ F)
	    assert((F ^ G) ^ H === F ^ (G ^ H))
	    assert(G ^ G === G)
	Text
	    The rank function is also semimodular.
	Example
	    assert(rank F + rank G >= rank(F ^ G) + rank(F | G))
	    assert(rank F + rank H >= rank(F ^ H) + rank(F | H))
	    assert(rank H + rank G >= rank(H ^ G) + rank(H | G))	    	    
    SeeAlso
    	(rank, Flat)
	(vee, Flat, Flat)
///

doc ///
    Key
	(vee, Flat, Flat)
	vee
	(symbol |, Flat, Flat)
    Headline
        compute the vee operation in the intersection lattice
    Usage
    	vee(F, G)
	F | G
    Inputs
        F : Flat
	G : Flat
	    in the same arrangement as $F$
    Outputs
        : Flat
    	    having the least codimension among those contained in both $F$
    	    and $G$
    Description
        Text
	    In the geometric lattice of flats, the vee (also known as the
	    supremum or least upper bound) is the join operation.
	    Equivalently, identifying flats with subspaces, this operation is
	    the closure of the union.
	Text
    	    The vee operation is commutative, associative, and idempotent.
	Example
    	    A = typeA 6;
	    F = flat(A, {0, 1, 6, 15, 20})
	    G = flat(A, {0, 1, 2, 6, 7, 11})
	    H = flat(A, {0, 1, 2, 3, 6, 7, 8, 11, 12, 15})
	    F | G
	    G | H
	    F | H
	    assert(vee(F, G) === F | G)
	    assert(F | G === G | F)
	    assert((F | G) | H === F | (G | H))
	    assert(G | G === G)
	Text
	    The rank function is also semimodular.
	Example
	    assert(rank F + rank G >= rank(F ^ G) + rank(F | G))
	    assert(rank F + rank H >= rank(F ^ H) + rank(F | H))
	    assert(rank H + rank G >= rank(H ^ G) + rank(H | G))	    	    
    SeeAlso
    	(rank, Flat)
	(vee, Flat, Flat)
///

doc ///
    Key
        (euler, CentralArrangement)
        (euler, Flat)
    Headline
        compute the Euler characteristic of the projective complement
    Usage
        euler A
    Inputs
        A : CentralArrangement
	    or a @TO(Flat)@
    Outputs
        : ZZ 
	    equal to the Euler characteristic    
    Description
        Text	 
    	    For any topological space, the {\em Euler characteristic} is
    	    the alternating sum of its Betti numbers (a.k.a. the ranks of its
    	    homology groups).  For a central hyperplane arrangement, the
    	    associated topological space is the projectivization of its
    	    complement.
	Text
	    The Euler characteristic for the hyperplane arrangements defined by
	    root systems are described by simple formulas.
	Example
	    A2 = typeA 2
	    euler A2
	    assert all(5, n -> euler typeA (n+1) === (-1)^(n) * n!)
	    B2 = typeB 2
	    euler B2
    	    assert all(4, n -> euler typeB (n+1) === (-1)^(n) * 2^n * n!)
	Text 
	    Given a flat, this method computes the Euler characteristic of
	    the subarrangement indexed by the flat.
	Example
	    A4 = typeA 4
	    F = flat(A4, {0,7})
	    euler F
	    assert(euler A4_F === euler F)
	    euler flat(A4, {2,3,9})
	    euler flat(A4, {0,1,2,4,5,7})
	    euler flat(A4, {2,4,6,8})
	Text
	    The Euler characteristic of the empty arrangement is just the
	    Euler characteristic of the ambient projective space.  For
	    instance, the Euler characteristic of the complex projective plane
	    is $3$.
	Example
	    assert (euler arrangement({}, ring A2) === 3)
    SeeAlso
        typeA
	typeB
	subArrangement
	flat
///

	  
doc ///
    Key
    	(deletion, Arrangement, RingElement)
	(deletion, Arrangement, List)
	(deletion, Arrangement, Set)
	(deletion, Arrangement, ZZ)
	deletion
    Headline 
    	deletion of a subset of an arrangement
    Usage
    	deletion(A,x) 
	deletion(A,S)
	deletion(A,i)
    Inputs
    	A : Arrangement
	x : RingElement
	    alternatively, the second argument can be the index of a hyperplane, or a set or list of indices of hyperplanes
    Outputs
    	: Arrangement
	    obtained by deleting the linear form $x$, or the subset $S$, or the $i$th linear form
    Description
    	Text
	    The deletion is obtained by removing hyperplanes from ${\mathcal A}$.
    	Example
	    A = arrangement "braid"
	    deletion(A,5)
	Text
	    You can also remove a hyperplane by specifiying its linear form.
	Example
	    R = QQ[x,y]
	    A = arrangement {x,y,x-y}
	    deletion(A, x-y)
	Text
	    If multiple linear forms define the same hyperplane $H$, deleting any one of those 
	    forms does the same thing: it finds the first linear form in $\mathcal A$
	    defining $H$, then deletes that one.
	Example
	    A = arrangement {x, x-y, y, x-y, y-x}
	    A1 = deletion(A, x-y)
	    A2 = deletion(A, y-x)
	    A3 = deletion(A, 2*(x-y))
	    assert(A1 == A2)
	    assert(A2 == A3)
    SeeAlso
    	(deletion, Matroid, List)
///	 
	  	  
doc ///
    Key
	(restriction, Arrangement, Ideal)
    	(restriction, Arrangement, RingElement)
	(restriction, Arrangement, List)
	(restriction, Arrangement, Set)
	(restriction, Arrangement, ZZ)
	(restriction, Arrangement, Flat)
	(symbol ^, Arrangement, Flat)
	(restriction, Flat)		  
	restriction
    Headline
    	construct the restriction a hyperplane arrangement to a subspace
    Usage
    	restriction(A, I)
	restriction(A, F)
	A ^ F
	restriction F
    Inputs
    	A : Arrangement
	I : Ideal
	    an ideal defining the subspace to which we restrict.  One may
	    also specify a single ring element or a set of indices.  In 
	    the latter case, the subspace is the intersection of the 
	    corresponding hyperplanes.
    Outputs
    	: Arrangement
    Description
    	Text
	    The restriction of an arrangement ${\mathcal A}$ to a subspace
	    $X$ is the (multi)arrangement with
	    hyperplanes $H_i\cap X$, where $H\in {\mathcal A}$ but 
	    $H\not\supseteq X$.  The subspace $X$ may be defined by a ring
	    element or an ideal.
	    
	    If an index or list (or set) of hyperplanes $S$ is given, then
	    $X=\bigcap_{i\in S}H_i$.  In this case, the restriction is a 
	    realization of the matroid contraction $M/S$, where $M$ denotes
	    the matroid of ${\mathcal A}$.
	    
	    In general, the restriction is denoted ${\mathcal A}^X$.  
	    Its ambient space is $X$.  
	    
	Example
	    A = typeA(3)
	    L = flats(2,A)
	    A' = restriction first L
	    x := (ring A)_0 -- the subspace need not be in the arrangement
	    restriction(A,x)
        Text
	    Unfortunately, the term ``restriction'' is used in conflicting
	    senses in arrangements versus matroids literature.  In the latter
	    terminology, ``restriction'' to $S$ is a synonym for the deletion
	    of the complement of $S$.
    SeeAlso
    	deletion
	subArrangement
    	eulerRestriction
///
 
doc ///
    Key
    	(eulerRestriction, CentralArrangement, List, ZZ)
	eulerRestriction
    Headline
    	form the Euler restriction of a central multiarrangement
    Usage
    	eulerRestriction(A, m, i)
    Inputs
        A : CentralArrangement
	m : List
	i : ZZ
    Outputs
    	: Sequence
	    the Euler restriction of (A,m)
    Description
    	Text
	    The Euler restriction of a multiarrangement (introduced by Abe, Terao, 
	    and Wakefield in @HREF("https://doi.org/10.1112/jlms/jdm110",
		"The Euler multiplicity and addition–deletion theorems for multiarrangements")@, 
	    {\em J. Lond. Math. Soc.} (2) 77 (2008), no. 2, 335348.) generalizes 
	    @TO2 {(restriction, Arrangement, Ideal), "restriction"}@ to multiarrangements
	    in such a way that addition-deletion theorems hold.  The underlying
	    simple arrangement of the Euler restriction is simply the usual
	    restriction; however, the multiplicities are generally smaller
	    than the naive ones.
	Text
	    If all of the multiplicities are $1$, the same is true of the 
	    Euler restriction:
	Example
	    R = QQ[x,y,z]
	    A = arrangement {x,y,z,x-y,x-z}
    	    (A'',m'') = eulerRestriction(A,{1,1,1,1,1},1)
    	    restriction(A,1) 
	    trim oo -- same underlying simple arrangement, different multiplicities
	Text
	    If $({\mathcal A},m)$ is a free multiarrangement and so is
	    $({\mathcal A},m')$, where $m'$ is obtained from $m$ by lowering a
	    single multiplicity by one, the Euler restriction is free as
	    well, and the modules of @TO2 {(der, CentralArrangement, List), 
		"logarithmic derivations"}@ form a short
	    exact sequence.  See the paper of Abe, Terao and Wakefield for
	    details.
	Example
	    m = {2,2,2,2,1}; m' = {2,2,2,1,1};
	    (A'',m'') = eulerRestriction(A,m,3)
    	    prune image der(A,m)
	    prune image der(A,m')
	    prune image der(A'',m'')       
	Text
	    It may be the case that the Euler restriction is free, while the
	    naive restriction is not:
	Example
	    A = arrangement "bracelet";
	    (B,m) = eulerRestriction(A,{1,1,1,1,1,1,1,1,1},0)
	    C = restriction(A,0)
	    assert(isFreeModule prune image der B) -- one is free
	    assert(not isFreeModule prune image der C) -- the other is not
    SeeAlso
     	 (restriction, Arrangement, ZZ)
///	   

doc ///
    Key 
        (prune, Arrangement)
    Headline
    	makes a new hyperplane arrangement in a polynomial ring
    Usage
    	prune A 
    Inputs
    	A : Arrangement
	Exclude =>
	    this optional input is ignored by this function 
    Outputs
    	: Arrangement
	    an isomorphic to the input but defined over a polynomial ring
    Description
    	Text
	    A hyperplane arrangement may sensibly be defined over a quotient
	    of a @TO2(PolynomialRing, "polynomial ring")@ by a linear ideal.
	    However, sometimes this is inconvenient.  This method creates an
	    isomorphic hyperplane arrangement in a polynomial ring.
    	Example
	    A = typeA 3
    	    A'' = restriction(A,0) -- restrict A to its first hyperplane
	    ring A''
	    B = prune A''
	    ring B
    SeeAlso
    	(trim, Arrangement)
	(compress, Arrangement)
	(restriction, Arrangement, ZZ)
///	    	    		
    	
doc ///
    Key
    	(cone, Arrangement, RingElement)
	(cone, Arrangement, Symbol)
    Headline
    	creates an associated central hyperplane arrangement
    Usage
    	cone(A, x)
	cone(A, h)
    Inputs
    	A : Arrangement
	x : RingElement 
	    that is a variable in the ring of $A$, or a @TO Symbol@ that will
	    become a variable in the ring of the new hyperplane arrangement
    Outputs
    	: CentralArrangement
	    constructed by adding a linear hyperplane and homogenizing the
	    given hyperplane equations with respect to it
    Description
    	Text
	    For any hyperplane arrangement $A$, the cone of $A$ is an
	    associated central hyperplane arrangement constructed by adding a
	    new hyperplane and homogenizing the hyperplane equations in $A$
	    with respect to it.  By definition, the cone of $A$ contains one
	    more hyperplane that $A$.
        Text
	    When the underlying ring of the input arrangement $A$ has a
	    variable not appearing in the its linear equations, one can
	    construct the cone over $A$ using that variable.
	Example
	    S = QQ[w,x,y,z];
	    A = arrangement{x, y, x-y, x-1, y-1}
	    assert not isCentral A
	    cA = cone(A, z)
	    assert isCentral cA
	    assert(# hyperplanes cA === 1 + # hyperplanes A)
	    assert(ring cA === ring A)
	    deCone(cA, z)
	    cA' = cone(A, w)
	    assert isCentral cA'
	    assert(cA != cA')
	    assert(# hyperplanes cA' === 1 + # hyperplanes A)
	Text
	    This method does not verify that the given @TO RingElement@ produces a
	    simple hyperplane arrangement.  Hence, one gets unexpected output
	    when the chosen variable already appears in the linear equations for $A$.
	Example
	    cone(A, x)
	    cA'' = trim cone(A, x)
	    assert isCentral cA''
	    assert(# hyperplanes cA'' =!= 1 + # hyperplanes A)
        Text
    	    When the second input is a @TO Symbol@, this method creates a
    	    new ring from the underlying ring of $A$ by adjoining the symbol as a
    	    variable and constructs the cone in this new ring.
	Example
	    S = QQ[x,y];
	    A = arrangement{x, y, x-y, x-1, y-1}
	    assert not isCentral A
	    cA = cone(A, symbol z)
	    assert isCentral cA
	    assert(# hyperplanes cA === 1 + # hyperplanes A)
	    ring cA
	    assert(ring cA =!= ring A)
	    deCone(cA, 5)
	    assert not isCentral A
	    cA' = cone(A, symbol w)
	    assert isCentral cA'
	    assert(# hyperplanes cA' === 1 + # hyperplanes A)
	    ring cA'	    	    
    SeeAlso
        deCone
	isCentral
	(trim, Arrangement)
///


doc ///
    Key
        (deCone, CentralArrangement, RingElement)
    	(deCone, CentralArrangement, ZZ)
	deCone
	"dehomogenization"
    Headline
    	produce an affine arrangement from a central one
    Usage
    	deCone(A, x)
	deCone(A, i)
    Inputs
    	A : CentralArrangement
	x : RingElement
	    a hyperplane of $A$ or the index of a hyperplane of $A$
    Outputs
    	: Arrangement
	    the decone of $A$ over $x$
    Description
    	Text
	    The decone of a @TO2(CentralArrangement, "central arrangement")@ $A$ at a 
	    hyperplane $H=H_i$ or $H=\ker x$ is the affine arrangement obtained from $A$
	    by first deleting the hyperplane $H$ then intersecting the remaining 
	    hyperplanes with the (affine) hyperplane $\{x=1\}$. In particular, if $R$ is
	    the @TO2((ring, Arrangement), "coordinate ring")@ of $A$, then
	    the coordinate ring of its decone over $x$ is $R/(x-1)$.
	    
	    The decone of a @TO2(CentralArrangement, "central arrangement")@ at $H$
	    can also be constructed by first projectivizing $A$, then removing the image of
	    $H$, and identifying the complement of $H$ with affine space.
	Example
	    A = arrangement "X3"
	    dA = deCone(A,2)
	    factor poincare A
	    poincare dA
	Text
	    The coordinate ring of $dA$ is $\mathbb{Q}[x_1,x_2,x_3]/(x_3-1)$.
	Example
	    ring dA
	Text
	    Use @TO2((prune, Arrangement),"prune")@ to get something whose coordinate
	    ring is a polynomial ring.
	Example
	    dA' = prune dA
	    ring dA'
    SeeAlso
    	(cone, Arrangement, RingElement)
///



doc ///
    Key
    	(subArrangement, Arrangement, Flat)
	(subArrangement, Flat)
	(symbol _, Arrangement, Flat)
	subArrangement
    Headline
    	create the hyperplane arrangement containing a flat
    Usage
    	subArrangement(A, F)
	subArrangement F
	A _ F
    Inputs
    	A : Arrangement
	F : Flat
	    of the hyperplane arrangement $A$
    Outputs
    	: Arrangement
	    consisting of those hyperplanes in $A$ that contain the linear
	    subspace indexed by the flat $F$
    Description
    	Text
	    For any hyperplane arrangement $A$ and any flat $F$ in $A$, this
	    methods creates a new hyperplane arrangement formed by the
	    hyperplanes in $A$ that contain the linear subspace associated to
	    the flat $A$.
        Text
    	    We illustrate this method with the 
	    @TO2(typeA, "Coxeter arrangement of type A")@.
	Example
	    S = QQ[w, x, y, z];
    	    A3 = typeA(3, S)
	    F1 = flat(A3, {3,4,5})
	    A3' = subArrangement(A3, F1)
	    assert(ring A3 === ring A3')
	    subArrangement flat(A3, {0, 5})
	    F2 = flat(A3, {0, 1, 3})
	    assert(typeA(2, S) == A3_F2)
	    assert(A3 === subArrangement flat(A3, {0,1,2,3,4,5}))
    	Text
	    An extension of the 
	    @TO2((arrangement, String, Ring), "bracelet arrangement")@
	    has several subarrangements isomorphic to $A_3$.
    	Example
	    B = arrangement("bracelet", S);
	    B' = arrangement({w+x+y+z} | hyperplanes B)
	    subArrangement flat(B', {0,1,2,6,8,9})
	    subArrangement flat(B', {0,1,3,5,7,9})
	    subArrangement flat(B', {0,2,3,4,7,8})
    SeeAlso
    	(restriction, Arrangement, Ideal)
    	(deletion, Arrangement, RingElement)	    	    	    	    	    	    
///



doc ///
    Key
    	(graphic, List, List, PolynomialRing)
	(graphic, List, List, Ring)
	(graphic, List, List)
	(graphic, List, PolynomialRing)
	(graphic, List, Ring)
    	(graphic, List)
	graphic
    Headline
    	make a graphic arrangement
    Usage
    	graphic(E, V, R)
    	graphic(E, R)
    	graphic(E, V)
	graphic E
    Inputs
    	E : List
	    the edges of a graph expressed as a list of pairs of vertices as
	    specified in $V$	    
	V : List
	    the vertices of a graph expressed as a list of elements	    
	R : PolynomialRing
	    an optional coordinate ring for the arrangement or @ofClass Ring@
	    to be interpreted as a coefficient ring
    Outputs
    	: Arrangement	
	    associated to the given graph    
    Description
    	Text
	    A graph $G$ is specified by a list $V$ of vertices and a list $E$
	    of pairs of vertices.  When $V$ is not specified, it is assumed to
	    be the list $1, 2, \ldots, n$, where $n$ is the largest integer
	    appearing as a vertex of $E$. The {\em graphic arrangement} $A(G)$
	    of $G$ is the subarrangement of the 
	    @TO2(typeA, "type $A_{n-1}$ arrangement")@ with hyperplanes
	    $x_i-x_j$ for each edge $\{i,j\}$ of the graph $G$.
        Example
	    G = {{1,2},{2,3},{3,4},{4,1}}; -- a four-cycle
	    AG = graphic G
	    rank AG -- the number of vertices minus number of components
	    ring AG
	Text
	    One can also specify the ambient ring.
	Example
	    AG' = graphic(G,QQ[x,y,z,w]) -- four variables because there are 4 vertices
	    ring AG'
	Text
	    Occasionally, one might want to give labels to the vertices. These labels can be anything!
	Example
	    V = {"a", "b", "c", "d"};
	    E = {{"a","b"}, {"b", "c"}, {"c","d"}, {"d","a"}};
	    graphic(E, V)
	Text
	    The vertices can also be the variables of a polynomial ring.
	Example
	    R = QQ[a,b,c,d];
	    arr = graphic({{a,b},{b,c},{c,d},{d,a}}, gens R, R)
	    ring arr === R
	Text
	    Loops and parallel edges are allowed.
	Example
	    graphic({{1,2}, {1,2}})
	    graphic({{1,1}, {1,2}})
    SeeAlso
    	(arrangement, List)
    	typeA
	(rank, CentralArrangement)
///

     
doc ///
     Key
     	 (der, CentralArrangement, List)
	 (der, CentralArrangement)
	 der
	 [der, Strategy]
     Headline 
         compute the module of logarithmic derivations
     Usage
     	 der(A, m)
	 der(A)
     Inputs
     	 A : CentralArrangement
	     a central arrangement of hyperplanes
    	 m : List
	     an optional list of multiplicities, one for each hyperplane
	 Strategy => Symbol
	     that specifies the algorithm.  If an arrangement has (squarefree)
	     defining polynomial $Q$, then the logarithmic derivations are
	     those derivations $D$ for which $D(Q)$ is in the ideal $(Q)$.
	     The {\tt Popescu} strategy assumes that the arrangement is simple
	     and implements this definition.  By contrast, the default
	     strategy treats all arrangements as multiarrangements.
     Outputs
         : Matrix
	     whose image is the module of logarithmic derivations
	     corresponding to the (multi)arrangement ${\mathcal A}$; see below.
     Description
     	 Text
	     The module of logarithmic derivations of an arrangement defined
     	     over a ring $S$ is, by definition, the submodule of $S$-derivations
     	     $D$ with the property that $D(f_i)$ is contained in the ideal
	     generated by $f_i$, for each linear form $f_i$ in the arrangement.
	     
	     In this package, we grade derivations so that a constant coefficient
	     derivation (i.e. a derivation $D$ which takes linear forms to constants)
	     has degree 0. In the literature, this is often called {\em polynomial
	     degree}.
	 Text
	     More generally, if the linear form $f_i$ is given a 
     	     positive integer multiplicity $m_i$, then the logarithmic derivations
	     are those $D$ with the property that $D(f_i)$ is in the ideal 
	     $(f_i^{m_i})$ for each linear form $f_i$. See Günter M. Ziegler, 
	     @HREF("https://doi.org/10.1090/conm/090/1000610", 
		 "Multiarrangements of hyperplanes and their freeness")@,
	     in {\em Singularities (Iowa City, IA, 1986)}, 345-359, Contemp. Math., 
	     90, Amer. Math. Soc., Providence, RI, 1989.
    	 Text
	     The $j$th column of the output matrix expresses the $j$th generator
	     of the derivation module in terms of its value on each linear
	     form, in order.
    	 Example
	     R = QQ[x,y,z];
	     der arrangement {x,y,z,x-y,x-z,y-z}
    	 Text
	     This method is implemented in such a way that any derivations of 
     	     degree 0 are ignored.  Equivalently, the arrangement ${\mathcal A}$
    	     is treated as if it were essential: that is, the intersection of
	     all the hyperplanes is the origin. So, the rank of the matrix produced by
	     {\tt der} equals the @TO2 {(rank, CentralArrangement), "rank"}@ of the arrangement.
	     For instance, although the @TO2{typeA, "$A_3$ arrangement"}@
	     is not essential, {\tt der} will produce a rank 3 matrix.
     	 Example
	     prune image der typeA(3)
	     prune image der typeB(4) 
	 Text 
	     A hyperplane arrangement ${\mathcal A}$ is {\em free} if the
	     module of derivations is a free $S$-module.  Not all arrangements
	     are free.
     	 Example
             R = QQ[x,y,z];
	     A = arrangement {x,y,z,x+y+z}
	     der A
	     betti res prune image der A
	 Text
	     The {\tt Popescu} strategy produces a different presentation of
	     the module of logarithm derivations. For instance, in the following example,
	     the first three rows of column 0 means that
	     $x\frac{\partial}{\partial x} + y\frac{\partial}{\partial y} + z\frac{\partial}{\partial z}$
	     is a logarithmic derivation of $\mathcal A$, and the last row of column 0 means
	     that applying this derivation to $xyz(x+y+z)$ produces $4xyz(x+y+z)$.
	 Example
	     der(A, Strategy => Popescu)
	 Text
     	     If a list of multiplicities is not provided, the occurrences of 
             each hyperplane are counted:
	 Example
       	     R = QQ[x,y]
	     prune image der arrangement {x,y,x-y,y-x,y,2*x}   -- rank 2 => free
	     prune image der(arrangement {x,y,x-y}, {2,2,2})  -- same
     SeeAlso
	     (makeEssential, CentralArrangement)
///
  
doc ///
    Key
    	(multiplierIdeal,QQ,CentralArrangement,List)
	(multiplierIdeal,QQ,CentralArrangement)
    	(multiplierIdeal,ZZ,CentralArrangement,List)
	(multiplierIdeal,ZZ,CentralArrangement)
	multiplierIdeal
	multIdeal
	(multIdeal,QQ,CentralArrangement,List)
	(multIdeal,QQ,CentralArrangement)
    	(multIdeal,ZZ,CentralArrangement,List)
	(multIdeal,ZZ,CentralArrangement)
    Headline
    	compute a multiplier ideal
    Usage
        multiplierIdeal(s,A,m)
    	multiplierIdeal(s,A)
	multIdeal(s,A,m)
    	multIdeal(s,A)
    Inputs
    	s : QQ
	    a rational number
    	A : CentralArrangement
	    a central hyperplane arrangement
	m : List
	    an optional list of positive integer multiplicities
    Outputs
    	: Ideal
	    the multiplier ideal of the arrangement at the value $s$
    Description
    	Text
	    The multiplier ideals of an given ideal depend on a nonnegative
	    real parameter.  This method computes the multiplier ideals of
	    the defining ideal of a hyperplane arrangement, optionally with
	    multiplicities $m$. This uses the explicit formula of M. Mustata 
	    [TAMS 358 (2006), no 11, 5015--5023] simplified by Z. Teitler 
	    [PAMS 136 (2008), no 5, 1902--1913].

	    Let's consider Example 6.3 of Berkesch and Leykin from 
	    arXiv:1002.1475v2:
	Example
	    R = QQ[x,y,z]
	    A = arrangement ((x^2 - y^2)*(x^2 - z^2)*(y^2 - z^2)*z)
	    multiplierIdeal(3/7,A)
	Text
	    Since the multiplier ideal is a step function of its
	    real parameter, one tests to see at what values it changes:
	Example
	    H = new MutableHashTable
	    scan(39,i -> (
			s := i/21;
			I := multiplierIdeal(s,A);
			if not H#?I then H#I = {s} else H#I = H#I|{s}))
	    netList sort values H -- values of s giving same multiplier ideal
    SeeAlso
    	logCanonicalThreshold
///   

doc ///
    Key
    	(logCanonicalThreshold, CentralArrangement)
	logCanonicalThreshold
	(lct, CentralArrangement)
	lct
    Headline
    	compute the log-canonical threshold of an arrangement
    Usage
    	logCanonicalThreshold(A)
	lct(A)
    Inputs
    	A : CentralArrangement
	    a central hyperplane arrangement
    Outputs
    	: QQ
	    the log-canonical threshold of $A$
    Description
    	Text
	    The log-canonical threshold of $A$ defined by a polynomial $f$ is the
	    least number $c$ for which the @TO2(multiplierIdeal, "multiplier ideal")@ $J(f^c)$ is 
	    nontrivial.
	    
	    Let's consider Example 6.3 of Berkesch and Leykin from 
     	    arXiv:1002.1475v2:
	Example
	    R = QQ[x,y,z]
	    A = arrangement ((x^2 - y^2)*(x^2 - z^2)*(y^2 - z^2)*z)
	    logCanonicalThreshold A
	Text
	    Note that $A$ is allowed to be a multiarrangement.
    SeeAlso
    	multiplierIdeal
///

doc ///
    Key
    	(EPY, Arrangement, PolynomialRing)
	(EPY, Ideal, PolynomialRing)
	(EPY, Ideal)
	(EPY, Arrangement)
	EPY
    Headline	
        compute the Eisenbud-Popescu-Yuzvinsky module of an arrangement
    Usage
        EPY(A) or EPY(A,S) or EPY(I) or EPY(I,S)
    Inputs
    	A : Arrangement
	    an arrangement of n hyperplanes, or I, an ideal of the exterior algebra, the quotient by which has a 
  	    linear, injective resolution
	S : PolynomialRing
	    an optional polynomial ring in $n$ variables
    Outputs
    	: Module    
	    the Eisenbud-Popescu-Yuzvinsky module (see below) of I, or
	    if an arrangement is given, of its Orlik-Solomon ideal.
    Description
    	Text
	    Let $\mathrm{OS}$ denote the @TO2(orlikSolomon, "Orlik-Solomon algebra")@ 
	    of the arrangement ${\mathcal A}$, regarded as a quotient of an
	    exterior algebra $E$.  The module $\mathrm{EPY}({\mathcal A})$ is, by
	    definition, the $S$-module which is BGG-dual to the linear, 
	    injective resolution of $\mathrm{OS}$ as an $E$-module.  
	Text
	    Equivalently, $\mathrm{EPY}({\mathcal A})$ is the single nonzero cohomology
	    module in the Aomoto complex of ${\mathcal A}$.  For details,
	    see D. Eisenbud, S. Popescu, S. Yuzvinsky, "Hyperplane 
	    arrangement cohomology and monomials in the exterior algebra", 
	    {\em Trans. AMS} 355 (2003) no 11, 4365-4383,
	    @HREF("https://arxiv.org/abs/math/9912212", "arXiv:math/9912212")@,
	    as well as Sheaf Algorithms Using the Exterior Algebra, 
	    by Wolfram Decker and David Eisenbud, in 
	    @HREF("https://faculty.math.illinois.edu/Macaulay2/Book/",
		    "Computations in algebraic geometry with Macaulay 2")@,
		 Algorithms and Computations in Mathematics, Springer-Verlag, 
		 Berlin, 2001.
   	Example
	    R = QQ[x,y];
	    FA = EPY arrangement {x,y,x-y}
	    betti res FA
    	Text
	    A consequence of the theory is that
	    $\mathrm{EPY}({\mathcal A})$ has a linear, free resolution
	    over the polynomial ring: namely, the Aomoto complex of ${\mathcal A}$.
	    The Betti numbers in the resolution are, up to a suitable shift,
	    equal to the degrees of the graded pieces of $\mathrm{OS}({\mathcal A})$.
    	Example
	    A = arrangement "prism"
    	    reduceHilbert hilbertSeries orlikSolomon A
	    betti res EPY A
///	    	    
	    

doc ///
    Key
	(isDecomposable, CentralArrangement, Ring)    
    	(isDecomposable, CentralArrangement)
	isDecomposable
    Headline
    	whether a hyperplane arrangement decomposable in the sense of Papadima-Suciu
    Usage
	isDecomposable(A, R)
    	isDecomposable A	
    Inputs
    	A : CentralArrangement
	R : Ring
	    an optional coefficient ring used as the coefficient field for
	    the holonomy Lie algebra.  If unspecified, R=QQ
    Outputs
    	: Boolean
	    that is @TO true@ if the hyperplane arrangement decomposes in the
	    sense of Papadima and Suciu over the given coefficient field

    Description
    	Text    	    
	    Following Definition 2.3 in Stefan Papadima and Alexander
	    I. Suciu's paper "When does the associated graded Lie algebra of
	    an arrangement group decompose?", {\em Commentarii Mathematici
	    Helvetici} (2006) 859-875,
	    @HREF("https://arxiv.org/abs/math/0309324", "arXiv:math/0309324")@,
	    a hyperplane arrangement is {\em decomposable} if the derived
	    subalgebra of its holonomy Lie algebra is a direct sum of the
	    derived subalgebras of free Lie algebras, indexed by the rank-2 
	    @TO2(Flat, "flats")@ of the arrangement.
	Text
	    As described in the introduction of Papadima-Suciu, the X3
	    arrangement is decomposable.  The hyperplane arrangement defined
	    by a type $A_3$ root system is not decomposable.  The authors
	    show that a @TO2((graphic,List),"graphic arrangement")@
	    is decomposable over ${\mathbb Q}$ if and only if it is 
	    decomposable over any other field.  In general, it is not
	    known if there exist arrangements for which property of being
	    decomposable depends on the choice of field.
    	Example
    	    X3 = arrangement "X3"
	    assert isDecomposable X3
	    assert isDecomposable(X3, ZZ/5)
	    assert not isDecomposable typeA 3
    SeeAlso
    	Flat
    	orlikSolomon
///

doc ///
    Key
    	(matroid, CentralArrangement)
    Headline
    	get the matroid of a central arrangement
    Usage
    	matroid arr
    Inputs
    	arr : CentralArrangement
    Outputs
    	: Matroid
	    the matroid of {\tt arr}
    Description
    	Text
    	    This computes the @ofClass Matroid@ of the given arrangement, which by definition is the matroid defined by
	    the @TO2 {(coefficients, Arrangement), "coefficient matrix"}@ of the arrangement.
    	Example
	    A = matrix{{1,1,0},{-1,0,1},{0,-1,-1}}
	    arr = arrangement A
	    matroid arr
    SeeAlso
    	(matroid, Matrix)
///

doc ///
    Key
    	(isCentral, Arrangement)
	isCentral
    Headline
    	test to see if a hyperplane arrangement is central
    Usage
    	isCentral A
    Inputs
    	A : Arrangement
    Outputs
    	: Boolean
	    true if A is central
    Description
    	Text
	    Some methods only apply to arrangements 
	    @ofClass CentralArrangement@, so it is useful to be able to check.
    	Example
	    S = QQ[x,y];
	    isCentral arrangement {x,y,x-1}
    SeeAlso
    	CentralArrangement
///	    	

doc ///
    Key
    	(arrangementSum, Arrangement, Arrangement)
	(symbol ++, Arrangement, Arrangement)
        arrangementSum
    Headline
    	make the direct sum of two arrangements
    Usage
    	arrangementSum(A,B)
	A ++ B
    Inputs
    	A : Arrangement
	B : Arrangement
    Outputs
    	: Arrangement
	    the sum ${\mathcal A} \oplus {\mathcal B}$
    Description
    	Text
	    Given two hyperplane arrangements ${\mathcal A}$ in $V$ and
	    ${\mathcal B}$ in $W$, the {\em sum} ${\mathcal A} \oplus
	    {\mathcal B}$ is the hyperplane arrangement in $V \oplus W$ with
	    hyperplanes $\{ H \oplus W \colon H \in {\mathcal A} \} \cup \{ V
	    \oplus H \colon H\in {\mathcal B} \}$.  The ring of the direct sum
	    is {\tt (ring A) ** (ring B)} with all the generators assigned
	    degree 1.	    
	Example
	    R = QQ[w,x];
	    S = QQ[y,z];
	    A = arrangement{w, x, w-x}
	    B = arrangement{y, z, y+z}
	    C = A ++ B
	    gens ring C
	    assert (degrees ring C === {{1}, {1}, {1}, {1}})
    Caveat
    	Both hyperplane arrangements must be defined over the same coefficient
	ring.
    SeeAlso
    	(subArrangement, Flat)
	(restriction, Flat)
	isDecomposable
///	    


doc ///
    Key
    	(hyperplanes, Arrangement)
	(toList, Arrangement)
	hyperplanes
    Headline
    	the defining linear forms of an arrangement
    Usage
    	hyperplanes A
	toList A
    Inputs
    	A : Arrangement
    Outputs
    	: List
	    the list of linear forms defining $A$.
    Description
    	Text
	    This returns the list of linear forms defining an arrangement. These forms
	    will be elements of the @TO2((ring, Arrangement), "coordinate ring")@ of $A$.
	Example
	    A = typeA 3
	    hyperplanes A
    SeeAlso
    	(matrix, Arrangement)
	(coefficients, Arrangement)
///

--===========================================================================
-- TESTS
--===========================================================================
-*
LIST OF TESTS
indented = completed
** = need to begin

arrangement(List, Ring)	 -- not clear what to do here
arrangement(List, Matrix)				    
arrangement String
arrangement Flat
arrangement(Flat, Validate=>true)

    circuits 
    closure 
coefficients Arrangement    -- modify for affine?
    compress
    cone 

    deCone 
    deletion 
    der 
    dual 

    EPY 
    euler     -- in doc
    eulerRestriction

    flat 
    flats

    genericArrangement
    graphic

    isCentral  -- in docs
    isDecomposable  -- in typeA tests

    logCanonicalThreshold  

    matrix 
    matroid
    makeEssential -- in doc
    meet and ^  -- in doc
    multiplierIdeal  -- in lct

    net Flat 

    orlikTerao 
    orlikSolomon

    poincare CentralArrangement
    prune

    randomArrangement  -- in doc
    rank Arrangement					    
    rank Flat
    restriction -- in doc
    ring    -- in doc

    sub(Arrangement, RingMap) and **
    subArrangement and _  -- in doc

    toList Flat 
    trim
    typeA
    typeB
    typeD

    vee and |   -- in doc

*-



--------------------------------------
-- Tests for `arrangement` and stuff
--------------------------------------
TEST ///
R = ZZ[x,y,z];
trivial = arrangement({},R);
nontrivial = arrangement({x},R);
assert(rank trivial == 0)
assert(ring trivial === R)
assert(0 == matrix trivial)
assert(0 == coefficients trivial)
assert(deletion(nontrivial,x) == trivial)
assert(trivial++trivial != trivial)
assert(trivial**QQ != trivial)
///


-----------------------------------------------------------
-- Testing `typeA` and making arrangements using matrices
-----------------------------------------------------------
TEST ///
R = ZZ[x_1..x_4];
hyps = {x_1-x_2,x_1-x_3,x_1-x_4,x_2-x_3,x_2-x_4,x_3-x_4}
A3 = arrangement(hyps, R)    	    		    -- arrangement(List, Ring)
A3ring = typeA(3, ZZ)				    -- typeA(ZZ, Ring)
A3poly = typeA(3, R)				    -- typeA(ZZ, PolynomialRing)
A3mat = arrangement(matrix {{1, 1, 1, 0, 0, 0},	    -- arrangement(List, Matrix)
	             {-1, 0, 0, 1, 1, 0},
		     {0, -1, 0, -1, 0, 1},
		     {0, 0, -1, 0, -1, -1}}, R)
assert(A3 === A3poly)
assert(A3 === A3mat) 
assert(A3 === sub(A3ring, map(R, ring A3ring, R_*)))	    -- sub(Arrangement, RingMap)
assert(rank A3 == 3)					    -- rank Arrangement
assert(pdim EPY (A3**QQ) == 3)				    -- EPY Arrangement
assert(not isDecomposable A3)				    -- isDecomposable Arrangement
assert(matrix A3 === matrix {hyps})    	       	       	    -- matrix Arrangement
assert(matroid (A3**QQ) === matroid coefficients (A3**QQ))  -- matroid CentralArrangement
///

-----------------------------------------------------------
-- Tests making arrangements using strings.
-----------------------------------------------------------
TEST ///
X3 = arrangement "X3"					    -- arrangement String
assert(isDecomposable X3)				    -- isDecomposable Arrangement
assert(multiplierIdeal(2,X3) == multiplierIdeal(11/5,X3))		    -- multiplierIdeal(ZZ, CentralArrangement)
time I1 = orlikTerao(X3);				    -- orlikTerao CentralArrangement
time I2 = orlikTerao(X3,ring I1,NaiveAlgorithm=>true);	    -- orlikTerao(CentralArrangement, PolynomialRing, NaiveAlgorithm=>true)
assert(I1==I2)

M = arrangement "MacLane"				    -- arrangement String
P = poincare M						    -- poincare CentralArrangement
t = (ring P)_0
assert(1+8*t+20*t^2+13*t^3 == P)
///


------------------------------------
-- Testing `typeB`
------------------------------------
TEST ///
R = ZZ[x_1..x_3]
B3 = arrangement({x_1,x_1-x_2,x_1+x_2,x_1-x_3,x_1+x_3,x_2,x_2-x_3,x_2+x_3,x_3})
B3alt = typeB(3,ZZ)
assert(B3 === sub(B3alt, map(R, ring B3alt, R_*)))

B3alt = typeB(3, R)
assert(B3 === B3alt)

S = R**QQ
B3alt = typeB 3
assert(sub(B3,S) === sub(B3alt, map(S, ring B3alt, S_*)))

assert(rank B3 === 3)
///


------------------------------------
-- Testing `typeD`
------------------------------------
TEST ///
R = ZZ[x_1..x_3]
D3 = arrangement({x_1-x_2,x_1+x_2,x_1-x_3,x_1+x_3,x_2-x_3,x_2+x_3})
D3alt = typeD(3,ZZ)
assert(D3 === sub(D3alt, map(R, ring D3alt, R_*)))

D3alt = typeD(3, R)
assert(D3 === D3alt)

S = R**QQ
D3alt = typeD 3
assert(sub(D3,S) === sub(D3alt, map(S, ring D3alt, S_*)))

assert(rank D3 === 3)
///

---------------------------------------------------
-- Testing `flat`, `flats`, and various things about `Flat`
---------------------------------------------------
TEST ///
A3 = typeA 3

F = flat(A3, {0,1,3})
assert(try(flat(A3, {0,1}); false) else true)	 -- `Validate=>true`
assert(A3 === arrangement F)			 -- `arrangement Flat`
assert(toList F === {0,1,3})			 -- `toList Flat`
assert(net F === net {0,1,3})			 -- `net Flat`
assert(rank F === 2)				 -- `rank Flat`

A2 = typeA 2
assert(flats A2 === {flats(0, A2), flats(1, A2), flats(2, A2)}) -- flats with and without rank
assert(flats (0,A2) === {flat(A2, {})})				-- flat(ZZ, Arrangement)
assert(flats (2,A2) === {flat(A2, {0,1,2})})			-- flat(ZZ, Arrangement)

empty = arrangement({}, QQ[])				    -- essential empty arrangement
assert(flats empty === {flats(0, empty)})
assert(flats (0, empty) === {flat(empty, {})})

R = QQ[x,y]
affine = arrangement({x,x+1,y}, R)
assert(flats(2, affine) === {flat(affine, {0,2}), flat(affine, {1,2})})
-- Test `closure` and comparison of Flats (moved to documentation)
--F' = closure(A3, ideal (hyperplanes A3)_{0,1})		    -- closure(Arrangement, Ideal)
--assert(F == F')
--F' = closure(A3, {0,1})					    -- closure(Arrangement, List)
--assert(F == F')
///

---------------------------
-- circuits
---------------------------
TEST ///
A3 = typeA 3
assert(set \ circuits A3 === set \ {{0, 1, 3}, {4, 0, 2}, {1, 2, 3, 4}, {5, 1, 2}, {0, 2, 3, 5}, {0, 1, 4, 5}, {4, 5, 3}})
///

---------------------------
-- coefficients
---------------------------
TEST ///
mat = matrix{{1,2,3,4},{5,6,7,8},{9,10,11,12}}
A = arrangement mat
assert(coefficients A === mat)

R = QQ[x,y,z]
A = arrangement({}, R)
assert(coefficients A === map(QQ^3, QQ^0, 0))		    -- empty arrangement

R = QQ[]
A = arrangement({0_R}, R)
assert(coefficients A === map(QQ^0, QQ^1, 0))		    -- loop
///


---------------------------
-- compress and trim
---------------------------
TEST ///
R = QQ[]
A = arrangement {0_R}
assert(compress A === arrangement ({}, R))

R = QQ[x]
A = arrangement {-x, -x}
assert(trim A === arrangement{x})
assert(compress A === A)

A = arrangement {0_R,-x,x}
assert(trim A === arrangement{x})
assert(compress A === arrangement{-x,x})
///


---------------------------
-- cone and deCone
---------------------------
TEST ///
R = QQ[x,h]
A = arrangement {x,x-1}
cA = arrangement{x,x-h,h}
assert(cone (A, h) === cA)		    -- cone(Arrangement, RingElement)

A' = deCone(cA, h)
dcA = sub(A', map(R, ring A', {x,1}))
assert(A === dcA)			    -- deCone(CentralArrangement, RingElement)

A' = deCone(cA, 2)
dcA = sub(A', map(R, ring A', {x,1}))
assert(A === dcA)		      	    -- deCone(CentralArrangement, ZZ)

R = QQ[y]
A = arrangement {y, y-1}
A' = cone (A, getSymbol "h")
R' = ring A'
assert(A' === arrangement {R'_"y", R'_"y"-R'_"h", R'_"h"})  -- cone(Arrangement, Symbol)

R = QQ[]
A = arrangement({0_R}, R)
A' = cone(A, getSymbol "h")
R' = ring A'
assert (A' === arrangement{0_R', R'_0})			    -- cone of a loop
///


---------------------------
-- deletion
---------------------------
TEST ///
R = QQ[x,y]
A = arrangement {x,x,y,x-y}
assert(deletion(A, x) == arrangement {x,y,x-y})		    -- deletion for multiarrangement
assert(deletion(A,2) == arrangement {x,x,x-y})		    -- deletion(Arrangement, ZZ)
assert(deletion(A,{0,2}) == arrangement{x,x-y})		    -- deletion(Arrangement, List)
assert(deletion(A,{0,0}) == deletion(A, {0}))		    -- deletion(Arrangement, List) with doubles
assert(deletion(A,set{0,2}) == arrangement{x,x-y})	    -- deletion(Arrangement, Set)
A = arrangement {x,-x,y,x-y}
assert(deletion(A,x) == deletion(A,-x))
assert(deletion(A,x) == arrangement {-x,y,x-y})
///

---------------------------
-- der
---------------------------
TEST ///
A = typeA(3)
assert((prune image der A) == (ring A)^{-1,-2,-3})  -- free module of derivations?
assert((prune image der(A, {2,2,2,2,2,2})) == (ring A)^{-4,-4,-4})
///

---------------------------
-- dual
---------------------------
TEST ///
R = QQ[x,y]
A = arrangement {x,y,x-y}
Rdual = QQ[z]
assert(dual (A, Rdual) === arrangement{-z, z, z})	    -- dual(CentralArrangement, Ring)

Adual = dual A
Rdual = ring Adual
assert(Adual === arrangement{-Rdual_0, Rdual_0, Rdual_0})   -- dual CentralArrangement

A = arrangement ({}, QQ[])
assert(try(dual A; false) else true)	 -- empty arrangement gives an error

R = QQ[x]
R' = QQ[]
coloop = arrangement ({x}, R)
loop = arrangement ({0}, R')
assert(dual(loop, R) === coloop)			    -- dual of a loop
assert(dual(coloop, R') === loop)			    -- dual of a coloop
///

---------------------------
-- euler
---------------------------
-- In documentation

---------------------------
-- eulerRestriction
---------------------------

-- in documentation

---------------------------
-- genericArrangement
---------------------------
TEST ///
arr = genericArrangement(3,5)
arrK = genericArrangement(3,5,QQ)

changeVars = map(ring arr, ring arrK, gens ring arr)
assert(arr === sub(arrK, changeVars))	    		    -- genericArrangement w/ and w/o K

assert(coefficients arr === matrix(QQ, {{1,1,1,1,1},	    -- coefficients
	                                {1,2,3,4,5},
					{1,4,9,16,25}}))

assert(circuits arr === {{0,1,2,3},{0,1,2,4},{0,1,3,4},	    -- circuits
	                 {0,2,3,4},{1,2,3,4}})
///

-------------
-- graphic
-------------
TEST ///
A3 = typeA 3
arr = graphic ({{2,1},{3,1},{4,1},{3,2},{4,2},{4,3}}, ring A3)
assert(A3 === arr)

R = ring A3
arr' = graphic ({{R_1,R_0},{R_2,R_0},{R_3,R_0},{R_2,R_1},{R_3,R_1},{R_3,R_2}}, gens R, ring A3)
assert(A3 === arr')
///


------------------
-- lct
------------------

TEST ///
R = QQ[x,y,z]
A = deletion(typeB(3), {0,1})
assert(3/7 == lct A) -- Berkesch and Leykin
///


------------------
-- orlikSolomon
------------------
TEST ///
e = symbol e
osDefault = orlikSolomon(typeA 3, e)
E = ring osDefault
osMatroids = orlikSolomon(typeA 3, E, Strategy=>Matroids)
osPopescu = orlikSolomon(typeA 3, E, Strategy=>Popescu)
assert(osDefault === osMatroids)
assert(osMatroids === osPopescu)
///

end---------------------------------------------------------------------------     

------------------------------------------------------------------------------
-- SCRATCH SPACE
------------------------------------------------------------------------------

--A3' = arrangement {x,y,z,x-y,x-z,y-z}
--A3' == A3
--product A3
--A3.hyperplanes

--NF = arrangement {x,y,z,x-y,x-z,y-z,x+y-z}
--///

end

    

path = append(path, homeDirectory | "exp/hyppack/")
installPackage("HyperplaneArrangements",RemakeAllDocumentation=>true,DebuggingMode => true)
loadPackage "HyperplaneArrangements"

-- uninstallPackage "SimplicialComplexes"
uninstallPackage "HyperplaneArrangements"
restart
installPackage "HyperplaneArrangements"
check HyperplaneArrangements
needsPackage "HyperplaneArrangements"
