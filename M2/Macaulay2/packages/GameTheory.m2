newPackage(
   "GameTheory",
   Version => "1.0",
   Date => "May, 2025",
   Authors => {
      {Name => "Erin Connelly",
         Email => "erin.connelly@uni-osnabrueck.de",
         HomePage => "https://erinconnelly96.github.io/"},
      {Name => "Vincenzo Galgano",
         Email => "galgano@mpi-cbg.de",
         HomePage => "https://sites.google.com/view/vincenzogalgano"},
      {Name => "Zhuang He",
         Email => "zhuang.he@unito.it",
         HomePage => "https://sites.google.com/view/hezhuang/home"},
      {Name => "Lars Kastner",
         Email => "kastner@math.tu-berlin.de",
         HomePage => "https://lkastner.github.io"},
      {Name => "Giacomo Maletto",
         Email => "gmaletto@kth.se",
         HomePage => "https://www.kth.se/profile/gmaletto"},
      {Name => "Elke Neuhaus",
         Email => "elke@neuhaus-rp.de",
         HomePage => "https://sites.google.com/view/elkeneuhaus"},
      {Name => "Irem Portakal",
         Email => "mail@irem-portakal.de",
         HomePage => "https://www.irem-portakal.de"},
      {Name => "Hannah Tillmann-Morris",
         Email => "tillmann@mis.mpg.de",
         HomePage => "https://sites.google.com/view/hannah-tillmann-morris"},
      {Name => "Chenyang Zhao",
         Email => "cz2922@ic.ac.uk",
         HomePage => "https://www.felixzhao.com/"}
   },
   Headline => "computing equilibria in game theory",
   Keywords => {"Game Theory"},
   PackageExports => {"Polyhedra","GraphicalModels"},
   PackageImports => {"Polyhedra"}
   )

export {
   "enumerateTensorIndices",
   "Tensor",
   "zeroTensor",
   "indexset",
   "randomTensor",
   "slice",
   "getVariableToIndexset",
   "assemblePolynomial",
   "assemblePlayeriPolynomials",
   "correlatedEquilibria",
   "nashEquilibriumRing",
   "nashEquilibriumIdeal",  
   "deltaList",
   "blockDerangements",
   "numberTMNE",
   "probabilityRing",
   "probabilitySumIdeal",
   "ProbabilityVariableName",
   "KonstanzVariableName",
   "randomGame",
   "spohnMatrices",
   "spohnIdeal",
   "konstanzMatrix",
   "toMarkovRing",
   "mapToMarkovRing",
   "mapToProbabilityRing",
   "ciIdeal",
   "intersectWithCImodel",
   "spohnCI"
}


--***************************************--
--   METHODS FOR CORRELATED EQUILIBRIA   --
--***************************************--

---------------------------------------------------
-- enumerateTensorIndices ZZ
-- enumerateTensorIndices List
--
-- Returns the list of index tuples for a tensor 
-- with the given dimensions.
--
-- Note: Indices start at 0 and go up to d_i-1,
-- where d_i is the i-th element of the input list.
---------------------------------------------------

enumerateTensorIndices = method()
enumerateTensorIndices ZZ := z -> apply(toList (0..z-1), e->{e})
enumerateTensorIndices List := s -> (
   if length s == 1 then 
      return enumerateTensorIndices s#0
   else
      start := enumerateTensorIndices s#0;
      ri := toList (1..(length(s)-1));
      rest := enumerateTensorIndices s_ri;
      result := {};
      for s in start do
         for r in rest do
            result = append(result, join(s,r));
      result
      )

---------------------------------------------------------
-- Defines a new type "Tensor" based on MutableHashTable.
---------------------------------------------------------

Tensor = new Type of MutableHashTable

-------------------------------------------------------------------
-- zeroTensor (Ring, List)
--
-- The method creates a zero tensor with the given format and ring.
-------------------------------------------------------------------

zeroTensor = method()
zeroTensor List := dims -> zeroTensor(QQ,dims)
zeroTensor(Ring,List) := (R,dims) -> (
   result := new Tensor;
   indexset := enumerateTensorIndices dims;
   for i in indexset do
      result#i = 0_R;
   result#"format" = dims;
   result#"coefficients" = R;
   result#"indexes" = indexset;
   result
   )

---------------------------------------------------------------------
-- randomTensor (Ring, List)
--
-- The method creates a random tensor with the given format and ring.
---------------------------------------------------------------------

randomTensor = method()
randomTensor List := dims -> randomTensor(QQ,dims)
randomTensor(Ring,List) := (R,dims) -> (
   result := new Tensor;
   indexset := enumerateTensorIndices dims;
   for i in indexset do
      result#i = random R;
   result#"format" = dims;
   result#"coefficients" = R;
   result#"indexes" = indexset;
   result
   )

----------------------------------------------
-- format Tensor
--
-- It prints the format of the defined tensor.
----------------------------------------------

format Tensor := T -> T#"format"
coefficientRing Tensor := T -> T#"coefficients"

---------------------------------------
-- indexset Tensor
--
-- It prints the indices of the tensor.
---------------------------------------

indexset = method()
indexset Tensor := T -> T#"indexes"

-----------------------------------------------------
-- slice (Tensor, List, List)
--
-- The first list (Lstart) specifies the fixed indices 
-- before the varying position, and the second list 
-- (Lend) specifies the fixed indices after it.
--
-- The method varies the index at the position 
-- given by the length of Lstart, from 0 to d_i-1, 
-- where d_i is the corresponding dimension size.
-----------------------------------------------------

slice = method()
slice (Tensor, List, List) := (T, Lstart, Lend) -> (
   dims := format T;
   iteratingPosition := length Lstart;
   result := {};
   for i from 0 to dims#iteratingPosition -1 do (
      mindex := join(Lstart, {i}, Lend);
      result = append(result, T#mindex);
   );
   result
   )

---------------------------------------------------
-- getVariableToIndexset(Ring, List)
--
-- Given a ring R and a list ki representing the
-- indices,returns the corresponding generator.
---------------------------------------------------

getVariableToIndexset = method()
getVariableToIndexset(Ring, List) := (R, ki) -> (
   p := position(apply(gens R, i -> last baseName i), i -> i == ki);
   R_p
   )

--------------------------------------------------------
-- assemblePolynomial(Ring, Tensor, List)
--
-- Constructs a single linear inequality (polynomial)
-- representing a deviation condition for correlated 
-- equilibrium constraints.
--
-- Inputs:
--   - PR: Polynomial ring containing variables p_{...}
--   - Xi: Tensor of strategy probabilities
--   - ikl: A list {i, k, l} where:
--        i = player index
--        k = current strategy
--        l = deviating strategy
-------------------------------------------------------

assemblePolynomial = method()
assemblePolynomial(Ring, Tensor, List) := (PR, Xi, ikl) -> (
   FBi := indexset Xi;
   reverseVarMap := new MutableHashTable;
   for k in FBi do (
      reverseVarMap#k = getVariableToIndexset(PR, k);
   );
   i := ikl#0;
   k := ikl#1;
   l := ikl#2;
   use PR;
   kindices := select(FBi, e->e#i==k);
   lindices := select(FBi, e->e#i==l);
   kterm := sum apply(kindices, ki -> Xi#ki*reverseVarMap#ki);
   lterm := sum apply(lindices , li->(tmp := new MutableList from li; tmp#i=k; a := toList tmp; Xi#li*reverseVarMap#a));
   ineq := kterm-lterm;
   ineq
   )

------------------------------------------------
-- assemblePlayeriPolynomials(Ring, Tensor, ZZ)
--
-- Returns a list of all polynomials for a given
-- player i in a correlated equilibrium.
------------------------------------------------

assemblePlayeriPolynomials = method()
assemblePlayeriPolynomials(Ring, Tensor, ZZ) := (PR, Xi, i) -> (
   result := {};
   di := (format Xi)#i;
   for k from 0 to di-1 do (
      for l from 0 to di-1 do (
         poly := assemblePolynomial(PR, Xi, {i,k,l});
         result = append(result, poly);
      );
   );
   result
   )

-------------------------------------------------------------
-- correlatedEquilibria(List)
--
-- Inputs:
--   - X: A list of tensors, one for each player's payoff

-- Assembles all incentive constraint polynomials and returns
-- a polytope by adding the probability constraints.
-------------------------------------------------------------

correlatedEquilibria = method()
correlatedEquilibria List := X -> (
   F := coefficientRing (X#0);
   FBi := indexset (X#0);
   p := getSymbol "p";
   PR := F[apply(FBi, fb->p_fb)];
   nplayers := length format X#0;
   L := flatten for i from 0 to nplayers-1 list assemblePlayeriPolynomials(PR, X#i, i);
   polyDim := length FBi;
   vectors := {};
   ineqs := for p in L list apply(generators PR, g -> coefficient(g, p));
   ineqs = (matrix ineqs) || (map identity (F^(#FBi)));
   ineqsrhs := transpose matrix {toList ((numRows ineqs):0_F)};
   eq := matrix {toList ((numColumns ineqs):1_F)};
   eqrhs := matrix {{1_F}};
   polyhedronFromHData(-ineqs, ineqsrhs,eq,eqrhs)
   )


--***************************************--
--     METHODS FOR NASH EQUILIBRIA       --
--***************************************--

-----------------------------------------------------------
-- mixedProbabilityRing (List)
-- mixedProbabilityRing (Tensor)
--
-- Returns the polynomial ring generated by the probability
-- variables p_{i,j}, from a single 
-- payoff tensor, or a list L of the format of the tensor. 
-----------------------------------------------------------

mixedProbabilityRing = method()
mixedProbabilityRing List := L ->(
    p := getSymbol "p";
    probabilityRing := QQ[flatten apply(#L, i -> apply(L#i, j->p_{i,j}))];
    probabilityRing
    )
mixedProbabilityRing Tensor := T ->(
    indexSet := format T;
    mixedProbabilityRing indexSet
    )

-------------------------------------------------------------
-- differencesFromFirst (List)
--
-- Returns a list of length 1 less than the input list, 
-- consisting of the differences between every other entries 
-- with the first entry.
-------------------------------------------------------------

differencesFromFirst = L -> (apply(toList(1..#L-1), i->L#i-L#0))

---------------------------------------------------------------------------------
-- monomialFromIndex (List, ZZ, Ring)
--
-- Given a list of n-1 strategies (j_0, j_1, ..., j_{i-1}, j_{i+1}, ..., j_{n-1}) 
-- for each player except for the i-th, and the integer i, 
-- this function returns the product of p_{k,j_k}, 0<=k<=n-1, k!=i, 
-- as a monomial in the probability ring R.
---------------------------------------------------------------------------------

monomialFromIndex = method()
monomialFromIndex (List, ZZ, Ring):= (L, i, R) ->(
    p := getSymbol "p";
    monomial := product toList apply(pairs L, (j,r)->(s := if j >= i then j + 1 else j; p_{s,r}_R));
    monomial
    )

------------------------------------------------------------------------------------------------
-- equilibriumPolynomials (Tensor, ZZ, Ring)
--
-- Input: T is the payoff tensor of the u-th player.
-- Output: a list of (d_u - 1) polynomials in the probability ring R,
-- consisting of the equilibrium conditions from the u-th player (zero-based).
-- 
-- This function constructs a tensor 'accumulatedHash', of format 
-- (d_0, d_1, ... d_{u-1}, d_{u+1}, ..., d_{n-1}), implemented as a MutableHashTable. Each entry 
-- of 'accumulatedHash' indexed by (j_0, j_1, ..., j_{u-1}, j_{u+1}, ..., j_{n-1}) is a list of 
-- length d_u, whose k-th term is T#(j_0, j_1, ..., j_{u-1}, k, j_{u+1}, ..., j_{n-1}).
--
-- After the tensor is fully filled, for each entry of it, the function 
-- computes the 'differencesFromFirst' of this entry, which gives a list of length d_u - 1, 
-- and then multiplies each term of this list with the same monomial, the product of p_{k,j_k}, 
-- for 0<=k<=n-1, k!=u, obtaining a list of d_u - 1 monomials.

-- The function returns the sum of all these lists of d_u - 1 monomials, as a list 
-- of d_u - 1 polynomoials, which are the polynomials of equilibrium conditions 
-- from the payoff tensor T of player u.

-- Note: accumulatedHash is not implemented with 'zeroTensor' to avoid pre-filling 
-- zeros, allowing a better performance.
-------------------------------------------------------------------------------------------------

equilibriumPolynomials = method()
equilibriumPolynomials (Tensor, ZZ, Ring) := (T, u, R)->(
    indexSet := format T;
    tensorIndices := T#"indexes";
    nStrategies := indexSet#u;
    accumulatedHash := new MutableHashTable;
    for tensorIndex in tensorIndices do (
        newCoefficient := T#tensorIndex;
        thisStrategy := tensorIndex#u;
        monomialIndex := drop(tensorIndex,{u,u});
        if not (accumulatedHash #? monomialIndex) then accumulatedHash#monomialIndex = new MutableList from nStrategies: 0;
        accumulatedHash#monomialIndex#thisStrategy = newCoefficient;
	);
    polynomials := apply(pairs accumulatedHash, (k,v)->(
        monomial := monomialFromIndex(k, u, R);
        apply(differencesFromFirst v, i-> i_R * monomial)
	)
    );
    sum polynomials
    )

-----------------------------------------------------------
-- nashEquilibriumRing (List)
--
-- Returns the polynomial ring of the probability variables
-- from a list of payoff tensors. This function is a wrapper 
-- for the internal function mixedProbabilityRing.
-- L is a list consisting of n tensors; requires them to be
-- of the same dimension/shape.
--
-- Caveat: it only looks at the format of the first tensor
-- in the list.
-----------------------------------------------------------

nashEquilibriumRing = method()
nashEquilibriumRing List := L -> (
    indexSet := format first L;
    polyRing := mixedProbabilityRing indexSet;
    polyRing
    )

----------------------------------------------------------------
-- nashEquilibriumIdeal (Ring, List)
--
-- Returns the ideal of totally mixed Nash equilibria, 
-- as an ideal in the probability ring R.
--
-- The input L must be a list of payoff tensors of
-- the same format. The input R should be a probability ring and
-- can be generated e.g. with nashEquilibriumRing.
----------------------------------------------------------------

nashEquilibriumIdeal = method()
nashEquilibriumIdeal (Ring, List) := (R, L) -> (
    indexSet := format first L;
    probabilityRing := R;
    completeGeneratingSet := flatten(apply(pairs L, (i,T) -> equilibriumPolynomials(T,i,probabilityRing)));
    p := getSymbol "p";
    linearRelations := apply(pairs indexSet, (i,j)-> sum(j, k->p_{i,k}_probabilityRing) - 1);
    fullGeneratingSet := join(completeGeneratingSet, linearRelations);
    ideal fullGeneratingSet
    )

---------------------------------------------------------------
-- directProductList (List)
--
-- Given a list of polytopes, it computes their direct product.
---------------------------------------------------------------

directProductList = method()
directProductList List := L -> (
    if #L == 0 then error "Empty list of polytopes";
    P := L#0;
    for i from 1 to (#L - 1) do (
        P = directProduct(P, L#i);
	);
    P
    )

------------------------------------------------------------------
-- deltaList (Ring, List)
--
-- Generates a list of delta polytopes that is used to calculate 
-- the mixed volume (the max number of Nash Equilibrium 
-- of the system).

-- These polytopes are the Newton polytopes of the polynomials
-- that are generated via nashEquilibriumIdeal.

-- Note: One can use the existing mixedVolume method from Polyhedra
-- package however our method numberTMNE performs much faster for
-- larger examples.
------------------------------------------------------------------

deltaList = method()
deltaList List := d -> (
    n := #d;
    result := {};
    for i from 0 to (n - 1) do (
        polyFactors := for j from 0 to (n - 1) list (
            if j == i then (
                convexHull(matrix(apply(d#i - 1, k -> {0})))
            ) else (
                simplex(d#j - 1)
            )
        );
        P := directProductList(polyFactors);
        for rep from 1 to (d#i - 1) do (
            result = append(result, P)
	    );
	);
    result
    )

--------------------------------------------------------------------------
-- blockDerangements(List)
--
-- Given a format of a tensor, it computes all block derangements
-- A = (A_1,...,A_n) with respect to the sets
-- F_i = {(i,j) | j \in [d_i-1]} and their union F.

-- Note: The number of all block derangements for a given format
-- coincides with the output of numberTMNE.

-- Reference: The Maximal Number of Regular Totally Mixed Nash Equilibria
-- by R. D. McKelvey, A. McLennan, Journal of Economic Theory, Vol. 72,
-- Issue 2, February 1997, Pages 411-425. 
--------------------------------------------------------------------------

blockDerangements = method()
blockDerangements List := D -> (
    F := apply(#D, i -> apply(D#i - 1, j -> (i, j)));  
    PP := permutations flatten F;                    
    partsPP := toList set apply(PP, p -> 
        apply(#D, i -> set apply(D#i - 1, j -> p#(j + sum(i, k -> D#k - 1)))));
    BD := {};                                         
    for p in partsPP do 
        if toList set flatten apply(#D, i -> apply(toList(p#i), j -> j#0 != i)) == {true} 
        then BD = append(BD, p);                     
    return BD
    )

--------------------------------------------------------------------------------
-- numberTMNE(List)
--
-- Given a format of a tensor, it computes the number of
-- totally mixed Nash equilibria of a generic game.
--
-- This number equals the coefficient of h_1^(d_1-1)*...*h_n^(d_n-1)
-- in the expansion of
-- (h_2+...+h_n)^(d_1-1)*(h_1+h_3...+h_n)^(d_2-1)*...*(h_1+...+h_(n-1))^(d_n-1)
-- which speeds up the usual mixed volume calculation.

-- Reference: A vector bundle approach to Nash Equilibria by H. Abo, I. Portakal
-- L. Sodomaco, arXiv:2504.03456, Theorem 2.7.
--------------------------------------------------------------------------------

numberTMNE = method()
numberTMNE List := D -> (
    KK := ZZ;
    h := local h;
    R := KK[h_0..h_(#D-1)];
    return sub(contract(product(#D, j -> h_(j)^(D#j-1)),
                        product(#D, j -> (sum(#D, i -> h_(i))-h_(j))^(D#j-1))), KK)
		)


--***************************************--
--   METHODS FOR DEPENDENCY EQUILIBRIA   --
--***************************************--

--------------------------------------------------
-- probabilityRing (List)
--
-- Given a list Di this constructs a ring of joint 
-- probabilities for a game of format Di
--------------------------------------------------

probabilityRing = method(Options => { CoefficientRing => QQ, ProbabilityVariableName => "p" })
probabilityRing List := Ring => opts -> Di -> (
    J := enumerateTensorIndices Di;
    p := getSymbol opts.ProbabilityVariableName;
    K := opts.CoefficientRing;
    R := K[apply(J, j -> p_j)];

    P := zeroTensor(R, Di);
    for j in J do P#j = (p_j)_R;
    R#"probabilityVariable" = P;

    R#"gameFormat" = Di;
    R
    )

-------------------------------------------------
-- probabilitySumIdeal (Ring)
--
-- Given a ring, it constructs a principal ideal
-- generated by the sum of the generators of the
-- ring minus one. 
-------------------------------------------------

probabilitySumIdeal = method()
probabilitySumIdeal Ring := Ideal => R -> (
    vars := gens R;
    summing := sum(vars, x->x);
    return ideal(summing - 1);
    )

------------------------------------------------
-- randomGame (List)
--
-- Given a list Di this constructs a random game 
-- of format Di
------------------------------------------------

randomGame = method(Options => {CoefficientRing => QQ})
randomGame List := List => opts -> Di -> (
    K := opts.CoefficientRing;
    apply(length Di, i -> randomTensor(K, Di)
	)
    )

--------------------------------------------------------
-- spohnMatrices (Ring, List)
--
-- Given the underlying probability ring R and a game X 
-- this constructs the Spohn matrices for X
--------------------------------------------------------

spohnMatrices = method()
spohnMatrices (Ring, List) := List => (PR, X) -> (
    p := PR#"probabilityVariable";
    n := length X;
    d := format X_0;
    J := indexset X_0;
    apply(n, i -> matrix apply(d_i, k -> {sum(select(J, j -> j_i==k), j -> p#j),
                                          sum(select(J, j -> j_i==k), j -> (X_i)#j * p#j) }
				      )
				  )
			      )

--------------------------------------------------------
-- spohnIdeal (Ring, List)
--
-- Given the underlying probability ring R and a game X 
-- this constructs the Spohn ideal for X
-------------------------------------------------------- 
			  
spohnIdeal = method()
spohnIdeal (Ring, List) := List => (PR, X) -> (
    M := spohnMatrices(PR, X);
    sum(M, m -> minors(2, m)
	)
    )

--------------------------------------------------------
-- konstanzMatrix (Ring, List)
--
-- Given the underlying probability ring R and a game X 
-- this constructs the Konstanz Matrix for X
--------------------------------------------------------

konstanzMatrix = method(Options=>{ KonstanzVariableName => "k" })
konstanzMatrix (Ring, List) := Matrix => opts -> (PR, X) -> (
    k := getSymbol opts.KonstanzVariableName;
    Di := PR#"gameFormat";
    p := PR#"probabilityVariable";
    n := #Di;
    J := enumerateTensorIndices Di;
    konstanzRing := PR[apply(n, i -> k_i)];
    M := spohnMatrices(PR, X);
    LinearForms := apply(n, i -> (M_i * matrix{{(k_i)_konstanzRing}, {-1}})
	);
    P := vector(apply(J, j -> p#j));
    fold((M0, M1) -> M0 || M1, 
         apply(n, i -> transpose matrix apply(Di_i,
                                              j -> diff(P, (LinearForms_i)_(j, 0))
					      )
					  )
				      )
				  )



--*****************************************************--
--   METHODS FOR CONDITIONAL INDEPENDENCE EQUILIBRIA   --
--*****************************************************--

----------------------------------
-- toMarkovRing Ring
-- input must be a probabilityRing
----------------------------------

toMarkovRing=method()
toMarkovRing Ring := R -> (
    if not R#?"gameFormat" then error "expected a ring created with probabilityRing";
    d:= R#"gameFormat";
    kk := coefficientRing(R);
    variableName := substring ( 0, 1, toString (gens(R))#0 );
    if variableName == "p" then (
	markovRing(toSequence(d), Coefficients=>kk, VariableName=>"q")
	)
    else (
	markovRing(toSequence(d), Coefficients=>kk)
	)
    )

--------------------------------------------------------------------
-- mapToMarkovRing Ring
-- mapToProbabilityRing Ring
-- inputs to both methods must be rings created with probabilityRing
--------------------------------------------------------------------

mapToMarkovRing=method()
mapToMarkovRing Ring := R -> (
    markovR := toMarkovRing(R);
    F := map(markovR, R, gens(markovR));
    F
    )

mapToProbabilityRing=method()
mapToProbabilityRing Ring := R -> (
    markovR := toMarkovRing(R);
    F := map(R, markovR, gens(R));
    F
    )

----------------------------------------------------------------
-- ciIdeal (PR, Stmts, PlayerNames)
-- ciIdeal (PR, Stmts)
-- ciIdeal (PR, G, PlayerNames)
-- ciIdeal (PR, G)
-- gives conditional independence ideal associated to a graogh G
-- or a set of conditional independence statements Stmts
-- as an ideal of the given probabilityRing
-----------------------------------------------------------------

ciIdeal = method()
ciIdeal (Ring, List, List) := (PR, Stmts, PlayerNames) -> (
    markovR := toMarkovRing(PR);
    phi := mapToProbabilityRing(PR);
    I := conditionalIndependenceIdeal ( markovR, Stmts, PlayerNames );
    phi I
    )
ciIdeal (Ring, List) := (PR, Stmts) -> (
    markovR := toMarkovRing(PR);
    phi := mapToProbabilityRing(PR);
    I := conditionalIndependenceIdeal ( markovR, Stmts );
    phi I
    )
ciIdeal (Ring, Graph, List) := (PR, G, PlayerNames) -> (
    Stmts := globalMarkov G;
    ciIdeal (PR, Stmts, PlayerNames)
    )
ciIdeal (Ring, Graph) := (PR, G) -> (
    Stmts := globalMarkov G;
    ciIdeal (PR, Stmts)
    )

-----------------------------------------------
-- intersectWithCImodel (V, Stmts, PlayerNames)
-- intersectWithCImodel (V, Stmts)
-- intersectWithCImodel (V, G, PlayerNames)
-- intersectWithCImodel (V, G)
-----------------------------------------------

intersectWithCImodel = method(Options => {Verbose => false})
intersectWithCImodel (Ideal, List, List) := o -> (V, Stmts, PlayerNames) -> (
    v := o.Verbose;
    R := ring V;
    H := map (R,ZZ);
    I := ciIdeal (R, Stmts, PlayerNames);
    if I + V == H(ideal(1)) then (
	result := H(ideal(1));
	result
	);
    for k from 0 to length(R_*)-1 do (
	I = saturate(I,R_k,Strategy=>Bayer);
	if v then print ("Completed step " | k+1 | " of saturating CI ideal");
	V = saturate(V,R_k,Strategy=>Bayer);
	if v then print ("Completed step " |k+1| " of saturating input ideal");
	);
    I = saturate(I,sum(R_*),Strategy=>Bayer);
    if v then print ("Completed step " |length(R_*)+1| " of saturating CI ideal");
    V = saturate(V, sum(R_*), Strategy=>Bayer);
    if v then print ("Completed step " |length(R_*) +1|" of saturating input ideal");
    J := I+V;
    for k from 0 to length(R_*)-1 do (
	J = saturate(J,R_k,Strategy=>Bayer);
	if v then print ("Completed step "|k+1|" of saturating sum");
	);
    J = saturate(J,sum(R_*),Strategy=>Bayer);
    result = J;
    result
    )
intersectWithCImodel (Ideal, List) := o -> (V, Stmts) -> (
    v := o.Verbose;
    d := (ring V)#"gameFormat";
    PlayerNames := toList (1..#d);
    intersectWithCImodel (V, Stmts, PlayerNames, Verbose=>v)
    )
intersectWithCImodel (Ideal, Graph, List) := o -> (V, G, PlayerNames) -> (
    v := o.Verbose;
    Stmts := globalMarkov G;
    intersectWithCImodel (V, Stmts, PlayerNames, Verbose=>v)
    )
intersectWithCImodel (Ideal, Graph) := o -> (V, G) -> (
    v := o.Verbose;
    d := (ring V)#"gameFormat";
    PlayerNames := toList (1..#d);
    intersectWithCImodel (V, G, PlayerNames, Verbose=>v)
    )

--------------------------------------
-- spohnCI (PR, X, G)
-- spohnCI (PR, X, G, PlayerNames)
-- spohnCI (PR, X, Stmts)
-- spohnCI (PR, X, Stmts, PlayerNames)
--------------------------------------

spohnCI = method(Options => {Verbose => false})
spohnCI (Ring, List, Graph) := o -> (PR, X, G) -> (
    v := o.Verbose;
    spohn := spohnIdeal(PR, X);
    intersectWithCImodel(spohn, G, Verbose => v)
    )
spohnCI (Ring, List, Graph, List) := o -> (PR, X, G, PlayerNames) -> (
    v := o.Verbose;
    spohn := spohnIdeal(PR, X);
    intersectWithCImodel(spohn, G, PlayerNames, Verbose => v)
    )
spohnCI (Ring, List, List) := o -> (PR, X, Stmts) -> (
    v := o.Verbose;
    spohn := spohnIdeal(PR, X);
    intersectWithCImodel(spohn, Stmts, Verbose => v)
    )
spohnCI (Ring, List, List, List) := o -> (PR, X, Stmts, PlayerNames) -> (
    v := o.Verbose;
    spohn := spohnIdeal(PR, X);
    intersectWithCImodel(spohn, Stmts, PlayerNames, Verbose => v)
    )


--********************************--
--         DOCUMENTATION          -- 
--********************************--

beginDocumentation()

doc ///
  Key
    GameTheory
  Headline
    a package for computing equilibria in game theory
  Description
    Text
     {\bf Game Theory} is a package for several equilibrium concepts in game theory. It constructs the algebro-geometric and
     combinatorial models for Nash, correlated, dependency, and conditional independence equilibria. The latter three notions of
     equilibria are all generalizations of Nash equilibria. An $n$-player game in normal form is defined by $n$-tensors of format
     $d_1 \times d_2 \times \cdots \times d_n$, where $d_i$ is the number of pure strategies of player $i$.
     The entry $\{j_1, j_2, \cdots, j_n\}$ of the $i$-th (payoff) tensor for player $i$ is the payoff when player $1$ chooses
     strategy $j_1$, player $2$ chooses strategy $j_2$, and so on. One can define a specific game or a random game, e.g.,
     a list with random tensors.
    Example
     -- Bach or Stravinsky game
     A = zeroTensor {2,2};
     B = zeroTensor {2,2};
     A#{0,0} = 3;  A#{0,1} = 0;  A#{1,0} = 0;  A#{1,1} = 2;
     B#{0,0} = 2;  B#{0,1} = 0;  B#{1,0} = 0;  B#{1,1} = 3;
     
     -- A random 3-player game
     X = {randomTensor {2,2,2}, randomTensor {2,2,2}, randomTensor {2,2,2}}
     randomGame {2,2,2}
    Text
     The notion of Nash equilibria is one of the central topics in game theory. 
     @TO nashEquilibriumIdeal@ computes a square system of $d_1 + \cdots + d_n$ polynomials that algebraically model the set of totally mixed Nash equilibria.
     In the case where the dimension of this ideal is zero (commonly referred to as a generic game), one can use the mixed volume of the Newton polytopes 
     of each polynomial in the system to obtain an upper bound on the number of totally mixed Nash equilibria. The list of these Newton polytopes is 
     provided by @TO deltaList@. The mixed volume of these polytopes equals the number of certain block derangements. The method @TO numberTMNE@ 
     is typically faster than @TO mixedVolume@ in this case.
    Example
     NR = nashEquilibriumRing X;
     I = nashEquilibriumIdeal(NR,X)
     dim I
     degree I
     -- For a generic game
     D = deltaList {2,2,2}
     mixedVolume D
     blockDerangements {2,2,2}
     numberTMNE {2,2,2}
    Text
     The set of correlated equilibria of a game forms a convex polytope inside the (probability) simplex which is the standard simplex of dimension $d_1 \cdots d_n -1$.
     Thus, the variables are taken from @TO probabilityRing@. In particular, the map from @TO nashEquilibriumRing@ to @TO probabilityRing@ is the Segre embedding.
    Example
     -- A full dimensional polytope which is a triangular bipyramid
     CE1 = correlatedEquilibria {A, B}
     dim CE1
     vertices CE1
     facets CE1
     -- The correlated equilibrium polytope for a random game
     CE2 = correlatedEquilibria randomGame{2,2,2}
     dim CE2
    Text
     The algebro-geometric model of dependency equilibria is called Spohn variety. Its defining ideal is given by rank one conditions on Spohn matrices.
     One can also define Konstanz matrices which is crucial to understand the projection of dependency equilibria to the payoff region.
    Example
      PR = probabilityRing {2,2,2};
      X = randomGame {2,2,2};
      spohnMatrices(PR,X)
      spohnIdeal(PR,X);
      konstanzMatrix(PR,X)
    Text
     The algebro-geometric model of conditional independence equilibria is obtained by intersecting the Spohn variety with the conditional independence model of
     a given set of conditional independence statements, followed by the removal of certain components. The set of conditional independence statements can be generated
     via graphical models. 
    Example
      G1 = graph ({}, Singletons => {1,2,3});
      G2 = graph ({{1,2}}, Singletons => {3});
      I1 = spohnCI(PR,X,G1)
      I2 = spohnCI(PR,X,G2)
      -- One can also add the linear constraint coming from the probabilities.
      -- For generic games, the ideal J1 models totally mixed Nash equilibria.
      J = probabilitySumIdeal(PR)
      J1 = I1 + J
      J2 = I1 + J
  References
    This package is based on the following papers:
         
      - Nash Equilibria: [@HREF("https://arxiv.org/abs/2504.03456","H. Abo, I. Portakal, and L. Sodomaco: A vector bundle approach to Nash equilibria")@]
        available on arXiv. 
        
      - Correlated Equilibria: [@HREF("https://www.tandfonline.com/doi/full/10.1080/10586458.2024.2340000","M.-C. Brandenburg, B. Hollering, I. Portakal: Combinatorics of Correlated Equilibria")@]
        Experimental Mathematics, 2024. 
           
      - Dependency Equilibria: [@HREF("https://www.openstarts.units.it/server/api/core/bitstreams/12c8c6f4-d535-49e3-b6a5-d4f12c331b0f/content","I. Portakal and B. Sturmfels: Geometry of dependency equilibria")@]
        published in Rend. Istit. Mat. Univ. Trieste 54 (Art. No. 5), 2022, 13, 2022.

      - Conditional Independence Equilibria: [@HREF("https://www.sciencedirect.com/science/article/pii/S0021869324006707","I. Portakal and J. Sendra-Arranz: Game theory of undirected graphical models")@]
        Journal of Algebra, Volume 666, 2025.
	
  Acknowledgement
    We thank Ben Hollering<@HREF"https://sites.google.com/view/benhollering"@> and Mahrud Sayrafi<@HREF"https://www-users.cse.umn.edu/~mahrud/"@> for their support
    during the Macaulay2 in the Sciences Workshop<@HREF"https://www.mis.mpg.de/de/events/series/macaulay2-in-the-sciences"@> where the development of this package began.
  Contributors
    The following people have generously contributed their time and effort to this project:  
    Luca Sodomaco<@HREF"https://sites.google.com/view/luca-sodomaco/home"@>.
  Caveat
    GameTheory uses Polyhedra.m2 for the methods of correlated equilibria and GraphicalModels.m2 for the methods of conditional independence equilibria.
    Throughout the package, we followed Macaulay2's convention of zero-based indexing. This can be seen e.g., in the methods of @TO nashEquilibriumRing@
    and @TO probabilityRing@. In particular, for an $n$-player game, the players are labeled $0, \ldots, n-1$, and if player $i$ has $d_i$ pure strategies,
    they are labeled $0, \ldots, d_i-1$.
///

------------------------------------------
-- Documentation enumerateTensorIndices --
------------------------------------------

doc ///
  Key
    enumerateTensorIndices
    (enumerateTensorIndices, ZZ)
    (enumerateTensorIndices, List)
  Headline
    generate index tuples for a tensor with given dimensions
  Usage
    enumerateTensorIndices d
  Inputs
    d: ZZ
      A single integer.
    d: List
      A list of integers, representing the format of a tensor.
  Outputs
    :List
      A list of index tuples, where each tuple corresponds to a coordinate of the tensor.
  Description

   Text
     This function generates all possible index tuples for a tensor with the specified dimensions.
     For a tensor of format $d_1 \times d_2 \cdots d_n$, it returns a list of all tuples $(i_1, i2, \ldots, i_n)$
     where $0 \leq i_j < d_j$.

   Example
     enumerateTensorIndices 5
     enumerateTensorIndices {2,2}
     enumerateTensorIndices {3,2,4}
      
  SeeAlso
   Tensor
   zeroTensor
   randomTensor
/// 

----------------------------
-- Documentation Tensor  --
----------------------------

doc ///
  Key
    Tensor
  Headline
    a mutable hash table representing a tensor
  Usage
    T = zeroTensor(R, d)
  Outputs
    :Tensor
      A Tensor object storing values at multi-indices.
  Description

    Text
      The type `Tensor` is a mutable hash table with additional metadata to represent multi-dimensional arrays.
      Each tensor has an associated format (list of dimensions), a coefficient ring, and a set of index keys.

    Example
      T = zeroTensor(QQ, {2,2});
      T#{0,0} = 1;
      T#{1,1} = 5;
      format T
      coefficientRing T
      indexset T

  SeeAlso
    zeroTensor
    randomTensor
///

----------------------------
-- Documentation indexset --
----------------------------

doc ///
  Key
    indexset
    (indexset, Tensor)
  Headline
    get the list of index tuples of a tensor
  Usage
    indexset T
  Inputs
    T: Tensor
      A tensor whose index set is to be retrieved.
  Outputs
    :List
      A list of index tuples corresponding to the entries of the tensor.
  Description
    Text
      Given a tensor $T$ of format $\{d_1, d_2, \dots, d_n\}$, this method returns a list of all index tuples
      $(i_1, i_2, \dots, i_n)$ such that $0 \leq i_j < d_j$ for each dimension $j$. Difference from @TO enumerateTensorIndices@
      is that indexset can only be used with a Tensor object. @TO indexset@ retrieves precomputed indices stored as metadata in a @TO Tensor@.

    Example
      T = zeroTensor(QQ, {2,3,4});
      indexset T

    Example
      T = zeroTensor(QQ, {2});
      indexset T
  SeeAlso
    zeroTensor
    randomTensor
    enumerateTensorIndices
///

-------------------------------
-- Documentation zeroTensor --
-------------------------------

doc ///
  Key
    zeroTensor
    (zeroTensor, List)
    (zeroTensor, Ring, List)
  Headline
    construct a tensor with zero entries from a given ring.
  Usage
    zeroTensor format
    zeroTensor(R, format)
  Inputs
    format: List
      A list of integers specifying the format of the tensor 
      (e.g., {2,2,2} creates a 2x2x2 tensor with zero entries).
    R: Ring
      A ring from which zeros will be drawn (optional).
  Outputs
   :Tensor
      A tensor whose entries are all zero.
  Description
  
    Text
      This method constructs a tensor with the specified format and fills it with 
      zeros from the given ring. Internally, it uses a hash table where each key is 
      a multi-index (a list of positions). Metadata such as the format, coefficient 
      ring, and index set are stored in the tensor as well. It is useful for defining 
      a custom tensor.
      
    Example
      T = zeroTensor {2,2};
      T#{0,0} = 1;
      T#{0,1} = 2;
      T#{1,0} = 3;
      T#{1,1} = 4;
      format T
      peek T

  SeeAlso
     randomTensor
///

--------------------------------
-- Documentation randomTensor --
--------------------------------

doc ///
  Key
    randomTensor
    (randomTensor, List)
    (randomTensor, Ring, List)
  Headline
    construct a tensor with random entries from a given ring
  Usage
    randomTensor format
    randomTensor(R, format)
  Inputs
    format: List 
        A list of integers specifying the format of the tensor (e.g. {2,2,2} creates a 2x2x2 tensor)
    R: Ring
        (Optional) A ring from which random coefficients will be drawn.
  Outputs
    :Tensor
      A tensor whose entries are randomly selected elements of the ring.
  Description

    Text
      This method constructs a tensor with the specified format and fills it with random elements from the given ring.
      Internally, it uses a hash table where each key is a multi-index (a list of positions) and the value is a random 
      element from the ring. Metadata such as the format, coefficient ring, and index set are stored in the 
      tensor as well.

    Example
      T1 = randomTensor {2,2,2};
      T1#{0,1,1}
      T2 = randomTensor(ZZ/101, {2,2});
      peek T2
  SeeAlso
      zeroTensor
/// 

-------------------------
-- Documentation slice --
-------------------------

doc ///
  Key
    slice
    (slice, Tensor, List, List)
  Headline
    extract a slice of a tensor
  Usage
    slice(T, Lstart, Lend)
  Inputs
    T: Tensor
      A tensor from which to extract the slice.
    Lstart: List
      A list of fixed indices before the varying position.
    Lend: List
      A list of fixed indices after the varying position.
  Outputs
    :List
      A list of entries of the tensor along the specified dimension.
  Description
    Text
      This method varies the index at the position given by the length of `Lstart`, iterating from 0 to $d_i - 1$, where $d_i$ is the corresponding index of the format.

      The slice is formed by fixing all other indices and varying only the one at the slicing position.

    Example
      T = zeroTensor(QQ, {2,3,2});
      T#{0,0,0} = 5;
      T#{0,1,0} = 6;
      T#{0,2,0} = 7;
      slice(T, {0}, {0})
  SeeAlso
    enumerateTensorIndices
    Tensor
///

-----------------------------------------
-- Documentation getVariableToIndexset --
-----------------------------------------

doc ///
  Key
    getVariableToIndexset
    (getVariableToIndexset, Ring, List)
  Headline
    retrieve a polynomial ring variable by its index tuple
  Usage
    getVariableToIndexset(R, indexTuple)
  Inputs
    R: Ring
      A polynomial ring whose variables are indexed by tuples.
    indexTuple: List
      A list representing the index of the variable.
  Outputs
    :RingElement
      The variable from the ring corresponding to the given index.
  Description
    Text
      Given a ring $R = \mathbb{Q}[p_{\{i,j\}}]$, this method retrieves $p_{\{i,j\}}$ when passed the index $\{i,j\}$.

    Example
      R = QQ[p_{0,0}, p_{0,1}, p_{1,0}, p_{1,1}]
      getVariableToIndexset(R, {1,0})
  SeeAlso
    probabilityRing
    assemblePolynomial
///

--------------------------------------
-- Documentation assemblePolynomial --
--------------------------------------

doc ///
  Key
    assemblePolynomial
    (assemblePolynomial, Ring, Tensor, List)
  Headline
    compute incentive constraint polynomial for deviation
  Usage
    assemblePolynomial(PR, Xi, {i,k,l})
  Inputs
    PR: Ring
      Polynomial ring of strategy probabilities.
    Xi: Tensor
      Tensor of strategy probabilities.
    {i, k, l}: List
      A triple where:
        - $i$ is the player index,
        - $k$ is the current strategy,
        - $l$ is the deviating strategy.
  Outputs
    :RingElement
      A polynomial representing the incentive constraint for player $i$ deviating from $k$ to $l$.
  Description
    Text
      This function computes a linear inequality encoding the condition for a correlated equilibrium:
      the expected utility from playing $k$ should be no less than from playing $l$.
    Example
      R = QQ[p_{0,0}, p_{0,1}, p_{1,0}, p_{1,1}];
      Xi = randomTensor(QQ, {2,2});
      assemblePolynomial(R, Xi, {0,0,1})
  SeeAlso
    probabilityRing
    getVariableToIndexset
    assemblePlayeriPolynomials
///

----------------------------------------------
-- Documentation assemblePlayeriPolynomials --
----------------------------------------------

doc ///
  Key
    assemblePlayeriPolynomials
    (assemblePlayeriPolynomials, Ring, Tensor, ZZ)
  Headline
    get all incentive constraint polynomials for a player
  Usage
    assemblePlayeriPolynomials(PR, Xi, i)
  Inputs
    PR: Ring
      Polynomial ring of probabilities.
    Xi: Tensor
      Tensor of strategy probabilities.
    i: ZZ
      The player index.
  Outputs
    :List
      A list of polynomials corresponding to the player's deviation constraints.
  Description
    Text
      For a given player $i$, this method computes all incentive constraint polynomials
      $\forall k,l \in [d_i]$, representing deviations from strategy $k$ to $l$.

    Example
      R = QQ[p_{0,0}, p_{0,1}, p_{1,0}, p_{1,1}];
      Xi = randomTensor(QQ, {2,2});
      assemblePlayeriPolynomials(R, Xi, 0)
  SeeAlso
    probabilityRing
    assemblePolynomial
    correlatedEquilibria
///

----------------------------------------
-- Documentation correlatedEquilibria --
----------------------------------------

doc ///
  Key
    correlatedEquilibria
    (correlatedEquilibria, List)
  Headline
    compute the correlated equilibrium polytope for a game
  Usage
    correlatedEquilibria X
  Inputs
    X: List
      A list of payoff tensors, one for each player. 
  Outputs
    :Polyhedron
      The polytope representing the set of correlated equilibria for the game.
  Description
    Text
      This method constructs and returns the correlated equilibrium polytope for a finite game.
      The input is a list of payoff tensors, one for each player. The tensor at position i gives the payoffs for player i.

    Example
      X1 = zeroTensor(QQ, {2,2});
      X2 = zeroTensor(QQ, {2,2});
      X1#{0,0} = -99; X1#{0,1} = 1; X1#{1,0} = 0; X1#{1,1} = 0;
      X2#{0,0} = -99; X2#{0,1} = 0; X2#{1,0} = 1; X2#{1,1} = 0;
      
      CE = correlatedEquilibria {X1, X2}
      vertices CE
      facets CE
      fVector CE

    Example
      X = randomGame {2,2,3};      
      CE = correlatedEquilibria X;
      dim CE      
  SeeAlso
    assemblePolynomial
    assemblePlayeriPolynomials
///

----------------------------------------
-- Documentation nashEquilibriumRing --
----------------------------------------

doc ///
  Key
   (nashEquilibriumRing, List)
   nashEquilibriumRing
  Headline
    define the Nash Equilibrium ring
  Usage
    nashEquilibriumRing L
  Inputs
    L:List 
     a list of n-@TO Tensor@s with same dimensions
  Outputs
    :Ring
     a polynomial ring generated by mixed strategies
  Description
   Text
    Let $p_{i,j}$ be the probability of $i$-th player choosing the $j$-th strategy, where $j \in \{0, \cdots, d_j-1\}$.
    The ideal of the totally mixed Nash equilibria of the game is defined in the polynomial ring over a field $k$
    with generators $\{p_{i,j} \ | \ 0\leq i\leq n-1, 0\leq j\leq d_j-1\}$. The ring generators are ordered lexicographically.
   Example
    tensors = randomGame {2,4,3};
    R = nashEquilibriumRing tensors;
    baseRing R
    gens R
  SeeAlso
   nashEquilibriumIdeal
   deltaList
   blockDerangements
   numberTMNE
///

----------------------------------------
-- Documentation nashEquilibriumIdeal --
----------------------------------------

doc ///
  Key
   (nashEquilibriumIdeal, Ring, List)
   nashEquilibriumIdeal
  Headline
    make the Nash Equilibrium ideal
  Usage
    nashEquilibriumIdeal(R, L)
  Inputs
    R:Ring 
     the Nash Equilibrium ring. Typically obtained via @TO nashEquilibriumRing@
    L:List
     a list of n payoff tensors
  Outputs
    :Ideal
      an ideal in the Nash Equilibrium ring R generated by the Nash equilibrium polynomials, along with the linear relations of the probability variables
  Description
   Text
    For an $n$-player game, the totally mixed Nash equilibria are the zero loci of a system of polynomials in the interior of product of probability simplices.
    The coefficients of these polynomials are certain differences of the entries of the payoff tensors. These polynomials, together with the linear constraints
    $\sum^{d_i -1}_{j=0} p_{i,j}=1$ for each player $i$, generate an ideal in the polynomial ring computed by @TO nashEquilibriumRing@.
   Example
    tensors = randomGame {2,2,2};
    R = nashEquilibriumRing tensors;
    I = nashEquilibriumIdeal(R, tensors)
   Text
    An introduction together with the relevant definitions is given in Chapter 6, Sturmfels, Bernd, @EM "Solving Systems of Polynomial Equations"@. American Mathematical Society,
    2002 and in Abo, Hirotachi, Portakal, Irem, and Sodomaco, Luca, @EM "A vector bundle approach to Nash equilibria"@, arXiv:2504.03456.
  SeeAlso
   nashEquilibriumRing
   deltaList
   blockDerangements
   numberTMNE
///

----------------------------------------
-- Documentation deltaList --
----------------------------------------

doc ///
  Key
   (deltaList, List)
   deltaList
  Headline
    generate the list of Newton polytopes for a generic game
  Usage
    deltaList d
  Inputs
    d:List
     a list of positive integers, representing the format of the game
  Outputs
    :List
     a list of polytopes formed by taking the product of certain convex sets and simplices
  Description
   Text
    For a generic $n$-player game where the $i$-th player has $d_i$ pure strategies, the maximum number of
    isolated totally mixed Nash equilibria is given by the mixed volume of the following list of polytopes:

    \[ (\Delta^{(1)}, \cdots, \Delta^{(1)},\Delta^{(2)}, \cdots, \Delta^{(2)}, \cdots, \Delta^{(n)}, \cdots, \Delta^{(n)}),\]

    where each $\Delta^{(i)}$ repeats itself $d_i - 1$ times, and is the product of simplices

    \[ \Delta^{(i)} := \Delta_{d_{1}-1}\times \Delta_{d_{2}-1} \times \cdots \times \Delta_{d_{i-1}-1} \times \{0\} \times \Delta_{d_{i+1}-1} \times \cdots \times \Delta_{d_{n}-1}.\]

    This function constructs and returns this list of polytopes. Each $\Delta^{(i)}$ is a polytope in an ambient vector space of dimension $d_1+d_2+\cdots+d_{n}-n$.
   Example
    DL = deltaList {2,2,2}
   Text
    Each entries of DL is a polytope of dimension 2, in a $2+2+2-3=3$ dimensional vector space.
   Example
    apply(DL, p -> dim p)
    apply(DL, p -> ambDim p)
  SeeAlso
   nashEquilibriumRing
   nashEquilibriumIdeal
///

-----------------------------------
-- Documentation numberTMNE --
-----------------------------------

doc ///
  Key
   (numberTMNE, List)
   numberTMNE
  Headline
    compute the maximum number of totally mixed Nash equilibria for a generic game
  Usage
    numberTMNE d
  Inputs
    d:List
     a list of integers representing the format of the game
  Outputs
    :ZZ
     an integer value, the degree of the top Chern class of a vector bundle on a product of projective spaces, representing the maximum number of totally mixed Nash equilibria of a generic game of format $\mathbf{d}$.
  Description
   Text
    For an $n$-player game where the $i$-th player has $d_i$ pure strategies, the maximum number of isolated totally mixed Nash equilibria is given
    by the degree $c(\mathbf{d})$ of the top Chern class of the following vector bundle:

    \[ E \coloneqq \bigoplus_{i=0}^{n-1} \ko_{\mathbb{P}^{\mathbf{d}}}({\mathbf{1}}_i)^{\oplus(d_i-1)},\]

    where $\mathbb{P}^{\mathbf{d}}=\prod_{i=0}^{n-1}\mathbb{P}^{d_i-1}$ and $\mathbf{1}_i=(1,\ldots,1,0,1,\ldots,1)$, where the entry $0$ is in the $i$-th component.
    In particular, this function computes the integer $c(\mathbf{d})$ as the coefficient of the monomial
    $\prod_{i=0}^{n-1} h_i^{d_i-1}$ in $\prod_{i=0}^{n-1} \hat{h}_i^{d_i-1}$ with $\hat{h}_i\coloneqq \sum_{j\neq i}h_j$,
    where $h_i$ denotes the pullback of the hyperplane class on the $i$-th factor $\mathbb{P}^{d_i-1}$ of $\PP^\bd$ via the projection map.

   Example
    d = {2,2,2};
    nTMNE = numberTMNE d
   Text
    Alternatively, if you have a tensor $T$, you can compute its maximum number as follows:
   Example
    T = randomTensor {2,2,2}
    nTMNE2 = numberTMNE format T
  SeeAlso
   blockDerangements
///

-------------------------------------
-- Documentation blockDerangements --
-------------------------------------

doc ///
  Key
   (blockDerangements, List)
   blockDerangements
  Headline
    compute the block derangements
  Usage
    blockDerangements D
  Inputs
    D:List
     a list of integers representing the format of the game
  Outputs
    :List
     a list of block derangements with respect to D.
  Description
   Text
    Given a partition $\{F_0,\ldots,F_{n-1}\}$ of a finite set $F$, a block derangement of $F$ with respect to $\{F_0,\ldots,F_{n-1}\}$ is
    a permutation $P\colon F\to F$ of $F$ such that $P(F_i)\cap F_i=\emptyset$ for every $i\in\{0,\ldots,n-1\}$.

    The function considers the input $D=(d_0,\ldots,d_{n-1})$, defines the set $F=F_0\cup\cdots\cup F_{n-1}$, where $F_i=\{(i,j)\mid j\in\{0,\ldots,d_i-2\}\}$
    for every $i\in\{0,\ldots,n-1\}$, and computes the set of permutations of F.
    Then, it creates an empty list BD, and for each permutation $P$, if $P(F_i)\cap F_i=\emptyset$ for every $i\in\{0,\ldots,n-1\}$, then $P$ is added to the list BD.
    The function returns the list BD.
    
    The number of elements of BD corresponds to the maximum number of totally mixed Nash equilibria of a generic game of a given format.
   
   Example
    D = {3,3,3};
    BD = blockDerangements D;
    netList BD
  SeeAlso
    deltaList
    numberTMNE
///

-----------------------------------
-- Documentation probabilityRing --
-----------------------------------

doc ///
  Key
      probabilityRing
      (probabilityRing, List)
  Headline
      ring of probability distributions of a game indexed by ordered multi-indices
  Usage
      probabilityRing(Di)
  Inputs
      Di:List
         a list of natural numbers $d_0,\dots,d_{n-1}$
  Outputs
      :Ring  
       a polynomial ring with a tensor of variables $p_{i_0,\dots,i_{n-1}}$
       such that $i_j$ runs from $0$ to $d_j-1$.
  Description
      Text
          The list $Di$ represents the format of the game.
          In this example, we create a ring of probability distributions coming from a
          game of format {2, 3, 2}. This format can be accessed from the ring through
          the field "gameFormat".
          
          The variables $p#i$ are the entries of the tensor $p$, which can be
          accessed from the ring through the field "probabilityVariable".
      Example
          Di = {2,3,2};
          PR = probabilityRing Di;
          numgens PR
          pairs PR#"probabilityVariable"  
      Text 
          The optional argument "CoefficientRing" allows to change the base field. If no choice is
          specified, the base field is set to QQ. It is also possible to change the name of the
          variable tensor through the optional argument "ProbabilityVariableName", which is set to
          the string "p" by default.
      Example
          PR2 = probabilityRing (Di, CoefficientRing=>RR, ProbabilityVariableName=>"q");
          coefficientRing PR2
          pairs PR2#"probabilityVariable"
      Text
          Some functions such as @TO spohnIdeal@, @TO konstanzMatrix@, @TO ciIdeal@ or @TO spohnCI@  require the ring to be created by this function
          or in a similar manner.
  SeeAlso
      spohnIdeal
      konstanzMatrix
      ciIdeal
      spohnCI
///

-----------------------------------------
-- Documentation probabilitySumIdeal --
-----------------------------------------

doc ///
  Key
      probabilitySumIdeal
      (probabilitySumIdeal, Ring)
  Headline
      ideal enforcing that a probability distribution sums to 1
  Usage
      probabilitySumIdeal R
  Inputs
      R:Ring
          a ring typically from @TO probabilityRing@
  Outputs
      :Ideal
          the ideal generated by the sum of all probabilities minus 1
  Description
      Text
          Constructs the ideal expressing normalization of a joint probability distribution.
          If given a list, it first calls @TO probabilityRing@ with default options.
      Example
          R = probabilityRing {2,3,4};
          probabilitySumIdeal R
  SeeAlso
      probabilityRing
      spohnIdeal
      spohnCI
///

------------------------------
-- Documentation randomGame --
------------------------------

doc ///
  Key
    randomGame 
  Headline
    construct a game of a given format with arbitrary payoffs
  Usage
    randomGame(Di)
  Inputs
    Di:List 
      a list describing the format of the game
  Outputs
    :List  
      a list of n tensors of the given format that are the payoff tensors of a random game
  Description
    Text 
      The list $Di$ represents the format of the game. 
      This example creates a random game of format $2 \times 2$.
      
    Example
      X = randomGame({2,2});
      peek X#0
      peek X#1
    Text
      The optional argument CoefficientRing allows to change the ring of payoffs. 
      If no coefficient choice is specified, the payoffs will be rational numbers.
      This example creates a random game of format $2 \times 2$ with integer coefficients.
    Example
      X = randomGame({2,2}, CoefficientRing => ZZ);
      peek X#0
      peek X#1
    Text
     Outputs of this function can be used as input for the functions @TO nashEquilibriumIdeal@, @TO spohnMatrices@, @TO spohnIdeal@ and @TO konstanzMatrix@. 
  SeeAlso
    nashEquilibriumIdeal
    spohnMatrices
    spohnIdeal
    konstanzMatrix    
///

---------------------------------
-- Documentation spohnMatrices --
---------------------------------

doc ///
  Key
    spohnMatrices   
  Headline
    compute the list of Spohn matrices of a given game
  Usage
    spohnMatrices(PR,X)
  Inputs
     PR:Ring 
      a probability ring obtained via probabilityRing(Di), where $Di$ is the format of the game
     X:List 
      a list of n tensors of format $D_i$ describing the payoffs of the game
  Outputs
    :List  
      the list of n Spohn matrices describing the dependency equilibria of the game $X$
  Description
    Text 
      It is crucial that the formats in PR and X match up.
      The Spohn matrix $M_i$ is the $d_i \times 2$ matrix encoding the denominators and nominators
      of the conditional expected payoffs of the $i$-th player.
      The Spohn matrices have rank one at the dependency equilibria of the game $X$.
      
    Example
      Di = {2,2,3};
      PR = probabilityRing(Di);
      X = randomGame(Di);
      M = spohnMatrices(PR,X)
  SeeAlso
    probabilityRing
    randomGame
    spohnIdeal
    konstanzMatrix
///

------------------------------
-- Documentation spohnIdeal --
------------------------------

doc ///
  Key
    spohnIdeal
  Headline
    compute the ideal of the Spohn variety of a given game
  Usage
    spohnIdeal(PR,X)
  Inputs
     PR:Ring 
      a probability ring obtained via probabilityRing(Di), where $Di$ is the format of the game
     X:List 
      a list of n tensors of format $D_i$ describing the payoffs of the game
  Outputs
    :List  
      the ideal generated by the $2\times 2$ minors of the Spohn matrices of the game $X$
  Description
    Text 
      It is crucial that the formats in PR and X match up.
      The Spohn ideal $I_X$ is the ideal defining the Spohn variety of a game $X$, which contains the dependency equilibria of the game $X$.
      Its generators are given by the $2\times 2$ minors of the Spohn matrices.
      This function uses the function spohnMatrices to compute the Spohn matrices of the given game.     
    Example
      Di = {2,2,3};
      PR = probabilityRing(Di);
      X = randomGame(Di);
      I = spohnIdeal(PR,X)
  SeeAlso
    probabilityRing
    randomGame
    spohnMatrices
    konstanzMatrix    
///

----------------------------------
-- Documentation konstanzMatrix --
----------------------------------

doc ///
  Key
    konstanzMatrix
  Headline
    construct the Konstanz matrix of a given game
  Usage
    konstanzMatrix(PR, X)
  Inputs
    PR:Ring 
      a probability ring obtained via probabilityRing(Di), where $Di = \{d_1 , \cdots , d_n\}$ is the format of the game
    X:List 
      a list of n tensors of format $D_i$ describing the payoffs of the game
  Outputs
    :Matrix  
      the $(d_1 + \ldots + d_n) \times (d-1 \cdots d_n)$-dimensional Konstanz matrix  
  Description
    Text 
      It is crucial that the formats in PR and X match up.
      The Konstanz matrix $K_X(k)$ is the unique matrix with monic polynomials as entries such that the Spohn variety is
      the union $\bigcup_{k \in (\mathbb P^1)^n} \ker K_X(k)$.   
    Example
      Di = {2,2,3};
      PR = probabilityRing(Di);
      X = randomGame(Di);
      K = konstanzMatrix(PR,X)
    Text
      The optional argument KonstanzVariableName allows to change the name of the variables. 
      If no variable name choice is specified, the variables will be named with k.    
    Example
      Di = {2,2};
      PR = probabilityRing(Di);
      X = randomGame(Di);
      K = konstanzMatrix(PR,X, KonstanzVariableName => "z")

  SeeAlso
    probabilityRing
    randomGame
    spohnMatrices
    spohnIdeal 
///

--------------------------------
-- Documentation toMarkovRing --
--------------------------------

doc ///
  Key
   toMarkovRing
   (toMarkovRing, Ring)
  Headline
   ring of joint probability distributions created with the markovRing function from the GraphicalModels package
  Usage
   toMarkovRing R
  Inputs
   R:PolynomialRing
    created using the probabilityRing method
  Outputs
   :PolynomialRing
    a polynomial ring isomorphic to the input ring created by the markovRing method from the GraphicalModels package,
    with variables $q_{(i_1+1, \dots , i_k+1)}$ corresponding to the variables $p_{\{i_1, \ldots, i_k\}}$
    of the input ring
  Description
   Text
    Given a ring created with the probabilityRing function, this function creates the canonically isomorphic ring
    defined by the markovRing function from the GraphicalModels package.
    The variable name of the output ring is set to be different from the variable name of the input ring:
    the default variable name of the output ring is "p",
    and if the variable name of the input ring is "p" then the variable name of the output ring becomes "q".   

   Example
    R = probabilityRing({2,3,4}, CoefficientRing => ZZ/32003, ProbabilityVariableName => "x")
    markovR = toMarkovRing R;
    numgens markovR
    R_0, R_11, R_23 

  SeeAlso
   probabilityRing
   gaussianRing
///

-----------------------------------
-- Documentation mapToMarkovRing --
-----------------------------------

doc ///
  Key
   mapToMarkovRing
   (mapToMarkovRing, Ring)
  Headline
   ring isomorphism from the given probabilityRing to the corresponding markovRing
  Usage
   mapToMarkovRing R
  Inputs
   R:Ring
    must be a probabilityRing
  Outputs
   :RingMap
    the isomorphism identifying R with toMarkovRing(R).
    The variable $p_{\{i_1, \ldots, i_k\}}$ is sent to $q_{(i_1+1, \dots , i_k+1)}$.
  
  Description
   Text
    This function creates the RingMap from a given probabilityRing to its canonically isomorphic
    markovRing.
   Example
    R = probabilityRing {2,3,4};
    markovR = toMarkovRing R;
    F = mapToMarkovRing R
    target F
    source F
    isInjective F
    F.matrix  

  SeeAlso
   toMarkovRing
   mapToProbabilityRing
///

----------------------------------------
-- Documentation mapToProbabilityRing --
----------------------------------------

doc ///
  Key
   mapToProbabilityRing
   (mapToProbabilityRing, Ring)
  Headline
   ring isomorphism to the given probabilityRing from the corresponding markovRing
  Usage
   mapToProbabilityRing R
  Inputs
   R:Ring
    must be a probabilityRing
  Outputs
   :RingMap
    the isomorphism identifying R with toMarkovRing(R).
    The variable $q_{(i_1+1, \dots , i_k+1)}$ is sent to $p_{\{i_1, \ldots, i_k\}}$.
  
  Description
   Text
    This function creates the RingMap to a given probabilityRing from its canonically isomorphic
    markovRing.
   Example
    R = probabilityRing {2,3,4};
    markovR = toMarkovRing R;
    F = mapToProbabilityRing R
    target F
    source F
    isInjective F
    F.matrix

  SeeAlso
   toMarkovRing
   mapToProbabilityRing
///

---------------------------
-- Documentation ciIdeal --
---------------------------

doc ///
  Key
   ciIdeal
   (ciIdeal, Ring, List)
   (ciIdeal, Ring, Graph)
   (ciIdeal, Ring, List, List)
   (ciIdeal, Ring, Graph, List)
  Headline
   the ideal of a list of conditional independence statements
  Usage
   ciIdeal (R, Stmts)
   ciIdeal (R, G)
   ciIdeal (R, Stmts, PlayerNames)
   ciIdeal (R, G, PlayerNames)
  Inputs
   R:Ring
     must be created using probabilityRing
   Stmts:List
     the list of conditional independence statements 
   G:Graph
     the graph modelling the conditional dependencies between players
   PlayerNames:List
     the ordered list of players - the names of the random variables in the conditional independence
     statements or vertices of the graph. If PlayerNames is omitted, the players
     (or the vertices of G) are assumed to be labelled 1..n.
  Outputs
   :Ideal
    the ideal in R of conditional independence relations
  Description
   Text
    {\tt ciIdeal} computes the ideal of a list of conditional independence statements.
    The input can be the list of conditional independence statements itself,
    or a graph modelling the conditional dependencies between players.
    This method is the same as @TO conditionalIndependenceIdeal@ from GameTheory.m2,
    included here for convenience and compatibility with this package.   

    A single conditional independence statement is a list consisting of three disjoint
    lists of indices for random variables, e.g. $\{ \{1,2\},\{4\}, \{3\} \}$
    which represents the conditional independence statement ``$(X_1, X_2)$
    is conditionally independent of $X_4$ given $X_3$''.
    Given an undirected graph $G$, the conditional independence statements are produced via
    the globalMarkov function from the GraphicalModels package. A global Markov statement
    for $G$ is a list $\{A, B, C\}$ of three disjoint lists of vertices of $G$, where the
    subset $C$ separates the subset $A$ from the subset $B$ in the graph $G$.   

    The output is an ideal of the given ring PR, which must be created using the
    probabilityRing function. This function computes the ideal using the
    @TO conditionalIndependenceIdeal@ function from the GraphicalModels package, then
    maps it to an ideal of PR via the mapToProbabilityRing function.

   Example
      FF = ZZ/32003
      d = {2,3,2};
      PR = probabilityRing (d, CoefficientRing => FF);
      G = graph ({}, Singletons => {1,2,3});
      I = ciIdeal (PR, G)

   Text
       Here is an example where the vertices of the graph need to be relabeled.
      
   Example  
      FF = ZZ/32003
      d = {2,3,2};
      PR = probabilityRing (d, CoefficientRing => FF);
      G = graph {{John,Matthew},{Matthew,Sarah}};
      I = ciIdeal (PR, G, {John,Matthew,Sarah})
     
   Text
       Here is an example where the conditional independence relations are given with a List.

   Example
       FF = ZZ/32003
       d = {2,3,2};
       PR = probabilityRing (d, CoefficientRing => FF);
       G = graph {{1,2},{2,3}};
       L = {{{1},{3},{2}}}
       I1 = ciIdeal (PR,G)
       I2 = ciIdeal (PR,L)
       I1 == I2
 
  SeeAlso
     conditionalIndependenceIdeal 
     mapToProbabilityRing
     toMarkovRing
     ciIdeal
     globalMarkov
///

-----------------------------------------
-- Documentation intersectWithCImodel  --
-----------------------------------------

doc ///
  Key
    intersectWithCImodel
    (intersectWithCImodel, Ideal, List)
    (intersectWithCImodel, Ideal, List, List)
    (intersectWithCImodel, Ideal, Graph)
    (intersectWithCImodel, Ideal, Graph, List) 
  Headline
    ideal of the intersection of a given variety with the conditional independence model
  Usage
    intersectWithCImodel(V, Stmts)
    intersectWithCImodel(V, Stmts, PlayerNames)
    intersectWithCImodel(V, G)
    intersectWithCImodel(V, G, PlayerNames)
  Inputs
    V:Ideal 
      an ideal of a ring created with probabilityRing 
    Stmts:List
      the list of conditional independence statements 
    G:Graph
      the graph modelling the conditional dependencies between players
    PlayerNames:List
      the ordered list of players - the names of the random variables in the conditional independence
      statements or vertices of the graph. If PlayerNames is omitted, the players
      (or the vertices of G) are assumed to be labelled 1..n.    
  Outputs
    :Ideal 
       The ideal of the intersection of the given variety with the conditional independence model
       determined by the conditional independence statements/graph. 
  Description
    Text
      {\tt intersectWithCImodel} calculates the ideal of the intersection of the given variety V with
      the conditional independence model determined by a set of conditional probability statements or an undirected graph.
      More precisely, the output is the ideal of the closure of the variety given by removing the components in
      the coordinate hyperplanes from the intersection of the variety V and the conditional independence
      model.

      The input for the conditional independence model can be a set of conditional probability statements or
      an undirected graph.
      A single conditional independence statement is a list consisting of three disjoint
      lists of indices for random variables, e.g. $\{ \{1,2\},\{4\}, \{3\} \}$
      which represents the conditional independence statement ``$(X_1, X_2)$
      is conditionally independent of $X_4$ given $X_3$''. In the context of game theory, the variable
      $X_i$ represents the strategy of player $i$.

      Given an undirected graph $G$, the conditional independence statements are produced via
      the globalMarkov function from the GraphicalModels package. A global Markov statement
      for $G$ is a list $\{A, B, C\}$ of three disjoint lists of vertices of $G$, where the
      subset $C$ separates the subset $A$ from the subset $B$ in the graph $G$.
    Example
     FF = ZZ/32003
     d = {2,2,2};
     X = randomGame(d, CoefficientRing => FF);
     PR = probabilityRing(d, CoefficientRing => FF);
     V = spohnIdeal(PR, X);
     G1 = graph ({}, Singletons => {1,2,3});
     G2 = graph ({{1,2}}, Singletons => {3});
     I1 = intersectWithCImodel(V, G1)
     I2 = intersectWithCImodel(V, G2)

    Text
      Here is an example where the vertices of the graph need to be relabeled.
      
    Example  
     FF = ZZ/32003;
     d = {2,2,2};
     X = randomGame(d, CoefficientRing => FF);
     PR = probabilityRing(d, CoefficientRing => FF);
     V = spohnIdeal(PR, X);
     G1 = graph {{John,Matthew},{Matthew,Sarah}};
     G2 = graph {{a,b},{b,c},{c,a}};
     I1 = intersectWithCImodel(V, G1, {John,Matthew,Sarah})
     I2 = intersectWithCImodel(V, G2, {a,b,c}) 
      
    Text
      Here is an example where the conditional independence relations are given with a List.

    Example
      FF = ZZ/32003;
      d = {2,2,2};
      X = randomGame(d, CoefficientRing => FF);
      PR = probabilityRing(d, CoefficientRing => FF);
      V = spohnIdeal(PR, X);
      G = graph ({{1,2}},Singletons => {3});
      L = {{{1,2},{3},{}}};
      I1 = intersectWithCImodel(V, G)
      I2 = intersectWithCImodel(V, L)
      I1 == I2

    Text
      The Verbose=>true option prints the progress of each step in the saturation process -
      a message is printed after saturating the ideal $V$, the conditional independence ideal $I$,
      and the sum $V + I$ with respect to each hyperplane of the probability simplex.
    Example
      FF = ZZ/32003;
      d = {2,3,2};
      X = randomGame(d, CoefficientRing => FF);
      PR = probabilityRing(d, CoefficientRing => FF);
      V = spohnIdeal(PR, X);
      L = {{{1,2},{3},{}}};
      I = intersectWithCImodel(V, L, Verbose=>true);
 
  SeeAlso
    conditionalIndependenceIdeal 
    mapToProbabilityRing
    toMarkovRing
    ciIdeal
    globalMarkov
///

---------------------------
-- Documentation spohnCI --
---------------------------

doc ///
  Key
    spohnCI
    (spohnCI, Ring, List, Graph)
    (spohnCI, Ring, List, Graph, List)
    (spohnCI, Ring, List, List)
    (spohnCI, Ring, List, List, List) 
  Headline
    ideal of the Spohn conditional independence (CI) variety
  Usage
    spohnCI(PR, X, G)
    spohnCI(PR, X, G, PlayerNames)
    spohnCI(PR, X, Stmts)
    spohnCI(PR, X, Stmts, PlayerNames)
  Inputs
    PR:Ring 
      the probability ring (must be created with @TO probabilityRing@)
    X:List 
      the n tensors defining the game 
    G:Graph
      the graph specifying the conditional independence conditions
    Stmts:List
      a list of lists {L1,L2,L3} corresponding to the relation "L1 and L2 are conditionally independent given L3".    
    PlayerNames:List
      the ordered list of players - the names of the random variables in the conditional independence
      statements or vertices of the graph. If PlayerNames is omitted, the players
      (or the vertices of G) are assumed to be labelled 1..n.
  Outputs
    :Ideal 
       The ideal of the Spohn CI variety
  Description
    Text
      {\tt spohnCI} computes the ideal of the Spohn conditional independence variety for a game $X$ and
      conditional independence model determined by an undirected graph $G$ or set of conditional
      independence statements $Stmts$.

      The input for the conditional independence model can be a set of conditional probability statements or
      an undirected graph.
      A single conditional independence statement is a list consisting of three disjoint
      lists of indices for players, e.g. $\{ \{1,2\},\{4\}, \{3\} \}$
      which represents the conditional independence statement ``The strategies of Players 1 and 2
      are conditionally independent of Player 4's strategy given Player 3's strategy''.

      Given an undirected graph $G$, the conditional independence statements are produced via
      the globalMarkov function from the GraphicalModels package. A global Markov statement
      for $G$ is a list $\{A, B, C\}$ of three disjoint lists of vertices of $G$, where the
      subset $C$ separates the subset $A$ from the subset $B$ in the graph $G$.
    Example
      FF = ZZ/32003
      d = {2,2,2};
      X = randomGame(d, CoefficientRing => FF);
      PR = probabilityRing(d, CoefficientRing => FF);
      G1 = graph ({}, Singletons => {1,2,3});
      G2 = graph ({{1,2}}, Singletons => {3});
      I1 = spohnCI(PR,X,G1)
      I2 = spohnCI(PR,X,G2)
      
    Text
      Here is an example where the vertices of the graph need to be relabeled.
      
    Example  
      FF = ZZ/32003
      d = {2,3,2};
      X = randomGame(d, CoefficientRing => FF);
      PR = probabilityRing(d, CoefficientRing => FF);
      G1 = graph {{John,Matthew},{Matthew,Sarah}};
      G2 = graph {{a,b},{b,c},{c,a}};
      I1 = spohnCI(PR,X,G1, {John,Matthew,Sarah})
      I2 = spohnCI(PR,X,G2, {a,b,c}) 
      
    Text
      Here is an example where the conditional independence relations are given with a List.

    Example
      FF = ZZ/32003
      d = {2,2,2};
      X = randomGame(d, CoefficientRing => FF);
      PR = probabilityRing(d, CoefficientRing => FF);
      G = graph ({{1,2}},Singletons => {3});
      L = {{{1,2},{3},{}}};
      I1 = spohnCI(PR,X,G)
      I2 = spohnCI(PR,X,L)
      I1 == I2

    Text
      The Verbose=>true option prints the progress of each step in the saturation process -
      a message is printed after saturating the ideal of the Spohn variety $V$, the conditional
      independence ideal $I$, and the sum $V + I$ with respect to each hyperplane of the
      probability simplex.
    Example
      FF = ZZ/32003;
      d = {2,3,2};
      X = randomGame(d, CoefficientRing => FF);
      PR = probabilityRing(d, CoefficientRing => FF);
      L = {{{1,2},{3},{}}};
      I = spohnCI(PR, X, L, Verbose=>true);
 
  SeeAlso
    spohnIdeal
    ciIdeal
    intersectWithCImodel
    conditionalIndependenceIdeal
///


--*************************--
--          TESTS          --
--*************************--

-----------------------------------
--- TEST enumerateTensorIndices ---
-----------------------------------

TEST ///
assert(enumerateTensorIndices 3 === {{0}, {1}, {2}})
assert(enumerateTensorIndices {2,2} === {
    {0,0}, {0,1},
    {1,0}, {1,1}
})
assert(enumerateTensorIndices {2,1,2} === {
    {0,0,0}, {0,0,1},
    {1,0,0}, {1,0,1}
})
///

-----------------------
--- TEST zeroTensor ---
-----------------------

TEST ///
T = zeroTensor(QQ, {2,2});
assert(class T === Tensor)
assert(format T === {2,2})
assert(coefficientRing T === QQ)
assert(indexset T === {{0,0},{0,1},{1,0},{1,1}})
assert(all(select(keys T, k -> class k === List), k -> T#k == 0_QQ))
///

-------------------------
--- TEST randomTensor ---
-------------------------

TEST ///
T = randomTensor(QQ, {2,2});
assert(class T === Tensor)
assert(format T === {2,2})
assert(coefficientRing T === QQ)
assert(indexset T === {{0,0},{0,1},{1,0},{1,1}})
///

------------------
--- TEST slice ---
------------------

TEST ///
T = zeroTensor(QQ, {2,3,2});
T#{0,0,0} = 5;
T#{0,1,0} = 6;
T#{0,2,0} = 7;
S = slice(T, {0}, {0});
assert(S === {5,6,7});
///

----------------------------------
--- TEST getVariableToIndexset ---
----------------------------------

TEST ///
R = QQ[p_{0,0}, p_{0,1}, p_{1,0}, p_{1,1}];
ki = {0,1};
result = getVariableToIndexset(R, ki);
assert(result === p_{0,1})
///

-------------------------------
--- TEST assemblePolynomial ---
-------------------------------

TEST ///
PR = QQ[p_{0,0}, p_{0,1}, p_{1,0}, p_{1,1}];
Xi = zeroTensor(QQ, {2,2});
Xi#{0,0} = 1;
Xi#{0,1} = 2;
Xi#{1,0} = 3;
Xi#{1,1} = 4;
ikl = {0, 0, 1}; -- Player index i=0, current strategy k=0, deviating strategy l=1
polytest = assemblePolynomial(PR, Xi, ikl);
expected = (-2*p_{0,0} - 2*p_{0,1});
assert(polytest == expected)
///

---------------------------------------
--- TEST assemblePlayeriPolynomials ---
---------------------------------------

TEST ///
PR = QQ[p_{0,0}, p_{0,1}, p_{1,0}, p_{1,1}];
Xi = zeroTensor(QQ, {2,2});
Xi#{0,0} = 1;
Xi#{0,1} = 2;
Xi#{1,0} = 3;
Xi#{1,1} = 4;
i = 0;
polystest = assemblePlayeriPolynomials(PR, Xi, i);
expected = {0, -2*p_{0,0} -2*p_{0,1}, 2*p_{1,0} + 2*p_{1,1}, 0};
assert(polystest == expected)
///

---------------------------------
--- TEST correlatedEquilibria ---
---------------------------------

TEST ///
X1 = randomTensor(QQ, {2,2,2});
X2 = randomTensor(QQ, {2,2,2});
X3 = randomTensor(QQ, {2,2,2});
CE = correlatedEquilibria {X1, X2, X3};
assert(class CE === Polyhedron)
assert(#vertices CE >= 1) -- CE polytope must be non-empty
///

---------------------------------
--- TEST correlatedEquilibria ---
---------------------------------

TEST ///
X1 = zeroTensor(QQ, {2,2});
X2 = zeroTensor(QQ, {2,2});
X1#{0,0} = -99; X1#{0,1} = 1; X1#{1,0} = 0; X1#{1,1} = 0;
X2#{0,0} = -99; X2#{0,1} = 0; X2#{1,0} = 1; X2#{1,1} = 0;
CE = correlatedEquilibria {X1, X2};
assert(class CE === Polyhedron)
assert(#vertices CE  == 5)
assert(vertices CE ==  matrix{
    {0, 0, 1/199, 0, 1/10000},
    {1, 0, 99/199, 1/101, 99/10000},
    {0, 1, 99/199, 1/101, 99/10000},
    {0, 0, 0, 99/101, 9801/10000}
})
///

----------------------------------------
--- TEST mixedProbabilityRing (List) ---
----------------------------------------
TEST ///
debug needsPackage "GameTheory"
L = {2,3,2};
R1 = mixedProbabilityRing L;
expectedVars = {p_{0,0},p_{0,1},p_{1,0},p_{1,1},p_{1,2},p_{2,0},p_{2,1}};
assert(toString gens R1 === toString expectedVars);
///

------------------------------------------
--- TEST mixedProbabilityRing (Tensor) ---
------------------------------------------
TEST ///
debug needsPackage "GameTheory"
T = randomTensor {2,3,2};
R2 = mixedProbabilityRing T;
-- should match the List‐case for {2,3,2}
assert(toString gens R2 == toString gens (mixedProbabilityRing {2,3,2}));
///

---------------------------------
--- TEST differencesFromFirst ---
---------------------------------
TEST ///
debug needsPackage "GameTheory"
assert(differencesFromFirst {2,3,10,15} == {1,8,13});
///

------------------------------
--- TEST monomialFromIndex ---
------------------------------
TEST ///
debug needsPackage "GameTheory"
L = {5,5,5,5};
R = mixedProbabilityRing L;
assert(monomialFromIndex({3,2,0},0,R) == p_{1,3} * p_{2,2} * p_{3,0})
assert(monomialFromIndex({3,2,0},1,R) == p_{0,3} * p_{2,2} * p_{3,0})
assert(monomialFromIndex({3,2,0},2,R) == p_{0,3} * p_{1,2} * p_{3,0})
assert(monomialFromIndex({3,2,0},3,R) == p_{0,3} * p_{1,2} * p_{2,0})
assert(monomialFromIndex({1,3,4},3,R) == p_{0,1} * p_{1,3} * p_{2,4})
///

-----------------------------------
--- TEST equilibriumPolynomials ---
-----------------------------------
TEST ///
debug needsPackage "GameTheory"
testIndices = enumerateTensorIndices {2,2,2};
T = zeroTensor {2,2,2};
TE = {0, 3, 4, 2, 5, 1, 0, 3};
scan(pairs testIndices, (j,i)->(T#i = TE#j));
R = nashEquilibriumRing toList(3:T);
polysPlayer0 = equilibriumPolynomials(T,0,R);
polysPlayer1 = equilibriumPolynomials(T,1,R);
polysPlayer2 = equilibriumPolynomials(T,2,R);
Targetpolys = {{5 * p_{1, 0} * p_{2, 0} - 4 * p_{1, 1} * p_{2, 0} - 2 * p_{1, 0} * p_{2, 1} + p_{1, 1} * p_{2, 1}},
    {4 * p_{0, 0} * p_{2, 0} - 5 * p_{0, 1} * p_{2, 0} - p_{0, 0} * p_{2, 1} + 2 * p_{0, 1} * p_{2, 1}},
    {3 * p_{0, 0} * p_{1, 0} - 4 * p_{0, 1} * p_{1, 0} - 2 * p_{0, 0} * p_{1, 1} + 3 * p_{0, 1} * p_{1, 1}}};
assert({polysPlayer0, polysPlayer1, polysPlayer2} == Targetpolys)
///

--------------------------------
--- TEST nashEquilibriumRing ---
--------------------------------

TEST ///
tensorList = apply(3, i -> randomTensor {2,2,2});
R = nashEquilibriumRing tensorList;
L = {p_{0,0}, p_{0,1}, p_{1,0}, p_{1,1}, p_{2,0}, p_{2,1}};
assert(gens R == L)
///

---------------------------------
--- TEST nashEquilibriumIdeal ---
---------------------------------
TEST ///
tensorList = apply(3, i -> randomTensor {2,2,2});
R = nashEquilibriumRing tensorList;
I = nashEquilibriumIdeal(R, tensorList);
assert(isIdeal I)
///

TEST ///
testIndices = enumerateTensorIndices {2,2,2};
TList = apply(3, i-> zeroTensor {2,2,2});
TE = {{2, 2, 0, 1, 2, 2, 1, 1}, {2, 2, 0, 2, 2, 0, 1, 2}, {1, 1, 1, 2, 0, 0, 2, 1}};
scan(3, k->scan(pairs testIndices, (j,i)->(TList#k#i = TE#k#j)));
R2 = nashEquilibriumRing TList;
I = nashEquilibriumIdeal(R2, TList);
ComputedGens = first entries gens I;
TargetGens = {p_{1,1} * p_{2,0}, -2 * p_{0,0} * p_{2,0} - p_{0,1} * p_{2,0} + 2 * p_{0,1} * p_{2,1},
    p_{0,0} * p_{1,1} - p_{0,1} * p_{1,1}, p_{0,0} + p_{0,1} - 1, p_{1,0} + p_{1,1} - 1, p_{2,0} + p_{2,1} - 1};
assert(ComputedGens == TargetGens)
///

------------------------------
--- TEST directProductList ---
------------------------------
TEST ///
debug needsPackage "GameTheory"
P1 = simplex 1;   -- dim=1
P2 = simplex 2;   -- dim=2
P = directProductList {P1,P2};
assert(dim P == 3);

v1 = entries transpose vertices P1;
v2 = entries transpose vertices P2;
expectedVerts = flatten apply(v1, u -> apply(v2, w -> u | w));
assert(sort entries transpose vertices P == sort expectedVerts)
///

TEST ///
debug needsPackage "GameTheory"
Q = simplex 3;  -- a tetrahedron should just return that unchanged
assert(directProductList {Q} === Q)
///

----------------------
--- TEST deltaList ---
----------------------
TEST ///
DL = deltaList {2,4,5};
d = apply(DL, p->dim p);
assert(d == {7, 5, 5, 5, 4, 4, 4, 4})
ad = apply(DL, p->ambDim p);
assert(ad == toList (8 : 8))
///

TEST ///
DL2 = deltaList {2,2,2};
ComputedV = apply(DL2, p->entries vertices p);
TargetV = {{{0, 0, 0, 0}, {0, 1, 0, 1}, {0, 0, 1, 1}}, {{0, 1, 0, 1}, {0, 0, 0, 0}, {0, 0, 1, 1}}, {{0, 1, 0, 1}, {0, 0, 1, 1}, {0, 0, 0, 0}}};
assert(ComputedV == TargetV)
///

--singleton skipped test
TEST ///
D = {1,2,3};
DL = deltaList D;

--  (1–1)+(2–1)+(3–1) = 0+1+2 = 3 polytopes
expectedCount = sum apply(D, i -> i-1);
assert(#DL == expectedCount);

--  one for i=1: dim = (1–1)+(3–1) = 0+2 = 2,
--  two for i=2: dim = (1–1)+(2–1) = 0+1 = 1.
assert(apply(DL, p -> dim p) == {2,1,1});

--  ambient dim = (1–1)+(2–1)+(3–1) = 3
assert(apply(DL, p -> ambDim p) == toList(3 : expectedCount));
///

------------------------------
--- TEST blockDerangements ---
------------------------------

TEST ///
BD = blockDerangements{2,2,2};
L = {{set {(2, 0)}, set {(0, 0)}, set {(1, 0)}}, {set {(1, 0)}, set {(2, 0)}, set {(0, 0)}}};
assert(BD == L)
///

-----------------------
--- TEST numbertMNE ---
-----------------------

TEST ///
mv = numberTMNE {2,2,2};
assert(mv == 2)
mv2 = numberTMNE {3,3,3};
assert(mv2 == 10)
///

----------------------------
--- TEST probabilityRing ---
----------------------------

TEST ///
Di = {2,2,2};
R = probabilityRing(Di, CoefficientRing=>QQ, ProbabilityVariableName=>"q");
Q = zeroTensor(Di);

Q#{0,0,0}=q_{0,0,0};
Q#{0,0,1}=q_{0,0,1};
Q#{0,1,0}=q_{0,1,0};
Q#{0,1,1}=q_{0,1,1};
Q#{1,0,0}=q_{1,0,0};
Q#{1,0,1}=q_{1,0,1};
Q#{1,1,0}=q_{1,1,0};
Q#{1,1,1}=q_{1,1,1};

assert(all for j in enumerateTensorIndices Di list Q#j === q_j)
///

--------------------------------
--- TEST probabilitySumIdeal ---
--------------------------------

TEST ///
R = probabilityRing {2,2};
I = probabilitySumIdeal R;
assert(generators I == matrix{{R_0 + R_1 + R_2 + R_3 - 1}})
///

-----------------------
--- TEST randomGame ---
-----------------------

TEST /// 
Di = {2,2,3};
X = randomGame(Di);
assert(#X == #Di and all(#Di, i -> format(X#i) == Di))
/// 

--------------------------
--- TEST spohnMatrices ---
--------------------------

TEST /// 
Di = {2,2,3};
PR = probabilityRing(Di);
X = randomGame(Di);
M = spohnMatrices(PR,X);
assert(length M == length Di)
assert(all(0..#Di-1, i -> class M#i === Matrix))
/// 

-----------------------
--- TEST spohnIdeal ---
-----------------------

TEST /// 
Di = {2,2,3};
PR = probabilityRing(Di);
X = randomGame(Di);
I = spohnIdeal(PR,X)
assert(I == sum(spohnMatrices(PR,X), m -> minors(2, m)))
/// 

---------------------------
--- TEST konstanzMatrix ---
---------------------------

TEST /// 
A = zeroTensor {2,2};
B = zeroTensor {2,2};
A#{0,0} = 3;  A#{0,1} = 0;  A#{1,0} = 0;  A#{1,1} = 2;
B#{0,0} = 2;  B#{0,1} = 0;  B#{1,0} = 0;  B#{1,1} = 3;
PR = probabilityRing {2,2};
KM = konstanzMatrix(PR, {A,B});
assert instance(KM, Matrix)
assert(numrows KM == 4)
assert(numcols KM == 4) 
///

-------------------------
--- TEST toMarkovRing ---
-------------------------

TEST ///
R = probabilityRing({2,3,4}, CoefficientRing => ZZ/32003, ProbabilityVariableName => "x");
markovR = toMarkovRing R;
correctGens = {p_(1,1,1), p_(1,1,2), p_(1,1,3), p_(1,1,4), p_(1,2,1), p_(1,2,2),
p_(1,2,3), p_(1,2,4), p_(1,3,1), p_(1,3,2), p_(1,3,3), p_(1,3,4),
p_(2,1,1), p_(2,1,2), p_(2,1,3), p_(2,1,4), p_(2,2,1), p_(2,2,2),
p_(2,2,3), p_(2,2,4), p_(2,3,1), p_(2,3,2), p_(2,3,3), p_(2,3,4)};
assert(toString gens markovR === toString correctGens)
///

----------------------------
--- TEST mapToMarkovRing ---
----------------------------

TEST ///
R = probabilityRing({2,3,4}, CoefficientRing => ZZ/32003, ProbabilityVariableName => "x");
markovR = toMarkovRing R;
F = mapToMarkovRing R;
assert(target F === markovR)
assert(source F === R)
assert(isInjective F)
///


---------------------------------
--- TEST mapToProbabilityRing ---
---------------------------------

TEST ///
R = probabilityRing({2,3,4}, CoefficientRing => ZZ/32003, ProbabilityVariableName => "x");
markovR = toMarkovRing R;
F = mapToProbabilityRing R;
assert(target F === R)
assert(source F === markovR)
assert(isInjective F)
///

--------------------
--- TEST ciIdeal ---
--------------------

TEST ///
FF = ZZ/32003;
d = {2,2,2};
PR = probabilityRing(d, CoefficientRing => FF);
G1 = graph ({{1,2},{2,3},{1,3}});
G2 = graph ({}, Singletons => {1,2,3});
I1 = ciIdeal(PR, G1);
I2 = ciIdeal(PR, G2);
assert(I1_0==0)
assert(numcols mingens I2 == 9)
///

TEST ///
FF = ZZ/32003;
d = {2,2,2};
PR = probabilityRing(d, CoefficientRing => FF);
G = graph ({{1,2},{2,3}});
I = ciIdeal(PR, G);
L = gens PR;
J = ideal(-L_1*L_4+L_0*L_5,-L_3*L_6+L_2*L_7);  
assert(I==J)
///

---------------------------------
--- TEST intersectWithCImodel ---
---------------------------------

TEST ///
FF = ZZ/32003
d = {2,3,2};
X = randomGame(d, CoefficientRing => FF);
PR = probabilityRing(d, CoefficientRing => FF);
G = graph ({{1,2}},Singletons => {3});
L={{{1,2},{3},{}}};
V = spohnIdeal(PR, X);
I1 = intersectWithCImodel(V, L);
I2 = intersectWithCImodel(V, G);
assert(I1==I2)
assert(numcols mingens I1 == 20)
///

TEST ///
FF = ZZ/32003
d = {2,2,2};
X = randomGame(d, CoefficientRing => FF);
PR = probabilityRing(d, CoefficientRing => FF);
G = graph ({{1,2},{2,3},{1,3}});
V = spohnIdeal(PR, X);
I = intersectWithCImodel(V, G);
assert(V==I)
///

--------------------
--- TEST spohnCI ---
--------------------

TEST ///
FF = ZZ/32003
d = {2,2,2};
X = randomGame(d, CoefficientRing => FF);
PR = probabilityRing(d, CoefficientRing => FF);
G = graph ({{1,2}},Singletons => {3});
L={{{1,2},{3},{}}};
V = spohnIdeal(PR, X);
I1 = spohnCI(PR, X, L);
I2 = spohnCI(PR, X, G);
assert(I1==I2)
///

TEST ///
FF = ZZ/32003
d = {2,2,2};
X = randomGame(d, CoefficientRing => FF);
PR = probabilityRing(d, CoefficientRing => FF);
V = spohnIdeal(PR, X);
G = graph ({{1,2},{2,3}});
I = spohnCI(PR, X, G);
assert(numcols mingens I==8)
///

TEST ///
FF = ZZ/32003
d = {2,2,2};
X = randomGame(d, CoefficientRing => FF);
PR = probabilityRing(d, CoefficientRing => FF);
G = graph ({{1,2},{2,3},{1,3}});
V = spohnIdeal(PR, X);
I = spohnCI(PR, X, G);
assert(V==I)
///
end

--******************************************--
--           DEVELOPMENT SECTION	    --   	    
--******************************************--

restart
debug needsPackage "GameTheory"
check "GameTheory"

uninstallPackage "GameTheory"
restart
installPackage "GameTheory"
viewHelp GameTheory
