-------------------------------- Weights arithmetics ----------------------------------------------------------------

-- project the weights to the sublattice of the semisimple part of the parabolic subgroup
toParabolicWeights = method();
toParabolicWeights (List, ParabolicGroup) := (S,P) -> (
    P0 := P#"parabolic";
    R := rootSystem(P#"dynkinType");
    RP := rootSystem(R, P0);
    T := toList(P0);
    T = apply(T, t -> t-1);
    T = sort T;
    apply(S, l -> weight(RP, entries l^T))
    );

-- l: a weight on the semisiple part of P_X
-- s: a list of integers on the marked nodes
-- LX: the indexes of the marked nodes
-- returns a weight of R_X
mixWeights = method();
mixWeights (Weight,List,List) := (l,s,LX) -> (
    n := length entries l + length s;
    v := new MutableList;
    j := 0, k := 0;
    l = entries l;
    for i from 0 to n-1 do (
	if toString (set {i+1}? set LX) == "<" or toString (set {i+1}? set LX) == "==" then (
	    v#i = s#j;
	    j = j + 1;
	    )
	else (
	    v#i = l#k;
	    k = k + 1;
	    );
	);
    v = toList v
    )
    
-- S: a list of weights in the lattice of the semisimple part of P
-- returns a list of weights in R given by the natural embedding of the lattices
-- In particular, every component on the marked nodes is zero
toGlobalWeights = method();
toGlobalWeights (List, ParabolicGroup) := (S,P) -> (
    P0 := P#"parabolic";
    R := rootSystem(P#"dynkinType");
    Ic := set toList(1..rank(R)) - P0;
    T := {};
    for l in S do (
	for i in sort toList Ic do (
	    l = l^{0..i-2} || vector{0} || l^{i-1..#(entries l)-1};
	    );
	T = T | {weight(R,l)};
	);
    T
    )

-- returns the weight of R obtained by l by keeping only the components on the marked nodes
projection = method();
projection (Weight, ParabolicGroup) := (l,P) -> (
    P0 := P#"parabolic";
    R := rootSystem(P#"dynkinType");
    I := toList(P0);
    I = apply(I, t -> t-1);
    I = sort I;
    l = new MutableList from entries l;
    for i in I do l#i = 0;
    l = toList l;
    l = weight(R,l)
    );

-- returns the only dominant weight conjugated to l and the index of l
-- namely the minimal number of reflections need to conjugate l and v
dominantConjugate = method();
dominantConjugate (Weight, RootSystem) := (l,R) -> (
    k := 0;
    v := l;
    while not all((1..rank(R)), i ->  v_(i-1) >= 0) do (
	for j from 1 to rank(R) do (
	    if v_(j-1) < 0 then (
		k = k + 1;
		v = reflect(R,j,v);
		break;
		);
	    );
	);
    (v,k)
    );


isDominant = method();
isDominant (Weight, RootSystem) := (l,R) -> (
    all((1..rank(R)), s -> l_(s-1) >= 0) 
);

isSingular=method();
isSingular (Weight,RootSystem) := (l,R) -> (
    any(1..rank(R), s -> eval(R,l,s) == 0)
    );

fromPartitions = method();
fromPartitions List := v -> (
    n := length(v) -1;
    R := rootSystemA(n);
    l := for i from 0 to n -1 list (
	v#i - v#(i+1) 
	);
    weight(R,l)
    )

toPartitions = method();
toPartitions Weight := v -> (
    v = entries v;
    n := length(v);
    s := sum  v;
    l := {s};
    for i from 0 to n -1 do (
	l = l | {s - v_i};
	s = s - v#i;
	);
    l
    )


---------------------- rank and dimension ------------------------------

weylFormula = method(TypicalValue => ZZ);
weylFormula (Weight, RootSystem) := (l,R) -> (
    ro := halfSumOfRoots(R);
    num := apply(toList(positiveRoots(R)), s -> eval(R,l+ro,s));
    den := apply(toList(positiveRoots(R)), s -> eval(R,ro,s));
    lift(product(num)/product(den),ZZ)
    );

dim HomogeneousVariety := X -> (
    if X#"dimVariety" =!= null then return X#"dimVariety";
    P := X#"parabolicSubgroup";
    R := X#"rootSystem";
    RP := rootSystem(R,P#"parabolic");
    X#"dimVariety" = coxeterLength(longestWeylGroupElement(R)) - coxeterLength(longestWeylGroupElement(RP))
    );


dim EmbeddedVariety := Y -> (
    if Y#?"dimVariety" then return Y#"dimVariety";
    X := Y#"ambientSpace";
    F := Y#"normalBundle";
    Y#"dimVariety" = dim X - rank F
    )

rank HomogeneousVectorBundle := E -> (
    if E#"rank" =!= null then return E#"rank";
    X := E#"underlyingVariety";
    RP := E#"rootSystem";
    T := E#"parabolicWeights";
    m := E#"multiplicities";
    s := 0;
    for i from 0 to #T-1 do s = s + m#i * weylFormula(T#i,RP);
    E#"rank" = s
    );


rank FiltrationBundle := E -> (
    if E#"rank" =!= null then return E#"rank";
    F := E#"factors";
    r := apply(toList F, f -> rank f);
    E#"rank" = sum r
    );


---------------------- Chern classes ---------------------------------------------------------------------------

-- chern classes are regarded as vectors in the Neron-Severi lattice of X
chern(ZZ, HomogeneousVariety) := (p,X) -> (
    if p =!= 1 then error "at the moment we compute only the first chern class";
    if X#"firstChernClass" =!= null then return X#"firstChernClass";
    P := X#"parabolicSubgroup";
    T := set (1..rank(rootSystem(P#"dynkinType"))) -  P#"parabolic";
    T = sort apply(toList T, s -> s-1);
    X#"firstChernClass" = (sum(X#"positiveRoots"))^T
    );

-- this class is not considered restricted to Y but instead lives in the Neron-Severi lattice of X
chern(ZZ, EmbeddedVariety) := (p,Y) -> (
    if p =!= 1 then error "at the moment we compute only the first chern class";
    if Y#"firstChernClass" =!= null then return Y#"firstChernClass";
    X := Y#"ambientSpace";
    F := Y#"normalBundle";
    Y#"firstChernClass" = chern(1,X) - chern(1,F)
    );


chern(ZZ, HomogeneousVectorBundle) := (p,E) -> (
    if p =!= 1 then error "at the moment we compute only the first chern class";
    if E#"totalChernClass" =!= null then return E#"totalChernClass";
    X := E#"underlyingVariety";
    P := X#"parabolicSubgroup";
    T := set (1..rank(rootSystem(P#"dynkinType"))) -  P#"parabolic";
    T = sort apply(toList T, s -> s-1);
    E#"totalChernClass" = (((determinant E)#"weights")#0)^T
    );

chern(ZZ, FiltrationBundle) := (p,E) -> (
    if p =!= 1 then error "at the moment we compute only the first chern class";
    if E#"totalChernClass" =!= null then return E#"totalChernClass";
    S := E#"factors";
    c := chern(p,S#0);
    for f in drop(S,1) do (
	c = c + chern(p,f);
	);
    E#"totalChernClass" = c
    )


--------------------------------- Ampleness -------------------------------------------------------------
isGloballyGenerated = method();
isGloballyGenerated (HomogeneousVectorBundle, HomogeneousVariety) := (E,X) -> (
    R := X#"rootSystem";
    S := E#"weights";
    all(S, l -> isDominant(l,R))
    )
    

isAmple = method ();
isAmple HomogeneousVectorBundle := L -> (
    if rank L =!= 1 then error "expected a line bundle";
    X := L#"underlyingVariety";
    R := X#"rootSystem";
    P := X#"parabolicSubgroup";
    l := (L#"weights")#0;
    all(toList(X#"positiveRoots"), s -> eval(R,l,s) > 0)
    )




----------------------- Fibrati notevoli ------------------------------------------------

plusRhoNorm = (R,w) -> (
    rho := halfSumOfRoots(R);
    scalarProduct(R,w+rho,w+rho)
    )

-- S: a list of weights of R
-- returns a list of the weights w in S sorted by decreasing value
-- of the squared norm of w+half the sum of positive roots of R
sortLayer = (R,S) -> (
    --pair the norm with the index of the weight in X
    --so I can distinguish two weights with the same norm
    Y := apply(#S,i->(plusRhoNorm(R,S_i),i));
    --rsort works lexicographically so the pairs are sorted
    --by decreasing values of the first entry i.e. the norm
    Y = rsort Y;
    --get the weights in X using the order from the second entry of Y
    apply(Y,i->S_(last i))
    )

adjointRepresentation = method();
adjointRepresentation RootSystem := R -> first sortLayer (R,toList positiveRoots R)

leqComponentwise = method();
leqComponentwise (Vector,Vector) := (u, v) -> (
    u = entries u;
    v = entries v;
    if #u =!= #v then error "Vectors must have the same length";
    all(#u, i -> u_i <= v_i)
    )


maximalVectors = method();
maximalVectors List := L -> (
    select(L, v -> not any(L, w -> (v =!= w and leqComponentwise(v, w))))
    )


homogeneousTangentBundle = method();
homogeneousTangentBundle HomogeneousVariety := X -> (
    R := X#"rootSystem";
    P := X#"parabolicSubgroup";
    P0 := P#"parabolic";
    n := rank R;
    LX := sort toList (set toList(1..n)) - P0; -- set of marked nodes
    LX' := LX/(i -> i-1);
    G := sortLayer(R,toList positiveRoots R); -- list of positive roots (sorted)
    M := transpose inverse promote(cartanMatrix(R),QQ); -- base change from fund weights to simple roots
    Groots := G/(g -> M*g); -- positive roots written as combinations of simple roots
    G1 := for g in Groots list (
	if g^LX' =!= promote(vector toList (#LX:0),QQ) then (g^LX',g)
	);
    -- erase roots which have zero components on simple roots associated to the marked nodes
    G1 = select(G1,g -> g =!= null);
    packRoots := new MutableHashTable; 
    for c in G1 do (
	if packRoots#?(c#0) then packRoots#(c#0) = packRoots#(c#0) | {(c#1)}
	else packRoots#(c#0) = {(c#1)};
	);
    -- group the roots with respect to their components on the marked nodes
    packRoots = new HashTable from packRoots;
    -- for each group select the maximal root (lexicographic order)
    maxRoots := for c in keys packRoots list (c, (maximalVectors (packRoots#c))#0);
    -- go back to the fundamental weights basis
    toWeights := maxRoots /(s->  (s#0,(transpose cartanMatrix(R))*(s#1))); 
    toWeights = toWeights/(s-> (s#0,weight(R,(entries s#1)/(i-> lift(i,ZZ)))));
    -- group the weights with respect to the sum of their components on the marked nodes (still with respect to the roots basis)
    totalSort := new MutableHashTable;
    for s in toWeights do (
	t := sum entries s#0;
	if totalSort#?t then totalSort#t = totalSort#t | {s#1}
	else totalSort#t = {s#1};
	);
    totalSort = new HashTable from totalSort;
    -- sort by the increasing values of the sums
    -- the ones with the same sum will be in direct sum
    listOfFactors := for i in rsort keys totalSort list totalSort#i; 
    listOfFactors = apply(listOfFactors, l -> homogeneousVectorBundle(l,toList apply(#l, i -> 1),X));
    if #listOfFactors == 1 then (
	output := listOfFactors#0;
	)
    else output = filtrationBundle (listOfFactors,X);
    --if chern(1,output) =!= chern(1,X) then << "wrong chern1" << endl;
    --if dim X =!= rank output then << "wrong rank" << endl;
    output
    )



homogeneousCotangentBundle = method();
homogeneousCotangentBundle HomogeneousVariety := X -> (
    if X#"cotangent" =!= null then return X#"cotangent";
    R := X#"rootSystem";
    P := X#"parabolicSubgroup";
    l := sort toList set toList(1..rank(R)) -  P#"parabolic"; 
    if ((dynkinType R)#0)#0 == "A" then (
	bundleR := bundles X;
	F := {};
	if #l == 1 then return X#"cotangent" = tensorProduct(bundleR#0, dual bundleR#1);
	for i from 1 to #bundleR -1 do (
	    for j from 0 to i-1 do (
		F = F | {tensorProduct(bundleR#(i-1-j), dual bundleR#i)};
		);
	    );
	return X#"cotangent" = filtrationBundle(F,X);
	)
    else if ((dynkinType(R))#0)#0 == "B" and l == {1} then (
	return X#"cotangent" = dual homogeneousTangentBundle X;
	)
    else if ((dynkinType(R))#0)#0 == "C" and l == {rank R} then (
	return X#"cotangent" = homogeneousVectorBundle({weight(R,flatten {toList((rank R -2) :0),2,-2})},{1},X);
	)
    else if ((dynkinType(R))#0)#0 == "D" and l == {1} then (
	return X#"cotangent" = homogeneousVectorBundle({weight(R,flatten {-2,1,toList((rank R -2) :0)})},{1},X);
	) 
    else if ((dynkinType(R))#0)#0 == "D" and l == {rank R} then (
	return X#"cotangent" = homogeneousVectorBundle({weight(R,flatten {toList((rank R -3) :0),1,0,-2})},{1},X);
	)
     else if ((dynkinType(R))#0)#0 == "E" and ((dynkinType(R))#0)#1 == 6 and l == {1} then (
	return X#"cotangent" = homogeneousVectorBundle({weight(R,{-2,0,1,0,0,0})},{1},X);
	)
    else if ((dynkinType(R))#0)#0 == "E" and ((dynkinType(R))#0)#1 == 7 and l == {7} then (
	return X#"cotangent" = homogeneousVectorBundle({weight(R,{0,0,0,0,0,1,-2})},{1},X);
	) 
    else (
	return X#"cotangent" = dual homogeneousTangentBundle X;
	);
    )


structureSheaf = method();
structureSheaf HomogeneousVariety := X -> (
    R := X#"rootSystem";
    P := X#"parabolicSubgroup";
    l := weight(R,toList(rank R : 0));
    homogeneousVectorBundle({l},{1},X)
    )



------------------------------------ equalities ----------------------------------

ParabolicGroup == ParabolicGroup := (P1, P2) -> P1#"parabolic"  == P2#"parabolic" 

HomogeneousVariety == HomogeneousVariety := (X1, X2) -> X1#"rootSystem" == X2#"rootSystem" and X1#"parabolicSubgroup" == X2#"parabolicSubgroup" 

HomogeneousVectorBundle == HomogeneousVectorBundle := (E1, E2) -> (
    if not E1#"underlyingVariety" == E2#"underlyingVariety" then return false;
    if not set E1#"weights" == set E2#"weights" then return false;
    L1 := pack(2,mingle(E1#"weights",E1#"multiplicities"));
    L2 := pack(2,mingle(E2#"weights",E2#"multiplicities"));
    M1 := tally flatten for c in L1 list toList(c#1:c#0);
    M2 := tally flatten for c in L2 list toList(c#1:c#0);
    return M1 == M2;
    )
    
EmbeddedVariety == EmbeddedVariety := (Y1, Y2) -> Y1#"normalBundle" == Y2#"normalBundle"

FiltrationBundle == FiltrationBundle := (E1, E2) -> (
    if not E1#"underlyingVariety" == E2#"underlyingVariety" then return false;
    return E1#"factors" == E2#"factors";
    )
