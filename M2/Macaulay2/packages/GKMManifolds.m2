-*-------------------------------------------------------------------------------------------
   Copyright 2020 Christopher Eur and Ritvik Ramkumar.

   You may redistribute this file under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 2 of
   the License, or any later version.
-------------------------------------------------------------------------------------------*-



newPackage("GKMManifolds",
	Version => "0.1",
	Date => "August 1, 2020",
	Authors => {
	    {Name => "Chris Eur",
       	     Email => "chriseur@stanford.edu",
       	     HomePage => "https://web.stanford.edu/~chriseur"},
	    {Name => "Ritvik Ramkumar",
	     Email => "ritvik@math.berkeley.edu",
	     HomePage => "https://math.berkeley.edu/~ritvik"}
	    },
	Headline => "computations with GKM manifolds and moment graphs",
	HomePage => "https://github.com/chrisweur/GKMManifolds",
	PackageExports => {"Graphs", "Matroids", "NormalToricVarieties"},
	AuxiliaryFiles => true,
	--Keywords => {"Equivariant Cohomology", "Toric Geometry", "Flag Varieties", "Matroids"},
	DebuggingMode => false
)
export {
	"TVariety",
	"tVariety",
    	"TKClass",
	"tKClass",
	"ampleTKClass",
	"trivialTKClass",
	"TMap",
	"tMap",
	"pushforward",	
	"diagonalTMap",
	"FlagMatroid",
	"flagMatroid",
	--"tFlagVariety",
	"tFlagMap",
	"kTutte",
	"hilb",
	"tvar",
	"charts",
	"points",
	"ptsMap",
	"charRing",
	"HTpt",
	"tProjectiveSpace",
	"MomentGraph",
	"momentGraph",
	"constituents",
	"makeCharRing",
	--"makeHTpt",
	"tGeneralizedFlagVariety",
	"lieType",
	"cellOrder",
	"bruhatOrder",
	"tGeneralizedSchubertVariety",
	"tChi",
	"tOrbitClosure",
	"toFraction",
	"affineToricRing",
	"setIndicator",
	--"unastrsk",
	--"toCharRing",
	--"tHilbNumer",
	"RREFMethod",
	"latticePts"
}




------------------------------------------------------------------------------------------------
-------------------------------< internal auxiliary functions >---------------------------------
------------------------------------------------------------------------------------------------

--input: a list L = {l_1, ... , l_d} of list of integers, or
--a matrix A whose columns are l_i's, representing exponents of a monomial map
--output: a quotient ring R/I, where R has variables x_1, ... x_d of degrees l_1, ... , l_d and
--I is the toric ideal defined by the monomial map
affineToricRing = method();
affineToricRing(List) := QuotientRing => L -> (
    d := #L;
    x := symbol x;
    R := QQ[apply(#L, i -> x_i), Degrees => L];
    I := if #L == 0 then ideal(0_R) else toricGroebner(transpose matrix L, R);
    R/I
    )

affineToricRing(Matrix) := QuotientRing => A -> affineToricRing(entries transpose A)

--internal auxiliary function:
--default Macaulay2 can't do fraction fields for Laurent rings
--but by shifting so that we exit Laurent rings and then going back, we can.
--Here given two Laurent polynomials f,g and a ring S with Inverses=>false, outputs the ratio
--f/g of f and g by multiplying both of them with a big enough monomial to make them polynomials
--the output is the ratio in the new ring S,
--and a function goBack which puts the element in the ring of f if the denominator is a monomial
toFraction = method();
toFraction(RingElement,RingElement,Ring) := (f,g,S) -> (
    R := ring f;
    if not R === ring g then error " the two polynomials live in different rings ";
    if not (#gens R) == (#gens S) then error " the temporary ring is no good ";
    Exps := -((transpose (exponents f | exponents g))/min);
    mult := product(#Exps, i -> R_i^(Exps_i));
    numer := sub(mult*f, apply(#(gens R), i -> R_i=>S_i));
    denom := sub(mult*g, apply(#(gens R), i -> R_i=>S_i));
    rat := numer / denom;
    goBack := ratFct -> (
	N := numerator ratFct;
	D := denominator ratFct;
	if not #(terms D) == 1 then error "denominator not a monomial";
	sub(N, apply(#(gens S), i -> S_i=>R_i))*(sub(D, apply(#(gens S), i -> S_i=>R_i)))^(-1)
	);
    {rat, goBack}
)

--auxiliary function:
--makes a polynomial ring with n variables, representing T-equivariant cohomology 
--ring of a point where T =  = (kk^*)^n.
makeHTpt = method()
makeHTpt(ZZ) := Ring => n -> (
    t := symbol t;
    QQ[t_0..t_(n-1)]
    )


-----------------------------------------------------------------------------------------------
--------------------------------------< moment graphs >----------------------------------------
-----------------------------------------------------------------------------------------------

--a MomentGraph G is a MutableHashTable with the minimal data of:
--G.vertices : a list representing vertices of the graph G
--G.edges : a hash table whose keys are pairs {p,q} of vertices and values v are directed weights
-- of the edge  p --> q where v is a list of numbers such that matrix{v} * basis(1,G.HTpt)
-- gives the weight
--G.HTpt : a polynomial ring, represent the T-equivariant cohomology ring of a point


MomentGraph = new Type of HashTable

MomentGraph.synonym = "moment graph"

globalAssignment MomentGraph
net MomentGraph := G -> net ofClass class G | " on " | toString(#G.vertices) | " vertices with " | toString(#G.edges) | " edges "

--given a list V representing the vertices, E a hash table of edges, and H a ring for HTpt
--outputs a moment graph with such data
momentGraph = method()
momentGraph(List,HashTable,Ring) := MomentGraph => (V,E,H) -> (
    new MomentGraph from {
	symbol vertices => V,
	symbol edges => E,
	symbol HTpt => H,
	cache => new CacheTable
	}
    )


--outputs the graph underlying a moment graph G
underlyingGraph(MomentGraph) := Graph => G -> graph(G.vertices, keys G.edges, EntryMode => "edges")


--If a moment graph G comes from a possibly singular GKM variety with a T-invariant
-- Whitney stratification consisting of affine spaces,
--the vertices of G correspond to each strata,
--and are ordered v1 <= v2 if the closure of stratum of v2 is contained in that of v1.
cellOrder = method()
cellOrder(MomentGraph,Poset) := (G,P) -> (
    if G.cache.?cellOrder then (
	print "warning: overwriting a previously defined cell order on this moment graph "
	);
    if G.vertices =!= P.GroundSet then (
	error "the ground set of the poset is not the vertices of this moment graph "
	);
    G.cache.cellOrder = P;
    )


cellOrder(MomentGraph) := Poset => G -> (
    if G.cache.?cellOrder then G.cache.cellOrder
    else error " no cell order defined on this moment graph"
    )

--------------------------------------< T-varieties >-------------------------------------------


--for setting up a character ring: given n, outputs the character ring of T = (kk^*)^n
makeCharRing = method();
makeCharRing(ZZ) := Ring => n -> (
    T := symbol T;
    ZZ[T_0..T_(n-1), MonomialOrder => GLex, Inverses=>true]
)


--a TVariety X is a HashTable with minimal data of:
--X.points : a list representing torus-invariant points
--X.charRing : a ring representing the character ring of the torus
--additionally, a TVariety X can have:
--X.charts : a hash table whose keys are X.points and values are (negative of) the characters 
--of the affine chart at the point
--X.momentGraph : a MomentGraph which is the moment graph of the T-variety
TVariety = new Type of MutableHashTable

TVariety.synonym = "T-variety"

globalAssignment TVariety
net TVariety := X -> net ofClass class X | " with an action of a " | toString(#gens X.charRing) | "-dimensional torus"


tVariety = method()

--a list L representing the torus-fixed points, and a character ring R defines a TVariety
tVariety(List, Ring) := TVariety => (L,R) -> (
    new TVariety from {
	symbol points => L,
	symbol charRing => R,
	cache => new CacheTable
	}
    )


--tVariety created from G, a moment graph, and a character ring R compatible with G.HTpt
tVariety(MomentGraph,Ring) := TVariety => (G,R) -> (
    if not #(gens R) == #(gens G.HTpt) then (
	error "HTpt not compatible with the character ring"
	);
    new TVariety from {
	symbol points => G.vertices,
	symbol charRing => R,
	symbol momentGraph => G,
	cache => new CacheTable
	}
    )


--tVariety created from a moment graph G
--the character ring is created from G.HTpt
tVariety(MomentGraph) := TVariety => G -> (
    R := makeCharRing numgens (G.HTpt);
    tVariety(G,R)
    )


--returns the moment graph of the T-variety X if it is defined
momentGraph(TVariety) := MomentGraph => X -> (
    if X.?momentGraph then X.momentGraph
    else error "no moment graph defined for this T-variety"
    )


--given TVariety X with X.momentGraph undefined, sets a moment graph G to be the
--moment graph of X, granted that the two sanity checks (charRing dimension, fixed points) work out
momentGraph(TVariety,MomentGraph) := (X,G) -> (
    if X.?momentGraph then (
	print " warning: overwriting a previously defined moment graph on this T-variety "
	);
    if #(gens X.charRing) != #(gens G.HTpt) then (
	error "HTpt not compatible with the character ring"
	);
    if not G.vertices === X.points then (
	error "the torus-fixed points are not compatible"
	);
    X.momentGraph = G;
    )


--tVariety created from list L of points and corresponding list M of their charts' characters,
--and R the character ring of the torus
tVariety(List,List,Ring) := TVariety => (L,M,R) -> (
    new TVariety from {
	symbol points => L,
	symbol charts => hashTable apply(#L, i -> (L_i,M_i)),
	symbol charRing => R,
	cache => new CacheTable 
    	}
    )

--given a TVariety X, whose X.charts have not been defined yet,
--and a list L = {l_1, ... , l_m} where l_i is a list of characters of the T-action
--at the affine chart around p_i where X.points = {p_1, ... , p_m}
--defines X.charts to be the hash table whose keys are X.points and values the characters
charts = method()

charts(TVariety) := X -> (
    if X.?charts then X.charts
    else error "no charts defined for this T-variety"
    )

charts(TVariety,List) := (X,L) -> (
    if X.?charts then (
	print " warning: overwriting previously defined charts on this T-variety " 
	);
    if not #X.points == #L then (
	error "number of charts in the list not equal to number of points"
	);
    X.charts = hashTable apply(#X.points, i -> (X.points_i,L_i))
    )



--internal auxiliary function: 
--input: a Laurent polynomial f and a TVariety X
--output: a Laurent polynomial f considered as an element of X.charRing
toCharRing = (X,f) -> (
    if f == 1 then return 1_(X.charRing);
    R := ring f; n := #(gens R);
    if not #(gens X.charRing) == n then
        error "rings have different number of variables";
    sub(f,apply(n, i -> R_i=>X.charRing_i))    
)


--projective space PP^n as a T=(k^*)^(n+1) variety, where (t_0, ... , t_n) acts on the
--coordinates [x_0, ... , x_n] by [t_0^(-1)x_0, ... , t_n^(-1)x_n]
--input: an integer n and the character ring R of T
tProjectiveSpace = method()
tProjectiveSpace(ZZ,Ring) := TVariety => (n,R) -> (
    V := apply(n+1, i -> set {i});
    E := hashTable apply(subsets(V,2), i -> 
	({i_0, i_1}, setIndicator(i_1,n+1) - setIndicator(i_0,n+1))
	);
    t := symbol t; H := QQ[t_0..t_n];
    G := momentGraph(V,E,H);
    X := tVariety(G,R);
    L := apply(V, v -> (select(V, w -> w =!= v))/(w -> setIndicator(w,n+1) - setIndicator(v,n+1)));
    charts(X,L);
    X.cache.ampleTKClass = tKClass(X, (X.points)/(i -> R_(setIndicator(i,n+1))));
    X
    )

--when only given an integer n
tProjectiveSpace(ZZ) := TVariety => n -> (
    R := makeCharRing(n+1);
    tProjectiveSpace(n,R)
    )


--product of two TVarieties X,Y with an action of a common torus T
--the product is endowed with the diagonal action of T
TVariety ** TVariety := TVariety => (X,Y) -> (
    R := X.charRing;
    if not R === Y.charRing then error "character rings need be same";
    L := elements ((set X.points) ** (set Y.points));
    XxY := tVariety(L,R);
    if X.?charts and Y.?charts then (
	M := apply(L, l -> (X.charts)#(first l) | (Y.charts)#(last l));
	charts(XxY,M);
	);
    XxY
    --TODO: if both has moment graphs, then should also compute the moment graph
)

--the product function above does NOT automatically compute the product moment graph
MomentGraph ** MomentGraph := MomentGraph => (G1,G2) -> (
    if numgens G1.HTpt =!= numgens G2.HTpt then (
	error " the two HTpt are different "
	);
    H := G1.HTpt;
    V := (elements((set G1.vertices) ** (set G2.vertices)))/splice;
    E1 := flatten apply(G2.vertices, q -> (keys G1.edges)/(k -> ({(k_0,q), (k_1,q)}, G1.edges#k)));
    E2 := flatten apply(G1.vertices, p -> (keys G2.edges)/(k -> ({(p,k_0), (p,k_1)}, G2.edges#k)));
    mixedCand := select(elements ((set keys G1.edges) ** (set keys G2.edges)), i -> 
	rank matrix {G1.edges#(i_0), G2.edges#(i_1)} < 2);
    mixedE := apply(mixedCand, i -> (
	    char1 := G1.edges#(i_0);
	    char2 := G2.edges#(i_1);
	    primitive := char1 // (gcd char1);
	    firsts := {first select(char1, i -> i != 0), first select(char2, i -> i != 0)};
	    g := gcd (firsts/(i -> i / first select(primitive, i -> i!= 0)));
	    trueChar := g * primitive;
	    if firsts_0 * firsts_1 > 0 then 
	    ({(first i_0, first i_1), (last i_0, last i_1)}, trueChar)
	    else ({(first i_0, last i_1), (last i_0, first i_1)}, trueChar)
	    )
	);
    momentGraph(V, hashTable (E1 | E2 | mixedE), H)	    
    )




--Given a list L of characters of torus of X, outputs the numerator in the Hilbert series
--of the associated monomial subalgebra
tHilbNumer = method();
tHilbNumer(TVariety,List) := RingElement => (X,L) -> (
    if L == {} then return toCharRing(X,1);
    A := affineToricRing L;
    I := ideal A;
    f := value numerator hilbertSeries I;
    toCharRing(X,f)
)

cellOrder(TVariety) := Poset => X -> (
    G := momentGraph X;
    cellOrder G
    )
    



--a TKClass C has data of:
--C.tvar = a TVariety X that the T-equiv K-class C lives on
--C.hilb = a hash table whose keys are points of X and values are the Hilbert series at the point
TKClass = new Type of HashTable

TKClass.synonym = "T-equivariant K-class"

globalAssignment TVariety
net TKClass := C -> net ofClass class C | " on a T-variety "


--a TKClass is given by a TVariety X and a list L of Laurent polynomials for each torus-invariant
--points in X, (listed in the order of X.points)
tKClass = method();
tKClass(TVariety,List) := TKClass => (X,L) -> (
    K := X.points;
    if any(L, l -> ring l =!= X.charRing) then L = L/(f -> toCharRing(X,f));
    new TKClass from {
	symbol tvar => X,
	symbol hilb => hashTable apply(#K, i -> (K_i,L_i))
    }
)

tVariety(TKClass) := TVariety => C -> C.tvar


--tests whether a TKClass satisfies the edge-compatibility criterion
isWellDefined(TKClass) := Boolean => C -> (
    X := C.tvar;
    if not X.?momentGraph then (
	error "a moment graph needs to be defined for this T-variety "
	);
    G := X.momentGraph;
    R := X.charRing;
    x := symbol x;
    S := QQ[x_0..x_(#gens R - 1)];
    badEdges := select(keys G.edges, e -> (
	    pt1 := first e;
	    pt2 := last e;
	    lambda := G.edges#e;
	    ratio := toFraction(C.hilb#pt1 - C.hilb#pt2,  1 - R_lambda, S);
	    #(terms(QQ,denominator first ratio)) != 1
	    )
	);
    if #badEdges != 0 then (
	<< "incompatible edges " | toString(badEdges) <<
	return false
	);
    true
    )


--the trivial TKClass (where X^T --> R is a constant 1 function) of a TVariety X
--in other words, the TKClass of the structure sheaf of X
trivialTKClass = method();
trivialTKClass(TVariety) := TKClass => X -> (
    R := X.charRing;
    L := apply(X.points, p -> 1_R);
    tKClass(X,L)
)


--multiplying two TKClasses
TKClass * TKClass := (C1,C2) -> (
    X1 := C1.tvar; X2 := C2.tvar;
    if not X1 === X2 then error "the T-varieties are different";
    L := apply(X1.points, p -> C1.hilb#p * C2.hilb#p);
    tKClass(X1,L)
)

TKClass ^ ZZ := (C,d) -> (
    if d > 0 then return product(d, i -> C)
    else if d == 0 then return trivialTKClass C.tvar
    else if d < 0 then (
	if not all(values C.hilb, f -> 1 == #terms f) then (
	    error "unable to compute the inverse of this K-class"
	    );
	L := apply(C.tvar.points, p -> (C.hilb#p)^(-1));
	Cneg := tKClass(C.tvar,L);
	return product(-d, i -> Cneg)
	)
    )


--adding two TKClasses
TKClass + TKClass := (C1,C2) -> (
    X1 := C1.tvar; X2 := C2.tvar;
    if not X1 === X2 then error "the T-varieties are different";
    L := apply(X1.points, p -> C1.hilb#p + C2.hilb#p);
    tKClass(X1,L)
)

--if a TVariety X has a distinguished O(1) T-equivariant line bundle, then returns its TKClass
ampleTKClass = method();
ampleTKClass(TVariety) := TKClass => X -> (
    if X.cache.?ampleTKClass then return X.cache.ampleTKClass
    else error "no distinguished ample line bundle on this T-variety";
)


--if a TVariety X does not have an ample class defined, and C is a TKClass on X, 
--sets the ampleTKClass of X to be C
ampleTKClass(TVariety,TKClass) := (X,C) -> (
    if X.cache?ampleTKClass then (
	print " warning: overwriting a previously defined ampleTKClasson this T-variety "
	);
    if C.tvar =!= X then error "not a TKClass of this T-variety";
    X.cache.ampleTKClass = C
    )


--a TMap f has data of:
--f.source: the source tVariety X,
--f.target: the target tVariety Y
--f.ptsMap: a hash table whose keys are X.points and values are the point Y.points that it maps to 
TMap = new Type of HashTable

TMap.synonym = "T-equivariant map"

globalAssignment TMap
net TMap := phi -> net ofClass class phi | " of T-varieties "



--a TMap is given by providing the source X and target Y and a list L of pairs (X point, Y point)
tMap = method();
tMap(TVariety,TVariety,List) := TMap => (X,Y,L) -> (
    if not X.charRing === Y.charRing then error "character rings need be same";
    new TMap from {
	symbol source => X,
	symbol target => Y,
	symbol ptsMap => hashTable L,
	cache => new CacheTable	
    }
)

--pullback map of TKClasses given a TMap
--pullback = method(); --from version 1.16 onward "pullback" is a built-in global variable
pullback(TMap) := FunctionClosure => phi -> (
    X := phi.source; 
    Y := phi.target;
    if not phi.cache.?pullback then phi.cache.pullback = C -> (
	if C.tvar =!= Y then (
	    error "the TKClass to pullback is not a TKClass of the target T-variety"
	    );
	L := apply(X.points, p -> C.hilb#((phi.ptsMap)#p));
	tKClass(X,L)
    );
    phi.cache.pullback
)

--given a TMap, computes the pushforward map as a function
pushforward = method();
pushforward(TMap) := FunctionClosure => phi -> (
    if phi.cache.?pushforward then return phi.cache.pushforward;
    X := phi.source;
    Y := phi.target;
    R := X.charRing;
    Ydenoms := hashTable apply(Y.points, q -> (q, product((Y.charts)#q, l -> 1-R_l)));
    Xdenoms := hashTable apply(X.points, p -> (p, product((X.charts)#p, l -> 1-R_l)));
    x := symbol x;
    S := QQ[x_0..x_(#(gens R)-1)];
    pushforwardFct := C -> (
	if not C.tvar === X then error " TKClass not of the source T-variety ";
	L := apply(Y.points, q -> (
	    	preimages := select(X.points, p -> (phi.ptsMap)#p === q);
	    	if #preimages == 0 then return 0_R;
	    	Ydenom := Ydenoms#q;
	    	toSum := apply(preimages, p -> toFraction(Ydenom * C.hilb#p, Xdenoms#p, S));
	    	val := (last first toSum) sum(toSum/first)
	    	)
	    );
	tKClass(Y,L)
    	);
    phi.cache.pushforward = pushforwardFct
    )


--given a TVariety X outputs the diagonal map X -> X x X
diagonalTMap = method();
diagonalTMap(TVariety) := TMap => X -> (
    Y := X ** X;
    ptPairs := apply(X.points, p -> (
	Q := first select(Y.points, q -> (first q) === (last q) and (first q) === p);
	(p,Q)
	)
    );
    tMap(X,Y,ptPairs)
)



--given two maps, takes Cartesian product of them
TMap ** TMap := TMap => (phi,psi) -> (
    X := phi.source ** psi.source;
    Y := phi.target ** psi.target;
    ptPairs := apply(X.points, p -> (
	Q := ((phi.ptsMap)#(first p),(psi.ptsMap)#(last p));
	(p,Q)
	)
    );
    tMap(X,Y,ptPairs)
)


--composition f o g of two TMaps g: X --> Y, f: Y --> Z;
compose(TMap,TMap) := TMap => (f,g) -> (
    Y1 := g.target;
    Y2 := f.source;
    if not Y1.points === Y2.points then (
	error " check sources and targets of the T-maps "
	);
    Z := f.target; X := g.source;
    ptPairs := apply(X.points, p -> (p, f.ptsMap#(g.ptsMap#p)));
    tMap(X,Z,ptPairs)
    )

--the T-equivariant Euler characteristic of a TKClass, i.e. the Lefschetz trace,
--i.e. the pushforward to a point
tChi = method()
tChi(TKClass) := RingElement => C -> (
    X := C.tvar;
    R := X.charRing;
    pt := symbol pt;
    tPoint := tVariety({pt},{{}},R);
    tpt := first tPoint.points;
    structureMap := tMap(X,tPoint,apply(X.points, i -> (i,tpt)));
    pushC := (pushforward(structureMap))C;
    pushC.hilb#tpt
    )


-----------------------------------------------------------------------------------------------
---------------------------------< normal toric varieties >------------------------------------

--given a NormalToricVariety X, outputs a T-variety Y with the data
--Y.points are lists where each list is the indices of the rays defining the T-fixed point
--X need be smooth, non-degenerate, and must have fixed-points.

--WARNING: as usual, if an affine chart AA^m has characters a_1, ... , a_m, the torus (kk^*)^n
--acts on AA^m by t * (x_1, ... , x_m) = (t^(-a_1)x_1, ... , t^(-a_m)x_m)
--in other words, to agree with the usual normal toric variety literature,
--one should consider OUTER normal cones of cones, not inner normal cones
tVariety(NormalToricVariety,Ring) := TVariety => (X,R) -> (
    if not isSmooth X then (
	error " the normal toric variety is not smooth "
	);
    n := dim X;
    if not n == #(gens R) then (
	error " the character ring is incompatible with the torus of the toric variety "
	);
    pts := select(max X, i -> #i == n);
    if pts == {} then (
	error " no torus-fixed points on this normal toric variety "
	);
    rys := rays X;
    chrts := apply(pts, p -> - entries transpose inverse matrix rys_p);
    Y := tVariety(pts,chrts,R);
    GEdgePairs := select(subsets(pts,2), i -> #(unique flatten i) == #(first i) + 1);
    GEdges := hashTable apply(GEdgePairs, i -> (
	    p := first i;
	    q := last i;
	    (i, first select(Y.charts#p, i -> member(-i,Y.charts#q)))
	    )
	);
    momentGraph(Y,momentGraph(Y.points, GEdges, makeHTpt n));
    Y.cache.normalToricVariety = X;
    Y
    )


tVariety(NormalToricVariety) := TVariety => X -> tVariety(X,makeCharRing dim X)



--given a T-variety X, returns a normal toric variety if X was constructed from one
normalToricVariety(TVariety) := NormalToricVariety => opts -> X -> (
    if X.cache.?normalToricVariety then X.cache.normalToricVariety
    else error " no normal toric variety structure on this T-variety "
    )


--TODO: the following unfunction is untested

--given a ToricDivisor D on a TVariety Y whose normal toric variety is X,
--outputs the TKClass of D on Y
tKClass(TVariety,ToricDivisor) := TKClass => (Y,D) -> (
    if not normalToricVariety D === normalToricVariety Y then (
	error " the toric divisor is not of this T-variety "
	);
    R := Y.charRing;
    hlbs := apply(Y.points, p -> (
	    l := flatten entries (matrix{(entries D)_p} * - matrix Y.charts#p);
	    R_l
	    )
	);
    tKClass(Y,hlbs)
    )


-------------------------------------------------------------------------------------------------
--------------------------------< generalized flag varieties >-----------------------------------
-------------------------------------------------------------------------------------------------


---------< internal auxiliary functions: signed subsets, flags, and indicator vectors >----------

--makes a string "n*" given a positive integer n
astrsk = n -> toString(n) | "*"

--given either integer n or "n*", outputs n
unastrsk = i -> if instance(i,String) then value first i else if instance(i,ZZ) then i else error

--given a set s consisting of elements e where e is an integer i or "i*" with 0 <= i <= n-1
--outputs the signed indicator vector
setIndicator = method()
setIndicator(Set,ZZ) := List => (s,n) -> (
    if (elements s)/unastrsk != unique (elements s/unastrsk) then (
	error " the signed subset is not admissible "
	);
    apply(n, i -> (
	    if member(i,s) then 1
    	    else if member(astrsk i, s) then -1
    	    else 0
    	    )
	)
    )

--given a list of integers of length n, outputs the corresponding flag of signed subsets
vecToFlag = v -> sort apply(max (v/abs), i -> (
	supp := positions(v, j -> abs j > i);
	set apply(supp, j -> if v_j > 0 then j else astrsk j)
	)
    )

--likewise, but applying "unique"
redVecToFlag = v -> unique vecToFlag v

flagToVec = (L,n) -> sum(L, s -> setIndicator(s,n))


--all signed permutations of a list
--allows for repeated entries, just like the ordinary "permutations" function
signedPermutations = L -> (
    if #L == 1 then {{first L}, {- first L}} 
    else flatten apply(#L, i -> flatten apply(signedPermutations(drop(L,{i,i})), j -> {j | {L_i}, j | {-L_i}})
	)
    )

--all even signed permutations (i.e. signed permutations with same parity of minus signs)
--again, allows for repeated entries, like ordinary "permutations" and "signedPermutations"
evenSignedPermutations = L -> (
    SP := signedPermutations L;
    if any(L, i -> i == 0) then SP
    else (
	parity := mod(#select(L, i -> i < 0),2);
	select(SP, l -> mod(#select(l, i -> i < 0),2) == parity)
	)
    )
 
---------------------< internal auxiliary functions: roots and weights >-------------------------


--internal auxiliary functions for tGeneralizedFlagVariety
--given a classical Lie type LT, the root system dimension d, and list L = {l_1, ... , l_m},
--outputs the highest weight vector w_(l_1) + ... + w_(l_m) where w_i's are fundamental weights
--(w_d in the orthogonal case ("B" and "D"), strictly speaking we are using
--twice of the fundamental weight)
highestWeight := (LT,d,L) -> (
    if LT == "A" then n := d+1 else n = d;
    if member(LT,{"A","C"}) or (LT == "B" and not member(d,L)) then
    sum(L, i -> setIndicator(set apply(i, j -> j),n))
    else if LT == "B" and member(d,L) then (
	hw := sum(L, i -> (
		if i < d then setIndicator(set apply(i, j -> j),n)
		else if i == d then 1/2 * setIndicator(set apply(i, j -> j),n)
		)
	    );
	if all(hw, i -> liftable(i,ZZ)) then hw/(i -> lift(i,ZZ))
	else error " spin groups not implemented yet "
	)
    else if LT == "D" then (
	hw = sum(L, i -> (
		if i < d-1 then setIndicator(set apply(i, j -> j),n)
		else if i == d then 1/2 * setIndicator(set apply(d, j -> j),n)
		else if i == d-1 then 1/2 * setIndicator(set (apply(d-1, j -> j) | {astrsk(d-1)}),n)
		)
	    );
	if all(hw, i -> liftable(i,ZZ)) then hw/(i -> lift(i,ZZ))
	else error " spin groups not implemented yet "
	)
    )

--the Weyl group orbit of the highest weight of (LT,d,L) with notation as above
extremalWeights := (LT,d,L) -> (
    lambda := highestWeight(LT,d,L);
    if LT == "A" then unique (permutations lambda)
    else if member(LT,{"B","C"}) then unique (signedPermutations lambda)
    else if LT == "D" then unique (evenSignedPermutations lambda)
    )

--given a classical Lie type LT, outputs a function f where, given a list v representing
--a nonzero vector, f outputs {v / (abs gcd v)}, that is, a root if v is a (positive)
-- multiple of a root, and f(v) = {} otherwise
toRoot := LT -> (
    if member(LT,{"A","D"}) then (
	f := v -> (
	    non0s := select(v, i -> i != 0);
	    if #non0s == 2 and abs first non0s == abs last non0s then {v // (abs gcd v)} else {}
	    )
	)
    else if LT == "B" then (
	f = v -> (
	    non0s := select(v, i -> i != 0);
	    if #non0s == 1 or (#non0s == 2 and abs first non0s == abs last non0s) then
	    {v // (abs gcd v)}
	    else {}
	    )
	)
    else if LT == "C" then (
	f = v -> (
	    non0s := select(v, i -> i != 0);
	    if #non0s == 1 then {2 * (v // (abs gcd v))}
	    else if (#non0s == 2 and abs first non0s == abs last non0s) then {v // (abs gcd v)}
	    else {}
	    )
	)
    )


--input: LT, d, L
--a string LT which is "A", "B", "C", or "D", representing the classical Lie types,
--(we follow the convention in Example 3.4 of https://arxiv.org/pdf/1904.11029.pdf
--for the ordering of simple roots and fundamental weights)
--an integer d, representing the dimension of the root system,
--(we do *not* check the low d isogenies,
--for example, A_3 = D_3 but the code will not recognized this and output different things)
--a list L of integers each ranging from 1 to d.
--output:
--a generalized flag variety G/P as a T-variety with charts,
--where G is the classical Lie group of type LT with an action of a d-torus T,
--(except in the case of "A", we let T be the (d+1)-torus by *not* projectivizing the torus)
--and P is the parabolic subgroup corresponding to W_([d]\L)
--that is, subgroup of the Weyl group generated by elements *not* in L
--the ample class is defined to be embedding of G/P coming from the representation of G
--whose highest weight vector equals sum(L, i -> w_i) where w_i is the i-th fundamental weight
--(except in the orthogonal case "B,D", the "half"-fundamental weights are doubled
--that is, we are not considering the Spin representations but representations of SO(n).
tGeneralizedFlagVariety = method()
tGeneralizedFlagVariety(String,ZZ,List) := TVariety => (LT,d,L) -> (
    if not member(LT,{"A","B","C","D"}) then (
	error " the first entry must be one of \"A\", \"B\", \"C\", or \"D\" "
	);
    if d <= 0 then (
	error " the second entry (dimension) must be a positive integer "
	);
    if not all(L, i -> 1 <= i and i <= d) then (
	error " indices for the weights must be between 1 and the dimension (inclusive) "
	);
    toRootFunc := toRoot LT;
    R := if LT == "A" then makeCharRing(d+1) else makeCharRing d;
    extrWts := extremalWeights(LT,d,L);
    GEdgesDouble := new MutableHashTable;
    chrts := apply(extrWts, v -> (
	    lambdas := {};
	    apply(delete(v,extrWts), w -> (
		    lambda := toRootFunc(w-v);
		    if #lambda == 1 then (
			lambdas = lambdas | lambda;
			GEdgesDouble#({v,w}/redVecToFlag) = first lambda;
			)
		    )
		);
	    lambdas
	    )
	);
    X := tVariety(extrWts/redVecToFlag, chrts, R);
    GEdges := hashTable apply(unique ((keys GEdgesDouble)/set), i -> (
	    e := elements i;
	    (e, GEdgesDouble#e)
	    )
	);
    H := if LT == "A" then makeHTpt(d+1) else makeHTpt d;
    G := momentGraph(X.points, GEdges, H);
    momentGraph(X,G);
    X.cache.ampleTKClass = tKClass(X, apply(extrWts, i -> R_i));
    X.cache.lieType = LT;
    X
    )

--same as tGeneralizedFlagVariety but with the ring R provided for the character ring.
tGeneralizedFlagVariety(String,ZZ,List,Ring) := TVariety => (LT,d,L,R) -> (
    X := tGeneralizedFlagVariety(LT,d,L);
    S := X.charRing;
    if #(gens R) != #(gens S) then error " check character ring ";
    conversion := map(R,S,gens R);
    X.charRing = R;
    C := ampleTKClass X;
    X.cache.ampleTKClass = tKClass(X, apply(X.points, i -> conversion C.hilb#i));
    X
    )

--if a TVariety is a generalized flag variety, returns its Lie type
lieType = method()
lieType(TVariety) := String => X -> (
    if X.cache.?lieType then X.cache.lieType
    else error " no Lie Type defined for this T-variety "
    )

--given a generalized flag variety, computes & stores the bruhat order for its moment graph
--and then returns the poset
bruhatOrder = method()
bruhatOrder(TVariety) := Poset => X -> (
    if not X.cache.?lieType then (
	error " not a generalized flag variety "
	);
    if X.momentGraph.cache.?cellOrder then return X.momentGraph.cache.cellOrder;
    LT := lieType X;
    G := X.momentGraph;
    n := #(gens X.charRing);
    posWt := apply(n, i -> n - i);
    negRoots := select(values G.edges, v -> sum(n, i -> v_i * posWt_i) < 0);
    ground := G.vertices;
    rels := apply(keys G.edges, k -> if member(G.edges#k,negRoots) then k else reverse k);
    P := poset(ground,rels);
    cellOrder(X.momentGraph, P);
    P
    )

--Schubert variety of a generalized flag variety
--input: is (X,v), where X is a generalized flag variety and v is a vertex in its moment graph
--output: a TVariety representing a Schubert variety whose T-fixed points correspond to all
--vertices w in the moment graph of X where v <= w
tGeneralizedSchubertVariety = method()
tGeneralizedSchubertVariety(TVariety,Thing) := TVariety => (X,v) -> (
    G := momentGraph X;
    if not member(v,G.vertices) then (
	error " the second entry must be a vertex of the moment graph of the TVariety "
	);
    P := bruhatOrder X;
    V := principalFilter(P,v);
    E := hashTable apply(select(keys G.edges, k -> all(k, i -> member(i,V))), j -> (j,G.edges#j));
    Y := tVariety(momentGraph(V,E,G.HTpt),X.charRing);
    cellOrder(Y.momentGraph, subposet(P,V));
    Y.cache.lieType = lieType X;
    Y
    )

--------------------------------------------------------------------------------------------------
--------------------------------< T-orbit closures of points >------------------------------------

---------------------------< auxiliary functions for TOrbClosure >--------------------------------
convertToNum = (n,L) -> (
    return apply(toList L, v -> if v === unastrsk(v) then v else n + unastrsk v)
    )

revMat = M -> return matrix apply( reverse entries M, v-> reverse v)

-- Takes in a mutable matrix and outputs the RREF with the identitiy block in the beginning
rowRed = M -> (
    Mat := matrix revMat(M) ;
    return mutableMatrix revMat transpose gens gb image transpose Mat;
    )



--the T-equivariant K-class of a torus orbit closure of a point in a generalized flag variety
--input:  X a tGeneralizedFlagVariety and MatLst a list of matrices, {M1,...,Mn}, that defines a point in X.
--    	  For convenience we assume that the ranks of the M_i are distinct.    	
--output: The tKClass of the closure of the orbit of the point corresponding to {M1,...,Mn}
tOrbClosure = method()
tOrbClosure(TVariety,List) := TKClass => (X,MatLst) -> ( 
    if X.cache.?lieType then Typ := X.cache.lieType else (
	 error "T-orbit closures are only implemented for Lie types"
	 );
    R := X.charRing;
    m := numgens X.charRing;
    x := symbol x;
    S := QQ[x_0..x_(m-1)];
    QS := frac(QQ[x_0..x_(m-1), Degrees => apply(m, i -> setIndicator(set {i},m))]);
    rks :=  apply(first X.points, v -> #(elements v));
    if not all(MatLst, v -> numcols v == (
	    if Typ === "A" then m
	    else if Typ === "B" then 2*m+1
	    else 2*m
	    )
	) then error "the column size of the matrices are incorrect";
    if not rks === apply(MatLst, v -> rank v) then (
	error " the rank of the matrices are incorrect "
	);
    (if Typ === "A" then Gens := apply(gens QS, v -> v^(-1))
	else if Typ  === "C" or Typ === "D" then Gens = apply(gens QS, v -> v^(-1)) | gens QS
	else if Typ === "B" then Gens = apply(gens QS, v -> v^(-1)) | gens QS |{1}
	);
    Lst := apply(X.points, pt -> (
	    degList := {};
	    convertPt := apply(pt, v -> convertToNum(m,v));
	    if any(#rks, v -> determinant (rowRed MatLst_v)_(convertPt_v) == 0) then 0_R else (
		for j in toList(0..#rks-1) do (
		    L := (reverse convertPt)#j;
		    M := mutableMatrix sub((reverse MatLst)#j,QS);
		    for i in toList(0..#L-1) do M = columnSwap(M,i,L#i);
		    M = rowRed M;
		    for i in reverse toList(0..#L-1) do M = columnSwap(M,i,L#i);
		    for i in L do M = columnMult(M,i,0);
		    for v in toList(0..#L-1) do M = rowMult(M,v,sub(Gens_(L_v)^(-1),ring M));
		    for v in toList(0..#Gens-1) do M = columnMult(M,v,sub(Gens_v, ring M));
		    degList = degList | apply(flatten entries M, v-> degree v);
		    );
		degs := - unique delete(-infinity, degList);
		tHilbSer := hilbertSeries affineToricRing degs;
		numer := toCharRing(X,value numerator tHilbSer);
		denom := toCharRing(X,value denominator tHilbSer);
		fracVal := toFraction(numer * product(X.charts#pt, l -> (1-R_l)), denom, S);
		(last fracVal)(first fracVal)
		-* degs *-
	    	)
	    )
    	);
    tKClass(X,Lst)
    -* hashTable apply(#X.points, i -> (X.points_i, Lst_i)) --and comment this *-
    )

--same as the TOrbClosure but computed in the different way
--(without row reducing but just computing minors)
--given a TVariety X that is a generalized flag variety consisting of linear subspaces of
--dimensions rks = {r_1, ... , r_k}, and a matrix A that is k' x (appropriate numcols),
--where k' >= k, and the first r_i rows span an appropriate isotropic subspace,
--outputs the TKClass of the torus-orbit closure.
tOrbitClosure = method(Options => {RREFMethod => false})
tOrbitClosure(TVariety,Matrix) := TKClass => opts -> (X,A) -> (
    rks :=  apply(first X.points, v -> #(elements v));
    MatLst := apply(rks, i -> A^(apply(i, j -> j)));
    if opts.RREFMethod then return tOrbClosure(X,MatLst);
    if X.cache.?lieType then Typ := X.cache.lieType else (
	 error "T-orbit closures are only implemented for Lie types"
	 );
    R := X.charRing;
    n := numgens R;
    col := (
	if Typ === "A" then n
	else if Typ === "B" then 2*n+1
	else 2*n
	);
    if not numcols A == col then (
	error "the column size of the matrices are incorrect"
	);
    if not rks === apply(MatLst, v -> rank v) then (
	error " the rank of the matrices are incorrect"
	);
    nonzeroMinors := Mat -> select(subsets(numcols Mat, numrows Mat), l -> determinant Mat_l != 0);
    toWeights := l -> (
	if Typ === "A" then setIndicator(set l,n)
	else if Typ === "B" then (
	    pos := setIndicator(set select(l, i -> i < n),n);
	    neg := setIndicator(set ((select(l, i -> i > n-1 and i < 2*n))/(j -> j-n)),n);
	    pos - neg
	    )
	else (
	    post := setIndicator(set select(l, i -> i < n),n);
	    nega := setIndicator(set ((select(l, i -> i > n-1))/(j -> j-n)),n);
	    post - nega
	    )
	);
    weightLst := apply(MatLst, m -> (unique ((nonzeroMinors m)/toWeights))/(i -> R_i));
    basePolytopeWeights := (
	init := matrix{{1_R}};
	apply(weightLst, l -> init = init ** matrix{l});
	(unique flatten entries init)/(f -> first exponents f)
	);
    x := symbol x;
    S := QQ[x_0..x_(n-1)];
    Lst := apply(X.points, pt -> (
	    start := sum(pt, i -> setIndicator(i,n));
	    if not member(start,basePolytopeWeights) then return 0_R;
	    --degs := delete(apply(n, i -> 0), basePolytopeWeights/(w -> w - start));
	    --above "degs" is mathematically easier to verify but is slower
	    degs := select(X.charts#pt, i -> member(start+i,basePolytopeWeights));
	    tHilbSer := hilbertSeries affineToricRing degs;
	    numer := toCharRing(X,value numerator tHilbSer);
	    denom := toCharRing(X,value denominator tHilbSer);
	    fracVal := toFraction(numer * product(X.charts#pt, l -> (1-R_l)), denom, S);
	    (last fracVal)(first fracVal)
	    )
	);
    tKClass(X,Lst)
    )



----------------------------------------------------------------------------------------------
--------------------------------< ordinary flag varieties >-----------------------------------


--swaps the (l_0,l_1)th places in the vector v
swap = (v,l) -> apply(#v, i -> 
    if i == first l then v_(last l)
    else if i == last l then v_(first l)
    else v_i
)


--converts a pair s into a length n list where s_0 has -1 and s_1 has 1 and 0 everywhere else
swapIndicator = (s,n) -> (
    apply(n, i -> 
	if i == first s then -1
	else if i == last s then 1
	else 0
    )
)


-*-----------< subsumed in tGeneralizedFlagVariety command now >--------------
--Given a list K = {k_1, ... , k_m} and a number n defining flag variety Fl(K;n) consisting of
--flags of linear subspaces of kk^n of dimensions k_1, ... , k_m, returns the TVariety where
--the action of T = (kk^*)^n on the vector space kk^n is (t.v) = (t^-1v)
tFlagVariety = method();
tFlagVariety(List,ZZ,Ring) := TVariety => (K,n,R) -> (
    if not #(gens R) == n then << "check character ring" << return error;
    if not max K < n then << "check rank sequence K" << return error;
    E := set toList(0..(n-1));
    pts := (unique permutations sum(K/(k -> toList(k:1) | toList(n-k:0))))/vecToFlag;
    chrts := apply(pts, p -> (toList sum(p, l -> l**(E-l)))/(s -> swapIndicator(s,n)));
    X := tVariety(pts,chrts,R);
    L := apply(X.points, p -> (Exps := flagToVec(p,n); product(#Exps, i -> R_i^(Exps_i))));
    X.cache.ampleTKClass = tKClass(X,L);
    X
)


--if one does not want to make the charRing beforehand.
--NOT recommended for use...
tFlagVariety(List,ZZ) := TVariety => (K,n) -> tFlagVariety(K,n,makeCharRing n)


--Given two lists Kso, Kta (for rank sequences of source and target flag varieties) and n,
--creates the two TVarieties and makes a TMap between them (given the charRing R).
tFlagMap(List,List,ZZ,Ring) := TMap => (Kso,Kta,n,R) -> (
    Flso := tFlagVariety(Kso,n,R);
    Flta := tFlagVariety(Kta,n,R);
    ptPairs := apply(Flso.points, p -> (p,first select(Flta.points, q -> isSubset(q,p))));
    tMap(Flso,Flta,ptPairs)
)
----------------*-

--if X and Y are already tGeneralizedFlagVariety, then the following outputs the corresponding
-- tFlagMap X --> Y
tFlagMap = method();
tFlagMap(TVariety,TVariety) := TMap => (X,Y) -> (
    if not (X.cache.?lieType and Y.cache.?lieType) then (
	error "both T-varieties need have a lieType"
	);
    if not X.charRing === Y.charRing then error "character ring need be same";
    ptPairs := apply(X.points, p -> (p,first select(Y.points, q -> isSubset(q,p))));
    tMap(X,Y,ptPairs)
)


----------------------------------< ordinary flag matroids >--------------------------------------

FlagMatroid = new Type of HashTable

FlagMatroid.synonym = "flag matroid"

globalAssignment FlagMatroid
net FlagMatroid := M -> net ofClass class M | " with rank sequence " | toString((M.constituents)/rank) | " on " | toString(#M.groundSet) | " elements "


flagMatroid = method()

--Given a list L of concordant matroids (M_1, ... , M_k) returns the flag matroid object
--Convention: M_i is quotient of M_(i+1)
--Does not check concordance of matroids in the list, but checks same cardinality of ground set.
flagMatroid(List) := FlagMatroid => L -> (
    E := (first L).groundSet; n := #E;
    if any(L, m -> not #m.groundSet == n) then error "ground set not all same size";
    new FlagMatroid from {
	symbol groundSet => E,
	symbol constituents => L,
	cache => new CacheTable
    }
)

--Given l x n matrix A and a list r of ranks, outputs the associated flag matroid
flagMatroid(Matrix,List) := FlagMatroid => (A,r) -> (
    k := #r; l := numcols A;
    if r == {} or any(k-1, i -> r_i > r_(i+1)) or r_(k-1) > l then (
	error "check rank sequence"
	);
    ML := apply(k, i -> matroid A^(toList(0..(r_i-1))) );
    flagMatroid ML
)

--checks that the constituents of a flag matroid M are concordant
isWellDefined(FlagMatroid) := Boolean => M -> (
    L := M.constituents; k := #L;
    all(k-1, i -> isSubset(flats L_i, flats L_(i+1))) and all(L, isWellDefined)
)

--for a flag matroid F, outputs a list of chains of bases of the constituent matroids
bases(FlagMatroid) := List => M -> (
    if not M.cache.?bases then M.cache.bases = (
	ML := M.constituents; k := #ML;
	BL := flatten apply(k, i -> (bases ML_i)/(b -> (i,b)) );
	rel := (a,b) -> first a < first b and isSubset(last a, last b);
	P := poset(BL, rel , AntisymmetryStrategy => "none");
	(maximalChains P)/(c -> c/last)
    );
    M.cache.bases
)


--computes the lattice points of the base polytope of a flag matroid M
latticePts = method()
latticePts(FlagMatroid) := M -> (
    n := #M.groundSet;
    BL := M.constituents/bases/(B -> B/(b -> setIndicator(b,n)));
    k := #BL;
    unique apply((k:0)..toSequence(BL/(B -> #B-1)), i -> sum(k, j -> (BL_j)_(i_j)))
)


-*-----------------< not needed at this the moment >----------
--direct sum, deletion, restriction, and contraction of a flag matroid is done by doing
--respective operation on the operation on each constituents
FlagMatroid ++ FlagMatroid := FlagMatroid => (M,N) -> (
    ML := M.constituents; NL := N.constituents;
    if not #ML == #NL then << "two flag matroids have different number of constituents" << return error;
    flagMatroid apply(#ML, i -> ML_i ++ NL_i)
)

restriction(FlagMatroid,Set) := FlagMatroid => (M,S) -> (
    flagMatroid(M.constituents/(m -> restriction(m,S)))
)

restriction(FlagMatroid,List) := FlagMatroid => (M,L) -> (
    flagMatroid(M.constituents/(m -> restriction(m,L)))
)

contraction(FlagMatroid,Set) := FlagMatroids => (M,S) -> (
    flagMatroid(M.constituents/(m -> contraction(m,S)))
)

contraction(FlagMatroid,List) := FlagMatroids => (M,L) -> (
    flagMatroid(M.constituents/(m -> contraction(m,L)))
)

deletion(FlagMatroid,Set) := FlagMatroid => (M,S) -> (
    flagMatroid(M.constituents/(m -> deletion(m,S)))
)

deletion(FlagMatroid,List) := FlagMatroid => (M,L) -> (
    flagMatroid(M.constituents/(m -> deletion(m,L)))
)

FlagMatroid | Set := (M,S) -> restriction(M,S)
FlagMatroid \ Set := (M,S) -> deletion(M,S)
FlagMatroid / Set := (M,S) -> contraction(M,S)
FlagMatroid | List := (M,L) -> restriction(M,L)
FlagMatroid \ List := (M,L) -> deletion(M,L)
FlagMatroid / List := (M,L) -> contraction(M,L)


dual(FlagMatroid) := FlagMatroid => {} >> opts -> M -> (
    ML := M.constituents;
    flagMatroid apply(reverse ML, m -> dual m)
)

face(Matroid,Set) := (M,S) -> (M | S) ++ (M / S)

face(FlagMatroid,Set) := (M,S) -> (M | S) ++ (M / S)


--rank (of a subset) in a flag matroid
rank(FlagMatroid) := ZZ => M -> sum(M.constituents/rank)

rank(FlagMatroid,Set) := ZZ => (M,A) -> sum(M.constituents, m -> rank(m,A))
-------------------------*-

--given a flag matroid M, returns the TKClass of its 'torus-orbit' in the flag-variety X
tKClass(TVariety,FlagMatroid) := TKClass => (X,M) -> (
    if not (lieType X) === "A" then error "the T-variety is not a flag variety";
    E := M.groundSet;
    K := M.constituents/rank;
    R := X.charRing;
    if not (#(gens R) == #E and (first X.points)/(s -> #s) == K) then 
    	error "wrong flag variety for the flag matroid";
    B := bases M;
    L := apply(X.points, p -> (
	if not member(p,B) then return 0_R;
	vp := flagToVec(p,#E);
	rays := select(X.charts#p, r -> member(vecToFlag(swap(vp,positions(r, i -> not i == 0))),B));
	nonrays := select(X.charts#p, r -> not member(r,rays));
	ConeP := tHilbNumer(X,rays);
	if #nonrays == 0 then ConeP else ConeP * product(nonrays, l -> (1-R_l))
	)
    );
    tKClass(X,L)
)


--internal method
--sets up the varieties involved in the Fourier-Mukai themed push-pull diagram for the
--flag geometric Tutte polynomial of a flag matroid.  The charRing R should be given.
--Given a list K and an integer n, sets up Fl(K;n) <-f- Fl(1,K,n-1;n) -g-> Fl(1;n) x Fl(n-1;n)
fourierMukai = method();
fourierMukai(List,ZZ,Ring) := List => (K,n,R) -> (
    FlK := tGeneralizedFlagVariety("A",n-1,K,R);
    Fl1Kn1 := tGeneralizedFlagVariety("A",n-1,unique ({1}|K|{n-1}), R);
    Fl1 := tGeneralizedFlagVariety("A",n-1,{1},R);
    Fln1 := tGeneralizedFlagVariety("A",n-1,{n-1},R);
    piK := tFlagMap(Fl1Kn1,FlK);
    f := tFlagMap(Fl1Kn1,Fl1); g := tFlagMap(Fl1Kn1,Fln1);
    pi1n1 := compose(f ** g, diagonalTMap Fl1Kn1);
    --<< "{{Fl(K;n), Fl(1,K,n-1;n),  Fl(1;n) x Fl(n-1;n)},{pi_K, pi_(1(n-1))}}" <<
    {{FlK, Fl1Kn1, pi1n1.target}, {piK, pi1n1}}
)

fourierMukai(List,ZZ) := List => (K,n) -> fourierMukai(K,n, makeCharRing n)


---< auxiliary functions for converting T-equivariant class to in terms of alpha,  beta >---

--takes in a tKClass C of Fl(1;n) x Fl(n-1;n) and outputs the matrix whose (i,j)th entry
--is the hilb value at the point {set{i}, [n]\set{j}}
toMatrix = C -> (
    R := C.tvar.charRing;
    n := #(gens R);
    E := set toList(0..(n-1));
    matrix apply(n, i -> apply(n, j -> (
        if i == j then return 0_R;
	C.hilb#(({set{i}}, {E - set{j}}))
	))
    )
)

aa = (i0,i,j,R) -> (R_i-R_(i0))*(R_i^(-1));
bb = (j0,i,j,R) -> (R_(j0) - R_j)*(R_(j0)^(-1));

equiToNonEquiStep = (i0,j0,X) -> (
    R := ring X_(0,0); n := #(gens R);
    t := symbol t; S := QQ[t_0..t_(n-1)]; 
    denom := sub(product(i0, k -> aa(k,i0,j0,R))*product(j0, k -> bb(k,i0,j0,R)),R);
    c := first toFraction(X_(i0,j0), denom,S);
    cVal := sub(c, apply(gens ring c, r -> r=>1));
    XX := matrix apply(n, i -> apply(n, j -> (
	numer := X_(i0,j0) * product(i0, k -> aa(k,i,j,R)) * product(j0, k -> bb(k,i,j,R));
	ratio := toFraction(numer,denom,S);
	(last ratio) (first toFraction(X_(i,j),1_R,S) - first ratio)
	))
    );
    {cVal,XX}
)

--internal method
--Given a TKClass in P^(n-1) x P^(n-1) = Gr(n-1;n) x Gr(1;n),
--outputs the polynomial in representing its K-class
--where x,y are the structure sheaves of the two hyperplanes
toPolynomial = method();
toPolynomial(TKClass) := Matrix => C -> (
    T := toMatrix C;
    n := numcols T;
    TList := {T};
    M := matrix apply(n, i -> apply(n, j -> (
    	out := equiToNonEquiStep(i,j,TList_(-1));
	TList = append(TList, last out);
	first out
	))
    );
    x := symbol x; y := symbol y;
    S := ZZ[x,y];
    sum(n, i -> sum(n, j -> M_(i,j) * S_0^j * S_1^i))
)


--given a flag matroid M, outputs the flag-geometric Tutte polynomial of M
kTutte = method();
kTutte(FlagMatroid) := RingElement => M -> (
    if not M.cache.?kTutte then M.cache.kTutte = (
	n := #M.groundSet;
    	R := makeCharRing n;
    	K := M.constituents/rank;
    	FM := fourierMukai(K,n,R);
    	FlK := first first FM;
    	f := first last FM; g := last last FM;
    	yM := tKClass(FlK,M);
    	YM := (pushforward g)( (pullback f)(yM * (ampleTKClass FlK)) );
    	toPolynomial YM
    );
    M.cache.kTutte
)




load "GKMManifolds/Documentation_GKMManifolds.m2"
load "GKMManifolds/Tests_GKMManifolds.m2"



end

---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------


restart
uninstallPackage "GKMManifolds"
installPackage "GKMManifolds" --42 seconds
viewHelp GKMManifolds
time check "GKMManifolds"

needsPackage "GKMManifolds"

--the canned examples
FM = flagMatroid {uniformMatroid(1,5),uniformMatroid(3,5)}
time kTutte FM -- 7 seconds
FM = flagMatroid {uniformMatroid(2,6),uniformMatroid(4,6)}
--time kTutte FM --700 seconds
-*--
i11 : FM = flagMatroid {uniformMatroid(2,6),uniformMatroid(4,6)}

o11 = a flag matroid with rank sequence {2, 4} on 6 elements 

o11 : FlagMatroid

i12 : time kTutte FM
     -- used 691.322 seconds

       4 4     4 3     3 4     4 2     3 3     2 4     4       3 2      2 3       4     4      3 
o12 = x y  + 2x y  + 2x y  + 3x y  - 6x y  + 3x y  + 4x y + 18x y  + 18x y  + 4x*y  + 5x  + 14x y
      --------------------------------------------------------------------------------------------
           2 2        3     4     3     2        2     3
      + 18x y  + 14x*y  + 5y  + 2x  + 6x y + 6x*y  + 2y

o12 : ZZ[x, y]
--*-

--canned example for displaying error messages
--for bruhatOrder
Fl3 = tGeneralizedFlagVariety("A",2,{1,2})
cellOrder Fl3
P = bruhatOrder Fl3
#(coveringRelations P) == 8
cellOrder Fl3
--for (cellOrder, MomentGraph, Poset)
PP3 = tProjectiveSpace 3
cellOrder PP3
V = (momentGraph PP3).vertices
P = poset(V, {{V_0,V_1},{V_1,V_2},{V_2,V_3}})
cellOrder(momentGraph PP3, P)
cellOrder PP3
--for setIndicator
T3 = set{1,"1*","2*",3}		
setIndicator(T3,4)

