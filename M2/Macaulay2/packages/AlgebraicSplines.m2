-- Copyright 2014-2015: Mike Dipasquale
-- You may redistribute this file under the terms of the GNU General Public
-- License as published by the Free Software Foundation, either version 2
-- of the License, or any later version.

------------------------------------------
------------------------------------------
-- Header
------------------------------------------
------------------------------------------

newPackage select((
    "AlgebraicSplines",
        Version => "0.1.0", 
        Date => "27. May 2015",
        Authors => {
            {Name => "Michael DiPasquale", Email => "mdipasq@okstate.edu", HomePage => "http://math.okstate.edu/people/mdipasq/"},
            {Name => "Gwyn Whieldon", Email => "whieldon@hood.edu", HomePage => "http://cs.hood.edu/~whieldon"},
	    {Name => "Eliana Duarte", Email => "emduart2@illinois.edu", HomePage => "https://faculty.math.illinois.edu/~emduart2/"},
	    {Name => "Daniel Irving Bernstein", Email=> "dibernst@ncsu.edu", HomePage =>"http://www4.ncsu.edu/~dibernst"}
        },
        Headline => "splines on simplicial complexes, polytopal complexes, and graphs",
	Keywords => {"Applied Algebraic Geometry"},
        Configuration => {},
        DebuggingMode => false,
	PackageImports => { "Elimination" },
        PackageExports => {
	    "FourierMotzkin"
	    }
        ), x -> x =!= null)

export {
   "BaseRing",
   "splineMatrix",
   "splineModule",
   "InputType",
   "RingType",
   "ByFacets",
   "ByLinearForms",
   "Homogenize",
   "VariableName",
   "GenVar",
   "IdempotentVar",
   "Trim",
   "VariableGens",
   "splineDimensionTable",
   "postulationNumber",
   "hilbertComparisonTable",
   "generalizedSplines",
   "ringStructure",
   "formsList",
   "cellularComplex",
   "idealsComplex",
   "splineComplex",
   "courantFunctions",
   "stanleyReisner",
   "stanleyReisnerPresentation"
   --Remove after testing
   --"getCodim1Intersections",
   --"simpBoundary",
   --"facetsN",
   --"boundaryComplex",
   --"polyBoundary",
   --"prune1",
   --"pruneR",
   --"expressInGens"
   }


------------------------------------------
------------------------------------------
-- Data Types and Constructors
------------------------------------------
------------------------------------------

--may include the following data types in a future iteration

--Create an object that gives ALL splines
--on a given subdivision.
--Splines = new Type of HashTable

--splines = method(Options => {
--	symbol InputType => "ByFacets", 
--	symbol Homogenize => true, 
--	symbol VariableName => getSymbol "t",
--	symbol CoefficientRing => QQ,
--	symbol BaseRing => null})

--splines(List,List,List,ZZ) := Matrix => opts -> (V,F,E,r) -> (
--    	AD := splineMatrix(V,F,E,r,opts);
--	K := ker AD;
--	b := #F;
--  	new Splines from {
--	    symbol cache => new CacheTable from {"name" => "Unnamed Spline"},
--	    symbol VertexCoordinates => V,
--	    symbol Regions => F,
--	    symbol SplineModule => image (gens K)^(toList(0..b-1))
--	}
--)

--net Splines := S -> S.SplineModule
   
------------------------------------------
------------------------------------------
-- Methods
------------------------------------------
------------------------------------------


------------------------------------------
subsetL=method()
------------------------------------------
--Containment function for lists--
subsetL(List,List):=Boolean=>(L1,L2)->(
    all(L1,f->member(f,L2))
    )

------------------------------------------
------------------------------------------
getCodim1Intersections = method(Options=>{
	symbol InputType => "Polyhedral"
	}
    )
------------------------------------------
------------------------------------------
--Inputs: 
------------------------------------------
--F = list of facets of a polytopal 
--or a simplicial complex
------------------------------------------
--Outputs:
-----------------------------------------
--E = largest (under containment) intersections
--of facets.  If input is hereditary, this is
--the list of interior codim 1 intersections
-----------------------------------------

getCodim1Intersections(List) := List => opts -> F ->(
     n := #F;
    if opts.InputType==="Polyhedral" then(
    	--For each pair of facets, take their intersection:
     	intersectFacets := unique flatten apply(#F-1, i-> 
    	    apply(toList(i+1..#F-1), 
	    	j-> sort select(F_i,
		    v-> member(v,F_j))));
	--Remove any non-maximal faces in this intersections:
    	codim1int:=select(intersectFacets, f -> (
    		(number(intersectFacets, g-> all(f, j-> member(j,g)))) === 1
    		))
    	) else if opts.InputType==="Simplicial" then(
    	d := #(F_0);
    	--For each non-final facet, construct all codimension 1 subsets.
    	codim1faces := apply(n-1, i -> subsets(F_i,d-1));
    	--Check if a codimension 1 subset is contained in another facet,
    	--store it as a codim 1 intersection.
    	codim1int=sort flatten apply(#codim1faces, i -> 
	    select(codim1faces_i, 
	    	s -> any(F_{i+1..n-1}, 
		    f-> subsetL(s,f))));
	);
    codim1int
)



------------------------------------------
------------------------------------------
getCodimDFacesSimplicial = method()
------------------------------------------
------------------------------------------
--Inputs: 
------------------------------------------
--F = list of facets of a simplicial complex
--d = desired codimension
------------------------------------------
--Outputs:
-----------------------------------------
--E = list of (all) codim d faces
-----------------------------------------
getCodimDFacesSimplicial(List,ZZ) := List => (F,D) -> (
    d := #first(F);
    unique flatten apply(F, f-> subsets(f,d-D))
    )


-----------------------------------------

formsList=method(Options=>{
	symbol InputType => "ByFacets", 
	symbol Homogenize => true, 
	symbol VariableName => getSymbol "t",
	symbol CoefficientRing => QQ,
	symbol BaseRing => null})
----------------------------------------------------
--This method returns a list of forms corresponding to codimension one faces
--------
--Input:
--V= vertex list
--E= codim 1 face list
--r= smoothness parameter
--------
--Output:
--List of forms defining input list of codimension one faces 
--raised to (r+1) power
------------------------------------------------------------

formsList(List,List,ZZ):= List => opts->(V,E,r)->(
    --To homogenize, we append a 1 as the final coordinate of each vertex coord in list V.
    --If not homogenizing, still need 1s for computing equations
    d := #(first V);
    V = apply(V, v-> append(v,1));
    if opts.BaseRing === null then (
	S := createSplineRing(d,opts);
	)
    else (
	S = opts.BaseRing;
	);
    if opts.Homogenize then (
	varlist := vars S;
	) else (
	varlist = (vars S)|(matrix {{sub(1,S)}});
	);
    varCol := transpose varlist;
    M := (transpose(matrix(S,V)));
    mM := numrows M;
    minorList := apply(E, e-> gens gb minors(mM,matrix(M_e)|varCol));
    if any(minorList, I-> ideal I === ideal 1) then (
    	error "Some vertices on entered face are not in codimension 1 face."
	    );
    flatten apply(minorList, m -> (m_(0,0))^(r+1))
)

createSplineRing = method(Options => {
    	symbol InputType => "ByFacets", 
	symbol Homogenize => true, 
	symbol VariableName => getSymbol "t",
	symbol CoefficientRing => QQ,
	symbol BaseRing => null
	}
    )


createSplineRing(ZZ):= PolynomialRing => opts -> (d) -> (
    t := opts.VariableName;
    if opts.BaseRing === null then (
    	if opts.Homogenize then (
	    S := (opts.CoefficientRing)[t_0..t_d];
	    ) else (
	    S = (opts.CoefficientRing)[t_1..t_d];
	    );
	)
	else (
	    S = opts.BaseRing
	);
    S
    )


-----------------------------------------
-----------------------------------------
splineMatrix = method(Options => {
	symbol InputType => "ByFacets", 
	symbol Homogenize => true, 
	symbol VariableName => getSymbol "t",
	symbol CoefficientRing => QQ,
	symbol BaseRing => null})
------------------------------------------
------------------------------------------

------------------------------------------
------------------------------------------
-- splineMatrix "ByFacets"
------------------------------------------
--Inputs: 
------------------------------------------
--("ByFacets")
--L = {L_0,L_1,L_2} (i.e. {V,F,E})
--r = degree of desired continuity 
--
-- OR!!!!
--
--("ByLinearForms")
--L = {L_0,L_1} (i.e. {B,L})
--r = degree of desired continuity
------------------------------------------
--Outputs:
-- BM = matrix with columns corresponding
-- to facets and linear forms separating facets.
------------------------------------------
splineMatrix(List,ZZ) := Matrix => opts -> (L,r) -> (
    --Use this if your list L = {V,F,E} contains
    --The inputs as a single list L.
    if opts.InputType === "ByFacets" then (
	splineMatrix(L_0,L_1,L_2,r)
	);
    if opts.InputType == "ByLinearForms" then (
	splineMatrix(L_0,L_1,r,InputType=>"ByLinearForms")
	)
    )

------------------------------------------
------------------------------------------
--Inputs: 
------------------------------------------
--("ByFacets")
--V = list of coordinates of vertices
--F = ordered lists of facets
--E = list of edges
--r = degree of desired continuity
------------------------------------------
--Outputs:
-- BM = matrix with columns corresponding
-- to facets and linear forms separating facets.
------------------------------------------
splineMatrix(List,List,List,ZZ) := Matrix => opts -> (V,F,E,r) -> (
    if opts.InputType === "ByFacets" then (
	d := # (first V);
	--Compute which facets are adjacent to each edge:
	facetEdgeH := apply(#E, e-> positions(F, f-> all(E_e,v-> member(v,f))));
	--Compute indices of interior edges, and replace edge list and 
	--facet adjacencies to only include these interior edges:
	indx := positions(facetEdgeH, i-> #i === 2);
	E = E_indx;
	facetEdgeH = facetEdgeH_indx;
	--Compute top boundary map for complex:
	BM := matrix apply(
	    facetEdgeH, i-> apply(
		#F, j-> if (
		    j === first i) then 1 else if (
		    j===last i) then -1 else 0));
	--List of forms definining interior codim one faces (raised to (r+1) power)
	flist := formsList(V,E,r,opts);
	T := diagonalMatrix(flist);
	splineM := BM|T;
	) else if opts.InputType === "ByLinearForms" then (
	 print "Wrong inputs, put in lists of adjacent facets and linear forms and continuity r."
    	 );
    splineM
)

------------------------------------------
------------------------------------------
--Inputs: 
------------------------------------------
------------------------------------------
-- ("ByFacets")
--V = list of vertex coordinates
--F = list of facets
--r = degree of desired continuity
--
--    OR!!!!
--
--("ByLinearForms")
--B = list of adjacent facets
--L = list of ordered linear forms
--defining codim 1 faces, ordered as in B
--r = degree of desired continuity
------------------------------------------
--Outputs:
-- BM = matrix with columns corresponding
-- to facets and linear forms separating facets.
------------------------------------------
splineMatrix(List,List,ZZ) := Matrix => opts -> (V,F,r) ->(
    --Warn user if they are accidentally using ByFacets method with too few inputs.
    --This code assumes that the polytopal complex is hereditary.
    if opts.InputType === "ByFacets" then (
	if issimplicial(V,F) then(
	    E := getCodim1Intersections(F,InputType=>"Simplicial");
	    SM := splineMatrix(V,F,E,r,opts)  
	    )
	else(
	    E = getCodim1Intersections(F);
	    SM = splineMatrix(V,F,E,r,opts)
	    );
	);
    --If user DOES want to define complex by regions and dual graph.
    if opts.InputType === "ByLinearForms" then (
	B := V;
	L := F;
	m := max flatten B;
	A := matrix apply(B, i-> apply(toList(0..m), j-> 
		if (j=== first i) then 1 
		else if (j===last i) then -1 
		else 0));
	D := matrix apply(#L, i-> apply(#L, j-> if i===j then L_i^(r+1) else 0));
	SM = A|D;
    );
    SM
)


------------------------------------------
------------------------------------------
splineModule = method(Options => {
	symbol InputType => "ByFacets",
	symbol Homogenize => true, 
	symbol VariableName => getSymbol "t",
	symbol CoefficientRing => QQ,
	symbol BaseRing => null}
    )
------------------------------------------
------------------------------------------
-- This method computes the splineModule
-- of a complex Delta, given by either
-- facets, codim 1 faces, and vertex coors,
-- or by pairs of adjacent faces and
-- linear forms.
------------------------------------------
--Inputs: 
------------------------------------------
--V = list of vertices
--F = list of facets
--E = list of edges
--r = desired continuity of splines
------------------------------------------
--Outputs:
--Spline module S^r(Delta)
------------------------------------------
splineModule(List,List,List,ZZ) := Matrix => opts -> (V,F,E,r) -> (
    	AD := splineMatrix(V,F,E,r,opts);
	K := ker AD;
	b := #F;
    	image submatrix(gens K, toList(0..b-1),)
)

------------------------------------------
--Inputs: 
------------------------------------------
--V = list of vertices
--F = list of facets
--r = desired continuity of splines
--
--    OR!!!!
--
--V = list of pairs of adjacent faces
--F = list of linear forms defining codim 1 faces.
--r = desired continuity of splines
------------------------------------------
--Outputs:
--Spline module S^r(Delta)
------------------------------------------
splineModule(List,List,ZZ) := Matrix => opts -> (V,F,r) -> (
    	AD := splineMatrix(V,F,r,opts);
	K := ker AD;
	b := #F;
	if opts.InputType==="ByLinearForms" then (
		b = #(unique flatten V)
		);
    	image submatrix(gens K, toList(0..b-1),)
)
------------------------------------------
-------------------------------------------
-------------------------------------------
splineDimensionTable=method()
-------------------------------------------
-----Inputs:
-------------------------------------------
----- a= lower bound of dim table
----- b= upper bound of dim table
----- M= module
--------------------------------------------
------ Outputs:
--------------------------------------------
------ A net with the degrees between a and b on top row
------ and corresponding dimensions of graded pieces
------ of M in bottom row
-------------------------------------------

splineDimensionTable(ZZ,ZZ,Module):=Net=>(a,b,M)->(
    r1:=prepend("Degree",toList(a..b));
    r2:=prepend("Dimension",apply(toList(a..b),i->hilbertFunction(i,M)));
    netList {r1,r2}
    )

-------------------------------------------
-----Inputs:
-------------------------------------------
----- a= lower bound of range
----- b= upper bound of range
----- L= list {V,F,E}, where V is a list of vertices, F a list of facets, E a list of codim 1 faces
----- r= degree of desired continuity
-------------------------------------------
-----Outputs:
-------------------------------------------
-------A table with the dimensions of the graded pieces
------ of the spline module in the range (a,b)
-------------------------------------------

splineDimensionTable(ZZ,ZZ,List,ZZ):= Net=>(a,b,L,r)->(
    M := splineModule(L_0,L_1,L_2,r);
    splineDimensionTable(a,b,M)
    )

-------------------------------------------
-----Inputs:
-------------------------------------------
----- a= lower bound of range
----- b= upper bound of range
----- L= list {V,F}, where V is list of vertices, F a list of facets
-------------
-----OR!!
-------------------------------------------
----- a= lower bound of range
----- b= upper bound of range
----- L= list {V,F}, where V is a list of adjacent facets, F a list of forms
-----------defining codim 1 faces along which adjacent facets meet
-------------------------------------------
-------Outputs:
-------------------------------------------
-------A table with the dimensions of the graded pieces
------ of the spline module in the range (a,b)
-------------------------------------------

splineDimensionTable(ZZ,ZZ,List,ZZ):= Net => (a,b,L,r)->(
    M := splineModule(L_0,L_1,r);
    splineDimensionTable(a,b,M)
    )


-------------------------------------------
-------------------------------------------
postulationNumber=method()
-------------------------------------------
-----Inputs:
-------------------------------------------
----- M, a graded module
--------------------------------------------
------ Outputs:
--------------------------------------------
------ The postulation number (largest integer 
------ for which Hilbert function and polynomial 
------ of M disagree).
--------------------------------------------
postulationNumber(Module):= (N) ->(
    k := regularity N;
    h := hilbertPolynomial(N);
    while hilbertFunction(k,N)==h(k) do	(k=k-1);
    k
    )

------------------------------------------
-----------------------------------------

hilbertComparisonTable=method()
-------------------------------------------
-----Inputs:
-------------------------------------------
----- a= an integer, lower bound
----- b= an integer, upper bound
----- M= graded module over a polynomial ring
--------------------------------------------
------ Outputs:
--------------------------------------------
------ A table whose top two rows are the same as
------ the output of splineDimensionTable and whose 
------ third row compares the first two to the
------ Hilbert Polynomial
--------------------------------------------

hilbertComparisonTable(ZZ,ZZ,Module):= (a,b,M) ->(
    h := hilbertPolynomial(M);
    r1 := prepend("Degree",toList(a..b));
    r2 := prepend("Dimension",apply(toList(a..b),i->hilbertFunction(i,M)));
    r3 := prepend("HilbertPoly",apply(toList(a..b),i->h(i)));
    netList {r1,r2,r3}
    )
---------------------------------------------

----------------------------------------------
courantFunctions=method(Options => {
	symbol InputType => "ByFacets", 
	symbol Homogenize => true, 
	symbol VariableName => getSymbol "t",
	symbol CoefficientRing => QQ,
	symbol BaseRing => null})
-----------------------------------------------
--Inputs:
---V: List of vertices (of a simplicial complex)
---F: List of facets
--Outputs:
---CF: #F\times #V matrix of Courant functions which are
---1 at a chosen vertex and 0 elsewhere
------------------------------------------------
courantFunctions(List,List) := Matrix => opts -> (V,F) -> (
    if not issimplicial(V,F) then(
	error "Input must be simplicial."
	)else(
	--To homogenize, we append a 1 as the final coordinate of each vertex coord in list V.
    	--If not homogenizing, still need 1s for computing equations
	d := #(first V);
    	Vh := apply(V, v-> append(v,1));
	if opts.BaseRing === null then (
	S := createSplineRing(d,opts);
	) else (
	S = opts.BaseRing;
	);
    if opts.Homogenize then (
	varlist := vars S;
	int := set {}
	) else (
	varlist = (vars S)|(matrix {{1_S}});
	int = set(F_0);
	scan(toList(1..length(F)-1),i->(
		int = set(F_i)*int
		))
	);
    varCol := transpose varlist;
    M := transpose(matrix(S,Vh));
    matrix table(length F,toList(0..(length V)-1)-int,(i,j)->(
	    if member(j,F_i) then(
		lk := (F_i)-set({j});
		Num := det(M_lk|varCol);
		Den := sub(det(M_lk|M_{j}),QQ);
		Num/Den
		)else(
		0
		)))
    ))


------------------------------------------
generalizedSplines = method(Options=>{
	symbol RingType => "Ambient"
	}
    )
------------------------------------------
------------------------------------------
-- This method computes the generalized spline module
-- associated to a graph whose edges are labeled by ideals.
------------------------------------------
--Inputs: 
------------------------------------------
--E = list of edges. Each edge is a list with two vertices.
--OR
--G=graph object
--ideals = list of ideals that label the edges. Ideals must 
--be entered in same order as corresponding edges in E, or if
--a graph G is entered, in the same order as the edges are listed
--in edges(G).  The default order for edges(G) is lexicographic.
--If a list of ring elements is entered, it is assumed these are 
--generators for principal ideals.
--If RingType is an integer,n, then this can be a list 
--of integers that label the edges.
-------------------------------------------------
--if RingType is an integer, n, then the ring is defined 
--internally as ZZ/n.
--If RingType is "Ambient", an ambient ring must already 
--be defined so that ideals can be entered.
------------------------------------------
--Outputs:
------------------------------------------
--Module of generalized splines on the graph given by the edgelist.
------------------------------------------

generalizedSplines(List,List) := Module => opts -> (E,ideals) ->(
    if opts.RingType === "Ambient" then(
    S := ring first ideals;
    --make sure ideals all lie in same ambient ring--
    ideals = apply(ideals, I->sub(I,S));
    --make sure ideals list consists of ideals.  If ring elements are
    --entered will automatically make these ideals.
    ideals = apply(ideals, I->(if class(I)===S then(ideal I)else(I)))
    ) else if toString(class(opts.RingType))==="ZZ" then(
       m := opts.RingType;
       R := ZZ[{}];
       S = R/ideal(m_R);
       ideals = apply(ideals,i-> ideal(i_S));
       );
    vert := sort unique flatten E;
    n := #vert;
    T := directSum(apply(ideals,I->coker gens I));
--Boundary Map from Edges to vertices (this encodes spline conditions)
    M := matrix apply(E,
	e->apply(vert,
	    v->if(v===first e) then 1
	    else if(v===last e) then -1
	    else 0));
    ker(map(T,S^n,sub(M,S)))
)

--generalizedSplines(Graph,List) := Module => opts -> (G,ideals) ->(
--E:=apply(edges G,e->elements e);
--generalizedSplines(E,ideals,opts)
--)


--------------------------------------
ringStructure = method(Options=>{
	symbol GenVar => getSymbol "Y",
	symbol IdempotentVar => getSymbol "e",
	symbol Trim => false,
	symbol VariableGens => true
	}
    )
--------------------------------------
--Input: A submodule, M, of the ring S^k (ring structure of direct sum),
-----which is known to have the structure of a sub-ring of S^k
--Output: A map whose image is M as a sub-ring of S^k and whose source
-----is a polynomial ring in as many variables as there are generators
-----of M as a sub-ring of S^k.
--------------------------------------
ringStructure(Module) := RingMap => opts -> M ->(
    S := M.ring;
    ng := rank (ambient M);
    if opts.VariableGens then (
	--diagonal embedding of polynomial ring--
	ident := matrix apply(ng,i->{1_S});
	--remove identity (if it exists) as a generator of M--
	pos := select(numcols gens M,i->(flatten entries((gens M)_{i}))!=(flatten entries ident));
	--add variables under diagonal embedding to generators of M--
	newM :=((ident)*(vars S))|((gens M)_(pos));
	)else(
	newM = (gens M)
	);
    dg := (degrees(newM))_1;
    --build direct sum--
    K := if (S===ZZ) then (ZZ) else coefficientRing(S);
    e := opts.IdempotentVar;
    T := K[e_0..e_(ng-1)];
    --ideal of idempotent relations--
    PR := apply(subsets(ng,2),L->(T_(first L)*T_(last L)));
    SR := apply(ng,i->T_i^2-T_i);
    Rel := ideal( join(PR,SR) );
    DS :=(T/Rel)**S;
    --turn module generators into ring generators
    mat := (sub(vars T,DS))*(sub(newM,DS));
    Y := opts.GenVar;
    Am := K[Y_0..Y_(numcols(newM)-1),Degrees=>dg];
    phi := map(DS,Am,flatten entries mat);
    if opts.Trim then(
	phi = pruneR(phi)
	);
    phi
    )

----------------------------------------------
prune1=method()
---------------------------------------------
--Input: a ring map whose source is a polynomial ring and the kernel of that map
--Output: a ring map whose source is a sub-ring of the same polynomial ring,
---with one extraneous generator removed
---------------------------------------------
prune1(RingMap,Ideal):=RingMap=> (phi,Kr) ->(
    H :=source phi;
    G :=gens H;
    extGen := select(1,G,var->(any(Kr_*,g->member(var,flatten entries monomials g))));
    --dg := apply(G,var->degree(var));
    --H1:= K[reverse(G),Degrees=>reverse(dg),MonomialOrder=>Lex];
    --I := sub(Kr,H1);
    --extGen := select(1,gens H,var->(
	--    nv := sub(var,H1);
	--    (nv%I)!=nv
	--    ));
    K :=coefficientRing(H);
    T := target phi; 
    I2 :=eliminate(Kr,extGen);
    G2 :=G-set(extGen);
    d2 :=apply(G2,g->degree(g));
    H2 :=K[G2,Degrees=>d2];
    phi2 := map(T,H2,apply(G2,g->phi(sub(g,H))));
    K2 := sub(I2,H2);
    (phi2,K2)
    )

--------------------------------------------------------
pruneR=method()
--------------------------------------------------------
--Input: a ring map whose source is a polynomial ring
--Output: a ring map whose source is a sub-ring of the same polynomial ring,
---with all extraneous generators removed
---------------------------------------------
pruneR(RingMap):=RingMap=> phi ->(
    G := gens (source phi);
    Kr := ker phi;
    (phi2,Kr2) := prune1(phi,Kr);
    G2 := gens(source phi2);
    while #G2<#G do(
	phi = phi2;
	G = G2;
	Kr = Kr2;
	(phi2,Kr2) = prune1(phi,Kr);
	G2 = gens(source phi2)
	);
    phi2
    )
    
-----------------------------------------------------------
stanleyReisner=method(Options=>{
    symbol InputType => "ByFacets",
    symbol Homogenize => true,
    symbol VariableName => getSymbol "t",
    symbol CoefficientRing => QQ,
    symbol BaseRing => null}
    )
-----------------------------------------------------------
--Input: V, list of vertices and F, list of facets
--Output: A ring map phi whose image is the ring of continuous piecewise polynomials on (V,F).  If
--(V,F) is simplicial, the basis of Courant functions is chosen, so ker phi
--is the stanleyReisner ideal of (F)
-----------------------------------------------------------
stanleyReisner(List,List):=Ring=>opts->(V,F)->(
    if issimplicial(V,F) then(
	CF := courantFunctions(V,F,opts);
	phi := ringStructure(image CF,VariableGens=>false,GenVar => getSymbol "X")
	)else(
	C0 := splineModule(V,F,0,opts);
	phi = ringStructure(C0,Trim=>true,GenVar => getSymbol "X")
	);
    phi
    )

-----------------------------------------------------------
expressInGens=method(Options=>{
    symbol GenVar => getSymbol "Z"}
    )
-----------------------------------------------------------
--Input: ring map phi, matrix representing element of free module R^k
----which is contained in the image of phi
--Output: expression for phi^{-1}(f)
-----------------------------------------------------------
expressInGens(RingMap,Matrix):= RingElement => opts -> (phi,f) -> (
    ng := rank (ambient image f);
    H := source phi;
    dg := flatten apply(gens H,var->degree(var));
    T := target phi;
    K := coefficientRing(H);
    Z := opts.GenVar;
    nH := K[prepend(Z,gens H),Degrees=>join((degrees f)_1_0,dg),MonomialOrder=>Lex];
    nG := drop(gens nH,1);
    img := apply(nG,var->phi(sub(var,H)));
    vList := matrix {apply(ng,i->T_i)};
    nf := matrix apply(flatten entries f,p->{sub(p,T)});
    I := ker map(T,nH,prepend((vList*nf)_(0,0),img));
    sub((nH_0)%I,H)
    )

-----------------------------------------------------------
stanleyReisnerPresentation=method(Options=>{
    symbol GenVar => getSymbol "Y",
    symbol IdempotentVar => getSymbol "e",
    symbol Trim => false,
    symbol VariableGens => true,
    symbol InputType => "ByFacets",
    symbol Homogenize => true,
    symbol VariableName => getSymbol "t",
    symbol CoefficientRing => QQ,
    symbol BaseRing => null}
    )
-----------------------------------------------------------
--Input: V, list of vertices, F, list of facets, r, smoothness
--Output: Presentation of ring C^r(\Delta) as sub-ring of
---Stanley Reisner ring of \Delta
-----------------------------------------------------------
stanleyReisnerPresentation(List,List,ZZ):=Matrix=>opts->(V,F,r)->(
    d := #(first V);
    S := createSplineRing(d,BaseRing=>opts.BaseRing,InputType=>opts.InputType,Homogenize=>opts.Homogenize,VariableName=>opts.VariableName,CoefficientRing=>opts.CoefficientRing);
    CR := splineModule(V,F,r,BaseRing=>S,InputType=>opts.InputType,Homogenize=>opts.Homogenize,VariableName=>opts.VariableName,CoefficientRing=>opts.CoefficientRing);
    ng := rank(ambient CR);
    ident := matrix apply(ng,i->{1_S});
    GCR := apply(numcols gens CR,i->(gens CR)_{i});
    --take away identity module generator
    GCR = select(GCR,f->(f!=ident));
    --add variables under diagonal embedding to ring generators of M--
    GCR = join(apply(gens S,v->(ident)*matrix{{v}}),GCR);
    dg := apply(GCR,f->(degrees f)_1_0_0);
    --create stanley reisner ring
    phi := stanleyReisner(V,F,BaseRing=>S,InputType=>opts.InputType,Homogenize=>opts.Homogenize,VariableName=>opts.VariableName,CoefficientRing=>opts.CoefficientRing);
    T := source phi;
    I := ker phi;
    Y := opts.GenVar;
    K := coefficientRing(S);
    H := K[apply(length GCR,i->Y_i),Degrees=>dg];
    MP := apply(GCR,f->sub(expressInGens(phi,f),T));
    phiN := map(T/I,H,MP);
    if opts.Trim then(
	phiN = pruneR(phiN)
	);
    phiN
    )

------------------------------------------
simpBoundary = method()
------------------------------------------
--Input:
--F = list of codim i faces
--E = list of codim i+1 faces
------------------------------------------
--Output:
--B = boundary map matrix between codim i and codim i+1 faces
------------------------------------------
--Example:
--F = {{0,1,2},{0,1,3},{1,3,4},{1,2,4},{2,4,5},{0,2,5},{0,3,5}}
--E = {{0,1},{1,2},{0,2},{3,0},{1,3},{1,4},{2,4},{2,5},{0,5},{3,4},{4,5}}
--V = {{0},{1},{2},{4}}
------------------------------------------
simpBoundary(List,List) := Matrix => (F,E) -> (
    F = apply(F, f-> sort f);
    E = apply(E, e-> sort e);
    if #F==0 then return(
	map(image matrix{{0}},image matrix{{0}},matrix{{0}})
	)else(
	if #E==0 then return(
	    map(image matrix{{0}},ZZ^(length F),matrix{apply(length F,i->0)})
	    )else(
	    tempLF := {};
	    rowList := {};
	    apply(F, f-> (
		    tempLF = hashTable apply(#f, v-> position(E,e-> e == drop(f,{v,v})) => (-1)^v);
		    rowList = append(rowList,apply(#E, j->if member(j,keys tempLF) then tempLF#j else 0));
		    )
		);
	    return transpose matrix rowList
	    )))
	

------------------------------------------
orient = method()
------------------------------------------
--Input:
--I = the ideal generated by forms vanishing
--on codim one faces containing a face P
------------------------------------------
--Output:
--L = list of length dim P, smallest variables
--(w.r.t. standard lex order)
--which are dependent on the affine span of P
------------------------------------------

orient(Ideal):=List=> I->(
    LT:= leadTerm I;
    reverse sort flatten apply(flatten entries LT,f-> support f)
    )

------------------------------------------
polyBoundaryPair=method()
------------------------------------------
----This method is for use when there are precomputed
----lists of ideals and orientations
------------------------------------------
--Input:
--V = vertices of complex
--L1 = {G,IG,OG}
--L2 = {H,IH,OH}
--G = codim (i+1) face (list of indices of vertices) 
--IG = ideal of G
--OG = orientation of G
--H = codim i face (list of indices of vertices)
--IH = ideal of H
--OH = orientation of H
------------------------------------------
--Output:
--O =  +1,-1, or 0, encoding compatibility
--of orientations between G and H
------------------------------------------

polyBoundaryPair(List,List,List):=ZZ=>(V,L2,L1)->(
    G:=L1_0;
    H:=L2_0;
    --if G is not a face of H then return 0 --
    if not subsetL(G,H) then (
	ort:=0) else(	
	--get ambient ring--	
	S:=ring (L1_1);
	--make sure ideals and variables sit inside same ambient ring--
	IG:=sub(L1_1,S);
	OG:=apply(L1_2,var->sub(var,S));
	IH:=sub(L2_1,S);
	OH:=apply(L2_2,var->sub(var,S));
	--get a vertex of H that is not a vertex of G--
	--if homogenized, append 1
	if (numcols vars S)==(#(V_0)+1) then(
	    testVindex :=select(1,H,v->not member(v,G));
	    testV := transpose matrix({append(flatten V_testVindex,1)})
	    --else, don't append 1--
	    ) else (
	    testVindex = select(1,H,v->not member(v,G));
	    testV = transpose matrix(V_testVindex)
	    );
	--get a generator of IG that is not a generator of IH--
	outVect :=first select(1,flatten entries gens IG,f->(f%IH!=0));
	--get row vector whose entries are coefficients of outVect
	matV :=transpose jacobian ideal outVect;
	--modify outVect so vector corresponding to outVect points 
	--outward from H
	if sub(((matV*testV)_(0,0)),ZZ)<0 then(outVect=-outVect);
	--reduce outVect modulo IH--
	outVect =(outVect%IH);
	--Get independent variables on the affine span of H and G--
	indH :=select(flatten entries vars S,v->not member(v,OH));
	indG :=select(flatten entries vars S,v-> not member(v,OG));
	--There is one independent variable on H that is dependent on G.  
	--Reduce it mod IG-- 
	pos :=position(indH,v->not member(v,indG));
	redHG :=(indH_pos)%IG;
	--Get coefficients of contraction of outVect with indH--
	inCont :=matrix{apply(length indH,i->(
		    (-1)^i*coefficient(indH_i,outVect)
		    ))};
	--Get list of coefficients for pulling back inCont to G--
        conv :=matrix{apply(length indH,i->(
		omit := drop(indH,{i,i});
		if member(indH_pos,omit) then(
		    posN := position(omit,v->(v==(indH_pos)));
		    pmvar := position(indG,v->(v==(indH_i)));
		    missvar :=indG_pmvar;
		    coef :=(-1)^posN*coefficient(missvar,redHG)*(-1)^pmvar
		    ) else (coef=1);
		coef))};
	sgn :=(inCont*(transpose conv))_(0,0);
	if (sgn>0) then(ort=1) else (ort=-1);
	);
    ort
    )

------------------------------------------
polyBoundary=method()
------------------------------------------
----This method is for use when there are precomputed
----lists of ideals and orientations
------------------------------------------
--Input:
--V = vertices of complex
--L1 = {G,IG,OG}
--L2 = {H,IH,OH}
--G = codim (i+1) faces (list of indices of vertices) 
--IG = ideals of G
--OG = orientations of G
--H = codim i faces (list of indices of vertices)
--IH = ideals of H
--OH = orientations of H
------------------------------------------
--Output:
--O =  +1,-1, or 0, encoding compatibility
--of orientations between G and H
------------------------------------------

polyBoundary(List,List,List):=Matrix=>(V,L2,L1)->(
    G := L1_0;
    H := L2_0;
    if #H==0 then(
	return map(image matrix{{0}},image matrix{{0}},matrix{{0}});
	)else(
	if #G==0 then(
	    return map(image matrix{{0}},ZZ^(#H),matrix {apply(#H,i->0)});
	    )else(
	    IG:=L1_1;
	    OG:=L1_2;
	    IH:=L2_1;
	    OH:=L2_2;
	    return matrix table(#G,#H,(i,j)->(
		    L1n :={G_i,IG_i,OG_i};
		    L2n :={H_j,IH_j,OH_j};
		    polyBoundaryPair(V,L2n,L1n)
		    ));
	    )))

------------------------------------------

boundaryComplex = method()
------------------------------------------
--Input:
--F= list of facets of a simplicial complex
----which is a pseudomanifold (Important!)
------------------------------------------
--Output:
--A list of codim one faces on the boundary
------------------------------------------
boundaryComplex(List) := List => F -> (
    n := #F;
    d := #(F_0);
    codim1faces := unique flatten apply(n,i-> subsets(F_i,d-1));
    select(codim1faces, f-> number(F, g-> all(f, v-> member(v,g))) === 1)
    )


------------------------------------------
--Input:
--V= vertex list
--F= list of facets of a complex (polyhedral or simplicial)
------------------------------------------------
--Output:
--A list of codim one faces on the boundary
------------------------------------------------
boundaryComplex(List,List):= List => (V,F)-> (
    candidateList := flatten apply(F,f->facetsN(V,f));
    select(candidateList, f->#(positions(candidateList,l->l==f))==1)
    )

facetsN = method()
------------------------------------------------
--Input:
--V=vertex list
--(Optional) F=list of indices of V to take for convex hull
------------------------------------------------
--Output: List of facets of convex hull of V_F
------------------------------------------------
facetsN(List,List):= List => (V,L)->(
    V = apply(V_L,v->prepend(1,v));
    --get minimal list of inequalities defining the conical hull of V
    H := fourierMotzkin transpose matrix V;
    halfspaces := H#0;
    --apply inequalities to vertex list to see which vertices are contained in faces
    M := (transpose halfspaces)*(transpose matrix V); 
    facetList := apply(numrows M, i->select(numcols M,j-> M_(i,j)==0));
    --label facet indices by elements of the list L
    apply(facetList,l->L_l)
    )

facetsN(List):= List => V ->(
    n := #V;
    F := toList(0..n-1);
    facetsN(V,F)
    )
------------------------------------------------

cellularComplex = method(
    	Options =>{
	    symbol InputType => "Polyhedral",
	    symbol Homogenize => true, 
	    symbol VariableName => getSymbol "t",
	    symbol CoefficientRing => QQ,
	    symbol BaseRing => null
	    }
    )
------------------------------------------------
---This method computes the cellular chain complex of a simplicial or
---polyhedral complex with coefficients in a polynomial
---ring, modulo the boundary
------------------------------------------------
---Inputs (if simplicial): A list of facets
------------------------------------------------
---Outputs: The cellular chain complex whose homology
--- is the homology of the simplicial complex relative
--- to its boundary.
--------------------------------------------------
cellularComplex(List) := ChainComplex => opts -> (F) -> (
    if opts.InputType === "Polyhedral" then (
	print "Need a List of Vertices";
	chain := null;
	);
    if opts.InputType === "Simplicial" then (
	d := (# first F)-1;
	S := createSplineRing(d,opts);
	boundaryF := boundaryComplex(F);
	C := apply(d+1, i-> getCodimDFacesSimplicial(F,i));
	boundaryC := join({{}},apply(d, i-> getCodimDFacesSimplicial(boundaryF,i)));
    	intC := apply(#C, i -> select(C_i, f -> not member(f,boundaryC_i)));
    	chain = chainComplex(reverse apply(#intC-1, c-> sub(simpBoundary(intC_c,intC_(c+1)),S)))
	);
    chain
    )


------------------------------------------------
---Inputs: 
-- V= list of vertices
-- F= list of facets
------------------------------------------------
---Outputs: The cellular chain complex whose homology
--- is the homology of the simplicial or polyhedral complex relative
--- to its boundary.
--To verify working correctly, try the following input.
--Cube and Octahedron--
--V={{1, 0, 0}, {-1, 0, 0}, {0, 1, 0}, {0, -1, 0}, {0, 0, 1}, {0, 0, -1}, {-2, -2, -2}, {-2, 2, -2}, {2, 2, -2}, {2, -2, -2}, {-2, -2, 2}, {-2, 2, 2}, {2, 2, 2}, {2, -2, 2}};
--F={{0, 1, 2, 3, 4, 5}, {0, 8, 9, 12, 13}, {1, 6, 7, 10, 11}, {2, 7, 8, 11, 12}, {3, 6, 9, 10, 13}, {4, 10, 11, 12, 13}, {5, 6, 7, 8, 9}, {0, 2, 8, 12}, {0, 3, 9, 13}, {0, 4, 12, 13}, {0, 5, 8, 9}, {1, 2, 7, 11}, {1, 3, 6, 10}, {1, 4, 10, 11}, {1, 5, 6, 7}, {2, 4, 11, 12}, {3, 4, 10, 13}, {3, 5, 6, 9}, {2, 5, 7, 8}, {0, 2, 4, 12}, {0, 2, 5, 8}, {0, 3, 4, 13}, {0, 3, 5, 9}, {1, 2, 4, 11}, {1, 2, 5, 7}, {1, 3, 4, 10}, {1, 3, 5, 6}};
--------------------------------------------------
--With this input, output should be an acyclic complex except for HH_3
--------------------------------------------------

cellularComplex(List,List) := ChainComplex => opts -> (V,F) -> (
    d := (# first V);
    S := createSplineRing(d,opts);
    if issimplicial(V,F) then (
	boundaryF := boundaryComplex(F);
	C := apply(d+1, i-> getCodimDFacesSimplicial(F,i));
	boundaryC := join({{}},apply(d, i-> getCodimDFacesSimplicial(boundaryF,i)));
    	intC := apply(#C, i -> select(C_i, f -> not member(f,boundaryC_i)));
    	chain := chainComplex(reverse apply(#intC-1, c-> sub(simpBoundary(intC_c,intC_(c+1)),S )))
	) else (
	bComp := boundaryComplex(V,F);
	--Construct list whose ith element is interior intersections of codim i--
	current :=F;
	intC ={current};
	scan(d,i->(
		current=select(getCodim1Intersections(current),f->(not any(bComp,F->subsetL(f,F))));
		intC =append(intC,current)
	));
    	--get the forms defining codimension 1 faces--
	fList :=formsList(V,intC_1,0,
	    Homogenize => opts.Homogenize,
	    VariableName => opts.VariableName,
	    CoefficientRing => opts.CoefficientRing,
	    BaseRing => S);
    	--create a list whose ith element is ideals of codim i faces--
	idList :={apply(F,f->ideal(0_S)),apply(fList,f->ideal f)};
	scan(d-1,i->(
		idList=append(idList,apply(intC_(i+2),G->(
			    ind:=codim1Cont(intC_1,G);
			    sub(ideal fList_ind,S)
		)))
        ));
        --set orientations of all faces (lexicographically lowest dependent
	--variables on each face)
	orList := apply(idList, L->apply(L,I->orient I));
	--set up the chain complex
	chain = chainComplex(reverse apply(#intC-1, c-> (
		    L1 := {intC_(c+1),idList_(c+1),orList_(c+1)};
		    L2 := {intC_c,idList_c,orList_c};
		    sub(polyBoundary(V,L2,L1),S)
		    )))
	);
    chain
    )



------------------------------------------
idealsComplex=method(Options=>{
	symbol Homogenize => true, 
	symbol VariableName => getSymbol "t",
	symbol CoefficientRing => QQ,
	symbol BaseRing => null
    }
    )
------------------------------------------
--This function computes the Schenck-Stillman chain complex
--of ideals for a simplicial or polyhedral complex
------------------------------------------
--Inputs:
--V: vertex list
--F: facet list
--r: desired order of smoothness
------------------------------------------
--Outputs: The Schenck-Stillman complex of ideals
------------------------------------------

idealsComplex(List,List,ZZ):=ChainComplex => opts -> (V,F,r)->(
    d := #(first V);
    S := createSplineRing(d,opts);
    if issimplicial(V,F) then (
	--list of interior faces in order of increasing codimension--
	boundaryF := boundaryComplex(F);
	C := apply(d+1, i-> getCodimDFacesSimplicial(F,i));
	boundaryC := join({{}},apply(d, i-> getCodimDFacesSimplicial(boundaryF,i)));
	intC := apply(#C, i -> select(C_i, f -> not member(f,boundaryC_i)));
	--if there are no interior faces of large codimension, get rid of the empty lists
	--intC = select(intC,L->( (length L)>0));
	--list of forms defining codim 1 interior faces
	intformslist :=formsList(V,intC_1,r,
	    Homogenize => opts.Homogenize,
	    VariableName => opts.VariableName,
	    CoefficientRing => opts.CoefficientRing,
	    BaseRing => S); 
	--list of modules which will define chain complex--
	fullmodulelist := apply(#intC,i->(
		if #(intC_i)==0 then(
		    newMod := image matrix{{0_S}};
		    )else(
		    newMod = directSum apply(intC_i,e->(
			    CE := positions(intC_1,f->subsetL(e,f));
			    sub(module ideal (intformslist_CE),S)
			    ));
		    );
		newMod
		));
	--defining the chain complex
	CCSS :=chainComplex(reverse apply(#intC-1, c-> (
		    inducedMap(fullmodulelist_(c+1),fullmodulelist_c,sub(simpBoundary(intC_c,intC_(c+1)),S))
		    ))
	    )
    	) else (
	bComp := boundaryComplex(V,F);
	--Construct list whose ith element is interior intersections of codim i--
	current :=F;
	intC ={current};
	scan(d,i->(
		current=select(getCodim1Intersections(current),f->(not any(bComp,F->subsetL(f,F))));
		intC =append(intC,current)
	));
    	--if there are no intersections of larger codimension, get rid of the empty lists
	--intC = select(intC,L->((length L)>0));    	
    	--get the forms defining codimension 1 faces--
	fList :=formsList(V,intC_1,0,
	    Homogenize => opts.Homogenize,
	    VariableName => opts.VariableName,
	    CoefficientRing => opts.CoefficientRing,
	    BaseRing => S);
	--create a list whose ith element is ideals of codim i faces--
	idList :={apply(F,f->ideal(0_S)),apply(fList,f->ideal f)};
	scan(#intC-2,i->(
		idList=append(idList,apply(intC_(i+2),G->(
			    ind:=codim1Cont(intC_1,G);
			    sub(ideal fList_ind,S)
		)))
        ));
        --set orientations of all faces (lexicographically lowest dependent
	--variables on each face)
	orList := apply(idList, L->apply(L,I->orient I));
	--set up list of forms to r+1 power--
	intformslist =apply(fList,f->f^(r+1));
	--list of modules which will define chain complex--
	fullmodulelist = apply(#intC,i->(
		if #(intC_i)==0 then(
		    newMod := image matrix{{0_S}};
		    )else(
		    newMod = directSum apply(intC_i,e->(
			    CE := positions(intC_1,f->subsetL(e,f));
			    sub(module ideal (intformslist_CE),S)
			    ));
		    );
		newMod
		));
	--set up the chain complex
	CCSS = chainComplex(reverse apply(#intC-1, c-> (
		    L1 := {intC_(c+1),idList_(c+1),orList_(c+1)};
		    L2 := {intC_c,idList_c,orList_c};
		    M := sub(polyBoundary(V,L2,L1),S);
		    inducedMap(fullmodulelist_(c+1),fullmodulelist_c,M)
		    )))
	);
    CCSS
    )


------------------------------------------
splineComplex=method(Options=>{
	symbol Homogenize => true, 
	symbol VariableName => getSymbol "t",
	symbol CoefficientRing => QQ,
	symbol BaseRing => null
    }
    )

------------------------------------------
--This function computes the Schenck-Stillman chain complex
--of quotient rings for a simplicial or polyhedral complex,
--called the spline complex
------------------------------------------
--Inputs:
--V: vertex list
--F: facet list
--r: desired order of smoothness
------------------------------------------
--Outputs: The Schenck-Stillman spline complex
------------------------------------------
--To verify working correctly, try the following input.
--Cube and Octahedron--
--V={{1, 0, 0}, {-1, 0, 0}, {0, 1, 0}, {0, -1, 0}, {0, 0, 1}, {0, 0, -1}, {-2, -2, -2}, {-2, 2, -2}, {2, 2, -2}, {2, -2, -2}, {-2, -2, 2}, {-2, 2, 2}, {2, 2, 2}, {2, -2, 2}};
--F={{0, 1, 2, 3, 4, 5}, {0, 8, 9, 12, 13}, {1, 6, 7, 10, 11}, {2, 7, 8, 11, 12}, {3, 6, 9, 10, 13}, {4, 10, 11, 12, 13}, {5, 6, 7, 8, 9}, {0, 2, 8, 12}, {0, 3, 9, 13}, {0, 4, 12, 13}, {0, 5, 8, 9}, {1, 2, 7, 11}, {1, 3, 6, 10}, {1, 4, 10, 11}, {1, 5, 6, 7}, {2, 4, 11, 12}, {3, 4, 10, 13}, {3, 5, 6, 9}, {2, 5, 7, 8}, {0, 2, 4, 12}, {0, 2, 5, 8}, {0, 3, 4, 13}, {0, 3, 5, 9}, {1, 2, 4, 11}, {1, 2, 5, 7}, {1, 3, 4, 10}, {1, 3, 5, 6}};
--r=1
--------------------------------------------------
--With this input, minimal associated primes of HH_2 should be
--ideals of every vertex in V along with ideals corresponding to (0:0:0:1),(0:0:1:0),(0:1:0:0) (there should be three ideals having t_0 as
--a generator)
--------------------------------------------------


splineComplex(List,List,ZZ):=ChainComplex => opts -> (V,F,r)->(
    d := #(first V);
    S := createSplineRing(d,opts);
    if issimplicial(V,F) then (
	--list of interior faces in order of increasing codimension--
	boundaryF := boundaryComplex(F);
	C := apply(d+1, i-> getCodimDFacesSimplicial(F,i));
	boundaryC := join({{}},apply(d, i-> getCodimDFacesSimplicial(boundaryF,i)));
	intC := apply(#C, i -> select(C_i, f -> not member(f,boundaryC_i)));
	--if there are no interior faces of large codimension, get rid of the empty lists
	--intC = select(intC,L->( (length L)>0));
	--list of forms defining codim 1 interior faces
	intformslist := formsList(V,intC_1,r,
	    Homogenize => opts.Homogenize,
	    VariableName => opts.VariableName,
	    CoefficientRing => opts.CoefficientRing,
	    BaseRing => S); 
	--list of modules which will define chain complex--
	fullmodulelist := {S^(#F)};
	scan(#intC-1,i->(
		if #(intC_(i+1))==0 then(
		    newMod := image matrix{{0_S}};
		    )else(
		    newMod = directSum apply(intC_(i+1),e->(
			    CE := codim1Cont(intC_1,e);
			    return coker sub(gens ideal (intformslist_CE),S)
			    ));
		    );
		fullmodulelist = append(fullmodulelist,newMod)
		));
	--defining the chain complex
	CCSS :=chainComplex(reverse apply(#intC-1, c-> (
		    map(fullmodulelist_(c+1),fullmodulelist_c,sub(simpBoundary(intC_c,intC_(c+1)),S))
		    ))
	    );
    	) else (
	bComp := boundaryComplex(V,F);
	--Construct list whose ith element is interior intersections of codim i--
	current :=F;
	intC ={current};
	scan(d,i->(
		current=select(getCodim1Intersections(current),f->(not any(bComp,F->subsetL(f,F))));
		intC =append(intC,current)
	));
    	--if there are no intersections of larger codimension, get rid of the empty lists
	--intC = select(intC,L->((length L)>0));
    	--get the forms defining codimension 1 faces--
	fList :=formsList(V,intC_1,0,
	    Homogenize => opts.Homogenize,
	    VariableName => opts.VariableName,
	    CoefficientRing => opts.CoefficientRing,
	    BaseRing => S);
    	--create a list whose ith element is ideals of codim i faces--
	idList :={apply(F,f->ideal(0_S)),apply(fList,f->ideal f)};
	scan(#intC-2,i->(
		idList=append(idList,apply(intC_(i+2),G->(
			    ind:=codim1Cont(intC_1,G);
			    sub(ideal fList_ind,S)
		)))
        ));
        --set orientations of all faces (lexicographically lowest dependent
	--variables on each face)
	orList := apply(idList, L->apply(L,I->orient I));
	--set up list of forms to r+1 power--
	intformslist =apply(fList,f->f^(r+1));
	--list of modules which will define chain complex--
	fullmodulelist = {S^(#F)};
	scan(#intC-1,i->(
		if #(intC_(i+1))==0 then(
		    newMod := image matrix{{0_S}};
		    )else(
		    newMod = directSum apply(intC_(i+1),e->(
			    CE := codim1Cont(intC_1,e);
			    return coker sub(gens ideal (intformslist_CE),S)
			    ));
		    );
		fullmodulelist = append(fullmodulelist,newMod)
		));
	--set up the chain complex
	CCSS = chainComplex(reverse apply(#intC-1, c-> (
		    L1 := {intC_(c+1),idList_(c+1),orList_(c+1)};
		    L2 := {intC_c,idList_c,orList_c};
		    M := sub(polyBoundary(V,L2,L1),S);
		    map(fullmodulelist_(c+1),fullmodulelist_c,M)
		    )))
	);
    CCSS
    )


------------------------------------------
--may be included in future iteration
--splineComplexMap=method(Options=>{
--	symbol Homogenize => true, 
--	symbol VariableName => getSymbol "t",
--	symbol CoefficientRing => QQ
--    }
--    )
------------------------------------------
--Builds the three complexes idealsComplex,
--cellularComplex, splineComplex, along with
--chain maps between these complexes
------------------------------------------
--Inputs:
--V: vertex list
--F: facet list
--r: desired order of smoothness
------------------------------------------
--Outputs: The three chain complexes with
--maps between them
------------------------------------------

--splineComplexMap(List,List,ZZ):=List=>opts->(V,F,r)->(
--    )
	    

------------------------------------------
codim1Cont=method()
------------------------------------------
----Inputs:
----E = list of codim one intersections
----G = a face of the complex (V,F)
------------------------------------------
----Output: Indices of E corresponding to
----- codim one faces containing G
-----------------------------------------

codim1Cont(List,List):=List=> (E,G)->(
    --positions of codim one faces containing G
    positions(E,e->subsetL(G,e))
    )


------------------------------------------
issimplicial = method()
------------------------------------------
-- Assumes that the inputted complex is pure
------------------------------------------
--Inputs:
-- V = vertex coordinates of Delta
-- F = list of facets of Delta
------------------------------------------
--Outputs:
--Boolean, if Delta is simplicial,
--checking that each facet is a simplex
--of the appropriate dimension.
------------------------------------------
issimplicial(List,List) := Boolean => (V,F) ->(
    n := #first(V);
    all(F,f->#f==(n+1))
)

-----------------------------------------

------------------------------------------
------------------------------------------
-- Documentation
------------------------------------------
------------------------------------------

beginDocumentation()

-- Front Page
doc ///
    Key
        AlgebraicSplines
    Headline
        a package for working with splines on simplicial complexes, polytopal complexes, and graphs
    Description
        Text
            This package provides methods for computations with piecewise polynomial functions (splines) over
	    polytopal complexes.
	Text
	    Let $\Delta$ be a partition (simplicial,polytopal,cellular,rectilinear, etc.) of a space $\RR^n$.
	    The spline module $S^{r}(\Delta)$ is the module of all functions $f$ continuously differentiable
	    to order $r$ such that $f$ is a polynomial when restricted to each facet $\sigma\in\Delta$.
	    The vector space $S^r_d(\Delta)$ consists of splines whose polynomial restrictions have degree at most $d$.
	    Its dimension is of particular interest in approximation theory (see [10]).
	    
	    The foundations of the algebraic approach to splines (as it applies to numerical analysis) was developed by Billera and Rose in [1],[2],[3].  
	    In particular, it is shown in [3] that dim$S^r_d(\Delta)=$dim$S^r(c\Delta)_d$, where $S^r(c\Delta)_d$
    	    is the vector space of splines of degree exactly $d$ on the cone $c\Delta$ over $\Delta$.  So
	    the statistic dim$S^r_d(\Delta)$ is the Hilbert function of $S^r(c\Delta)$.  The functions
	    @TO splineMatrix@ and @TO splineModule@ construct the matrix from [3] whose kernel is $S^r(\Delta)$, and
	    the module $S^r(\Delta)$, respectively.  The functions @TO splineDimensionTable@ and @TO hilbertComparisonTable@ display the statistic
	    dim$S^r_d(\Delta)$ and compare it to the long term dimension formula, respectively.
	    
	    In [1], Billera uses a homological approach to solve a conjecture of Strang on the dimension of the space
	    $S^1_d(\Delta)$.  The chain complex which he defines in [1] was later modified by Schenck and Stillman in [10]; we will
	    call this the Billera-Schenck-Stillman chain complex.  It has appeared in many papers due to its use in 
	    finding dim$S^r_d(\Delta)$; perhaps most notable is [11].  In [10] the chain complex is also used to compute 
	    dimension formulas in the polytopal setting.  The Billera-Schenck-Stillman complex is implemented by the method
	    @TO splineComplex@.  The Billera-Schenck-Stillman chain complex is a quotient of the cellular chain complex of
	    $\Delta$ (relative to the boundary of $\Delta$); the latter is computed by @TO cellularComplex@.  The kernel of
	    this surjection is given by @TO idealsComplex@.
	    
	    The functions mentioned thus far are concerned only with the structure of $S^r(\Delta)$ as a module over the polynomial
	    ring.  The method @TO ringStructure@ constructs $S^r(\Delta)$ as a quotient of a polynomial ring, thus recovering its
	    ring structure.  In [2], Billera shows that there is a natural isomorphism between the affine Stanley-Reisner ring $K_a[\Delta]$
	    (see explanation in documentation for @TO ringStructure@) of a simplicial complex and the ring $S^0(\Delta)$ of continuous 
	    piecewise polynomials on $\Delta$. The isomorphism identifies the variable corresponding to a vertex with the Courant function 
	    for that vertex. The method @TO courantFunctions@ computes these functions, and the method @TO stanleyReisner@ constructs
	    the isomorphism identified in [2].  Moreover, the rings $S^r(\Delta)$ live inside $S^0(\Delta)$ - this is of particular interest
	    when $\Delta$ is simplicial due to Billera's result.  The method @TO stanleyReisnerPresentation@ constructs $S^r(\Delta)$ as a
	    quotient of a ring map from a polynomial ring into $S^0(\Delta)$; in particular the generators of $S^r(\Delta)$ are presented
	    in terms of generators of $S^0(\Delta)$.  If $\Delta$ is simplicial these are selected to be the Courant functions.
	    
	    In topology, splines arise as equivariant cohomology of spaces with a torus action via GKM theory - see [14] for a survey of how
	    this relates to splines in numerical analysis; and [6] for the precise relationship between continuous splines and the equivariant
	    Chow cohomology of toric varieties.  From this perspective, the notion of generalized splines on graphs was introduced in [5].  The
	    method @TO generalizedSplines@ computes splines in this more flexible setting.  The relationship to splines in numerical analysis is
	    via the dual graph described by Rose in [7],[8].
	    
	    Additionally, there are connections between splines and the module of multi-derivations of a hyperplane arrangement.  A basic structural
	    connection was noticed in [3].  For the braid arrangement and its sub-arrangements, the module of derivations is isomorphic to a ring of
	    splines in a natural way (see [11] and [4]).
	    
            Methods in this package borrow from code written by Hal Schenck.
	    
	    References:\break
	    [1] Louis J. Billera. Homology of smooth splines: generic triangulations and a conjecture of Strang. Trans. Amer. Math. Soc., 310(1):325–340, 1988.\break
    	    [2] Louis J. Billera, The Algebra of Continuous Piecewise Polynomials, Adv. in Math. 76, 170-183 (1989).\break
    	    [3] Louis J. Billera and Lauren L. Rose. A dimension series for multivariate splines. Discrete Comput. Geom., 6(2):107–128, 1991.\break
    	    [4] Michael DiPasquale. Generalized Splines and Graphic Arrangements. J. Algebraic Combin. 45  (2017),  no. 1, 171-189.\break
	    [5] Simcha Gilbert, Julianna Tymoczko, and Shira Viel. Generalized splines on arbitrary graphs. Pacific J. Math.  281  (2016),  no. 2, 333-364.\break
    	    [6] Sam Payne, Equivariant Chow cohomology of toric varieties, Math. Res. Lett. 13 (2006), 29-41.\break
	    [7] Lauren Rose, Combinatorial and topological invariants of modules of piecewise polynomials, Adv. Math. 116 (1995), 34-45.\break
	    [8] Lauren Rose, Graphs, syzygies, and multivariate splines, Discrete Comput. Geom. 32 (2004), 623-637.\break
	    [9] T. McDonald, H. Schenck, Piecewise polynomials on polyhedral complexes, Adv. in Appl. Math. 42 (2009), 82-93.\break
	    [10] Hal Schenck and Mike Stillman. Local cohomology of bivariate splines. J. Pure Appl. Algebra,117/118:535–548, 1997. Algorithms for algebra (Eindhoven, 1996).\break
	    [11] Hal Schenck, A Spectral Sequence for Splines, Adv. in Appl. Math. 19, 183-199 (1997).\break 
    	    [12] Schenck, Hal . Splines on the Alfeld split of a simplex and type A root systems. J. Approx. Theory  182  (2014), 1-6.\break
	    [13] Gilbert Strang, Piecewise Polynomials and the Finite Element Method, Bull. Amer. Math. Soc. 79 (1973) 1128-1137.\break
	    [14] Julianna Tymoczko. Splines in geometry and topology.  Comput. Aided Geom. Design  45  (2016), 32-47.
    	    
///

------------------------------------------
-- Data type & constructor
------------------------------------------

-- Spline Matrix Constructor
doc ///
    Key
        splineMatrix
	(splineMatrix,List,List,ZZ)
	(splineMatrix,List,List,List,ZZ)
	(splineMatrix,List,ZZ)
	InputType
	ByFacets
	ByLinearForms
	Homogenize
	BaseRing
	VariableName
    Headline
        compute matrix whose kernel is the module of $C^r$ splines on $\Delta$
    Usage
    	S = splineMatrix(V,F,E,r)
	S = splineMatrix(V,F,r)
	S = splineMatrix(L,r)
	S = splineMatrix(B,H,r)
    Inputs
    	V:List
	    list of coordinates of vertices of $\Delta$
	F:List
	    list of facets of $\Delta$ (each facet is recorded as a list of indices of vertices taken from V)
	E:List
	    list of codimension one faces of $\Delta$ (interior or not); each codimension one face is recorded as a list of indices of vertices taken from V
	r:ZZ
	    degree of desired continuity
	L:List
	    list $\{V,F,E\}$ of vertices, facets, and codimension one faces of $\Delta$
	H:List
	    list of forms defining codimension one faces of $\Delta$
	InputType=>String
	
	BaseRing=>Ring
	    
	Homogenize=>Boolean
	
	CoefficientRing=>Ring
	
	VariableName=>Symbol
	
    Outputs
    	S:Matrix
	  resulting matrix whose kernel is the spline module
    Description
        Text
	    This creates the basic spline matrix that has splines as
	    its kernel.
	Example
	    V = {{0,0},{1,0},{1,1},{-1,1},{-2,-1},{0,-1}};-- the coordinates of vertices
            F = {{0,2,1},{0,2,3},{0,3,4},{0,4,5},{0,1,5}};  -- a list of facets (pure complex)
            E = {{0,1},{0,2},{0,3},{0,4},{0,5}};   -- list of edges
    	    splineMatrix(V,F,E,1)
        Text
	    If each codimension one face of $\Delta$ is the intersection of exactly two facets, then the list of
	    edges is unnecessary.
	Example
	    V = {{0,0},{1,0},{1,1},{-1,1},{-2,-1},{0,-1}};-- the coordinates of vertices
            F = {{0,2,1},{0,2,3},{0,3,4},{0,4,5},{0,1,5}};  -- a list of facets (pure complex)
    	    splineMatrix(V,F,1)
	Text
	    Splines are automatically computed on the cone over the given complex $\Delta$, and the last variable of the polynomial ring is always the variable used to homogenize.  If the user desires splines over $\Delta$,
	    use the option Homogenize=>false.
	Example
	    V = {{0,0},{1,0},{1,1},{-1,1},{-2,-1},{0,-1}};-- the coordinates of vertices
            F = {{0,2,1},{0,2,3},{0,3,4},{0,4,5},{0,1,5}};  -- a list of facets (pure complex)
    	    splineMatrix(V,F,1,Homogenize=>false)
	Text
	    If the user would like to define the underlying ring (e.g. for later reference), this may be done
	    using the option BaseRing=>R, where R is a polynomial ring defined by the user.
	Example
	    V = {{0,0},{1,0},{1,1},{-1,1},{-2,-1},{0,-1}};-- the coordinates of vertices
            F = {{0,2,1},{0,2,3},{0,3,4},{0,4,5},{0,1,5}};  -- a list of facets (pure complex)
	    R = QQ[x,y] --desired polynomial ring
    	    splineMatrix(V,F,1,Homogenize=>false,BaseRing=>R)
	Text
	    Here is an example where the output is homogenized.  Notice that homogenization occurs with respect to the last variable
	    of the polynomial ring.
	Example
	    V = {{0,0},{1,0},{1,1},{-1,1},{-2,-1},{0,-1}};-- the coordinates of vertices
            F = {{0,2,1},{0,2,3},{0,3,4},{0,4,5},{0,1,5}};  -- a list of facets (pure complex)
	    R = QQ[x,y,z] --desired polynomial ring
    	    splineMatrix(V,F,1,BaseRing=>R)
	Text
            Alternately, the spline matrix can be created directly from the
	    dual graph (with edges labeled by linear forms).  Note: This way of
	    entering data requires the ambient polynomial ring to be defined.  See also
	    the @TO generalizedSplines@ method.
	Example
	    R = QQ[x,y]
	    B = {{0,1},{1,2},{2,3},{3,4},{4,0}}
	    H = {x-y,y,x,y-2*x,x+y}
	    splineMatrix(B,H,1,InputType=>"ByLinearForms")

///

doc ///
    Key
        splineModule
	(splineModule,List,List,List,ZZ)
	(splineModule,List,List,ZZ)
    Headline
        compute the module of all splines on partition of a space
    Usage
        M = splineModule(V,F,E,r)
	M = splineModule(V,F,r)
    Inputs
        V:List
	    list of coordinates of vertices of $\Delta$
	F:List
	    list of facets of $\Delta$; each facet is recorded as a list of indices of vertices taken from V
	E:List
	    list of codimension one faces of $\Delta$ (interior or not); each codimension one face is recorded as a list of indices of vertices taken from V
	r:ZZ
	    desired degree of smoothness
	BaseRing=>Ring
	    
	Homogenize=>Boolean
	
	CoefficientRing=>Ring
	
	VariableName=>Symbol
	
	InputType=>String
	
    Outputs
        M:Module
	    module of splines on $\Delta$
    Description
        Text
	    This method returns the spline module.  It is presented as the image of a matrix
	    whose columns generate splines as a module over the polynomial ring.  Each column
	    represents a spline whose entries are the polynomials restricted to the facets of the
	    complex $\Delta$.
	Example
	    V = {{0,0},{1,0},{1,1},{0,1}}
	    F = {{0,1,2},{0,2,3}}
	    E = {{0,1},{0,2},{0,3},{1,2},{2,3}}
	    splineModule(V,F,E,1)
    SeeAlso
    	splineMatrix
	
	
///

doc ///
    Key
        splineDimensionTable
	(splineDimensionTable,ZZ,ZZ,Module)
	(splineDimensionTable,ZZ,ZZ,List,ZZ)
    Headline
        a table with the dimensions of the graded pieces of a graded module
    Usage
        T=splineDimensionTable(a,b,M)
	T=splineDimensionTable(a,b,L,r)
    Inputs
        a:ZZ
	    lowest degree in the table
	b:ZZ
	    largest degree in the table
	M:Module
	    graded module
	L:List
	    a list {V,F,E} of the vertices, facets and codimension one faces of a polyhedral complex
	r:ZZ
	    degree of smoothnes 

    Outputs
        T:Table
	    T= table with the dimensions of the graded pieces of M in the range a,b
    Description
        Text
	    The output table gives you the dimensions of the graded pieces
	    of the module M where the degree is between a and b. 
	Example
	    V = {{0,0},{1,0},{1,1},{0,1}};
	    F = {{0,1,2},{0,2,3}};
	    E = {{0,1},{0,2},{0,3},{1,2},{2,3}};
	    M=splineModule(V,F,E,2);
	    splineDimensionTable(0,8,M)
    	Text
	    The table above records the dimensions dim$S^2_d(\Delta)$ (i.e. splines on $\Delta$ of smoothness 2
	    and degree at most d) for $d=$0,..,8.
	Text
	    You may instead input the list L={V,F,E} (or L={V,F}) of the vertices, facets and codimension one faces of the complex $\Delta$.
	Example
	    V = {{0,0},{1,0},{1,1},{0,1}};
	    F = {{0,1,2},{0,2,3}};
	    L = {V,F,E};
	    splineDimensionTable(0,8,L,2)
	Text
	    The following complex, known as the Morgan-Scot partition, illustrates the subtle changes in dimension of spline spaces 
	    which may occur depending on geometry.
    	Example
	    V = {{-1,-1},{1,-1},{0,1},{10,10},{-10,10},{0,-10}};
	    V'= {{-1,-1},{1,-1},{0,1},{10,10},{-10,10},{1,-10}};
	    F = {{0,1,2},{2,3,4},{0,4,5},{1,3,5},{1,2,3},{0,2,4},{0,1,5}};
	    M = splineModule(V,F,1);
	    M' = splineModule(V',F,1);
	    splineDimensionTable(0,4,M)
	    splineDimensionTable(0,4,M')
	Text
	    Notice that the dimension of the space of $C^1$ quadratic splines changes depending on the geometry of $\Delta$.
///

doc ///
    Key
        postulationNumber
	(postulationNumber,Module)
    Headline
        computes the largest degree at which the hilbert function of the graded module M is not equal to the hilbertPolynomial
    Usage
        v = postulationNumber(M)
    Inputs
        M:Module
	    graded module
    Outputs
        v:ZZ
	    largest degree at which the hilbert function of the graded module M is not equal to the hilbertPolynomial
    Description
        Text
	    This function computes the postulation number of M which is defined as the
	    largest degree at which the hilbert function of the graded module M is not equal to the hilbertPolynomial
	Example
	    V = {{0,0},{1,0},{1,1},{0,1}};
	    F = {{0,1,2},{0,2,3}};
	    E = {{0,1},{0,2},{0,3},{1,2},{2,3}};
	    M = splineModule(V,F,E,2)
	    postulationNumber(M)
	    
///
        

doc ///
    Key
        hilbertComparisonTable
	(hilbertComparisonTable,ZZ,ZZ,Module)
    Headline
        a table to compare the values of the hilbertFunction and hilbertPolynomial of a graded module
    Usage
        T = hilbertComparisonTable(a,b,M)
    Inputs
        a:ZZ
	    lowest degree in the  table
	b:ZZ
	    largest degree in the table
	M:Module
	    graded module
    Outputs        
	T:Table
	    table with the degrees and values of the hilbertFunction and hilbertPolynomial
    Description
        Text
	    The first row of the output table contains the degrees, the second row contains the 
	    values of the hilbertFunction, the third row contains the values of the hilbertPolynomial.
	    In the following example, the hilbertFunction and polynomial always agree.
	Example
	    V = {{0,0},{1,0},{1,1},{0,1}}
	    F = {{0,1,2},{0,2,3}}
	    E = {{0,1},{0,2},{0,3},{1,2},{2,3}}
	    hilbertComparisonTable(0,8,splineModule(V,F,E,1))
    	Text
	    The dimension of splines of degree at most d on a complex $\Delta$ is eventually given by a polynomial in d,
	    which is the Hilbert polynomial of the spline module.  Below we illustrate with the Morgan-Scot partition.  Notice
	    that the Hilbert polynomial for the symmetric Morgan-Scot partition and the asymmetric Morgan-Scot partition are the
	    same, however the Hilbert functions disagree in degree 2.
	Example
	    V = {{-1,-1},{1,-1},{0,1},{10,10},{-10,10},{0,-10}};
	    V'= {{-1,-1},{1,-1},{0,1},{10,10},{-10,10},{1,-10}};
	    F = {{0,1,2},{2,3,4},{0,4,5},{1,3,5},{1,2,3},{0,2,4},{0,1,5}};
	    M = splineModule(V,F,1);
	    hilbertPolynomial(M,Projective=>false)
	    M' = splineModule(V',F,1);
	    hilbertPolynomial(M',Projective=>false)
	    hilbertComparisonTable(0,4,M)
	    postulationNumber(M) --final integer for which hilbert function and polynomial disagree
	    hilbertComparisonTable(0,4,M')
	    postulationNumber(M')
    	Text
	    In the following example, we compare the hilbert polynomial and hilbert function of splines over a centrally triangulated
	    octahedron; the behavior is very similar to the Morgan-Scot partition, except there is an extra degree of symmetry 
	    available which alters the Hilbert polynomials.  Notice the use of the option Homogenize=>false to consider splines 
	    of degree precisely d instead of splines of degree at most d.
	Example
	    V={{0,0,0},{1,0,0},{0,1,0},{0,0,1},{-1,0,0},{0,-1,0},{0,0,-1}}; --most symmetric variant
	    V'={{0,0,0},{0,2,-1},{-1,-1,-1},{1,-1,-1},{0,-2,2},{1,1,2},{-1,1,2}}; --somewhat symmetric variant
	    V''={{0,0,0},{10,1,1},{-1,10,1},{-1,1,10},{-10,1,-1},{1,-10,1},{-1,-1,-10}}; --asymmetric variant
	    F={{0,1,2,3},{0,1,2,6},{0,1,3,5},{0,1,5,6},{0,2,3,4},{0,2,4,6},{0,3,4,5},{0,4,5,6}};
	    M=splineModule(V,F,1,Homogenize=>false);
	    hilbertPolynomial(M,Projective=>false)
	    M'=splineModule(V',F,1,Homogenize=>false);
	    hilbertPolynomial(M',Projective=>false)
	    M''=splineModule(V'',F,1,Homogenize=>false);
	    hilbertPolynomial(M'',Projective=>false)
	    hilbertComparisonTable(0,6,M)
	    postulationNumber(M) --largest integer for which hilbert function and polynomial disagree
	    hilbertComparisonTable(0,6,M')
	    postulationNumber(M')
	    hilbertComparisonTable(0,6,M'')
	    postulationNumber(M'')
///

doc ///
    Key
    	formsList
	(formsList,List,List,ZZ)
    Headline
    	list of powers of (affine) linear forms cutting out a specified list of codimension one faces.
    Usage
    	L = formsList(V,E,r)
    Inputs
    	V:List
	    list of coordinates of vertices
	E:List
	    list of codimension 1 faces (each codimension 1 face is recorded as a list of indices of vertices taken from V)
	r:ZZ
	    desired degree of smoothness
	BaseRing=>Ring
	    
	Homogenize=>Boolean
	
	CoefficientRing=>Ring
	
	VariableName=>Symbol
	
	InputType=>String

    Outputs
    	L:List
	    L = list of (affine) linear forms cutting out codimension one
	    faces specified by E, raised to the power (r+1)
    Description
    	Text
	    This method returns a list of (affine) linear forms cutting out codimension one faces, raised to the power (r+1).
	Example
	    V = {{0,0},{1,0},{1,1},{0,1}};
	    E = {{0,1},{0,2},{0,3},{1,2},{2,3}};
	    formsList(V,E,0)
	    S=QQ[x,y];--can specify the polynomial ring to use, and whether to homogenize
	    formsList(V,E,0,BaseRing=>S,Homogenize=>false)

///

doc ///
    Key
    	generalizedSplines
	(generalizedSplines,List,List)
	RingType
    Headline
    	the module of generalized splines associated to a simple graph with an edge labelling
    Usage
    	M = generalizedSplines(E,I)
--	M = generalizedSplines(G,I)
    Inputs
    	E:List
	    list of edges of a graph (an edge is represented as a list with two elements)
	I:List
	    list of ideals in a ring
	RingType=>ZZ
	
    Outputs
    	M:Module
	    module of generalized splines on the graph with edges E and edge labels I
    Description
    	Text
	    This method returns the module of generalized splines on a graph with edgeset E on v vertices,
	    whose edges are labelled by ideals of some ring R.  By definition this is the 
	    submodule of $R^v$ consisting of tuples of polynomials such that the difference of
	    polynomials corresponding to adjacent vertices are congruent module the ideal labelling
	    the edge between them.
	Example
	    S = QQ[x_0,x_1,x_2]; --the underlying ring
	    E = {{0,1},{1,2},{0,2}} --edges of the graph (in this case a triangle)
	    I = {x_0-x_1,x_1-x_2,x_2-x_0} --ideals of S (elements of S are interpreted as principal ideals)
	    generalizedSplines(E,I) --in this case this is the module of derivations on the $A_2$ arrangement
--	Text
--	    The input can also be a graph.  Care must be taken that ideals in the list I are listed in the
--	    same order edges(G).
--	Example
--	    G = completeGraph(4);
--	    S = QQ[apply(vertices G,i->x_i)];
--	    I = apply(edges G,e->(E=elements(e); x_(E_0)-x_(E_1)) );
--	    generalizedSplines(G,I) --module of derivations on $A_3$
	Text
	    If edge labels are integers, generalizedSplines is computed as a ZZ module by default.
	Example
	    E={{0,1},{1,2},{2,3},{0,3}};
	    I={3,4,5,6};
	    generalizedSplines(E,I)
	Text
	    The above splines may also be computed over ZZ modulo some integer.
	Example
	    E={{0,1},{1,2},{2,3},{0,3}};
	    I={3,4,5,6};
	    generalizedSplines(E,I,RingType=>9) --computes spline module with underlying ring ZZ/9
	Text
	    Arbitrary ideals may also be entered as edge labels.
	Example
	    S=QQ[x,y,z]
	    E={{1,2},{2,3},{3,4}}
	    I={ideal(x,y),ideal(y),ideal(z)}
	    generalizedSplines(E,I)
	Text
	    This method can be used to compute splines over non-linear partitions.  The example below can be found in Exercise 13 of Section 8.3 in
	    the book Using Algebraic Geometry by Cox,Little, and O'Shea.
	Example
	    E={{0,1},{1,2},{0,2}};
	    S=QQ[x,y];
	    I={y-x^2,x+y^2,y-x^3};--these three curves meet at the origin 
	    generalizedSplines(E,I)--this is the module of C^0 splines on the partition
///

doc ///
    Key
    	cellularComplex
	(cellularComplex,List,List)
	(cellularComplex,List)
    Headline
    	create the cellular chain complex whose homologies are the singular homologies of the complex $\Delta$ relative to its boundary
    Usage
    	C = cellularComplex(F,InputType=>Simplicial)  --for use only if $\Delta$ is simplicial
    	C = cellularComplex(V,F)
    Inputs
    	V:List
	    list of coordinates of vertices of $\Delta$
	F:List
	    list of facets of $\Delta$ (each facet is recorded as a list of indices of vertices taken from V)
	BaseRing=>Ring
	    
	Homogenize=>Boolean
	
	CoefficientRing=>Ring
	
	VariableName=>Symbol
	
	InputType=>String
	
    Outputs
    	C:ChainComplex
	    cellular chain complex of $\Delta$ relative to its boundary
    Description
    	Text
	    This method returns the cellular chain complex of $\Delta$ relative to its
	    boundary.  If $\Delta$ is homeomorphic to a disk, the homologies will vanish
	    except for the top dimension.
	Example
	    V = {{0,0},{1,0},{0,1},{-1,-1}};
     	    F = {{0,1,2},{0,2,3},{0,1,3}};
	    C = cellularComplex(V,F)
	    prune HH C
	Text
	    If the complex is simplicial, there is no need for a vertex list.
	Example
	    F = {{0,1,2},{0,1,3},{0,2,3}};
	    C = cellularComplex(F,InputType=>"Simplicial")
	Text
	    Arbitrary dimensions and polyhedral input are allowed.
	Example
	    V = {{1, 0, 0}, {-1, 0, 0}, {0, 1, 0}, {0, -1, 0}, {0, 0, 1}, {0, 0, -1}, {-2, -2, -2}, {-2, 2, -2}, {2, 2, -2}, {2, -2, -2}, {-2, -2, 2}, {-2, 2, 2}, {2, 2, 2}, {2, -2, 2}};
	    F = {{0, 1, 2, 3, 4, 5}, {0, 8, 9, 12, 13}, {1, 6, 7, 10, 11}, {2, 7, 8, 11, 12}, {3, 6, 9, 10, 13}, {4, 10, 11, 12, 13}, {5, 6, 7, 8, 9}, {0, 2, 8, 12}, {0, 3, 9, 13}, {0, 4, 12, 13}, {0, 5, 8, 9}, {1, 2, 7, 11}, {1, 3, 6, 10}, {1, 4, 10, 11}, {1, 5, 6, 7}, {2, 4, 11, 12}, {3, 4, 10, 13}, {3, 5, 6, 9}, {2, 5, 7, 8}, {0, 2, 4, 12}, {0, 2, 5, 8}, {0, 3, 4, 13}, {0, 3, 5, 9}, {1, 2, 4, 11}, {1, 2, 5, 7}, {1, 3, 4, 10}, {1, 3, 5, 6}};
	    C = cellularComplex(V,F);
	    prune HH C
    SeeAlso
        idealsComplex
	splineComplex
 
 ///
 
 
doc ///
    Key
    	idealsComplex
	(idealsComplex, List,List,ZZ)
    Headline
    	creates the Billera-Schenck-Stillman chain complex of ideals
    Usage
    	C = idealsComplex(V,F,r)
    Inputs
    	V:List
	    list of vertex coordinates of $\Delta$
	F:List
	    list of facets of $\Delta$ (each facet is recorded as a list of indices of vertices taken from V)
	r:ZZ
	    integer, desired degree of smoothness
	BaseRing=>Ring
	    
	Homogenize=>Boolean
	
	CoefficientRing=>Ring
	
	VariableName=>Symbol
	
    Outputs
    	C:ChainComplex
	    Billera-Schenck-Stillman chain complex of ideals
    Description
    	Text
	    This method returns the Billera-Schenck-Stillman chain complex of ideals whose
	    top homology is the module of non-trivial splines on $\Delta$.
	Example
	    V = {{0,0},{1,0},{0,1},{-1,-1}};
	    F = {{0,1,2},{0,2,3},{0,1,3}};
	    C = idealsComplex(V,F,1);
	    prune HH C
	Text
	    The output from the above example shows that there is only one nonvanishing
	    homology, and it is free as a module over the polynomial ring in three variables.
	Example
	    V = {{-1,-1},{1,-1},{0,1},{-2,-2},{2,-2},{0,2}};
	    F = {{0,1,2},{0,1,3,4},{1,2,4,5},{0,2,3,5}};
	    C = idealsComplex(V,F,1);
	    prune HH C
	Text
	    The output from the above example shows that there are two nonvanishing homologies,
	    but the spline module, which is (almost) the homology HH_1, is still free.  This shows
	    that freeness of the spline module does not depend on vanishing of lower homologies if
	    the underlying complex is polyhedral.
    SeeAlso
	cellularComplex
	splineComplex

///

doc ///
    Key
    	courantFunctions
	(courantFunctions, List,List)
    Headline
    	returns the Courant functions of a simplicial complex
    Usage
    	M=courantFunctions(V,F)
    Inputs
    	V:List
	    a list of vertex coordinates
	F:List
	    a list of facets, recorded as indices of V
	BaseRing=>Ring
	    
	Homogenize=>Boolean
	
	CoefficientRing=>Ring
	
	VariableName=>Symbol
	
	InputType=>String
	
    Outputs
    	M:Matrix
	    a matrix so that column $i$ is the Courant function corresponding to vertex $V_i$
    Description
    	Text
	    This method returns a matrix with as many rows as facets and as many columns as vertices.  Column $i$
	    of the matrix is the Courant function corresponding to vertex $V_i$.  This is the piecewise linear function
	    which takes the value $1$ at the vertex $V_i$ and $0$ at all other vertices.
	Example
	    V={{0,0},{0,1},{-1,-1},{1,0}};
	    F={{0,1,2},{0,2,3},{0,1,3}};
	    courantFunctions(V,F)
	Text
	    If the option Homogenize=>false is given, the Courant function corresponding to a cone vertex (if there is
	    one) will be discarded.
	Example
	    S=QQ[x,y];
	    courantFunctions(V,F,Homogenize=>false,BaseRing=>S)
	Text
	    The Courant functions are used to construct the Stanley Reisner ring of a simplicial complex.  See @TO stanleyReisner@.
	    
    SeeAlso
        stanleyReisner
	ringStructure
	stanleyReisnerPresentation
///

doc ///
    Key
    	stanleyReisner
	(stanleyReisner, List, List)
    Headline
    	Creates a ring map whose image is the ring of piecewise continuous polynomials on $\Delta$.  If $\Delta$ is simplicial, the Stanley Reisner ring of $\Delta$ is returned.
    Usage
    	phi=stanleyReisner(V,F)
    Inputs
    	V:List
	    a list of vertex coordinates
	F:List
	    a list of facets, recorded as indices of V
	BaseRing=>Ring
	    
	Homogenize=>Boolean
	
	CoefficientRing=>Ring
	
	VariableName=>Symbol
	
	InputType=>String
	
    Outputs
    	phi:RingMap
	    a ring map whose image is the ring of piecewise polynomials on $\Delta$.    
    Description
    	Text
	    This method returns the ring of continuous piecewise polynomials on the complex $\Delta$ with vertices
	    $V$ and facets $F$.  If $\Delta$ is a simplicial complex, write $\Delta_0$ for its vertices and $K[\Delta]$ for the Stanley-Reisner ring of $\Delta$, which is the quotient of the polynomial 
	    ring $K[X_v:v\in\Delta_0]$ by monomials corresponding to non-faces.  Then, as a ring, $C^0(\Delta)$ is isomorphic to the 
	    so-called affine Stanley Reisner ring $K_a[\Delta]=\frac{K[\Delta]}{(1-\sum_{v} X_v)K[\Delta]}$.  If $\Delta$ is a cone, then $K_a[\Delta]=K[\Delta_{dc}]$, where
	    $\Delta_{dc}$ is the decone of $\Delta$.  The map $K_a[\Delta]\rightarrow C^0(\Delta)$ is given by $X_v\rightarrow C_v$, where $C_v$ is the Courant function at the vertex $v$.  See @TO courantFunctions@.
	Example
	    V={{0,0},{0,1},{-1,-1},{1,0}};
	    F={{0,1,2},{0,2,3},{0,1,3}};
	    phi=stanleyReisner(V,F)
	    ker phi--decone of homogenized simplicial complex is three triangles meeting at a vertex
	Example
	    R=QQ[x,y];
	    phi=stanleyReisner(V,F,BaseRing=>R,Homogenize=>false)
	    ker phi--decone of simplicial complex is a three-cycle
	Text
	    The Stanley Reisner ring of a simpicial complex is obtained using the function @TO ringStructure@, where the
	    ring generators are chosen to be the Courant functions.
	Example
	    V={{0,0},{0,1},{-1,-1},{1,0}};
	    F={{0,1,2},{0,2,3},{0,1,3}};
	    R=QQ[x,y];
	    CF = courantFunctions(V,F,Homogenize=>false,BaseRing=>R);
	    phi=ringStructure(image CF,VariableGens=>false)
	    ker phi
	Text
	    If $\Delta$ is not simplicial, then the image of the map is a minimal set of generators for the
	    ring $C^0(\Delta)$.  There is no canonical choice of generators, and $C^0(\Delta)$ is no longer
	    a combinatorial object.
	Example
	    V={{0,1},{-1,-1},{1,-1},{0,10},{-2,-2},{2,-2}};--symmetric triangular prism
	    V'={{0,1},{-1,-1},{1,-1},{1,10},{-2,-2},{2,-2}};--asymmetric triangular prism
	    F={{0,1,2},{0,1,3,4},{0,2,3,5},{1,2,4,5}};
	    S=QQ[x,y,z];
	    phi=stanleyReisner(V,F,BaseRing=>S) --four generators in degree one
	    phi'=stanleyReisner(V',F,BaseRing=>S) --six generators in degrees one and two
	    ker phi --kernel generated by single polynomial of degree four
	    mingens ker phi' --kernel has six minimal generators of degree four
    SeeAlso
        courantFunctions
        ringStructure
	stanleyReisnerPresentation
///

doc ///
    Key
    	ringStructure
	(ringStructure, Module)
	GenVar
	IdempotentVar
	Trim
	VariableGens
    Headline
    	given a sub-module of a free module (viewed as a ring with direct sum structure) which is also a sub-ring, creates a ring map whose image is the module with its ring structure
    Usage
    	phi=ringStructure(M)
    Inputs
    	M:Module
	    a sub-module of a free module over a polynomial ring which is also a sub-ring
	GenVar=>Symbol
	    
	IdempotentVar=>Symbol
	
	Trim=>Symbol
	
	VariableGens=>Symbol
	
    Outputs
    	phi:RingMap
	    map of rings whose image is the module M with its ring structure, assuming M contains the identity element
    Description
    	Text
	    Let M be a sub-module of the free module $R^k$, where $R=K[x_0,\ldots,x_n]$ for some field K.  We view $R^k=Re_0+Re_1+\cdots+Re_{k-1}$ as a ring with 
	    component-wise multiplication (i.e. $e_0,\ldots,e_{k-1}$ are idempotents).  Suppose further that the identity element of $R^k$, namely $e=e_0+\cdots+e_{k-1}$,
	    is an element of M.  Then the M is also a ring; this method creates a map whose image is M with its ring structure.
	Example
	    V={{0,1},{1,0},{0,-1},{-1,0}};
	    F={{0,1,2},{0,2,3}};--two triangles meeting along an edge
	    R=QQ[x,y];
	    M=splineModule(V,F,1,BaseRing=>R,Homogenize=>false);--create spline module
	    phi=ringStructure(M)
	Text
    	    The target of the map is $R^k$ as a ring.
	Example
	    T=target phi; 
	    describe T --direct sum of two copies of R, one for each face; e_i corresponds to the face F_i    
	Text
	    As above, write $e$ for the identity of $R^k$.  Then the generators for $M$ as a sub-ring of $R^k$ are the generators of $M$ as an $R$-module (other than $e$), together with 
	    the generators $x_0e,x_1e,\ldots,x_ne$.  The source of the map created by ringStructure(M) is a polynomial ring in variables $Y_0,\ldots,Y_n,Y_{n+1},\cdots,Y_m$, where $Y_0,\cdots,Y_n$ corresond to the 
	    generators $x_0e,\ldots,x_ne$ and $Y_{n+1},\cdots,Y_m$ correspond to the usual generators of M (except for e, if it happens to be a generator of M).
	Example
	    H=source phi;
	    describe H --polynomial ring in three variables, one for each generator of R and each (non-identity) generator of M
	    scan(3,i->print phi(H_i))--these are the splines the variables map to  
	Text
	    The kernel of the map created by ringStructure(M) is the ideal of relations on the ring generators.  If M is graded, the grading on the source
	    of the map matches up with the grading on M.
	Example
	    degrees H --degrees of variables are same as degrees of generators of M
	    I=ker phi--the ideal of relations among generators
	    reduceHilbert(hilbertSeries (H/I))--cokernel of phi has the same graded structure as M
	    reduceHilbert(hilbertSeries M)
	Text
	    It often happens that module generators are redundant as ring generators.  These can be eliminated with the option
	    Trim=>true.  The remaining variables in the source ring retain their indexing from the output of ringStructure without
	    the Trim=>true option.
	Example
	    V={{0,0},{0,1},{-1,-1},{1,0}};
	    F={{0,1,2},{0,2,3},{0,1,3}};--three triangles meeting at (and surrounding) a vertex
	    R=QQ[x,y];
	    M=splineModule(V,F,0,BaseRing=>R,Homogenize=>false);
	    phi = ringStructure(M);
	    ker phi --notice a generator involves an isolated variable
	    phi' = ringStructure(M,Trim=>true);
	    ker phi' --now all variables correspond to irredundant generators	    
	Text
	    If $\Delta$ is a simplicial complex, write $\Delta_0$ for its vertices and $K[\Delta]$ for the Stanley-Reisner ring of $\Delta$, which is the quotient of the polynomial 
	    ring $K[X_v:v\in\Delta_0]$ by monomials corresponding to non-faces.  Then, as a ring, $C^0(\Delta)$ is isomorphic to the 
	    so-called affine Stanley Reisner ring $\frac{K[\Delta]}{(1-\sum_{v} X_v)K[\Delta]}$.  If $\Delta$ is a cone, as above, then the affine 
	    Stanley Reisner ring is just the Stanley Reisner ring of the decone of $\Delta$.  In the above example the decone is an empty triangle, so $C^0(\Delta)$ should be
	    isomorphic to a polynomial ring in three variables modulo the ideal generated by the product of the variables.  The isomorphism can be seen by the following change of
	    variables.  Alternatively, the ring structure of $C^0(\Delta)$ with this preferred set of generators is obtained with the command @TO stanleyReisner@.
	Example
	    f = (ker phi')_0
	    sub(f,{Y_0=>Y_0-Y_1,Y_1=>Y_0-Y_2,Y_2=>Y_0})
	Text
	    To obtain the sub-ring of $R^k$ which is generated by a specific set of module generators of M, i.e. without adding $x_0e,\ldots,x_ne$ as ring generators,
	    use the option VariableGens=>false.  This may be done if the user has a particular choice of ring generators they would like to use - for instance
	    the Courant functions.  See @TO courantFunctions@.
	Text
	    We may also use ringStructure to create the ring of splines for more general spline modules created using the @TO generalizedSplines@ method.
	Example
	    E={{0,1},{1,2},{0,2}};
	    S=QQ[x,y];
	    I={y-x^2,x+y^2,y-x^3};
	    C0=generalizedSplines(E,I);--splines on a non-linear partition
	    ringStructure(C0)
    Caveat
        The Trim option will not work for quotients of polynomial rings over ZZ or ZZ modulo a non-prime integer.
    SeeAlso
    	courantFunctions
	stanleyReisner
	stanleyReisnerPresentation
///

doc ///
    Key
    	stanleyReisnerPresentation
	(stanleyReisnerPresentation, List,List,ZZ)
    Headline
    	creates a ring map whose image is the sub-ring of $C^0(\Delta)$ generated by $C^r(\Delta)$.  If $\Delta$ is simplicial, $C^0(\Delta)$ is the Stanley Reisner ring of $\Delta$.
    Usage
    	phi = stanleyReisnerPresentation(V,F,r)
    Inputs
    	V:List
	    list of vertex coordinates of $\Delta$
	F:List
	    list of facets of $\Delta$ (each facet is recorded as a list of indices of vertices taken from V)
	r:ZZ
	    integer, desired degree of smoothness
	BaseRing=>Ring
	    
	Homogenize=>Boolean
	
	CoefficientRing=>Ring
	
	VariableName=>Symbol
	
	GenVar=>Symbol
	    
	IdempotentVar=>Symbol
	
	Trim=>Symbol
	
	VariableGens=>Symbol
	
	InputType=> String
    
    Outputs
    	phi:RingMap
	    ring map whose image is $C^r(\Delta)$ as a sub-ring of $C^0(\Delta)$
    Description
    	Text
	    This method creates a ring map whose image is the sub-ring of $C^r$ splines expressed in
	    terms of $C^0$ generators.  If $\Delta$ is simplicial, $C^0(\Delta)$ is expressed as the
	    Stanley Reisner ring.
    	Example
    	    V={{0,0},{0,1},{-1,-1},{1,0}};
	    F={{0,1,2},{0,2,3},{0,1,3}};
	    R=QQ[x,y];
	    phi=stanleyReisnerPresentation(V,F,1,Homogenize=>false,BaseRing=>R)
	    H=source phi;
	    scan(gens H, g->print phi(g))--see the expression of each generator in the stanley reisner ring
    	Text
	    The kernel of the map is the same as the kernel of the map returned by @TO ringStructure@, but the generators
	    are expressed differently.
	Example
	    M=splineModule(V,F,1,Homogenize=>false,BaseRing=>R);
	    phi1=ringStructure(M); 
	    H=source phi; H1=source phi1;
	    gens H
	    gens H1	     
	    psi=map(H,H1,gens H);--phi1 has "same" source as H, but they are viewed as different rings by Macaulay2
	    scan(gens H1,g->print {g,phi1(g),phi(psi(g))})--phi expresses generators of M in the Stanley Reisner ring, while phi1 expresses generators in the free module R^3
    	    (ker phi)==psi(ker phi1)--the kernels are the same
	Text
	    The option Trim=>true may be used to get a minimal number of ring generators.
	Example
	    V={{0,0,0},{1,0,0},{0,1,0},{0,0,1},{-1,0,0},{0,-1,0},{0,0,-1}};
	    F={{0,1,2,3},{0,1,2,6},{0,2,3,4},{0,2,4,6},{0,1,3,5},{0,3,4,5},{0,4,5,6},{0,1,5,6}};--centrally triangulated octahedron
	    S=QQ[x,y,z];
	    stanleyReisnerPresentation(V,F,1,Homogenize=>false,BaseRing=>S)
	    stanleyReisnerPresentation(V,F,1,Homogenize=>false,BaseRing=>S,Trim=>true)
	Text
	    The geometry effects $C^1$ simplicial splines, although it doesn't effect $C^0$ simplicial splines.
	Example
	    V'={{0,0,0},{1,0,0},{0,1,0},{1,1,1},{-1,0,0},{0,-1,0},{0,0,-1}}; --centrally triangulated octahedron that has been perturbed
	    F={{0,1,2,3},{0,1,2,6},{0,2,3,4},{0,2,4,6},{0,1,3,5},{0,3,4,5},{0,4,5,6},{0,1,5,6}};
	    stanleyReisnerPresentation(V',F,1,Homogenize=>false,BaseRing=>S,Trim=>true)
	Text
	    If $\Delta$ is not simplicial, $C^0(\Delta)$ does not have the same nice structure.
	Example
	    V={{0,1},{-1,-1},{1,-1},{0,2},{-2,-2},{2,-2}};
	    F={{0,1,2},{0,1,3,4},{0,2,3,5},{1,2,4,5}}; --symmetric triangular prism--
	    S=QQ[x,y,z];
	    stanleyReisnerPresentation(V,F,1,BaseRing=>S,Trim=>true)
    SeeAlso
    	courantFunctions
	stanleyReisner
	ringStructure
///
    		    
doc ///
    Key
    	splineComplex
	(splineComplex, List,List,ZZ)
    Headline
    	creates the Billera-Schenck-Stillman chain complex
    Usage
    	C = splineComplex(V,F,r)
    Inputs
    	V:List
	    list of vertex coordinates of $\Delta$
	F:List
	    list of facets of $\Delta$ (each facet is recorded as a list of indices of vertices taken from V)
	r:ZZ
	    integer, desired degree of smoothness
	BaseRing=>Ring
	    
	Homogenize=>Boolean
	
	CoefficientRing=>Ring
	
	VariableName=>Symbol
	
    Outputs
    	C:ChainComplex
	    Billera-Schenck-Stillman spline complex
    Description
    	Text
	    This method returns the Billera-Schenck-Stillman chain complex whose
	    top homology is the module of splines on $\Delta$.
	Example
	    V = {{0,0},{1,0},{0,1},{-1,-1}};
	    F = {{0,1,2},{0,2,3},{0,1,3}};
	    C = splineComplex(V,F,1);
	    prune HH C
	Text
	    The output from the above example shows that there is only one nonvanishing
	    homology, and it is free as a module over the polynomial ring in three variables.
	    Notice that the rank is one more than the corresponding nonvanishing homology given
	    by idealsComplex.
	Example
	    V = {{-1,-1},{1,-1},{0,1},{-2,-2},{2,-2},{0,2}};
	    F = {{0,1,2},{0,1,3,4},{1,2,4,5},{0,2,3,5}};
	    C = splineComplex(V,F,1);
	    prune HH C
	Text
	    The output from the above example shows that there are two nonvanishing homologies,
	    but the spline module, which is (almost) the homology HH_1, is still free.  This shows
	    that freeness of the spline module does not depend on vanishing of lower homologies if
	    the underlying complex is polyhedral.
	Example
	    R = (ZZ/4021)[x,y,z];
	    V = {{-1,-1},{1,-1},{0,1},{10,10},{-10,10},{0,-10}};
	    V'= {{-1,-1},{1,-1},{0,1},{10,10},{-10,10},{1,-10}};
	    F = {{0,1,2},{2,3,4},{0,4,5},{1,3,5},{1,2,3},{0,2,4},{0,1,5}};
	    C = splineComplex(V,F,1,BaseRing=>R);
	    C' = splineComplex(V',F,1,BaseRing=>R);
	    prune HH C
	    prune HH C'
	Text
	    The above example is known as the Morgan-Scot partition.  It shows
	    how subtle spline calculations can be.  Note that the spline module over
	    the symmetric Morgan-Scot partition above (vertices V) is not free,
	    while the spline module over the non-symmetric Morgan-Scot partition is free.
	Example
	    R = QQ[x,y,z];
	    V = {{-1,-1},{1,-1},{0,1},{10,10},{-10,10},{0,-10}};
	    F = {{0,1,2},{2,3,4},{0,4,5},{1,3,5},{1,2,3},{0,2,4},{0,1,5}};
	    Id = idealsComplex(V,F,1,BaseRing=>R);
	    C = cellularComplex(V,F,BaseRing=>R);
	    phi = inducedMap(C,Id);
	    SC = splineComplex(V,F,1,BaseRing=>R);
	    SC == (coker phi)
	Text
	    The above example shows that the Billera-Schenck-Stillman spline complex is the cokernel of the natural map between the complex
	    of ideals (given by idealsComplex) and the cellular chain complex of $\Delta$ relative to its boundary.
	Example
	    V = {{1, 0, 0}, {-1, 0, 0}, {0, 1, 0}, {0, -1, 0}, {0, 0, 1}, {0, 0, -1}, {-2, -2, -2}, {-2, 2, -2}, {2, 2, -2}, {2, -2, -2}, {-2, -2, 2}, {-2, 2, 2}, {2, 2, 2}, {2, -2, 2}};
	    F = {{0, 1, 2, 3, 4, 5}, {0, 8, 9, 12, 13}, {1, 6, 7, 10, 11}, {2, 7, 8, 11, 12}, {3, 6, 9, 10, 13}, {4, 10, 11, 12, 13}, {5, 6, 7, 8, 9}, {0, 2, 8, 12}, {0, 3, 9, 13}, {0, 4, 12, 13}, {0, 5, 8, 9}, {1, 2, 7, 11}, {1, 3, 6, 10}, {1, 4, 10, 11}, {1, 5, 6, 7}, {2, 4, 11, 12}, {3, 4, 10, 13}, {3, 5, 6, 9}, {2, 5, 7, 8}, {0, 2, 4, 12}, {0, 2, 5, 8}, {0, 3, 4, 13}, {0, 3, 5, 9}, {1, 2, 4, 11}, {1, 2, 5, 7}, {1, 3, 4, 10}, {1, 3, 5, 6}};
	    C = splineComplex(V,F,1);
	    associatedPrimes annihilator HH_2 C
	Text
	    The above example showcases a fairly complex three dimensional polyhedral complex.
	    It is a three-dimensional analog of the Morgan-Scot partition in the sense that
	    it consists of an octahedron inside of a cube (note these are dual polytopes).  Its
	    facets consist of the octahedron, twelve tetrahedra coming from joining dual edges on
	    the cube and the octahedron, and six 'Egyptian pyramids' coming from joining each vertex
	    of the octahedron to a square face on the cube.  
	    The homologies of this spline complex
	    are quite interesting.  The two lower homologies vanish (the lowest always does). 
	    The top homology (HH_3) is the spline module.  The second homology is nonzero and illustrates 
	    the fact that the associated primes of the homologies of the spline complex are all linear.
	    There is an associated prime for every vertex of the complex, plus three more that lie on the
	    hyperplane at infinity, plus the maximal ideal.
    SeeAlso
	cellularComplex
	idealsComplex

///

TEST ///
V = {{0,0},{1,0},{1,1},{-1,1},{-2,-1},{0,-1}}
F = {{0,2,1},{0,2,3},{0,3,4},{0,4,5},{0,1,5}}
E = {{0,1},{0,2},{0,3},{0,4},{0,5}}
S = QQ[x,y,z]
R = QQ[u,v]
assert(splineMatrix(V,F,E,0) == matrix {{1, 0, 0, 0, -1, t_1, 0, 0, 0, 0}, {1, -1, 0, 0, 0, 0, t_0-t_1,
      0, 0, 0}, {0, 1, -1, 0, 0, 0, 0, t_0+t_1, 0, 0}, {0, 0, 1, -1, 0, 0, 0,
      0, t_0-2*t_1, 0}, {0, 0, 0, 1, -1, 0, 0, 0, 0, t_0}})
assert(splineMatrix(V,F,E,0,Homogenize=>false) == matrix {{1, 0, 0, 0, -1, t_2, 0, 0, 0, 0}, {1, -1, 0, 0, 0, 0, t_1-t_2,
      0, 0, 0}, {0, 1, -1, 0, 0, 0, 0, t_1+t_2, 0, 0}, {0, 0, 1, -1, 0, 0, 0,
      0, t_1-2*t_2, 0}, {0, 0, 0, 1, -1, 0, 0, 0, 0, t_1}})
assert(splineMatrix(V,F,E,1) == matrix {{1, 0, 0, 0, -1, t_1^2, 0, 0, 0, 0}, {1, -1, 0, 0, 0, 0,
      t_0^2-2*t_0*t_1+t_1^2, 0, 0, 0}, {0, 1, -1, 0, 0, 0, 0,
      t_0^2+2*t_0*t_1+t_1^2, 0, 0}, {0, 0, 1, -1, 0, 0, 0, 0,
      t_0^2-4*t_0*t_1+4*t_1^2, 0}, {0, 0, 0, 1, -1, 0, 0, 0, 0, t_0^2}})
assert(splineMatrix(V,F,0,BaseRing=>S)==matrix {{1, 0, 0, 0, -1, y, 0, 0, 0, 0}, {1, -1, 0, 0, 0, 0, x-y, 0, 0,
     0}, {0, 1, -1, 0, 0, 0, 0, x+y, 0, 0}, {0, 0, 1, -1, 0, 0, 0, 0, x-2*y,
     0}, {0, 0, 0, 1, -1, 0, 0, 0, 0, x}})
assert(splineMatrix(V,F,0,Homogenize=>false,BaseRing=>R)==matrix {{1, 0, 0, 0, -1, v, 0, 0, 0, 0}, {1, -1, 0, 0, 0, 0, u-v, 0, 0,
      0}, {0, 1, -1, 0, 0, 0, 0, u+v, 0, 0}, {0, 0, 1, -1, 0, 0, 0, 0, u-2*v,
      0}, {0, 0, 0, 1, -1, 0, 0, 0, 0, u}})
assert(splineModule(V,F,1,BaseRing=>S)==image matrix {{1, 6*y^2, -4*y^2, 4*x*y^2, -4*y^3}, {1, 3*x^2-6*x*y+9*y^2,
      -x^2+2*x*y-5*y^2, x^2*y+2*x*y^2+y^3, x^3-3*x*y^2-2*y^3}, {1,
      2*x^2-8*x*y+8*y^2, 4*x*y-4*y^2, 0, 0}, {1, 0, x^2, 0, 0}, {1, 0, 0, 0,
      0}})
///

TEST ///
V={{-5,0},{-3,0},{-1,-4},{-1,4},{-1,-2},{-1,2},{0,-1},{0,1},{1,-2},{1,2},{1,-4},{1,4},{3,0},{5,0}}
F={{0, 1, 4, 2}, {0, 1, 5, 3}, {8, 10, 13, 12}, {9, 11, 13, 12}, {1, 4, 6, 7, 5}, {2, 4, 6, 8, 10}, {3, 5, 7, 9, 11}, {6, 7, 9, 12, 8}}
E={{0, 1}, {0, 2}, {0, 3}, {1, 4}, {1, 5}, {2, 4}, {2, 10}, {3, 5}, {3, 11}, {4, 6}, {5, 7}, {6, 7}, {6, 8}, {7, 9}, {8, 10}, {8, 12}, {9, 11}, {9, 12}, {10, 13}, {11, 13}, {12, 13}}
assert(splineMatrix(V,F,E,0) == matrix {{1, -1, 0, 0, 0, 0, 0, 0, t_1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0}, {1, 0, 0, 0, -1, 0, 0, 0, 0, t_0+t_1+3*t_2, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0}, {0, 1, 0, 0, -1, 0, 0, 0, 0, 0, t_0-t_1+3*t_2, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0,
      t_0+t_2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 1, 0, 0, 0, 0, -1, 0, 0,
      0, 0, 0, t_0+t_2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 1, -1, 0,
      0, 0, 0, 0, 0, 0, t_0-t_1-t_2, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0,
      1, 0, -1, 0, 0, 0, 0, 0, 0, 0, t_0+t_1-t_2, 0, 0, 0, 0, 0, 0, 0, 0}, {0,
      0, 0, 0, 1, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, t_0, 0, 0, 0, 0, 0, 0, 0}, {0,
      0, 0, 0, 0, 1, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, t_0+t_1+t_2, 0, 0, 0, 0, 0,
      0}, {0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, t_0-t_1+t_2, 0,
      0, 0, 0, 0}, {0, 0, 1, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      t_0-t_2, 0, 0, 0, 0}, {0, 0, 1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, t_0-t_1-3*t_2, 0, 0, 0}, {0, 0, 0, 1, 0, 0, -1, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, t_0-t_2, 0, 0}, {0, 0, 0, 1, 0, 0, 0, -1, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, t_0+t_1-3*t_2, 0}, {0, 0, 1, -1, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, t_1}})
assert(splineMatrix(V,F,E,0,Homogenize=>false) == matrix {{1, -1, 0, 0, 0, 0, 0, 0, t_2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0}, {1, 0, 0, 0, -1, 0, 0, 0, 0, t_1+t_2+3, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0}, {0, 1, 0, 0, -1, 0, 0, 0, 0, 0, t_1-t_2+3, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0}, {1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, t_1+1, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0}, {0, 1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, t_1+1, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0,
      t_1-t_2-1, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 1, 0, -1, 0, 0, 0, 0,
      0, 0, 0, t_1+t_2-1, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 1, 0, 0, -1, 0,
      0, 0, 0, 0, 0, 0, t_1, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 1, 0, -1, 0,
      0, 0, 0, 0, 0, 0, 0, t_1+t_2+1, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 1,
      -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, t_1-t_2+1, 0, 0, 0, 0, 0}, {0, 0, 1, 0, 0,
      -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, t_1-1, 0, 0, 0, 0}, {0, 0, 1, 0,
      0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, t_1-t_2-3, 0, 0, 0}, {0, 0,
      0, 1, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, t_1-1, 0, 0}, {0,
      0, 0, 1, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, t_1+t_2-3,
      0}, {0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      t_2}})
///

TEST ///
V={{0,0,0},{1,1,1},{-1,1,1},{-1,1,-1},{1,1,-1},{1,-1,1},{-1,-1,1},{-1,-1,-1},{1,-1,-1}};
F={{0,1,2,3,4},{0,1,4,5,8},{0,5,6,7,8},{0,2,3,6,7},{0,1,2,5,6}};
C=cellularComplex(V,F);
sC=splineComplex(V,F,0);
iC=idealsComplex(V,F,0);
assert((prune HH_0 C)==prune image matrix{{0_(C.ring)}})
assert((prune HH_1 C)==prune image matrix{{0_(C.ring)}})
assert((prune HH_2 C)==prune image matrix{{0_(C.ring)}})
assert((prune HH_3 C)==prune image matrix{{1_(C.ring)}})
assert((prune HH_0 sC)==prune image matrix{{0_(sC.ring)}})
assert((prune HH_1 sC)==prune image matrix{{0_(sC.ring)}})
assert((prune HH_2 sC)==coker map((sC.ring)^{1:-1},(sC.ring)^2,sub(matrix{{t_1,t_0}},(sC.ring))))
assert((prune HH_3 sC)==(sC.ring)^{1:0,1:-1,2:-2,1:-3})
assert((prune HH_0 iC)==prune image matrix{{0_(iC.ring)}})
assert((prune HH_1 iC)==coker map((iC.ring)^{1:-1},(iC.ring)^2,sub(matrix{{t_1,t_0}},(iC.ring))))
assert((prune HH_2 iC)==(iC.ring)^{1:-1,2:-2,1:-3})
assert((prune HH_3 iC)==prune image matrix{{0_(iC.ring)}})
///

TEST ///
V = {{-1,-1},{1,-1},{0,1},{10,10},{-10,10},{0,-10}};
V'= {{-1,-1},{1,-1},{0,1},{10,10},{-10,10},{1,-10}};
F = {{0,1,2},{2,3,4},{0,4,5},{1,3,5},{1,2,3},{0,2,4},{0,1,5}};
S1 = splineModule(V,F,1);
S1' = splineModule(V',F,1);
assert(splineDimensionTable(0,5,S1)==netList{{Degree,0,1,2,3,4,5},{"Dimension",1,3,7,16,33,57}})
assert(splineDimensionTable(0,5,S1')==netList{{Degree,0,1,2,3,4,5},{"Dimension",1,3,6,16,33,57}})
assert(hilbertComparisonTable(0,5,S1')==netList{{Degree,0,1,2,3,4,5},{"Dimension",1,3,6,16,33,57},{"HilbertPoly",7,3,6,16,33,57}})
assert(hilbertComparisonTable(0,5,S1)==netList{{Degree,0,1,2,3,4,5},{"Dimension",1,3,7,16,33,57},{"HilbertPoly",7,3,6,16,33,57}})
assert(postulationNumber(S1)==2)
assert(postulationNumber(S1')==0)
///

TEST ///
V = {{0,0},{1,0},{1,1},{0,1}};
E = {{0,1},{0,2},{0,3},{1,2},{2,3}};
S=QQ[x,y];
assert(formsList(V,E,0)=={t_1, t_0-t_1, t_0, t_0-t_2, t_1-t_2})
assert(formsList(V,E,0,BaseRing=>S,Homogenize=>false)=={y, x-y, x, x-1, y-1})
///

TEST ///
S = QQ[x,y];
E = {{0,1},{1,2},{2,3},{3,4},{0,4}};
L = {ideal(y^2),ideal((x-y)^2),ideal((x+y)^2),ideal((x-2*y)^2),ideal(x^2)}
assert(generalizedSplines(E,L)==image matrix {{1, 2*x^2, -3*x^2, x^2*y, x^3}, {1, 2*x^2+2*y^2, -3*x^2,
      x^2*y-2*x*y^2+y^3, x^3-3*x*y^2+2*y^3}, {1, x^2+2*x*y+y^2, -6*x*y+3*y^2,
      0, 0}, {1, 0, x^2-4*x*y+4*y^2, 0, 0}, {1, 0, 0, 0, 0}})
///

TEST ///
V={{0,0},{0,1},{-1,-1},{1,0}}; F={{0,1,2},{0,2,3},{0,1,3}}; R=QQ[x,y];
M=splineModule(V,F,0,BaseRing=>R,Homogenize=>false);
phi = ringStructure(M);
assert((ker phi)==ideal(Y_1*Y_3-Y_2*Y_3,Y_0*Y_2-Y_2^2-Y_3));
phi' = ringStructure(M,Trim=>true);
assert((ker phi')==ideal(Y_0*Y_1*Y_2-Y_0*Y_2^2-Y_1*Y_2^2+Y_2^3));
///

TEST ///
V={{-1,0},{1,0},{2,1},{2,-1},{0,-1},{-2,-1},{-2,1},{0,1}};
F={{0,1,7},{0,1,4},{0,4,5},{0,5,6},{0,6,7},{1,2,7},{1,2,3},{1,3,4}};
S=QQ[x,y,z];
assert(courantFunctions(V,F,BaseRing=>S)==matrix {{-(1/2)*x-(1/2)*y+(1/2)*z, (1/2)*x-(1/2)*y+(1/2)*z, 0, 0, 0, 0,
      0, y}, {-(1/2)*x+(1/2)*y+(1/2)*z, (1/2)*x+(1/2)*y+(1/2)*z, 0, 0, -y, 0,
      0, 0}, {y+z, 0, 0, 0, (1/2)*x-(1/2)*y+(1/2)*z, -(1/2)*x-(1/2)*y-(1/2)*z,
      0, 0}, {x+2*z, 0, 0, 0, 0, -(1/2)*x-(1/2)*y-(1/2)*z,
      -(1/2)*x+(1/2)*y-(1/2)*z, 0}, {-y+z, 0, 0, 0, 0, 0,
      -(1/2)*x+(1/2)*y-(1/2)*z, (1/2)*x+(1/2)*y+(1/2)*z}, {0, -y+z,
      (1/2)*x+(1/2)*y-(1/2)*z, 0, 0, 0, 0, -(1/2)*x+(1/2)*y+(1/2)*z}, {0,
      -x+2*z, (1/2)*x+(1/2)*y-(1/2)*z, (1/2)*x-(1/2)*y-(1/2)*z, 0, 0, 0, 0},
      {0, y+z, 0, (1/2)*x-(1/2)*y-(1/2)*z, -(1/2)*x-(1/2)*y+(1/2)*z, 0, 0, 0}})
V={{0,0},{0,1},{-1,-1},{1,0}}; F={{0,1,2},{0,2,3},{0,1,3}}; R=QQ[x,y];
assert(courantFunctions(V,F,BaseRing=>R,Homogenize=>false)==matrix {{-x+y, -x, 0}, {0, -y, x-y}, {y, 0, x}})
///

TEST ///
V={{0,0},{0,1},{-1,-1},{1,0}}; F={{0,1,2},{0,2,3},{0,1,3}}; R=QQ[x,y];
phi=stanleyReisner(V,F);
assert((ker phi)==ideal(X_1*X_2*X_3))
phi1=stanleyReisner(V,F,BaseRing=>R,Homogenize=>false);
assert((ker phi1)==ideal(X_0*X_1*X_2))
V={{-1,0},{1,0},{2,1},{2,-1},{0,-1},{-2,-1},{-2,1},{0,1}};
F={{0,1,7},{0,1,4},{0,4,5},{0,5,6},{0,6,7},{1,2,7},{1,2,3},{1,3,4}};
phi=stanleyReisner(V,F);
H = source phi;
G = apply(gens H,v->phi(v));
phi1=ringStructure(image courantFunctions(V,F),VariableGens=>false);
H1 = source phi1;
T1 = target phi1;
G0 = apply(G,i->sub(i,T1));
G1 = apply(gens H1,v->phi1(v));
assert(G0==G1)
///

TEST ///
V={{0,0},{0,1},{-1,-1},{1,0}}; F={{0,1,2},{0,2,3},{0,1,3}}; R=QQ[x,y];
phi=stanleyReisnerPresentation(V,F,1,BaseRing=>R,Homogenize=>false); H=source phi;
SR=target phi;
assert(phi(H_0)==(-SR_1+SR_2))
assert(phi(H_1)==(SR_0-SR_1))
assert(phi(H_2)==(-SR_1^3+3*SR_1^2*SR_2))
assert(phi(H_3)==(SR_0*SR_1^2-SR_1^3+2*SR_1^2*SR_2))
M=splineModule(V,F,1,Homogenize=>false,BaseRing=>R);
phi1=ringStructure(M); H1=source phi1; psi=map(H,H1,gens H);
assert((ker phi)==psi(ker phi1))
V={{0,0,0},{1,0,0},{0,1,0},{0,0,1},{-1,0,0},{0,-1,0},{0,0,-1}};
F={{0,1,2,3},{0,1,2,6},{0,2,3,4},{0,2,4,6},{0,1,3,5},{0,3,4,5},{0,4,5,6},{0,1,5,6}};
S=QQ[x,y,z];
phi=stanleyReisnerPresentation(V,F,1,BaseRing=>S,Homogenize=>false,Trim=>true);
H=source phi; SR=target phi; use SR;
assert(apply(gens H,g->phi(g))=={X_0-X_3, X_1-X_4, X_2-X_5, X_2^2, X_1^2, X_3^2})
///

end

--may restore to documentation in future iterations

--doc ///
--    Key
--        Splines
--	VertexCoordinates
--	Regions
--	SplineModule
--    Headline
--    	a class for splines (piecewise polynomial functions on subdivisions)
--    Description
--    	Text
--	    This class is a type of @TO "HashTable"@ that stores information on
--	    a subdivision $\Delta$ of ${\mathbb R}^n$, given by a set of vertex
--	    coordinates and a list of facets (and possibly edges), along with a
--	    module of all splines on $\Delta$ of continuity $r$.
--	Example
--	    V = {{0,0},{1,0},{1,1},{-1,1},{-2,-1},{0,-1}};
--	    F = {{0,2,1},{0,2,3},{0,3,4},{0,4,5},{0,1,5}};
--	    E = {{0,1},{0,2},{0,3},{0,4},{0,5}};
--	    S = splines(V,F,E,1) -- splines in R^2 with smoothness 1
--    SeeAlso
--        splines
--///
