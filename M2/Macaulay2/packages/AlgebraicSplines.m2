-- Copyright 2014-2015: Mike Dipasquale
-- You may redistribute this file under the terms of the GNU General Public
-- License as published by the Free Software Foundation, either version 2
-- of the License, or any later version.

------------------------------------------
------------------------------------------
-- Header
------------------------------------------
------------------------------------------

if version#"VERSION" <= "1.4" then (
    needsPackage "FourierMotzkin"
    )

newPackage select((
    "AlgebraicSplines",
        Version => "0.1.0", 
        Date => "27. May 2015",
        Authors => {
            {Name => "Mike DiPasquale", Email => "midipasq@gmail.com", HomePage => "http://illinois.edu/~dipasqu1"},
            {Name => "Gwyn Whieldon", Email => "whieldon@hood.edu", HomePage => "http://cs.hood.edu/~whieldon"},
	    {Name => "Eliana Duarte", Email => "emduart2@illinois.edu", HomePage => "http://illinois.edu/~emduart2"},
	    {Name => "Daniel Irving Bernstein", Email=> "dibernst@ncsu.edu", HomePage =>"http://www4.ncsu.edu/~dibernst"}
        },
        Headline => "Package for computing topological boundary maps and piecewise continuous splines on polyhedral complexes.",
        Configuration => {},
        DebuggingMode => true,
        if version#"VERSION" > "1.4" then PackageExports => {
	    "FourierMotzkin"
	    }
        ), x -> x =!= null)

if version#"VERSION" <= "1.4" then (
    needsPackage "FourierMotzkin"
    )

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
   "splineDimensionTable",
   "postulationNumber",
   "hilbertComparisonTable",
   "generalizedSplines",
   "formsList",
   "cellularComplex",
   "idealsComplex",
   "splineComplex"
   --Remove after testing
   --"getCodim1Intersections",
   --"simpBoundary",
   --"facetsN",
   --"boundaryComplex",
   --"polyBoundary"
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
    while hilbertFunction(k,N)==hilbertPolyEval(k,N) do	(k=k-1);
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
    r1:=prepend("Degree",toList(a..b));
    r2:=prepend("Dimension",apply(toList(a..b),i->hilbertFunction(i,M)));
    r3:=prepend("HilbertPoly",apply(toList(a..b),i->hilbertPolyEval(i,M)));
    netList {r1,r2,r3}
    )
---------------------------------------------

hilbertPolyEval=method()
---------------------------------------------
-------------------------------------------
-----Inputs:
-------------------------------------------
----- i= integer at which you will evaluate the Hilbert polynomial
----- M= module
--------------------------------------------
------ Outputs:
--------------------------------------------
------ An Hilbert polynomial of the module M
------ evaluated at i.
--------------------------------------------

hilbertPolyEval(ZZ,Module):=(i,M)->(
    P:=hilbertPolynomial(M,Projective=>false);
    sub(P,(vars ring P)_(0,0)=>i)
    )

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
        a package for building splines and computing bases
    Description
        Text
            This package provides methods for computations with piecewise polynomial functions (splines) over
	    polytopal complexes.
-- 	Text 
--	    "Definitions"
	Text
	    Let $\Delta$ be a partition (simplicial,polytopal,cellular,rectilinear, etc.) of a space $\RR^n$.
	    The spline module $S_d^{r}(\Delta)$ is the module of all functions $f\in C^r(\Delta)$ such that
	    $f$ is a polynomial of degree $d$ when restricted to each facet $\sigma\in\Delta$.
	Text
	    This package computes the @TO splineModule@ and @TO splineMatrix@ of $\Delta$, as well as the Billera-Schenck-Stillman
	    @TO splineComplex@ of $\Delta$.
--	    , as well
--	    as defining new type @TO Splines@ that contain geometric data 
--	    for $\Delta$ (if entered) and details on the associated spline module $S_d^r(\Delta)$.
--        Text
--	    @SUBSECTION "Other acknowledgements"@
            --
            Methods in this package borrows heavily from code written by Hal Schenck
	    and Mike DiPasquale.
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

end

--may restore to documentation in future iterations

--doc ///
--    Key
--        hilbertPolyEval
--	(hilbertPolyEval,ZZ,Module)
--    Headline
--        a function to evaluate the hilbertPolynomial of a graded module at an integer
--    Usage
--        v = hilbertPolyEval(a,M)
--    Inputs
--        a:ZZ
--	    a= integer at which you will evaluate the hilbertPolynomial of the graded module M
--	M:Module
--	    M= graded module
--    Outputs
--        v:ZZ
--	    v= hilbertPolynomial of the graded module M evaluated at a
--    Description
--        Text
--            For any graded module M and any integer a, you may evaluate the hilberPolynomial of M
--	    at a.
--	Example
--	    V = {{0,0},{1,0},{1,1},{0,1}};
--	    F = {{0,1,2},{0,2,3}};
--	    E = {{0,1},{0,2},{0,3},{1,2},{2,3}};
--	    M = splineModule(V,F,E,2)
--	    hilbertPolyEval(2,M)
--	    
--///


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