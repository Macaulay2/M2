-- -*- coding: utf-8 -*-

--------------------------------------------------------------------------------
-- Copyright 2008, 2009, 2010  Chris Francisco, Andrew Hoefel, and Adam Van Tuyl
-- 
-- You may redistribute this program under the terms of the GNU General Public
-- License as published by the Free Software Foundation, either version 2 of the
-- License, or any later version.
--------------------------------------------------------------------------------

newPackage(
	"EdgeIdeals", 
	Version => "1.0.2",
	Date => "March 30, 2011",
	PackageExports => {"SimplicialComplexes"},
	PackageImports => {"GenericInitialIdeal"},
	Certification => {
		"journal name" => "The Journal of Software for Algebra and Geometry: Macaulay2",
		"journal URI" => "http://j-sag.org/",
		"article title" => "EdgeIdeals: a package for (hyper)graphs",
		"acceptance date" => "2009-06-27",
		"published article URI" => "http://j-sag.org/Volume1/jsag-1-2009.pdf",
		"published code URI" => "http://j-sag.org/Volume1/EdgeIdeals.m2",
		"repository code URI" => "https://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/EdgeIdeals.m2",
		"release at publication" => "38e96fec660168d488ad0449f8632e6608cc9ede",
		"version at publication" => "1.0.0",
		"volume number" => "1",
		"volume URI" => "http://j-sag.org/Volume1/"
	},
	Authors => {
		{
			Name => "Chris Francisco", 
			Email => "chris@math.okstate.edu",
			HomePage => "http://www.math.okstate.edu/~chris/"
			},
		{
			Name => "Andrew Hoefel", 
			Email => "handrew@mathstat.dal.ca",
			HomePage => "http://www.mathstat.dal.ca/~handrew/"
		},
		{
			Name => "Adam Van Tuyl", 
			Email => "avantuyl@lakeheadu.ca",
			HomePage => "http://flash.lakeheadu.ca/~avantuyl/"
		}
	},
	Headline => "edge ideals",
	Keywords => {"Edge Ideals"},
	DebuggingMode => false
)

export {
	"HyperGraph", 
	"hyperGraph", 
	"Graph",
	"graph",
	"adjacencyMatrix",
	"allOddHoles",
	"allEvenHoles",
	"antiCycle",
	"changeRing",
	"chromaticNumber",
	"cliqueComplex",
	"cliqueNumber",
	"coverIdeal",
	"complementGraph",
	"completeGraph",
	"completeMultiPartite",
	"connectedComponents",
	"connectedGraphComponents",
	"cycle",
	"degreeVertex",
	"deleteEdges",
	"edgeIdeal",
	"edges", 
	"getCliques",
	"getEdge",
	"getEdgeIndex",
	"getGoodLeaf",
	"getGoodLeafIndex",
	"getMaxCliques",
	"hasGoodLeaf",
	"hasOddHole",
	"hyperGraphToSimplicialComplex",
	"incidenceMatrix",
	"independenceComplex",
	"independenceNumber",
	"inducedGraph",
	"inducedHyperGraph",
	"isBipartite",
	"isChordal",
	"isCM",
	"isConnected",
	"isConnectedGraph",
	"isEdge",
	"isForest",
	"isGoodLeaf",
	"isGraph",
	"isLeaf",
	"isolatedVertices",
	"isPerfect",
	"isSCM",
	"lineGraph",
	"neighbors",
	"numConnectedComponents",
	"numConnectedGraphComponents",
	"numTriangles",
	"randomGraph",
	"randomUniformHyperGraph",
	"randomHyperGraph",
	"simplicialComplexToHyperGraph",
	"smallestCycleSize",
	"spanningTree",
	"vertexCoverNumber",
	"vertexCovers",
	"Gins",
	"BranchLimit",
	"TimeLimit",
	"MaximalEdges",
	"OriginalRing"
};

----------------------------------------------------------------------------------------
--
-- TYPES
--
----------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------
-- HyperGraph
----------------------------------------------------------------------------------------

HyperGraph = new Type of HashTable;
HyperGraph.synonym = "hypergraph";

hyperGraph = method(TypicalValue => HyperGraph);

hyperGraph (PolynomialRing, List) := HyperGraph => (R, E) -> ( 
     -- Output: HyperGraph over R with edges E
     -- Assert: R is a polynomial ring
     -- Assert: E is a List of Lists of variables of R or
     --         E is a List of square-free monomials in R 
     if any(E, e -> class e =!= List) and any(E, e -> class class e =!= PolynomialRing)
     then ( print apply(E, e -> class e) ;error "Edges must be lists of variables or monomials.");

     V := gens R;
     --- check needed for square free 
     if any(E, e-> class class e === PolynomialRing) then E = apply(E, support);
     E = apply(E, unique) / rsort; --- Enforces square-free if edges are given as lists
     H := new HyperGraph from hashTable({"ring" => R, "vertices" => V, "edges" => E});
     if any(H#"edges", e -> not instance(e, List)) then error "Edges must be lists.";
     if any(H#"edges", e -> not isSubset(e,H#"vertices")) then error "Edges must be subsets of the vertices.";
     if any(0..#(H#"edges") -1, I -> 
	  any(0..I-1, J-> isSubset(H#"edges"#I, H#"edges"#J) or isSubset(H#"edges"#J, H#"edges"#I))
     	  )
     then error "Edges satisfy a inclusion relation";
     return H;
)

hyperGraph (MonomialIdeal) := HyperGraph => (I) -> 
( 
     if not isSquareFree I then error "Ideals must be square-free.";
     hyperGraph(ring I, apply(flatten entries gens I, support))
)

hyperGraph (Ideal) := HyperGraph => (I) -> 
( 
     hyperGraph monomialIdeal I
)

hyperGraph (List) := HyperGraph => (E) -> 
( 
     M := null; 
     if all(E, e-> class e === List) then (
          if E == {} or E == {{}} then error "Use alternate construction with PolynomialRing to input empty hyperGraph" else
          M = monomialIdeal apply(E, product);
     );
     if all(E, e-> class class e === PolynomialRing) then M = monomialIdeal E;
     if M === null then error "Edge must be represented by a list or a monomial.";
     if #E =!= numgens M then error "Edges satisfy an inclusion relation."; 
     hyperGraph M
)


----------------------------------------------------------------------------
-- Graph
----------------------------------------------------------------------------

Graph = new Type of HyperGraph;

Graph.synonym = "graph";

graph = method(TypicalValue => Graph);

graph (PolynomialRing, List) := Graph => (R, E) ->
(
     H := hyperGraph(R, E);
     if not isGraph(H) then error "Edges must be of size two.";
     new Graph from H
)

graph (MonomialIdeal) := Graph => (I) -> 
(
     H := hyperGraph(I);
     if not isGraph(H) then error "Ideal must have quadratic generators.";
     new Graph from H
)

graph (Ideal) := Graph => (I) -> 
(
     H := hyperGraph(I);
     if not isGraph(H) then error "Ideal must have quadratic generators.";
     new Graph from H
)

graph List := Graph => E -> 
(
     if E == {} then error "Use alternate construction with PolynomialRing to input empty graph";
     H := hyperGraph(E);
     if not isGraph(H) then error "Edges must be of size two.";
     new Graph from H
)

graph (HyperGraph) := Graph => (H) -> 
(
     if not isGraph(H) then error "Edges must be of size two.";
     new Graph from H
)

hyperGraph (Graph) := HyperGraph => (G) -> 
(
     new HyperGraph from G
)


-------------------------------------------------------------------
--
-- FUNCTIONS
--
------------------------------------------------------------------




--------------------------------------------------------------
-- Mathematical equality 
-- return true if two graphs are equal (defined over same ring,
-- have same edge sets in some order).
--------------------------------------------------------------

HyperGraph == HyperGraph := (G,H) -> (
     G#"ring" === H#"ring" and
     set(apply(G#"edges", set)) === set(apply(H#"edges",set))
     ) 

--------------------------------------------------------------
-- adjacencyMatrix
-- return the adjacency matrix of a graph
--------------------------------------------------------------

adjacencyMatrix = method();

adjacencyMatrix Graph := G -> (
     vert:= G#"vertices";
     n := #vert;
     m := apply(n,i-> apply(n,j-> if isEdge(G,{vert_i,vert_j}) then 1 else 0));  
     matrix m
     )


------------------------------------------------------------
-- allEvenHoles
-- returns a list of even induced cycles
-- NOTE: This function is slow.
-----------------------------------------------------------

newVar := local newVar

allEvenHoles = method();
allEvenHoles Graph := G -> (
     R := G#"ring";
     S := (coefficientRing R)[append(gens R,newVar)];
     edges := G#"edges";
     numEdges := #edges;
     count := 0;
     evenCycles := {};
     while count < numEdges do (
       newEdges := {{first(edges#count),newVar},{newVar,(edges#count)#1}};
       tempEdges := apply(join(drop(edges,{count,count}),newEdges),i->apply(i,j->substitute(j,S)));
       tempGraph := graph(S,tempEdges);
       evenCycles = append(evenCycles,select(allOddHoles tempGraph,i->member(newVar,i)));
       count = count+1;
       );
     use R;
     apply(unique apply(flatten evenCycles,i->select(i,j->(j != newVar))),k->apply(k,l->substitute(l,R)))
     )

--------------------------------------------------------------
-- allOddHoles
-- returns a list of all the odd holes in a graph
--------------------------------------------------------------

allOddHoles = method();
allOddHoles Graph := G -> (
     coverI := coverIdeal G;
     apply(select(ass coverI^2,i->codim i > 3),j->flatten entries gens j)
     )


-------------------------------------------------------------------
-- antiCycle
-- return the complement of a cycle.
------------------------------------------------------------------

antiCycle = method(TypicalValue=>Graph);
antiCycle (Ring) := Graph =>(R) -> antiCycle(generators R)

antiCycle (Ring, ZZ) := Graph =>(R, N) -> antiCycle(apply(N, i->R_i))

antiCycle (List) := Graph =>(L)-> (
     if #L < 3 then error "Cannot construct anticycles of length less than three";
     antiCycleEdgeSet := subsets(L,2) - set append(apply(#L-1, i-> {L#i,L#(i+1)}), {first L, last L});
     graph(ring L#0,toList antiCycleEdgeSet)
     )


------------------------------------------------------------
-- changeRing
-- moves a HyperGraph into a new Ring
-----------------------------------------------------------

changeRing = method(Options=>{MaximalEdges=>false})
changeRing (HyperGraph, PolynomialRing, List) :=  option -> (H, R, L) -> (
     E := edges H;
     f := map(R, ring H, L);
     E = toList set apply(E, e-> set apply(e, v-> f v));
     I := if option.MaximalEdges then (
       select(toList(0..#E-1), i-> all(toList(0..#E-1), j-> j===i or not isSubset(E_i,E_j)))
     ) else (
       select(toList(0..#E-1), i-> all(toList(0..#E-1), j-> j===i or not isSubset(E_j,E_i)))
     ) ;
     hyperGraph(R,apply(I, i->toList E_i))
     );

---------------------------------------------------------------
-- chromaticNumber
-- returns the chromatic number of a (hyper)graph
-- NOTE: based upon work in progress by Francisco-Ha-Van Tuyl
---------------------------------------------------------------

chromaticNumber = method();
chromaticNumber HyperGraph := H -> (
     E := edges H;
     s := toList(apply(E,e->#e));
     if ((class H === HyperGraph) and (member(1,s))) then error "A hypergraph with 
     an edge of cardinality one does not have a chromatic number";
     Chi := 2; -- assumes graph has at least one edge
     m := product H#"vertices";
     j := coverIdeal H;
     while ((m^(Chi-1) % j^Chi) != 0) do (
       Chi = Chi + 1;
     );
     Chi 
     )


---------------------------------------------------------------
-- cliqueComplex
-- return the simplicial complex whose faces are the cliques of a graph
---------------------------------------------------------------
cliqueComplex = method();
cliqueComplex Graph := G -> independenceComplex complementGraph G;


-------------------------------------------------
-- cliqueNumber
-- return the clique number of a graph
------------------------------------------------

cliqueNumber = method();
cliqueNumber Graph := G -> (
     #(last getCliques G)
     )


---------------------------------------------------------------
-- complementGraph
-- returns the complement of a graph or hypergraph
-- NOTE:  does something different for graphs vs hyerpergraphs
--------------------------------------------------------------

complementGraph = method();
complementGraph Graph := G -> (
     v := G#"vertices";
     alledges := set(subsets(v,2) / set);
     gedges := set((G#"edges") / set);
     gcedges := alledges - gedges;  -- edges of the complement
     graph(G#"ring",toList(gcedges/toList))
     )

complementGraph HyperGraph := H -> (
     hcedge := apply(H#"edges",e-> toList (set(H#"vertices") - set e));  -- create edge set of hypergraph
     hyperGraph(H#"ring",toList hcedge)
     )


----------------------------------------------------------------------
-- completeGraph
-- return graph of complete n-graph
----------------------------------------------------------------------

completeGraph = method();
completeGraph (Ring) := Graph =>(R) -> completeGraph(generators R)

completeGraph (Ring, ZZ) := Graph =>(R, N) -> completeGraph(apply(N, i->R_i))

completeGraph (List) := Graph =>(L)-> (
     if #L === 0 then error "Cannot construct complete graph on no vertices";
     E := for i from 0 to #L -2 list
     for j from i+1 to #L-1 list
     L#i * L# j;
     graph(ring first L, flatten E)  
     )     


--------------------------------------------------------------------------
-- completeMultiPartite
-- return the complete multi-partite graph
--------------------------------------------------------------------------

completeMultiPartite = method();

completeMultiPartite (Ring, ZZ, ZZ) := Graph =>(R,N,M) -> 
     completeMultiPartite(R, toList apply(N, i->M))

completeMultiPartite (Ring, List) := Graph =>(R, L) -> (
     if all(L, l -> class l === ZZ) then (
     if sum L > #gens(R) then 
     error "Too few variables in ring to make complete multipartite graph";	
     N := 0;
     L = for i from 0 to #L-1 list (
          E := toList apply(L#i, j -> R_(j+N));
          N = N+L#i;
          E
          );
     );
     if all(L, l -> class l === List) then (
     K := flatten for i from 0 to #L-2 list
       flatten for j from i+1 to #L-1 list
         flatten for x in L#i list
           for y in L#j list {x,y};
     return graph(R, K);
     ) else error "completeMultipartite must be passed a list of partition sizes or a list of partitions.";
     )


-----------------------------------------------------------------------
-- connectedComponents
-- returns all the connected components of a hypergraph
----------------------------------------------------------------------

connectedComponents = method();
connectedComponents HyperGraph := H -> (
     V := select(H#"vertices", v-> any(H#"edges", e -> member(v,e)));
     while #V > 0 list (
       C := {V#0};
       i := 0;
       while i < #C do (
         N := select(neighbors(H, C#i), v-> not member(v,C));
         C = join(C,N);
         i = i+1;
       );
       V = select(V, v -> not member(v,C));
       rsort C
       )
     )

-----------------------------------------------------------------------
-- connectedGraphComponents
-- returns all the connected components of a graph
----------------------------------------------------------------------

connectedGraphComponents = method();
connectedGraphComponents HyperGraph := H -> join(apply(isolatedVertices(H), v->{v}), connectedComponents H)

----------------------------------------------------------------------
-- coverIdeal
-- return the Alexander dual of edge ideal, otherwise known as the cover ideal
------------------------------------------------------------------------
coverIdeal = method();
coverIdeal HyperGraph := H -> dual edgeIdeal H




----------------------------------------------------------------------------
-- cycle
-- return graph of the cycle on n vertices
---------------------------------------------------------------------------

cycle = method(TypicalValue=>Graph);
cycle (Ring) := Graph =>(R) -> cycle(generators R)

cycle (Ring, ZZ) := Graph =>(R, N) -> cycle(apply(N, i->R_i))

cycle (List) := Graph =>(L)-> (
     if #L < 3 then error "Cannot construct cycles of length less than three";
     graph(ring L#0,append(apply(#L-1, i-> L#i*L#(i+1)), (last L)*(first L)))
     )     	   


----------------------------------------------------------------------
-- degreeVertex
-- returns the degree of a vertex
----------------------------------------------------------------------

degreeVertex = method();
degreeVertex (HyperGraph, ZZ) := (H,N) ->	(
		degreeVertex(H, (H#"ring")_N)
	)
degreeVertex (HyperGraph, RingElement) := (H,V) ->	(
		use H#"ring";
		N := index V;
		if N === null then error "Variable is not a vertex of the given HyperGraph";
		number(H#"edges", E-> member(V,E))
	)

----------------------------------------------------------------------------------
-- deleteEdges
-- remove edges from a (hyper)graph
----------------------------------------------------------------------------------
deleteEdges = method();

deleteEdges (HyperGraph,List) := (H,E) -> (
     if all(E, e -> class class e === PolynomialRing) then E = apply(E, support);
     if (isSubset(set E,set H#"edges") =!= true) then error "Second argument must be a subset of the edges, entered as a list";
     hyperGraph(ring H, toList(set(H#"edges")-set(E)))
     )

--deleteEdges (Graph,List) := (H,E) -> (graph deleteEdges (hyperGraph(H),E))


----------------------------------------------------------------------
-- edgeIdeal
-- return the edge ideal of a graph or hypergraph
----------------------------------------------------------------------

edgeIdeal = method();
edgeIdeal HyperGraph := H -> (
     if H#"edges" == {} then return monomialIdeal(0_(H#"ring"));
     if H#"edges" == {{}} then return monomialIdeal(1_(H#"ring"));
     monomialIdeal apply(H#"edges",product)) 


------------------------------------------------------------
-- edges
-- returns edges of a (hyper)graph
------------------------------------------------------------

edges = method();
edges HyperGraph := H -> H#"edges";


----------------------------------------------------------------
-- getCliques
-- return all cliques of the graph
----------------------------------------------------------------

getCliques = method();
getCliques (Graph,ZZ) := (G,d) -> (
     subs := apply(subsets(G#"vertices",d),i->subsets(i,2));
     cliqueIdeals := apply(subs,i->ideal apply(i,j->product j));
     edgeId := edgeIdeal G;
     apply(select(cliqueIdeals,i->isSubset(i,edgeId)),j->support j)
       )

getCliques Graph := G -> (
     numVerts := #(G#"vertices");
     cliques := {};
     count := 2;
     while count <= numVerts do (
	  newCliques:=getCliques(G,count);
	  if newCliques == {} then return flatten cliques;
	  cliques = append(cliques,newCliques);
	  count = count+1;
	  );
     flatten cliques
     )


------------------------------------------------------------
-- getEdge
-- returns a specific edge
------------------------------------------------------------

getEdge = method();
getEdge (HyperGraph, ZZ) := (H,N) -> H#"edges"#N;

------------------------------------------------------------
-- getEdgeIndex
-- returns position of a given edge in a list of edges
------------------------------------------------------------

getEdgeIndex = method();
getEdgeIndex (HyperGraph, List) := (H,E) -> ( 
     if class class E === PolynomialRing then E = support E;
     N :=  select(0..#(H#"edges")-1, N -> set H#"edges"#N === set E);
     if #N === 0 then return -1; 
     first N
)

getEdgeIndex (HyperGraph, RingElement) := (H,E) -> ( 
     getEdgeIndex(H, support E)
)

-----------------------------------------------------------
-- getGoodLeaf
-- return a "Good Leaf" of a hypergraph
----------------------------------------------------------

getGoodLeaf = method();
getGoodLeaf HyperGraph := H -> ( 
     H#"edges"#(getGoodLeafIndex H)
)


------------------------------------------------------------
-- getGoodLeafIndex
-- return the index of a "Good Leaf" in a hypergraph
------------------------------------------------------------

getGoodLeafIndex = method();
getGoodLeafIndex HyperGraph := H ->
(  GL := select(0..#(H#"edges")-1, N -> isGoodLeaf(H,N));
   if #GL == 0 then return -1;
   first GL
);

--------------------------------------------------------------------------
-- getMaxCliques
-- return all cliques of maximal size
--------------------------------------------------------------------------

 -- return all cliques of maximal size
getMaxCliques = method();
getMaxCliques Graph := G -> (
     cliqueList := getCliques G;
     clNum := #(last cliqueList);
     select(cliqueList,i->#i == clNum)
     )


-----------------------------------------------------------------------------
-- hasGoodLeaf
-- checks if a hypergraph has any "Good Leaves"
----------------------------------------------------------------------------

hasGoodLeaf = method();
hasGoodLeaf HyperGraph := H -> any(0..#(H#"edges")-1, N -> isGoodLeaf(H,N))


------------------------------------------------------------------------------
-- hasOddHole
-- checks if a graph has an odd hole (not triangle)
-----------------------------------------------------------------------------

hasOddHole = method();
hasOddHole Graph := G -> (
     coverI := coverIdeal G;
     any(ass coverI^2,i->codim i > 3)
     )     

--------------------------------------------------
-- hyperGraphToSimplicialComplex
-- make a simplicialComplex from a (hyper)graph 
---------------------------------------------------
hyperGraphToSimplicialComplex = method()
hyperGraphToSimplicialComplex HyperGraph := H -> (
     if H#"edges" == {} then return simplicialComplex monomialIdeal 1_(ring H);
     simplicialComplex flatten entries gens edgeIdeal H
     )




-----------------------------------------------------------------------------
-- incidenceMatrix
-- return the incidence matrix of a graph
-----------------------------------------------------------------------------

incidenceMatrix = method();

incidenceMatrix HyperGraph := H -> (
     v:= H#"vertices";
     e := H#"edges";
     m := apply(#v,i-> apply(#e,j-> if member(v_i,e_j) then 1 else 0));  
     matrix m
     )


-------------------------------------------------------------------------------
-- independenceComplex
-- returns the simplicial complex whose faces are the independent sets of a (hyper)graph
--------------------------------------------------------------------------------
independenceComplex = method();

independenceComplex HyperGraph := H -> (simplicialComplex edgeIdeal H)


------------------------------------------------------------------
-- independenceNumber
-- return the independence number, the size of the largest independent set of a vertices
------------------------------------------------------------------

independenceNumber = method();
independenceNumber Graph:= G -> (
     dim edgeIdeal G
     )

--------------------------------------------------------------------------------
-- inducedGraph
-- given a set of vertices, return induced graph on those vertices
--------------------------------------------------------------------------------
--if OriginalRing is true, then the graph stays in the larger ring.
--by default, the ring of the induced graph is the smaller ring.
--this avoids having lots of isolated vertices in the resulting hypergraph.

inducedGraph = method(Options=>{OriginalRing=>false});
inducedGraph (Graph,List) := opts -> (H,S) -> (
     graph inducedHyperGraph(H,S,OriginalRing=>opts#OriginalRing)
     )

--------------------------------------------------------------------------------
-- inducedHyperGraph
-- given a set of vertices, return induced hypergraph on those vertices
--------------------------------------------------------------------------------
--if OriginalRing is true, then the hypergraph stays in the larger ring.
--by default, the ring of the induced hypergraph is the smaller ring.
--this avoids having lots of isolated vertices in the resulting hypergraph.

inducedHyperGraph = method(Options=>{OriginalRing=>false});
inducedHyperGraph (HyperGraph,List) := opts -> (H,S) -> (
     if (isSubset(set S, set H#"vertices") =!= true) then error "Second argument must be a subset of the vertices";
     ie := select(H#"edges",e -> isSubset(set e,set S));
     if not opts#OriginalRing then (
	  R := (coefficientRing H#"ring")(monoid [S]);
	  F := map(R,H#"ring");
     	  ienew := apply(ie,e->apply(e,v->F(v)));
	  return(hyperGraph(R,ienew));
	  );
     hyperGraph(ring H,ie)
     )


-----------------------------------------------------------
-- isBipartite
-- checks if a graph is bipartite
-----------------------------------------------------------

isBipartite = method();
isBipartite Graph := G -> (chromaticNumber G == 2); -- checks if chromatic number is 2


-------------------------------------------------------------
-- isChordal
-- check if a graph is a chordal graph
-------------------------------------------------------------

isChordal = method(); -- based upon Froberg's characterization of chordal graphs
isChordal Graph := G -> (
     I := edgeIdeal complementGraph G;
     graphR := G#"ring";
     if I == ideal(0_graphR) then return true;
     D := min flatten degrees I;
     B := coker gens I;
     R := regularity(B);
     if D-1 =!= R then return false;
     true
     )

-------------------------------------------------------------
-- isCM
-- checks if a (hyper)graph is Cohen-Macaulay
------------------------------------------------------------

isCM = method();

isCM HyperGraph := H -> (
     I := edgeIdeal H;
     codim I == pdim coker gens I
     )

--    R := H#"ring";
--    M := R^1 / edgeIdeal H;
--    Q := R^1 / ideal gens R;
--    D := dim M;
--    Ext^D(Q,M) !=0 and Ext^(D-1)(Q,M) == 0
--    )

------------------------------------------------------------
-- isConnected
-- checks if a graph is connected
-- (the graph is connected <=> A, the adjacency the matrix,
-- and I, the identity matrix of size n, has the 
-- property that (A+I)^{n-1} has no zero entries)
------------------------------------------------------------

isConnected = method();
isConnected HyperGraph := H -> numConnectedComponents H == 1

------------------------------------------------------------
-- isConnectedGraph
-- checks if a graph is connected
-- isolated vertices are considered separate components
------------------------------------------------------------

isConnectedGraph = method();
isConnectedGraph HyperGraph := H -> numConnectedGraphComponents H == 1

------------------------------------------------------------
-- isEdge
-- checks if a set is an edge of a (hyper)graph
------------------------------------------------------------

isEdge = method();
isEdge (HyperGraph, List) := (H,E) -> (
		if class class E === PolynomialRing then E = support E;
		any(H#"edges", G->set G === set E)
	)
isEdge (HyperGraph, RingElement) := (H,E) -> (
		isEdge(H, support E)
	)

-------------------------------------------------------------
-- isForest
-- checks if a (hyper)graph is a tree
------------------------------------------------------------

isForest = method();
isForest Graph := G -> (smallestCycleSize G == infinity);

isForest HyperGraph := H -> (
    E := toList(0..#(H#"edges") -1);
    while #E =!= 0 do (
	L := select(E, i-> isGoodLeaf(H,i));
	if #L === 0 then return false;
        H = hyperGraph(H#"ring", drop(H#"edges", {first L, first L}));
	E = toList(0..#(H#"edges") -1);
    );
    true
    )

-------------------------------------------------------------
-- isGoodLeaf
-- checks if the n-th edge of a hypergraph is a "Good Leaf"
----------------------------------------------------------

isGoodLeaf = method();
isGoodLeaf (HyperGraph, ZZ) := (H,N) -> ( 
     intersectEdges := (A,B) -> set H#"edges"#A * set H#"edges"#B;
     overlaps := apply(select(0..#(H#"edges")-1, M -> M =!= N), M -> intersectEdges(M,N));
     overlaps = sort toList overlaps;
     --Check if the overlaps are totally ordered
     all(1..(#overlaps -1), I -> overlaps#(I-1) <= overlaps#I)
     );

------------------------------------------------------------
-- isGraph
-- checks if a hypergraph is a graph
------------------------------------------------------------

isGraph = method();
isGraph HyperGraph := Boolean => (H) -> (
		H#"edges" == {} or all(H#"edges", e-> #e === 2 )
	)


--------------------------------------------------------------
-- isLeaf
-- checks if the n-th edge of the (hyper)graph is a leaf
--------------------------------------------------------------

isLeaf = method();
isLeaf (HyperGraph, ZZ) := (H,N) -> ( 
     intersectEdges := (A,B) -> set H#"edges"#A * set H#"edges"#B;
     overlaps := apply(select(0..(#(H#"edges")-1), M -> M =!= N), M -> intersectEdges(M,N));
     overlapUnion := sum toList overlaps;
     any(overlaps, branch -> isSubset(overlapUnion,branch))
     )

isLeaf (Graph, ZZ) := (G,N) -> ( 
     any(G#"edges"#N, V -> degreeVertex(G,V) === 1)
     ---Note N refers to an edge index
     )

isLeaf (HyperGraph, RingElement) := (H,V) -> ( 
     E := select(0..#(H#"edges")-1, I -> member(V, H#"edges"#I));
     #E == 1 and isLeaf(H, E#0)
     )

--------------------------------------------------------------
-- isolatedVertices
-- returns vertices contained in no edges
--------------------------------------------------------------

isolatedVertices = method();
isolatedVertices (HyperGraph) := (H) -> ( 
     edgeUnion := sum apply(H#"edges", set);
     if #(H#"edges")==0 then return H#"vertices" else
     select(H#"vertices", v -> not member(v, edgeUnion))
     )

------------------------------------------------------------
-- isPerfect
-- checks if a graph is a perfect graph
------------------------------------------------------------

isPerfect = method();
isPerfect Graph := G -> (
     if hasOddHole G then return false;
     if hasOddHole complementGraph G then return false;
     true
     )

------------------------------------------------------------
-- isSCM
-- checks if (hyper)graph is Sequentially Cohen-Macaulay
-------------------------------------------------------------
--uses GenericInitialIdeals package for the gin
--if the user selects the Gins option

isSCM= method(Options=>{Gins=>false});
isSCM HyperGraph := opts -> H -> (
     J := dual edgeIdeal H;
     if opts#Gins then (
	  g := gin J;
	  return (#(flatten entries mingens g) == #(flatten entries mingens J));
	  );
     degs := sort unique apply(flatten entries gens J,i->first degree i);
     numDegs := #degs;
     count := 0;
     while count < numDegs do (
	  Jdeg:=monomialIdeal super basis(degs#count,J);
	  if regularity Jdeg != degs#count then return false;
	  count = count+1;
	  );
     true
     )
     

------------------------------------------------------------------
-- lineGraph
-- return the graph with E(G) as its vertices where two
--  vertices are adjacent when their associated edges are adjacent in G.
------------------------------------------------------------------

lineGraph = method();

lineGraph HyperGraph := H -> (
    x := local x;
    R := QQ[x_0..x_(#edges(H)-1)];
    E := apply(H#"edges", set);
    L := select(subsets(numgens R, 2), e -> #(E#(e#0) * E#(e#1)) > 0);
    graph(R, apply(L,e->apply(e, i-> x_i)))
    )


-----------------------------------------------------------
-- neighbors
-- returns all the neighbors of a vertex or a set
-----------------------------------------------------------

neighbors = method();

neighbors (HyperGraph, ZZ) :=  (H, N) -> neighbors(H, H#"ring"_N)

neighbors (HyperGraph, RingElement) := (H,V) -> (
     unique select(flatten select(H#"edges", E-> member(V,E)), U-> U =!= V)
     )

neighbors (HyperGraph, List) := (H,L) -> (
     if any(L, N-> class N === ZZ) then L = apply(L, N-> H#"ring"_N);
     unique select(flatten apply(L, V-> neighbors(H,V)), U -> not member(U, L))
     )

------------------------------------------------------------
-- numConnectedComponents
-- the number of connected components of a (hyper)Graph
------------------------------------------------------------

numConnectedComponents = method();
numConnectedComponents HyperGraph:= H -> (
     if (H#"edges" == {}) or (H#"edges"== {{}}) then return 0;
     (rank HH_0 hyperGraphToSimplicialComplex H)+1
     )

------------------------------------------------------------
-- numConnectedGraphComponents
-- the number of connected components of a (hyper)Graph
-- this includes isolated vertices
------------------------------------------------------------

numConnectedGraphComponents = method();
numConnectedGraphComponents HyperGraph := H -> (
     if (H#"edges" == {}) or (H#"edges"== {{}}) then (
	  return #isolatedVertices(H);
	  );
     numConnectedComponents(H) + #isolatedVertices(H)
     )

-----------------------------------------------------------
-- numTriangles
-- returns the number of triangles in a graph
-----------------------------------------------------------

numTriangles = method();
numTriangles Graph := G -> (
     number(ass (coverIdeal G)^2,i->codim i==3)
     )

-----------------------------------------------------------
-- randomGraph
-- returns a graph with a given vertex set and randomly chosen
-- edges with the user determining the number of edges
-----------------------------------------------------------
randomGraph = method();
randomGraph (PolynomialRing,ZZ) := (R,num) -> (
     graph randomUniformHyperGraph(R,2,num)
     )

-----------------------------------------------------------
-- randomUniformHyperGraph
-- returns a random hypergraph on a given vertex set
-- user chooses cardinality of edges and the number of edges
-----------------------------------------------------------

randomUniformHyperGraph = method();
randomUniformHyperGraph (PolynomialRing,ZZ,ZZ) := (R,card,num) -> (
     if card <= 0 then error "cardinalities of hypergraphs must be positive integers";
     if num < 0 then error "number of edges must be nonnegative";
     if num > binomial(numgens R,card) then error "can't make that many edges";
     edges := take(random subsets(gens R,card),num);
     hyperGraph(R,edges)
     )

-----------------------------------------------------------
-- randomHyperGraph
-- returns a random hypergraph on a given vertex set
-- user chooses the size of each edge
-----------------------------------------------------------
recursiveRandomHyperGraph = (V,L,D,BranchLimit,TerminateTime) -> (
     if #D === 0 then return L;
     V = random V;
     W := set take(V, D#0);
     if any(L, l -> all(W, w-> member(w#0,l))) then return null;
     if any(L, l -> all(l, w-> member(w#0,W))) then return null;
     L = append(L, W);
     D = drop(D,1);
     H := recursiveRandomHyperGraph(V,L,D,BranchLimit,TerminateTime);
     I := 0;
     while H === null and I < BranchLimit + #D  and currentTime() < TerminateTime do (
          H = recursiveRandomHyperGraph(V,L,D,BranchLimit,TerminateTime);
	  I = I - 1;
     );
     H
)

randomHyperGraph = method(Options => {TimeLimit => 5, BranchLimit => 3});
randomHyperGraph (PolynomialRing,List) := opts -> (R,D) -> (
     if any(D, d-> d < 0) then error "edge sizes must be nonnegative";
     if any(D, d-> d > dim R) then error "edge sizes cannot exceed the number of variables available";
     if sum(apply(toList(0..#D-1), i-> (binomial(numgens R, D_i))^(-1))) > 1 then return null;
     V := gens R;
     if opts.TimeLimit === 0 then opts.TimeLimit === 24*60*60;
     TerminateTime := currentTime() + opts.TimeLimit;
     H := null;
     i := 0;
     while H === null and i < opts.BranchLimit + #D and currentTime() < TerminateTime do (
	H = recursiveRandomHyperGraph(V,{},D,opts.BranchLimit,TerminateTime);
	i = i+1;
        );
     if H === null then return null;
     hyperGraph(R, apply(H, h-> toList h))
     )

-----------------------------------------------------------
-- ring
-- returns the ring of a hypergraph
-----------------------------------------------------------

ring HyperGraph := H -> H#"ring"

--------------------------------------------------
-- simplicialComplexToHyperGraph
-- make a (hyper)graph from a simplicial complex 
---------------------------------------------------

simplicialComplexToHyperGraph = method()

simplicialComplexToHyperGraph SimplicialComplex := D -> (
	  hyperGraph flatten entries facets D
	  )

------------------------------------------------------
-- smallestCycleSize
-- length of smallest induced cycle in a graph
-------------------------------------------------------
smallestCycleSize = method();

smallestCycleSize Graph := G -> (
     if numTriangles G =!= 0 then return 3;
     R :=  res edgeIdeal complementGraph G;
     smallestCycle := 0;
     i := 1;
     -- this loop determines if there is a non-linear syzygy
     -- the first non-linear syzygy tells us the smallest induced
     -- cycle has length >= 4.  This is based upon 
     -- the paper of Eisenbud-Green-Hulek-Popescu,
     -- "Restricting linear syzygyies: algebra and geometry"
     while  ((smallestCycle == 0) and (i <= pdim betti R)) do (
	  A := R_i;
          B := flatten degrees A     ;
	  t := tally B;
	  if (t #? (i+1)) then (
               d := rank A;
	       if d == t#(i+1) then i = i+1 else smallestCycle = i+2;
               )	   
       	  else smallestCycle = i+2;     
       );
     -- If the resolution is linear, smallestCycle still has the value of 0
     -- Because the resolution is linear, the graph is chordal, by
     -- a result of Froberg.  Since we have taken care of the case
     -- that G has a triangle, the graph will be a tree.
     if smallestCycle==0 then return infinity else return smallestCycle;
     )



------------------------------------------------------------
-- spanningTree
-- returns a spanning tree of a graph
-----------------------------------------------------------

spanningTree = method();
spanningTree Graph:= G-> (
     if #edges(G) === 0 then return G;
     E := G#"edges";
     W := set(G#"vertices"); -- vertices not visited yet
     V := {}; -- vertices visited
     T := {}; -- edges in tree
     M := 0; -- index of vertex to visit next
     while #W > 0 do (
	X := first toList W;
	W = W - set{X};
	V = append(V,X);
	while M < #V do (
	    L := select(E, e-> member(V#M,e) and not member(first toList(set(e)-set{V#M}), V));
	    T = T | L;
	    L = toList(set(flatten L) - set{V#M});
	    V = V | L;
	    W = W - set(L);
	    M = M + 1;
	    );
	);
     graph T
     );


----------------------------------------------------
-- vertexCoverNumber
-- return the vertex cover number of a (hyper)graph
---------------------------------------------------

vertexCoverNumber = method();
vertexCoverNumber HyperGraph := H -> (
     min apply(flatten entries gens coverIdeal H,i->first degree i)
     )

----------------------------------------
-- vertexCovers
-- return all minimal vertex covers 
-- (these are the generators of the Alexander dual of the edge ideal)
----------------------------------------

vertexCovers  = method();
vertexCovers HyperGraph := H -> (
     flatten entries gens coverIdeal H
     )

-----------------------------------------
-- vertices
-- returns the vertices of the (hyper)graph
--------------------------------------------

--exported by SimplicialComplexes
vertices(HyperGraph) := H -> H#"vertices";

beginDocumentation()

---------------------------------------------------------
---------------------------------------------------------
-- Simple Doc information
---------------------------------------------------------
---------------------------------------------------------

--*******************************************************
-- DOCUMENTATION FOR PACKAGE
--*******************************************************

doc ///
	Key 
		EdgeIdeals
	Headline
		A package for working with the edge ideals of (hyper)graphs
	Description
		Text
			@EM "EdgeIdeals"@ is a package to work with the edge ideals of (hyper)graphs.

			An edge ideal is a square-free monomial ideal where the generators of the monomial ideal correspond to the edges
			of the (hyper)graph.  An edge ideal complements the Stanley-Reisner correspondence 
			(see @TO "SimplicialComplexes::SimplicialComplexes" @) by providing an alternative combinatorial interpretation of the 
			monomial generators.  

			This package exploits the correspondence between square-free monomial ideals and the combinatorial
			objects, by using commutative algebra routines to derive information about (hyper)graphs.
			For some of the mathematical background on this material, see Chapter 6 of the textbook 
			{\it Monomial Algebras} by R. Villarreal and the survey paper
			of T. Ha and A. Van Tuyl ("Resolutions of square-free monomial ideals via facet ideals: a survey," 
			Contemporary Mathematics. 448 (2007) 91-117). 

			See the @TO "Constructor Overview"@ and the @TO "Extended Example"@ for some illustrations of 
			ways to use this package.

			{\bf Note:} We require all hypergraphs to be clutters, which are hypergraphs in which no 
			edge is a subset of another. If $H$ is a hypergraph that is not a clutter, then the edge 
			ideal of $H$ is indistinguishable from the edge ideal of the clutter of minimal edges 
			in $H$. (Edges of $H$ that are supersets of other edges would not appear as minimal 
			generators of the edge ideal of $H$.) The edge ideal of a hypergraph is similar to the 
			facet ideal of a simplicial complex, as defined by S. Faridi in  "The facet ideal of a 
			simplicial complex," Manuscripta Mathematica 109, 159-174 (2002).
///

document {
	Key => "Constructor Overview",
	Headline => "a summary of the many ways of making graphs and hypergraphs",
	PARA { "The following is separated into four sections:"},
	UL {"Basic Constructors", "Converting Types", "Special Graphs", "Random (Hyper)Graphs"},
	SUBSECTION "Basic Constructors",
	PARA { 
		"The main way of constructing ", TO "Graph", " and " , TO "HyperGraph", " objects is to use the ",
		TO "graph", " and ", TO "hyperGraph", " methods. These methods are overriden to provide many ways ",
		"of specifying edges." },
	PARA { "For the purposes of the EdgeIdeals package, every graph and hypergraph is associated to a ring ",
		"whose variables correspond to the vertices of the (hyper)graph. Thus, the most explicit way to ",
		"make a graph or hypergraph is by ", TO (graph, PolynomialRing, List), " and ", TO (hyperGraph, PolynomialRing, List), ".",
		"The list parameter must contain edges which themselves are lists of variables in the ring."},
	EXAMPLE {"R = QQ[x,y,z,w];","G = graph(R, {{x,y},{x,z},{y,z},{x,w}})", "H = hyperGraph(R, {{x,y,z},{x,w}})"},
	PARA { "Probably the most convenient way of specifying edges is as a list of monomials. Using the ", TO (graph, List), " and ",
	       TO (hyperGraph, List), " methods implicitly defines the ring of the (hyper)graph to be the ring containing the monomials ",
	       " in the ", TO List, ". The following example gives the same hypergraphs as before."},
	EXAMPLE { "R = QQ[x,y,z,w];", "G = graph {x*y, x*z, y*z, x*w}", "H = hyperGraph {x*y*z, x*w}" },
	PARA { "The ", TO "graph", " and ", TO "hyperGraph", " constructors can also be used to make (hyper)graphs from square-free monomial ideals.",
	       "The minimal generators of the ideal become the edges of the (hyper)graph. The ideal must be generated by quadratics if the ", TO "graph",
	       " constructor is used."},
	EXAMPLE  "G = graph ideal(x*y, x*z, y*z, x*w)",
	SUBSECTION "Converting Types",
	PARA { "In this section, we will see how to convert between ", TO SimplicialComplex ,"es and ", TO HyperGraph, 
	       "s, as well as between ", TO Graph, "s and ", TO HyperGraph, "s."},
	PARA { "The methods ", TO simplicialComplexToHyperGraph, " and ", TO hyperGraphToSimplicialComplex, 
	       " accomplish the former conversion in the following way. In ", TO simplicialComplexToHyperGraph,
	       " facets of the simplicial complex become the edges of the hypergraph, while in ", TO hyperGraphToSimplicialComplex,
	       " the edges of the hypergraph become the facets of the new simplicial complex."
	},
	EXAMPLE { "R = QQ[x,y,z,w];", 
		  "H = hyperGraph {x*y*z,x*w};", 
		  "D = hyperGraphToSimplicialComplex H", 
		  "simplicialComplexToHyperGraph D"},
	PARA { "The conversion of a graph into a hypergraph and vice versa use the constructors ", TO "graph", " and ", TO "hyperGraph", ". ",
	       "Any graph can be converted to a hyperGraph, but when a hyperGraph is converted into a graph, a check is run to ensure ",
	       "that the edges are all of size two. An error will be produced if this is not the case."},
	EXAMPLE { "R = QQ[x,y,z,w];", 
		  "G = graph {x*y, x*z, y*z, x*w};", 
		  "H = hyperGraph G", 
		  "graph H"},
	PARA {  "Since the ", TO Graph, " type is a subclass of ", TO HyperGraph, ", any method that takes a ", TO HyperGraph, 
		" will also work on ", TO Graph, "s. So the conversion from graph to hypergraph is seldom needed; it is ",
		"only needed when a method works differently on graphs than on hypergraphs (see ", TO complementGraph, " for an example)."},
	PARA {  "On the other hand, the conversion from hypergraph to graph is very important as many methods are only defined on graphs. ",
		"In the following example, we use the ", TO isChordal, " method which only applies to graphs and hence necessitates a ",
		"conversion of types."},
	EXAMPLE { "R = QQ[x,y,z,w];", 
		  "D = simplicialComplex {x*y, x*z, y*z, x*w};", 
		  "H = simplicialComplexToHyperGraph D", 
		  "G = graph H", 
		  "isChordal G "},
	SUBSECTION "Special Graphs",
	PARA {  "In addition to the more general constructors, there a number of methods which produce certain special graphs."},
	PARA {  EM "Cycles", " can be constructed using ", TO cycle, " which, depending on the parameters, uses all or some of the variables",
		" in the ring to define a graph cycle."},
	EXAMPLE { "R = QQ[x,y,z,w];", "cycle R", "cycle(R,3)", "cycle {x,y,w} "},
	PARA {  EM "Anti-Cycles", ", the graph complements of cycles, can be constructed using ", TO antiCycle, ", which takes parameters",
		" similar to those of ", TO cycle, "."},
	EXAMPLE { "R = QQ[x,y,z,w];", "antiCycle R"},
	PARA {  EM "Complete graphs", " can be constructed using ", TO completeGraph, ", which defines a graph with every possible edge between",
		" a given set a vertices."},
	EXAMPLE { "R = QQ[x,y,z,w];", "completeGraph R", "completeGraph(R,3)", "completeGraph {x,y,w} "},
	PARA {  EM "Complete multipartite graphs", " can be constructed using ", TO completeMultiPartite, ", which defines a graph with every ",
		"possible edge between certain partitions of the vertices. See ", TO completeMultiPartite, " for more details."},
	EXAMPLE { "R = QQ[a,b,x,y];", "completeMultiPartite(R,2,2)"},
	SUBSECTION "Random (Hyper)Graphs",
	PARA { "Three methods are provided for the construction of random (hyper)graphs."},
	UL{ TOH randomGraph, TOH randomUniformHyperGraph, TOH randomHyperGraph},
	PARA { "Each method allows you to specify the number of edges desired. For the random hypergraph methods, the sizes of the edges must",
	       " also be specified."},
	EXAMPLE { "R = QQ[x,y,z,u,v];", 
		  "randomGraph(R,3)", 
		  "randomUniformHyperGraph(R,2,3)",
		  "randomHyperGraph(R,{3,2,1})"},
	PARA { "The ", TO randomHyperGraph, " method is not guaranteed to return a hypergraph; sometimes it returns null.",
	       " Please see the documentation of this method for more details."},
	SeeAlso => { "Extended Example", Graph, HyperGraph, graph, hyperGraph, simplicialComplexToHyperGraph, hyperGraphToSimplicialComplex, cycle, antiCycle, completeGraph, 
		     completeMultiPartite, randomGraph, randomUniformHyperGraph, randomHyperGraph}
}

document {
	Key => "Extended Example",
	Headline => "an extended example using EdgeIdeals",
	PARA {"This is an example from the write-up of the ", EM "EdgeIdeals", " package in the ", EM "Journal of 
	     Software for Algebra and Geometry: Macaulay 2", "."},
	PARA {"At the heart of the ", EM "EdgeIdeals", " package are two new classes that are entitled ", TO HyperGraph, 
	     " and ", TO Graph, ". The ", TO HyperGraph, " class can only be used to represent hypergraphs. The 
	     class ", TO Graph, " extends from ", TO HyperGraph," and inherits all of the methods of ", TO HyperGraph,
	     ". Functions have been made that accept objects of either type as input."},
	PARA {"In our example below, we illustrate Theorem 6.4.7 from R. Villarreal's ", EM "Monomial Algebras", 
	      ", which says that the independence complex of a Cohen-Macaulay bipartite graph has a simplicial 
	      shelling. We begin by creating a graph and verifying the Cohen-Macaulay and bipartite properties."},
	EXAMPLE { "R = QQ[x_1..x_3,y_1..y_3];",
	     	  "G = graph(R,{x_1*y_1,x_2*y_2,x_3*y_3, x_1*y_2,x_1*y_3,x_2*y_3})",
		  "isCM G and isBipartite G"},
	PARA {"When defining a (hyper)graph, the user specifies the vertex set by defining a polynomial ring, 
	     while the edges are written as a list of square-free monomials (there are alternative ways of listing 
	     the edges).  A (hyper)graph is stored as a hash table which contains the list of edges, the polynomial 
	     ring, and the list of vertices."},
     	EXAMPLE { "L = getGoodLeaf(G)",
	     	  "degreeVertex(G,y_1)",
		  "H = inducedHyperGraph(G, vertices(G) - set(L))"},
	PARA {"A Cohen-Macaulay bipartite graph must contain a leaf, which we retrieve above. We remove the 
	     leaf, to form the induced graph, and at the same time, we identify the vertex of degree one in 
	     the leaf."},
        EXAMPLE { "K = simplicialComplexToHyperGraph independenceComplex H;",
	     	  "edges K"},
     	PARA {"Above, we formed the independence complex of ", TT "H", ", that is, the simplicial complex whose 
	     facets correspond to the maximal independent sets of ", TT "H", ".  We then change the type from a 
	     simplicial complex to a hypergraph, which we call ", TT "K", ". Notice that these edges give a shelling."},
	EXAMPLE { "use ring K;",
	     	  "A = apply(edges(K), e->append(e, y_1));",
		  "B = apply(edges inducedHyperGraph(K, {x_2,x_3}), e-> append(e, x_1));",
		  "shelling = join(A,B)",
		  "independenceComplex(G)"},
	PARA {"Using the method found in the proof of Theorem 6.4.7 from R. Villarreal's ", 
	     EM "Monomial Algebras", ", we now can form a shelling of the original independence complex. Notice 
	     that our shelling is a permutation of the facets of the independence complex defined from ", TT "G", "."},
	SeeAlso => { "Constructor Overview", Graph, HyperGraph, graph, hyperGraph, isCM, isBipartite, getGoodLeaf, 
	     	     degreeVertex, inducedHyperGraph, simplicialComplexToHyperGraph, edges, independenceComplex}
}

doc ///
	Key
		"Connected Components Tutorial"
	Headline 
		clarifying the difference between graph and hypergraph components
	Description
		Text
			In this tutorial, we discuss the various methods that deal with connected components 
			of graphs and hypergraphs. Our main objective is to make a distinction between the
			two different definitions of connected components that are used in the @TO EdgeIdeals@ package.

			A vertex of a (hyper)graph {\tt H} said to be an isolated vertex if
			it is not contained in any edge of {\tt H}. In particular, if a vertex of {\tt H} 
			is contained in a edge of size one then it is not considered isolated.

		Example
			R = QQ[u,v,x,y,z];
			H = hyperGraph({{u,v},{x}});
			isolatedVertices H
		Text
			@EM "Graph Components"@. 
			A connected component of a graph is any maximal set of vertices which 
			are pairwise connected by a (possibly trivial) path. The most important part of this
			definition is that isolated vertices count as connected components. 

			The following methods use this definition of a connected component: @TO connectedGraphComponents@,
			@TO numConnectedGraphComponents@ and @TO isConnectedGraph@. 

			@EM "Hypergraph Components"@.
			A connected component of a hypergraph is any maximal set of vertices which 
			are pairwise connected by a non-trivial path. Here isolated vertices do not count as connected components. 
			
			The following methods use the hypergraph definition of a connected component: @TO connectedComponents@,
			@TO numConnectedComponents@ and @TO isConnected@.

			The next example uses all of these methods on a graph to illustrate the difference between the two definitions.

		Example
			R = QQ[u,v,x,y,z];
			G = graph({{x,y},{y,z}});
			isolatedVertices G
			connectedGraphComponents G
			numConnectedGraphComponents G
			isConnectedGraph G
			connectedComponents G
			numConnectedComponents G
			isConnected G
	SeeAlso
		connectedComponents
		connectedGraphComponents
		isConnected
		isConnectedGraph
		isolatedVertices
		numConnectedComponents
		numConnectedGraphComponents
///

--*******************************************************
-- DOCUMENTATION FOR TYPES
--*******************************************************

---------------------------------------------------------
-- DOCUMENTATION HyperGraph
---------------------------------------------------------


doc ///
	Key
		HyperGraph
	Headline 
		a class for hypergraphs
	Description
		Text
			This class represents hypergraphs. A hypergraph is a tuple {\tt (V,E)} of vertices {\tt V} and edges {\tt E} 
			which are subsets of the vertices. In this package, all hypergraphs have the additional property that no edge
			is a subset of any other edge. Hypergraphs of this form are often referred to as clutters.
		Example
			R = QQ[w,x,y,z];
			H = hyperGraph(R, {{w,x},{w,y,z},{x,y,z}});
			vertices H
			edges H
			ring H
		Text
			Hypergraphs are always associated with a polynomial ring whose variables are the vertices of the hypergraph. 
	SeeAlso
		hyperGraph
		Graph
		"Constructor Overview"
///

doc ///
	Key
		Graph
	Headline 
		a class for graphs
	Description
		Text
			This class represents simple graphs. This class extends @TO HyperGraph@ and hence
			inherits all HyperGraph methods. 
		Example
			R = QQ[w,x,y,z];
			G = graph(R, {{w,x},{w,y},{w,z},{y,z}});
			vertices G
			edges G
			ring G
		Text
			Like hypergraphs, graphs are associated with a polynomial ring whose variables are the 
			vertices of the graph. Isolated vertices should not appear in the edge list. As a 
			consequence, the @TO edgeIdeal@ of a graph is always generated by quadratics. The 
			fact that isolated vertices are not edges in a graph affects the output of the methods 
			@TO connectedComponents@, @TO numConnectedComponents@, and @TO isConnected@. One 
			can use @TO connectedGraphComponents@, @TO numConnectedGraphComponents@, and 
			@TO isConnectedGraph@ to ensure that each isolated vertex is counted as a separate 
			connected component.
	SeeAlso
		graph
		HyperGraph
		"Constructor Overview"
///


---------------------------------------------------------
-- DOCUMENTATION hyperGraph
---------------------------------------------------------

doc ///
	Key
		hyperGraph
		(hyperGraph, PolynomialRing, List)
		(hyperGraph, MonomialIdeal)
		(hyperGraph, Ideal)
		(hyperGraph, List)
		(hyperGraph, Graph)
	Headline 
		constructor for HyperGraph
	Usage
		H = hyperGraph(R,E)
		H = hyperGraph(I)
		H = hyperGraph(J)
		H = hyperGraph(E)
		H = hyperGraph(G)
	Inputs
		R:PolynomialRing
			whose variables correspond to vertices of the hypergraph
		E:List
			a list of edges, which themselves are lists of vertices
		I:MonomialIdeal
			which must be square-free and whose generators become the edges of the hypergraph
		J:Ideal
			which must be square-free monomial and whose generators become the edges of the hypergraph
		G:Graph
			which is to be converted to a HyperGraph
	Outputs 
		H:HyperGraph
	Description
		Text 
			The function {\tt hyperGraph} is a constructor for @TO HyperGraph @.  The user
			can input a hypergraph in a number of different ways, which we describe below.
			The information decribing the hypergraph is stored in a hash table. We require that
			there be no inclusion relations between the edges of a hypergraph; that is, that it
			be a clutter. The reason is that this package is designed for edge ideals, which would
			lose any information about edges that are supersets of other edges.

			For the first possiblity, the user inputs a polynomial ring, which specifices the vertices
			of graph, and a list of the edges of the graph.  The edges are represented as lists.
		Example
			R = QQ[a..f]
			E = {{a,b,c},{b,c,d},{c,d,e},{e,d,f}}
			h = hyperGraph (R,E)
		Text
		        Alternatively, if the polynomial ring has already been defined, it suffices simply to enter
			the list of the edges.
		Example
		        S = QQ[z_1..z_8]
			E1 = {{z_1,z_2,z_3},{z_2,z_4,z_5,z_6},{z_4,z_7,z_8},{z_5,z_7,z_8}}
			E2 = {{z_2,z_3,z_4},{z_4,z_5}}
			h1 = hyperGraph E1
			h2 = hyperGraph E2      
		Text
			The list of edges could also be entered as a list of square-free monomials.
		Example
			T = QQ[w,x,y,z]
			e = {w*x*y,w*x*z,w*y*z,x*y*z}
			h = hyperGraph e           
		Text
			Another option for defining an hypergraph is to use an @TO ideal @ or @TO monomialIdeal @.
		Example
			C = QQ[p_1..p_6]
			i = monomialIdeal (p_1*p_2*p_3,p_3*p_4*p_5,p_3*p_6)
			hyperGraph i
			j = ideal (p_1*p_2,p_3*p_4*p_5,p_6)
			hyperGraph j
		Text
			From any graph we can make a hypergraph with the same edges.
		Example
			D = QQ[r_1..r_5]
			g = graph {r_1*r_2,r_2*r_4,r_3*r_5,r_5*r_4,r_1*r_5}	
			h = hyperGraph g
		Text
			Not all hypergraph constructors are able to make the empty hypergraph, that is, the hypergraph with no
			edges. Specifically, the constructors that take only a list cannot make the empty hypergraph because
			the underlying ring is not given. To define the empty hypergraph, give an explicit polynomial ring or give the (monomial) ideal.
		Example
			E = QQ[m,n,o,p]
			hyperGraph(E, {})
			hyperGraph monomialIdeal(0_E)  -- the zero element of E (do not use 0)
			hyperGraph ideal (0_E)
	SeeAlso
		graph
		"Constructor Overview"
///


---------------------------------------------------------
-- DOCUMENTATION graph
---------------------------------------------------------

doc ///
	Key
		graph
		(graph, PolynomialRing, List)
		(graph, MonomialIdeal)
		(graph, Ideal)
		(graph, List)
		(graph, HyperGraph)
	Headline 
		constructor for Graph
	Usage
		G = graph(R,E)
		G = graph(I)
		G = graph(J)
		G = graph(E)
		G = graph(H)
	Inputs
		R:PolynomialRing
			whose variables correspond to vertices of the hypergraph
		E:List
			a list of edges, which themselves are lists of vertices
		I:MonomialIdeal
			which must be square-free and quadratic, and whose generators become the edges of the graph
		J:Ideal
			which must be square-free, quadratic, and monomial, and whose generators become the edges of the graph
		H:HyperGraph
			to be converted to a graph. The edges in {\tt H} must be of size two.
	Outputs 
		G:Graph
        Description
	        Text	 
		        The function {\tt graph} is a constructor for @TO Graph @, a type of @TO HyperGraph @.  The user
			can input a graph in a number of different ways, which we describe below.  The information
			describing the graph is stored in a hash table.
			
			For the first possiblity, the user inputs a polynomial ring, which specifices the vertices
			of graph, and a list of the edges of the graph.  The edges are represented as lists.
		Example
		        R = QQ[a..f];
			E = {{a,b},{b,c},{c,f},{d,a},{e,c},{b,d}}
			g = graph (R,E) 
		Text
		        As long as the edge list is not empty, the ring can be omitted.
			When a ring is not passed to the constructor, the underlying hypergraph takes its ring from the first variable found.
		Example
			S = QQ[z_1..z_8];
			E1 = {{z_1,z_2},{z_2,z_3},{z_3,z_4},{z_4,z_5},{z_5,z_6},{z_6,z_7},{z_7,z_8},{z_8,z_1}}
			E2 = {{z_1,z_2},{z_2,z_3}}
			g1 = graph E1
			g2 = graph E2      
		Text
			The list of edges could also be entered as a list of square-free quadratic monomials.
		Example
			T = QQ[w,x,y,z];
			e = {w*x,w*y,w*z,x*y,x*z,y*z}
			g = graph e           
		Text
			Another option for defining an graph is to use an @TO ideal @ or @TO monomialIdeal @.
		Example
			C = QQ[p_1..p_6];
			i = monomialIdeal (p_1*p_2,p_2*p_3,p_3*p_4,p_3*p_5,p_3*p_6)
			graph i
			j = ideal (p_1*p_2,p_1*p_3,p_1*p_4,p_1*p_5,p_1*p_6)
			graph j
		Text
			A graph can be made from any hypergraph whose edges are all of size two.
		Example
			D = QQ[r_1..r_5];
			h = hyperGraph {r_1*r_2,r_2*r_4,r_3*r_5,r_5*r_4,r_1*r_5}
			g = graph h
		Text
			Not all graph constructors are able to make the empty graph, that is, the graph with no
			edges. Specifically, the constructors that take only a list cannot make the empty graph because
			the underlying ring is not given. To define the empty graph, give an explicit polynomial 
			ring, or give the (monomial) ideal.
		Example
			E = QQ[m,n,o,p]
			graph(E, {})
			graph monomialIdeal(0_E)  -- the zero element of E (do not use 0)
			graph ideal(0_E)
        SeeAlso
       	        hyperGraph
		"Constructor Overview"
///



---------------------------------------------------------------------------------------------

--**********************************************************
-- DOCUMENTATION FOR FUNCTIONS
--**********************************************************

	      
------------------------------------------------------------
-- DOCUMENTATION equality  ==
------------------------------------------------------------

doc ///
        Key
		(symbol ==, HyperGraph, HyperGraph)
	Headline
	        equality 
	Usage
	        g == h
	Inputs
	        g:HyperGraph
	        h:HyperGraph
	Outputs
	        b:Boolean
		       {\tt true} if {\tt g} and {\tt h} are equal
        Description
	        Text
		       This function determines if two HyperGraphs are mathematically equal.
		       Two HyperGraphs are equal if they are defined over the same ring, have 
                       the same variables, and have the same set of edges. In particular, 
                       the order of the edges and the order of variables within each edge 
                       do not matter.
		Example
                       R = QQ[a..f];
		       g = hyperGraph {{a,b,c},{b,c,d},{d,e,f}};
		       h = hyperGraph {{b,c,d},{a,b,c},{f,e,d}};
		       k = hyperGraph {{a,b},{b,c,d},{d,e,f}};
		       g == h
		       g == k
///

------------------------------------------------------------
-- DOCUMENTATION adjacencyMatrix
------------------------------------------------------------

doc ///
        Key
	        adjacencyMatrix
		(adjacencyMatrix, Graph)
	Headline
	        returns the adjacency Matrix of a graph
	Usage
	        M = adjacencyMatrix G
	Inputs
	        G:Graph
	Outputs
	        M:Matrix
		       the adjacency matrix of the graph
        Description
	        Text
		       This function returns the adjacency matrix of the given graph {\tt G}.  The (i,j)^{th} position
		       of the matrix is 1 if there is an edge between the i^{th} vertex and j^{th} vertex,
		       and 0 otherwise.  The rows and columns are indexed by the variables of the ring and use the 
		       ordering of the variables for determining the order of the rows and columns.
		Example
                       S = QQ[a..f];
		       G = graph {a*b,a*c,b*c,c*d,d*e,e*f,f*a,a*d}
		       t = adjacencyMatrix G
		       T = QQ[f,e,d,c,b,a];
		       G = graph {a*b,a*c,b*c,c*d,d*e,e*f,f*a,a*d}
		       t = adjacencyMatrix G -- although the same graph, matrix is different since variables have different ordering
	SeeAlso
		incidenceMatrix
		(vertices,HyperGraph)
///

------------------------------------------------------------
-- DOCUMENTATION allEvenHoles
------------------------------------------------------------

doc ///
	Key
		allEvenHoles
		(allEvenHoles, Graph)
	Headline 
		returns all even holes in a graph
	Usage
		L = allEvenHoles G
	Inputs
		G:Graph
	Outputs 
		L:List
			returns all even holes contained in {\tt G}.
	Description
	     Text
	     	  The method is based on work of Francisco-Ha-Van Tuyl, looking at the associated primes
		  of the square of the Alexander dual of the edge ideal. An even hole is an even induced
		  cycle (necessarily of length at least four). The algorithm for allEvenHoles uses an 
		  observation of Mermin. Fix an edge, and split this edge into two different edges, 
		  introducing a new vertex. Find all the odd holes in that graph. Do that for each edge 
		  in the graph, one at a time, and pick out all the odd holes containing the additional 
		  vertex. Dropping this vertex from each of the odd holes gives all the even holes in 
		  the original graph.

		  See C.A. Francisco, H.T. Ha, A. Van Tuyl, "Algebraic methods for detecting odd holes in a graph." 
		  (2008) Preprint. {\tt arXiv:0806.1159v1}.
	     Example
	     	  R = QQ[a..f];
		  G = cycle(R,6);
		  allEvenHoles G 
		  H = graph(monomialIdeal(a*b,b*c,c*d,d*e,e*f,a*f,a*d)) --6-cycle with a chord
		  allEvenHoles H --two 4-cycles
	SeeAlso
	     allOddHoles
	     hasOddHole
///

------------------------------------------------------------
-- DOCUMENTATION allOddHoles
------------------------------------------------------------

doc ///
	Key
		allOddHoles
		(allOddHoles, Graph)
	Headline 
		returns all odd holes in a graph
	Usage
		L = allOddHoles G
	Inputs
		G:Graph
	Outputs 
		L:List
			returns all odd holes contained in {\tt G}.
	Description
	     Text
		  An odd hole is an odd induced cycle of length at least 5.
	     	  The method is based on work of Francisco-Ha-Van Tuyl, looking at the associated primes
		  of the square of the Alexander dual of the edge ideal. 

		  See C.A. Francisco, H.T. Ha, A. Van Tuyl, "Algebraic methods for detecting odd holes in a graph." 
		  (2008) Preprint. {\tt arXiv:0806.1159v1}.
	     Example
	     	  R = QQ[x_1..x_6];
		  G = graph({x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_1*x_5,x_1*x_6,x_5*x_6}) --5-cycle and a triangle
		  allOddHoles G --only the 5-cycle should appear
		  H = graph({x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_1*x_5,x_1*x_6,x_5*x_6,x_1*x_4}) --no odd holes
		  allOddHoles H
	SeeAlso
	     allEvenHoles
	     hasOddHole
///
	      
	      
------------------------------------------------------------
-- DOCUMENTATION antiCycle
------------------------------------------------------------

doc ///
	Key
		antiCycle
		(antiCycle, Ring)
		(antiCycle, Ring, ZZ)
		(antiCycle, List)
	Headline
		returns a graph of an anticycle
	Usage
		C = antiCycle R 
		C = antiCycle(R,N)
		C = antiCycle L
	Inputs
		R:Ring
		N:ZZ
			length of anticycle
		L:List
			of vertices to make into the complement of a cycle in the order provided
	Outputs
		C:Graph
			an anticycle on the vertices in {\tt L} or on the variables of {\tt R}.
	Description
		Text
				This function returns the graph that is the complement of the cycle
				obtained from {\tt L} by applying the function @TO cycle@.
		Example
			R = QQ[a,b,c,d,e];
			antiCycle R
			antiCycle(R,4)
			antiCycle {e,c,d,b}
			complementGraph antiCycle R == cycle R
	SeeAlso
		cycle
		"Constructor Overview"
///


	      
------------------------------------------------------------
-- DOCUMENTATION changeRing
------------------------------------------------------------

doc ///
        Key
	        changeRing
		(changeRing, HyperGraph, PolynomialRing, List)
	Headline
	        replaces vertices with variables of a different ring
	Usage
	        G = changeRing(H,R,L)
		G = changeRing(H,R,L, MaximalEdges => B)
	Inputs
	        H:HyperGraph
	        R:PolynomialRing
		    containing the new vertices as variables
	        L:List
		    of substitutions, one variable in R for each vertex of H
	        B:Boolean
	Outputs
	        G:HyperGraph
			over the ring R with edges obtained by making substitutions into the edges of H
        Description
		Text
			This method is meant for moving a @TO HyperGraph@ that is defined over 
			one ring to another ring R. The parameter L must be a list containing 
			variables of R that should replace the vertices of H. For the most 
			basic way to use this method, see the first example:
		Example
		     P = QQ[a,b,c];
		     H = hyperGraph({a*b,b*c});
		     S = QQ[x,y,z,w];
		     changeRing(H,S,{x,z,y})
		Text
			In the example above, {\tt a} is replaced with {\tt x}, {\tt b} is replaced with {\tt z}, and {\tt c} is replaced with {\tt y}. A more complex situation arises when two vertices of {\tt H} are replaced by the same variable.
		Example
		     P = QQ[a,b,c];
		     H = hyperGraph({a*b*c});
		     G = hyperGraph({a*b,b*c});
		     S = QQ[x,y,z,w];
		     changeRing(H,S,{x,y,x})
		     changeRing(G,S,{x,y,x})
		Text
			Note that duplicate variables are removed from edges after substitution. 
			Duplicate edges are also reduced to a single edge. 
			As all HyperGraphs in this package have the property that no edge is a 
			subset of any other edge, some edges may need to be dropped 
			after substitution. This happens in the next example.
		Example
		     P = QQ[a,b,c];
		     H = hyperGraph({a*b,b*c});
		     S = QQ[x,y];
		     changeRing(H,S,{x,y,y})
		     changeRing(H,S,{x,y,y},MaximalEdges=>true)
		Text
			By default, {\tt changeRing} uses minimal edges that appear after substitution to
			construct its output. The optional argument @TO MaximalEdges@ allows 
			one to get the maximal edges instead.
	SeeAlso
		inducedHyperGraph
///


	      
------------------------------------------------------------
-- DOCUMENTATION chromaticNumber
------------------------------------------------------------

doc ///
        Key
	        chromaticNumber
		(chromaticNumber, HyperGraph)
	Headline
	        computes the chromatic number of a hypergraph
	Usage
	        c = chromaticNumber H
	Inputs
	        H:HyperGraph
	Outputs
	        c:ZZ
		       the chromatic number of {\tt H}
        Description
	        Text
		     Returns the chromatic number, the smallest number of colors needed to color the vertices of a graph.  This method
		     is based upon a result of Francisco-Ha-Van Tuyl that relates the chromatic number to an ideal membership problem.
		Example
		     S = QQ[a..f];
		     c4 = cycle(S,4) -- 4-cycle; chromatic number = 2
		     c5 = cycle(S,5) -- 5-cycle; chromatic number = 3
		     k6 = completeGraph S  -- complete graph on 6 vertices; chormatic number = 6
		     chromaticNumber c4
		     chromaticNumber c5
		     chromaticNumber k6
	Caveat
	     This method should not be used with a hypergraph that has an edge of
	     cardinality one since no coloring is possible.
///



------------------------------------------------------------
-- DOCUMENTATION cliqueComplex
------------------------------------------------------------

doc ///
        Key
	        cliqueComplex
		(cliqueComplex, Graph)
	Headline
	        returns the clique complex of a graph
	Usage
	        D = cliqueComplex G
	Inputs
	        G:Graph
	Outputs
	        D:SimplicialComplex
		       the clique complex of {\tt G}
        Description
	        Text
		     This function returns the clique complex of a graph $G$.  This is the simplicial
		     complex whose faces correspond to the cliques in the graph.  That is,
		     $F = \{x_{i_1},...,x_{i_s}\}$ is a face of the clique complex of $G$ if and only
		     if the induced graph on $\{x_{i_1},...,x_{i_s}\}$ is a clique of $G$.
		Example
		     R = QQ[w,x,y,z];
		     e = graph {w*x,w*y,x*y,y*z}  -- clique on {w,x,y} and {y,z}
		     cliqueComplex e  -- max facets {w,x,y} and {y,z}
		     g = completeGraph R
		     cliqueComplex g
	SeeAlso
	     cliqueNumber
	     getCliques
	     getMaxCliques
///



------------------------------------------------------------
-- DOCUMENTATION cliqueNumber
------------------------------------------------------------

doc ///
        Key
	        cliqueNumber
		(cliqueNumber, Graph)
	Headline
	        computes the clique number of a graph
	Usage
	        c = cliqueNumber G
	Inputs
	        G:Graph
	Outputs
	        c:ZZ
		       the clique number of {\tt G}
        Description
	        Text
		     cliqueNumber returns the clique number of a graph, the size of the largest clique
		     contained in the graph.  This number is also related to the dimension of 
		     the clique complex of the graph.
		Example
		     R = QQ[a..d];
		     cliqueNumber completeGraph R
		     G = graph({a*b,b*c,a*c,a*d})
		     cliqueNumber G
		     dim cliqueComplex G + 1 == cliqueNumber G
	SeeAlso
	     cliqueComplex
	     getCliques
	     getMaxCliques
	     
///


	      
------------------------------------------------------------
-- DOCUMENTATION complementGraph
------------------------------------------------------------

doc ///
        Key
	        complementGraph
		(complementGraph, Graph)
		(complementGraph, HyperGraph)
	Headline
	        returns the complement of a graph or hypergraph 
	Usage
	        g = complementGraph G
		h = complementGraph H
	Inputs
	        G:Graph
		H:HyperGraph
	Outputs
	        g:Graph
		       the complement of G, whose edges are the set of edges not in G
		h:HyperGraph
		       the complement of H, whose edge set is found by taking the complement of each
		       edge of H in the vertex set
        Description
	        Text
		       The function {\tt complementGraph} finds the complement of a graph and hypergraph.  Note
		       that this function behaves differently depending upon the type of input.  When applied to a graph,
		       complementGraph returns the graph whose edge set is the set of edges not in G.
		       When applied to a hypergraph, the edge set is found by taking the complement of 
		       each edge of H in the vertex set.
		Example
		       R = QQ[a,b,c,d,e];
		       c5 = graph {a*b,b*c,c*d,d*e,e*a}; -- graph of the 5-cycle
		       complementGraph c5 -- the graph complement of the 5-cycle
		       c5hypergraph = hyperGraph c5 -- the 5-cycle, but viewed as a hypergraph
		       complementGraph c5hypergraph
	Caveat
	        Notice that {\tt complementGraph} works differently on graphs versus hypergraphs.
///

------------------------------------------------------------
-- DOCUMENTATION completeGraph
------------------------------------------------------------

doc ///
	Key
		completeGraph
		(completeGraph, Ring)
		(completeGraph, Ring, ZZ)
		(completeGraph, List)
	Headline
		returns a complete graph
	Usage
		K = completeGraph R
		K = completeGraph(R,n)
		K = completeGraph L
	Inputs
		R:Ring
		n:ZZ
			number of variables to use
		L:List
			of vertices to make into a complete graph
	Outputs
		K:Graph
			a complete graph on the vertices in {\tt L} or on the variables of {\tt R}
	Description
		Text
		        This function returns a special graph, the complete graph.  The input specifies a set of vertices that 
			will have the property that every vertex is adjacent to every other vertex.  Non-specified vertices are
			treated as isolated vertices.
		Example
			R = QQ[a,b,c,d,e];
			completeGraph R
			completeGraph(R,3)
			completeGraph {a,c,e}
	SeeAlso
		completeMultiPartite
		"Constructor Overview"
///

------------------------------------------------------------
-- DOCUMENTATION completeMulitPartite
------------------------------------------------------------

doc ///
	Key
		completeMultiPartite
		(completeMultiPartite, Ring, ZZ,ZZ)
		(completeMultiPartite, Ring, List)
	Headline
		returns a complete multipartite graph
	Usage
		K = completeMultiPartite(R,n,m)
		K = completeMultiPartite(R,L)
	Inputs
		R:Ring
		n:ZZ
			number of partitions
		m:ZZ
			size of each partition
		L:List
			of integers giving the size of each partition, or a list of partitions that are lists of variables
	Outputs
		K:Graph
			the complete multipartite graph on the given partitions
	Description
		Text
			A complete multipartite graph is a graph with a partition of the vertices
			such that every pair of vertices, not both from the same partition, 
			is an edge of the graph. The partitions can be specified by their number 
			and size, by a list of sizes, or by an explicit partition of the variables. 
			Not all variables of the ring need to be used.
		Example
			R = QQ[a,b,c,x,y,z];
			completeMultiPartite(R,2,3)
			completeMultiPartite(R,{2,4})
			completeMultiPartite(R,{{a,b,c,x},{y,z}})
		Text
		        When {\tt n} is the number of variables and {\tt M = 1}, we recover the complete graph.
		Example
		        R = QQ[a,b,c,d,e];
			t1 = completeMultiPartite(R,5,1)
			t2 = completeGraph R
			t1 == t2
        SeeAlso
     	        completeGraph 
		"Constructor Overview"
///

------------------------------------------------------------
-- DOCUMENTATION connectedComponents
------------------------------------------------------------

doc ///
	Key
		connectedComponents
		(connectedComponents, HyperGraph)
	Headline
		returns the connected components of a hypergraph
	Usage
		L = connectedComponents H
	Inputs
		H:HyperGraph
	Outputs
		L:List
			of lists of vertices. Each list of vertices is a connected component of H.
	Description
		Text
			This function returns the connected components of a hypergraph. 
			A connected component of a hypergraph is any maximal set of vertices which 
			are pairwise connected by a non-trivial path. Isolated vertices, which are those 
			not appearing in any edge, do not appear in any connected components. 
			This is in contrast to @TO connectedGraphComponents@ in which isolated 
			vertices form their own connected components. See the @TO "Connected Components Tutorial"@
			for more information.

		Example
			R = QQ[a..l];
			H = hyperGraph {a*b*c, c*d, d*e*f, h*i, i*j, l}
			L = connectedComponents H
			apply(L, C -> inducedHyperGraph(H,C))

		Text
			In the following example, hypergraph {\tt H} contains the isolated vertex 
			{\tt d} and the vertex {\tt c} which is in an edge of size one. Notice that 
			{\tt d} does not appear in any connected component while {\tt c} does.

		Example
			R = QQ[a,b,c,d];
                        H = hyperGraph {a*b, c}
                        connectedComponents H
			isolatedVertices H
        SeeAlso
	     "Connected Components Tutorial"
	     connectedGraphComponents
	     isConnected
	     numConnectedComponents
	     isolatedVertices
///

------------------------------------------------------------
-- DOCUMENTATION connectedGraphComponents
------------------------------------------------------------

doc ///
	Key
		connectedGraphComponents
		(connectedGraphComponents, HyperGraph)
	Headline
		returns the connected components of a graph
	Usage
		L = connectedGraphComponents G
	Inputs
		G:HyperGraph
	Outputs
		L:List
			of lists of vertices. Each list of vertices is a connected component of G.
	Description
		Text
			This function returns the connected components of a graph. 
			A connected component of a graph is any maximal set of vertices which 
			are pairwise connected by a (possibly trivial) path. Isolated vertices, which are those 
			not appearing in any edge, form their own connected components. 
			This is in contrast to @TO connectedComponents@ in which isolated 
			vertices do not appear in any connected components. See the @TO "Connected Components Tutorial"@
			for more information.

		Example
			R = QQ[a..k];
			G = graph {a*b,b*c,c*d,a*d,f*g,h*i,j*k,h*k}
			L = connectedGraphComponents G

		Text
			In the following example, graph {\tt G} contains the isolated vertex 
			{\tt d}. Notice that {\tt d} appears in its own connected component and hence {\tt G}
			is not connected.

		Example
			R = QQ[a,b,c,d];
			G = graph {a*b, b*c}
			connectedGraphComponents G
			isolatedVertices G
			isConnectedGraph G
        SeeAlso
	     "Connected Components Tutorial"
	     connectedComponents
	     isConnectedGraph
	     numConnectedGraphComponents
	     isolatedVertices
///

 
	      
------------------------------------------------------------
-- DOCUMENTATION coverIdeal
------------------------------------------------------------

doc ///
        Key
	        coverIdeal
		(coverIdeal, HyperGraph)
	Headline
	        creates the cover ideal of a (hyper)graph
	Usage
	        I = coverIdeal H
	Inputs
	        H:HyperGraph
	Outputs
	        I:MonomialIdeal
		       the cover ideal of H
        Description
	        Text
		 Returns the monomial ideal generated by the minimal vertex covers.  This is also the Alexander dual 
		 of the edge ideal of the hypergraph {\tt H}.
		Example
		 S = QQ[a,b,c,d,e,f];
		 k6 = completeGraph S  -- complete graph on 6 vertices
		 coverIdeal k6 -- each generator corresponds to a minimal vertex of k6
                 h = hyperGraph {a*b*c,c*d,d*e*f}
		 coverIdeal h
		 dual coverIdeal h == edgeIdeal h
	SeeAlso
		dual
	        edgeIdeal
		vertexCoverNumber
		vertexCovers
///

------------------------------------------------------------
-- DOCUMENTATION cycle
------------------------------------------------------------

doc ///
	Key
		cycle
		(cycle, Ring)
		(cycle, Ring, ZZ)
		(cycle, List)
	Headline
		returns a graph cycle
	Usage
		C = cycle R
		C = cycle(R,n)
		C = cycle L
	Inputs
		R:Ring
		n:ZZ
			length of cycle
		L:List
			of vertices to make into a cycle in the order provided
	Outputs
		C:Graph
			a cycle on the vertices in {\tt L} or on the variables of {\tt R}.
	Description
		Text
		        Give a list of vertices (perhaps in some specified order), this function returns the graph of the
			cycle on those vertices, using the order given or the internal ordering of the
			@TO vertices @.  Unspecified vertices are treated as isolated vertices.
		Example
			R = QQ[a,b,c,d,e];
			cycle R
			cycle(R,3)
			cycle {e,c,d,b}
			R = QQ[a,c,d,b,e];-- variables given different order
			cycle R
	SeeAlso
	        antiCycle
		"Constructor Overview"
///


------------------------------------------------------------
-- DOCUMENTATION degreeVertex
------------------------------------------------------------
doc ///
	Key
		degreeVertex
		(degreeVertex, HyperGraph, ZZ)
		(degreeVertex, HyperGraph, RingElement)
	Headline 
		returns the degree of a vertex
	Usage
		d = degreeVertex(H,n)
		d = degreeVertex(H,V)
	Inputs
		H:HyperGraph
		n:ZZ
			the index of a vertex
		V:RingElement
			a vertex/variable of the HyperGraph
	Outputs 
		d:ZZ
			the degree of vertex {\tt V} (or vertex number {\tt n})
	Description
		Text
			The degree of a vertex in a (hyper)graph is the number of edges that contain the vertex.
			In a graph, the degree is also the number of elements in the neighbor set of a vertex.
	        Example
		        S = QQ[a,b,c,d,e];
			k5 = completeGraph S
			dv = degreeVertex(k5,a)
			n = neighbors(k5,a)
			#n == dv
			degreeVertex(k5,2)
			h = hyperGraph {a*b*c,c*d,a*d*e,b*e,c*e}
			degreeVertex(h,a)
			degreeVertex(h,2) -- degree of c
	SeeAlso
		neighbors
		(vertices,HyperGraph)
///



------------------------------------------------------------
-- DOCUMENTATION deleteEdges
------------------------------------------------------------
 
doc ///
        Key
	        deleteEdges 
		(deleteEdges, HyperGraph, List)
	Headline
	        returns the (hyper)graph with specified edges removed
	Usage
	        h = deleteEdges(H, S) 
	Inputs
		H:HyperGraph
		S:List
		     a subset of the edges of the graph or hypergraph
	Outputs
		h:HyperGraph
		       the hypergraph with edges in S removed
	Description
	        Text
		       This function enables the user to remove specified edges from a hypergraph to form
		       a subhypergraph.
		Example
		       S = QQ[a,b,c,d,e];
		       g = cycle S
		       T = {{a,b},{d,e}}
		       gprime = deleteEdges (g,T)
		       h = hyperGraph {a*b*c,c*d*e,a*e}
		       T = edges h
                       hprime = deleteEdges (h,T)
///



------------------------------------------------------------
-- DOCUMENTATION edgeIdeal
------------------------------------------------------------


doc ///
        Key
	     edgeIdeal
	     (edgeIdeal, HyperGraph)
	Headline
	     creates the edge ideal of a (hyper)graph
	Usage
	     I = edgeIdeal H
	Inputs
	     H:HyperGraph
	Outputs
	     I:MonomialIdeal
	          the edge ideal of H
	Description
	     Text
	     	  The edge ideal of a (hyper)graph is a square-free monomial ideal in which the minimal 
		  generators correspond to the edges of a (hyper)graph.  Along with @TO coverIdeal @,
		  the function {\tt edgeIdeal} enables us to translate many graph theoretic properties into 
		  algebraic properties.
		  
		  When the input is a finite simple graph, that is, a graph with no loops or multiple
		  edges, then the edge ideal is a quadratic square-free monomial ideal generated by
		  terms of the form $x_ix_j$ whenever $\{x_i,x_j\}$ is an edge of the graph.
	     Example
	     	  S = QQ[a..e];
		  c5 = cycle S
		  edgeIdeal c5
		  graph flatten entries gens edgeIdeal c5 == c5 
		  k5 = completeGraph S
		  edgeIdeal k5             
     	     Text
	          When the input is a hypergraph, the edge ideal is a square-free monomial ideal
		  generated by monomials of the form $x_{i_1}x_{i_2}...x_{i_s}$ whenever
		  $\{x_{i_1},...,x_{i_s}\}$ is an edge of the hypergraph.  Because all of our
		  hypergraphs are clutters, that is, no edge is allowed to be a subset of another edge,
		  we have a bijection between the minimal generators of the edge ideal of hypergraph and the edges
		  of the hypergraph.
	     Example
	     	  S = QQ[z_1..z_8];
		  h = hyperGraph {{z_1,z_2,z_3},{z_2,z_3,z_4,z_5},{z_4,z_5,z_6},{z_6,z_7,z_8}}
		  edgeIdeal h
        SeeAlso
	     coverIdeal
///



------------------------------------------------------------
-- DOCUMENTATION edges
------------------------------------------------------------


doc ///
	Key
		edges
		(edges, HyperGraph)
	Headline 
		gets the edges of a (hyper)graph
	Usage
		E = edges(H)
	Inputs
		H:HyperGraph
	Outputs 
		E:List
			of the edges of {\tt H}
	Description
	        Text
		      This function takes a (hyper)graph, and returns the edge set of the (hyper)graph.
	        Example
		       S = QQ[a..d];
		       g = graph {a*b,b*c,c*d,d*a} -- the four cycle
     	       	       edges g
		       h = hyperGraph{a*b*c}
     	       	       edges h	 
		       k4 = completeGraph S
		       edges k4
	SeeAlso
		(vertices,HyperGraph)
///


------------------------------------------------------------
-- DOCUMENTATION getCliques
------------------------------------------------------------

doc ///
	Key
		getCliques
		(getCliques, Graph, ZZ)
		(getCliques, Graph)
	Headline 
		returns cliques in a graph
	Usage
		C = getCliques(G,d)
		C = getCliques G
	Inputs
		G:Graph
		d:ZZ
			representing the size of the cliques desired
	Outputs 
		C:List
			of cliques of size {\tt d} or, if no {\tt d} is entered, all cliques
	SeeAlso
	        cliqueNumber
	Description
		Text
			A clique of a graph is a subset of its vertices which induces a complete subgraph. 
			That is, a set of vertices is a clique if every pair of vertices in the set forms an 
			edge of the graph. This function returns all cliques of a specified size, and if no 
			size is given, it returns all cliques.  Note that all the edges of the graph are 
			considered cliques of size two.
		Example
		     	R = QQ[a..d];
			G = completeGraph R 
     	       	    	getCliques(G,3)
			getCliques(G,4)
			getCliques G
	       
///


------------------------------------------------------------
-- DOCUMENTATION getEdge
------------------------------------------------------------

doc ///
	Key
		getEdge
		(getEdge, HyperGraph, ZZ)
	Headline 
		gets the n-th edge in a (hyper)graph
	Usage
		E = getEdge(H,n)
	Inputs
		H:HyperGraph
		n:ZZ
			an index of an edge in {\tt H}
	Outputs 
		E:List
			the {\tt n}-th edge of {\tt H}
	Description
	        Text
		        This function returns the n^{th} edge of the (hyper)graph.
		Example
		        S = QQ[a..f];
			g = cycle S
			edges g
			getEdge (g,3)  -- counting starts from 0, so the 4th element in the above list
			h = hyperGraph {a*b*c*d,d*e,a*f*c,a*d*f}
     	       	    	getEdge (h,0) -- first edge
	SeeAlso
	        edges
		getEdgeIndex
///

------------------------------------------------------------
-- DOCUMENTATION getEdgeIndex
------------------------------------------------------------

doc ///
	Key
		getEdgeIndex
		(getEdgeIndex, HyperGraph, List)
		(getEdgeIndex, HyperGraph, RingElement)
	Headline 
		finds the index of an edge in a HyperGraph
	Usage
		n = getEdgeIndex(H,E)
		n = getEdgeIndex(H,M)
	Inputs
		H:HyperGraph
		E:List
			of vertices
		M:RingElement
			a monomial that is the product of vertices
	Outputs 
		n:ZZ
			the index of {\tt E} as an edge of {\tt H}. If {\tt E} is not in {\tt H}, 
			then -1 is returned.
	Description
	        Text
		        This function returns the index of the edge of the (hyper)graph, where the ordering
			is determined by the internal ordering of the edges. Note that the internal order of the
			edges may not be preserved by methods that change the hypergraph 
			(i.e., @TO inducedHyperGraph@, @TO changeRing@, @TO (hyperGraph, MonomialIdeal)@, etc.).
		Example
		     	S = QQ[z_1..z_8];
			h = hyperGraph {z_2*z_3*z_4,z_6*z_8,z_7*z_5,z_1*z_6*z_7,z_2*z_4*z_8}
			edges h
			getEdgeIndex (h,{z_2,z_4,z_8})  -- although entered last, edge is internally stored in 4th spot (counting begins at 0)
			getEdge(h,3)
			getEdgeIndex (h,{z_1,z_2}) -- not in the edge list
	SeeAlso
		getEdge
		isEdge
///

------------------------------------------------------------
-- DOCUMENTATION getGoodLeaf
------------------------------------------------------------

doc ///
	Key
		getGoodLeaf
		(getGoodLeaf, HyperGraph)
	Headline 
		returns an edge that is a good leaf
	Usage
		L = getGoodLeaf(H) 
	Inputs
		H:HyperGraph
	Outputs 
		L:List
			of vertices that comprise an edge in H that is a good leaf
	Description
		Text
			A good leaf of a hypergraph {\tt H} is an edge {\tt L} whose intersections
			with all other edges form a totally ordered set. It follows that
			{\tt L} must have a free vertex, i.e., a vertex contained in no other edges. 
			In the graph setting, a good leaf is 
			an edge containing a vertex of degree one.  The notion of a good
			leaf was introduced by X. Zheng in her Ph.D. thesis (2004).
		Example
		     	R = QQ[a..g];
			H = hyperGraph {a*b*c*d, b*c*d*e, c*d*f, d*g, e*f*g};
			getGoodLeaf(H)
	SeeAlso
		getGoodLeafIndex
		hasGoodLeaf
		isGoodLeaf
///

------------------------------------------------------------
-- DOCUMENTATION getGoodLeafIndex
------------------------------------------------------------

doc ///
	Key
		getGoodLeafIndex
		(getGoodLeafIndex, HyperGraph)
	Headline 
		returns the index of an edge that is a good leaf
	Usage
		n = getGoodLeafIndex(H) 
	Inputs
		H:HyperGraph
	Outputs 
		n:ZZ
			the index of an edge in H of a good leaf. 
			Returns -1 if H does not have a good leaf.
	Description
		Text
			A good leaf of hypergraph {\tt H} is an edge {\tt L} whose intersections
			with all other edges form a totally ordered set. It follows that
			{\tt L} must have a free vertex. In the graph setting, a good leaf is 
			an edge containing a vertex of degree one.
			The notion of a good leaf was introduced by X. Zheng in her Ph.D. thesis (2004).
		Example
		     	R = QQ[a..g];
			H = hyperGraph {b*c*d*e, a*b*c*d, c*d*f, d*g, e*f*g};
			getGoodLeaf(H)
			edges(H)
			getGoodLeafIndex(H)
	SeeAlso
		getGoodLeaf
		hasGoodLeaf
		isGoodLeaf
///

------------------------------------------------------------
-- DOCUMENTATION getMaxCliques
------------------------------------------------------------

doc ///
	Key
		getMaxCliques
		(getMaxCliques, Graph)
	Headline 
		returns maximal cliques in a graph
	Usage
		C = getMaxCliques G
	Inputs
		G:Graph
	Outputs 
		C:List
			of cliques of maximal size contained in {\tt G}
	Description
	     	Text
		     This function returns all cliques of maximal size in a graph as a list of lists. For more details, see @TO getCliques@.
		Example
		     	R = QQ[a..d];
			G = completeGraph R 
     	       	    	getMaxCliques G
			H = graph({a*b,b*c,a*c,c*d,b*d})
			getMaxCliques H
	SeeAlso
	        cliqueNumber
		getCliques
///

------------------------------------------------------------
-- DOCUMENTATION hasGoodLeaf
------------------------------------------------------------

doc ///
	Key
		hasGoodLeaf
		(hasGoodLeaf, HyperGraph)
	Headline 
		determines if a HyperGraph contains a good leaf
	Usage
		b = hasGoodLeaf(H) 
	Inputs
		H:HyperGraph
	Outputs 
		b:Boolean
			true if H contains an edge that is a good leaf
	Description
		Text
			A good leaf of hypergraph {\tt H} is an edge {\tt L} whose intersections
			with all other edges form a totally ordered set. It follows that
			{\tt L} must have a free vertex. In the graph setting, a good leaf is 
			an edge containing a vertex of degree one.  The notion of a good
			leaf was introduced by X. Zheng in her Ph.D. thesis (2004).
		Example
		     	R = QQ[a..g];
			H = hyperGraph {b*c*d*e, a*b*c*d, c*d*f, d*g, e*f*g};
			hasGoodLeaf(H)
			getGoodLeaf(H)
	SeeAlso
		getGoodLeaf
		getGoodLeafIndex
		isGoodLeaf
///

------------------------------------------------------------
-- DOCUMENTATION hasOddHole
------------------------------------------------------------

doc ///
	Key
		hasOddHole
		(hasOddHole, Graph)
	Headline 
		tells whether a graph contains an odd hole
	Usage
		b = hasOddHole G
	Inputs
		G:Graph
	Outputs 
		b:Boolean
			{\tt true} if {\tt G} has an odd hole and {\tt false} otherwise
	Description
	     Text
		  An odd hole is an odd induced cycle of length at least 5.
	     	  The method is based on work of Francisco-Ha-Van Tuyl, looking at the associated primes
		  of the square of the Alexander dual of the edge ideal. 

		  See C.A. Francisco, H.T. Ha, A. Van Tuyl, "Algebraic methods for detecting odd holes in a graph." 
		  (2008) Preprint. {\tt arXiv:0806.1159v1}.
	     Example
	     	  R = QQ[x_1..x_6];
		  G = graph({x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_1*x_5,x_1*x_6,x_5*x_6}) --5-cycle and a triangle
		  hasOddHole G
		  H = graph({x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_1*x_5,x_1*x_6,x_5*x_6,x_1*x_4}) --no odd holes
		  hasOddHole H
	SeeAlso
	     allOddHoles
///



------------------------------------------------------------
-- DOCUMENTATION hyperGraphToSimplicialComplex
------------------------------------------------------------

doc ///
	Key
		hyperGraphToSimplicialComplex
		(hyperGraphToSimplicialComplex, HyperGraph)
	Headline 
		makes a simplicial complex from a (hyper)graph
	Usage
		D = hyperGraphToSimplicialComplex H
	Inputs
		H:HyperGraph
	Outputs 
		D:SimplicialComplex
			whose facets are given by the edges of {\tt H}
	Description
		Text
			This function produces a simplicial complex from a (hyper)graph.
			The facets of the simplicial complex are given by the edge set of the (hyper)graph.
			This function is the inverse of @TO simplicialComplexToHyperGraph @ and enables users
			to make use of functions in the package @TO "SimplicialComplexes::SimplicialComplexes" @.
		Example
			R = QQ[x_1..x_6];
			G = graph({x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_1*x_5,x_1*x_6,x_5*x_6}) --5-cycle and a triangle
			DeltaG = hyperGraphToSimplicialComplex G
			hyperGraphDeltaG = simplicialComplexToHyperGraph DeltaG
			GPrime = graph(hyperGraphDeltaG)
			G === GPrime
	SeeAlso
		simplicialComplexToHyperGraph     
		"Constructor Overview"
///


------------------------------------------------------------
-- DOCUMENTATION incidenceMatrix
------------------------------------------------------------

doc ///
        Key
	        incidenceMatrix
		(incidenceMatrix, HyperGraph)
	Headline
	        returns the incidence matrix of a hypergraph
	Usage
	        M = incidenceMatrix H
	Inputs
	        H:HyperGraph
	Outputs
	        M:Matrix
		       the incidence matrix of the hypergraph
        Description
	        Text
			This function returns the incidence matrix of the given hypergraph {\tt H}. 
			The rows of the matrix are indexed by the variables of the hypergraph, 
			and the columns are indexed by the edges. The (i,j)^{th} entry in the 
			matrix is 1 if vertex i is contained in edge j, and is 0 otherwise.
			The order of the rows and columns are determined by the internal order of
			the vertices and edges. See @TO edges@ and @TO (vertices,HyperGraph)@.
		Example
                       S = QQ[a..f];
		       g = hyperGraph {a*b*c*d,c*e,e*f}
		       incidenceMatrix g	  
		       T = QQ[f,e,d,c,b,a];
		       h = hyperGraph {a*b*c*d,c*e,e*f}
		       incidenceMatrix h -- although the same graph, matrix is different since variables have different ordering
	SeeAlso
		adjacencyMatrix
		edges	
		(vertices,HyperGraph)
///

------------------------------------------------------------
-- DOCUMENTATION independenceComplex
------------------------------------------------------------


doc ///
        Key
	        independenceComplex
		(independenceComplex, HyperGraph)
	Headline
	        returns the independence complex of a (hyper)graph 
	Usage
	        D = independenceComplex H
	Inputs
	        H:HyperGraph
	Outputs
	        D:SimplicialComplex
		       the independence complex associated to the (hyper)graph
        Description
	        Text
		       This function associates to a (hyper)graph a simplicial complex whose faces correspond
		       to the independent sets of the (hyper)graph.  See, for example, the paper 
		       of A. Van Tuyl and R. Villarreal 
		       "Shellable graphs and sequentially Cohen-Macaulay bipartite graphs,"
		       Journal of Combinatorial Theory, Series A 115 (2008) 799-814.
	        Example
		       S = QQ[a..e];
		       g = graph {a*b,b*c,c*d,d*e,e*a} -- the 5-cycle
		       independenceComplex g 
		       h = hyperGraph {a*b*c,b*c*d,d*e}
		       independenceComplex h
                Text
		       Equivalently, the independence complex is the simplicial complex associated
		       to the edge ideal of the (hyper)graph {\tt H} via the Stanley-Reisner correspondence.
		Example
		       S = QQ[a..e];
		       g = graph {a*b,b*c,a*c,d*e,a*e}
		       Delta1 = independenceComplex g 
		       Delta2 = simplicialComplex edgeIdeal g
                       Delta1 == Delta2
	SeeAlso
	         independenceNumber       	  
///
     



------------------------------------------------------------
-- DOCUMENTATION independenceNumber
------------------------------------------------------------


doc ///
        Key
	        independenceNumber
		(independenceNumber, Graph)
	Headline
	        determines the independence number of a graph 
	Usage
	        d = independenceNumber G
	Inputs
	        G:Graph
	Outputs
	        d:ZZ
		       the independence number of {\tt G}
        Description
	        Text
		       This function returns the maximum number of independent vertices in a graph.  This number
		       can be found by computing the dimension of the simplicial complex whose faces are the 
		       independent sets (see @TO independenceComplex @) and adding 1 to this number.
                Example
		       R = QQ[a..e];
		       c4 = graph {a*b,b*c,c*d,d*a} -- 4-cycle plus an isolated vertex!!!!
		       c5 = graph {a*b,b*c,c*d,d*e,e*a} -- 5-cycle
		       independenceNumber c4 
		       independenceNumber c5 
		       dim independenceComplex c4 + 1 == independenceNumber c4
		       
        SeeAlso
	        independenceComplex
///

------------------------------------------------------------
-- DOCUMENTATION inducedGraph
------------------------------------------------------------


doc ///
	Key
		inducedGraph
		(inducedGraph, Graph, List)
	Headline
		returns the induced subgraph of a graph
	Usage
		h = inducedGraph(G, L)
	Inputs
		G:Graph
		L:List
			of vertices (i.e. variables in the ring of {\tt G})
	Outputs
		h:Graph
			the induced subgraph of {\tt G} whose edges are contained in {\tt L}
	Description
		Text
			This function returns the induced subgraph of a graph on a specified set of vertices. 
			This function enables the user to create subgraphs of the original graph. 
			{\tt inducedGraph} accepts a @TO Graph@ as input and returns a @TO Graph@ as well. 
			Use @TO inducedHyperGraph@ for a @TO HyperGraph@ instead.
			
			The default option is for the ring of the induced subgraph to contain only 
			variables in {\tt L}. Then the current ring must be changed before working with the 
			induced subgraph. We use this setup to avoid having a lot of isolated vertices in 
			the induced graph. However, one can set the option @TO OriginalRing@ to {\tt true} 
			if one wants to give the induced graph the same ring as the original graph.
			
		Example
			R = QQ[a,b,c,d,e];
			G = graph {a*b,b*c,c*d,d*e,e*a} -- graph of the 5-cycle
			H1 = inducedGraph(G,{b,c,d,e})
			H2 = inducedGraph(G,{a,b,d,e})
			use ring H1
			inducedGraph(H1,{c,d,e})
			use ring G
			inducedGraph(G,{b,c,d,e},OriginalRing=>true) --H1 but in bigger ring
		Text
			Equivalently, one can use @TO changeRing@ (and @TO graph@) to move the induced graph
			back into the original ring.
		Example
			R = QQ[a,b,c,d,e]; 
			G = graph {a*b,b*c,c*d,d*e,e*a} -- graph of the 5-cycle
			H = inducedGraph(G,{b,c,d})
			graph changeRing(H,R,{b,c,d})
        SeeAlso
     	        changeRing
	        deleteEdges
	        inducedHyperGraph
///   

------------------------------------------------------------
-- DOCUMENTATION inducedHyperGraph
------------------------------------------------------------


doc ///
	Key
		inducedHyperGraph
		(inducedHyperGraph, HyperGraph, List)
	Headline
		returns the induced subgraph of a (hyper)graph
	Usage
		h = inducedHyperGraph(H, L)
	Inputs
		H:HyperGraph
		L:List
			of vertices (i.e. variables in the ring of {\tt H})
	Outputs
		h:HyperGraph
			the induced subgraph of {\tt H} whose edges are contained in {\tt L}
	Description
		Text
			This function returns the induced subgraph of a (hyper)graph on a specified set of vertices.  
			This function enables the user to create subgraphs of the original (hyper)graph. 
			
			The default option is for the ring of the induced subgraph to contain only 
			variables in {\tt L}. Then the current ring must be changed before working with the induced subgraph.
			We use this setup to avoid having a lot of isolated vertices in the induced (hyper)graph. However, one
			can set the option @TO OriginalRing@ to {\tt true} if one wants to give the induced (hyper)graph the
			same ring as the original (hyper)graph.
			
			Note: Since @TO Graph@ is a @TO Type@ of @TO HyperGraph@, {\tt inducedHyperGraph} can 
			be used for graphs, but the output is a @TO HyperGraph@. If one prefers to have a 
			graph returned instead, use @TO inducedGraph@. 
		Example
			R = QQ[a,b,c,d,e];
			G = graph {a*b,b*c,c*d,d*e,e*a} -- graph of the 5-cycle
			H1 = inducedHyperGraph(G,{b,c,d,e})
			H2 = inducedHyperGraph(G,{a,b,d,e})
			use ring H1
			inducedHyperGraph(H1,{c,d,e})
			use ring G
			inducedHyperGraph(G,{b,c,d,e},OriginalRing=>true) --H1 but in bigger ring
		Text
			Equivalently, one can use @TO changeRing@ to move the induced hypergraph
			back into the original ring.
		Example
			R = QQ[a,b,c,d,e]; 
			G = graph {a*b,b*c,c*d,d*e,e*a} -- graph of the 5-cycle
			H = inducedHyperGraph(G,{b,c,d})
			changeRing(H,R,{b,c,d})
        SeeAlso
     	        changeRing
	        deleteEdges
	        inducedGraph
///   


------------------------------------------------------------
-- DOCUMENTATION isBipartite
------------------------------------------------------------

doc ///
        Key
	        isBipartite
		(isBipartite, Graph)
	Headline
	        determines if a graph is bipartite
	Usage
	        b = isBipartite G
	Inputs
	        G:Graph
	Outputs
	        b:Boolean
		       {\tt true} if {\tt G} is bipartite, {\tt false} otherwise
        Description
	        Text
		       The function {\tt isBipartite} determines if a given graph is bipartite.  A graph is 
		       said to be bipartite if the vertices can be partitioned into two sets W and Y such
		       that every edge has one vertex in W and the other in Y.  Since a graph is bipartite
		       if and only if its chromatic number is 2, we can check if a graph is bipartite by 
		       computing its chromatic number.
                Example
		       S = QQ[a..e];
		       t = graph {a*b,b*c,c*d,a*e} -- a tree (and thus, bipartite)
		       c5 = cycle S -- 5-cycle (not bipartite)
		       isBipartite t
		       isBipartite c5
	SeeAlso
	        chromaticNumber
///


------------------------------------------------------------
-- DOCUMENTATION isChordal
------------------------------------------------------------

doc ///
        Key
	        isChordal
		(isChordal, Graph)
	Headline
	        determines if a graph is chordal
	Usage
	        b = isChordal G
	Inputs
	        G:Graph
	Outputs
	        b:Boolean
		       {\tt true} if the graph is chordal
	Description
	        Text
		       A graph is chordal if the graph has no induced cycles of length 4 or more (triangles are allowed).
		       To check if a graph is chordal, we use a characterization of Fr\"oberg
		       (see "On Stanley-Reisner rings,"  Topics in algebra, Part 2 (Warsaw, 1988),  57-70, 
		       Banach Center Publ., 26, Part 2, PWN, Warsaw, 1990.) that says that a graph G is
		       chordal if and only if the edge ideal of G^c has a linear resolution, where G^c is 
		       the complementary graph of G.
		Example
		    S = QQ[a..e];
		    C = cycle S;
		    isChordal C
		    D = graph {a*b,b*c,c*d,a*c};
		    isChordal D
                    E = completeGraph S; 
		    isChordal E
///


------------------------------------------------------------
-- DOCUMENTATION isCM
------------------------------------------------------------

doc ///
        Key
	        isCM
		(isCM, HyperGraph)
	Headline
	        determines if a (hyper)graph is Cohen-Macaulay
	Usage
	        b = isCM H
	Inputs
	        H:HyperGraph
	Outputs
	        b:Boolean
		       {\tt true} if the @TO edgeIdeal@ of {\tt H} is Cohen-Macaulay
	Description
	     	Text
		     This uses the edge ideal notion of Cohen-Macaulayness; a hypergraph is called C-M if
		     and only if its edge ideal is C-M.
		Example
		    R = QQ[a..e];
		    C = cycle R;
		    UnmixedTree = graph {a*b, b*c, c*d};
		    MixedTree = graph {a*b, a*c, a*d};
		    isCM C
		    isCM UnmixedTree
		    isCM MixedTree
	SeeAlso
		isSCM
		edgeIdeal
///

------------------------------------------------------------
-- DOCUMENTATION isConnected
------------------------------------------------------------

doc ///
        Key
	        isConnected
		(isConnected, HyperGraph)
	Headline
	        determines if a (hyper)graph is connected
	Usage
	        b = isConnected H
	Inputs
	        H:HyperGraph
	Outputs
	        b:Boolean
		       {\tt true} if {\tt H} is connected, {\tt false} otherwise
        Description
	        Text
			This function checks if the given (hyper)graph {\tt H} is connected. A (hyper)graph is said to be
			connected if it has exactly one connected component. 

			Isolated vertices do not count as connected components and will not make this
			method return {\tt false}. This is in contrast to @TO isConnectedGraph@ in which isolated vertices
			form their own connected components. See the @TO "Connected Components Tutorial"@ for more information.

		Example
			S = QQ[a..e];
			G = graph {a*b,b*c,c*d,d*e,a*e} -- the 5-cycle (connected)
			H = graph {a*b,b*c,c*a,d*e} -- a 3-cycle and a disjoint edge (not connected)
			isConnected G
			isConnected H

		Text
			In the following example, the graph {\tt G} has the isolated vertex {\tt d}. As {\tt d}
			is not considered to be in any connected component, this graph is connected.

		Example
			S = QQ[a,b,c,d];
			G = graph {a*b,b*c} 
			isolatedVertices G
			isConnected G
	SeeAlso
		"Connected Components Tutorial"
		isConnectedGraph
		connectedComponents
		isolatedVertices
		numConnectedComponents
///

------------------------------------------------------------
-- DOCUMENTATION isConnectedGraph
------------------------------------------------------------

doc ///
        Key
	        isConnectedGraph
		(isConnectedGraph, HyperGraph)
	Headline
	        determines if a graph is connected
	Usage
	        b = isConnectedGraph G
	Inputs
	        G:HyperGraph
	Outputs
	        b:Boolean
		       {\tt true} if {\tt G} is connected, {\tt false} otherwise
        Description
	        Text
			This function checks if the given graph {\tt G} is connected. A graph is said to be
			connected if it has exactly one connected component. 

			Isolated vertices form their own connected components and will cause 
			this method return {\tt false}. This is in contrast to @TO isConnected@ in which isolated vertices
			are not in any connected components. See the @TO "Connected Components Tutorial"@ for more information.

		Example
			S = QQ[a..e];
			G = graph {a*b,b*c,c*d,d*e,a*e} -- the 5-cycle (connected)
			H = graph {a*b,b*c,c*a,d*e} -- a 3-cycle and a disjoint edge (not connected)
			isConnectedGraph G
			isConnectedGraph H

		Text
			In the following example, the graph {\tt G} has the isolated vertex {\tt e}. As {\tt d}
			forms its own connected component, this graph is not connected.

		Example
			S = QQ[a..e];
     	       		G = graph {a*b,b*c,c*d,a*d} -- 4-cycle with isolated vertex (not connected)	 
		       	isolatedVertices G
		       	isConnectedGraph G
	SeeAlso
		"Connected Components Tutorial"
	        connectedGraphComponents
		isConnected
		isolatedVertices
		numConnectedGraphComponents
///



------------------------------------------------------------
-- DOCUMENTATION isEdge
------------------------------------------------------------

doc ///
	Key
		isEdge
		(isEdge, HyperGraph, List)
		(isEdge, HyperGraph, RingElement)
	Headline 
		determines if an edge is in a (hyper)graph
	Usage
		b = isEdge(H,E)
		b = isEdge(H,M)
	Inputs
		H:HyperGraph
		E:List
			of vertices
		M:RingElement
			a monomial representing an edge
	Outputs 
		b:Boolean
			{\tt true} iff {\tt E} (or {\tt support M}) is an edge of {\tt H}
	Description
	        Text
		        This function checks if a given edge, represented either as a list or monomial, belongs
			to a given (hyper)graph.
	        Example
		        S = QQ[z_1..z_8];
			h = hyperGraph {z_2*z_3*z_4,z_6*z_8,z_7*z_5,z_1*z_6*z_7,z_2*z_4*z_8};
			edges h
			isEdge (h,{z_2,z_4,z_8})  
			isEdge (h,z_2*z_3*z_4)
			isEdge (h,{z_1,z_2}) 
	SeeAlso
		getEdgeIndex
///

------------------------------------------------------------
-- DOCUMENTATION isForest
------------------------------------------------------------

doc ///
	Key
		isForest
		(isForest, Graph)
		(isForest, HyperGraph)
	Headline 
		determines whether a (hyper)graph is a forest
	Usage
		b = isForest G
		b = isForest H
	Inputs
		G:Graph
		H:HyperGraph
	Outputs 
		b:Boolean
			{\tt true} if {\tt G} (or {\tt H}) is a forest
        Description
	     Text
	        This function determines if a graph or hypergraph is a forest.  A graph is a forest if 
		if the graph has no cycles. We say that a hypergraph is forest if each
		connected component is a tree in the sense of S. Faridi.  See the paper
		"The facet ideal of a simplicial complex," Manuscripta Mathematica 109, 159-174 (2002).
	     Example
	        S = QQ[a..f];
		t = graph {a*b,a*c,a*e}
		isForest t
		h = hyperGraph {a*b*c,c*d*e,b*d*f}
		isForest h
///

------------------------------------------------------------
-- DOCUMENTATION isGoodLeaf
------------------------------------------------------------

doc ///
	Key
		isGoodLeaf
		(isGoodLeaf, HyperGraph, ZZ)
	Headline 
		determines if an edge is a good leaf
	Usage
		b = isGoodLeaf(H,n) 
	Inputs
		H:HyperGraph
		n:ZZ
			index of an edge
	Outputs 
		b:Boolean
			{\tt true} if edge {\tt n} of {\tt H} is a good leaf
	Description
		Text
			A good leaf of hypergraph {\tt H} is an edge {\tt L} whose intersections
			with all other edges form a totally ordered set. It follows that
			{\tt L} must have a free vertex. In the graph setting, a good leaf is 
			an edge containing a vertex of degree one.  The notion of a good
			leaf was introduced by X. Zheng in her Ph.D. thesis (2004).
		Example
		     	R = QQ[a..g];
			H = hyperGraph {a*b*c*d, b*c*d*e, c*d*f, d*g, e*f*g};
			edges(H)
			isGoodLeaf(H,0)
			isGoodLeaf(H,1)
	SeeAlso
		getGoodLeaf
		getGoodLeafIndex
		hasGoodLeaf
///

------------------------------------------------------------
-- DOCUMENTATION isGraph
------------------------------------------------------------

doc ///
        Key
	        isGraph
		(isGraph, HyperGraph)
	Headline
	        determines if a hypergraph is a graph 
	Usage
	        b = isGraph H
	Inputs
	        H:HyperGraph
	Outputs
	        b:Boolean
		       {\tt true} if all edges in {\tt H} have size two
        Description
	        Example
		    QQ[a,b,c,d];
		    isGraph(hyperGraph {a*b,b*c,c*d})
		    isGraph(hyperGraph {a*b,b*c*d})
		    isGraph(hyperGraph {a*b,b*c,d})
///

------------------------------------------------------------
-- DOCUMENTATION isLeaf
------------------------------------------------------------

doc ///
	Key
		isLeaf
		(isLeaf, Graph, ZZ )
		(isLeaf, HyperGraph, ZZ)
		(isLeaf, HyperGraph, RingElement )
	Headline 
		determines if an edge (or vertex) is a leaf of a (hyper)graph
	Usage
		b = isLeaf(G,N)
		b = isLeaf(H,n)
		b = isLeaf(H,V)
	Inputs
		G:Graph
		H:HyperGraph
		n:ZZ
			an index of an edge 
		V:RingElement
			a vertex 
	Outputs 
		b:Boolean
			{\tt true} if edge {\tt n} is a leaf or if vertex {\tt V} has degree 1
        Description
	     Text
		  An edge in a graph is a leaf if it contains a vertex of degree one.
		  An edge {\tt E} in a hypergraph is a leaf if there is another edge {\tt B} with the
	          property that for all edges {\tt F} (other than {\tt E}), the intersection of {\tt F} 
		  with {\tt E} is contained in the interesection of {\tt B} with {\tt E}.

		  A vertex of a graph is a leaf if it has degree one.
		  A vertex of a hypergraph is a leaf if it is contained in precisely one
		  edge which is itself is leaf.
	     Example
     	       	  R = QQ[a..f];
		  G = graph {a*b,b*c,c*a,b*d};
		  isLeaf(G, d)
		  isLeaf(G, getEdgeIndex(G, {b,d}))
		  isLeaf(G, a)
		  isLeaf(G, getEdgeIndex(G, {a,b}))
		  H = hyperGraph {a*b*c,b*d,c*e,b*c*f};
		  K = hyperGraph {a*b*c,b*d,c*e};
		  isLeaf(H, a)
		  isLeaf(H, getEdgeIndex(H, {a,b,c}))
		  isLeaf(K, a)
		  isLeaf(K, getEdgeIndex(K, {a,b,c}))
	SeeAlso	
	    isForest
	    isGoodLeaf
///

------------------------------------------------------------
-- DOCUMENTATION isolatedVertices
------------------------------------------------------------

doc ///
	Key
		isolatedVertices
		(isolatedVertices, HyperGraph)
	Headline 
		returns all vertices not contained in any edge
	Usage
		L = isolatedVertices(H)
	Inputs
		H:HyperGraph
	Outputs 
		L:List
			all vertices that are not contained in any edge of {\tt H}
        Description
	     Text
		  A vertex of a hypergraph is called isolated if it is not contained in any edges.
		  Vertices in a hypergraph that are contained in an edge of size one are not considered to be isolated.
	     Example
     	       	  R = QQ[a,b,c,d,e];
		  G = graph {a*b,c*d}
		  isolatedVertices G 
		  H = hyperGraph {a*b,c}
		  isolatedVertices H 
	SeeAlso	
	    connectedComponents
	    isConnected
	    numConnectedComponents
///

------------------------------------------------------------
-- DOCUMENTATION isPerfect
------------------------------------------------------------

doc ///
	Key
		isPerfect
		(isPerfect, Graph)
	Headline 
		determines whether a graph is perfect
	Usage
		b = isPerfect G
	Inputs
		G:Graph
	Outputs 
		b:Boolean
			{\tt true} if {\tt G} is perfect and {\tt false} otherwise
        Description
	     Text
	     	  The algorithm uses the Strong Perfect Graph Theorem, which says that {\tt G} is
		  perfect if and only if neither {\tt G} nor its complement contains an odd hole.
		  @TO hasOddHole@ is used to determine whether these conditions hold.
	     Example
     	       	  R = QQ[x_1..x_7];
		  G = complementGraph cycle R; --odd antihole with 7 vertices
		  isPerfect G
		  H = cycle(R,4)
		  isPerfect H
		  	     	  
	SeeAlso
		hasOddHole
///

------------------------------------------------------------
-- DOCUMENTATION isSCM
------------------------------------------------------------

doc ///
        Key
	        isSCM
		(isSCM, HyperGraph)
	Headline
	        determines if a (hyper)graph is sequentially Cohen-Macaulay
	Usage
	        b = isSCM H
	Inputs
	        H:HyperGraph
	Outputs
	        b:Boolean
		       {\tt true} if the @TO edgeIdeal@ of {\tt H} is sequentially Cohen-Macaulay
	Description
	     	Text
		     This uses the edge ideal notion of sequential Cohen-Macaulayness; a 
		     hypergraph is called SCM if and only if its edge ideal is SCM. The 
		     algorithm is based on work of Herzog and Hibi, using the Alexander 
		     dual. {\tt H} is SCM if and only if the Alexander dual of the edge ideal 
		     of {\tt H} is componentwise linear.
		     
		     There is an optional argument called @TO Gins@ for {\tt isSCM}. The 
		     default value is {\tt false}, meaning that {\tt isSCM} takes the 
		     Alexander dual of the edge ideal of {\tt H} and checks in all relevant 
		     degrees to see if the ideal in that degree has a linear resolution. In 
		     characteristic zero with the reverse-lex order, one can test for 
		     componentwise linearity using gins, which may be faster in some cases. This
		     approach is based on work of Aramova-Herzog-Hibi and Conca. We make no attempt
		     to check the characteristic of the field or the monomial order, so use caution
		     when using this method. 
		Example
		    R = QQ[a..f];
     	       	    G = cycle(R,4)
		    isSCM G
		    H = graph(monomialIdeal(a*b,b*c,c*d,a*d,a*e)); --4-cycle with whisker
		    isSCM H
		    isSCM(H,Gins=>true) --use Gins technique
	SeeAlso
		isCM
		edgeIdeal
///

------------------------------------------------------------
-- DOCUMENTATION lineGraph
------------------------------------------------------------

doc ///
	Key
		lineGraph
		(lineGraph, HyperGraph)
	Headline 
		returns the line graph of a (hyper)graph
	Usage
		L = lineGraph H
	Inputs
		H:HyperGraph
	Outputs 
		L:Graph
			the line graph of {\tt H}
        Description
	     Text
	     	  The line graph {\tt L} of a hypergraph {\tt H} has a vertex for each edge in {\tt H}. 
		  Two vertices in {\tt L} are adjacent if their edges in {\tt H} share a vertex.
		  The order of the vertices in {\tt L} are determined by the implict order 
		  on the edges of {\tt H}. See @TO edges@.
	     Example
     	       	  R = QQ[a..e];
		  G = graph {a*b,a*c,a*d,d*e}
		  lineGraph G
	SeeAlso
	     edges
///

------------------------------------------------------------
-- DOCUMENTATION neighbors
------------------------------------------------------------

doc ///
	Key
		neighbors
		(neighbors, HyperGraph, RingElement)
		(neighbors, HyperGraph, ZZ)
		(neighbors, HyperGraph, List)
	Headline 
		returns the neighbors of a vertex or list of vertices
	Usage
		N = neighbors(H,V)
		N = neighbors(H,n)
		N = neighbors(H,L)
	Inputs
		H:HyperGraph
		V:RingElement
			a vertex
		n:ZZ
			the index of a vertex
		L:List
			a list of vertices or indices of vertices
	Outputs 
		N:List
			of neighbors to the given vertex or vertices
        Description
	    Text
		The vertices adjacent to vertex {\tt V} are called the neighbors of {\tt V}. The neighbors
		of a list of vertices {\tt L} are those vertices that are not in {\tt L} and are adjacent 
		to a vertex in {\tt L}.
	    Example
     	       	R = QQ[a..f];
		G = graph {a*b, a*c, a*d, d*e, d*f};
		neighbors(G,a)
		neighbors(G,0)
		neighbors(G,{a,d})
		neighbors(G,{0,3})
        SeeAlso
	     degreeVertex
	     (vertices,HyperGraph)
///

------------------------------------------------------------
-- DOCUMENTATION numConnectedComponents
------------------------------------------------------------

doc ///
	Key
		numConnectedComponents
		(numConnectedComponents, HyperGraph)
	Headline 
		returns the number of connected components in a (hyper)graph
	Usage
		d = numConnectedComponents H
	Inputs
		H:HyperGraph
	Outputs 
		d:ZZ
			the number of connected components of {\tt H}
	Description
	     Text
	     	  This function returns the number of connected components of a hypergraph. 
		  A connected component of a hypergraph is any maximal set of vertices which 
		  are pairwise connected by a non-trivial path.  Isolated vertices, which are those 
		  not appearing in any edge, do not count as connected components. 
		  This is in contrast to @TO numConnectedGraphComponents@ in which isolated 
		  vertices are counted as connected components. See the @TO "Connected Components Tutorial"@
		  for more information.

		  The algorithm used by {\tt numConnectedComponents} turns {\tt H} 
		  into a simplicial complex, and then computes the rank of the 0^{th} reduced
		  homology group. This number plus 1 gives the number of connected components of {\tt H}. 

		  We depart from this method in two cases: We define the hypergraph with only 
		  the empty edge (corresponding to the irrelevant simplicial complex) and the 
		  hypergraph with empty edge set (corresponding to the void simplicial complex) 
		  to have 0 connected components.

		  Although this method can be applied to graphs, its output does not match the 
		  most common meaning for the number of connected components of a graph. Instead,
		  one should use @TO numConnectedGraphComponents@.
	     Example
	     	   S = QQ[a..e];
		   g = graph {a*b,b*c,c*d,d*e,a*e} -- the 5-cycle (connected)
		   h = graph {a*b,b*c,c*a,d*e} -- a 3-cycle and a disjoint edge (not connected)
		   numConnectedComponents g
		   numConnectedComponents h
	     Text
		   The following example contains a hypergraph with an edge of size one. The vertex in this edge
		   is not considered isolated and does count as a connected component.
	     Example
		       S = QQ[a..d];
		       H = hyperGraph {a*b,c} 
		       isolatedVertices H
		       connectedComponents H
		       numConnectedComponents H
	SeeAlso
	     "Connected Components Tutorial"
	     connectedComponents
	     numConnectedGraphComponents
	     isConnected
	     isolatedVertices
///

------------------------------------------------------------
-- DOCUMENTATION numConnectedGraphComponents
------------------------------------------------------------

doc ///
	Key
		numConnectedGraphComponents
		(numConnectedGraphComponents, HyperGraph)
	Headline 
		returns the number of connected components in a graph
	Usage
		d = numConnectedGraphComponents G
	Inputs
		G:HyperGraph
	Outputs 
		d:ZZ
			the number of connected components of G
	Description
	     Text
	     	  This function returns the number of connected components of a graph. 
		  A connected component of a graph is any maximal set of vertices which 
		  are pairwise connected by a path.  Isolated vertices, which are those 
		  not appearing in any edge, count as connected components. 
		  This is in contrast to @TO numConnectedComponents@ in which isolated 
		  vertices are not counted as connected components. See the @TO "Connected Components Tutorial"@
		  for more information.

		  The algorithm used by {\tt numConnectedGraphComponents} turns {\tt G} 
		  into a simplicial complex, and then computes the rank of the 0^{th} reduced
		  homology group. This number plus 1 plus the number of isolated vertices of 
		  {\tt G} gives the number of connected components of {\tt G}. 

		  This method is intended to match the most common meaning for the number of
		  connected components of a graph. This method can also be used on
		  hypergraphs.
	     Example
	     	   S = QQ[a..e];
		   g = graph {a*b,b*c,c*d,d*e,a*e} -- the 5-cycle (connected)
		   h = graph {a*b,b*c,c*a,d*e} -- a 3-cycle and a disjoint edge (not connected)
		   k = graph {a*b,b*c,c*d,a*d} -- 4-cycle and isolated vertex (not connected)
		   numConnectedGraphComponents g
		   numConnectedGraphComponents h
		   numConnectedGraphComponents k
	SeeAlso
	     "Connected Components Tutorial"
	     connectedGraphComponents
	     isConnectedGraph
	     isolatedVertices
	     numConnectedComponents
///


------------------------------------------------------------
-- DOCUMENTATION numTriangles
------------------------------------------------------------

doc ///
	Key
		numTriangles
		(numTriangles, Graph)
	Headline 
		returns the number of triangles in a graph
	Usage
		d = numTriangles G
	Inputs
		G:Graph
	Outputs 
		d:ZZ
			the number of triangles contained in {\tt G}
	Description
	     Text
	     	  This method is based on work of Francisco-Ha-Van Tuyl, looking at the associated primes
		  of the square of the Alexander dual of the edge ideal. The algorithm counts the number
		  of these associated primes of height 3.
		  
		  See C.A. Francisco, H.T. Ha, A. Van Tuyl, "Algebraic methods for detecting odd holes in a graph." 
		  (2008) Preprint. {\tt arXiv:0806.1159v1}.
	     Example
	     	  R = QQ[x_1..x_6];
		  G = graph({x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_1*x_5,x_1*x_6,x_5*x_6}) --5-cycle and a triangle
		  numTriangles G
		  H = completeGraph R;
		  numTriangles H == binomial(6,3)
	SeeAlso
	     allOddHoles
	     getCliques
///

------------------------------------------------------------
-- DOCUMENTATION randomGraph
------------------------------------------------------------

doc ///
	Key
		randomGraph
		(randomGraph,PolynomialRing, ZZ)
	Headline 
		returns a random graph
	Usage
		G = randomGraph(R,d)
	Inputs
		R:PolynomialRing
		     which gives the vertex set of {\tt G}
		d:ZZ
		     the number of edges in {\tt G}
	Outputs 
		G:Graph
			a graph with {\tt d} edges on vertex set determined by {\tt R}
	Description
	     Text
	     	  This function allows one to create a graph on an underlying vertex set with 
		  a given number of randomly chosen edges.
	     Example
	     	  R = QQ[x_1..x_9];
		  randomGraph(R,4)
     	       	  randomGraph(R,4)  
	SeeAlso
		randomUniformHyperGraph
		randomHyperGraph
		"Constructor Overview"
///


------------------------------------------------------------
-- DOCUMENTATION randomHyperGraph
------------------------------------------------------------

doc ///
	Key
		randomHyperGraph
		(randomHyperGraph,PolynomialRing,List)
	Headline 
		returns a random hypergraph
	Usage
		H = randomHyperGraph(R,D)
	Inputs
		R:PolynomialRing
		     which gives the vertex set of {\tt H}
		D:List
		     of integers that are the cardinalities of the edges of {\tt H}
	Outputs 
		H:HyperGraph
			a hypergraph with edges {\tt E_i} each of size {\tt D_i}. Returns {\tt null} if none can be found.
	Description
	     Text
		  This method is not guaranteed to return a HyperGraph, even if one exists with the given edge sizes. This method
		  searches for a hypergraph with the given edge sizes using a random recursive algorithm. Limits can be placed on the
		  both number of recursive steps taken (see @TO BranchLimit@) and on the time taken (see @TO TimeLimit@). The method
		  will return null if it cannot find a hypergraph within the branch and time limits.
	     Example
	     	  R = QQ[x_1..x_5];
		  randomHyperGraph(R,{3,2,4})
		  randomHyperGraph(R,{3,2,4})
		  randomHyperGraph(R,{3,2,4})
     	       	  randomHyperGraph(R,{4,4,2,2}) -- impossible, returns null when time/branch limit reached
	     Text
		  The {\tt randomHyperGraph} method will return null immediately if the sizes of the edges fail to pass
		  the LYM-inequality: $1/(n choose D_1) + 1/(n choose D_2) + ... + 1/(n choose D_m) \leq 1$ where $n$ is 
		  the number of variables in {\tt R} and $m$ is the length of {\tt D}. Note that even if {\tt D} passes 
		  this inequality, it is not necessarily true that there is some hypergraph with edge sizes given by {\tt D}.
		  See D. Lubell's "A short proof of Sperner's lemma," J. Combin. Theory, 1:299 (1966).
	SeeAlso
		randomGraph
		randomUniformHyperGraph
		BranchLimit
		TimeLimit
		"Constructor Overview"
///

------------------------------------------------------------
-- DOCUMENTATION randomUniformHyperGraph
------------------------------------------------------------

doc ///
	Key
		randomUniformHyperGraph
		(randomUniformHyperGraph,PolynomialRing,ZZ,ZZ)
	Headline 
		returns a random uniform hypergraph
	Usage
		H = randomUniformHyperGraph(R,c,d)
	Inputs
		R:PolynomialRing
		     which gives the vertex set of {\tt H}
		c:ZZ
		     the cardinality of the edge sets
		d:ZZ
		     the number of edges in {\tt H}
	Outputs 
		H:HyperGraph
			a hypergraph with {\tt d} edges of cardinality {\tt c} on vertex set determined by {\tt R}
	Description
	     Text
	     	  This function allows one to create a uniform hypergraph on an underlying vertex set with 
		  a given number of randomly chosen edges of given cardinality.
	     Example
	     	  R = QQ[x_1..x_9];
		  randomUniformHyperGraph(R,3,4)
     	       	  randomUniformHyperGraph(R,4,2)  
	SeeAlso
		randomGraph
		randomHyperGraph
		"Constructor Overview"
///


------------------------------------------------------------
-- DOCUMENTATION ring
------------------------------------------------------------

doc ///
	Key
		(ring, HyperGraph)
	Headline 
		gives the ring of a (hyper)graph
	Usage
		R = ring H 
	Inputs
		H:HyperGraph
	Outputs 
		R:Ring
	Description
	        Text
		     Every (hyper)graph is defined over some polynomial ring. This method returns the ring of a 
		     hypergraph. 
	        Example
		       S = QQ[a..d];
		       g = cycle S;
		       h = inducedHyperGraph(g,{a,b,c});
		       describe ring g
		       describe ring h
	SeeAlso
	        edges
	        (vertices,HyperGraph)
///


---------------------------------------------------------
-- DOCUMENTATION simplicialComplexToHyperGraph
----------------------------------------------------------



doc ///
	Key
		simplicialComplexToHyperGraph
		(simplicialComplexToHyperGraph, SimplicialComplex)
	Headline 
		makes a (hyper)graph from a simplicial complex
	Usage
		H = simplicialComplexToHyperGraph(D) 
	Inputs
		D:SimplicialComplex
			the input
	Outputs
		H:HyperGraph
			whose edges are the facets of D
	Description
		Text
			This function makes a @TO HyperGraph@ from a @TO SimplicialComplex@.
			The edges of the HyperGraph are given by the facets of the SimplicialComplex.
			This is the inverse of the function @TO hyperGraphToSimplicialComplex @.
		Example
			S = QQ[a..f];
			Delta = simplicialComplex {a*b*c,b*c*d,c*d*e,d*e*f}
			H = simplicialComplexToHyperGraph Delta
	SeeAlso
		hyperGraphToSimplicialComplex
		"Constructor Overview"
///
 

---------------------------------------------------------
-- DOCUMENTATION smallestCycleSize
----------------------------------------------------------
 
doc ///
        Key
	        smallestCycleSize 
		(smallestCycleSize, Graph)
	Headline
	        returns the size of the smallest induced cycle of a graph
	Usage
	        s = smallestCycleSize(G)
	Inputs
		G:Graph
		     the input
	Outputs
		s:ZZ
		     the size of the smallest induced cycle
        Description
	        Text
		     This function returns the size of the smallest induced cycle of a graph.
		     It is based upon Theorem 2.1 in the paper "Restricting linear syzygies:
		     algebra and geometry" by Eisenbud, Green, Hulek, and Popsecu.  This theorem
		     states that if G is graph, then the edge ideal of the complement of G satisfies
		     property N_{2,p}, that is, the resolution of I(G^c) is linear up to the p-th step,
		     if and only if the smallest induced cycle of G has length p+3.  The algorithm
		     looks at the resolution of the edge ideal of the complement to determine the size
		     of the smallest cycle.    	  
		Example      
     	       	     T = QQ[x_1..x_9];
		     g = graph {x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_5*x_6,x_6*x_7,x_7*x_8,x_8*x_9,x_9*x_1} -- a 9-cycle
		     smallestCycleSize g
		     h = graph {x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_5*x_6,x_6*x_7,x_7*x_8,x_8*x_9} -- a tree (no cycles)
		     smallestCycleSize h
		     l =  graph {x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_5*x_6,x_6*x_7,x_7*x_8,x_8*x_9,x_9*x_1,x_1*x_4}
		     smallestCycleSize l
		Text
		     Note that {\tt G} is a tree if and only if {\tt smallestCycleSize G} is {\tt infinity}.
///



------------------------------------------------------
-- DOCUMENTATION spanningTree
----------------------------------------------------------
 
doc ///
        Key
	        spanningTree 
		(spanningTree, Graph)
	Headline
	        returns a spanning tree of a graph
	Usage
	        T = spanningTree(G)
	Inputs
		G:Graph
	Outputs
		T:Graph
		     the spanning tree of G
        Description
	        Text
		     This function returns a breadth first spanning tree of a graph. 
		Example      
     	       	     R = QQ[x_1..x_6];
		     C = cycle R; -- a 6-cycle
		     spanningTree C
		     T = graph {x_1*x_2,x_2*x_3, x_1*x_4,x_1*x_5,x_5*x_6}; -- a tree (no cycles)
		     T == spanningTree T
		     G = graph {x_1*x_2,x_2*x_3,x_3*x_1,x_4*x_5,x_5*x_6,x_6*x_4}; -- two three cycles
		     spanningTree G
///



---------------------------------------------------------
-- DOCUMENTATION vertexCoverNumber
---------------------------------------------------------

doc ///
	Key
		vertexCoverNumber
		(vertexCoverNumber, HyperGraph)
	Headline 
		find the vertex covering number of a (hyper)graph
	Usage
		c = vertexCoverNumber(H) 
	Inputs
		H:HyperGraph
		        the input
	Outputs 
		c:ZZ
			the vertex covering number
        Description
	        Text
		        This function returns the vertex covering number of a (hyper)graph. The vertex covering number is
			the size of smallest vertex cover of the (hyper)graph.  This corresponds to the smallest
			degree of a generator of the cover ideal of the (hyper)graph.
		Example
	                S = QQ[a..d];
			g = graph {a*b,b*c,c*d,d*a} -- the four cycle
			vertexCoverNumber g
		        S = QQ[a..e];
			g = graph {a*b,a*c,a*d,a*e,b*c,b*d,b*e,c*d,c*e,d*e} -- the complete graph K_5
			vertexCoverNumber g
		      	h = hyperGraph {a*b*c,a*d,c*e,b*d*e}
			vertexCoverNumber(h)
        SeeAlso
	        coverIdeal
		vertexCovers
///
 

---------------------------------------------------------
-- DOCUMENTATION vertexCovers
---------------------------------------------------------

doc ///
	Key
		vertexCovers
		(vertexCovers, HyperGraph)
	Headline 
		list the minimal vertex covers of a (hyper)graph
	Usage
		c = vertexCovers(H) 
	Inputs
		H:HyperGraph
		        the input
	Outputs 
		c:List
			of the minimal vertex covers of {\tt H}.  The vertex covers are represented as monomials.
        Description
	        Text
		        This function returns the minimal vertex covers of a (hyper)graph. 
			A vertex cover is a subset of the vertices such that every edge of the (hyper)graph has
			non-empty intersection with this set.  The minimal vertex covers are given by the minimal generators
			of the cover ideal of {\tt H}.
		Example
	                S = QQ[a..d];
			g = graph {a*b,b*c,c*d,d*a} -- the four cycle
			vertexCovers g
		        coverIdeal g
			flatten entries gens coverIdeal g == vertexCovers g
			S = QQ[a..e];
			h = hyperGraph {a*b*c,a*d,c*e,b*d*e}
			vertexCovers(h)
        SeeAlso
	        coverIdeal
		vertexCoverNumber
///
 
 
---------------------------------------------------------
-- DOCUMENTATION vertices
---------------------------------------------------------

doc ///
	Key
		(vertices, HyperGraph)
	Headline 
		gets the vertices of a (hyper)graph
	Usage
		V = vertices(H) 
	Inputs
		H:HyperGraph
		        the input
	Outputs 
		V:List
			of the vertices of {\tt H}
        Description
	        Text
		        This function takes a graph or hypergraph and returns the vertex set of the graph.
		Example
	                S = QQ[a..d];
			g = graph {a*b,b*c,c*d,d*a} -- the four cycle
			vertices g 
			h = hyperGraph{a*b*c}
			vertices h  -- the vertex d is treated as an isolated vertex
        SeeAlso 
	        edges
///

----------------------------
--Options documentation-----
----------------------------

------------------------------------------------------------
-- DOCUMENTATION Gins
------------------------------------------------------------

doc ///
        Key
	        Gins
	Headline
	        optional argument for isSCM
	Description
	     	Text
     	       	    Directs @TO isSCM@ to use generic initial ideals to determine whether the
		    Alexander dual of the edge ideal of a hypergraph is componentwise linear. See
		    full discussion at @TO isSCM@.
	SeeAlso
		isSCM
///

doc ///
     	  Key
	       [isSCM, Gins]
	  Headline
	       use gins inside isSCM
	  Usage
	       B = isSCM(H,Gins=>true)
	  Inputs
	       H:HyperGraph
	       	    the hypergraph being considered
     	  Outputs
	       B:Boolean
	       	    whether {\tt H} is SCM or not
	  Description
	       Text
	       	    The default value for {\tt Gins} is {\tt false} since using generic
		    initial ideals makes the @TO isSCM@ algorithm probabilistic, and there 
		    are characteristic and monomial order requirements. See full 
		    discussion at @TO isSCM@.
	  SeeAlso
	       isSCM
///

------------------------------------------------------------
-- DOCUMENTATION BranchLimit
------------------------------------------------------------

doc ///
        Key
	        BranchLimit
	Headline
	        optional argument for randomHyperGraph
	Description
	     	Text
     	       	    The @TO randomHyperGraph@ method follows a backtracking algorithm
		    to generate edges with no inclusions between them. At each step in the
		    recursive tree,  @TO randomHyperGraph@ will make {\tt BranchLimit} attempts to
		    complete its list of edges. Thus, if a hypergraph with {\tt N} edges is required,
		    @TO randomHyperGraph@ may take {\tt BranchLimit^N} steps before terminating.
		    To be more precise, the method is implemented so that it makes {\tt BranchLimit + L} attempts
		    at level {\tt L} of the recursion.
		    The default value is 3.
	SeeAlso
		TimeLimit
		randomHyperGraph
///

doc ///
        Key
	        [randomHyperGraph, BranchLimit]
	Headline
	        limit recursive branching in randomHyperGraph
	Usage
		G = randomHyperGraph(R,D,BranchLimit=>n)
	Inputs
		n:ZZ
		    number of branches at each level in the recursion of {\tt randomHyperGraph}
	Description
	     	Text
     	       	    The @TO randomHyperGraph@ method follows a backtracking algorithm
		    to generate edges with no inclusions between them. At each step in the
		    recursive tree,  @TO randomHyperGraph@ will make {\tt BranchLimit} attempts to
		    complete its list of edges. Thus, if a hypergraph with {\tt N} edges is required,
		    @TO randomHyperGraph@ may take {\tt BranchLimit^N} steps before terminating.
		    To be more precise, the method is implemented so that it makes {\tt BranchLimit + L} attempts
		    at level {\tt L} of the recursion.
		    The default value is 3.
	SeeAlso
		TimeLimit
		randomHyperGraph
///

------------------------------------------------------------
-- DOCUMENTATION MaximalEdges
------------------------------------------------------------

doc ///
        Key
	        MaximalEdges
	Headline
	        optional argument for changeRing
	Description
	     	Text
     	       	    This is an option to tell @TO changeRing@ whether to 
		    make a hypergraph using maximal or minimal edges after substitution
		    for the variables.
///

doc ///
        Key
	        [changeRing, MaximalEdges]
	Headline
	        optional argument for changeRing
	Usage
	        G = changeRing(H,R,L,MaximalEdges=>true)
	Description
	     	Text
     	       	    This is an option to tell @TO changeRing@ whether to 
		    make a hypergraph using maximal or minimal edges after substitution
		    for the variables.
///

------------------------------------------------------------
-- DOCUMENTATION OriginalRing
------------------------------------------------------------

doc ///
        Key
	        OriginalRing
	Headline
	        optional argument for inducedHyperGraph
	Description
	     	Text
     	       	    This is an option to tell @TO inducedGraph@ and  @TO inducedHyperGraph@ whether to return
		    a (hyper)graph with the original (larger) ring attached or the subring
		    only involving the variables of {\tt L}. The smaller ring is the default.
	SeeAlso
		inducedGraph
		inducedHyperGraph
///

doc ///
     	  Key
	       [inducedGraph, OriginalRing]
	       [inducedHyperGraph, OriginalRing]
	  Headline
	       use OriginalRing inside inducedGraph or inducedHyperGraph
	  Usage
	       J = inducedGraph(G,L,OriginalRing=>true)
	       K = inducedHyperGraph(H,L,OrignalRing=>true)
	  Inputs
	       G:Graph
	       	    the graph being considered
	       H:HyperGraph
	       	    the hypergraph being considered
	       L:List
	       	    a list of vertices
     	  Outputs
	       G:Graph
	       	    the induced Graph of {\tt G} on the vertices of {\tt L}
	       K:HyperGraph
	       	    the induced HyperGraph of {\tt H} on the vertices of {\tt L}
	  Description
	       Text
     	       	    When {\tt OriginalRing} is set to {\tt true}, @TO inducedGraph@ or  @TO inducedHyperGraph@ 
		    returns a (hyper)graph with the original (larger) ring attached rather than the subring
		    only involving the variables of {\tt L}.
	  SeeAlso
	       inducedGraph
	       inducedHyperGraph
///

------------------------------------------------------------
-- DOCUMENTATION TimeLimit
------------------------------------------------------------

doc ///
        Key
	        TimeLimit
	Headline
	        optional argument for randomHyperGraph
	Description
	     	Text
     	       	    The @TO randomHyperGraph@ method follows a backtracking algorithm
		    to generate edges with no inclusions between them. As it may take 
		    a long time to terminate, a time limit is imposed on the method.
		    The {\tt TimeLimit} option is the amount of time, in seconds, that 
		    the method @TO randomHyperGraph@ will take before terminating.
		    A value of 0 is interpreted as one day. The default value is 5 (seconds).
	SeeAlso
		BranchLimit
		randomHyperGraph
///

doc ///
        Key
	        [randomHyperGraph, TimeLimit]
	Headline
	        limit the running time of randomHyperGraph
	Usage
		randomHyperGraph(R,D,TimeLimit=>s)
	Inputs
		s:ZZ
		    time in seconds before randomHyperGraph returns null
	Description
	     	Text
     	       	    The @TO randomHyperGraph@ method follows a backtracking algorithm
		    to generate edges with no inclusions between them. As it may take 
		    a long time to terminate, a time limit is imposed on the method.
		    The {\tt TimeLimit} option is the amount of time, in seconds, that 
		    the method @TO randomHyperGraph@ will take before terminating.
		    A value of 0 is interpreted as one day. The default value is 5 (seconds).
	SeeAlso
		BranchLimit
		randomHyperGraph
///

-----------------------------
-- Constructor Tests --------
-- Test hyperGraph and Graph
-----------------------------

TEST///
R = QQ[a,b,c]
H = hyperGraph(R, {{a,b},{b,c}})
assert(#(edges H) == 2)
assert(#(vertices H) == 3)
///

TEST///
R = QQ[a,b,c]
H = hyperGraph(R, {{a,b,c}})
assert(#(edges H) == 1)
assert(#(vertices H) == 3)
///

TEST///
R = QQ[a,b,c]
H = hyperGraph(R, {a*b,b*c})
assert(#(edges H) == 2)
assert(#(vertices H) == 3)
///

TEST///
R = QQ[a,b,c]
H = hyperGraph({{a,b},{b,c}})
assert(#(edges H) == 2)
assert(#(vertices H) == 3)
///

TEST///
R = QQ[a,b,c]
H = hyperGraph({a*b,b*c})
assert(#(edges H) == 2)
assert(#(vertices H) == 3)
///

TEST///
R = QQ[a,b,c]
H = hyperGraph(ideal {a*b,b*c})
assert(#(edges H) == 2)
assert(#(vertices H) == 3)
///

TEST///
R = QQ[a,b,c]
H = hyperGraph(monomialIdeal {a*b,b*c})
assert(#(edges H) === 2)
assert(#(vertices H) === 3)
///

-----------------------------
-- Test Equality ==
-----------------------------

TEST///
R = QQ[a,b,c,d]
G1 = hyperGraph(R, {{a,b},{b,c}})
G2 = hyperGraph(R, {{a,b},{b,c}})
G3 = hyperGraph(R, {{a,b},{c,b}})
G4 = hyperGraph(R, {{b,c}, {b,a}})
G5 = hyperGraph(R, {{b,c}, {a,c}})

S = QQ[a,b,c]
G6 = hyperGraph(S, {{a,b}, {b,c}})

assert(G1 == G1) 
assert(G1 == G2)
assert(G1 == G3)
assert(G1 == G4)
assert(G1 != G5)
assert(G1 != G6)
///

-----------------------------
-- Test adjacencyMatrix
-----------------------------

TEST///
R = QQ[a..e]
c4 = graph {a*b,b*c,c*d,d*a} -- 4-cycle plus an isolated vertex
adjacencyMatrix c4
m = matrix {{0,1,0,1,0},{1,0,1,0,0},{0,1,0,1,0},{1,0,1,0,0}, {0,0,0,0,0}}
assert(adjacencyMatrix c4 == m)
///

-----------------------------
-- Test allEvenHoles
-----------------------------

TEST///
R=QQ[a..f]
c4=graph {a*b,b*c,c*d,d*a}
assert (#(allEvenHoles c4)==1)
H=graph(monomialIdeal(a*b,b*c,c*d,d*e,e*f,a*f,a*d)) --6-cycle with a chord
assert (allEvenHoles H == {{a, b, c, d}, {a, d, e, f}})
///

-----------------------------
-- Test allOddHoles
-----------------------------

TEST///
R=QQ[a..f]
assert (allOddHoles cycle(R,3) == {})
G=graph(monomialIdeal(a*b,b*c,c*d,d*e,e*f,a*f,a*c))
assert (allOddHoles G == {{a,c,d,e,f}})
///

----------------------------
-- Test antiCycle
---------------------------

TEST///
S= QQ[a..d]
g = graph {a*c,b*d}
assert(antiCycle(S) == g)
assert((complementGraph antiCycle(S)) == cycle(S))
///

------------------------
-- Test changeRing
------------------------ 

TEST///
P = QQ[a,b,c]
H = hyperGraph {a*b,b*c}
G = hyperGraph {a*b*c}
R = QQ[x,y,z,w]
A1 = hyperGraph {x*z,z*y}
A2 = hyperGraph {x*y}
A3 = hyperGraph {x*y}
A4 = hyperGraph {y}
A5 = hyperGraph {x*y}
A6 = hyperGraph {y}
assert(A1 == changeRing(H,R,{x,z,y}))
assert(A2 == changeRing(G,R,{x,y,y}))
assert(A3 == changeRing(H,R,{x,y,x}))
assert(A4 == changeRing(H,R,{x,y,y}))
assert(A5 == changeRing(H,R,{x,y,y}, MaximalEdges=>true))
assert(A6 == changeRing(H,R,{x,y,y}, MaximalEdges=>false))
///

------------------------
-- Test chromaticNumber
------------------------ 

TEST///
R = QQ[a..e]
c4 = graph {a*b,b*c,c*d,d*a} -- 4-cycle
c5 = graph {a*b,b*c,c*d,d*e,e*a} -- 5-cycle
assert(chromaticNumber c4 == 2)
assert(chromaticNumber c5 == 3)
///

--------------------------
-- Test cliqueComplex and cliqueNumber
-------------------------------

TEST///
R=QQ[w,x,y,z]
e = graph {w*x,w*y,x*y,y*z}  -- clique on {w,x,y} and {y,z}
Delta1 = cliqueComplex e  -- max facets {w,x,y} and {y,z}
Delta2 = simplicialComplex {w*x*y,y*z}
assert(Delta1 == Delta2)
assert(cliqueNumber e -1 == dim Delta1)
///

-----------------------------
-- Test complementGraph
-----------------------------

TEST///
R = QQ[a,b,c,d,e]	   
c5 = graph {a*b,b*c,c*d,d*e,e*a} 
c5c = graph {a*c,a*d,b*d,b*e,c*e}
assert(complementGraph c5 == c5c)
///

-----------------------------
-- Test completeGraph
-----------------------------

TEST///
R = QQ[a,b,c,d]	   
assert(completeGraph(R) == graph {a*b,a*c,a*d,b*c,b*d,c*d})
assert(completeGraph(R, 3) == graph {a*b,a*c,b*c})
///

-----------------------------
-- Test completeMultiPartite
-----------------------------

TEST///
R = QQ[a,b,c,d]	   
assert(completeMultiPartite(R, 2,2) == graph {a*d,a*c,b*c,b*d})
assert(completeMultiPartite(R, {1,3}) == graph {a*b,a*c,a*d})
assert(completeMultiPartite(R, {{b},{a,c,d}}) == graph {b*a,b*c,b*d})
///

-----------------------------
-- Test connectedComponents
-----------------------------

TEST///
R = QQ[a..k]	   
H = hyperGraph {a*b, c*d*e,e*k, b*f, g, f*i}
assert(# connectedComponents(H) == 3 )
R = QQ[a,b,c,d]
G = hyperGraph {a*b*c}
H = hyperGraph {a,b,c}
assert(# connectedComponents(G) == 1 )
assert(# connectedComponents(H) == 3 )
///

-----------------------------
-- Test connectedGraphComponents
-----------------------------

TEST///
R = QQ[a..i]	   
H = graph {a*b, b*c,c*d,d*e,a*e,f*g,g*h}
assert(# connectedGraphComponents(H) == 3 )
R = QQ[a..h]
H = graph {a*b, b*c,c*d,d*e,a*e,f*g,g*h}
assert(# connectedGraphComponents(H) == 2)
R = QQ[a,b,c,d]
G = graph {a*b,c*d}
assert(# connectedGraphComponents(G) == 2 )
///

-----------------------------
-- Test coverIdeal
-----------------------------

TEST///
R = QQ[a,b,c]
i = monomialIdeal {a*b,b*c}
j = monomialIdeal {b,a*c}
h = hyperGraph i
assert((coverIdeal h) == j) 
///

-----------------------------
-- Test cycle
-----------------------------

TEST///
R = QQ[a..d]
G = graph(monomialIdeal(a*b,b*c,c*d,a*d))
assert (G == cycle(R,4))
///

-----------------------------
-- Test degreeVertex
-----------------------------

TEST///
R = QQ[a,b,c,d]
H = hyperGraph(monomialIdeal {a*b,b*c,c*d,c*a})
assert( degreeVertex(H,a) == 2)
assert( degreeVertex(H,0) == 2)
assert( degreeVertex(H,b) == 2)
assert( degreeVertex(H,1) == 2)
assert( degreeVertex(H,c) == 3)
assert( degreeVertex(H,2) == 3)
assert( degreeVertex(H,d) == 1)
assert( degreeVertex(H,3) == 1)
///

-----------------------------
-- Test deleteEdges
-----------------------------

TEST///
R = QQ[a..d]
G = deleteEdges(completeGraph R,{{b,d},{a,c}})
assert (G == graph(monomialIdeal(a*b,a*d,b*c,c*d)))
///

-----------------------------
-- Test edgeIdeal
-----------------------------

TEST///
R = QQ[a,b,c]
i = monomialIdeal {a*b,b*c}
h = hyperGraph i
assert((edgeIdeal h) == i) 
///

-----------------------------
-- Test edges
-----------------------------

TEST///
R = QQ[a..d]
G = graph(monomialIdeal(a*b,c*d,a*d))
assert (edges G == {{a,b},{a,d},{c,d}})
///

-----------------------------
-- Test getCliques
-- Test getMaxCliques
-----------------------------

TEST///
R = QQ[a..d]
G = completeGraph R
assert (getMaxCliques G == {{a,b,c,d}})
assert (#(getCliques G) == binomial(4,2)+binomial(4,3)+binomial(4,4))
///

-----------------------------
-- Test getEdge
-----------------------------
TEST///
R = QQ[a..d]
H = hyperGraph(R,{{a,b,d},{d,c,b}})
assert (getEdge(H,0) == {a,b,d})
///

-----------------------------
-- Test getEdgeIndex 
-----------------------------

TEST///
R = QQ[a,b,c]
H = hyperGraph(monomialIdeal {a*b,b*c})
assert( getEdgeIndex(H,{a,b}) == 0)
assert( getEdgeIndex(H,a*b) == 0)
assert( getEdgeIndex(H,{c,b}) == 1)
assert( getEdgeIndex(H,c*b) == 1)
assert( getEdgeIndex(H,{a,c}) == -1)
assert( getEdgeIndex(H,a*c) == -1)
///

-----------------------------
-- Test getGoodLeaf 
-- Test getGoodLeafIndex 
-- Test hasGoodLeaf 
-- Test isGoodLeaf 
-----------------------------

TEST///
R = QQ[a..g]
H = hyperGraph {a*b*c*d,b*c*d*e,c*d*f,d*g,e*f*g}
G = hyperGraph {b*c*d*e,d*g,e*f*g,a*b*c*d}
C = graph {a*b,b*c,c*d,d*e,e*a} -- 5-cycle
assert( getGoodLeaf(H) === {a,b,c,d})
assert( getGoodLeafIndex(H) === getEdgeIndex(H, {a,b,c,d}))
assert( getGoodLeaf(G) === {a,b,c,d})
assert( hasGoodLeaf G )
assert( isGoodLeaf(H, getEdgeIndex(H,{a,b,c,d})) )
assert( not isGoodLeaf(H, getEdgeIndex(H,{b,c,d,e})) )
assert( not hasGoodLeaf C )
///

-----------------------------
-- Test hasOddHole
-- Test isPerfect
-----------------------------

TEST///
R = QQ[a..g]
G = graph {a*b,b*c,c*d,d*e,e*f,f*g,a*g} 
H = complementGraph G
assert hasOddHole G
assert not hasOddHole H
assert not isPerfect G
///

-----------------------------
-- Test hyperGraphToSimplicialComplex
----------------------------------

TEST///
R = QQ[x_1..x_6]
G = graph({x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_1*x_5,x_1*x_6,x_5*x_6}) --5-cycle and a triangle
DeltaG = hyperGraphToSimplicialComplex G
hyperGraphDeltaG = simplicialComplexToHyperGraph DeltaG
GPrime = graph(hyperGraphDeltaG)
assert(G === GPrime)
///

-----------------------------
-- Test incidenceMatrix
-----------------------------

TEST///
R = QQ[a..f]
H = hyperGraph {a*b*c*d,c*d*e,f} 
assert(incidenceMatrix H == matrix {{1,0,0},{1,0,0},{1,1,0},{1,1,0},{0,1,0},{0,0,1}})
///

-----------------------------------
-- Test independenceComplex
-----------------------------------

TEST///
R = QQ[a..e]
c5 = graph {a*b,b*c,c*d,d*e,e*a}
D = simplicialComplex monomialIdeal (a*b,b*c,c*d,d*e,e*a)
assert(D == independenceComplex c5)
///


-----------------------------
-- Test independenceNumber
-----------------------------

TEST///
R = QQ[a..e]
c4 = graph {a*b,b*c,c*d,d*a} -- 4-cycle plus an isolated vertex!!!!
c5 = graph {a*b,b*c,c*d,d*e,e*a} -- 5-cycle
assert(independenceNumber c4 == 3)
assert(independenceNumber c5 == 2)
///

-----------------------------
-- Test inducedGraph
-----------------------------

TEST///
R = QQ[a..e]
G = graph {a*b,b*c,c*d,d*e,a*e}
assert(inducedHyperGraph(G,{a,d,e},OriginalRing=>true)==graph(monomialIdeal(d*e,a*e)))
H = inducedGraph(G,{a,d,e})
use ring H
assert(H == graph(monomialIdeal(d*e,a*e)))
///

-----------------------------
-- Test inducedHyperGraph
-----------------------------

TEST///
R = QQ[a..e]
G = graph {a*b,b*c,c*d,d*e,a*e}
assert(inducedHyperGraph(G,{a,d,e},OriginalRing=>true)==graph(monomialIdeal(d*e,a*e)))
H = inducedHyperGraph(G,{a,d,e})
use ring H
assert(H == hyperGraph(monomialIdeal(d*e,a*e)))
///

TEST///
R = QQ[a,b,c]
G = graph {a*b,b*c}
H = inducedHyperGraph(G,{b,c})
f = map(ring H, ring G)
I = inducedHyperGraph(H, {f(c)})
assert(ring a === ring b)
///

-----------------------------
-- Test isBipartite
-----------------------------

TEST///
R = QQ[a..e]
c4 = graph {a*b,b*c,c*d,d*a} -- 4-cycle
c5 = graph {a*b,b*c,c*d,d*e,e*a} -- 5-cycle
assert(isBipartite c4 == true)
assert(isBipartite c5 == false)
///


-----------------------------------
-- Test isChordal
----------------------------------

TEST///
R = QQ[a..e];
C = cycle R;
assert(isChordal C == false)
D = graph {a*b,b*c,c*d,a*c}
assert(isChordal D == true)
assert((isChordal completeGraph (QQ[a..e])) == true)
///

-----------------------------
-- Test isCM
-----------------------------

TEST///
R = QQ[a..e];
C = cycle R;
UnmixedTree = graph {a*b, b*c, c*d};
MixedTree = graph {a*b, a*c, a*d};
assert isCM C
assert isCM UnmixedTree
assert not isCM MixedTree
///		      

-----------------------------
-- Test isConnected
-----------------------------

TEST///
S = QQ[a..e]
g = graph {a*b,b*c,c*d,d*e,a*e} -- the 5-cycle (connected)
h = graph {a*b,b*c,c*a,d*e} -- a 3-cycle and a disjoint edge (not connected)
assert(isConnected g) 
assert(not isConnected h)
///

-----------------------------
-- Test isConnectedGraph
-----------------------------

TEST///
S = QQ[a..e]
g = graph {a*b,b*c,c*d,d*e,a*e} -- the 5-cycle (connected)
h = graph {a*b,b*c,c*d,a*d} -- 4-cycle with isolated vertex (not connected)	 
k = graph {a*b,b*c,c*a,d*e} -- a 3-cycle and a disjoint edge (not connected)
assert(isConnectedGraph g)
assert(not isConnectedGraph h)
assert(not isConnectedGraph k)
///

-----------------------------
-- Test isEdge 
-----------------------------

TEST///
R = QQ[a,b,c]
H = hyperGraph(monomialIdeal {a*b,b*c})
assert( isEdge(H,{a,b}) )
assert( isEdge(H,a*b) )
assert( isEdge(H,{c,b}) )
assert( isEdge(H,b*c) )
assert( not isEdge(H,{a,c}) )
assert( not isEdge(H,a*c) )
///

-----------------------------
-- Test isForest 
-----------------------------

TEST///
R = QQ[a..h]
H = hyperGraph {a*b*h, b*c*d, d*e*f, f*g*h, b*d*h*f}
K = hyperGraph {a*b*h, b*c*d, d*e*f, b*d*h*f}
G = graph {a*b,b*c,b*d,d*e, f*g, g*h}
J = graph {a*b,b*c,b*d,d*e, f*g, g*h, e*a}
assert( not isForest H )
assert( isForest K )
assert( isForest G )
assert( not isForest J )
assert( isForest hyperGraph G )
assert( not isForest hyperGraph J )
///

-----------------------------
-- Test isLeaf
-----------------------------

TEST///
R = QQ[a..e]
G = graph {a*b,b*c,c*d,d*a,a*e} 
H = hyperGraph {a*b*c,b*d,c*e} 
I = hyperGraph {a*b*c,b*c*d,c*e} 
assert(isLeaf(G,4))
assert(not isLeaf(G,1))
assert(not isLeaf(G,0))
assert(not isLeaf(G,a))
assert(isLeaf(G,e))
assert(not isLeaf(H,0))
assert(isLeaf(I,0))
///

-----------------------------
-- Test isolatedVertices
-----------------------------

TEST///
R = QQ[a..e]
G = graph {a*b,c*d} 
H = hyperGraph ideal(0_R)
assert(isolatedVertices(G) == {e})
assert(isolatedVertices(H) == {a,b,c,d,e})
///

-----------------------------
-- Test lineGraph
-----------------------------

TEST///
R = QQ[a..e]
G = graph {a*b,a*c,a*d,d*e} 
assert(adjacencyMatrix lineGraph G == matrix {{0,1,1,0},{1,0,1,0},{1,1,0,1},{0,0,1,0}})
///

-----------------------------
-- Test neighbors
-----------------------------

TEST///
S = QQ[a..f]
G = graph {a*b,a*c,a*d,d*e,d*f} 
assert(apply(gens S, V -> #neighbors(G, V)) == {3,1,1,3,1,1})
assert(apply(numgens S, N -> #neighbors(G, N)) == {3,1,1,3,1,1})
assert(neighbors(G, {a,c}) == {b,d})
assert(neighbors(G, {e,f}) == {d})
///


-----------------------------
-- Test numConnectedComponents
-----------------------------
TEST///
S = QQ[a..e]
g = graph {a*b,b*c,c*d,d*e,a*e} -- the 5-cycle (connected)
h = graph {a*b,b*c,c*a,d*e} -- a 3-cycle and a disjoint edge (not connected)
assert(numConnectedComponents g == 1) 
assert(numConnectedComponents h == 2)
///

-----------------------------
-- Test numConnectedGraphComponents
-----------------------------
TEST///
S = QQ[a..e]
g = graph {a*b,b*c,c*d,d*e,a*e} -- the 5-cycle (connected)
h = graph {a*b,b*c,c*a,d*e} -- a 3-cycle and a disjoint edge (not connected)
k = graph {a*b,b*c,c*d,a*d} -- 4-cycle and isolated vertex (not connected)
numConnectedGraphComponents g
numConnectedGraphComponents h
numConnectedGraphComponents k
assert(numConnectedGraphComponents g == 1) 
assert(numConnectedGraphComponents h == 2)
assert(numConnectedGraphComponents k == 2)
///

-------------------------------------
-- Test randomGraph
-------------------------------------

TEST///
R = QQ[a..e]
G = randomGraph(R,3)
assert(#(edges G) == 3)
assert(vertices G == {a,b,c,d,e})
///

-------------------------------------
-- Test randomUniformHyperGraph
-------------------------------------

TEST///
R = QQ[a..e]
G = randomUniformHyperGraph(R,3,2)
assert(#(edges G)==2)
assert(#(first edges G)==3)
///

-------------------------------------
-- Test ring
-------------------------------------

TEST///
S = QQ[a..e]
G = graph monomialIdeal(a*b,c*e)
assert(ring G === S)
H = hyperGraph(S,{{a,b,c},{a,d}})
assert(ring H === S)
///

-------------------------------------
-- Test simplicialComplexToHyperGraph
-------------------------------------

TEST///
S = QQ[a..f]
Delta = simplicialComplex {a*b*c,b*c*d,c*d*e,d*e*f}
h = simplicialComplexToHyperGraph Delta
assert(class h === HyperGraph)
///

-----------------------------
-- Test smallestCycleSize
-----------------------------

TEST///
T = QQ[x_1..x_9]
g = graph {x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_5*x_6,x_6*x_7,x_7*x_8,x_8*x_9,x_9*x_1} -- a 9-cycle
assert(smallestCycleSize g == 9)
///

--------------------------------
-- Test spanningTree
------------------------------
TEST///
T = QQ[x_1..x_9]
h = graph {x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_5*x_6,x_6*x_7,x_7*x_8,x_8*x_9} -- a tree (no cycles)
assert(spanningTree h === h)
///

-----------------------------
-- Test vertexCoverNumber
-----------------------------
TEST///
S = QQ[a..d]
g = graph {a*b,b*c,c*d,d*a} -- the four cycle
assert(vertexCoverNumber g == 2)
///

-----------------------------
-- Test vertexCovers
-----------------------------
TEST///
S = QQ[a..d]
g = graph {a*b,b*c,c*d,d*a} -- the four cycle
vertexCovers g
coverIdeal g
assert(flatten entries gens coverIdeal g == vertexCovers g)
///

-----------------------------
-- Test vertices
-----------------------------

TEST///
R = QQ[a..f]
G = graph {a*b,b*c,c*d,d*e,e*f} 
V = vertices(G)
assert(vertices(G) == {a,b,c,d,e,f})
///


end


--restart
--installPackage ("EdgeIdeals", UserMode=>true)
--loadPackage "EdgeIdeals"
--viewHelp

