-*
Copyright 2020 Carlos Amendola, Luis David Garcia Puente, Roser Homs Pons, 
Olga Kuznetsova, Harshit J Motwani.

You may redistribute this file under the terms of the GNU General Public
License as published by the Free Software Foundation, either version 2 of
the License, or any later version.
*-

newPackage(
        "StatGraphs",
        Version => "0.1", 
        Date => "3 August 2020",
        Authors => {{Name=> "Carlos Amendola", 
	   Email=> "carlos.amendola@tum.de",
	   HomePage=>"http://www.carlos-amendola.com/"},
       
	  {Name => "Luis David Garcia Puente",
	   Email => "lgarcia@shsu.edu",
	   HomePage => "http://www.shsu.edu/~ldg005"},
       
          {Name=> "Roser Homs Pons", 
	   Email=> "roser.homs@tum.de",
	   HomePage=>"https://personal-homepages.mis.mpg.de/homspons/index.html"},
       
          {Name=> "Olga Kuznetsova", 
	   Email=> "kuznetsova.olga@gmail.com",
	   HomePage=>"https://okuznetsova.com"},
       
          {Name=> "Harshit J Motwani", 
	   Email=> "harshitmotwani2015@gmail.com",
	   HomePage=> "https://sites.google.com/view/harshitjmotwani/home"}},
        Headline => "Graphs specific for algebraic statistics",
	Keywords => {"Algebraic Statistics", "Graph Theory"},
        DebuggingMode => false,
	PackageExports => {"Graphs"}
        )

export {
    "Bigraph",
    "bigraph",
    "MixedGraph",
    "mixedGraph",
    "collateVertices",
    "partitionLMG",
    "isLoopless",
    "undirectedGraph"
    }

if Graphs.Options.Version < "0.3.3" then error "StatGraphs requires Graphs version 0.3.3 or later"

Bigraph = new Type of Graph

bigraph = method(TypicalValue =>Bigraph, Options => {Singletons => null, EntryMode => "auto"})
bigraph HashTable := opts -> g -> new Bigraph from graph(g, opts)
bigraph List := opts -> L -> new Bigraph from graph(L, opts)
bigraph (List, List):= opts -> (V,L) -> new Bigraph from graph(V,L, opts)
bigraph (List, Matrix) :=  opts -> (V,A) -> new Bigraph from graph(V,A, opts)
bigraph Matrix := opts -> A -> new Bigraph from graph(A, opts)


MixedGraph = new Type of HashTable

mixedGraph = method(TypicalValue =>MixedGraph)
mixedGraph (Graph, Digraph, Bigraph) := (g,d,b) -> (
    C := new MutableHashTable;
    C#cache = new CacheTable from {};
    h := new MutableHashTable;
    h#Graph = g;
    h#Digraph = d;
    h#Bigraph = b;
    C#graph = new HashTable from h;
    new MixedGraph from C)
mixedGraph Graph := g -> mixedGraph(g,digraph {}, bigraph {})
mixedGraph Digraph := d -> mixedGraph(graph {},d, bigraph {})
mixedGraph Bigraph := b -> mixedGraph(graph {},digraph {}, b)
mixedGraph (Digraph, Bigraph) := (d,b) -> mixedGraph(graph {},d,b)
mixedGraph (Bigraph, Digraph) := (b,d) -> mixedGraph(graph {},d,b)
mixedGraph (Graph, Digraph) := (g,d) -> mixedGraph(g,d,bigraph {})
mixedGraph (Digraph, Graph) := (d,g) -> mixedGraph(g,d,bigraph {})
mixedGraph (Graph, Bigraph) := (g,b) -> mixedGraph(g,digraph {},b)
mixedGraph (Bigraph, Graph) := (b,g) -> mixedGraph(g,digraph {},b)
mixedGraph (Digraph, Bigraph, Graph) := (d,b,g) -> mixedGraph(g,d,b)
mixedGraph (Bigraph, Graph, Digraph) := (b,g,d) -> mixedGraph(g,d,b)
mixedGraph (Graph, Bigraph, Digraph) := (g,b,d) -> mixedGraph(g,d,b)
mixedGraph (Bigraph, Digraph, Graph) := (b,d,g) -> mixedGraph(g,d,b)
mixedGraph (Digraph, Graph, Bigraph) := (d,g,b) -> mixedGraph(g,d,b)

net MixedGraph := g -> horizontalJoin flatten (
     net class g,
    "{",
    stack (horizontalJoin \ sort apply(pairs (g#graph),(k,v) -> (net k, " => ", net v))),
    "}"
    )

toString MixedGraph := g -> concatenate(
    "new ", toString class g#graph,
    if parent g#graph =!= Nothing then (" of ", toString parent g),
    " from {",
    if #g#graph > 0 then demark(", ", apply(pairs g#graph, (k,v) -> toString k | " => " | toString v)) else "",
    "}"
    )

graph MixedGraph := opts -> g -> g#graph     	     --used to transform the MixedGraph into a hashtable

undirectedGraph = method(TypicalValue =>Graph)
undirectedGraph MixedGraph := g -> g#graph#Graph

digraph MixedGraph := opts -> g -> g#graph#Digraph
bigraph MixedGraph := opts -> g -> g#graph#Bigraph
vertices MixedGraph := G -> toList sum(apply(keys(G#graph),i->set keys(graph (G#graph)#i)))
vertexSet MixedGraph := G -> vertices G

descendents (MixedGraph, Thing) := (G,v) -> descendents(digraph collateVertices G, v)
descendants (MixedGraph, Thing) := (G,v) -> descendants(digraph collateVertices G, v)
nondescendents (MixedGraph, Thing) := (G,v) -> nondescendents(digraph collateVertices G, v)
nondescendants (MixedGraph, Thing) := (G,v) -> nondescendants(digraph collateVertices G, v)
parents (MixedGraph, Thing) := (G,v) -> parents(digraph collateVertices G, v)
foreFathers (MixedGraph, Thing) := (G,v) -> foreFathers(digraph collateVertices G, v)
children (MixedGraph, Thing) := (G,v) -> children(digraph collateVertices G, v)
neighbors (MixedGraph, Thing) := (G,v) -> neighbors(undirectedGraph collateVertices G, v)
nonneighbors (MixedGraph, Thing) := (G,v) -> nonneighbors(undirectedGraph  collateVertices G, v)


collateVertices = method(TypicalValue =>MixedGraph)
collateVertices MixedGraph := g -> (
    v := vertices g;
    hh := new MutableHashTable;
    G := graph g;
    -- Graph
    x := graph G#Graph;
    scan(v,j->if x#?j then hh#j=x#j else hh#j={});
    gg := graph(new HashTable from hh);
    -- Digraph
    x = graph G#Digraph;
    scan(v,j->if x#?j then hh#j=x#j else hh#j={});
    dd := digraph(new HashTable from hh);
    -- Bigraph
    x = graph G#Bigraph;
    scan(v,j->if x#?j then hh#j=x#j else hh#j={});
    bb := bigraph(new HashTable from hh);
    mixedGraph(gg,dd,bb))

graphComponents = {}
graphFunctions ={graph,bigraph,digraph}

indexLabelGraph MixedGraph := MixedGraph => G -> (
    V := vertices G;
    h := hashTable apply(#V, i -> V_i => i);
    U := G#graph#Graph;
    B := G#graph#Bigraph;
    D := G#graph#Digraph;
    
    inputG:=new MutableHashTable;
    inputG#graphComponents={U,B,D};
    inputG#graphFunctions={graph,bigraph,digraph};
    inputG=new HashTable from inputG;
    
    G=mixedGraph toSequence(
      for i to #inputG#graphComponents-1 list (
      E := apply(toList \ edges inputG#graphComponents_i, e -> {h#(e_0), h#(e_1)});
      inputG#graphFunctions_i(flatten E, E,EntryMode => "edges")
      )
     )
    );


-- Makes a partition U\cup W of the vertices V of a loopless mixed graph (inputed as a MixedGraph) 
-- such that U contains all the vertices adjacent to undirected edges, 
-- W contains all the vertices adjacent to bidirected edges 
-- and there are no directed edges from W to U
-- and all vertices in U have lower value than those in W.
partitionLMG = method(TypicalValue =>Sequence)
partitionLMG MixedGraph := g -> (
   --check it's a loopless graph
   if isLoopless(g)==false then error ("The expected input is a loopless mixed graph.");
   --retrieve graph, bigraph and digraph
   G:= g#graph#Graph;
   B:= g#graph#Bigraph;
   D:= g#graph#Digraph;
   --check there are no directed cycles
   if isCyclic g  then error ("The expected input is a loopless mixed graph without directed cycles.");
   --naive partition (vertices only adjacent to directed edges are not considered) 
   U:=vertices G;
   W:=vertices B;
   --check there are no common vertices for undirected and bidirected edges
   if not (set U * set W===set {}) then 
   error("Vertices cannot be adjacent to bidirected and undirected edges simultaneously.");
   --check that vertices in U (undir) have lower value than vertices in W (bidir):
   if max U > min W then error ("Vertex ordering issues: vertices adjacent to undirected edges should have lower value than vertices adjacent to bidirected edges");
   --check there are no directed edges from set U to set W
   for e in edges D do (if (member(e_0,set W) and member(e_1,set U)) 
   then error("Directed edges cannot go from vertices adjacent to a bidirected edge to vertices adjacent to an undirected edge"));
   --check directed edges always go from lower value to higher value
   for e in edges D do (
       if e_0 > e_1 then error ("Vertex ordering issues: directed edges must go from low to high");
   );
   --check whether there are remaining vertices (only adjacent to directed edges)
   V:=set vertices g-set W-set U;
   if V===set{} then return (U,W);
   --place remaining vertices in either U or W depending on their value
   for v in toList V do (if v < max U then U=append(U,v) else W=append(W,v););
   sort U,sort W
)

--Check whether a graph is loopless in each type of edges
isLoopless = method(TypicalValue =>Boolean)
isLoopless MixedGraph := Boolean => g -> (
   --retrieve graph, digraph and bigraph
   G:= g#graph#Graph;
   B:= g#graph#Bigraph;
   D:= g#graph#Digraph;
   --check there are no loops and no repetitions
   isSimple(G) and isSimple(underlyingGraph D) and isSimple(B)
   )

isLoopless Graph := Boolean => g -> (
   isSimple(g)
   )

isLoopless Bigraph := Boolean => g -> (
   isSimple(g)
   )

isLoopless Digraph := Boolean => g -> (
   isSimple(underlyingGraph g)
   )

-- Internal function to check whether a MixedGraph contains multiple edges
hasMultipleEdges = g -> (
   --retrieve graph, digraph and bigraph
   G:= g#graph#Graph;
   B:= g#graph#Bigraph;
   D:= g#graph#Digraph;
   --retrieve underlying undirected edges 	
   edG:=edges G;
   edD:=edges underlyingGraph D;
   edB:=edges B;
   --build list of underlying edges with and without repetitions
   d:=join(edG,edD,edB);
   e:=unique(d);
   --check there are no repetitions
   not #d==#e
   )

-- Check whether a MixedGraph is simple
isSimple MixedGraph := Boolean => G -> (
    isLoopless G and not hasMultipleEdges G
    )


-- Check whether a MixedGraph is cyclic, i.e., does not contain any directed cycles
isCyclic MixedGraph := Boolean => g -> (
    G:=indexLabelGraph g;
    flag:= isCyclic digraph G;
    if flag then flag else(
    U:= graph(sort vertices G#graph#Graph,edges G#graph#Graph);
    B:= bigraph(sort vertices G#graph#Bigraph,edges G#graph#Bigraph);
    D:= digraph(vertices G,edges G#graph#Digraph);
    compU:=connectedComponents U;
    compB:=connectedComponents B;
    vertOnlyDir:=vertices D - set vertices U - set vertices B;
    allComp:=flatten  {connectedComponents U,connectedComponents B, pack(vertOnlyDir,1)};
    n:=# compU + # compB + #vertOnlyDir;
    adjMG:=mutableMatrix(ZZ,n,n);
    -- form the adjacency matrix of the graph of chain components 
    for i from 0 to  n - 1 do
	   (
	       for j from 0 to  n - 1 do
	       (if not submatrix(adjacencyMatrix D, toList(set(allComp_i)*set(vertices D)), toList(set(allComp_j)*set(vertices D)))==0 then adjMG_(i,j)=1 else adjMG_(i,j)=0
	       ));

    adjMG=matrix adjMG;
    isCyclic (digraph adjMG)))  

--******************************************--
-- DOCUMENTATION     	       	    	    -- 
--******************************************--

beginDocumentation()

doc ///
    Key
        StatGraphs
    Headline
        a package for graphs used in statistical models
    Description        
        Text
            This package contains the types of graphs that are used in algebraic 
      	    statistics: @TO Bigraph@ and  @TO MixedGraph@. 
	    
	    A bigraph is graph with bidirected edges and no multiple edges. 
	    A mixed graph is a graph with undirected, directed and bidirected edges. 
	    
	    This is an example of a bigraph on 4 vertices. It is created using the method @TO bigraph@.
	    
        Example	   
	    G = bigraph {{3,4},{1,2},{2,4}}
	
	Text
	    Next is an example of a mixed graph on 4 vertices with undirected, directed and bidirected edges. It is created using
	    the method @TO mixedGraph@. 
	       
        Example	   
	    G = mixedGraph(graph{{1,2}},digraph {{1,3},{2,3}},bigraph {{3,4}})
	    
	Text
	    One can extract key information about mixed graphs using:
	    @TO (undirectedGraph,MixedGraph)@,
	    @TO (bigraph,MixedGraph)@,
	    @TO (digraph,MixedGraph)@,
	    @TO (vertices,MixedGraph)@,
	    
	    or convert a mixed graph into a more convenient form using:
	    @TO (collateVertices,MixedGraph)@,
	    @TO (indexLabelGraph,MixedGraph)@. 
	    
	    There are several methods that allow to check the properties of mixed graphs:
	    @TO (isCyclic,MixedGraph)@, 
	    @TO (isLoopless,MixedGraph)@,
	    @TO (isSimple,MixedGraph)@.
	     
	    One can also study the properties of vertices using:
	    @TO (children, MixedGraph, Thing)@,     
    	    @TO (parents, MixedGraph, Thing)@,  
    	    @TO (descendants, MixedGraph, Thing)@,	    
    	    @TO (nondescendants, MixedGraph, Thing)@,
    	    @TO (forefathers, MixedGraph, Thing)@,
    	    @TO (neighbors, MixedGraph, Thing)@,
	    @TO (nonneighbors, MixedGraph, Thing)@.
	    
	    This package allows to construct loopless mixed graphs (LMG) as introduced in 
	    Kayvan Sadeghi and Steffen Lauritzen, {\em Markov properties for mixed graphs}, 
	    Bernoulli 20.2 (2014): 676-696 and to check whether a mixedGraph is an LMG using
	    @TO partitionLMG@.
	    	    	       	
    Caveat
       StatGraphs requires  @TO Graphs@ version 0.3.3 or later.
       
    SeeAlso
       Graphs
    	   
///

--------------------------------
-- Documentation
--------------------------------

--------------------------------------------
-- Documentation Bigraph
--------------------------------------------

doc ///
  Key 
     Bigraph

  Headline
     a graph with bidirected edges and no multiple edges
  Description
     Text  
         Bigraph is a simple graph that has  bidirected edges.
	 To create a bigraph, use @TO bigraph@. 
	
  SeeAlso
     bigraph

///

--------------------------------------------
-- Documentation bigraph
--------------------------------------------

doc ///
  Key
     bigraph
     (bigraph, HashTable)
     (bigraph, List)
     (bigraph, List, List)
     (bigraph, List, Matrix)
     (bigraph, Matrix)

    
  Headline
     create a bigraph 
  Usage
     G= bigraph(H) 
     G= bigraph(L) 
     G= bigraph(V,L)
     G= bigraph(V,A)
     G= bigraph(A)   
 
  Inputs
     H:HashTable
       hashtable of edges	 	 	 
     L:List
       list of edges
     V:List 
       list of vertices
     A:Matrix
       adjacency matrix   
    
  Outputs
     :Bigraph
    
  Description
    Text
        This is a constructor of a simple graph of class @TO Bigraph@.  One can use the same input types
	as in @TO graph@. 
    Example
        G = bigraph {{3,4},{1,2},{2,4}}

  SeeAlso
    Bigraph

///

--------------------------------------------
-- Optional arguments for bigraph
--------------------------------------------
doc ///
  Key 
    [bigraph,EntryMode]

  Headline
     optional input, accepts 'auto', 'edges', or 'neighbors' 
  Description
      Text
          The options for EntryMode are 'auto'(the default), 'neighbors' and  'edges'.
	  If 'edges' is selected, then the input should contain a list of lists and
	  the inner lists correspond to pairs of vertices incident to a given edge.
	  If 'neighbors' is selected, then the input should contain a list of lists and
	  in the inner lists, the 0th entry is a vertex and the 1st entry is the list of
	  its neighbors.
	  The default 'auto' option distinguished between the other two options automatically.
      
      Example    
	   graph ({{a,{b,c,d,e}}, {b,{d,e}}, {e,{a}}},EntryMode=>"neighbors")
	   graph ({{a,{b,c,d,e}}, {b,{d,e}}, {e,{a}}},EntryMode=>"auto")
	   graph ({{a,b}, {b,d}, {c,d},EntryMode=>"edges"})
	   graph ({{a,b}, {b,d}, {c,d},EntryMode=>"auto"})
  SeeAlso
     bigraph
     [graph,EntryMode]
     graph

///

doc ///
  Key 
    [bigraph,Singletons]

  Headline
     optional input, accepts the list of isolated vertices in a bigraph  
  Description
     Example
          bigraph({{1,2},{2,3},{3,4}}, Singletons => {5,6,7})
	
  SeeAlso
     bigraph
     [bigraph,Singletons]
     graph

///
--------------------------------------------
-- Documentation MixedGraph
--------------------------------------------

doc ///
  Key 
     MixedGraph

  Headline
     a graph that has undirected, directed and bidirected edges 
  Description
     Text  
         MixedGraph is a graph that has undirected, directed and bidirected edges.
	 To create a mixed graph, use @TO mixedGraph@. Each type of subgraph (undirected,
	 directed and bidirected) does not have multiple edges and can
     	 appear at most once.
	
  SeeAlso
     mixedGraph
     collateVertices

///

--------------------------------------------
-- Documentation mixedGraph
--------------------------------------------

doc ///
  Key
     mixedGraph
     (mixedGraph, Graph, Digraph, Bigraph)
     (mixedGraph, Graph,Bigraph,Digraph) 
     (mixedGraph, Digraph,Graph,Bigraph)
     (mixedGraph, Digraph,Bigraph,Graph)
     (mixedGraph, Bigraph,Graph,Digraph)
     (mixedGraph, Bigraph,Digraph,Graph) 
     (mixedGraph, Graph, Digraph)
     (mixedGraph, Digraph,Graph)
     (mixedGraph, Digraph, Bigraph)
     (mixedGraph, Bigraph,Digraph)
     (mixedGraph, Graph, Bigraph)
     (mixedGraph, Bigraph, Graph)
     (mixedGraph, Graph)
     (mixedGraph, Digraph)
     (mixedGraph, Bigraph)
    
  Headline
     create a mixed graph from a combination of undirected graph, digraph and bigraph 
  Usage
     G= mixedGraph(U, D, B) 
     G= mixedGraph G 
     G= mixedGraph D
     G= mixedGraph B
     G= mixedGraph(G, D)
     G= mixedGraph(G, B)
     G= mixedGraph(D, B)
 
  Inputs
     U:Graph
       component that contains all undirected edges of the graph	 	 	 
     D:Digraph
       component that contains all directed edges of the graph
     B:Bigraph
       component that contains all bidirected edges of the graph
    
  Outputs
     :MixedGraph
    
  Description
    Text
        This is a constructor of graphs of class @TO MixedGraph@ from a combination of subgraphs of classes @TO Graph@, 
	@TO Digraph@ and @TO Bigraph@.  One can also input any subset 
        and any permutation of the arguments. Each type of subgraph can appear at most once.
    
        Note that this constructor does not check the input satisfies the properties of loopless mixed graphs from 
        Sadeghi and Lauritzen, 2020 <@HREF"https://arxiv.org/pdf/1109.5909.pdf"@>.
      
    Example
        G = mixedGraph(graph{{1,2}},digraph {{1,3},{2,3}},bigraph {{3,4}})

  SeeAlso
    MixedGraph

///

--------------------------------------------
-- Operations on MixedGraph
--------------------------------------------

--------------------------------------------
-- Documentation bigraph(MixedGraph)
--------------------------------------------

doc ///
  Key
     (bigraph, MixedGraph)
  Headline
     extract the bigraph component of a mixed graph
  Usage
     B=bigraph G
 
  Inputs
     G:MixedGraph
    
  Outputs
     :Bigraph
    
  Description
    Text
        This method extracts the largest component of class @TO Bigraph@ of a mixed graph.
      
    Example
        G= mixedGraph(graph{{a,b},{b,c}},digraph {{a,d},{c,e},{f,g}},bigraph {{d,e}})
        bigraph G

  SeeAlso
    MixedGraph
    (digraph, MixedGraph) 
    (graph, MixedGraph)
    (undirectedGraph, MixedGraph) 

///

--------------------------------------------
-- Documentation digraph(MixedGraph)
--------------------------------------------

doc ///
  Key
     (digraph, MixedGraph)
  Headline
     extract the digraph component of a mixed graph
  Usage
     D=digraph G
 
  Inputs
     G:MixedGraph
    
  Outputs
     :Digraph
    
  Description
    Text
        This method extracts the largest component of class @TO Digraph@ of a mixed graph.
      
    Example
        G= mixedGraph(graph{{a,b},{b,c}},digraph {{a,d},{c,e},{f,g}},bigraph {{d,e}})
        digraph G

  SeeAlso
    MixedGraph
    (bigraph, MixedGraph) 
    (graph, MixedGraph)
    (undirectedGraph, MixedGraph) 

///

--------------------------------------------
-- Documentation undirectedGraph(MixedGraph)
--------------------------------------------

doc ///
  Key
     undirectedGraph
     (undirectedGraph, MixedGraph)
  Headline
     extract the undirected graph component of a mixed graph
  Usage
     U=undirectedGraph G
 
  Inputs
     G:MixedGraph
    
  Outputs
     :Graph
    
  Description
    Text
        This method extracts the largest component of class @TO Graph@ of a mixed graph.
      
    Example
        G= mixedGraph(graph{{a,b},{b,c}},digraph {{a,d},{c,e},{f,g}},bigraph {{d,e}})
        undirectedGraph G

  SeeAlso
    MixedGraph
    (bigraph, MixedGraph)
    (digraph, MixedGraph) 
    (graph, MixedGraph) 

///

--------------------------------------------
-- Documentation graph(MixedGraph)
--------------------------------------------

doc ///
  Key
     (graph, MixedGraph)
  Headline
     convert mixed graph to a hash table
  Usage
     U=graph G
 
  Inputs
     G:MixedGraph
    
  Outputs
     :HashTable
    
  Description
    Text
        This method creates a hash table whose key-value pairs correspond to the
	components of G.
      
    Example
        G= mixedGraph(graph{{a,b},{b,c}},digraph {{a,d},{c,e},{f,g}},bigraph {{d,e}})
        graph G
	keys (graph G)
	(graph G)#Bigraph === bigraph G
	

  SeeAlso
    MixedGraph
    (bigraph, MixedGraph)
    (digraph, MixedGraph) 
    (undirectedGraph, MixedGraph) 

///
--------------------------------------------
-- Documentation collateVertices 
--------------------------------------------

doc /// 
    Key
        collateVertices
        (collateVertices, MixedGraph) 
	
    Headline
        convert a mixed graph into a new mixed graph where each component subgraph has the same set of vertices
    Usage
        collateVertices(G)
    Inputs
        G:MixedGraph
    Outputs
         :MixedGraph 
    Description 
        Text
	    Let G=mixedGraph(U,D,B) and denote the vertices of U by V1, 
	    the vertices of D by V2 and the vertices of B by  V3.
	    Then the method collateVertices(G) outputs a mixedGraph with same 
	    edges as before but with V1 \cup V2 \cup V3 as the vertices of U,D 
	    and B.
	    
        Example
	   U = graph{{1,2},{2,3},{3,4},{1,4},{1,5}}
	   D = digraph{{2,1},{3,1},{7,8}}
	   B = bigraph{{1,5}}
	   G = mixedGraph(U,D,B)
	   collateVertices G
   ///
   
--------------------------------------------
-- Documentation isSimple(MixedGraph)  
--------------------------------------------

doc /// 
    Key 
        (isSimple, MixedGraph) 
    Headline
        check whether a mixed graph is simple
    Usage
        isSimple(G)
    Inputs
        G:MixedGraph
    Outputs
         :Boolean 
    Description 
        Text
	  This method checks whether a graph is simple: does not contain
	  loops or multiple edges.
	  Note that since @TO graph@, @TO digraph@ and @TO bigraph@ do not
	  allow multiple edges, a graph of class @TO MixedGraph@ can only have multiple edges
	  of different types.
	  
	  In the following example, there are no loops or multiple edges.

        Example
	   U = graph{{1,2},{2,3},{3,4}}
	   D = digraph{{2,5}}
	   B = bigraph{{5,6}}
	   G = mixedGraph(U,D,B)
	   isSimple G
	   
	Text
	   This example contains multiple edges on vertices 1 and 2.
	Example
	   U = graph{{1,2},{2,3},{3,4}}
	   D = digraph{{1,2},{2,5}}
	   B = bigraph{{5,6}}
	   G = mixedGraph(U,D,B)
	   isSimple G
	   
	Text    
	   This example contains a loop.
	Example
	   U = graph{{1,2},{2,3},{3,4}}
	   D = digraph{{2,5}}
	   B = bigraph{{5,6},{5,5}}
	   G = mixedGraph(U,D,B)
	   isSimple G
   ///
   
--------------------------------------------
-- Documentation indexLabelGraph
--------------------------------------------

doc ///
  Key
     (indexLabelGraph, MixedGraph)
  Headline
     relabel the vertices of a mixed graph according to their indices, indexed from 0
  Usage
     G=indexLabelGraph G
 
  Inputs
     G:MixedGraph
    
  Outputs
     :MixedGraph
    
  Description
    Text
        This method relabels the vertices of a graph of class @TO MixedGraph@  according to their indices. 
	The method indexes from 0 to the number of vertices minus one.
	This is an adaptation of the method @TO indexLabelGraph@.
      
    Example
        G= mixedGraph(graph{{a,b},{b,c}},digraph {{a,d},{c,e},{f,g}},bigraph {{d,e}})
	indexLabelGraph G

  SeeAlso
    MixedGraph
    indexLabelGraph

///

--------------------------------------------
-- Documentation isLoopless 
--------------------------------------------

doc /// 
    Key
        isLoopless 
        (isLoopless, MixedGraph) 
	(isLoopless, Graph) 
	(isLoopless, Bigraph) 
	(isLoopless, Digraph) 
    Headline
        check whether a graph contains a loop
    Usage
        isLoopless(G)
    Inputs
        G:
	 graph of class  @TO MixedGraph@,  @TO Graph@,  @TO Digraph@ or  @TO Bigraph@
    Outputs
         :Boolean 
    Description 
        Text
	  This method checks whether a graph contains a loop. 
	  
	  If the input is a  @TO Graph@ or a  @TO Bigraph@, then this is equivalent to 
	   @TO isSimple@. 
	  
	  If the input is a  @TO Digraph@, then this is equivalent to checking whether 
	  the  @TO underlyingGraph@ @TO isSimple@.
	  
	  If the input is a  @TO MixedGraph@, then this checks whether the undirected,
	  directed and bidirected subgraphs separately contain loops.
        Example
	   U = graph{{1,2},{2,3},{3,4}}
	   D = digraph{{2,5}}
	   B = bigraph{{5,6}}
	   G = mixedGraph(U,D,B)
	   isLoopless G
	   
	Example
	   U = graph{{1,1}}
	   isLoopless U   
   ///

--------------------------------------------
-- Documentation (isCyclic, MixedGraph)
--------------------------------------------

doc /// 
    Key
        (isCyclic, MixedGraph) 
	
    Headline
        check whether a mixed graph contains a directed cycle
    Usage
        isCyclic(G)
    Inputs
        G:MixedGraph
    Outputs
         :Boolean 
    Description 
        Text
	   This method checks whether a @TO MixedGraph@ is cyclic, i.e. contains
	   a directed cycle or a cycle on directed edges. 
	   
	   A directed cycle is a cycle in the @TO Digraph@ constructed from a
	   mixed graph G by identifying all connected components on bidirected and undirected edges.
	   Such a connected component contains either bidirected edges only or 
	   undirected edges only.
	   
	   In the following example, there are no directed cycles.	    
        Example
	   U = graph{{1,2},{2,3},{3,4},{1,4},{1,5}}
	   D = digraph{{2,1},{3,1},{7,8}}
	   B = bigraph{{1,5}}
	   G = mixedGraph(U,D,B)
	   isCyclic G
	   
	Text
	  In the next example, there are no cycles inside the digraph of the 
	  mixed graph, but there is a directed cycle after you identify the 
	  vertices {1,2} and {3,4}.
	  	   
        Example
	   U = graph{{1,2},{3,4}}
	   D = digraph{{1,3},{4,2}}
	   G = mixedGraph(U,D)
	   isCyclic G 
	
	Text
	  This is a similar example as before, but now the vertices {1,2} are
	  connected by an undirected edge and {3,4} by a bidirected edge.
	Example
	   U = graph{{1,2}}
	   B = bigraph{{3,4}}
	   D = digraph{{1,3},{4,2}}
	   G = mixedGraph(U,D,B)
	   isCyclic G 
	
	Text
	  In the following example, there is a cycle on the directed edges
	  that is inside a connected undirected component.
	Example
	  G = mixedGraph(graph{{1,2}},digraph {{1,2},{2,1}})
	  isCyclic G
   ///
   
--------------------------------------------
-- Documentation partitionLMG
--------------------------------------------

doc /// 
    Key
        partitionLMG 
        (partitionLMG, MixedGraph) 
	
    Headline
        partition the vertices of a loopless mixed graph into adjacent to undirected edges and adjacent to bidirected edges
    Usage
        partitionLMG(G)
    Inputs
        G:MixedGraph
    Outputs
         :Sequence
	  (U,W) where U are vertices adjacent to undirected edges and W are vertices adjacent to bidirected edges 
    Description 
        Text
	   This function makes a partition $U\cup W$ of the vertices V of a loopless mixed graph such that:
   	   
            - if $i-j$ in $G$ then $i,j\in U$,
    
            - if $i\leftarrow \rightarrow j$ in $G$ then $i,j\in W$ 
    
            - there is no directed edge $i\to j$ in $G$ such that $i\in W$ and $j\in U$.
    
           These conditions are equivalent to those introduced in Seth Sullivant, Kelli Talaska, and Jan Draisma, 
	   {\em Trek separation for Gaussian graphical models}, The Annals of Statistics 
	   38.3 (2010): 1665-1685.
	   
	   For technical purposes we assume, without loss of generality, that vertices in the LMG are ordered such that:
      
           1. all vertices in U come before vertices in W,
      
           2. if there is a directed edge from $i$ to $j$, then $i<j$. 

	   This method checks that the input contains no loops and 
	   it requires the graph to be directed acyclic, i.e., there should not be any
           directed cycles after the identification of the connected undirected and bidirected components. 
    
        Example
	   U = graph{{1,2},{2,3},{1,3}}
	   D = digraph{{1,4},{3,7}}
	   B = bigraph{{4,5},{5,6},{7,8}}
	   G = mixedGraph(U,D,B)
	   partitionLMG G
	   
	Text
	  The vertices that are adjacent only to directed edges are sorted depending on their order.
	  If  v  is such a vertex and v < max U, then v is added to U. Otherwise, it is added to W. 
	Example       	   
	   U = graph{{2,3},{3,4},{4,2}}
	   D = digraph{{1,2},{2,5},{4,9}}
	   B = bigraph{{5,6},{6,7},{8,9}}
	   G = mixedGraph(U,D,B)
	   partitionLMG G
	   	   
	Example       	   
	   U = graph{{1,2},{2,3},{1,3}}
	   D = digraph{{1,4},{3,7},{8,9}}
	   B = bigraph{{4,5},{5,6},{7,9}}
	   G = mixedGraph(U,D,B)
	   partitionLMG G

   ///  
--------------------------------------------
-- Printing of a MixedGraph
--------------------------------------------   
--------------------------------------------
-- Documentation (net,MixedGraph)
--------------------------------------------
doc ///
  Key
    (net,MixedGraph)
    
  Headline
    print a mixed graph as a  net 
  Usage
    net G
 
  Inputs
    G: MixedGraph
    
  Outputs
     :Net
    
  Description
    Text
        This methods defines how to manipulate a mixed graph to produce a  @TO Net@
    Example
        G = mixedGraph(graph{{3,1}},digraph {{1,2},{2,3}},bigraph {{3,4},{2,4}})
    	net G

  SeeAlso
    MixedGraph
    (toString, MixedGraph)
    Net

///  

--------------------------------------------
-- Documentation (toString, MixedGraph)
--------------------------------------------
doc ///
  Key
    (toString, MixedGraph)
    
  Headline
    print a mixed graph as a string 
  Usage
    toString G
 
  Inputs
    G: MixedGraph
    
  Outputs
     :String
    
  Description
    Text
        This methods defines how to manipulate a mixed graph to produce a  @TO String@
    Example
        G = mixedGraph(graph{{3,1}},digraph {{1,2},{2,3}},bigraph {{3,4},{2,4}})
    	toString G

  SeeAlso
    MixedGraph
    (net, MixedGraph)
    String

///   
--------------------------------------------
-- Operations on vertices of a  MixedGraph
--------------------------------------------  

--------------------------------------------
-- Documentation children
--------------------------------------------
doc ///
  Key
     (children,MixedGraph,Thing)
    
  Headline
    return the children of a vertex of a mixed graph
  Usage
    children (G,v)
 
  Inputs
    G: MixedGraph
    v: Thing
       a vertex of G
    
  Outputs
     :Set
    
  Description
    Text
        The children of v are the all the vertices u such that v,u is in the directed edge 
	set of the @TO MixedGraph@ G. So the children of a vertex v are exactly those 
	vertices of the largest digraph component of a  mixed graph that v points to.
    Example
        G = mixedGraph(graph{{3,1}},digraph {{1,2},{2,3}},bigraph {{3,4},{2,4}})
	children (G,1)
	children (G,2)
    	children (G,3)

  SeeAlso
    MixedGraph
    children
    parents
    (parents, MixedGraph, Thing)

///

--------------------------------------------
-- Documentation parents
--------------------------------------------
doc ///
  Key
     (parents,MixedGraph,Thing)
    
  Headline
    return the parents of a vertex of a mixed graph
  Usage
    parents (G,v)
 
  Inputs
    G: MixedGraph
    v: Thing
       a vertex of G
    
  Outputs
     :Set
    
  Description
    Text
        The parents of v are the all the vertices u such that u,v is in the directed edge 
	set of the @TO MixedGraph@ G. So the parents of a vertex v are exactly those 
	vertices of the largest digraph component of a mixed graph that point to v.
    Example
        G = mixedGraph(graph{{3,1}},digraph {{1,2},{2,3}},bigraph {{3,4},{2,4}})
	parents (G,1)
	parents (G,2)
    	parents (G,3)

  SeeAlso
    MixedGraph
    parents
    children
    (children, MixedGraph, Thing)

///
 
--------------------------------------------------------------------
-- Documentation descendents(MixedGraph) and descendants(MixedGraph)
--------------------------------------------------------------------
doc ///
  Key
     (descendants,MixedGraph,Thing)
    
  Headline
    return the descendants of a vertex of a mixed graph
  Usage
    descendents (G,v)
    descendants (G,v)
 
  Inputs
    G: MixedGraph
    v: Thing
       a vertex of G
    
  Outputs
     :Set
    
  Description
    Text
        The descendants of v are the all the vertices u such that u is reachable from v in the directed edge 
	set of the @TO MixedGraph@ G. Another way to more intuitively see what the descendants are is to 
	see the descendants of a vertex v can be found by first taking the children of v. Then if you take 
	the children of each of the children, and continue the process until the list stops growing, this 
	will form all the descendants of v.
	
	The output also includes the vertex v from the input in the set of the descendants.
    Example
        G = mixedGraph(graph{{3,1}},digraph {{1,2},{2,3}},bigraph {{3,4},{2,4}})
	descendants (G,1)
	descendants (G,2)
    	descendants (G,3)

  SeeAlso
    MixedGraph
    descendants
    foreFathers
    (forefathers, MixedGraph, Thing)
    nondescendants
    (nondescendants, MixedGraph, Thing)

/// 

--------------------------------------------------------------------
-- Documentation nondescendents(MixedGraph) and nondescendants(MixedGraph)
--------------------------------------------------------------------
doc ///
  Key
     (nondescendants,MixedGraph,Thing)
    
  Headline
    return the nondescendents of a vertex of a mixed graph
  Usage
    nondescendents (G,v)
    nondescendants (G,v)
 
  Inputs
    G: MixedGraph
    v: Thing
       a vertex of G
    
  Outputs
     :Set
    
  Description
    Text
        The nondescendant  of v are the all the vertices u such that u is not reachable
	from v in the directed edge set of the @TO MixedGraph@ G. 
	
    Example
        G = mixedGraph(graph{{3,1}},digraph {{1,2},{2,3}},bigraph {{3,4},{2,4}})
	nondescendants (G,1)
	nondescendants (G,2)
    	nondescendants (G,3)

  SeeAlso
    MixedGraph
    nondescendants
    descendants
    (descendants, MixedGraph, Thing)

/// 

--------------------------------------------------------------------
-- Documentation foreFathers (MixedGraph)
--------------------------------------------------------------------
doc ///
  Key
     (foreFathers,MixedGraph,Thing)
    
  Headline
    return the forefathers of a vertex of a mixed graph 
  Usage
    foreFathers (G,v)
    foreFathers (G,v)
 
  Inputs
    G: MixedGraph
    v: Thing
       a vertex of G
    
  Outputs
     :Set
    
  Description
    Text
        The forefathers of v are the all the vertices u such that v is reachable from u in the directed edge 
	set of the @TO MixedGraph@ G. 
	
	The output also includes the vertex v from the input in the set of the forefathers.
    Example
        G = mixedGraph(graph{{3,1}},digraph {{1,2},{2,3}},bigraph {{3,4},{2,4}})
	foreFathers (G,1)
	foreFathers (G,2)
    	foreFathers (G,3)

  SeeAlso
    MixedGraph
    foreFathers 
    descendants
    (descendants, MixedGraph, Thing)

/// 

--------------------------------------------------------------------
-- Documentation neighbors (MixedGraph)
--------------------------------------------------------------------
doc ///
  Key
     (neighbors,MixedGraph,Thing)
    
  Headline
    return the neighbors of a vertex of a mixed graph
  Usage
    neighbors (G,v)
    neighbors (G,v)
 
  Inputs
    G: MixedGraph
    v: Thing
       a vertex of G
    
  Outputs
     :Set
    
  Description
    Text
        The neighbors of v are the all the vertices u such that u,v is an undirected
	edge in G. 
	
    Example
        G = mixedGraph(graph{{3,1}},digraph {{1,2},{2,3}},bigraph {{3,4},{2,4}})
        neighbors (G,1)
	neighbors (G,2)

  SeeAlso
    MixedGraph
    neighbors 
    nonneighbors
    (nonneighbors, MixedGraph, Thing)

/// 

--------------------------------------------------------------------
-- Documentation nonneighbors (MixedGraph)
--------------------------------------------------------------------
doc ///
  Key
     (nonneighbors,MixedGraph,Thing)
    
  Headline
    return the neighbors of a vertex of a mixed graph
  Usage
    nonneighbors (G,v)
    nonneighbors (G,v)
 
  Inputs
    G: MixedGraph
    v: Thing
       a vertex of G
    
  Outputs
     :Set
    
  Description
    Text
        The nonneighbors of v are the all the vertices u such that u,v is not an undirected
	edge in G. 
	
    Example
        G = mixedGraph(graph{{3,1}},digraph {{1,2},{2,3}},bigraph {{3,4},{2,4}})
        nonneighbors (G,1)
	nonneighbors (G,2)

  SeeAlso
    MixedGraph
    nonneighbors
    neighbors
    (neighbors, MixedGraph, Thing) 

/// 


--------------------------------------------
-- Documentation vertices and vertexSet
--------------------------------------------

doc ///
  Key
     (vertices, MixedGraph)
     (vertexSet, MixedGraph)
    
  Headline
     create a union of all vertices of a mixed graph
  Usage
     V=vertices G
     V=vertexSet G
 
  Inputs
     G:MixedGraph
    
  Outputs
     :List
    
  Description
    Text
        This function creates a union of all vertices of a graph of class @TO MixedGraph@.
	This is an adaptation of vertices and vertexSet from @TO Graphs@.
      
    Example
        G = mixedGraph(graph{{3,1}},digraph {{1,2},{2,3}},bigraph {{3,4}})
	vertices G
	vertexSet G

  SeeAlso
    MixedGraph

///
--******************************************--
-- TESTS     	       	    	      	    --
--******************************************--

--------------------------------------------
--  Tests for Bigraph and bigraph
--------------------------------------------

TEST /// 
G = bigraph {{3,4},{1,2},{2,4}}
assert(instance (G, Bigraph))
///

--------------------------------------------
-- Tests for MixedGraph and mixedGraph
--------------------------------------------

TEST /// 
G = mixedGraph(graph{{1,2}},digraph {{1,3},{2,3}},bigraph {{3,4}})
assert(instance (G, MixedGraph))
///

--------------------------------------------
-- Tests for MixedGraph
--------------------------------------------

--------------------------------------------
-- Tests for bigraph(MixedGraph)
--------------------------------------------

TEST ///
        G= mixedGraph(graph{{a,b},{b,c}},digraph {{a,d},{c,e},{f,g}},bigraph {{d,e}})
        assert(bigraph G=== G#graph#Bigraph)
///

--------------------------------------------
--  Tests for digraph(MixedGraph)
--------------------------------------------

TEST ///
        G= mixedGraph(graph{{a,b},{b,c}},digraph {{a,d},{c,e},{f,g}},bigraph {{d,e}})
	assert(digraph G=== G#graph#Digraph)
///

--------------------------------------------
--  Tests for undirectedGraph(MixedGraph)
--------------------------------------------

TEST ///
        G= mixedGraph(graph{{a,b},{b,c}},digraph {{a,d},{c,e},{f,g}},bigraph {{d,e}})
	assert(undirectedGraph G=== G#graph#Graph)
///

--------------------------------------------
--  Tests for graph(MixedGraph)
--------------------------------------------

TEST ///

        G= mixedGraph(graph{{a,b},{b,c}},digraph {{a,d},{c,e},{f,g}},bigraph {{d,e}})
	assert(graph G === new HashTable from {Bigraph => bigraph ({d, e}, {{d, e}}), Graph =>
      graph ({a, b, c}, {{a, b}, {c, b}}), Digraph => digraph ({a, d, c, e, f, g}, {{a, d}, {c, e}, {f, g}})})
///

--------------------------------------------
-- Tests for collateVertices 
--------------------------------------------

TEST /// 
	   U = graph{{1,2},{2,3},{3,4},{1,4},{1,5}}
	   D = digraph{{2,1},{3,1},{7,8}}
	   B = bigraph{{1,5}}
	   G1 = mixedGraph(U,D,B)
	   G2 = collateVertices G1
	   assert(vertices bigraph G2 === vertices G1)
///


TEST /// 
	   U = graph{{1,2},{2,3},{3,4},{1,4},{1,5}}
	   D = digraph{{2,1},{3,1},{7,8}}
	   B = bigraph{{1,5}}
	   G1 = mixedGraph(U,D,B)
	   G2 = collateVertices G1
	   assert(edges bigraph G2 === edges B)
///

   
--------------------------------------------
-- Tests for  isSimple(MixedGraph)  
--------------------------------------------

TEST /// 
   
	   U = graph{{1,2},{2,3},{3,4}}
	   D = digraph{{2,5}}
	   B = bigraph{{5,6}}
	   G = mixedGraph(U,D,B)
	   assert (isSimple G)
 ///	  
 
TEST ///       
	   U = graph{{1,2},{2,3},{3,4}}
	   D = digraph{{1,2},{2,5}}
	   B = bigraph{{5,6}}
	   G = mixedGraph(U,D,B)
	   assert (not isSimple G) 
   ///

--------------------------------------------
-- Tests for indexLabelGraph
--------------------------------------------

TEST ///
        G1 = mixedGraph(graph{{a,b},{b,c}},digraph {{a,d},{c,e},{f,g}},bigraph {{d,e}})
	G2 = indexLabelGraph G1
	assert(graph G2 ===  new HashTable from {Bigraph => bigraph ({1, 2}, {{1, 2}}), Graph =>
     		graph ({5, 6, 0}, {{5, 6}, {0, 6}}), Digraph => digraph ({5, 1, 0, 2,
     			3, 4}, {{5, 1}, {0, 2}, {3, 4}})})
///

--------------------------------------------
-- Tests for  isLoopless 
--------------------------------------------

TEST /// 
	   U = graph{{1,2},{2,3},{3,4}}
	   D = digraph{{2,5}}
	   B = bigraph{{5,6}}
	   G = mixedGraph(U,D,B)
	   assert(isLoopless G)  
   ///
   

TEST /// 
	   U = graph{{1,1}}
	   assert (not isLoopless U)   
   ///   

--------------------------------------------
-- Tests for isCyclic(Mixed Graph) 
--------------------------------------------

TEST /// 
	   U = graph{{1,2},{2,3},{3,4},{1,4},{1,5}}
	   D = digraph{{2,1},{3,1},{7,8}}
	   B = bigraph{{1,5}}
	   G = mixedGraph(U,D,B)
	   assert(not isCyclic G)    
///

TEST /// 
	   U = graph{{1,2},{3,4}}
	   D = digraph{{1,3},{4,2}}
	   G = mixedGraph(U,D)
	   assert(isCyclic G)     
///

TEST /// 
	   U = graph{{1,2}}
	   B = bigraph{{3,4}}
	   D = digraph{{1,3},{4,2}}
	   G = mixedGraph(U,D,B)
	   assert(isCyclic G)      
///

TEST /// 
	   U = graph{{1,2}}
	   D = digraph{{1,2},{2,1}}
	   G = mixedGraph(U,D)
	   assert(isCyclic G)      
///
--------------------------------------------
-- Tests for partititionLMG 
--------------------------------------------

TEST /// 

	   U = graph{{1,2},{2,3},{3,4},{1,4},{1,5}}
	   D = digraph{{2,1},{3,1},{7,8}}
	   B = bigraph{{1,5}}
	   G = mixedGraph(U,D,B)
	   assert(try partitionLMG G then false else true)
  ///
  
TEST ///
	   U = graph{{1,2},{3,4}}
	   D = digraph{{1,3},{4,2}}
	   G = mixedGraph(U,D)
	   assert(try partitionLMG G then false else true)
  ///
  
TEST ///
	   U = graph{{1,2}}
	   B = bigraph{{3,4}}
	   D = digraph{{1,3},{4,2}}
	   G = mixedGraph(U,D,B)
	   assert(try partitionLMG G then false else true)    
   ///

TEST ///
    	   U = graph{{1,2},{2,3},{1,3}}
	   D = digraph{{4,1},{3,7}}
	   B = bigraph{{4,5},{5,6},{7,8}}
	   G = mixedGraph(U,D,B)
	   assert(try partitionLMG G then false else true)   
   ///
  	 
TEST ///
    	   U = graph{{1,2},{2,3},{1,3}}
	   D = digraph{{1,4},{3,7}}
	   B = bigraph{{4,5},{5,6},{7,8}}
	   G = mixedGraph(U,D,B)
	   assert(partitionLMG G === ({1, 2, 3}, {4, 5, 6, 7, 8}))
   ///     
  
TEST ///
    	   U = graph{{1,2},{2,3},{1,3}}
	   D = digraph{{1,4},{3,7},{8,9}}
	   B = bigraph{{4,5},{5,6},{7,9}}
	   G = mixedGraph(U,D,B)
	   assert(partitionLMG G === ({1, 2, 3}, {4, 5, 6, 7, 8, 9}))
   ///  
   
TEST ///      	   
	   U = graph{{2,3},{3,4},{4,2}}
	   D = digraph{{1,2},{2,5},{4,9}}
	   B = bigraph{{5,6},{6,7},{8,9}}
	   G = mixedGraph(U,D,B)
	   assert(partitionLMG G ===  ({1, 2, 3, 4}, {5, 6, 7, 8, 9}))
   ///   
--------------------------------------------
-- Operations on vertices of a  MixedGraph
--------------------------------------------  

--------------------------------------------
-- Tests for children
--------------------------------------------

TEST ///

        G = mixedGraph(graph{{3,1}},digraph {{1,2},{2,3}},bigraph {{3,4},{2,4}})
	assert(children (G,1)===set{2})
	assert(children (G,2)===set{3})
    	assert(children (G,3)===set{})
///

--------------------------------------------
-- Tests for parents
--------------------------------------------

TEST ///
        G = mixedGraph(graph{{3,1}},digraph {{1,2},{2,3}},bigraph {{3,4},{2,4}})
	assert(parents (G,1)===set{})
	assert(parents (G,2)===set{1})
    	assert(parents (G,3)===set{2})
///	
--------------------------------------------
-- Tests for descendants
--------------------------------------------

TEST ///
        G = mixedGraph(graph{{3,1}},digraph {{1,2},{2,3}},bigraph {{3,4},{2,4}})
	assert(descendants (G,1)===set{1,2,3})
	assert(descendants (G,2)===set{2,3})
    	assert(descendants (G,3)===set{3})	
///

--------------------------------------------
-- Tests for nondescendants
--------------------------------------------

TEST ///
        G = mixedGraph(graph{{3,1}},digraph {{1,2},{2,3}},bigraph {{3,4},{2,4}})
	assert(nondescendents (G,1)===set{4})
	assert(nondescendents (G,2)===set{1,4})
    	assert(nondescendents (G,3)===set{1,2,4})	
///

--------------------------------------------
-- Tests for foreFathers
--------------------------------------------

TEST ///
        G = mixedGraph(graph{{3,1}},digraph {{1,2},{2,3}},bigraph {{3,4},{2,4}})
	assert(foreFathers (G,1)===set{1})
	assert(foreFathers (G,2)===set{1,2})
    	assert(foreFathers (G,3)===set{1,2,3})	
///

--------------------------------------------
-- Tests for neighbors
--------------------------------------------

TEST ///
        G = mixedGraph(graph{{3,1}},digraph {{1,2},{2,3}},bigraph {{3,4},{2,4}})
	assert(neighbors (G,1)===set{3})
	assert(neighbors (G,2)===set{})	
///

--------------------------------------------
-- Tests for nonneighbors
--------------------------------------------

TEST ///
        G = mixedGraph(graph{{3,1}},digraph {{1,2},{2,3}},bigraph {{3,4},{2,4}})
	assert(nonneighbors (G,1)===set{2,4})
	assert(nonneighbors (G,2)===set{1,3,4})	
///

--------------------------------------------
-- Tests for  vertices and vertexSet
--------------------------------------------

TEST ///
        G = mixedGraph(graph{{3,1}},digraph {{1,2},{2,3}},bigraph {{3,4}})
	assert(vertices G=== {1,2,3,4})
	assert(vertexSet G=== {1,2,3,4})
///

--------------------------------------
--------------------------------------
end--
--------------------------------------
--------------------------------------










