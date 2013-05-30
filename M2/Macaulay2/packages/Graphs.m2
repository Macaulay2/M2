-- -*- coding: utf-8 -*-

   {*
   Copyright 2010 Amelia Taylor and Augustine O'Keefe.

   You may redistribute this file under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 2 of
   the License, or any later version.
   *}

newPackage("Graphs",
     Authors => {
	  {Name => "Amelia Taylor", Email => "originalbrickhouse@gmail.com"},
	  {Name => "Augustine O'Keefe", Email => "aokeefe@tulane.edu"}
	  },
     ---- Also Doug Torrance.  --- clearly a current author.  Current role of Amelia and Tina?
     ---- Shaowei Lin and Alex Diaz contributed mixedGraph
     DebuggingMode => false,
     Headline => "Data types, visualization, and basic functions for graphs",
     Version => "0.1"
     )

export {Graph,
     Digraph,
     Bigraph,
     MixedGraph,
     LabeledGraph,
     graph,
     digraph,
     bigraph,
     mixedGraph,
     labeledGraph,
     Singletons,
     vertices,
     edges,
     descendents,
     nondescendents,
     parents,
     children,
     neighbors,
     nonneighbors,
     foreFathers,     
     displayGraph,
     showTikZ,
     simpleGraph,      
     removeNodes, 
     inducedSubgraph,
     completeGraph,
     cycleGraph,
     writeDotFile,
     SortedDigraph,
     topSort,
     DFS,
     isCyclic,
     adjacencyMatrix,
     degreeMatrix,
     laplacianMatrix,
     incidenceMatrix,
     reachable,
     floydWarshall,
     collateVertices
     }
exportMutable {dotBinary,jpgViewer}

graphData = "graphData"
labels = "labels"
newDigraph = "newDigraph"

------------------------------------------------
-- Set graph types and constructor functions. -- 
------------------------------------------------

-- Give a graph as a hash table i => children for DAG and neighbors 
--                                   for undirected graphs. 

Digraph = new Type of HashTable
     -- a directed graph is a hash table in the form:
     -- { A => set {B,C,...}, ...}, where there are edges A->B, A->C, ...
     -- and A,B,C are symbols or integers. 

Graph = new Type of Digraph   
     -- an undirected graph is a hash table in the form:
     -- { A => set {B,C, ...}, where the set consists of 
     -- the neighbors of A. OR it is the neighbors for that 
     -- edge that are not a key earlier in the hash table. This 
     -- version removes the redunancy of each edge appearing twice. 
     -- simpleGraph is an internal conversion function. 

Bigraph = new Type of Digraph
     -- a bigraph is a hash table in the form { A => set {B,C, ...}, 
     -- where the set consists of the neighbors of A. This in only
     -- used in GraphicalModels and MixedGraph.  

MixedGraph = new Type of HashTable
     -- a mixed graph is a HashTable of Digraphs whose keys (vertex sets) are the same.
     
LabeledGraph = new Type of HashTable
   

digraph = method()
digraph HashTable := (g) -> (
     -- Input:  A hash table with keys the names of the nodes of 
     --         and the values the children of that node.      
     -- Output: A hash table of type Digraph.
     --         If a value of the hash table g is a List, Sequence or Array, it is converted into a set.
     --         If a value x of the hash table g is not a Set or VisibleList, it is converted into a set {x}.
     C := new MutableHashTable;
     C#cache = new CacheTable from {};
     if g === (new HashTable) then (
	  C#graph = g;
	  new Digraph from C) 
     else (
	  -- convert values of input hash table to sets
     	  G := applyValues(g, x->if instance(x,VisibleList) then set x else if instance(x,Set) then x else set {x});
	  -- find vertices which were referenced as values in the hash tables but do not show up as keys
     	  nullVertices := toList (sum(values G) - keys G);
     	  C#graph = merge(G,hashTable apply(nullVertices,i->{i,set {}}),plus);
	  new Digraph from C)
     )

digraph List := (g) -> (
     -- Input:  A list of pairs where the first element of the pair is the 
     --         name of a node and the second is the list of 
     --         children for that node. If a node has no children,
     --         then the second element of the pair should be empty. 
     -- Output:  A hashtable with keys the names of the nodes 
     --          with values the children.
     if g === {} then (
	  C := new MutableHashTable;
     	  C#cache = new CacheTable from {};
	  C#graph = new HashTable;
	  new Digraph from C) 
     else (
	  -- convert second element of input pairs to sets
     	  G := apply(g, x->{x#0,if instance(x#1,VisibleList) then set x#1 else if instance(x#1,Set) then x#1 else set {x#1}});
	  -- create an empty hashtable and add pairs to the hashtable
	  -- we did this instead of directly creating a hashtable from the pairs
	  -- because several pairs could have the same first element
     	  H := new MutableHashTable from apply(G,x->{x#0,set {}});     
     	  scan(G, x -> H#(x#0) = H#(x#0) + x#1);
	  -- create digraph from the hash table, to add 'null vertices' to the digraph (see digraph constructor for hash tables)
     	  digraph (new HashTable from H)
	  )
     )
     
graph = method(Options => {Singletons => null})
graph HashTable := opts -> (g) -> (
     -- Input:  A hash table with keys the names of the nodes of 
     --         the graph and the values the neighbors of that node. 
     --         If the set of Singletons clashes with data in the hash table, the hash table data takes precedence.
     -- Output: A hash table of type Graph. 
     G := digraph (if opts.Singletons === null then g else merge(g,hashTable apply(toList(set(opts.Singletons)-keys g),i->{i,set {}}),plus));
     if G === digraph({}) then new Graph from G else (
	  C := new MutableHashTable;
     	  C#cache = G#cache;
     	  C#graph = new MutableHashTable from G#graph;
          -- make sure that for every edge A-B, B appears in the value of A and vice versa.
	  scan(keys G#graph, i->scan(toList G#graph#i, j-> C#graph#j=C#graph#j+set{i}));
	  -- convert MutableHashTable to HashTable
     	  C#graph = new HashTable from C#graph;
     	  new Graph from C)
     )

graph List := opts -> (g) -> (
     -- Input:  A list of lists with two elements which describe the 
     --         edges of the graph. 
     -- Output:  A hash table with keys the names of the nodes and the 
     --          values are the neighbors corresponding to that node. 
     ---- Note to Selves --- this code should also nicely build
     ---- hypergraphs as hash tables with again, nodes as keys and
     ---- neighbors as values.
     G := digraph (g|if opts.Singletons === null then {} else apply(opts.Singletons,i->{i,{}}));
     if G === digraph({}) then new Graph from G else (
	  C := new MutableHashTable;
     	  C#cache = G#cache;
     	  C#graph = new MutableHashTable from G#graph;
          -- make sure that for every edge A-B, B appears in the value of A and vice versa.
     	  scan(keys G#graph, i->scan(toList G#graph#i, j-> C#graph#j=C#graph#j+set{i}));
	  -- convert MutableHashTable to HashTable
     	  C#graph = new HashTable from C#graph;
     	  new Graph from C)
     )

bigraph = method(Options => {Singletons => null})
bigraph HashTable := opts -> (g) -> (
     -- Input:  A hash table with keys the names of the nodes of 
     --         the graph and the values the neighbors of that node. 
     -- Output: A hash table of type Graph. 
     H := graph g;
     new Bigraph from H)
     
bigraph List := opts -> (g) -> (
     -- Input:  A list of lists with two elements which describe the 
     --         edges of the graph. 
     -- Output:  A hash table with keys the names of the nodes and the 
     --          values are the neighbors corresponding to that node. 
     -- Caveat: The final object looks just like a Graph.  Thus the
     -- purpose of this code and the type Bigraph is to have clear
     -- constructions and interpretations in GraphicalModels. 
     H := graph g;
     new Bigraph from H)

mixedGraph = method()
mixedGraph (Graph, Digraph, Bigraph) := (g,d,b) -> (
    -- Input: A hashtable of digraphs.
    -- Output: A hashtable of digraphs with the same vertex set,
    --         which is the union of the vertex sets of the input
    --         digraphs.
    C := new MutableHashTable;
    C#cache = new CacheTable from {};
    h := new MutableHashTable;
    h#Graph = g;
    h#Digraph = d;
    h#Bigraph = b;
    C#graph = new HashTable from h;
    new MixedGraph from C)

mixedGraph (Digraph, Bigraph) := (d,b) -> (
    -- Input: A hashtable of digraphs.
    -- Output: A hashtable of digraphs with the same vertex set,
    --         which is the union of the vertex sets of the input
    --         digraphs.
    mixedGraph(graph {},d,b))

mixedGraph (Graph, Digraph) := (g,d) -> (
    -- Input: A hashtable of digraphs.
    -- Output: A hashtable of digraphs with the same vertex set,
    --         which is the union of the vertex sets of the input
    --         digraphs.
    mixedGraph(g,d,bigraph {}))

mixedGraph (Digraph) := (d) -> (
    -- Input: A hashtable of digraphs.
    -- Output: A hashtable of digraphs with the same vertex set,
    --         which is the union of the vertex sets of the input
    --         digraphs.
    mixedGraph(graph {},d, bigraph {}))

labeledGraph = method()
labeledGraph (Digraph,List) := (g,L) -> (
     -- Input:  A graph and a list of lists with two elements one of
     --         which is a list giving an edge and the other is the labels. 
     --         For directed graphs, the list of labels are of the form {{A,B},edgeName} for the directed edge A->B.
     --         For undirected graphs, the labels could be {{A,B},edgeName} or {{B,A},edgeName} for the edges A-B.
     -- Output:  A hash table of two hash tables, the first of which is the
     --      	 base graph and the second of which has for keys edges of the
     --      	 graph whose values are the name of the edge.
     --          For directed graphs, the keys are {A,B} for the edge A->B.
     --          For undirected graphs, the keys are {A,B} for the edge A-B if {A,B} is in the list returned by the function 'edges'.
     --          To access the graph and labels, call 'L = graph G' where G is the LabeledGraph.
     --          Then, L#graphData is the hash table encoding the base graph, while L#labels is the hash table of labels.
     ---- Note to Selves --- this code should also nicely build
     ---- hypergraphs as hash tables with again, nodes as keys and
     ---- neighbors as values. 
     C := new MutableHashTable;
     C#cache = new CacheTable from {};
     lg := new MutableHashTable;
     lg#graphData = g;
     label := new MutableHashTable;
     if instance(g,Graph) then (
       -- want to make sure that the label is attached to an edge {A,B} that shows up 
       -- in the list of edges returned by the function 'edges', and not to {B,A}
       -- one way to avoid this complication is to have 'edges' return a list of sets {A,B}
       -- rather than a list of lists {A,B}.
       sg := simpleGraph g;
       scan(L, i -> (
  	 if (sg#graph#(i#0#0))#?(i#0#1) then
	   label#(i#0) = i#1
	 else if (sg#graph#(i#0#1))#?(i#0#0) then
	   label#({i#0#1,i#0#0}) = i#1
	 else
	   error (toString(i#0)|" is not an edge of the graph");
       ));
     ) else (
       scan(L, i -> (
	 if (g#graph#(i#0#0))#?(i#0#1) then
	   label#(i#0) = i#1
	 else
	   error (toString(i#0)|" is not an edge of the graph");
       ));
     );
     lg#labels = new HashTable from label;
     C#graph = lg;
     new LabeledGraph from C)

----------------------------------------------------------------------
-- Set the display and string functions for the various graph types --
-- e.g. net, toString, toExternalString and other functions to      --
-- disguise the internal structure.   
----------------------------------------------------------------------

net Digraph := g -> (
     horizontalJoin flatten ( 
     	  net class g,
	  "{", 
	  -- the first line prints the parts vertically, second: horizontally
 	  stack (horizontalJoin \ sort apply(pairs g#graph, (k,v) -> (net k, " => ", net v))),
	  "}" 
     	  ))

net MixedGraph := g -> (
     horizontalJoin flatten ( 
     	  net class g,
	  "{", 
	  -- the first line prints the parts vertically, second: horizontally
 	  stack (horizontalJoin \ sort apply(pairs (g#graph),(k,v) -> (net k, " => ", net v))),
	  "}" 
     	  ))

net Bigraph := g -> (
     horizontalJoin flatten ( 
     	  net class g,
	  "{", 
	  -- the first line prints the parts vertically, second: horizontally
 	  stack (horizontalJoin \ sort apply(pairs (g#graph),(k,v) -> (net k, " => ", net v))),
	  "}" 
     	  ))

net LabeledGraph := g -> (
     horizontalJoin flatten ( 
     	  net class g,
	  "{", 
	  -- the first line prints the parts vertically, second: horizontally
 	  stack (horizontalJoin \ sort apply(pairs (g#graph),(k,v) -> (net k, " => ", net v))),
	  "}" 
     	  ))

-- expression Graph := (I) -> new FunctionApplication from { graph,
-- unsequence apply(toSequence edges I, expression) }

-- net Ideal := (I) -> net expression I
-- toString Ideal := toExternalString Ideal := (I) -> toString expression I

toString Digraph := g -> (
     concatenate(
	  "new ", toString class g#graph,
	  if parent g#graph =!= Nothing then (" of ", toString parent g),
	  " from {",
	  if # g#graph > 0
	  then 
	  demark(", ", apply(pairs g#graph, (k,v) -> toString k | " => " | toString v))
	  else "",
	  "}"))     

toString Bigraph := g -> (
     concatenate(
	  "new ", toString class g#graph,
	  if parent g#graph =!= Nothing then (" of ", toString parent g),
	  " from {",
	  if # g#graph > 0
	  then 
	  demark(", ", apply(pairs g#graph, (k,v) -> toString k | " => " | toString v))
	  else "",
	  "}"))     

toString MixedGraph := g -> (
     concatenate(
	  "new ", toString class g#graph,
	  if parent g#graph =!= Nothing then (" of ", toString parent g),
	  " from {",
	  if # g#graph > 0
	  then 
	  demark(", ", apply(pairs g#graph, (k,v) -> toString k | " => " | toString v))
	  else "",
	  "}"))     

toString LabeledGraph := g -> (
     concatenate(
	  "new ", toString class g#graph,
	  if parent g#graph =!= Nothing then (" of ", toString parent g),
	  " from {",
	  if # g#graph > 0
	  then 
	  demark(", ", apply(pairs g#graph, (k,v) -> toString k | " => " | toString v))
	  else "",
	  "}"))     

graph Digraph := opts -> g -> g#graph
graph Bigraph := opts -> g -> g#graph
graph MixedGraph := opts -> g -> g#graph
graph LabeledGraph := opts -> g -> g#graph

digraph MixedGraph := (g) -> g#graph#Digraph    
bigraph MixedGraph := (g) -> g#graph#Bigraph
---graph MixedGraph := opts -> g -> g#graph#Graph
--- this is the obvious function here, but until we can write the
--- functions above as hashTable rather than graph, we will have a
--- conflict, so I am commenting it out for now. 

-----------------------------
-- Graph Display Functions --
-----------------------------

-- dotBinary = "/sw/bin/dot"
dotBinary = "dot"
-- jpgViewer = "/usr/bin/open"
jpgViewer = "open"

simpleGraph = method()
simpleGraph(Digraph) := H -> (
     -- Input: A Graph.
     -- Output: A new Graph in which the keys are still the nodes but
     --         the values are the lists of neighbors so that if an
     --         edge has already appeared before, it will not appear
     --         again.
     --if (H#cache)#?simpleGraph then H#cache#simpleGraph else 
     if not (instance(H,Graph) or instance(H,Bigraph)) then error "simpleGraph only works for Graphs and Bigraphs";
     H = graph H;
     pairH := new MutableList from pairs H;
     for k from 1 to #pairH-1 do (
	  testVertices := set for i to k-1 list pairH#i#0;
      	  pairH#k = (pairH#k#0, pairH#k#1-testVertices)
	  );
     digraph hashTable pairH)

writeDotFile = method()
writeDotFile(String, Graph) := (filename, G) -> (
     -- Input: The desired file name for the DOT file created and a graph.
     -- Output: The code for the inputted graph to be constructed in Graphviz 
     --         with the specified file name.
     fil := openOut filename;
     fil << "graph G {" << endl;
     V := vertices G;
     H := hashTable apply(#V,i->V_i=>i);
     scan(V,v-> fil << "\t"|toString H#v|" [label=\""|toString v|"\"];" << endl);
     E := edges G;
     scan(E,e->(e = toList e;
	       fil << "\t"|toString H#(e_0)|" -- "|toString H#(e_1)|";" << endl;
	       )
	  );
     fil << "}" << endl << close;
     )

writeDotFile(String, Digraph) := (filename, G) -> (
     -- Input:  The desired file name for the Dot file created and a digraph
     -- Output:  The code for the inputted digraph to be constructed in Graphviz 
     --          with the specified file name.
     fil := openOut filename;
     fil << "digraph G {" << endl;
     V := vertices G;
     H := hashTable apply(#V,i->V_i=>i);
     scan(V,v-> fil << "\t"|toString H#v|" [label=\""|toString v|"\"];" << endl);
     E := edges G;
     scan(E,e->(e = toList e;
	       fil << "\t"|toString H#(e_0)|" -> "|toString H#(e_1)|";" << endl;
	       )
	  );
     fil << "}" << endl << close;
     )     

runcmd := cmd -> (
     stderr << "--running: " << cmd << endl;
     r := run cmd;
     if r != 0 then error("--command failed, error return code ",r);
     )

displayGraph = method()
    -- Displays a graph or digraph using Graphviz
    -- Input:  A digraph or graph and optionally names for the dot
    --         and jpg files.
displayGraph(String,String,Digraph) := (dotfilename,jpgfilename,G) -> (
     writeDotFile(dotfilename,G);
     runcmd(dotBinary | " -Tjpg "|dotfilename | " -o "|jpgfilename);
     runcmd(jpgViewer | " " | jpgfilename);
     )
-- Note, when specifying a jpgfilename w/o the .jpg extension the
-- graph opens in Quicktime player.  Otherwise the graph opens in
-- Preview.  Why?  Does this matter? -T.O.
displayGraph(String,Digraph) := (dotfilename,G) -> (
     jpgfilename := temporaryFileName() | ".jpg";
     displayGraph(dotfilename,jpgfilename,G);
     --removeFile jpgfilename;
     )
displayGraph Digraph := (G) -> (
     dotfilename := temporaryFileName() | ".dot";
     displayGraph(dotfilename,G);
     --removeFile dotfilename;
     )

showTikZ = method(Options=>{Options=>"-t math --prog=dot -f tikz --figonly"})
showTikZ(Digraph) := opt -> G -> (
     dotfilename := temporaryFileName() | ".dot";
     writeDotFile(dotfilename,G);
     output := temporaryFileName();
     runcmd("dot2tex "|opt#Options|" "|dotfilename|" >> "|output);
     get output
     )

------------------
-- Graph basics --
------------------

vertices = method()
     -- Input: A digraph
     -- Output:  A list of vertices
vertices(Digraph) := G -> (
     if G#cache#?vertices then G#cache#vertices else(
     	  G1 := graph G;
     	  V := keys G1;
     	  G#cache#vertices = V;
     	  V))   
vertices(MixedGraph) := G -> (
     if G#cache#?vertices then G#cache#vertices else(
	  G1 := graph G;
	  V := toList sum(apply(keys(G1),i->set keys(graph G1#i)));
	  G#cache#vertices = V;
	  V))

edges = method()
     -- Input: A graph
     -- Output: A list of sets of order 2, each corresponding to an edge
edges(Digraph) := G -> (
     if G#cache#?edges then G#cache#edges else (
     	  G1 := graph G;
     	  E := flatten apply(keys(G1),i->apply(toList G1#i,j->{i,j}));
	  G#cache#edges = E;
	  E)
     )
edges(Graph) := G -> (
     if G#cache#?edges then G#cache#edges else (
     	  E := apply(edges simpleGraph G,set);
	  G#cache#edges = E;
	  E)
     )

edges(Bigraph) := G -> (
     if G#cache#?edges then G#cache#edges else (
     	  E := apply(edges simpleGraph G,set);
	  G#cache#edges = E;
	  E)
     )


descendents = method()
descendents(Digraph,Thing) := (G,v) -> (
     -- Input: A digraph and the key for the vertex of interest.
     -- Output: The set of vertices that are descendents of the vertex 
     --         of interest.
     if G.cache#?descendents and G.cache#descendents#?v then G.cache#descendents#v else(
     	  notDone := true;
     	  cC := children(G,v);
     	  dE := cC;
     	  while notDone === true do(
	       if (toList cC) === {} then notDone = false
	       else (
	       	    cC = set flatten apply(toList cC, i -> toList children(G,i));
	       	    dE = dE + cC;
	       	    )
	       );
	  if G.cache#?descendents then (
	       h := new MutableHashTable from G.cache#descendents;
	       h#v = dE;
	       G.cache#descendents = new HashTable from h)
	  else (
	       h = new MutableHashTable;
	       h#v = dE;
	       G.cache#descendents = new HashTable from h);
     	  dE)
     )

descendents(MixedGraph, Thing) := (G,v) -> (
     G1 := digraph G;
     if G1.cache#?descendents and G1.cache#descendents#?v then G1.cache#descendents#?v
     else (
	  C := descendents(G1,v);
	  dE := C;  -- added (Mike Stillman)
	  if G1.cache#?descendents then (
	       h := new MutableHashTable from G1.cache#descendents;
	       h#v = dE;
	       G1.cache#descendents = new HashTable from h)
	  else (
	       h = new MutableHashTable;
	       h#v = C;
	       G1.cache#descendents = new HashTable from h);
	  C)
     )
     
--     result := G#v;
--     scan(keys(G), i -> (
--	  if member(i,result) then result = result + G#i;
--     ));
--     result)

nondescendents = method()
     -- Input: A digraph and the key for the vertex of interest.
     -- Output: The set of vertices that are not descendents of 
     --	        the vertex of interest.
nondescendents(Digraph,Thing) := (G,v) -> (
     if G.cache#?nondescendents and G.cache#nondescendents#?v then G.cache#nondescendents#v else(
     	  C := set keys (graph G) - descendents(G,v) - set {v};
	  if G.cache#?nondescendents then (
	       h:= new MutableHashTable from G.cache#nondescendents;
	       h#v = C;
	       G.cache#nondescendents = new HashTable from h)
	  else (
	       h = new MutableHashTable;
	       h#v = C;
	       G.cache#nondescendents = new HashTable from h);
	  C)
     )
nondescendents(MixedGraph,Thing) := (G,v) -> (
     G1 := digraph G;
     if G1.cache#?nondescendents and G1.cache#nondescendents#?v then G1.cache#nondescendents#v else(
     	  C := set keys (graph G1) - descendents(G1,v) - set {v};
	  if G1.cache#?nondescendents then (
	       h:= new MutableHashTable from G1.cache#nondescendents;
	       h#v = C;
	       G1.cache#nondescendents = new HashTable from h)
	  else (
	       h = new MutableHashTable;
	       h#v = C;
	       G1.cache#nondescendents = new HashTable from h);
	  C)
     )

parents = method()
     -- Input: A digraph and the key for the vertex of interest.
     -- Output: The set of vertices that are the parents of the vertex 
     --	    	of interest.
parents(Digraph,Thing) := (G,v) -> (
     if G.cache#?parents and G.cache#parents#?v then G.cache#parents#v else(
     	  C := set select(keys(graph G), i -> member(v, (graph G)#i));
	  if G.cache#?parents then (
	       h:= new MutableHashTable from G.cache#parents;
	       h#v = C;
	       G.cache#parents = new HashTable from h)
	  else (
	       h = new MutableHashTable;
	       h#v = C;
	       G.cache#parents = new HashTable from h);
	  C)
     )
parents(MixedGraph,Thing) := (G,v) -> (
     G1 := digraph G;
     if G1.cache#?parents and G1.cache#parents#?v then G1.cache#parents#v else(
     	  C := set select(keys(graph G1), i -> member(v, (graph G1)#i));
	  if G1.cache#?parents then (
	       h:= new MutableHashTable from G1.cache#parents;
	       h#v = C;
	       G1.cache#parents = new HashTable from h)
	  else (
	       h = new MutableHashTable;
	       h#v = C;
	       G1.cache#parents = new HashTable from h);
	  C)
     )
     	  
foreFathers = method()
     -- Input: A digraph and the key for the vertex of interest.
     -- Output: The set of vertices that are the ancestors of the vertex 
     --	    	of interest.
foreFathers(Digraph, Thing) := (G,v) -> (
     if G.cache#?foreFathers and G.cache#foreFathers#?v then G.cache#foreFathers#v else(
     	  notDone := true;
     	  cP := parents(G,v);
     	  aN := cP;
     	  while notDone === true do(
	       if (toList cP) === {} then notDone = false
	       else (
	       	    cP = set flatten apply(toList cP, i -> toList parents(G,i));
	       	    aN = aN + cP;
	       	    )
	       );
	  if G.cache#?foreFathers then (
	       h := new MutableHashTable from G.cache#foreFathers;
	       h#v = aN;
	       G.cache#foreFathers = new HashTable from h)
	  else (
	       h = new MutableHashTable;
	       h#v = aN;
	       G.cache#foreFathers = h);
     	  aN)
     )	  
foreFathers(MixedGraph, Thing) := (G,v) -> (
     G1 := digraph G;
     if G1.cache#?foreFathers and G1.cache#foreFathers#?v then G1.cache#foreFathers#?v
     else (
	  C := foreFathers(G1,v);
	  if G1.cache#?foreFathers then (
	       h:= new MutableHashTable from G1.cache#foreFathers;
	       h#v = C;
	       G1.cache#foreFathers = new HashTable h)
	  else (
	       h = new MutableHashTable;
	       h#v = C;
	       G1.cache#foreFathers = new HashTable h);
	  C)
     )
     

children = method()
     -- Input: A digraph and the key for the vertex of interest.
     -- Output: The set of vertices that are the children of the vertex 
     --	    	of interest.
children(Digraph,Thing) := (G,v) -> (
     if G.cache#?children and G.cache#children#?v then G.cache#children#v else (
	  C := (graph G)#v;
	  if G.cache#?children then (
	       h := new MutableHashTable from G.cache#children;
	       h#v = C;
	       G.cache#children = new HashTable from h)
	  else(
	       h = new MutableHashTable;
	       h#v = C;
	       G.cache#children = new HashTable from h);
	  C)
     )

children(MixedGraph, Thing) := (G,v) -> (
     G1 := digraph G;
     if G1.cache#?children and G1.cache#children#?v then G1.cache#children#?v
     else (
	  C := children(G1,v);
	  if G1.cache#?children then (
	       h := new MutableHashTable from G1.cache#children;
	       h#v = C;
	       G1.cache#children = new HashTable from h)
	  else (
	       h = new MutableHashTable;
	       h#v = C;
	       G1.cache#children = new HashTable from h);
	  C)
     )
     

neighbors = method()
     -- Input: A graph and the key for the vertex of interest.
     -- Output: The set of vertices that are neighbors of the vertex 
     --	    	of interest.
neighbors(Graph,Thing) := (G,v) -> (
     if G.cache#?neighbors and G.cache#neighbors#?v then G.cache#neighbors#v else (
	  n := (graph G)#v;
	  if G.cache#?neighbors then (
	       h := new MutableHashTable from G.cache#neighbors;
	       h#v = n;
	       G.cache#neighbors = new HashTable from h)
	  else (
	       h = new MutableHashTable;
	       h#v = n;
	       G.cache#neighbors = new HashTable from h);
	  n)
     )
neighbors(MixedGraph,Thing) := (G,v) -> (
     G1 := (graph G)#Graph;
     if G1.cache#?neighbors and G1.cache#neighbors#?v then G1.cache#neighbors#v else (
	  n := (graph G1)#v;
	  if G1.cache#?neighbors then (
	       h := new MutableHashTable from G1.cache#neighbors;
	       h#v = n;
	       G1.cache#neighbors = new HashTable from h)
	  else (
	       h = new MutableHashTable;
	       h#v = n;
	       G1.cache#neighbors = new HashTable from h);
	  n)
     )
	 
nonneighbors = method()
     -- Input: A graph and the key for the vertex of interest.
     -- Output: The set of vertices that are not neighbors of the vertex 
     --	    	of interest.
nonneighbors(Graph, Thing) := (G,v) -> (
     if G.cache#?nonneighbors and G.cache#nonneighbors#?v then G.cache#nonneighbors#v else (
     	  n := set vertices G - neighbors(G,v) - set{v};
	  if G.cache#?nonneighbors then (
	       h := new MutableHashTable from G.cache#nonneighbors;
	       h#v = n;
	       G.cache#nonneighbors = new HashTable from h)
	  else (
	       h = new MutableHashTable;
	       h#v = n;
	       G.cache#nonneighbors = new HashTable from h);
	  n)
     )
nonneighbors(MixedGraph,Thing) := (G,v) -> (
     G1 := (graph G)#Graph;
     if G1.cache#?nonneighbors and G1.cache#nonneighbors#?v then G1.cache#nonneighbors#v else (
	  n := nonneighbors(G1, v);
	  if G1.cache#?nonneighbors then (
	       h := new MutableHashTable from G1.cache#nonneighbors;
	       h#v = n;
	       G1.cache#nonneighbors = new HashTable from h)
	  else (
	       h = new MutableHashTable;
	       h#v = n;
	       G1.cache#nonneighbors = new HashTable from h);
	  n)
     )

removeNodes = method()
     -- Input: A digraph and the list of nodes you want to remove.
     -- Output: The digraph induced by removing the specified nodes
     --         and all edges incident to those nodes.
removeNodes(Digraph,List) := (G,v) -> (
     v = set v;
     G = select(pairs G, x -> not member(x#0,v));
     G = apply(G, x -> (x#0, x#1 - v));
     new Digraph from G
     )
--removeNodes(Digraph,ZZ) := (G,v) -> removeNodes(G, {v})

inducedSubgraph = method()
     -- Input: A digraph and the list of nodes you want to keep.
     -- Output: The digraph induced by keeping the specified nodes and 
     --         all edges whose endpoints are in the specified list.
inducedSubgraph(Digraph, List) := (G,v) -> (
     G = removeNodes(G,toList(keys(G)-set v));
     new Digraph from G
     )

adjacencyMatrix = method()
     -- Input:  A digraph
     -- Output:  A matrix M, such that M_(i,j)=1 iff there exists an arc from i to j
adjacencyMatrix(Digraph) := G -> (
     if G#cache#?adjacencyMatrix then G#cache#adjacencyMatrix else (
     	  G1 := graph G;
     	  VV := vertices G;
     	  AM := matrix apply(VV,i->apply(VV,j->#positions(toList G1#i,k->k===j)));
	  G#cache#adjacencyMatrix = AM;
	  AM))

degreeMatrix = method()
degreeMatrix(Graph) := G -> (
    -- The actual graph is stored in in the value of 'graph'
    H := G#graph;
    matrix apply(#keys(H),i->apply(#keys(H),j->if i==j then #(H#((keys H)_i)) else 0)))

laplacianMatrix = method()
laplacianMatrix(Graph) := G -> degreeMatrix G - adjacencyMatrix G

incidenceMatrix = method()
     -- Input: A graph
     -- Output: A matrix M, such that M_(i,j)=1 iff vertex i is incident to edge j     
incidenceMatrix(Graph) := G -> matrix apply(keys(G),i->(apply(edges G,j->(if j#?i then 1 else 0))))

reachable = method()
    -- Input: A directed graph G and subset A of vertices.
    -- Output: Set of vertices of G which have a directed path from A.
reachable (Digraph, Set) := (G,A) -> (
    G1 := graph G;
    V := vertices G;
    reached := new MutableHashTable from apply(V,i->{i,false});
    queue := toList A;   
    while #queue > 0 do (
      topVertex := first queue;
      if not reached#topVertex then (
        reached#topVertex = true;
        queue = join(drop(queue,1),toList G1#topVertex);
      ) else (
        queue = drop(queue,1);
      );
    );
    set select(V,i->reached#i)
    )
reachable (Digraph,List) := (G,A) -> toList reachable(G,set A)

floydWarshall = method()
     -- Input:  A digraph
     -- Output:  A hash table whose keys are pairs of vertices and the value is the length of the shortest path between the first vertex and the second vertex
floydWarshall(Digraph) := G -> (
     D := new MutableHashTable from flatten apply(vertices(G),u->(apply(vertices(G),v->((u,v)=> if u===v then 0 else if member(v,(children(G,u))) then 1 else 1/0.))));
     scan(vertices(G),w->(
	       scan(vertices(G),u->(
			 scan(vertices(G),v->(
				   D#(u,v)=min(D#(u,v),D#(u,w)+D#(w,v))
				   )
			      )
			 )
		    )
	       )
	  );
     new HashTable from D
     )

collateVertices = method()
collateVertices (MixedGraph) := (g) -> (
    -- Input: A MixedGraph
    -- Output: A MixedGraph where the hash tables for the graph, bigraph and digraph all have the same keys (vertices)
    v := vertices(g);
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

----------------------
-- Topological Sort --
----------------------

SortedDigraph = new Type of HashTable
     -- 3 keys:
     -- digraph: original digraph
     -- newDigraph: digraph with vertices labeled as integers obtained from sorting
     -- map: gives sorted order
     
-- functions adapted from pseudocode given in Cormen, Introduction to algorithms --

topSort = method()
     -- Input: A digraph
     -- A sorted digraph, topologically sorted
topSort(Digraph) := G -> (
     if not isCyclic G then (
     	  L := reverse apply(sort apply(pairs ((DFS G)#"finishingTime"),reverse),p->p_1);
     	  H := hashTable{
	       digraph => G,
	       newDigraph => digraph hashTable apply(#L,i->i+1=>apply(toList (graph G)#(L_i),j->position(L,k->k==j)+1)),
	       map => hashTable apply(#L,i->L_i => i+1)
	       };
     	  new SortedDigraph from H
	  )
     else error("digraph must be acyclic")
     )     

DFS = method()
     -- Input: A digraph
     -- Output: the discovery and finishing times for each vertex after a depth-first search
DFS(Digraph) := G -> (
     H := new MutableHashTable;
     H#graph = G;
     H#"color" = new MutableHashTable;
     H#"p" = new MutableHashTable;
     H#"d" = new MutableHashTable;
     H#"f" = new MutableHashTable;
     H#"t" = 0;
     scan(vertices G,u->(H#"color"#u="white"; H#"p"#u=1));
     scan(vertices G,u->if H#"color"#u == "white" then H = DFSvisit(H,u));
     new HashTable from {"discoveryTime" => new HashTable from H#"d", "finishingTime" => new HashTable from H#"f"}
     )

DFSvisit = (H,u) -> (
     H#"color"#u = "gray";
     H#"t" = H#"t"+1;
     H#"d"#u = H#"t";
     scan(toList children(H#graph,u),v->if H#"color"#v == "white" then (H#"p"#v = u;H = DFSvisit(H,v)));
     H#"color"#u = "black";
     H#"t" = H#"t"+1;
     H#"f"#u = H#"t";
     H
     )

isCyclic = method()
     -- Input:  A digraph (not undirected)
     -- Output:  Whether the digraph contains a cycle
     -- uses Cormen Lemma 22.11 and Exercise 22.3-4
isCyclic(Digraph) := G -> (
     if instance(G,Graph) then error ("must be a digraph") else (
     	  D := DFS G;
     	  member(true,flatten unique apply(select(vertices G,u->#children(G,u)>0),u->(
	       	    	 apply(toList children(G,u),v->(
		    	      	   L := {D#"discoveryTime"#v,D#"discoveryTime"#u,D#"finishingTime"#u,D#"finishingTime"#v};
     	       	    	      	   L == sort L
		    	      	   )
	       	    	      )
	       	    	 )
     	       	    )
     	       )
     	  )
     )

-------------------
-- Common Graphs --
-------------------


completeGraph = method()
     -- Input: a positive integer n
     -- Output: the complete graph on n nodes labeled 0 through n-1
completeGraph(ZZ) := n -> (
      i := 0;
      G := new MutableHashTable;
      L := while i < n list i do i = i+1;
      apply(L, i-> G#i =  set L - set {i});
      graph(new HashTable from G)
      )   

cycleGraph = method()
     -- Input: a positive integer n
     -- Output: the cyclic graph on n nodes labeled 0 through n-1 
cycleGraph(ZZ) := n -> (
     i := 0;
     G := new MutableHashTable;
     while i < n do(
	  G#i = set{(i-1)%n,(i+1)%n}; 
	  i = i+1;
	  );
     graph(new HashTable from G)
     )
  

--------------------
-- Documentation  --
--------------------


///
restart
loadPackage "Graphs"
A = graph({{a,b},{c,d},{a,d},{b,c}}, Singletons => {f})
B = digraph({{a,{b,d}},{b,{c}},{d,{a,c}}})
C = digraph({{a,{b,c}}, {b,{d}}, {c,{}}, {d,{}}})
H = digraph({{a,{d,f}}, {d,{h,e}}, {f,{e,c}}, {h,{g}}, {e,{g,b}}, {g,{}}, {b,{}}, {c,{}}})
errorinducedSubgraph(H,{a,f,h,g})
K = graph({{a,b},{b,c},{c,d},{d,e},{a,e},{b,e}})
inducedSubgraph(K,{a,e,b})
///



beginDocumentation()

doc ///
  Key
    Graphs
  Headline
    Data types and basic functions on graphs used in algebra and algebraic geometry. 
  Description
    Text
      This package is used to construct digraphs and graphs and
      perform basic functions on them. The user should note that this
      package assumes that all digraphs are acyclic.  Also, graphs are 
      assumed to have no loops or multiple edges. This package has
      functions to view graphs and digraphs.  These functions call the programs
      Graphviz and dot2tex and are only set up to function on Unix-like computers (e.g., Macintosh, Linux) at
      this time. 
///

doc ///
  Key
    Graph
  Headline
    The data type for an undirected graph.
  Description
    Text    
///

doc ///
  Key
    Digraph
  Headline
    The data type for a directed graph.
  Description
    Text
///

doc ///
  Key
    MixedGraph
  Headline
    The data type for a mixed graph consisting of a Graph, Digraph, Bigraph.
  Description
    Text  
       The Graph or the Bigraph may be empty. 
///  
    
doc ///
  Key
    Bigraph
  Headline
    The data type for a bigraph.
  Description
    Text    
       We note that this type is typically only used in modeling and
       was primarily necessary to design the data type MixedGraph
       appropriately. 
///

doc ///
  Key
    graph
    (graph, HashTable)
    (graph, List)
  Headline
    The function that creates a graph.
  Usage
    graph(L)
  Inputs
    L:List 
      of edges of the graph or @ofClass HashTable@ with keys the nodes
      and values the neighbors of that node. 
  Outputs
    :Graph
  Description
    Text
      The graph is stored as a hash table whose keys are the nodes and
      whose values are the neighbors of the nodes.  The user inputs a
      graph by inputting a list of edges. There is an optional
      argument called Singletons to input nodes that have no neighbors. 
    Example
      A = graph({{x_1,x_3},{x_2,x_4},{x_1,x_4}})
      B = graph({{a,b},{c,d},{a,d},{b,c}}, Singletons => {f})
    Text  
      Alternatively, one can also create a graph by inputting a hash
      table whose keys are again the nodes of the graph and the values
      the neighbors of the nodes.
///



doc ///
  Key
    digraph
    (digraph, HashTable)
    (digraph, List)
  Headline
    The function that creates a digraph
  Usage
    digraph(L) 
  Inputs
    L:List
      of pairs consisting of a node and its children or @ofClass
      HashTable @ with keys the nodes and values the children of that
      node.  
  Outputs
    :Digraph
  Description
    Text
      The digraph is stored as a hash table whose keys are the names
      of the nodes and whose values are the children of those nodes.
      The user inputs a digraph by inputting a list of pairs whose first
      element is a node and whose second element is the list of children
      of that node.  If a node has no children, it should still be
      included in the list of pairs followed by an empty list.
      Alternatively, one could also input a hash table where the keys are
      the nodes and the values the set of children.
    Example
      D = digraph({{a, {b,c}}, {b,{d,e}}, {c, {e,h}}, {d, {f}}, {e, {f,g}},
  	  {f, {}}, {g, {}}, {h, {}}})
///

doc ///
  Key
    mixedGraph
    (mixedGraph, Graph, Digraph, Bigraph)
    (mixedGraph, Graph, Digraph)
    (mixedGraph, Digraph, Bigraph)
    (mixedGraph, Digraph)
  Headline
    The function that creates a mixed graph.
  Usage
    mixedGraph(G,D,B) 
    mixedGraph(G,D) 
    mixedGraph(D,B) 
    mixedGraph(D)
  Inputs
    G:Graph
    D:Digraph
    B:Bigraph
  Outputs
    :MixedGraph
  Description
    Text
      A mixed graph is stored as a HashTable whose keys are the types
      Graph, Digraph, and Bigraph.  The values are the corresponding
      graphs.   The order of the input matters.  An error is issued if
      the first argument is not a graph, the second a Digraph and the
      third a Bigraph.  
   Example
      D = digraph({{a, {b,c}}, {b,{d,e}}, {c, {e,h}}, {d, {f}}, {e, {f,g}},
  	  {f, {}}, {g, {}}, {h, {}}})
      G = graph({{a,b},{c,d},{a,c},{d,h},{b,h}})
      B = bigraph({{c,d},{e,f}})
      H = mixedGraph(G,D,B)
///

doc ///
  Key
    Singletons
  Headline
    Optional argument for the function graph
  Description
    Text
      This is an optional argument for @TO graph@ to input a list of nodes
      with no neighbors.
  
///

doc ///
  Key
    descendents
    (descendents, Digraph, Thing)
    (descendents, MixedGraph, Thing)
  Headline 
    Returns the descendents of a node in a digraph or mixed graph. 
  Usage
    descendents(D,v)
  Inputs
    D:Digraph 
      or @ofClass MixedGraph@ 
    v:Thing
      representing a node in the digraph or the mixed graph.
  Outputs
    :Set
       consisting of the descendents of v.
  Description
    Text
      This is a function which takes as input a @TO digraph@ or a @TO
      mixedGraph@ and a symbol giving the node of interest.  It returns a set 
      of the keys for the descendents of that node.  
    Example
      D = digraph({{a, {b,c}}, {b,{d,e}}, {c, {e,h}}, {d, {f}}, {e, {f,g}},
  	  {f, {}}, {g, {}}, {h, {}}})
      descendents(D,c)
    Text
      In the case of a @TO mixedGraph@ the set is the descendents of
      the node as a node in the digraph part of the mixed graph. 
    Example
      H = mixedGraph(graph{{a,b},{a,f},{c,f}}, D)
      descendents(H,c)
      descendents(H,e)
///

doc ///
  Key
    nondescendents
  Headline
    Returns the nondescendents of a node in a digraph
  Usage
    nD = nondescendents(D,v)
  Inputs
    D:Digraph
    v:Thing
      representing a node in the digraph
  Outputs
    nD:Set
       consisting of the nondescendents of v. 
  Description
    Text
      This function takes as input a @TO digraph@ and the name given
      to the node of interest.  It returns the set of vertices that
      are not descendents of that node.
    Example
      D = digraph({{a, {b,c}}, {b,{d,e}}, {c, {e,h}}, {d, {f}}, {e, {f,g}},
  	  {f, {}}, {g, {}}, {h, {}}})
      nD = nondescendents(D,c)
///



doc ///
  Key
    parents
  Headline
    Returns the parents of a vertex in a digraph
  Usage
    pA = parents(D,v)
  Inputs
    D:Digraph
    v:Thing
      that is the name of the node of interest
  Outputs
    pA:Set
       containing the names of the parents of the node
  Description
    Text
      This function takes as input a @TO digraph@ and the name of
      the node of interest.  It returns the set of the names
      for the vertices that are the parents of the node given.
    Example
      D = digraph({{a, {b,c}}, {b,{d,e}}, {c, {e,h}}, {d, {f}}, {e, {f,g}},
  	  {f, {}}, {g, {}}, {h, {}}})
      pA = parents(D,e)
///

doc ///
  Key
    children
  Headline
    Returns the children of a node in a digraph
  Usage
    cH = children(D,v)
  Inputs
    D:Digraph
    v:Thing
      that is the name of the node of interest
  Outputs
    cH:Set
       containing the names of the children of the node
  Description
    Text
       This function takes as input a @TO digraph@ and the name of 
       the node of interest.  This function returns the set of the names
       for the vertices that are the children of the node given. 
    Example
      D = digraph({{a, {b,c}}, {b,{d,e}}, {c, {e,h}}, {d, {f}}, {e, {f,g}},
  	  {f, {}}, {g, {}}, {h, {}}})
      cH = children(D,e)
///

doc ///
  Key
    vertices
    (vertices, Digraph)
    (vertices, MixedGraph)
  Headline 
    Returns the vertices of any type of grpah.  . 
  Usage
    vertices H
  Inputs
    H:Digraph 
      or @ofClass MixedGraph@ or @ofClass Graph@ or @ofClass Bigraph@
  Outputs
    :Set
       consisting of the vertices of v.
  Description
    Text
    Example
      D = digraph({{c, {e,h}}, {a, {b,c}}, {d, {f}}, {e, {f,g}},
  	  {f, {}}, {g, {}}, {b,{d,e}}, {h, {}}})
     vertices D 
///

doc ///
  Key
    reachable
    (reachable,Digraph,List)
    (reachable,Digraph,Set)
  Headline
    Computes the vertices which are reachable from a vertex set by a path
  Usage
    reachable(G,A)
  Inputs
    G:Digraph
    A:Set 
      or @ofClass List@ of vertices of the digraph
  Outputs
    :Set 
      or @ofClass List@ of vertices which are reachable from A by a path
  Description
    Text
      If the user inputs a set A, then the output will be a set of vertices. 
      If the user inputs a list A, then the output will be a list of vertices.
    Example
      G = digraph {{1,2},{2,3},{4,5},{2,5}}
      reachable(G,{2})
///


doc ///
         Key
	      adjacencyMatrix
	      (adjacencyMatrix,Digraph)
         Headline
	      Computes the adjacency matrix of a graph or digraph
         Usage
	      M = adjacencyMatrix(G)
         Inputs
	      G:Digraph
         Outputs
	      M:Matrix
         Description
            Text
 	      The (i,j)-entry of the adjacency matrix is 1 if there exists an arc or edge connected the ith vertex to the jth vertex and 0 otherwise.
            Example
	    	 adjacencyMatrix completeGraph 5
      ///
      
doc ///
         Key
	      (showTikZ,Digraph)
	      showTikZ
         Headline
	      outputs TikZ syntax for displaying a graph or digraph in TeX
         Usage
	      showTikZ G
         Inputs
	      G:Digraph
         Outputs
	      S:String
	      	    TikZ syntax which can be pasted into a .tex file to display G
         Description
            Text
	    	 showTikZ requires the external program dot2tex, available at http://www.fauskes.net/code/dot2tex/.
		 
		 The following code gives TikZ syntax for the complete graph K_5.
            Text
     	       	 showTikZ completeGraph 5
      ///
      
doc ///
        Key
	     [showTikZ,Options]
        Headline
	     a string which is passed to dot2tex.  Defaults to "-t math --prog=dot -f tikz --figonly".  Run "dot2tex --help" for all possibilties.
        Usage
	     foo
	Inputs
	     S:String     
     ///

TEST /// ---- family members, neighbors and non-neighbors. 
G = graph({{a,b},{b,c},{a,c},{c,d}}, Singletons => {e})
H = digraph{{a,{b,c}},{b,{c,d}},{c,{d}},{c,{}}}
assert(neighbors(G,a) === set {b,c})
assert(nonneighbors(G,b) === set {d,e})
assert(parents(H,a) === set {})
assert(parents(H,c) === set {a,b})
assert(children(H,a) === set {b,c})
assert(descendents(H,a) === set {b,c,d})
assert(nondescendents(H,c) === set {a,b})
assert(foreFathers(H,c) === set {a,b})
///

-- laplacianMatrix
TEST ///
K5 = graph { {x1, {x2,x3,x4,x5}}, {x2, {x3,x4,x5}}, {x3, {x4,x5}}, {x4, {x5}} }
M = matrix {{4, -1, -1, -1, -1}, {-1, 4, -1, -1, -1}, {-1, -1, 4, -1, -1}, {-1, -1, -1, 4, -1}, {-1, -1, -1, -1, 4}}
assert (laplacianMatrix K5 == M);
///



---------- UNUSED STUFF FOR FUTURE EXPERIMENTS ----------

end

doc ///
  Key
    neighbors
    (neighbors,Graph)
  Headline
    Returns the neighbors of a given node in a graph 
  Usage
       neighbors(G,v)
  Inputs
       G:Digraph
       v:Thing, a vertex of G
  Outputs
       List
  Description
    Text
///


doc ///
  Key
    nonneighbors
  Headline
  Description
    Text
///

doc ///
  Key
    displayGraph
  Headline
  Description
    Text
///

doc ///
  Key
    simpleGraph
  Headline
  Description
    Text
///

doc ///
  Key
    removeNodes
  Headline
  Description
    Text
///

doc ///
  Key
    dotBinary
  Headline
  Description
    Text
///

doc ///
  Key
    jpgViewer
  Headline
  Description
    Text
///


end

restart
loadPackage"Graphs"


TEST /// ---- display?

///

TEST /// ---- Functions on Graphs. 

///



     mixedGraph, labeledGraph, displayGraph,
     simpleGraph, removeNodes, inducedSubgraph, completeGraph,
     cycleGraph, writeDotFile, SortedDigraph, topSort, DFS,
     adjacencyMatrix, edgeSet, incidenceMatrix}
exportMutable {dotBinary,jpgViewer 

break

TEST ///
  -- descendents of a Digraph and MixedGraph
  G = digraph { {1, {2,3}}, {2, {4}}, {3, {4}} }
  assert(descendents(G, 1) === set{2,3,4})
  assert(descendents(G, 4) === set{})
  assert(try (descendents(G, 5); false) else true)
  
  G = digraph { {1, {2,3}}, {2, {4}}, {3, {4}} }
  peek G
  descendents(G, 1)
  peek G.cache
  
  B = bigraph { {1,4}, {1,5} }
  G1 = mixedGraph(G, B)
  vertices G1
  peek G1
  peek G1.cache
  descendents(G1, 1)
///
