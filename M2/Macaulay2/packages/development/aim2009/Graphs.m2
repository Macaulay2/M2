-- -*- coding: utf-8 -*-
newPackage("Graphs",
     Authors => {
	  {Name => "Amelia Taylor"},
	  {Name => "Others"}
	  },
     DebuggingMode => false,
     Headline => "Data types for graphs",
     Version => "0.1"
     )

export {Graph, Digraph, graph, digraph, Singletons}
     --  simpleGraph, descendents, nondescendents, parents, children, removeNodes,neighbors, nonneighbors
--exportMutable {dotBinary,jpgViewer}


Digraph = new Type of HashTable
     -- a directed graph is a hash table in the form:
     -- { A => set {B,C,...}, ...}, where there are edges A->B, A->C, ...
     -- and A,B,C are integers.  

Graph = new Type of Digraph   
     -- an undirected graph is a hash table in the form { A => set {B,C, ...}, 
     -- where the set consists of the neighbors of A. 

union := S -> (
     x = new MutableHashTable;
     for t in S do scanKeys(t, z -> x#z = 1);
     new Set from x)  

graph = method(Options => {Singletons => null})
graph List := opts -> (g) -> (
     -- Input:  A list of lists.  The first list is the names of the nodes, the second 
     -- list is a list of lists giving the neighbors for each node.
     -- Output:  A hashtable with keys the names of the nodes with the 
     -- children/neighbors assigned. 
     h := new MutableHashTable;
     gSets := apply(g, i -> set i);
     gSetsUnion := union gSets;
     if opts.Singletons === null then (
	  vertices := toList gSetsUnion;
	  neighbors := for j to #vertices-1 list( for k to #g-1 list (if member(vertices#j, set g#k) then set g#k - set {vertices#j} else continue));
	  neighbors = apply(neighbors, i -> union i);
	  )
     else ( vertices = join(toList gSetsUnion, opts.Singletons);
	  newEdges := apply(for i to #opts.Singletons - 1 list {}, i -> set i);
	  neighbors = for j to #vertices-1 list( for k to #g-1 list (if member(vertices#j, set g#k) then set g#k - set {vertices#j} else continue));
	  neighbors = apply(neighbors, i -> union i);
	  neighbors = join(neighbors,newEdges);
--	  error "what is vertices";
	  );
     apply(#vertices, i -> h#(vertices#i) = neighbors#i);
     new Graph from h)

--graph MutableHashTable := opts -> (g) -> (
--     new Graph from h)

graph HashTable := opts -> (g) -> (
     new Graph from g)

digraph = method()
digraph List := (g) -> (
     -- Input:  A list of lists.  The first list is the names of the nodes, the second 
     -- list is a list of lists giving the children for each node.
     -- Output:  A hashtable with keys the names of the nodes with the 
     -- children assigned. 
     h := new MutableHashTable;
     vertices := g#0;
     children := g#1;
     apply(#vertices, i -> h#(vertices#i) = set children#i);
     new Digraph from h)
     
-----------------------------
-- Graph Display Functions --
-----------------------------

-- dotBinary = "/sw/bin/dot"
dotBinary = "dot"
-- jpgViewer = "/usr/bin/open"
jpgViewer = "open"

simpleGraph := H -> (
     q := pairs H;
     qmute := new MutableList;
     for i to #q-1 do qmute#i = q#i;
     for k from 1 to #q-1 do (
	  testVertices := set for i to k-1 list qmute#i#0;
      	  qmute#k = (qmute#k#0, qmute#k#1-testVertices)
	  );
     newGraph := new MutableHashTable;
     for k to #q-1 do newGraph#(qmute#k#0) = qmute#k#1;
     new Graph from newGraph)

 
writeDotFile = method()
writeDotFile(String, Graph) := (filename, G) -> (
     G = simpleGraph G;
     fil := openOut filename;
     fil << "graph G {" << endl;
     q := pairs G;
     for i from 0 to #q-1 do (
	  e := q#i;
	  fil << "  " << toString e#0;
	  if #e#1 === 0 or all(q, j->member(e#0,j#1)) then
	    fil << ";" << endl
	  else (
	    fil << " -- {";
	    links := toList e#1;
	    for j from 0 to #links-1 do
		 fil << toString links#j << ";";
     	    fil << "};" << endl;
	    )
	  );
     fil << "}" << endl << close;
     )

writeDotFile(String, Digraph) := (filename, G) -> (
     fil := openOut filename;
     fil << "digraph G {" << endl;
     q := pairs G;
     for i from 0 to #q-1 do (
	  e := q#i;
	  fil << "  " << toString e#0;
	  if #e#1 === 0 then
	    fil << ";" << endl
	  else (
	    fil << " -> {";
	    links := toList e#1;
	    for j from 0 to #links-1 do
		 fil << toString links#j << ";";
     	    fil << "};" << endl;
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

displayGraph(String,String,Digraph) := (dotfilename,jpgfilename,G) -> (
     writeDotFile(dotfilename,G);
     runcmd(dotBinary | " -Tjpg "|dotfilename | " -o "|jpgfilename);
     runcmd(jpgViewer | " " | jpgfilename);
     )
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

------------------
-- Graph basics --
------------------

descendents = method()
descendents(Digraph,ZZ) := (G,v) -> (
     -- returns a set of vertices
     result := G#v;
     scan(reverse(1..v-1), i -> (
	  if member(i,result) then result = result + G#i;
     ));
     result)

nondescendents = method()
nondescendents(Digraph,ZZ) := (G,v) -> set(1..#G) - descendents(G,v) - set {v}

parents = method()
parents(Digraph,ZZ) := (G,v) -> set select(1..#G, i -> member(v, G#i))

children = method()
children(Digraph,ZZ) := (G,v) -> G#v

neighbors = method()
neighbors(Graph,Thing) := (G,v) -> G#v  

nonneighbors = method()
nonneighbors(Graph, Thing) := (G,v) -> set(1..#G) - neighbors(G,v)-set{v}


--------------------
-- Documentation  --
--------------------

beginDocumentation()

doc ///
  Key
    Graphs
  Headline
    Data types and basic functions on graphs used in algebra and algebraic geometry. 
  Description
    Text
      This package is used to construct graphs. 
///

end