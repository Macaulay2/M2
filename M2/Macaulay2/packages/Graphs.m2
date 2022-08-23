 -*
Copyright 2010 Amelia Taylor and Augustine O'Keefe.
You may redistribute this file under the terms of the GNU General Public
License as published by the Free Software Foundation, either version 2 of
the License, or any later version.


Copyright 2014: Jack Burkart, David Cook II, Caroline Jansen
You may redistribute this file under the terms of the GNU General Public
License as published by the Free Software Foundation, either version 2
of the License, or any later version.
*-
------------------------------------------
------------------------------------------
-- To Do List
------------------------------------------
------------------------------------------
-- Add more documentation
-- Add tests

------------------------------------------
------------------------------------------
-- Header
------------------------------------------
------------------------------------------

newPackage (
    "Graphs",
        Version => "0.3.4",
        Date => "May 15, 2021",
        Authors => {
            {Name => "Jack Burkart", Email => "jburkar1@nd.edu"},
            {Name => "David Cook II", Email => "dcook.math@gmail.com", HomePage => "http://ux1.eiu.edu/~dwcook/"},
            {Name => "Caroline Jansen", Email => "cjansen@alumni.nd.edu"},
            {Name => "Amelia Taylor", Email => "originalbrickhouse@gmail.com"},
            {Name => "Augustine O'Keefe", Email => "aokeefe@tulane.edu"},
            {Name => "Contributors of note: Carlos Amendola, Alex Diaz, Luis David Garcia Puente, Roser Homs Pons, Olga Kuznetsova,  Shaowei Lin, Sonja Mapes, Harshit J Motwani, Mike Stillman, Doug Torrance"}
        },
        Headline => "graphs and directed graphs (digraphs)",
	Keywords => {"Graph Theory"},
        Configuration => {
            "DotBinary" => "dot"
            },
	PackageImports => { "PrimaryDecomposition" },
        PackageExports => {
            "SimplicialComplexes"
            },
        DebuggingMode => false
        )

-- Load configurations
graphs'DotBinary = if instance((options Graphs).Configuration#"DotBinary", String) then (options Graphs).Configuration#"DotBinary" else "dot";

-- Exports
export {
    --
    -- Data type & constructor
    "Digraph",
    "Graph",
    "digraph",
    "graph",
        "EntryMode",
        "Singletons",
    --
    -- Basic data
    "adjacencyMatrix",
    "degreeMatrix",
    "degreeSequence",
    "edges",
    "incidenceMatrix",
    "laplacianMatrix",
    "simpleGraph",
    "vertexSet",
  --"vertices",
    --
    -- Display Methods
    "displayGraph",
    "showTikZ",
    "writeDotFile",
    --
    -- Derivative graphs
    "barycenter",
    "complementGraph",
    "digraphTranspose",
    "lineGraph",
    "underlyingGraph",
    --
    -- Enumerators
    "barbellGraph",
    "circularLadder",
    "cocktailParty",
    "completeGraph",
    "completeMultipartiteGraph",
    "crownGraph",
    "cycleGraph",
    "doubleStar",
    "friendshipGraph",
    "generalizedPetersenGraph",
    "graphLibrary",
    "kneserGraph",
    "ladderGraph",
    "lollipopGraph",
    "monomialGraph",
    "pathGraph",
    "prismGraph",
    "rattleGraph",
    "starGraph",
    "thresholdGraph",
    "wheelGraph",
    "windmillGraph",
    --
    -- Cut properties
    "edgeConnectivity",
    "edgeCuts",
    "minimalVertexCuts",
    "vertexConnectivity",
    "vertexCuts",
    --
    -- Properties
    "breadthFirstSearch",
        "discoveryTime",
        "finishingTime",
    "BFS",
    "center",
    "children",
    "chromaticNumber",
    "cliqueComplex",
    "cliqueNumber",
    "closedNeighborhood",
    "clusteringCoefficient",
    "coverIdeal",
    "criticalEdges",
    "degeneracy",
    "degreeCentrality",
    "degreeIn",
    "degreeOut",
    "density",
    "depthFirstSearch",
    "DFS",
    "descendants",
    "descendents",
    "distance",
    "distanceMatrix",
    "eccentricity",
    "edgeIdeal",
    "expansion",
    "findPaths",
    "floydWarshall",
    "forefathers",
    "foreFathers",
    "girth",
    "independenceComplex",
    "independenceNumber",
    "leaves",
    "lowestCommonAncestors",
    "minimalDegree",
    "neighbors",
    "nondescendants",
    "nondescendents",
    "nonneighbors",
    "numberOfComponents",
    "numberOfTriangles",
    "parents",
    "radius",
    "reachable",
    "reverseBreadthFirstSearch",
    "sinks",
    "sources",
    "spectrum",
    "vertexCoverNumber",
    "vertexCovers",
    --
    -- Boolean properties
    "hasEulerianTrail",
    "hasOddHole",
    "isBipartite",
    "isCM",
    "isChordal",
    "isConnected",
    "isCyclic",
    "isEulerian",
    "isForest",
    "isLeaf",
    "isPerfect",
    "isReachable",
    "isRegular",
    "isRigid",
    "isSimple",
    "isSink",
    "isSource",
    "isStronglyConnected",
    "isTree",
    "isWeaklyConnected",
    --
    -- Graph operations
    "cartesianProduct",
    "disjointUnion",
    "graphComposition",
    "graphPower",
    "lexicographicProduct",
    "strongProduct",
    "tensorProduct",
    --
    -- Graph manipulations
    "addEdge",
    "addEdges'",
    "addVertex",
    "addVertices",
    "bipartiteColoring",
    "deleteEdges",
    "deleteVertex",
    "deleteVertices",
    "indexLabelGraph",
    "inducedSubgraph",
    "reindexBy",
    "removeNodes",
    "spanningForest",
    "vertexMultiplication",
    
      -- "LabeledGraph",
    --"labeledGraph",
     "topologicalSort",
    "topSort",
    "SortedDigraph",
    "newDigraph"
    }

------------------------------------------
------------------------------------------
-- Methods
------------------------------------------
------------------------------------------

------------------------------------------
-- Non-exported functions
------------------------------------------

runcmd := cmd -> (
     stderr << "-- running: " << cmd << endl;
     r := run cmd;
     if r != 0 then error("-- command failed, error return code ", r);
     )

------------------------------------------
-- Data type & constructor
------------------------------------------
Digraph = new Type of HashTable
Graph = new Type of Digraph

Digraph.synonym = "digraph"
Graph.synonym = "graph"

digraph = method(Options => {symbol Singletons => null, symbol EntryMode => "auto"})
digraph List := Digraph => opts -> L -> (
    mode := if #L == 0 or opts.EntryMode == "edges" then "e" 
        else if opts.EntryMode == "neighbors" then "n"
        else if opts.EntryMode == "auto" then (if all(#L, i -> instance(L_i_1, List)) then "n" else "e")
        else error "EntryMode must be 'auto', 'edges', or 'neighbors'.";
    if mode == "e" then digraph(unique flatten (toList \ L), L, Singletons => opts.Singletons, EntryMode => "edges")
    else digraph(hashTable apply(L, x -> x_0 => toList(x_1)), Singletons => opts.Singletons, EntryMode => "neighbors")
    )
digraph HashTable := Digraph => opts -> g -> digraph(unique join(keys g, flatten (toList \ values g)), flatten apply(keys g, v -> apply(toList g#v, u -> {v, u})), Singletons => opts.Singletons, EntryMode => "edges")
digraph (List, List) := Digraph => opts -> (V, L) -> (
    mode := if #L == 0 or opts.EntryMode == "edges" then "e" 
        else if opts.EntryMode == "neighbors" then "n"
        else if opts.EntryMode == "auto" then (if all(#L, i -> instance(L_i_1, List)) then "n" else "e")
        else error "EntryMode must be 'auto', 'edges', or 'neighbors'.";
    E := if mode == "e" then toList \ L else flatten apply(L, N -> apply(N_1, v -> {N_0, v}));
    if not isSubset(flatten E, V) then error "There are edges with vertices outside the vertex set.";
    V = unique join(V, if instance(opts.Singletons, List) then opts.Singletons else {});
    A := if #V == 0 then map(ZZ^0, ZZ^0, 0) else matrix apply(#V, i -> apply(#V, j -> if member({V#i, V#j}, E) then 1 else 0));
    digraph(V, A)
    )
digraph (List, Matrix) := Digraph => opts -> (V, A) -> (
    if ( sort unique join( {0,1}, flatten entries A) != {0,1} ) then error "The given matrix is not an adjacency matrix.";
    if #V != numrows A or numrows A != numcols A then error "The given vertex set and matrix are incompatible.";
    V' := if instance(opts.Singletons, List) then opts.Singletons - set V else {};
    A' := matrix {{A, map(ZZ^(#V), ZZ^(#V'), 0)}, {map(ZZ^(#V'), ZZ^(#V), 0), 0}};
    new Digraph from {
        symbol vertexSet => join(V, V'),
        symbol adjacencyMatrix => A',
        symbol cache => new CacheTable from {}}
    )
digraph Matrix := Digraph => opts -> A -> digraph(toList(0..<numRows A), A, opts)

graph = method(Options => {symbol Singletons => null, symbol EntryMode => "auto"})
graph (List, List) := Graph => opts -> (V, L) -> (
    mode := if #L == 0 or opts.EntryMode == "edges" then "e" 
        else if opts.EntryMode == "neighbors" then "n"
        else if opts.EntryMode == "auto" then (if all(#L, i -> instance(L_i_1, List)) then "n" else "e")
        else error "EntryMode must be 'auto', 'edges', or 'neighbors'.";
    E := if mode == "e" then toList \ L else flatten apply(L, N -> apply(N_1, v -> {N_0, v}));
    if not isSubset(flatten E, V) then error "There are edges with vertices outside the vertex set.";
    V = unique join(V, if instance(opts.Singletons, List) then opts.Singletons else {});
    A := if #V == 0 then map(ZZ^0, ZZ^0, 0) else matrix apply(#V, i -> apply(#V, j -> if member({V#i, V#j}, E) or member({V#j, V#i}, E) then 1 else 0));
    graph(V, A)
    )
graph List := Graph => opts -> L -> (
    mode := if #L == 0 or opts.EntryMode == "edges" then "e" 
        else if opts.EntryMode == "neighbors" then "n"
        else if opts.EntryMode == "auto" then (if all(#L, i -> instance(L_i_1, List)) then "n" else "e")
        else error "EntryMode must be 'auto', 'edges', or 'neighbors'.";
    if mode == "e" then graph(unique flatten (toList \ L), L, Singletons => opts.Singletons, EntryMode => "edges")
    else graph(hashTable apply(L, x -> x_0 => toList(x_1)), Singletons => opts.Singletons, EntryMode => "neighbors")
    )
graph HashTable := Graph => opts -> g -> graph(unique join(keys g, flatten (toList \ values g)), flatten apply(keys g, v -> apply(toList g#v, u -> {v, u})), Singletons => opts.Singletons, EntryMode => "edges")
graph (List, Matrix) := Graph => opts -> (V,A) -> (
    if ( sort unique join( {0,1}, flatten entries A) != {0,1} ) then error "The given matrix is not an adjacency matrix.";
    if #V != numrows A or numrows A != numcols A then error "The given vertex set and matrix are incompatible.";
    V' := if instance(opts.Singletons, List) then opts.Singletons - set V else {};
    A' := matrix {{A, map(ZZ^(#V), ZZ^(#V'), 0)}, {map(ZZ^(#V'), ZZ^(#V), 0), 0}};
    new Graph from {
        symbol vertexSet => join(V, V'),
        symbol adjacencyMatrix => A',
        symbol cache => new CacheTable from {}}
        )
graph Matrix := Graph => opts -> A -> graph(toList(0..<numrows A), A)

graph Digraph := HashTable => opts -> D -> (
    V := vertexSet D;
    A := adjacencyMatrix D;
    hashTable apply(#V, i -> V_i => V_(positions(first entries A^{i}, j -> j != 0)))
    )

Digraph _ ZZ := Thing => (D, i) -> D.vertexSet#i
Digraph _ List := List => (D, L) -> D.vertexSet_L
installMethod(symbol _*, Digraph, D -> D.vertexSet)

net Digraph := Net => G -> (
    V := vertexSet G;
    A := adjacencyMatrix G;
    H := hashTable apply(#V, i -> V_i => V_(positions(first entries A^{i}, j -> j != 0)));
    horizontalJoin flatten (
        net class G,
        "{",
        stack (horizontalJoin \ sort apply(pairs H, (k,v) -> (net k, " => ", net v))),
        "}"
    ))

toString Digraph := String => D -> (
       concatenate( -- issue #1473 in github
 --   horizontalJoin(
        toLower toString class D, 
        " (",
        toString vertexSet D,
        ", ",
        toString (toList \ edges D),
        ")"
        )
    )

------------------------------------------
-- Basic data
------------------------------------------

adjacencyMatrix = method()
adjacencyMatrix Digraph := Matrix => D -> D.adjacencyMatrix

degree (Graph,Thing) := ZZ => (G,v) -> #neighbors(G,v)
degree (Digraph,Thing) := ZZ => (D,v) -> #(children(D,v) + parents(D,v))

degreeMatrix = method()
degreeMatrix Digraph := Matrix => G -> diagonalMatrix apply(entries transpose adjacencyMatrix G, a -> #positions(a, j -> j != 0))

degreeSequence = method()
degreeSequence Graph := List => G -> rsort \\ sum \ entries adjacencyMatrix G

edges = method()
edges Digraph := List => D -> (
    V := vertexSet D;
    A := adjacencyMatrix D;
    flatten for i from 0 to #V - 1 list for j from 0 to #V - 1 list if A_(i,j) == 1 then {V#i, V#j} else continue
    )
edges Graph := List => G -> (
    V := vertexSet G;
    A := adjacencyMatrix G;
    flatten for i from 0 to #V - 1 list for j from i+1 to #V - 1 list if A_(i,j) == 1 or A_(j,i) == 1 then set {V#i, V#j} else continue
    )

incidenceMatrix = method()
incidenceMatrix Graph := Matrix => G -> matrix apply(vertexSet G, v -> apply(edges G, e -> if member(v, e) then 1 else 0))

laplacianMatrix = method()
laplacianMatrix Graph := Matrix => G -> degreeMatrix G - adjacencyMatrix G

simpleGraph = underlyingGraph

vertexSet = method()
vertexSet Digraph := List => D -> D#(symbol vertexSet)
vertices Digraph := List => D -> D#(symbol vertexSet)

-------------------------------------------
-- Display Methods
-------------------------------------------

displayGraph = method()
displayGraph (String, String, Digraph) := (dotfilename, jpgfilename, G) -> (
     writeDotFile(dotfilename, G);
     runcmd(graphs'DotBinary  | " -Tjpg " | dotfilename | " -o " | jpgfilename);
     show URL("file://" | toAbsolutePath jpgfilename);
     )
displayGraph (String, Digraph) := (dotfilename, G) -> (
     jpgfilename := temporaryFileName() | ".jpg";
     displayGraph(dotfilename, jpgfilename, G);
     )
displayGraph Digraph := G -> (
     dotfilename := temporaryFileName() | ".dot";
     displayGraph(dotfilename, G);
     )

showTikZ = method(Options => {Options=>"-t math --prog=dot -f tikz --figonly"})
showTikZ Digraph := opt -> G -> (
     dotfilename := temporaryFileName() | ".dot";
     writeDotFile(dotfilename, G);
     output := temporaryFileName();
     runcmd("dot2tex "|opt#Options|" "|dotfilename|" >> "|output);
     get output
     )

html Digraph := G -> if G.cache#?"svg" then G.cache#"svg" else (
     dotfilename := temporaryFileName() | ".dot";
     writeDotFile(dotfilename, G);
     svgfilename := temporaryFileName() | ".svg";
     runcmd(graphs'DotBinary  | " -Tsvg " | dotfilename | " -o " | svgfilename);
     G.cache#"svg" = get svgfilename
     )

writeDotFile = method()
writeDotFile (String, Graph) := (filename, G) ->
    writeDotFileHelper(filename, G, "graph", "--")
writeDotFile (String, Digraph) := (filename, G) ->
    writeDotFileHelper(filename, G, "digraph", "->")

writeDotFileHelper = (filename, G, type, op) -> (
    fil := openOut filename;
    fil << type << " G {" << endl;
    V := vertexSet G;
    I := hashTable apply(#V, i -> V_i => i);
    scan(V, v -> fil << "\t" << I#v << " [label=\"" << toString v << "\"];" << endl);
    E := toList \ edges G;
    scan(E, e -> fil << "\t" << I#(e_0) << " " << op << " " << I#(e_1) << ";"
	<< endl);
    fil << "}" << endl << close;
    )

------------------------------------------
-- Derivative graphs
------------------------------------------
barycenter = method()
barycenter Graph := Graph => G -> inducedSubgraph(G, center G)

complementGraph = method()
complementGraph Graph := Graph => G -> graph(vertexSet G, subsets(vertexSet G, 2) - set(join(toList \ edges G, reverse@@toList \ edges G)), EntryMode => "edges")

digraphTranspose = method()
digraphTranspose Digraph := Digraph => D -> digraph(vertexSet D, reverse \ edges D, EntryMode => "edges")


lineGraph = method()
lineGraph Graph := Graph => (G) -> (
   E:=edges(G);
   if #E==0 then return graph({});
   EE:={};
   for e in E do (
      for f in E do (
         if not e===f then (
            if #(e*f)>0 then (
               EE=EE|{{e,f}};
               ); 
            );   
         ); 
      );
   --non-singletons
   nS:=unique flatten EE;
   --singletons
   S:=for e in E list if member(e,nS)===false then e else continue;
   return graph(EE,Singletons=>S);
)


underlyingGraph = method()
underlyingGraph Digraph := Graph => D -> graph(vertexSet D, edges D, EntryMode => "edges")

------------------------------------------
-- Enumerators
------------------------------------------

barbellGraph = method()
barbellGraph ZZ := Graph => n -> (
    V := toList(0..<2*n);
    E := select(flatten flatten apply(toList(0..<n), i -> apply(toList(0..<n), j -> {{i,j},{i+n,j+n}})), e -> e_0 != e_1) | {{n-1,n}};
    graph(V,E, EntryMode => "edges")
    )

circularLadder = method()
circularLadder ZZ := Graph => n -> generalizedPetersenGraph (n,1)

cocktailParty = method()
cocktailParty ZZ := Graph => n -> (
    V := toList(0..<2*n);
    E := subsets(2*n,2) - set apply(toList(0..<n), i -> {i, i+n});
    graph(V,E, EntryMode => "edges")
    )

completeGraph = method()
completeGraph ZZ := Graph => n -> graph(toList(0..<n), subsets(n, 2), EntryMode => "edges")

completeMultipartiteGraph = method()
completeMultipartiteGraph List := Graph => P -> (
    if #P == 1 then return graph(toList(0..<P_0), {});
    offset := 0;
    V := apply(sort P, p -> ( ret := toList(offset..(offset+p-1)); offset = offset + p; ret ));
    graph (flatten apply(#V-1, i -> flatten toList apply(i+1..#V-1, j -> toList \ toList((set V_i) ** (set V_j)))), EntryMode => "edges")
    )

crownGraph = method()
crownGraph ZZ := Graph => n -> (
    V := toList(0..<2*n);
    E := select(flatten apply(toList(0..<n), i -> apply(toList(0..<n), j -> {i, j+n})), e -> e_0 + n != e_1);
    graph (V,E, EntryMode => "edges")
    )

cycleGraph = method()
cycleGraph ZZ := Graph => n -> graph(toList(0..<n), prepend({0,n-1}, apply(n-1, i -> {i, i+1})), EntryMode => "edges")

doubleStar = method()
doubleStar (ZZ,ZZ) := Graph => (m,n) -> (
    V := toList(0..m+n+1);
    E := apply(toList(1..n), i -> {0,i}) | apply(toList(n+2..m+n+1), i -> {n+1, i}) | {{0, n+1}};
    graph (V,E, EntryMode => "edges")
)

friendshipGraph = method()
friendshipGraph ZZ := Graph => n -> windmillGraph(3, n)

generalizedPetersenGraph = method()
generalizedPetersenGraph (ZZ, ZZ) := Graph => (n, k) -> (
    V := toList (0..2*n-1);
    E := flatten apply(n, l -> {{l,(l+1) % n}, {l,l+n}, {l+n,n + ((l+k) % n)}});
    graph(V, E, EntryMode => "edges")
    )

graphLibrary = method()
graphLibrary String := Graph => s -> (
    s = toLower s;
    if s == "petersen" then return generalizedPetersenGraph(5,2);
    if s == "bidiakis cube" then return addEdges'(cycleGraph 12, {{0,6},{1,5},{7,11},{2,10},{3,9},{4,8}});
    if s == "desargues" then return generalizedPetersenGraph(10,3);
    if s == "dodecahedron" then return generalizedPetersenGraph(10,2);
    if s == "durer" then return generalizedPetersenGraph(6,2);
    if s == "claw" then return starGraph 4;
    if s == "cubical" then return circularLadder 4;
    if s == "f26a" then return addEdges'(cycleGraph 26, apply(select(toList(0..24),even), i -> {i,(i+7) % 26}));
    if s == "franklin" then return addEdges'(cycleGraph 12, apply(toList(0..5), i -> if even i then {i, i+7} else {i, i+5}));
    if s == "chvatal" then return addEdges'(cycleGraph 12, apply({0,1,2,5}, i -> {i,i+6}) | apply({0,1,2,6,7,8}, i -> {i,i+3}) | {{3,10},{4,9}});
    if s == "heawood" then return addEdges'(cycleGraph 14, apply(select(toList(0..12),even), i -> {i, i+5 % 14}));
    if s == "paw" then return addEdge(starGraph 4, set {2,3});
    if s == "mobius" then return generalizedPetersenGraph (8,3);
    if s == "nauru" then return generalizedPetersenGraph (12,5);
    if s == "kite" then return addEdge(cycleGraph 4, set {0,2});
    if s == "house" then return addEdge(cycleGraph 5; set {1,3});
    if s == "bull" then return addEdge(pathGraph 5, set {1,3});
    if s == "bowtie" then return friendshipGraph 2;
    if s == "dart" then return addEdges'(addVertex(cycleGraph 4, 4),{{0,2},{2,4}});
    )

kneserGraph = method()
kneserGraph (ZZ,ZZ) := Graph => (n,k) -> (
    S := subsets(n,k);
    L := for i from 0 to (#S-1) list
        for j from i+1 to (#S-1) list
            if #((set S_i)*(set S_j)) == 0 then {i,j} else continue;
    graph(toList(0..<#S), flatten L, EntryMode => "edges")
    )

ladderGraph = method()
ladderGraph ZZ := Graph => n -> (
    A := adjacencyMatrix pathGraph n;
    B := map(ZZ^n, ZZ^n, 1);
    C := matrix {{A, B}, {transpose B, A}};
    graph C
    )

lollipopGraph = method()
lollipopGraph (ZZ, ZZ) := Graph => (m,n) -> (
    V := toList (0..n+m-1);
    E := subsets(m,2) | apply(toList(m-1..m+n-2), i -> {i, i+1});
    graph(V,E, EntryMode => "edges")
    )

monomialGraph = method()
monomialGraph (MonomialIdeal, ZZ) := Graph => (I, d) -> (
    V := first entries lift(basis(d, quotient I),ring I);
    E := {};
    for v in V do(
        L := select(V, i -> first degree lcm (v,i) == d+1);
        E = E | apply(L, i -> {v,i});
        );
    E = unique E;
    graph(V,E, EntryMode => "edges")
    )

pathGraph = method()
pathGraph ZZ := Graph => n -> graph (apply(n-1, i -> {i, i+1}), EntryMode => "edges")

prismGraph = circularLadder

rattleGraph = method()
rattleGraph (ZZ, ZZ) := Graph => (m,n) -> (
    V := toList (0..n+m-1);
    E := apply(toList(0..m-2), i -> {i,i+1}) | {{0,m-1}} | apply(toList(m-1..m+n-2), i -> {i, i+1});
    graph(V,E, EntryMode => "edges")
    )

starGraph = method()
starGraph ZZ := Graph => n -> windmillGraph(2, n)

thresholdGraph = method()
thresholdGraph List := Graph => L ->(
    n := #L+1;
    P := positions(L, i -> i == 1); -- assumes other positions are 0
    E := if P === null then {} else flatten apply(P, i -> apply(i+1, j -> (j, i+1)));
    graph(toList(0..<n), E, EntryMode => "edges")
    )

wheelGraph = method()
wheelGraph ZZ := Graph => n -> (
    spokes := apply(toList(1..n-1), i -> {0,i});
    outside := apply(toList(1..n-2), i -> {i,i+1}) | {{1,n-1}};
    graph(toList(0..<n), join(spokes,outside), EntryMode => "edges")
    )

windmillGraph = method()
windmillGraph (ZZ,ZZ) := Graph => (k,d) -> (
    E := apply(subsets(k, 2), s -> (
        apply(d, i -> (
            apply(s, j -> if j != 0 then j + i*(k-1) else 0)
            ))
        ));
    graph(flatten E, EntryMode => "edges")
    )

------------------------------------------
-- Cut properties
------------------------------------------

edgeConnectivity = method()
edgeConnectivity Graph := ZZ => G -> #first edgeCuts G

edgeCuts = method()
edgeCuts Graph := List => G -> (
    if #edges G == 0 then return {{}};
    E := toList \ edges G;
    EC := {};
    for i from 0 to #E - 1 do (
        possibleSubsets := subsets(E, i);
        EC = for x in possibleSubsets list (
            G' := deleteEdges(G,x);
            if not isConnected G' then x else continue
            );
        if #EC != 0 then break;
        );
    EC
    )

minimalVertexCuts = method()
minimalVertexCuts Graph := List => G -> (
    V := vertexSet G;
    VC := {};
    for i from 0 to #V-1 do (
        possibleSubsets := subsets(V,i);
        VC = for x in possibleSubsets list (
            if not isConnected deleteVertices(G, x) then x else continue
            );
        if #VC != 0 then break;
        );
    VC
)

minimalDegree = method()
minimalDegree Graph := ZZ => G -> (
   return min for v in vertexSet(G) list degree(G,v);
)

vertexConnectivity = method()
--returns n-1 for K_n as suggested by West
vertexConnectivity Graph := ZZ => G -> (
if #(vertexSet G)==0 then return 0;
if cliqueNumber G == #(vertexSet G) then (
    return #(vertexSet G) - 1;
    ) else (
    return #(first minimalVertexCuts G);
    );
)

vertexCuts = method()
--West does not specify, but Wikipedia does, that K_n has no vertex cuts.  
--The method currently returns an empty list, which is technically correct.
vertexCuts Graph := List => G -> (
    V := vertexSet G;
    possibleSubsets := drop(subsets V, -1);
    for x in possibleSubsets list (
        G' := deleteVertices(G,x);
        if not isConnected G' then x else continue
        )
    )

------------------------------------------
-- Properties
------------------------------------------

breadthFirstSearch = method()
breadthFirstSearch (Digraph, Thing) := List => (G, v) -> (
    V := vertexSet G;
    if not member(v, V) then error "";
    Q := {{v, 0}};
    V = V - set {v};
    i := 0;
    while i < #Q do (
        C := select(toList children(G, first Q_i), u -> member(u, V));
        Q = Q | apply(C, c -> {c, last Q_i + 1});
        V = V - set C;
        i = i + 1;
        );
    P := partition(last, Q);
    apply(sort keys P, k -> first \ P#k)
    )

BFS = breadthFirstSearch

center = method()
center Graph := List => G -> select(vertexSet G, i -> eccentricity(G, i) == radius G)

children = method()
children (Digraph, Thing) := Set => (G, v) -> (
    i := position(vertexSet G, u -> u === v);
    if i === null then error "v is not a vertex of G.";
    set (vertexSet G)_(positions(first entries (adjacencyMatrix G)^{i}, j -> j != 0))
    )


chromaticNumber = method()
chromaticNumber Graph := ZZ => G -> (
    if #edges G == 0 and #vertexSet G == 0 then 0
    else if #edges G == 0 and #vertexSet G != 0 then 1
    else if isBipartite G then 2
    else (
        chi := 3;
        J := coverIdeal G;
        m := product gens ring J;
        while ((m^(chi - 1) % J^chi) != 0) do chi = chi+ 1;
        chi
        )
    )

cliqueComplex = method()
cliqueComplex Graph := SimplicialComplex => G -> simplicialComplex edgeIdeal complementGraph G

cliqueNumber = method()
cliqueNumber Graph := ZZ => G -> independenceNumber complementGraph G

closedNeighborhood = method()
closedNeighborhood (Graph, Thing) := Set => (G,a) -> neighbors(G,a) + set {a}

clusteringCoefficient = method()
clusteringCoefficient Graph := QQ => G -> (
    Cv := for v in vertexSet G list clusteringCoefficient(G,v);
    sum Cv / #Cv
    )
clusteringCoefficient (Graph, Thing) := QQ => (G,v) -> (
    N := toList neighbors(G,v);
    if #N == 0 or #N == 1 then return 0;
    G' := inducedSubgraph(G, N);
    2 * #edges G' / (#N * (#N - 1))
    )

-- the 'conneectedComponents' methods is defined in 'SimplicialComplexies'
connectedComponents Graph := List => G -> (
    V := vertexSet G;
    while #V != 0 list (
        C := {first V};
        i := 0;
        while i!= #C do (
            N := toList neighbors(G, C_i);
            C = unique(C | N);
            V = V - set C;
            i = i + 1;
            if #V == 0 then break;
            );
        C
        )
    )

coverIdeal = method()
coverIdeal Graph := Ideal => G -> dual edgeIdeal G

criticalEdges = method()
criticalEdges Graph := List => G -> (
    J := edgeIdeal G;
    isSquarefree := x -> all(first exponents x, i -> i <= 1);
    indSets := select(first entries basis(1,quotient J), isSquarefree);
    i := 2;
    while i >= 2 do (
        potential := select(first entries basis(i,quotient J), isSquarefree);
        if #potential != 0 then indSets = potential else break;
        i = i + 1;
        );
    indSets = indices \ indSets;
    A := adjacencyMatrix G;
    V := vertexSet G;
    E := flatten for i from 0 to #V - 1 list for j from i+1 to #V - 1 list if A_(i,j) == 1 or A_(j,i) == 1 then {i,j} else continue;
    nbrs := v -> positions(first entries (adjacencyMatrix G)^{v}, j -> j != 0);
    iE := select(E, e -> any (indSets, A -> (member(e_0, A) and #((set nbrs e_1)*set A) == 1) or (member(e_1, A) and #((set nbrs e_0)*set             A) == 1)));
    apply(iE, e -> set V_e)
    )

degeneracy = method()
degeneracy Graph := ZZ => G -> (
    nbrs := v -> positions(first entries (adjacencyMatrix G)^{v}, j -> j != 0);
    n := #vertexSet G;
    L := {};
    dV := new MutableHashTable from apply(n, i -> i => #nbrs i);
    D := apply(n, j -> select(n, i -> dV#i == j));
    k := 0;
    for l from 1 to n do (
        i := position(D, j -> #j != 0);
        k = max(k, i);
        v := first D_i;
        L = append(L, v);
        D = replace(i, drop(D_i, 1), D);
        scan(nbrs v, w -> dV#w = dV#w - 1);
        V' := toList(0..n-1) - set L;
        D = apply(n, j -> select(V', i -> dV#i == j));
        );
    k
    )

degreeCentrality = method()
degreeCentrality (Graph, Thing) := QQ => (G, v) -> degree(G, v)/(2*#edges G)

degreeIn = method()
degreeIn (Digraph,Thing) := ZZ => (D,v) -> #parents(D,v)

degreeOut = method()
degreeOut (Digraph,Thing) := ZZ => (D,v) -> #children(D,v)

density = method()
density Graph := QQ => G -> (
    E := #edges G;
    V := #vertexSet G;
    2*E / (V * (V - 1))
    )

depthFirstSearch = method()
depthFirstSearch Digraph := HashTable => G -> (
    V := vertexSet G;
    Q := {};
    discovery := new MutableHashTable from apply(V, v -> v => 0);
    finishing := new MutableHashTable from apply(V, v -> v => -1);
    parent := new MutableHashTable from apply(V, v -> v => null);
    t := 0;
    -- V maintains the vertexSet that have yet to be queued
    while #V != 0 do (
        Q = {V_0};
        V = drop(V, 1);
        -- While the queue is not empty...
        while #Q != 0 do (
            v := Q_0;
            Q = drop(Q, 1);
            t = t + 1;
            discovery#v = t;
            -- Find the unqueued children of v; mark v as their 'parent'
            C := select(toList children(G, v), u -> member(u, V));
            scan(C, u -> parent#u = v);
            -- If all children have been queued, then we are finished,
            -- as are all 'forefathers' of v that have no children in the queue.
            if #C == 0 then (
                while v =!= null and (#Q == 0 or parent#(first Q) =!= v) do (
                    t = t + 1;
                    finishing#v = t;
                    v = parent#v;
                    );
                )
            else (
                Q = C | Q;
                V = V - set C;
                );
            ); -- while #Q != 0
    ); -- while #V != 0
    hashTable{symbol discoveryTime => new HashTable from discovery, symbol finishingTime => new HashTable from finishing}
    )

DFS = depthFirstSearch

descendants = method()
descendants (Digraph, Thing) := Set => (D,v) -> set flatten breadthFirstSearch(D, v)
descendents = descendants

diameter Graph := ZZ => G -> (
    allEntries := flatten entries distanceMatrix G;
    if member(-1, allEntries) then infinity else max allEntries
    )

distance = method()
distance (Digraph, Thing, Thing) := ZZ => (G,v,u) -> (
    if not member(v, vertexSet G) or not member(u, vertexSet G) then error "The given vertexSet are not vertexSet of G.";
    n := #vertexSet G;
    v = position(vertexSet G, i -> i == v);
    u = position(vertexSet G, i -> i == u);
    C := new MutableList from toList(#vertexSet G:infinity);
    Q := {v};
    C#v = 0;
    while #Q != 0 do (
        y := first Q;
        Q = drop(Q, 1);
        N := select(positions(first entries (adjacencyMatrix G)^{y}, j -> j != 0), x -> C#x == infinity);
        if any(N, x -> x == u) then (
            C#u = C#y + 1;
            break;
            );
        Q = Q | N;
        for z in N do C#z = C#y + 1;
        );
    C#u
    )
distance (Digraph, Thing) := HashTable => (G, v) -> (
    if not member(v, vertexSet G) then error "The given vertex is not a vertex of G.";
    n := #vertexSet G;
    v = position(vertexSet G, i -> i === v);
    C := new MutableList from toList(#vertexSet G:infinity);
    Q := {v};
    C#v = 0;
    while #Q != 0 do (
        y := first Q;
        Q = drop(Q, 1);
        N := select(positions(first entries (adjacencyMatrix G)^{y}, j -> j != 0), x -> C#x == infinity);
        Q = Q | N;
        for z in N do C#z = C#y + 1;
        );
    hashTable apply(n, i -> (vertexSet G)_i => C#i)
    )

distanceMatrix = method()
distanceMatrix Digraph := Matrix => G -> (
    V := vertexSet G;
    matrix for i to #V - 1 list (
        H := distance(G, V_i);
        for j from 0 to #V -1 list (if H#(V_j) == infinity then -1 else H#(V_j))
        )
    )

eccentricity = method()
eccentricity (Graph, Thing) := ZZ => (G,v) ->(
    if isConnected G == false then error "Input graph must be connected";
    max apply(vertexSet G, i -> distance(G, v, i))
    )

edgeIdeal = method()
edgeIdeal Graph := Ideal => G -> (
    G = indexLabelGraph G;
    V := vertexSet G;
    x := local x;
    R := QQ(monoid[x_1..x_(#V)]);
    monomialIdeal (
        if #edges G == 0 then 0_R
        else apply(toList \ edges G, e -> R_(position(V, i -> i === e_0)) * R_(position(V, i -> i === e_1)))
        )
    )

expansion = method ()
expansion Graph := QQ => G -> (
   V:=set(vertexSet(G));
   E:=edges(G);
   --return 0 if graph is empty graph
   if #E===0 then return 0;
   n:=floor((#V)/2);
   --CS:={};
   RS:={};
   qq:=0;
   ee:=degree(G,(toList(V))_0);
   for i in 1..n do (
      for S in subsets(V,i) do (
           CS:=V-S;
           qq:=sum for e in edges(G) list if #(e*S)>0 and #(e*CS)>0 then 1 else 0;
           ee=min(ee,qq/#S);
           if(ee == qq) then RS=S;
           );
      );
   return ee;
)

findPaths = method()
findPaths (Digraph,Thing,ZZ) := List => (G,v,l) -> (
    if l < 0 then error "integer must be nonnegative";
    if l == 0 then {{v}}
    else(
        nbors := toList children (G,v);
        nPaths := apply(nbors, n -> findPaths(G,n,l-1));
        flatten apply(nPaths, P -> apply(P, p -> {v} | p))
    )
)

floydWarshall = method()
floydWarshall Digraph := HashTable => G -> (
    V := vertexSet G;
    D := new MutableHashTable from flatten apply(V, u -> apply(V, v-> (u,v) =>
        if u===v then 0 else if member(v, children(G,u)) then 1 else infinity));
    scan(V, w -> scan(V, u -> scan(V, v -> D#(u,v) = min(D#(u,v), D#(u,w) + D#(w,v)))));
    new HashTable from D
    )

forefathers = method()
forefathers (Digraph, Thing) := Set => (D,v) -> set flatten reverseBreadthFirstSearch(D,v)
foreFathers = forefathers

girth = method()
girth Graph := Thing => G -> (
    g := infinity;
    n := #vertexSet G;
    P := new MutableList from toList(n:0);
    D := new MutableList from toList(n:0);
    for v from 0 to n-1 do (
        S := {};
        R := {v};
        P#v = null;
        D#v = 0;
        while R != {} do(
            x := first R;
            S = append(S, x);
            R = drop(R, 1);
            L := positions(first entries (adjacencyMatrix G)^{x}, j -> j != 0) - set {P#x};
            for y in L do (
                if member(y, S) then
                    ( g = min {g, D#y + D#x + 1}; )
                else(
                    P#y = x;
                    D#y = 1 + D#x;
                    R = unique append(R, y);
                    );
                );
            );
        );
    g
    )

independenceComplex = method()
independenceComplex Graph := SimplicialComplex => G -> simplicialComplex edgeIdeal G

independenceNumber = method()
independenceNumber Graph := ZZ => G -> dim edgeIdeal G

leaves = method()
leaves Graph := List => G -> if not isTree G then error "input must be a tree" else select(vertexSet G, i -> degree(G, i) == 1)

lowestCommonAncestors = method()
lowestCommonAncestors (Digraph,Thing,Thing) := Thing => (D,u,v) -> (
    x := v;
    y := u;
    orderedVertices := flatten breadthFirstSearch(D, first vertexSet D);
    if position(orderedVertices, i -> i == u) >= position(orderedVertices, i -> i == v) then (
        x = u;
        y = v;
        );
    ancestX := reverseBreadthFirstSearch(D,x);
    ancestY := reverseBreadthFirstSearch(D,y);
    for i in ancestY do (
        for j in ancestX do (
            a := set i * set j;
            if #a != 0 then return toList a;
            );
        );
    {}
    )
highestCommonDescendant = method()
highestCommonDescendant(Digraph,Thing,Thing) := Thing => (D,u,v) -> (
    x := v;
    y := u;
    orderedVertices := flatten breadthFirstSearch(D, first vertexSet D);
    if position(orderedVertices, i -> i == u) >= position(orderedVertices, i -> i == v) then (
        x = u;
        y = v;
        );
    descendX := breadthFirstSearch(D,x);
    descendY := breadthFirstSearch(D,y);
    for i in descendY do (
        for j in descendX do (
            a := set i * set j;
            if #a != 0 then return toList a;
            );
        );
    {}
    )

neighbors = method()
neighbors (Graph, Thing) := Set => (G,v) -> (
    i := position(vertexSet G, u -> u === v);
    if i === null then error "v is not a vertex of G.";
    set (vertexSet G)_(positions(first entries (adjacencyMatrix G)^{i}, j -> j != 0))
    )

nondescendants = method()
nondescendants (Digraph, Thing) := Set => (D,v) -> set vertexSet D - (set {v} + descendants(D, v))
nondescendents = nondescendants

nonneighbors = method()
nonneighbors (Graph, Thing) := Set => (G,v) -> set vertexSet G - (set {v} + neighbors(G, v))

numberOfComponents = method()
numberOfComponents Graph := ZZ => G -> #connectedComponents G

numberOfTriangles = method()
numberOfTriangles Graph := ZZ => G -> number(ass (coverIdeal G)^2, i -> codim i == 3)

parents = method()
parents (Digraph, Thing) := Set => (G, v) -> (
    i := position(vertexSet G, u -> u === v);
    if i === null then error "v is not a vertex of G.";
    set (vertexSet G)_(positions(flatten entries (adjacencyMatrix G)_{i}, j -> j != 0))
    )

radius = method()
radius Graph := ZZ => G -> min apply(vertexSet G, i -> eccentricity(G,i))

reachable = method()
reachable (Digraph, List) := (D, A) -> unique flatten apply(A, v -> toList descendants(D, v))
reachable (Digraph, Set) := (D, A) -> set reachable(D, toList A)

reverseBreadthFirstSearch = method()
reverseBreadthFirstSearch (Digraph, Thing) := List => (G, v) -> (
    V := vertexSet G;
    if not member(v, V) then error "";
    Q := {{v, 0}};
    V = V - set {v};
    i := 0;
    while i < #Q do (
        C := select(toList parents(G, first Q_i), u -> member(u, V));
        Q = Q | apply(C, c -> {c, last Q_i + 1});
        V = V - set C;
        i = i + 1;
        );
    P := partition(last, Q);
    apply(sort keys P, k -> first \ P#k)
    )

sinks = method()
sinks Digraph := List => D -> select(vertexSet D, i -> isSink(D,i))

sources = method()
sources Digraph := List => D -> select(vertexSet D, i -> isSource(D, i))

spec = spectrum

spectrum = method()
spectrum Graph := List => G -> sort toList eigenvalues (adjacencyMatrix G, Hermitian => true)



topologicalSort = method(TypicalValue =>List)
topologicalSort Digraph := List => D -> topologicalSort(D, "")
topologicalSort (Digraph, String) := List => (D,s) -> (
    if instance(D, Graph) or isCyclic D then error "Topological sorting is only defined for acyclic directed graphs.";
    s = toLower s;
    processor := if s == "random" then random
        else if s == "min" then sort
        else if s == "max" then rsort
        else if s == "degree" then L -> last \ sort transpose {apply(L, v -> degree(D, v)), L}
        else identity;
    S := processor sources D;
    L := {};
    v := null;
    while S != {} do (
        v = S_0;
        L = L|{v};
        S = processor join(drop(S, 1), select(toList children (D, v), c -> isSubset(parents(D, c), L)));
        );
    L
    )




SortedDigraph = new Type of HashTable;

-- Keys:
--      digraph: the original digraph
--      NewDigraph: the digraph with vertices labeled as integers obtained from sorting
--      map: the map giving the sorted order

topSort = method(TypicalValue =>HashTable)
topSort Digraph := SortedDigraph => D ->  topSort(D,"") 
topSort (Digraph, String) := SortedDigraph => (D,s) -> ( 
L := topologicalSort (D,s);
g := graph D;
new SortedDigraph from {
digraph => D,
newDigraph => digraph hashTable apply(#L, i -> i + 1 => apply(toList g#(L_i), j -> position(L, k -> k == j) + 1)),
map => hashTable apply(#L, i -> L_i => i + 1)
}
)


vertexCoverNumber = method()
vertexCoverNumber Graph := ZZ => G -> min apply(vertexCovers G, i -> #i)

vertexCovers = method()
vertexCovers Graph := List => G -> (
    J := coverIdeal G;
    factoredIdealList := apply(J_*, indices);
    apply(factoredIdealList, i -> (vertexSet G)_i)
    )

weaklyConnectedComponents = method()
weaklyConnectedComponents Digraph := List => D -> connectedComponents underlyingGraph D

------------------------------------------
-- Boolean properties
------------------------------------------

hasEulerianTrail = method()
hasEulerianTrail Graph := Boolean => G -> (
    V := vertexSet G;
    V' := V - set select(V, v -> degree(G,v) == 0);
    oddDegrees := select(V, v -> odd (degree(G,v)));
    #oddDegrees <= 2 and isConnected inducedSubgraph(G,V')
    )
hasEulerianTrail Digraph := Boolean => G -> (
    V := vertexSet G;
    G' := underlyingGraph G;
    V' := V - set select(V, v -> degree(G',v) == 0);
    inMinusOut := {};
    outMinusIn := {};
    inDegrees := {};
    outDegrees := {};
    for v in V' do (
        outDeg := #children(G,v);
        inDeg := #parents(G,v);
        i := inDeg - outDeg;
        o := outDeg - inDeg;
        if i == 1 then inMinusOut = inMinusOut | {v};
        if o == 1 then outMinusIn = outMinusIn | {v};
        if i != 1 and o != 1 then (
            inDegrees = inDegrees | {inDeg};
            outDegrees = outDegrees | {outDeg};
            );
        );
    #inMinusOut <= 1 and #outMinusIn <= 1 and #(unique inDegrees) <= 1 and #(unique outDegrees) <= 1 and isConnected inducedSubgraph(G',V')
    )

hasOddHole = method()
hasOddHole Graph := Boolean => G -> any(ass (coverIdeal G)^2, i -> codim i > 3)

isBipartite = method()
isBipartite Graph := Boolean => G ->
    try bipartiteColoring G then true else false

isCM = method()
isCM Graph := Boolean => G -> (
    I := edgeIdeal G;
    codim I == pdim coker gens I
    )

isChordal = method()
isChordal Graph := Boolean => G -> (
    I := edgeIdeal complementGraph G;
    if I == ideal 0_(ring I) then true
    else (min flatten degrees I - 1) == regularity coker gens I
    )

isConnected = method()
isConnected Graph := Boolean => G -> numberOfComponents G <= 1

isCyclic = method()
isCyclic Graph := Boolean => G -> isConnected G and all(vertexSet G, v -> degree(G, v) == 2)
isCyclic Digraph := Boolean => G -> (
        D := depthFirstSearch G;
        any(vertexSet G, u ->
            any(toList children(G, u), v ->
                (D#symbol discoveryTime)#v < (D#symbol discoveryTime)#u and (D#symbol finishingTime)#u < (D#symbol finishingTime)#v
                )
            )
        )

isEulerian = method()
isEulerian Graph := Boolean => G -> all(apply(vertexSet G, v -> degree(G,v)), even) and isConnected G
isEulerian Digraph := Boolean => G -> (
    if #edges G == 0 then return false;
    V := vertexSet G;
    G' := underlyingGraph G;
    V' := V - set select(V, v -> degree(G',v) == 0);
    inDegree := #(parents(G, first V'));
    outDegree := #(children(G, first V'));
    all(V', v -> #parents(G, v) == inDegree) and all(V', v -> #children(G, v) == outDegree)    and isConnected inducedSubgraph(G', V')
    )

isForest = method()
isForest Graph := Boolean => G -> girth(G) == infinity

isLeaf = method()
isLeaf (Graph, Thing) := Boolean => (G,a) -> degree(G,a) == 1

isPerfect = method()
isPerfect Graph := Boolean => G -> not (hasOddHole G or hasOddHole complementGraph G)

isReachable = method()
isReachable (Digraph, Thing, Thing) := Boolean => (D,u,v) -> member(u, descendants(D,v))

isRegular = method()
isRegular Graph := Boolean => G -> (
    n := degree(G, first vertexSet G);
    all(drop(vertexSet G,1), v -> degree(G,v) == n)
    )


-- input: A graph G
-- output: Uses Laman's Theorem to determine if a graph is rigid or not
-- written by Tom Enkosky
--
isRigid = method();
isRigid Graph := G -> (
    local rigidity; local i; local j;
    
    rigidity=true;
    
    if #edges G < 2*#vertices G-3 then rigidity = false
    else (
	 for j from 2 to #vertices G-1 do(
	     for i in subsets(vertices G,j) do(
		 if #edges inducedSubgraph(G,i)>2*#i-3  then rigidity = false 
		 );
	     );
	 );
    return rigidity;
    )


isSimple = method()
isSimple Graph := Boolean => G -> (
    A := adjacencyMatrix G;
    all(toList(0..<numrows A), i -> A_(i,i) == 0)
)

isSink = method()
isSink (Digraph, Thing) := Boolean => (D,v) -> #children(D,v) == 0

isSource = method()
isSource (Digraph, Thing) := Boolean => (D,v) -> #parents(D,v) == 0

isStronglyConnected = method()
isStronglyConnected Digraph := Boolean => D -> all(unique flatten entries distanceMatrix D, i -> i>=0)

isTree = method()
isTree Graph := Boolean => G -> isConnected G and #edges G == #vertexSet G - 1

isWeaklyConnected = method()
isWeaklyConnected Digraph := Boolean => D -> isConnected underlyingGraph D

------------------------------------------
-- Graph operations
------------------------------------------

cartesianProduct = method()
cartesianProduct(Graph, Graph) := Graph => (G, H) -> (
    V := toList(set vertexSet G ** set vertexSet H);
    E := flatten for u in V list for v in V list
        if (u_0 == v_0 and member(set {u_1, v_1}, edges H))
        or (u_1 == v_1 and member(set {u_0, v_0}, edges G))
        then {u, v} else continue;
    graph(V, E, EntryMode => "edges")
    )

-- the 'directProduct' method is defined in 'Polyhedra'
directProduct(Graph,Graph) := Graph => (G, H) -> (
    V := toList(set vertexSet G ** set vertexSet H);
    E := flatten for u in V list for v in V list
        if member(set {u_0, v_0}, edges G) and member(set {u_1, v_1}, edges H)
        then {u, v} else continue;
    graph(V, E, EntryMode => "edges")
    )

disjointUnion = method()
disjointUnion List := Graph => L -> (
    if not all(L, G -> instance(G,Graph)) then error "must be a list of graphs";
    V := flatten for i to #L-1 list apply(vertexSet L_i, v -> {v, i});
    E := flatten for i to #L - 1 list apply(toList \ edges L_i, e -> {{e_0, i},{e_1, i}});
    graph(V, E, EntryMode => "edges")
    )

graphComposition = method()
graphComposition (Graph, Graph) := Graph => (G, H) -> (
    V := toList(set vertexSet G ** set vertexSet H);
    E := flatten for u in V list for v in V list
        if member(set {u_0, v_0}, edges G)
        or (u_0 == v_0 and member(set {u_1, v_1}, edges H))
        then {u, v} else continue;
    graph(V, E, EntryMode => "edges")
    )

graphPower = method()
graphPower (Graph, ZZ) := Graph => (G,k) -> (
    V := vertexSet G;
    E := flatten for i from 0 to #V-2 list (
        for j from i+1 to #V-1 list if distance(G,V_i, V_j) <= k then {V_i, V_j} else continue
        );
    graph(V, E, EntryMode => "edges")
    )

lexicographicProduct = graphComposition

strongProduct = method()
strongProduct (Graph, Graph) := Graph => (G, H) -> (
    V := toList \ toList(set vertexSet G ** set vertexSet H);
    E' := flatten for u in V list for v in V list
        if (u_0 == v_0 and member(set {u_1, v_1}, edges H))
        or (u_1 == v_1 and member(set {u_0, v_0}, edges G))
        then {u, v} else continue;
    E'' := flatten for u in V list for v in V list
        if member(set {u_0, v_0}, edges G) and member(set {u_1, v_1}, edges H)
        then {u, v} else continue;
    E := unique join(E', E'');
    graph(V, E, EntryMode => "edges")
    )

tensorProduct = directProduct

---------------------------
--Graph Manipulations
---------------------------
addEdge = method()
addEdge (Digraph, Set) := Graph => (G, s) -> addEdges'(G, {toList s})

addEdges' = method()
addEdges' (Graph, List) := Graph => (G, L) -> (
    A := mutableMatrix adjacencyMatrix G;
    while L != {} do(
        l := first L;
        e := apply(toList l, i -> position(vertexSet G, j -> j == i));
        f := sequence(first e, last e);
        A_f = 1;
        A_(reverse f) = 1;
        L = drop(L,1);
        );
    graph (vertexSet G, matrix A)
    )
addEdges' (Digraph, List) := Digraph => (G, L) -> (
    A := mutableMatrix adjacencyMatrix G;
    while L != {} do(
        l := first L;
        e := apply(toList l, i -> position(vertexSet G, j -> j == i));
        f := sequence(first e, last e);
        A_f = 1;
        L = drop(L,1);
        );
    digraph (vertexSet G, matrix A)
    )

addVertex = method()
addVertex (Digraph, Thing) := Digraph => (G, v) -> addVertices(G, {v})

addVertices = method()
addVertices (Graph, List) := Graph => (G, L) -> (
    L = L - set vertexSet G;
    n := #vertexSet G;
    m := #L;
    A := adjacencyMatrix G;
    B := map(ZZ^n, ZZ^m, 0);
    D := map(ZZ^m, ZZ^m, 0);
    A' := matrix {{A, B}, {transpose B, D}};
    graph(join(vertexSet G, L), A')
    )
addVertices(Digraph, List) := Graph => (G, L) -> (
    L = L - set vertexSet G;
    n := #vertexSet G;
    m := #L;
    A := adjacencyMatrix G;
    B := map(ZZ^n, ZZ^m, 0);
    D := map(ZZ^m, ZZ^m, 0);
    A' := matrix {{A, B}, {transpose B, D}};
    digraph (join(vertexSet G, L), A')
    )

bipartiteColoring = method()
bipartiteColoring Graph := List => G -> (
    n := # vertexSet G;
    v := 0;
    if n == 0 then return {{},{}};
    D := new MutableList from toList(n: infinity);
    while v != n do (
        uncolored := {position(toList D, i -> i == infinity)};
        D#(first uncolored) = 0;
        v = v + 1;
        while #uncolored != 0 do (
            x := first uncolored;
            uncolored = drop(uncolored, 1);
            N := positions(first entries (adjacencyMatrix G)^{x}, j -> j != 0);
            for y in N do (
                if D#y == infinity then (
                    D#y = 1 + D#x;
                    v = v + 1;
                    uncolored = append(uncolored, y);
                    ) else if (D#x - D#y) % 2 == 0 then
                        error "graph must be bipartite";
                );
            );
        );
    blue := positions(toList D, even);
    gold := toList(0..(n-1)) - set blue;
    {(vertexSet G)_blue, (vertexSet G)_gold}
    )

deleteEdges = method()
deleteEdges (Graph, List) := Graph => (G,L) -> (
    E := set edges G;
    E' := E - set(for l in L list set l);
    graph(vertexSet G, toList(E'), EntryMode => "edges")
    )
deleteEdges (Digraph, List) := Graph => (G,L) -> digraph(vertexSet G, edges G - set L)

deleteVertex = method()
deleteVertex (Graph, Thing) := Graph => (G, v) -> (
    if not member(v, vertexSet G) then error "v must be a vertex of G";
    V := vertexSet G - set {v};
    E := select(toList \ edges G, e -> not member(v, e));
    graph(V,E, EntryMode => "edges")
    )
deleteVertex (Digraph, Thing) := Digraph => (G, v) -> (
    if not member(v, vertexSet G) then error "v must be a vertex of G";
    V := vertexSet G - set {v};
    E := select(edges G, e -> not member(v,e));
    digraph(V,E, EntryMode => "edges")
    )

deleteVertices = method()
deleteVertices (Digraph, List) := Digraph => (D, L) -> inducedSubgraph(D, vertexSet D - set L)

indexLabelGraph = method()
indexLabelGraph Graph := Graph => G -> (
    V := vertexSet G;
    h := hashTable apply(#V, i -> V_i => i);
    E := apply(toList \ edges G, e -> {h#(e_0), h#(e_1)});
    graph(toList(0..<#V), E, EntryMode => "edges")
    )

indexLabelGraph Digraph := Digraph => G -> (
    V := vertexSet G;
    h := hashTable apply(#V, i -> V_i => i);
    E := apply(edges G, e -> {h#(e_0), h#(e_1)});
    digraph(toList(0..<#V), E, EntryMode => "edges")
    )

inducedSubgraph = method()
inducedSubgraph (Graph, List) := Graph => (G, S) -> (
    if S == {} then graph {}
    else E' := select(edges G, e -> isSubset(e,S));
    graph(S, E', EntryMode => "edges")
    )
inducedSubgraph (Digraph,List) := Digraph => (D,S) -> (
    E' := select(edges D, e -> isSubset(e,S));
    digraph(S, E', EntryMode => "edges")
    )

reindexBy = method()
reindexBy (Graph, String) := Graph => (G, s) -> (
    s = toLower s;
    if s == "maxdegree" then (
        V := vertexSet G;
        V' := {};
        while V != {} do (
            x := hashTable apply(V, i -> i => degree (G,i));
            S := select(keys x, i -> x#i == max values x);
            V = V - set S;
            V' = V' | S;
             );
        return graph (V', edges G)
        );
    if s == "mindegree" then (
        V = vertexSet G;
        V' = {};
        while V != {} do (
            x = hashTable apply(V, i -> i => degree (G,i));
            S = select(keys x, i -> x#i == max values x);
            V = V - set S;
            V' = S | V'
             );
        return graph (V', edges G)
        );
    if s == "random" then return graph (random vertexSet G, edges G, EntryMode => "edges");
    if s == "components" then return graph (flatten connectedComponents G, edges G, EntryMode => "edges");
    if s == "sort" then return graph (sort vertexSet G, edges G, EntryMode => "edges");
    )
reindexBy (Digraph, String) := Digraph => (D, s) -> (
    s = toLower s;
    if s == "maxdegreein" then (
        V := vertexSet D;
        V' := {};
        while V != {} do (
            x := hashTable apply(V, i -> i => #parents(D,i));
            S := select(keys x, i -> x#i == max values x);
            V = V - set S;
            V' = V' | S;
             );
        return digraph (V', edges D, EntryMode => "edges")
        );
    if s == "mindegreein" then (
        V = vertexSet D;
        V' = {};
        while V != {} do (
            x = hashTable apply(V, i -> i => #parents(D,i));
            S = select(keys x, i -> x#i == max values x);
            V = V - set S;
            V' = S | V';
             );
        return digraph (V', edges D, EntryMode => "edges")
        );
    if s == "maxdegreeout" then (
        V = vertexSet D;
        V' = {};
        while V != {} do (
            x = hashTable apply(V, i -> i => #children(D,i));
            S = select(keys x, i -> x#i == max values x);
            V = V - set S;
            V' = V' | S;
             );
        return digraph (V', edges D, EntryMode => "edges")
        );
    if s == "mindegreeout" then (
        V = vertexSet D;
        V' = {};
        while V != {} do (
            x = hashTable apply(V, i -> i => #children(D,i));
            S = select(keys x, i -> x#i == max values x);
            V = V - set S;
            V' = S | V';
             );
        return digraph (V', edges D, EntryMode => "edges")
        );
    if s == "maxdegree" then (
        V = vertexSet D;
        V' = {};
        while V != {} do (
            x = hashTable apply(V, i -> i => (#children(D,i) + #parents(D,i)));
            S = select(keys x, i -> x#i == max values x);
            V = V - set S;
            V' = V' | S;
             );
        return digraph (V', edges D, EntryMode => "edges")
        );
    if s == "mindegree" then (
        V = vertexSet D;
        V' = {};
        while V != {} do (
            x = hashTable apply(V, i -> i => (#children(D,i) + #parents(D,i)));
            S = select(keys x, i -> x#i == max values x);
            V = V - set S;
            V' = S | V';
             );
        return digraph (V', edges D, EntryMode => "edges")
        );
    if s == "random" then return digraph(random vertexSet D, edges D, EntryMode => "edges");
    if s == "sort" then return digraph(sort vertexSet D, edges D, EntryMode => "edges");
    )

removeNodes = deleteVertices

spanningForest = method()
spanningForest Graph := Graph => G -> (
    V := vertexSet G;
    E := {};
    for v in V do (
        N := toList (neighbors (G,v) - set select(V, x -> member(x, flatten E)));
        E = E | apply(N, n -> {v,n});
        );
    graph(V, E, EntryMode => "edges")
    )

vertexMultiplication = method()
vertexMultiplication (Graph, Thing, Thing) := Graph => (G,v,u) -> (
    if member(u, vertexSet G) == true then error "3rd argument is already a vertex of the input graph";
    if member(v, vertexSet G) == false then error "2nd argument must be in the input graph's vertex set";
    graph(append(vertexSet G, u), edges G | apply(toList neighbors (G,v), i -> {i,u}), EntryMode => "edges")
    )




-* 

--This code is written for an older version of Graphs and is not functional with current version of the packages.

graphData = "graphData"
labels = "labels"

LabeledGraph = new Type of HashTable

labeledGraph = method(TypicalValue =>LabeledGraph)
labeledGraph (Digraph,List) := (g,L) -> (
    C := new MutableHashTable;
    C#cache = new CacheTable from {};
    lg := new MutableHashTable;
    lg#graphData = g;
    label := new MutableHashTable;
    if instance(g,Graph) then (
        sg := simpleGraph g;
        scan(L, i -> 
            if (sg#graph#(i#0#0))#?(i#0#1) then label#(i#0) = i#1
            else if (sg#graph#(i#0#1))#?(i#0#0) then label#({i#0#1,i#0#0}) = i#1
            else error (toString(i#0)|" is not an edge of the graph");
            );
        )
    else (
        scan(L, i -> 
            if (g#graph#(i#0#0))#?(i#0#1) then label#(i#0) = i#1
            else error (toString(i#0)|" is not an edge of the graph");
            );
        );
    lg#labels = new HashTable from label;
    C#graph = lg;
    new LabeledGraph from C
    )

net LabeledGraph := g -> horizontalJoin flatten (
     net class g,
    "{",
    stack (horizontalJoin \ sort apply(pairs (g#graph),(k,v) -> (net k, " => ", net v))),
    "}"
    )

toString LabeledGraph := g -> concatenate(
    "new ", toString class g#graph,
    if parent g#graph =!= Nothing then (" of ", toString parent g),
    " from {",
    if #g#graph > 0 then demark(", ", apply(pairs g#graph, (k,v) -> toString k | " => " | toString v)) else "",
    "}"
    )

graph LabeledGraph := opts -> g -> g#graph  --used to transform the LabeledGraph into a hashtable

*- 




------------------------------------------
------------------------------------------
-- Documentation
------------------------------------------
------------------------------------------

beginDocumentation()

-- authors: add some text to this documentation node:
doc ///
  Key
    Graphs
///

-------------------------------
--Data Types
doc ///
    Key
    	Digraph
///

doc ///
    Key
    	Graph
///





-------------------------------
--Graph Constructors
-------------------------------

--digraph
doc///
    Key
        digraph
        (digraph, List)
        (digraph, List, List)
        (digraph, HashTable)
        (digraph, List, Matrix)
        (digraph, Matrix)
    Headline
        Constructs a digraph
    Usage
        G = digraph E
        G = digraph H
        G = digraph (V, E)
        G = digraph (V, A)
        G = digraph A
    Inputs
        E:List
            Denotes an edge list (a list of ordered pair lists)
        V:List
            Denotes a vertex list
        H:HashTable
        A:Matrix
            Denotes an adjacency matrix
    Outputs
        G:Digraph
    Description
        Text
            A digraph is a set of vertices connected by directed edges. Unlike the case with simple graphs, {u,v} being an edge does not imply that {v,u} is also an edge. Notably, this allows for non-symmetric adjacency matrices.
        Example
            G = digraph ({{1,2},{2,1},{3,1}}, EntryMode => "edges")
            G = digraph hashTable{1 => {2}, 3 => {4}, 5 => {6}}
            G = digraph ({{a,{b,c,d,e}}, {b,{d,e}}, {e,{a}}}, EntryMode => "neighbors")
            G = digraph ({x,y,z}, matrix {{0,1,1},{0,0,1},{0,1,0}})
            G = digraph matrix {{0,1,1},{0,0,1},{0,1,0}}
    SeeAlso
        graph
///

--graph
doc ///
    Key
        graph
        (graph, List)
        (graph, List, List)
        (graph, HashTable)
        (graph, List, Matrix)
        (graph, Matrix)
        [graph, Singletons]
        [graph, EntryMode]
	EntryMode
    Headline
        Constructs a simple graph
    Usage
        G = graph E
        G = graph (V,E)
        G = graph H
        G = graph (V, A)
        G = graph A
    Inputs
        E:List
        V:List
        H:HashTable
        A:Matrix
    Outputs
        G:Graph
            The graph with edges E and vertices V, or constructed from HashTable H, or from an adjacency matrix A, or from a new naming of vertices V and an adjacency matrix A.
    Description
        Text
            A graph consists of two sets, a vertex set and an edge set which is a subset of the collection of subsets of the vertex set. Edges in graphs are symmetric or two-way; if u and v are vertices then if {u,v} is an edge connecting them, {v,u} is also an edge (which is implicit in the definition, we will almost always just use one of the pairs). Graphs are defined uniquely from their Adjacency Matrices. These matrices use the entries as 0 or 1 to signal the existence of an edge connecting vertices.
            The options for EntryMode are "neighbors" and "edges" (the default).  This means that in including EntryMode => "edges" in the constructor allows the user to simply type in a list of edges to construct a graph.  See example 1 below.  Using the default takes an input of a list of pairs, where the first entry of each pair is a vertex and the second entry of each pair is that vertex's neighborhood.
            The options for Singletons allows the user to enter Singletons => {list of single points} in a graph if they desire to have isolated points in a graph.  See second example below.
        Example
            G = graph({{1,2},{2,3},{3,4}})
            G = graph({{1,2},{2,3},{3,4}}, Singletons => {5,6,7})
            G = graph ({{a,{b,c,d,e}}, {b,{d,e}}, {e,{a}}})
            G = graph hashTable {{1,{2}},{2,{1,3}},{3,{2,4}},{4,{3}}}
            G = graph(matrix {{0,1,1},{1,0,0},{1,0,0}})
            G = graph({a,b,c}, matrix {{0,1,1},{1,0,0},{1,0,0}})
    SeeAlso
        digraph

///

--graph
doc ///
    Key
        (graph, Digraph)
    Headline
        Returns the legacy G#graph hash table
    Usage
        G = graph D
    Inputs
        D:Digraph
    Outputs
        H:HashTable
            The hash table with a graph's vertices as keys and list of neighbors as values.
    Description
        Text
            A graph consists of two sets, a vertex set and an edge set which is a subset of the collection of subsets of the vertex set. Edges in graphs are symmetric or two-way; if u and v are vertices then if {u,v} is an edge connecting them, {v,u} is also an edge (which is implicit in the definition, we will almost always just use one of the pairs). The options for EntryMode are "neighbors" (the default) and "edges".  This method returns a hash table where the keys are vertices of a given graph or digraph and the values are their children (or neighbors, in the case of undirected graphs).
        Example
            G = graph digraph({{1,2},{2,1},{3,1}}, EntryMode => "edges")
            G = graph digraph(matrix {{0,1,1},{1,0,0},{1,0,0}})
    SeeAlso
        digraph
///

--------------------------------
--Graphs: Basic Data
--------------------------------

--adjacencyMatrix
doc ///
    Key
        adjacencyMatrix
        (adjacencyMatrix, Digraph)
    Headline
        Returns the adjacency matrix of a Graph or Digraph
    Usage
        A = adjacencyMatrix D
        A = adjacencyMatrix G
    Inputs
        D:Digraph
        G:Graph
    Outputs
        A:Matrix
    Description
        Text
            The adjacency matrix is the n by n matrix (where n is the number of vertices in graph/digraph G) with rows and columns indexed by the vertices of G. Entry A_(u,v) is 1 if and only if {u,v} is an edge of G and 0 otherwise. It is easy to observe that if we just use a simple graph G, then its adjacency matrix must be symmetric, but if we use a digraph, then it is not necessarily symmetric.
        Example
            D = digraph({{1,2},{2,3},{3,4},{4,3}},EntryMode=>"edges");
            adjacencyMatrix D
            G = graph({1,2,3,4}, {{1,2},{2,3},{3,4},{4,3}})
            adjacencyMatrix G
    SeeAlso
        degreeMatrix
        laplacianMatrix
///

--degree
doc ///
    Key
        (degree, Digraph, Thing)
    Headline
        returns the degree of a vertex in a digraph
    Usage
        x = degree(D,v)
    Inputs
        D:Digraph
        v:Thing
            a vertex in the graph/digraph
    Outputs
        x:ZZ
    Description
        Text
            In a simple graph, the degree of a vertex is the number of neighbors of the vertex. 
            In a digraph, we define the degree of a vertex to be the number of elements in the unique union of the parents and children of the vertex.
        Example
            D = digraph({1,2,3,4},{{1,2},{2,3},{3,4},{4,2},{2,4}});
            degree(D, 3)
            degree(D, 2)
    SeeAlso
        neighbors
        parents
        children
///

--degreeMatrix
doc ///
    Key
        degreeMatrix
        (degreeMatrix, Digraph)
    Headline
        Returns the degree matrix of a graph
    Usage
        D = degreeMatrix G
    Inputs
        G:Graph
    Outputs
        D:Matrix
            The degree matrix of graph G
    Description
        Text
            The degree matrix is the n by n diagonal matrix (where n is the number of vertices in the vertex set of the graph G) indexed by the vertices of G where A_(u,u) is the degree of vertex u. The degree of a vertex u is the number of edges such that {u,v} is an edge for any v also in the vertex set. This matrix is always diagonal.
        Example
            G = graph({1,2,3,4,5},{{1,2},{2,3},{3,4},{3,5},{4,5}});
            degreeMatrix G
    SeeAlso
        adjacencyMatrix
        laplacianMatrix
        degree
///

doc ///
    Key
        degreeSequence
        (degreeSequence, Graph)
    Headline
        the degree sequence of a graph
    Usage
        degreeSequence G
    Inputs
        G:Graph
    Outputs
        :List -- the degree sequence of G
    Description
        Text
            The degree sequence of a graph is the list of the degrees of its
            vertices sorted in nonincreasing order.
        Example
            degreeSequence pathGraph 5
///

--edges
doc ///
    Key
        edges
        (edges, Digraph)
        (edges, Graph)
    Headline
        Returns the edges of a digraph or graph
    Usage
        E = edges D
        E = edges G
    Inputs
        D:Digraph
        G:Graph
    Outputs
        E:List
            The edges of digraph D or graph G
    Description
        Text
            The edges of a graph are pairs (or ordered pairs if we are dealing with digraphs) of vertices that are connected in a graph. Any edge must be a member of the collection of subsets of the vertex set of a graph.
        Example
            D = digraph({{1,2},{2,1},{3,1}},EntryMode=>"edges");
            edges D
            G = cycleGraph 4;
            edges G
    SeeAlso
        vertexSet
///

--incidenceMatrix
doc ///
    Key
        incidenceMatrix
        (incidenceMatrix, Graph)
    Headline
        computes the incidence matrix of a graph
    Usage
        M = incidenceMatrix G
    Inputs
        G:Graph
    Outputs
        M:Matrix
            the incidence matrix of graph G
    Description
        Text
            An incidence matrix M is the #vertexSet of G by #edges of G matrix where entry (i,j) equals 1 if vertex i is incident to edge j, and equals 0 otherwise.
        Example
            M = incidenceMatrix cycleGraph 3
    SeeAlso
        adjacencyMatrix
///

--add laplacianMatrix to export list!
--laplacianMatrix
doc ///
    Key
        laplacianMatrix
        (laplacianMatrix, Graph)
    Headline
        Returns the laplacian matrix of a graph
    Usage
        L = laplacianMatrix G
    Inputs
        G:Graph
    Outputs
        L:Matrix
            the laplacian matrix of graph G
    Description
        Text
            The laplacian matrix of a graph is the adjacency matrix of the graph subtracted from the degree matrix of the graph.
        Example
            G = graph({1,2,3,4,5},{{1,2},{2,3},{3,4},{3,5},{4,5}});
            laplacianMatrix G
    SeeAlso
        adjacencyMatrix
        degreeMatrix
///

--vertexSet
doc ///
    Key
        vertexSet
        (vertexSet, Digraph)
        (vertices, Digraph)
    Headline
        Returns the vertices of a graph or digraph
    Usage
        V = vertexSet D
        V = vertexSet G
    Inputs
        D:Digraph
        G:Digraph
    Outputs
        V:List
            The vertices of digraph D
    Description
        Text
            The vertices of a graph are just singletons that can be indexed by numbers, letters, or even in some cases something as exotic as a monomial. These form the base of a graph; the edges are 2 member subsets of the vertex set of a graph.
        Example
            D = digraph({{1,2},{2,1},{3,1}},EntryMode=>"edges");
            vertexSet D;
            G = completeGraph 4;
            vertexSet G
            A = adjacencyMatrix G;
            graph({a,b,c,d}, A)
    SeeAlso
        edges
///

--------------------------------
--Graphs: Display Methods
--------------------------------

-- displayGraph
doc ///
    Key
        displayGraph
        (displayGraph, String, String, Digraph)
        (displayGraph, String, Digraph)
        (displayGraph, Digraph)
    Headline
        displays a digraph or graph using Graphviz
    Usage
        displayGraph(dotFileName,jpgFileName,G)
        displayGraph(dotFileName,G)
        displayGraph G
    Inputs
        G:Digraph
        dotFileName:String
        jpgFileName:String
    Description
        Text
            Displays a digraph or graph using Graphviz
        -- Example
        --     --G = graph({1,2,3,4,5},{{1,3},{3,4},{4,5}});
        --     --displayGraph("chuckDot","chuckJpg", G)
        --     --displayGraph("chuck", G)
        --     --displayGraph G
    SeeAlso
        showTikZ
        writeDotFile
///

-- showTikZ
doc ///
    Key
        showTikZ
        (showTikZ, Digraph)
    Headline
        Writes a string of TikZ syntax that can be pasted into a .tex file to display G
    Usage
        S = showTikZ(G)
    Inputs
        G:Digraph
        S:String
            TikZ syntax used to display G
    Description
        Text
            Writes a string of TikZ syntax that can be pasted into a .tex file to display G
        -- Example
        --     --G = graph({1,2,3,4,5},{{1,3},{3,4},{4,5}});
        --     --showTikZ G
    SeeAlso
        displayGraph
///

-- html
doc ///
    Key
        (html, Digraph)
    Headline
        Create an .svg representation of a graph or digraph
    Usage
        html G
    Inputs
        G:Digraph
    Description
        Text
            Uses graphviz to create an .svg representation of @TT "G"@,
            which is returned as a string.
        CannedExample
            i2 : html completeGraph 2
            -- running: dot -Tsvg /tmp/M2-2729721-0/0.dot -o /tmp/M2-2729721-0/1.svg

            o2 = <?xml version="1.0" encoding="UTF-8" standalone="no"?>
                 <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
                  "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
                 <!-- Generated by graphviz version 2.43.0 (0)
                  -->
                 <!-- Title: G Pages: 1 -->
                 <svg width="62pt" height="116pt"
                  viewBox="0.00 0.00 62.00 116.00" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
                 <g id="graph0" class="graph" transform="scale(1 1) rotate(0) translate(4 112)">
                 <title>G</title>
                 <polygon fill="white" stroke="transparent" points="-4,4 -4,-112 58,-112 58,4 -4,4"/>
                 <!-- 0 -->
                 <g id="node1" class="node">
                 <title>0</title>
                 <ellipse fill="none" stroke="black" cx="27" cy="-90" rx="27" ry="18"/>
                 <text text-anchor="middle" x="27" y="-86.3" font-family="Times,serif" font-size="14.00">0</text>
                 </g>
                 <!-- 1 -->
                 <g id="node2" class="node">
                 <title>1</title>
                 <ellipse fill="none" stroke="black" cx="27" cy="-18" rx="27" ry="18"/>
                 <text text-anchor="middle" x="27" y="-14.3" font-family="Times,serif" font-size="14.00">1</text>
                 </g>
                 <!-- 0&#45;&#45;1 -->
                 <g id="edge1" class="edge">
                 <title>0&#45;&#45;1</title>
                 <path fill="none" stroke="black" d="M27,-71.7C27,-60.85 27,-46.92 27,-36.1"/>
                 </g>
                 </g>
                 </svg>
///

-- writeDotFile
doc ///
    Key
        writeDotFile
        (writeDotFile, String, Graph)
        (writeDotFile, String, Digraph)
    Headline
        Writes a graph to a dot file with a specified filename
    Usage
        writeDotFile(fileName,G)
    Inputs
        G:Graph
        fileName:String
    Description
        Text
            Writes the code for an inputted graph to be constructed in Graphviz with specified file name.
        -- Example
        --     --G = graph({1,2,3,4,5},{{1,3},{3,4},{4,5}});
        --     --writeDotFile("chuck", G)
    SeeAlso
///

--------------------------------
--Graphs: Derivative Graphs
--------------------------------

--barycenter
doc///
    Key
        barycenter
        (barycenter, Graph)
    Headline
        Returns the barycenter of a grah
    Usage
        H = barycenter G
    Inputs
        G:Graph
    Outputs
        H:Graph
            The barycenter of G
    Description
        Text
            The barycenter of a graph is the subgraph induced by all the vertices with eccentricity equal to the graph's radius, in other words, it is the subgraph induced by the center of a graph.
        Example
            barycenter pathGraph 6
            barycenter completeGraph 6
    SeeAlso
        center
///

--complementGraph
doc ///
    Key
        complementGraph
        (complementGraph,Graph)
    Headline
        Returns the complement of a graph
    Usage
        G' = complementGraph G
    Inputs
        G:Graph
    Outputs
        G':Graph
            The complement graph of G
    Description
        Text
            The complement graph of a graph G is the graph G^c where any two vertices are adjacent in G^c iff they are not adjacent in G. The original vertex set is preserved, only the edges are changed.
        Example
            G = cycleGraph 4
            complementGraph G
///


--digraphTranspose
doc ///
    Key
        digraphTranspose
        (digraphTranspose,Digraph)
    Headline
        returns the transpose of a Digraph
    Usage
        G = digraphTranspose D
    Inputs
        D:Digraph
    Outputs
        G:Digraph
            the transpose digraph of D
    Description
        Text
            The transpose of a digraph D is the graph formed by taking every edge (u,v) in D and changing it to (v,u). Intuitively, it reverses the direction of all the edges while keeping the same vertex set.  One can also view the transpose in terms of adjacency matrices, where the adjacency matrix of the transpose of D is the transpose of the adjacency matrix of D. In this way, we quickly see that the transpose of a transpose digraph is the original digraph, and that this operator is trivial for simple graphs since they have symmetric matrices.
        Example
            D = digraph ({{1,2},{2,3},{3,4},{4,1},{1,3},{4,2}},EntryMode=>"edges")
            D' = digraphTranspose D
            D'' = digraphTranspose D'
///

--lineGraph
doc ///
    Key
        lineGraph
        (lineGraph, Graph)
    Headline
        Returns the line graph of an undirected graph
    Usage
        L = lineGraph G
    Inputs
        G:Graph
    Outputs
        L:Graph
            The line graph of G
    Description
        Text
            The line graph L of an undirected graph G is the graph whose
            vertex set is the edge set of the original graph G and in
            which two vertices are adjacent if their corresponding
            edges share a common endpoint in G.
        Example
            G = graph({{1,2},{2,3},{3,4},{4,1},{1,3},{4,2}},EntryMode=>"edges")
            lineGraph G
    SeeAlso
///

--underlyingGraph
doc ///
    Key
        underlyingGraph
        (underlyingGraph, Digraph)
    Headline
        Returns the underlying graph of a digraph
    Usage
        G = underlyingGraph D
    Inputs
        D:Digraph
    Outputs
        G:Graph
            The underlying graph of digraph D
    Description
        Text
            The underlying graph of a digraph is the simple graph constructed by all edges {u,v} such that (u,v) or (v,u) is a directed edge in the digraph.
        Example
            D = digraph hashTable{1 => {2,3}, 2 => {1,3}, 3 => {}};
            underlyingGraph D
    SeeAlso
///

--------------------------------
--Graphs: Enumerators
--------------------------------
--barbellGraph
doc ///
    Key
        barbellGraph
        (barbellGraph, ZZ)
    Headline
        Returns the barbell graph
    Usage
        G = barbellGraph n
    Inputs
        n:ZZ
    Outputs
        G:Graph
            The barbell graph
    Description
        Text
            The barbell graph corresponding to an integer n is formed by the disjoint union of two complete graphs on n vertices joined together by exactly on edge connecting these complete graphs.
        Example
            G = barbellGraph 6
///

--circularLadder
doc ///
    Key
        circularLadder
        (circularLadder, ZZ)
    Headline
        Returns a circular ladder graph
    Usage
        G = circularLadder n
    Inputs
        n:ZZ
    Outputs
        G:Graph
            The circular ladder graph
    Description
        Text
            The circular ladder graph corresponding to an integer n is a ladder of size n with two extra edges that connect the each top vertex with its respective bottom vertex. This creates two cycles, one inside and the other outside, that are connected by edges.
        Example
            G = circularLadder 5
    SeeAlso
        ladderGraph
///

--cocktailParty
doc ///
    Key
        cocktailParty
        (cocktailParty, ZZ)
    Headline
        Returns a cocktail party graph
    Usage
        G = cocktailParty n
    Inputs
        n:ZZ
            The number of vertices on each side; there will be 2*n total vertices
    Outputs
        G:Graph
            The cocktail party graph
    Description
        Text
            The cocktail party graph with respect to an integer n is a graph with 2*n vertices. Its edge set is formed by taking a disjoint union of n path graphs on 2 vertices and taking its complement, yielding an edge set of every possible edge except for those that were initially adjacent on the ladder.
        Example
            cocktailParty 4
///

--completeGraph
doc ///
    Key
        completeGraph
        (completeGraph, ZZ)
    Headline
        Constructs a complete graph
    Usage
        K = completeGraph n
    Inputs
        n:ZZ
    Outputs
        K:Graph
            The complete graph with n vertices
    Description
        Text
            A complete graph on n vertices is a graph in which all the vertices are adjacent to each other.
        Example
            K = completeGraph 5
///

--completeMultipartiteGraph
doc ///
    Key
        completeMultipartiteGraph
        (completeMultipartiteGraph, List)
    Headline
        constructs a complete multipartite graph
    Usage
        G = completeMultipartiteGraph P
    Inputs
        P:List
            if P has k elements, the graph is k-partite. P_i determines how many vertices are in each partite group
    Outputs
        G:Graph
            a complete multipartite graph
    Description
        Text
            A complete multipartite graph is a graph that is first and foremost multi-partite. That is, the vertex set of a complete multipartite graph can be partitioned into k sets such that within each set, none of the vertices are connected by an edge.  The second condition is that each vertex is connected to ever vertex except for those in its partition so that it is "almost" a complete graph. For programming this graph, the input is a list P. The length of the list P will be the number of groups of vertices. For example, in a complete bipartite graph, the length of the list would be 2. The entry P_i will determine how many vertices are in each partition; necisarrily, we see that the entries of the list must be positive integers.
        Example
            G = completeMultipartiteGraph {1,2,3}
///

--crownGraph
doc ///
    Key
        crownGraph
        (crownGraph, ZZ)
    Headline
        Returns a crown graph
    Usage
        G = crownGraph n
    Inputs
        n:ZZ
            The number of vertices on each side; there will be 2*n total vertices
    Outputs
        G:Graph
    Description
        Text
            The crown graph with respect to n is a type of bipartite graph. More specifically, it is the complement of the ladder graph with respect to n.
        Example
            crownGraph 4
    SeeAlso
        ladderGraph
///

--cycleGraph
doc ///
    Key
        cycleGraph
        (cycleGraph, ZZ)
    Headline
        Constructs a cycle graph
    Usage
        C = cycleGraph n
    Inputs
        n:ZZ
    Outputs
        C:Graph
            The cycle graph with n vertices
    Description
        Text
            A cycle graph is a graph on n vertices in which all the vertices are in a closed chain of edges.
        Example
            C = cycleGraph 5
///

--doubleStar
doc ///
    Key
        doubleStar
        (doubleStar,ZZ,ZZ)
    Headline
        returns a double star graph
    Usage
        G = doubleStar (m,n)
    Inputs
        n:ZZ
        m:ZZ
    Outputs
        G:Graph
    Description
        Text
            A double star graph is a graph formed by starting with 2 vertices and joining them together. Then each vertex is connected to a fixed amount of leaves (vertices of degree 1) specified by the user in the inputs.
        Example
            G = doubleStar(4,5)
    SeeAlso
        starGraph
        isLeaf
///

--friendshipGraph
doc ///
    Key
        friendshipGraph
        (friendshipGraph, ZZ)
    Headline
        Returns a friendship Graph
    Usage
        G = friendshipGraph n
    Inputs
        n:ZZ
    Outputs
        G:Graph
    Description
        Text
            Friendship graphs of size n are a special case of windmill graphs. A friendship graph of size n is n 3-cycles that all share one common vertex.
        Example
            G = friendshipGraph 4
            H = windmillGraph (3,4)
    SeeAlso
        windmillGraph
///

--generalizedPetersenGraph
doc ///
    Key
        generalizedPetersenGraph
        (generalizedPetersenGraph, ZZ, ZZ)
    Headline
        Returns a generalized petersen graph
    Usage
        G = generalizedPetersenGraph (n, k)
    Inputs
        n:ZZ
            The number of vertices will be 2*n, n in the outer ring and n in the inside ring
        k:ZZ
            The middle ring is a complete graph but looks like a star, k is the number of vertices that get jumped for each connection k must be less than n/2.
    Outputs
        G:Graph
            The generalized petersen graph
    Description
        Text
            The generalized Petersen Graph is a class of graphs with a particular edge set. There are two equal sets of vertices and each set is a cycle graph. This forms two disjoint cyclegraphs. Then each inside edge connects to an adjacent outside edge, similar to the circular ladder graph. The outer loop keeps a more "canonical" order for the cycle, in the sense that it does not "skip" vertices, while the inner cycle takes on a "star-like pattern" that jumps vertices but is still connected.
        Example
            generalizedPetersenGraph (5,2)
            --The standard petersen graph
    SeeAlso
        circularLadder
///

--graphLibrary
doc ///
    Key
        graphLibrary
        (graphLibrary, String)
    Headline
        constructs a graph of a type specified in the string input
    Usage
        G = graphLibrary(name)
    Inputs
        name:String
    Outputs
        G:Graph
            The graph of the type specified by the String name
    Description
        Text
            The graph library takes in a name of a special graph and constructs a graph of that type.  Possible inputs include:  "petersen", "bidiakis cube", "desargues", "dodecahedron", "durer", "claw", "cubical", "f26a", "franklin", "chvatal", "heawood", "paw", "mobius", "nauru", "kite", "house", "bull", "bowtie", "dart".  Each of these create the special graph described clearly by their name.
        Example
            G = graphLibrary("petersen")
            G = graphLibrary("f26a")
            G = graphLibrary("chvatal")
///

--kneserGraph
doc ///
    Key
        kneserGraph
        (kneserGraph, ZZ, ZZ)
    Headline
        constructs a kneser graph of specified size
    Usage
        G = kneserGraph(n,k)
    Inputs
        n:ZZ
        k:ZZ
    Outputs
        G:Graph
            The kneser graph constructed with vertices corresponding to the k-element subsets of a set of n elements.
    Description
        Text
            A kneser graph (n,k) has vertices corresponding to the k-element subsets of a set of n elements, where two vertices are adjacent if and only if their corresponding k-element subsets are disjoint.
        Example
            G = kneserGraph(5,2)
///

--ladderGraph
doc ///
    Key
        ladderGraph
        (ladderGraph, ZZ)
    Headline
        Returns a ladder graph
    Usage
        G = ladderGraph n
    Inputs
        n:ZZ
            The length length of the ladder
    Outputs
        G:Graph
            The ladder Graph
    Description
        Text
            A ladder graph of length n is two path graphs of size n each joined by a set of n edges. The first edge connects the top elements, the second the second elements and the last edge connects the bottom elements, making a 2 by n grid that looks like a ladder.
        Example
            ladderGraph 5
    SeeAlso
        circularLadder
///

--lollipopGraph
doc ///
    Key
        lollipopGraph
        (lollipopGraph, ZZ, ZZ)
    Headline
        constructs a lollipop graph
    Usage
        G = lollipopGraph (m, n)
    Inputs
        m:ZZ
            The number of vertices in the complete graph element
        n:ZZ
            The length of stem coming out of the complete graph element
    Outputs
        G:Graph
            The lollipop graph
    Description
        Text
            A lollipop graph is a graph that is the union of two major elements. The "candy" portion is a complete graph of size m. Coming out of this is the stick or stem, just a path graph of size n. The combination of these yields a lollipop graph.
        Example
            lollipopGraph (6,2)
    SeeAlso
        rattleGraph
///

--monomialGraph
doc///
    Key
        monomialGraph
        (monomialGraph, MonomialIdeal, ZZ)
    Headline
        Returns a monomial graph
    Usage
        G = monomialGraph(I,n)
    Inputs
        I:MonomialIdeal
            This monomial ideal be part of forming a quotient ring with respect to the ambient ring the ideal is in
        n:ZZ
            This integer determines the degree of monomials that will be considered
    Outputs
        G:Graph
            The monomial graph
    Description
        Text
            The monomial graph with respect to a monomial ideal and an integer n is a graph with a vertex set of the monomials of the expression of the sum of the generators of the ambient ring for I to the power n.  The edge set is formed by the rule that there is an edge between two of the vertices (which we are reminded are monomials) if and only if the degree of the least common multiple of the two vertices is n+1.
        Example
            R = QQ[x,y];
            I = monomialIdeal (x^3, y^2*x);
            monomialGraph (I, 3)
    SeeAlso
        lcm
///

--pathGraph
doc///
    Key
        pathGraph
        "pathGraph(ZZ)"
    Headline
        A method that makes a path graph
    Usage
        P = pathGraph n
    Inputs
        n:ZZ
    Outputs
        P:Graph
            the path grah of n vertices
    Description
        Text
            A path graph on n vertices is a cycle graph of n vertices minus one edge.
        Example
            pathGraph 5
    SeeAlso
        cycleGraph
///

--rattleGraph
doc///
    Key
        rattleGraph
        (rattleGraph, ZZ, ZZ)
    Headline
        Returns a rattle graph
    Usage
        G = rattleGraph (n, k)
    Inputs
        n:ZZ
            n determines the amount of vertices for the bulb or rattle part of the graph
        k:ZZ
            k determines the length of the stem coming out of the rattle part of the graph
    Outputs
        G:Graph
    Description
        Text
            The rattle graph is the union of two graphs. The rattle or bulb part of the graph is simply an n-cycle. This n cycle is joined to a stem or handle of a rattle of length k, so the second piece is just a path graph on k vertices.
        Example
            rattleGraph (6, 3)
    SeeAlso
        lollipopGraph
///

--starGraph
doc ///
    Key
        starGraph
        (starGraph, ZZ)
    Headline
        Returns a star graph
    Usage
        G = starGraph n
    Inputs
        n:ZZ
    Outputs
        G:Graph
    Description
        Text
            The star graph is a special class of the general windmill graph class, in particular, it is windmill(2,n). A star graph is best visualized having one vertex in the center of a circle of other vertices.  The edge set is formed by connecting this center vertex to each of the outside vertices. The outside vertices are only connected to the center vertex.
        Example
            starGraph 5
    SeeAlso
        windmillGraph
///

--thresholdGraph
doc ///
    Key
        thresholdGraph
        (thresholdGraph, List)
    Headline
        A method that generates a threshold graph from a binary list
    Usage
        G = thresholdGraph L
    Inputs
        L:List
            This list is of 0's and 1's only
    Outputs
        G:Graph
    Description
        Text
            A threshold graph is a graph that is constructed by starting with an isolated vertex and iteratively adding another isolated vertex or a vertex that shares an edge with each vertex generated before it (the dominating vertices). The isolated vertices are represented by 0's and the dominating vertices are represented by 1's. In this method, the initial vertex is implicit and by default is constructed, so the first entry need not always be 0 in the list.
        Example
            L = {1,0,0,1,0,1}
            thresholdGraph L
///

--wheelGraph
doc ///
    Key
        wheelGraph
        (wheelGraph, ZZ)
    Headline
        Constructs a wheel graph
    Usage
        G = wheelGraph n
    Inputs
        n:ZZ
    Outputs
        G:Graph
            The wheel graph with n vertices
    Description
        Text
            A wheel graph is a cycle graph on n-1 vertices with an extra single vertex adjacent to every vertex in the cycle.
        Example
            G = wheelGraph 6
    SeeAlso
        cycleGraph
        windmillGraph
///

--windmillGraph
doc ///
    Key
        windmillGraph
        (windmillGraph, ZZ, ZZ)
    Headline
        Constructs a windmill graph
    Usage
        G = windmillGraph(k,d)
    Inputs
        k:ZZ
        d:ZZ
    Outputs
        G:Graph
            The windmill graph constructed by joining d copies of the complete graph K_k at a shared vertex.
    Description
        Text
            A whidmill joins d amount of copies of a complete graph on k vertices at exactly one shared vertex.
        Example
            G = windmillGraph(4,5)
    SeeAlso
        completeGraph
        wheelGraph
        starGraph
        friendshipGraph
///

--------------------------------
--Graphs: Cut Properties
--------------------------------

--edgeConnectivity
doc ///
    Key
        edgeConnectivity
        (edgeConnectivity, Graph)
    Headline
        computes the edge connectivity of a graph
    Usage
        C = edgeConnectivity G
    Inputs
        G:Graph
    Outputs
        C:ZZ
            the edge connectivity of a graph
    Description
        Text
            The edge connectivity of a graph is the smallest amount of edges that need be removed from a graph to make it not connected.  This corresponds to the size of the edge cuts.  A not connected graph has edge connectivity equal to 0.
        Example
            G = graph({{1,2},{2,3},{3,1},{3,4},{4,5},{5,3}},EntryMode=>"edges");
            edgeConnectivity G
    SeeAlso
        edgeCuts
        vertexConnectivity
///

--edgeCuts
doc ///
    Key
        edgeCuts
        (edgeCuts, Graph)
    Headline
        returns the edge cuts of a graph
    Usage
        C = edgeCuts G
    Inputs
        G:Graph
    Outputs
        C:List
            the edge cuts of a graph
    Description
        Text
            An edge cut is a minimal set of edges that, when removed from a graph, make the graph not connected.  If the graph is already not connected, the method returns the empty set.
        Example
            G = graph({{1,2},{2,3},{3,1},{3,4},{4,5},{5,3}},EntryMode=>"edges");
            edgeCuts G
    SeeAlso
        vertexCuts
        minimalVertexCuts
        edgeConnectivity
///

--minimalVertexCuts
doc ///
    Key
        minimalVertexCuts
        (minimalVertexCuts, Graph)
    Headline
        finds the minimal vertex cuts of a graph
    Usage
        C = minimalVertexCuts G
    Inputs
        G:Graph
    Outputs
        C:List
            the minimal vertex cuts of a graph
    Description
        Text
            A vertex cut is a set of vertices that, when removed from a graph, make the graph have more than one component.  The minimal vertex cuts are the only the vertex cuts removing only the smallest amount of vertices.  If the graph is complete, it has no vertex cuts, so the method returns an empty list.
        Example
            G = graph({{1,2},{2,3},{3,1},{3,4},{4,5},{5,3}},EntryMode=>"edges");
            minimalVertexCuts G
    SeeAlso
        vertexCuts
        vertexConnectivity
///


--minimalDegree
doc ///
    Key
        minimalDegree
        (minimalDegree, Graph)
    Headline
       computes the minimal degree of a graph 
    Usage
        d = minimalDegree G
    Inputs
        G:Graph
    Outputs
        d:ZZ
            the minimal degree of a graph
    Description
        Text
           This computes the minimal vertex degree of an undirected
           graph.
        Example
            G = graph({{1,2}});
            minimalDegree G
    SeeAlso
        degree
///

--vertexConnectivity
doc ///
    Key
        vertexConnectivity
        (vertexConnectivity, Graph)
    Headline
        computes the vertex connectivity of a graph
    Usage
        C = vertexConnectivity G
    Inputs
        G:Graph
    Outputs
        C:ZZ
            the vertex connectivity of a graph
    Description
        Text
            The vertex connectivity of a graph is the smallest amount of vertices that need be removed from a graph to make it not connected or have one vertex.  This corresponds to the size of the smallest vertex cut.  A not connected graph has connectivity equal to 0.
        Example
            G = graph({{1,2},{2,3},{3,1},{3,4},{4,5},{5,3}},EntryMode=>"edges");
            vertexConnectivity G
    SeeAlso
        minimalVertexCuts
        vertexCuts
        edgeConnectivity
///

--vertexCuts
doc ///
    Key
        vertexCuts
        (vertexCuts, Graph)
    Headline
        lists all the vertex cuts of a graph
    Usage
        C = vertexCuts G
    Inputs
        G:Graph
    Outputs
        C:List
            the list of vertex cuts of a graph
    Description
        Text
            A vertex cut is a set of vertices that, when removed from a graph, make the graph have more than one component.  The complete graph has no vertex cuts, so the method returns an empty list.
        Example
            G = cycleGraph 5;
            vertexCuts G
    SeeAlso
        minimalVertexCuts
        vertexConnectivity
        edgeCuts
///

--------------------------------
--Graphs: Properties
--------------------------------

--breadthFirstSearch
doc ///
    Key
        breadthFirstSearch
        (breadthFirstSearch, Digraph, Thing)
    Headline
        runs a breadth first search on the digraph starting at a specified node and returns a list of the vertices in the order they were discovered
    Usage
        bfs = breadthFirstSearch(D,v)
    Inputs
        D:Digraph
        v:Thing
    Outputs
        bfs:List
            A list of the vertices of D in order discovered by the breadth first search
    Description
        Text
            A breadth first search begins the search at the specified vertex of a digraph, followed by that vertex's children (or in the case of an undirected graph, its neighbors), followed by     their children (or neighbors), etc, until all the descendants are exhausted, and returns a list, such that the list's index number indicates the     depth level of the vertex, of lists of the vertices in order searched.
        Example
            D = digraph ({{0,1},{0,2},{2,3},{3,4},{4,2}},EntryMode=>"edges");
            bfs = breadthFirstSearch(D,0)
            G = cycleGraph 6
            bfs = breadthFirstSearch(G,3)
    SeeAlso
        reverseBreadthFirstSearch
        depthFirstSearch
        topologicalSort
///

--center
doc ///
    Key
        center
        (center,Graph)
    Headline
        Returns the center of a graph
    Usage
        L = center G
    Inputs
        G:Graph
    Outputs
        L:List
            This is a list of the vertices of G that form the center of G
    Description
        Text
            The center of a graph G is defined to be the set of all vertices of G such that the eccentricity of the vertex is equal to the graph's radius. This list often will contain 1 member, for example, path graphs on with an odd amount of vertices.  It can also contain all the vertices (such as complete graphs).
        Example
            center graphLibrary "dart"
    SeeAlso
        barycenter
        eccentricity
        radius
///

--children
doc ///
    Key
        children
        (children, Digraph, Thing)
    Headline
        returns the children of a vertex of a digraph
    Usage
        C = children (D, v)
    Inputs
        D:Digraph
        v:Thing
            the vertex that we want to find the children of
    Outputs
        C:Set
            a set of the children of v
    Description
        Text
            The children of v are the all the vertices u such that {v,u} is in the edge set of the digraph D. So the children of a vertex v are exactly those vertices on a directed graph that v points to.
        Example
            D = digraph({a,b,c,d,e},{{a,b},{b,c},{b,d},{e,b}});
            children(D, b)
    SeeAlso
        descendants
///

--chromaticNumber
doc ///
    Key
        chromaticNumber
        (chromaticNumber,Graph)
    Headline
        Computes the chromatic number of a graph
    Usage
        chi = chromaticNumber G
    Inputs
        G:Graph
    Outputs
        chi:ZZ
            The chromatic number of G
    Description
        Text
            The chromatic number of G is chi(G) = min{k | there exists a k-coloring of G}. A k-coloring of G is a partition into k sets of vertices such that in each of these sets, none of the members form edges with each other.
        Example
            G = cycleGraph 5;
            chromaticNumber G
    SeeAlso
        independenceNumber
///

--cliqueComplex
doc ///
    Key
        cliqueComplex
        (cliqueComplex,Graph)
    Headline
        Returns the clique complex of a graph
    Usage
        clG = cliqueComplex G
    Inputs
        G:Graph
    Outputs
        clG: SimplicialComplex
            The clique complex of G
    Description
        Text
            The clique complex of a graph G is the set of all cliques of G.
        Example
            G = graph(toList(1..4),{{1, 2}, {1, 3}, {2, 3}, {3, 4}});
            cliqueComplex G
    SeeAlso
        independenceComplex
        cliqueNumber
///

-- cliqueNumber
doc ///
    Key
        cliqueNumber
        (cliqueNumber,Graph)
    Headline
        Returns the clique number of a graph
    Usage
        omega = cliqueComplex G
    Inputs
        G:Graph
    Outputs
        omega:ZZ
            the clique number of G
    Description
        Text
            The clique number is the maximum number of vertices comprising a clique in G. A clique in a graph G is a set of vertices such that all the the vertices are mutually adjacent (they are all connected to each other).
        Example
            G = graph({{1, 2}, {1, 3}, {2, 3}, {3, 4}},EntryMode=>"edges");
            cliqueNumber G
    SeeAlso
        independenceNumber
        cliqueComplex
///

--closedNeighborhood
doc ///
    Key
        closedNeighborhood
        (closedNeighborhood, Graph, Thing)
    Headline
        Returns the closed neighborhood of a vertex of a graph
    Usage
        N = closedNeighborhood(G,v)
    Inputs
        G:Graph
        v:Thing
    Outputs
        N:List
            The closed neighborhood of vertex v in graph G
    Description
        Text
            The closed neighborhood of a vertex v just the union of the open neighborhood (or neighbors) of v and the vertex v itself
        Example
            G = cycleGraph 4;
            closedNeighborhood(G,2)
    SeeAlso
        neighbors
///

--missing documentation for clusteringCoefficient

--connectedComponents
doc ///
    Key
        (connectedComponents, Graph)
    Headline
        Computes the connected components of a graph
    Usage
        C = connectedComponents G
    Inputs
        G:Graph
    Outputs
        C:List
            The list of connected components of G
    Description
        Text
            A connected component is a list of vertices of a graph that are connected, in other words there exists a path of edges between any two vertices in the component.
        Example
            G = graph(toList(1..8),{{1,2},{2,3},{3,4},{5,6}});
            connectedComponents G
    SeeAlso
        isConnected
        numberOfComponents
///

--coverIdeal
doc ///
    Key
        coverIdeal
        (coverIdeal, Graph)
    Headline
        Returns the vertex cover ideal of a graph
    Usage
        J = coverIdeal G
    Inputs
        G:Graph
    Outputs
        J:Ideal
            The vertex cover ideal of a graph G
    Description
        Text
            The vertex cover ideal of a graph G is the ideal generated by (m_s | s in [n] is a vertex cover of G), where m_s = product_(i in S)(x_i)
        Example
            G = graph({{1, 2}, {1, 3}, {2, 3}, {3, 4}},EntryMode=>"edges");
            coverIdeal G
    SeeAlso
        edgeIdeal
        vertexCovers
        vertexCoverNumber
///

--criticalEdges
doc ///
    Key
        criticalEdges
        (criticalEdges, Graph)
    Headline
        Finds the critical edges of a graph
    Usage
        C = criticalEdges G
    Inputs
        G:Graph
    Outputs
        C:List
            the critical edges of G
    Description
        Text
            A critical edge is an edge such that the removal of the edge from the graph increases the graph's independence number.
        Example
            G = graph({{1,2},{2,3},{3,1},{3,4},{4,1},{4,2},{4,5}},EntryMode=>"edges");
            criticalEdges G
    SeeAlso
            independenceNumber
///

--degeneracy
doc ///
    Key
        degeneracy
        (degeneracy, Graph)
    Headline
        Computes the degeneracy of a graph
    Usage
        d = degeneracy G
    Inputs
        G:Graph
    Outputs
        d:ZZ
    Description
        Text
            The degeneracy of a graph is the maximum degree of all vertices in any subgraph of G.  This is essentially equivalent to the coloring number of G, which is the least number k such that there exists an ordering of the vertices of G in which each vertex has less than k neighbors earlier in the ordering.  The coloring number is equal to the degeneracy plus one.
        Example
            G = completeGraph 10;
            degeneracy G
    SeeAlso
///

--degreeCentrality
doc ///
    Key
        degreeCentrality
        (degreeCentrality, Graph, Thing)
    Headline
        Returns the degreeCentrality of a vertex of a graph
    Usage
        x = degreeCentrality (G, v)
    Inputs
        G:Graph
        v:Thing
            v must be a vertex of G
    Outputs
        x:RR
            x is a real number between 0 and 1
    Description
        Text
            The degreeCentrality of a vertex of a graph is the degree of a vertex divided by 2 times the number of edges of the graph.  Intuitively, this number will give a measure of how "central" a vertex is in a graph. In other words, if a vertex has a relatively high degreeCentrality, it is connected to more vertexSet than other vertexSet of G, so it is more central or a bottleneck in the graph. Note that the sum of the degree centralities must be 1.
        Example
            L = apply(vertexSet pathGraph 5, i -> degreeCentrality (pathGraph 5, i))
            sum L
    SeeAlso
        center
        distance
        degree
///

--degreeIn
doc ///
    Key
        degreeIn
        (degreeIn, Digraph, Thing)
    Headline
        returns the "in-degree" of a vertex in a digraph
    Usage
        x = degreeIn (D, v)
    Inputs
        D:Digraph
        v:Thing
            a vertex of D
    Outputs
        x:ZZ
    Description
        Text
            In a directed graph, we define the degree into a vertex or the "in-degree" of a vertex to be the number of parents of that vertex. Intuitively, this give the number of edges that point into the vertex.
        Example
            D = digraph({1,2,3,4},{{1,2},{2,3},{3,4},{4,2}});
            degreeIn(D, 2)
    SeeAlso
        degree
        degreeOut
        parents
///

--degreeOut
doc ///
    Key
        degreeOut
        (degreeOut, Digraph, Thing)
    Headline
        returns the "out-degree" of a vertex in a digraph
    Usage
        x = degreeIn (D, v)
    Inputs
        D:Digraph
        v:Thing
            a vertex of D
    Outputs
        x:ZZ
    Description
        Text
            In a directed graph, we define the degree out of a vertex or the "out-degree" of a vertex to be the number of children of that vertex. Intuitively, this give the number of edges pointing out of a vertex to another vertex.
        Example
            D = digraph({1,2,3,4},{{1,2},{2,3},{3,4},{4,2}});
            degreeOut(D, 2)
    SeeAlso
        degree
        degreeIn
        children
///

--density
doc ///
    Key
        density
        (density, Graph)
    Headline
        computes the density of a graph
    Usage
        d = density G
    Inputs
        G:Graph
    Outputs
        d:QQ
            the density of G
    Description
        Text
            A dense graph has a high edge to vertex ratio, whereas a sparse graph has a low edge to vertex ratio.  Density is equal to 2*|E| divided by |V|*(|V|-1).  A complete graph has density 1; the minimal density of any graph is 0.
        Example
            G = graph({{1,2},{2,3},{3,4},{4,2},{1,4}},EntryMode=>"edges");
            density G
    SeeAlso
        degreeCentrality
///

--depthFirstSearch
doc ///
    Key
        depthFirstSearch
        (depthFirstSearch, Digraph)
    Headline
        runs a depth first search on the digraph or digraph and returns the discovery time and finishing time for each vertex in the digraph
    Usage
        dfs = depthFirstSearch D
        dfs = depthFirstSearch G
    Inputs
        D:Digraph
        G:Graph
    Outputs
        dfs:HashTable
            A hash table with keys discoveryTime and finishingTime, whose values are hash tables containing for each vertex the discovery time and finishing time, respectively.
    Description
        Text
            A depth first search begins at the first vertex of a graph as a root and searches as far as possible along one branch from that root before backtracking to the next branch to the right. Discovery time denotes the order in which the vertex was searched first; finishing time denotes the time in which the vertex's descendents were all finished.
        Example
            D = digraph ({{0,1},{1,3},{1,4},{4,7},{4,8},{0,2},{2,5},{2,6}},EntryMode=>"edges")
            dfs = depthFirstSearch D
            G = cycleGraph 6
            dfs = depthFirstSearch G
    SeeAlso
        breadthFirstSearch
        topologicalSort
///

--descendants
doc ///
    Key
        descendants
        (descendants, Digraph, Thing)
    Headline
        returns the descendants of a digraph
    Usage
        L = descendants (D, v)
    Inputs
        D:Digraph
        v:Thing
            a vertex of the digraph
    Outputs
        L:Set
            a set of all the descendants of v
    Description
        Text
            The descendants of a directed graph are all the vertexSet u of D such that u is reachable from v.
            Another way to more intuitively see what the descendants are is to see the descandants of a vertex v
            can be found by first taking the children of v. Then if you take the children of each of the
            children, and continue the process until the list stops growing, this will form all the descandants of v.
        Example
            D = digraph({a,b,c,d,e},{{a,b},{b,c},{b,d},{e,b}});
            descendants (D, a)
    SeeAlso
        children
        isReachable
        nondescendants
///

--diameter
doc ///
    Key
        (diameter, Graph)
    Headline
        Computes the diameter of a graph
    Usage
        d = diameter G
    Inputs
        G:Graph
    Outputs
        d:ZZ
            The diameter of G
    Description
        Text
            The diameter of a graph is the maximum of the distances between all the vertexSet of G.  If the graph is not connected, the diameter is infinity.
        Example
            G = graph({{1,2},{2,3},{3,4}},EntryMode=>"edges");
            d = diameter G
            G = graph({1,2,3,4},{{2,3},{3,4}});
            d = diameter G
    SeeAlso
        distance
        distanceMatrix
///

--distance
doc ///
    Key
        distance
        (distance, Digraph, Thing, Thing)
    Headline
        Computes the distance between two vertexSet in a graph
    Usage
        d = distance(G,v,u)
    Inputs
        G:Graph
        v:Thing
        u:Thing
    Outputs
        d:ZZ
            The distance between vertexSet v and u
    Description
        Text
            The distance between two vertexSet is calculated as the number of edges in the shortest path between the two vertexSet.  If the two vertexSet are not connected, the distance between them is infinity by convention.
        Example
            G = graph({{1,2},{2,3},{3,4}},EntryMode=>"edges");
            d = distance (G,1,4)
            G = graph({1,2,3,4},{{2,3},{3,4}});
            d = distance(G, 1, 4)
    SeeAlso
        (diameter, Graph)
        distanceMatrix
///

--distanceMatrix
doc ///
    Key
        distanceMatrix
        (distanceMatrix, Digraph)
    Headline
        Computes the distance matrix of a digraph
    Usage
        M = distanceMatrix(G)
    Inputs
        G:Digraph
    Outputs
        M:Matrix
            the distance matrix of G
    Description
        Text
            The distance matrix is the matrix where entry M_(i,j) corresponds to the distance between vertex indexed i and vertex indexed j in the specified graph.  If the distance between two vertexSet is infinite (i.e. the vertexSet are not connected) the matrix lists the distance as -1.
        Example
            G = graph({{1,2},{2,3},{3,4}},EntryMode=>"edges");
            d = distanceMatrix G
            G = digraph({1,2,3,4},{{2,3},{3,4}},EntryMode=>"edges");
            d = distanceMatrix G
    SeeAlso
        (diameter, Graph)
        distance
///

--eccentricity
doc ///
    Key
        eccentricity
        (eccentricity,Graph,Thing)
    Headline
        Returns the eccentricity of a vertex of a graph
    Usage
        k = eccentricity (G, v)
    Inputs
        G:Graph
            G must be a connected graph
        v:Thing
            v needs to be a vertex of the graph
    Outputs
        k:ZZ
    Description
        Text
            The eccentricity of a vertex is the maximal distance between the given vertex and any other vertex in the graph. It gives a measure of how far away a vertex is from the rest of the graph.
        Example
            eccentricity(pathGraph 5, 2)
            eccentricity(pathGraph 5, 1)
            eccentricity(pathGraph 5, 0)
    SeeAlso
        distance
        radius
        isConnected
///

--edgeIdeal
doc ///
    Key
        edgeIdeal
        (edgeIdeal, Graph)
    Headline
        returns the edge ideal of a graph
    Usage
        I = edgeIdeal G
    Inputs
        G:Graph
    Outputs
        I:Ideal
            the edge ideal of a graph G
    Description
        Text
            The edge ideal of a graph G is the ideal generated by the minimal nonfaces of the independence complex of G.
        Example
            G = graph({{1, 2}, {1, 3}, {2, 3}, {3, 4}},EntryMode=>"edges");
            edgeIdeal G
    SeeAlso
        coverIdeal
        independenceComplex
///

--expansion
doc ///
    Key
        expansion
        (expansion, Graph)
    Headline
        returns the expansion of a graph
    Usage
        h=expansion G
    Inputs
        G:Graph
    Outputs
        h:QQ
            the expansion of a graph G
    Description
        Text
            The expansion of a subset S of vertices is the ratio of
            the number of edges leaving S and the size of S. The
            (edge) expansion of a graph G is the minimal expansion of
            all not too large subsets of the vertex set. The expansion
            of a disconnected graph is 0 whereas the expansion of the
            complete graph on n vertices is ceiling(n/2)
        Example
            G = graph({{1, 2}, {1, 3}, {2, 3}, {3, 4}},EntryMode=>"edges");
            expansion G
            expansion pathGraph 7
            
///

--findPaths
doc ///
    Key
        findPaths
        (findPaths, Digraph, Thing, ZZ)
    Headline
        finds all the paths in a digraph of a given length starting at a given vertex
    Usage
        F = findPaths(D,v,l)
    Inputs
        D:Digraph
        v:Thing
            vertex at which paths start
        l:ZZ
            length of desired paths
    Outputs
        F:List
            list of paths starting at v of length l
    Description
        Text
            The method will return a list of all the paths of length l starting at a vertex v in the digraph D.  The method is compatible for graphs with loops or cycles, as variables can be repeatedly visited in paths.
        Example
            D = digraph(toList(1..5), {{1,2},{1,3},{2,5},{2,4}})
            F = findPaths(D,1,2)
            D = digraph(toList(a..d), {{a,c},{a,b},{b,b},{b,d}})
            F = findPaths(D,a,100)
    SeeAlso
        distance
        distanceMatrix
///

--floydWarshall
doc ///
    Key
        floydWarshall
        (floydWarshall, Digraph)
    Headline
        runs the Floyd-Warshall algorithm on a digraph to determine the minimum distance from one vertex to another in the digraph
    Usage
        F = floydWarshall D
    Inputs
        D:Digraph
    Outputs
        F:HashTable
            A hash table with the keys representing pairs of vertexSet (u,v) and the value being the distance from u to v.
    Description
        Text
            The distance from one vertex u to another v in digraph D is the minimum number of edges forming a path from u to v.  If v is not reachable from u, the distance is infinity; if u = v, the distance is 0.
        Example
            D = digraph({{0,1},{0,2},{2,3},{3,4},{4,2}},EntryMode=>"edges")
            F = floydWarshall D
    SeeAlso
        distance
        distanceMatrix
///

--forefathers
doc ///
    Key
        forefathers
        (forefathers,Digraph,Thing)
	symbol foreFathers
    Headline
        returns the forefathers of a digraph
    Usage
        L = forefathers (D, v)
    Inputs
        D:Digraph
        v:Thing
            v must be a vertex of D
    Outputs
        L:Set
            a set of all the forefathers of v in D
    Description
        Text
            The forefathers of a vertex v in a digraph D are all the vertexSet u in D such that v is reachable from u. Another way to more intuitively see what the forefathers are is to see the forefathers of a vertex v can be found by first taking the parents of v. Then if you find the parents of each of the parents of v, and continue the process until the list stops growing, this will form all the descandants of v.
        Example
            D = digraph({a,b,c,d,e},{{a,b},{b,c},{b,d},{e,b}});
            forefathers (D, d)
    Caveat
        The forefathers of a vertex in a digraph are more commonly known as the ancestors. But ancestors is an entirely different function in Macaulay 2, so forefathers is the convention we will use
    SeeAlso
        isReachable
        parents
///

--girth
doc ///
    Key
        girth
        (girth, Graph)
    Headline
        A method for computing the girth of a graph
    Usage
        g = girth G
    Inputs
        G:Graph
    Outputs
        g:ZZ
            the girth of G
    Description
        Text
            This method computes the girth (the smallest n such that G contains an n-cycle) of any graph. If the graph has no n-cycle as a subgraph, the output will be infinity.
        Example
            girth completeGraph 6
            girth pathGraph 6
///

--independenceComplex
doc ///
    Key
        independenceComplex
        (independenceComplex, Graph)
    Headline
        constructs the independence complex of a graph
    Usage
        indG = independenceComplex G
    Inputs
        G:Graph
    Outputs
        indG:SimplicialComplex
            the independence complex of G
    Description
        Text
            The independence complex of a graph G is the set of all the independent sets of G.
        Example
            G = graph({{1,2},{2,3},{3,4},{4,5}},EntryMode=>"edges");
            independenceComplex G
    SeeAlso
        independenceNumber
        cliqueComplex
///

--independenceNumber
doc ///
    Key
        independenceNumber
        (independenceNumber, Graph)
    Headline
        computes the independence number of a graph
    Usage
        alpha = independenceNumber G
    Inputs
        G:Graph
    Outputs
        alpha:ZZ
            the independence number of G
    Description
        Text
            The independence number of a graph G is the maximum number of vertexSet in any independent set of G.
        Example
            G = graph({{1,2},{2,3},{3,4},{4,5}},EntryMode=>"edges");
            independenceNumber G
    SeeAlso
        independenceComplex
        cliqueNumber
///

--leaves
doc ///
    Key
        leaves
        (leaves, Graph)
    Headline
        lists the leaves of a tree graph
    Usage
        L = leaves G
    Inputs
        G:Graph
    Outputs
        L:List
            list of leaves of a tree graph
    Description
        Text
            A vertex of a tree graph is a leaf if the degree of the vertex is 1
        Example
            G = graph({{1,2},{1,3},{3,4},{3,5}},EntryMode=>"edges");
            leaves G;
    SeeAlso
        isForest
        isLeaf
        isTree
///

--lowestCommonAncestors
doc ///
    Key
        lowestCommonAncestors
        (lowestCommonAncestors, Digraph, Thing, Thing)
    Headline
        determines the lowest common ancestors between two vertexSet
    Usage
        A = lowestCommonAncestors(D,u,v)
    Inputs
        D:Digraph
        u:Thing
        v:Thing
    Outputs
        A:List
            A list of the lowest common ancestors of u and v
    Description
        Text
            The lowest common ancestors between two vertexSet are the vertexSet that are ancestors of both u and v and are the shortest distance from the vertexSet in the digraph.
        Example
            D = digraph({{0,1},{0,2},{2,3},{3,4},{4,2}},EntryMode=>"edges");
            A = lowestCommonAncestors(D,1,3)
    SeeAlso
        reverseBreadthFirstSearch
        forefathers
///

-- neighbors
doc ///
    Key
        neighbors
        (neighbors, Graph, Thing)
    Headline
        returns the neighbors of a vertex in a graph
    Usage
        N = neighbors(G,v)
    Inputs
        G:Graph
        v:Thing
    Outputs
        N:Set
            the neighbors of vertex v in graph G
    Description
        Text
            The neighbors of a vertex v are all the vertexSet of G adjacent to v. That is, if u is a neighbor to v, {v,u} is an edge of G.
        Example
            G = graph({1,2,3,4},{{2,3},{3,4}});
            neighbors(G,3)
    SeeAlso
        nonneighbors

///

--nondescendants
doc ///
    Key
        nondescendants
        (nondescendants, Digraph, Thing)
    Headline
        returns the nondescendants of a vertex of a digraph
    Usage
        L = nondescendants (D, v)
    Inputs
        D:Digraph
        v:Thing
            a vertex of the digraph
    Outputs
        L:Set
            a set of all the nondescendants of v
    Description
        Text
            The nondescendants of a directed graph are all the vertexSet u of D such that u is not reachable from v.
        Example
            D = digraph({a,b,c,d,e},{{a,b},{b,c},{b,d},{e,b}});
            nondescendants (D, d)
    SeeAlso
        children
        descendants
        isReachable
///

--nonneighbors
doc ///
    Key
        nonneighbors
        (nonneighbors, Graph, Thing)
    Headline
        returns the non-neighbors of a vertex in a graph
    Usage
        N = nonneighbors(G,v)
    Inputs
        G:Graph
        v:Thing
    Outputs
        N:Set
            the non-neighbors of vertex v in graph G
    Description
        Text
            The non-neighbors of a vertex v are all the vertexSet of G that are not adjacent to v. That is, if u is a non-neighbor to v, {u,v} is not an edge in G.
        Example
            G = graph({1,2,3,4},{{2,3},{3,4}});
            nonneighbors(G,2)
    SeeAlso
        neighbors
///

--numberOfComponents
doc ///
    Key
        numberOfComponents
        (numberOfComponents, Graph)
    Headline
        computes the number of connected components of a graph
    Usage
        n = numberOfComponents G
    Inputs
        G:Graph
    Outputs
        n:ZZ
            the number of connected components of G
    Description
        Text
            A connected component is a list of vertexSet of a graph that are connected, i.e. there exists a path of edges between any two vertexSet in the component.
        Example
            G = graph(toList(1..8),{{1,2},{2,3},{3,4},{5,6}});
            numberOfComponents G;
    SeeAlso
        isConnected
        connectedComponents
///

--numberOfTriangles
doc ///
    Key
        numberOfTriangles
        (numberOfTriangles, Graph)
    Headline
        counts how many subtriangles are present in a graph
    Usage
        t = numberOfTriangles G
    Inputs
        G:Graph
    Outputs
        t:ZZ
            number of subtriangles in a graph
    Description
        Text
            A triangle is formed by three vertexSet which are mutually adjacent.
        Example
            G = graph({{1,2},{2,3},{3,1},{3,4},{2,4}},EntryMode=>"edges");
            numberOfTriangles G
    SeeAlso
        hasOddHole
        isCyclic
        inducedSubgraph
///

--parents
doc ///
    Key
        parents
        (parents, Digraph, Thing)
    Headline
        returns the parents of a vertex on a digraph
    Usage
        P = parents (D, v)
    Inputs
        D:Digraph
        v:Thing
            the vertex whose parents we want to find
    Outputs
        P:Set
            the parents of v
    Description
        Text
            The parents of a vertex v in a digraph D are all the vertexSet u in D such that {u,v} is an edge of D. In other words, the parents of v are all the vertexSet that have edges coming out of them that point at v.
        Example
            D = digraph({a,b,c,d,e},{{a,b},{b,c},{b,d},{e,b}});
            parents (D, b)
    SeeAlso
        forefathers
///

--radius
doc ///
    Key
        radius
        (radius,Graph)
    Headline
        Returns the radius of a graph
    Usage
        r = radius G
    Inputs
        G:Graph
    Outputs
        r:ZZ
    Description
        Text
            The radius of a graph is defined to be the minimum of the eccentricities of the vertexSet, i.e, the smallest number k such that for some vertex v, the distance between v and another vertex is less than or equal to k.
        Example
            radius completeGraph 5
            radius pathGraph 5
            radius graphLibrary "dart"
    SeeAlso
        eccentricity
        barycenter
        distance
///

--reachable
doc ///
    Key
        reachable
        (reachable,Digraph,List)
        (reachable,Digraph,Set)
    Headline
        Returns the vertices reachable in a digraph from a given collection of vertices
    Usage
        Rl = reachable(D, L)
        Rs = reachable(D, S)
    Inputs
        D:Digraph
        L:List
            a list of vertices
        S:Set
            a set of vertices
    Outputs
        Rl:List
            the list of reachable vertices
        Rs:Set
            the set of reachable vertices
    Description
        Text
            Given a collection of vertices of a digraph, the reachable vertices are those
            that are on a path away from a vertices in the collection.
    SeeAlso
        descendants
        isReachable
///

--reverseBreadthFirstSearch
doc ///
    Key
        reverseBreadthFirstSearch
        (reverseBreadthFirstSearch, Digraph, Thing)
    Headline
        runs a reverse breadth first search on the digraph and returns a list of the vertexSet in the order they were discovered
    Usage
        bfs = reverseBreadthFirstSearch(D,v)
    Inputs
        D:Digraph
    Outputs
        bfs:List
            A list of the vertexSet of D in order discovered by the breadth first search
    Description
        Text
            A reverse breadth first search first searches the specified of a digraph, followed by that vertex's parents, followed by their parents, etc, until all the ancestors are exhausted, and returns a list, with the index of the item of the list signifying the     depth level of the result, of the vertexSet in order searched.
        Example
            D = digraph ({{0,1},{0,2},{2,3},{3,4},{4,2}},EntryMode=>"edges")
            bfs = reverseBreadthFirstSearch(D,2)
    SeeAlso
        breadthFirstSearch
        depthFirstSearch
        topologicalSort
///

--sinks
doc ///
    Key
        sinks
        (sinks, Digraph)
    Headline
        returns the sinks of a digraph
    Usage
        L = sinks D
    Inputs
        D:Digraph
            digraph whose sinks we are searching for
    Outputs
        L:List
            list of all the sinks (if there are any)
    Description
        Text
            A sink of a Digraph D is a vertex of D that has no children. That is, v is a sink of D if and only if there are only edges pointing into v; none can be pointing out (there is no edge of the form (v,u)).
        Example
            D = digraph({a,b,c,d,e},{{a,b},{b,c},{b,d},{e,b}});
            sinks D
    SeeAlso
        sources
        isSink
///

--sources
doc ///
    Key
        sources
        (sources, Digraph)
    Headline
        returns the sources of a digraph
    Usage
        L = sources D
    Inputs
        D:Digraph
            digraph whose sources we are searching for
    Outputs
        L:List
            list of all the sources (if there are any)
    Description
        Text
            A source of a Digraph D is a vertex of D that has no parents. That is, v is a source of D if and only if there are only edges pointing from v; none can be pointing into v (there is no edge of the form (v,u)).
        Example
            D = digraph({a,b,c,d,e},{{a,b},{b,c},{b,d},{e,b}});
            sources D
    SeeAlso
        sinks
        isSource
///

--spectrum
doc ///
    Key
        spectrum
        (spectrum, Graph)
    Headline
        Returns the spectrum of a graph
    Usage
        L = spectrum G
    Inputs
        G:Graph
    Outputs
        L:List
    Description
        Text
            The spectrum of a graph G is the set of the eigenvalues of the adjacency matrix A corresponding to G. For simple graphs, these eigenvalues are all real since A must be symmetric. The user should be aware that Macaulay 2 does not give exact values for these eigenvalues, they are numerical approximations, but it is still a good tool to use to check if two graphs are isomorphic; isomorphic graphs share the same spectrum although the converse is not necessarily true.
        Example
            spectrum completeGraph 6
            spectrum graphLibrary "petersen"
///

--vertexCoverNumber
doc ///
    Key
        vertexCoverNumber
        (vertexCoverNumber, Graph)
    Headline
        returns the vertex cover number of a graph
    Usage
        v = vertexCoverNumber G
    Inputs
        G:Graph
    Outputs
        v:ZZ
            the vertex cover number of graph G
    Description
        Text
            The vertex cover number is the minimum length of the set of vertex covers of a graph.
        Example
            G = graph({{1,2},{1,3},{1,4},{2,3}},EntryMode=>"edges");
            vertexCoverNumber G
    SeeAlso
        vertexCovers
        coverIdeal
///

--vertexCovers
doc ///
    Key
        vertexCovers
        (vertexCovers, Graph)
    Headline
        returns a list of the minimal vertex covers of a graph
    Usage
        V = vertexCovers G
    Inputs
        G:Graph
    Outputs
        V:List
            the list of minimal vertex covers of graph G
    Description
        Text
            A vertex cover of G is a set of vertexSet which intersects with every edge of G. In other words, L is a vertex cover of a graph if and only if their does not exist an edge {u,v} such that both u and v are not in L.
        Example
            G = graph({{1,2},{1,3},{1,4},{2,3}},EntryMode=>"edges");
            vertexCovers G
    SeeAlso
        vertexCoverNumber
        coverIdeal
///

------------------------------------------
--Boolean Methods
------------------------------------------

--hasEulerianTrail
doc ///
    Key
        hasEulerianTrail
        (hasEulerianTrail, Graph)
        (hasEulerianTrail, Digraph)
    Headline
        determines whether a graph or a digraph has an Eulerian trail
    Usage
        E = hasEulerianTrail G
        E = hasEulerianTrail D
    Inputs
        G:Graph
        D:Digraph
    Outputs
        E:Boolean
            whether G or D has an Eulerian trail
    Description
        Text
            A graph has an Eulerian trail if there is a path in the graph that visits each edge exactly once.  A digraph has a Eulerian trail if there is a directed path in the graph that visits each edge exactly once.  An Eulerian trail is also called an Eulerian path.  Unconnected graphs can have a Eulerian trail, but all vertices of degree greater than 0 of a graph (or all vertices of degree greater than 0 in the underlying graph of a digraph) must belong to a single connected component.
        Example
            G = cycleGraph 5;
            hasEulerianTrail G
            D = digraph(toList(1..4), {{1,2},{2,3},{3,4}});
            hasEulerianTrail D
    SeeAlso
        isEulerian
///


--hasOddHole
doc ///
    Key
        hasOddHole
        (hasOddHole, Graph)
    Headline
        checks whether a graph has a odd hole
    Usage
        oddHole = hasOddHole G
    Inputs
        G:Graph
    Outputs
        oddHole:Boolean
            whether the graph has an odd hole
    Description
        Text
            A graph has an odd hole if it has an induced cycle that is odd and has length of at least 5.
        Example
            G = graph({{1,2},{2,3},{3,4},{4,5}},EntryMode=>"edges");
            hasOddHole G
    SeeAlso
        cycleGraph
        isPerfect
        isChordal
///

--isBipartite
doc ///
    Key
        isBipartite
        (isBipartite, Graph)
    Headline
        determines whether a graph is bipartite
    Usage
        b = isBipartite G
    Inputs
        G:Graph
    Outputs
        b:Boolean
            whether graph G is bipartite
    Description
        Text
            A graph is bipartite if it has a chromatic number less than or equal to 2.
        Example
            G = graph({{0,1},{1,2},{2,4},{3,4},{4,5}},EntryMode=>"edges");
            isBipartite G
    SeeAlso
        bipartiteColoring
///

--isCM
doc ///
    Key
        isCM
        (isCM, Graph)
    Headline
        determines if a graph is Cohen-Macaulay
    Usage
        c = isCM G
    Inputs
        G:Graph
    Outputs
        c:Boolean
            whether the graph is Cohen-Macaulay
    Description
        Text
            This uses the edge ideal notion of Cohen-Macaulayness; a graph is called C-M if and only if its edge ideal is C-M.
        Example
            G = graph({{1,2},{1,3},{1,4},{2,5},{5,3},{3,2}},EntryMode=>"edges");
            isCM G
    SeeAlso
        edgeIdeal
///

--isChordal
doc ///
    Key
        isChordal
        (isChordal, Graph)
    Headline
        checks whether a graph is chordal
    Usage
        c = isChordal G
    Inputs
        G:Graph
    Outputs
        c:Boolean
            whether the graph is chordal
    Description
        Text
            A graph is chordal if its cycles with at least four vertices contain at least one edge between two vertices which are not adjacent in the cycle.
        Example
            G = graph({{1,2},{2,3},{3,4},{4,1},{2,4}}, EntryMode => "edges");
            isChordal G
    SeeAlso
        cycleGraph
        isPerfect
        hasOddHole
///

--isConnected
doc ///
    Key
        isConnected
        (isConnected, Graph)
    Headline
        determines whether a graph is connected
    Usage
        C = isConnected G
    Inputs
        G:Graph
    Outputs
        C:Boolean
            whether a graph is connected
    Description
        Text
            A graph is connected when there exists a path of edges between any two vertices in the graph.
        Example
            G = graph({{1,2},{2,3},{3,4},{5,6}},EntryMode=>"edges");
            isConnected G;
    SeeAlso
        connectedComponents
        numberOfComponents
///

--isCyclic
doc ///
    Key
        isCyclic
        (isCyclic, Graph)
    Headline
        determines whether a graph is cyclic
    Usage
        c = isCyclic G
    Inputs
        G:Graph
    Outputs
        c:Boolean
            whether a graph is cyclic
    Description
        Text
            A graph is cyclic if it is composed of vertices connected by a single chain of edges.
        Example
            G = graph({{1,2},{2,3},{3,1}},EntryMode=>"edges");
            isCyclic G
            G = graph({{1,2},{2,3},{3,4}},EntryMode=>"edges");
            isCyclic G
    SeeAlso
        cycleGraph
///

--isCyclic
doc ///
    Key
        (isCyclic, Digraph)
    Headline
        determines whether a digraph is cyclic
    Usage
        C = isCyclic D
    Inputs
        D:Digraph
    Outputs
        C:Boolean
            Whether the digraph D is cyclic
    Description
        Text
            A digraph is cyclic if it contains a cycle, i.e. for some vertex v of D, by following the edges of D, one can return to v.
        Example
            D = digraph ({{0,1},{0,2},{2,3},{3,4},{4,2}},EntryMode=>"edges")
            isCyclic D
    SeeAlso
        isCyclic
        --the isCyclic method for graphs!
///

--isEulerian
doc ///
    Key
        isEulerian
        (isEulerian, Graph)
        (isEulerian, Digraph)
    Headline
        determines if a graph or digraph is Eulerian
    Usage
        E = isEulerian G
        E = isEulerian D
    Inputs
        G:Graph
        D:Digraph
    Outputs
        E:Boolean
            whether G or D is Eulerian
    Description
        Text
            A graph is Eulerian if it has a path in the graph that visits each vertex exactly once.  A digraph is Eulerian if it has a directed path in the graph that visits each vertex exactly once.  Such a path is called an Eulerian circuit.  Unconnected graphs can be Eulerian, but all vertices of degree greater than 0 of a graph (or all vertices of degree greater than 0 in the underlying graph of a digraph) must belong to a single connected component.
        Example
            bridges = graph ({{0,1},{0,2},{0,3},{1,3},{2,3}}, EntryMode => "edges");
            E = isEulerian bridges
            D = digraph(toList(1..4), {{2,3},{3,4},{4,2}});
            E = isEulerian D
    SeeAlso
        hasEulerianTrail
///

--isForest
doc ///
    Key
        isForest
        (isForest, Graph)
    Headline
        determines whether a graph is a forest
    Usage
        f = isForest G
    Inputs
        G:Graph
    Outputs
        f:Boolean
            whether a graph is a forest
    Description
        Text
            A graph is a forest if it is a disjoint collection of trees.
        Example
            G = graph({{1,2},{1,3},{6,4},{4,5}},EntryMode=>"edges");
            isForest G
    SeeAlso
        isTree
        isLeaf
        leaves
///

--isLeaf
doc ///
    Key
        isLeaf
        (isLeaf, Graph, Thing)
    Headline
        determines whether a vertex is a leaf
    Usage
        l = isLeaf(G,v)
    Inputs
        G:Graph
        v:Thing
    Outputs
        l:Boolean
            whether vertex v of graph G is a leaf
    Description
        Text
            A vertex of a tree graph is a leaf if its degree is 1
        Example
            G = graph({{1,2},{1,3},{3,4},{3,5}},EntryMode=>"edges");
            isLeaf(G,2)
    SeeAlso
        isForest
        isTree
        leaves
///

--isPerfect
doc ///
    Key
        isPerfect
        (isPerfect, Graph)
    Headline
        checks whether a graph is perfect
    Usage
        p = isPerfect G
    Inputs
        G:Graph
    Outputs
        p:Boolean
            whether the graph is perfect
    Description
        Text
            A perfect graph is a graph where the chromatic number of every induced subgraph of G is equal to the clique number in that subgraph.
        Example
            G = graph {{1,2},{1,3},{1,4},{2,5},{5,3},{3,2}};
            isPerfect G
    SeeAlso
        chromaticNumber
        cliqueNumber
        hasOddHole
///

--isReachable
doc ///
    Key
        isReachable
        (isReachable, Digraph, Thing, Thing)
    Headline
        checks if a vertex u is reachable from a vertex v
    Usage
        r = isReachable(D, u, v)
    Inputs
        D:Digraph
        u:Thing
            this is the vertex that we are attempting to reach
        v:Thing
            this is the vertex that we are starting from
    Outputs
        r:Boolean
            whether or not us is reachable from v
    Description
        Text
            In a Digraph D, a vertex u of D is reachable from another vertex v of D if u is a descendant of v. Alternatively, u is reachable from v if there is some set of vertices u_0, ... , u_n such that u_n = u and u_0 = v and (u_i, u_i+1) is and edge of D for all i from 0 to n-1.
        Example
            D = digraph({a,b,c,d,e},{{a,b},{b,c},{b,d},{e,b}});
            isReachable(D, e, a)
            isReachable(D, d, e)
    SeeAlso
        descendants
        forefathers
///

--isRegular
doc ///
    Key
        isRegular
        (isRegular, Graph)
    Headline
        determines whether a graph is regular
    Usage
        r = isRegular G
    Inputs
        G:Graph
    Outputs
        r:Boolean
            whether the graph G is regular or not.
    Description
        Text
            A graph is regular if all of its vertices have the same degree.
        Example
            G = cycleGraph 5;
            isRegular G
    SeeAlso
        completeGraph
        cycleGraph
///

--isRigid
doc ///
    Key
        isRigid
        (isRigid,Graph)
    Headline
        checks if a graph is rigid
    Usage
        r = isRigid G
    Inputs
        G:Graph
    Outputs
        r:Boolean
    Description
        Text
            A drawing of a graph is rigid in the plane if any continuous motion 
	    of the vertices that preserve edge lengths must preserve the distance 
	    between every pair of vertices.  A graph is generically rigid if any 
	    drawing of the graph with vertices in general position is rigid. This 
	    method uses Laman's Theorem to determine if a graph is rigid or not.
        Example
            G = cycleGraph 4;
            isRigid G
	    G' = addEdges' (G, {{1,1},{3,1}})
            isRigid G'
///


--isSimple
doc ///
    Key
        isSimple
        (isSimple,Graph)
    Headline
        checks if a graph is simple
    Usage
        r = isSimple G
    Inputs
        G:Graph
    Outputs
        r:Boolean
    Description
        Text
            A graph is said to be simple if it has a maximum of one edge between each vertex, contains no loops (vertices connected to themselves by edges), and is undirected. Since the Graph Type does not allow for multiple edges and directed edges, it is sufficient to check that the graph has no loops.
        Example
            G = cycleGraph 5;
            isSimple G
            G' = addEdge (G, set {1,1});
            isSimple G'
///

--isSink
doc ///
    Key
        isSink
        (isSink, Digraph, Thing)
    Headline
        determines if a vertex of a digraph is a sink or not
    Usage
        r = isSink (D, v)
    Inputs
        D:Digraph
        v:Thing
            the vertex being texted
    Outputs
        r:Boolean
            whether the vertex v is a sink or not
    Description
        Text
            A vertex v of a Digraph D is a sink if v has no children
        Example
            D = digraph({a,b,c,d,e},{{a,b},{b,c},{b,d},{e,b}});
            isSink (D,b)
            isSink (D,d)
    SeeAlso
        sinks
///

--isSource
doc ///
    Key
        isSource
        (isSource, Digraph, Thing)
    Headline
        determines if a vertex of a digraph is a source or not
    Usage
        r = isSource (D, v)
    Inputs
        D:Digraph
        v:Thing
            the vertex being texted
    Outputs
        r:Boolean
            whether the vertex v is a source or not
    Description
        Text
            A vertex v of a Digraph D is a source if v has no parents
        Example
            D = digraph({a,b,c,d,e},{{a,b},{b,c},{b,d},{e,b}});
            isSource (D,c)
            isSource (D,e)
    SeeAlso
        sources
///

--isStronglyConnected
doc ///
    Key
        isStronglyConnected
        (isStronglyConnected,Digraph)
    Headline
        checks if a digraph is strongly connected
    Usage
        r = isStronglyConnected D
    Inputs
        D:Digraph
    Outputs
        r:Boolean
    Description
        Text
            A digraph is said to be strongly connected if for each vertex u of D, any other vertex of D is reachable from u. An equivalent definition is that D is strongly connected if the distance matrix of D has only positive terms in the non-diagonal entries.
        Example
            D = digraph({1,2,3,4},{{1,2},{2,3},{3,4},{4,2}});
            isStronglyConnected D
            D' = digraph({1,2,3,4},{{1,2},{2,1},{2,3},{3,4},{4,2}});
            isStronglyConnected D'
    SeeAlso
        isWeaklyConnected
        distanceMatrix
        isReachable
///

--isTree
doc ///
    Key
        isTree
        (isTree, Graph)
    Headline
        determines whether a graph is a tree
    Usage
        t = isTree G
    Inputs
        G:Graph
    Outputs
        t:Boolean
            whether a graph is a tree
    Description
        Text
            A graph is a tree if any two vertices are connected by a unique path of edges.
        Example
            G = graph({{1,2},{1,3},{3,4},{3,5}},EntryMode=>"edges");
            isTree G
    SeeAlso
        isForest
        isLeaf
        leaves
///

--isWeaklyConnected
doc ///
    Key
        isWeaklyConnected
        (isWeaklyConnected,Digraph)
    Headline
        checks if a digraph is weakly connected
    Usage
        r = isWeaklyConnected D
    Inputs
        D:Digraph
    Outputs
        r:Boolean
    Description
        Text
            A digraph is said to be weakly connected if the underlying graph of D, that is, the graph formed by taking away direction from the edges so each edge becomes "2-way" again, is connected.
        Example
            D = digraph({1,2,3,4},{{1,2},{2,3},{3,4},{4,2}});
            isWeaklyConnected D
    SeeAlso
        weaklyConnectedComponents
        isStronglyConnected
///

------------------------
--Graph operations
------------------------

--cartesianProduct
doc///
    Key
        cartesianProduct
        (cartesianProduct, Graph, Graph)
    Headline
        Computes the cartesian product of two graphs
    Usage
        F = cartesianProduct(G,H)
    Inputs
        G:Graph
        H:Graph
    Outputs
        F:Graph
            The Cartesian Product of G and H
    Description
        Text
            This method will take in any two graphs and output the cartesian product of the two graphs. The vertex set of this new graph is the cartesian product of the vertex sets of the two input graphs.  The keys for each vertex will be output as a sequence. Any two vertices (u,u') and (v,v') are adjacent in the cartesian product of G and H if and only if either u = v and u' is adjacent with v' in H, or u' = v' and u is adjacent with v in G.
        Example
            G = graph({1,2},{{1,2}});
            H = graph({3,4,5},{{3,4},{4,5}});
            G' = cartesianProduct(G,H)
    SeeAlso
        strongProduct
        directProduct
        graphComposition
///


--directProduct
doc ///
    Key
        directProduct
        (directProduct, Graph, Graph)
	symbol tensorProduct
    Headline
        Computes the direct product of two graphs
    Usage
        F = directProduct(G,H)
    Inputs
        G:Graph
        H:Graph
    Outputs
        F:Graph
            The Direct Product of G and H
    Description
        Text
            This method will take in any two graphs and output the direct product of these two graphs. The vertex set of the direct product of G and H is the cartesian product of G and H's vertex sets. The keys for each vertex will be output as a sequence to represent this.  Any two vertices (u,u') and (v,v') form an edge in the direct product of G and G if and only if u' is adjacent with v' and u is adjacent with v in the original graphs.
        Example
            G = graph({1,2},{{1,2}});
            H = graph({3,4,5},{{3,4},{4,5}});
            G'= directProduct(G,H)
    SeeAlso
        graphComposition
        strongProduct
        cartesianProduct
///

-- disjointUnion
doc ///
    Key
        disjointUnion
        (disjointUnion, List)
    Headline
        Returns the disjoint union of a list of graphs.
    Usage
        G = disjointUnion L
    Inputs
        L:List
    Outputs
        G:Graph
            The disjoint union of the graphs in list L
    Description
        Text
            The disjoint union of a list of graphs is the graph constructed from the unions of their respective vertex sets and edge sets. By default, the vertex set of the union will be listed as sequences.
        Example
            A = graph({{1,2},{2,3}},EntryMode=>"edges");
            B = graph({1,2,3,4,5},{{1,2},{4,5}});
            disjointUnion {A,B}
///

--graphComposition
doc///
    Key
        graphComposition
        (graphComposition, Graph, Graph)
    Headline
        A method for composing two graphs
    Usage
        F = graphComposition(G,H)
    Inputs
        G:Graph
        H:Graph
    Outputs
        F:Graph
            The Graph Composition of G and H
    Description
        Text
            This method will take in any two graphs and output the composition of the two graphs. The vertex set of the graph composition of G and H is the cartesian product of the vertex sets of G and H. The keys for each vertex will be output as a sequence to represent this. The edge set is formed by the rule that any two vertices (u,v) and (x,y) are adjacent the composition of G and H if and only if either u is adjacent with x in G or u = x and v is adjacent with y in H. Be careful, since this operation is not commutative, and the user needs to be mindful what order the graphs are entered into the method.
        Example
            G = graph({1,2},{{1,2}});
            H = graph({3,4,5},{{3,4},{4,5}})
            GH = graphComposition(G,H)
            HG = graphComposition(H,G)
    SeeAlso
        strongProduct
        directProduct
        cartesianProduct
///

--graphPower
doc ///
    Key
        graphPower
        (graphPower, Graph, ZZ)
    Headline
        constructs a graph raised to a power
    Usage
        G' = graphPower(G,k)
    Inputs
        G:Graph
        k:ZZ
    Outputs
        G':Graph
            graph G to the kth power
    Description
        Text
            G^k is the graph with the same vertices as G, where the vertices of G^k are adjacent if they are separated by distance less than or equal to k in graph G.  If the diameter of G is d, G^d is the complete graph with the same number of vertices as G.
        Example
            G = cycleGraph 6;
            graphPower(G,2)
    SeeAlso
        distance
        (diameter, Graph)
///

--strongProduct
doc ///
    Key
        strongProduct
        (strongProduct, Graph, Graph)
    Headline
        a method for taking the strong product of two graphs
    Usage
        F = strongProduct(G,H)
    Inputs
        G:Graph
        H:Graph
    Outputs
        F:Graph
            The Strong Product of G and H
    Description
        Text
            This method will take in any two graphs and output the strong product of the two graphs. The vertex set of 
            the strong product of G and H is the cartesian product of the vertex sets of G and H. The keys for each 
            vertex will be output as a sequence to represent this clearly. The edge set of the strong product of G and H 
            is formed by the rule any two distinct vertices (u,u') and (v,v') are adjacent in G and H if and only if u' 
            is adjacent with v' or u'=v' , and u is adjacent with v or u = v.
        Example
            G = graph({1,2},{{1,2}});
            H = graph({3,4,5},{{3,4},{4,5}});
            strongProduct(G,H)
    SeeAlso
        graphComposition
        directProduct
        cartesianProduct
///

---------------------------
--Graph Manipulations
---------------------------

--addEdges'
doc ///
    Key
        addEdge
        (addEdge, Digraph, Set)
        addEdges'
        (addEdges', Digraph, List)
    Headline
        A method for adding edges to a graph
    Usage
        H = addEdges' (G, L)
        H = addEdge (G, S)
    Inputs
        G:Graph
        L:List
            This List should be composed of other Lists, just as one would input an edge set
        S:Set
            This is used when only one edge needs to be used via addEdge
    Outputs
        H:Graph
            The Graph with the new edge
    Description
        Text
            This method will take in a Graph and a List of new edges, and output a graph with these new edges along with the previous edges.
        Example
            H = cycleGraph 4;
            G = addEdge(H, set {0,2})
            G = addEdges'(H, {{0,2},{3,1}})
    SeeAlso
        addVertices
///

--addVertices
doc ///
    Key
        addVertex
        (addVertex, Digraph, Thing)
        addVertices
        (addVertices, Digraph, List)
    Headline
        A method for adding a set of vertices to a graph
    Usage
        H = addVertex (G, v)
        H = addVertices (G, L)
    Inputs
        G:Graph
        L:List
            list of the names of the new vertices
        v:Thing
            name of the new vertex
    Outputs
        H:Graph
            This is the graph with the additional vertices specified in L
    Description
        Text
            This method will add vertices (as singletons) to any already present graph. Note that if you add a vertex that is already in the vertex set of the input graph, that vertex will be ignored.
        Example
            G = completeGraph 4
            H = addVertices(G, {3,4,5})
            --Notice that since 3 is already a vertex of G, it is ignored
    SeeAlso
        addEdges'
///

--bipartiteColoring
doc ///
    Key
        bipartiteColoring
        (bipartiteColoring, Graph)
    Headline
        Returns a coloring of a bipartite graph
    Usage
        coloring = bipartiteColoring G
    Inputs
        G:Graph
    Outputs
        coloring:List
            a coloring of bipartite graph G
    Description
        Text
            A graph is bipartite if it has a chromatic number less than or equal to 2.  This method colors the graph two colors. In other words, it partitions the graph into two sets such that there is no edge connecting any vertex within each set.
        Example
            G = graph({{0,1},{1,2},{2,4},{3,4},{4,5}},EntryMode=>"edges");
            bipartiteColoring G
    SeeAlso
        isBipartite
///

--deleteEdges
doc ///
    Key
        deleteEdges
        (deleteEdges, Graph, List)
    Headline
        Deletes a list of edges from a graph
    Usage
        G' = deleteEdges(G,E)
    Inputs
        G:Graph
        E:List
    Outputs
        G':Graph
            the graph resulting from deleting edges in list E from graph G
    Description
        Text
            This method deletes the specified edges from the graph, but preserves the original vertex set of the graph, and outputs the adjusted graph.
        Example
            G = cycleGraph 10;
            deleteEdges(G,{{1,2},{3,4},{7,8}})
    SeeAlso
        deleteVertices
        deleteVertex
///

--deleteVertex
doc ///
    Key
        deleteVertex
        (deleteVertex, Graph, Thing)
    Headline
        a method for deleting the vertex of a graph
    Usage
        H = deleteVertex(G, v)
    Inputs
        G:Graph
        v:Thing
            v should be a member of the vertex set of G
    Outputs
        H:Graph
            The graph with the vertex v and any edges touching it
    Description
        Text
            This is a method that takes in any graph and outputs this graph minus one specified vertex. This will also remove any edge that contained this vertex as one of its entries.
        Example
            G = cycleGraph 4;
            -- the graph
            G' = deleteVertex(G,1);
            -- the graph G minus vertex 1
    SeeAlso
        inducedSubgraph
        deleteVertices
        deleteEdges
///

--delete method for Graphs in package!
--deleteVertices
doc ///
    Key
        deleteVertices
        (deleteVertices, Digraph, List)
    Headline
        Deletes specified vertices from a digraph or graph
    Usage
        G' = deleteVertices(G,L)
    Inputs
        G:Digraph
        L:List
    Outputs
        G':Digraph
            Graph with vertices in list L and their incident edges deleted.
    Description
        Text
            Removes specified list of vertices and their incident edges from a graph or digraph.
        Example
            G = graph({1,2,3,4,5},{{1,3},{3,4},{4,5}});
            L = {1,2};
            deleteVertices(G,L)
    SeeAlso
        deleteVertex
        inducedSubgraph
        deleteEdges
///

--indexLabelGraph
doc ///
    Key
        indexLabelGraph
        (indexLabelGraph, Graph)
        (indexLabelGraph, Digraph)
    Headline
        Relabels the vertices of a graph or digraph according to their indices, indexed from 0.
    Usage
        G' = indexLabelGraph G
        D' = indexLabelGraph D
    Inputs
        G:Graph
        D:Digraph
    Outputs
        G':Graph
        D':Digraph
            the graph or digraph with vertices relabeled according to their indices starting from 0.
    Description
        Text
            This method relabels the vertices of a graph or digraph according to their indices.  The method indexes from 0 to the number of vertices minus one.
        Example
            G = graph({1,2,3,4,5},{{1,3},{3,4},{4,5}});
            indexLabelGraph G
            D = digraph({1,2,3,4,5},{{1,2},{2,3},{3,1},{4,5},{5,4}})
            indexLabelGraph D
    SeeAlso
        reindexBy
///

--inducedSubgraph
doc ///
    Key
        inducedSubgraph
        (inducedSubgraph, Graph, List)
        (inducedSubgraph, Digraph, List)
    Headline
        A method for finding the induced subgraph of any Graph or Digraph
    Usage
        H = inducedSubgraph(G, L)
        D' = inducedSubgraph(D, L)
    Inputs
        G:Graph
        D:Digraph
        L:List
            This list should contain vertices of G
    Outputs
        H:Graph
        D':Digraph
            The subgraph induced by removing the vertices in L
    Description
        Text
            This method takes a graph or digraph and a list as the inputs. The List should be the vertices of the subgraph the user wants to consider, and the output will contain just those vertices and any edges from G that connect them.  This method also is a way of iterating deleteVertex several times in a quick way.
        Example
            G = completeGraph 5
            S = {3,4}
            inducedSubgraph(G,S)
                --Observe that the output is a complete graph with 3 vertices, as desired
            D = digraph ({{1,2},{2,3},{3,4},{4,1},{2,4}},EntryMode=>"edges");
            D' = inducedSubgraph(D,{1,2,4})
    SeeAlso
        deleteVertex
///

--reindexBy
doc ///
    Key
        reindexBy
        (reindexBy, Graph, String)
        (reindexBy, Digraph, String)
    Headline
        reindexes the vertices according to the input ordering.
    Usage
        G' = reindexBy(G, ordering)
        D' = reindexBy(D, ordering)
    Inputs
        G:Graph
        D:Digraph
        ordering:String
    Outputs
        G':Graph
        D':Digraph
            the graph or digraph with vertices reindexed according to the String ordering
    Description
        Text
            This method reindexes the vertices of a specified graph or digraph according to the ordering method entered by the user.  The orderings available for graphs are:  "maxdegree" (orders the vertices in order of highest to lowest degree), "mindegree" (orders the vertices in order of lowest to highest degree), "random" (orders the vertices randomly), "components" (orders the vertices in the same connected components close together in indices), and "sort" (orders the vertices by sorting their names).  For digraphs, the orderings available are:  "maxdegreein" (orders the vertices in order of highest in-degree to lowest in-degree), "mindegreein" (orders the vertices in order of lowest in-degree to highest in-degree), "maxdegreeout" (orders the vertices in order of highest out-degree to lowest out-degree), "mindegreeout" (orders the vertices in order of lowest out-degree to highest out-degree), "maxdegree" (orders the vertices in order of highest to lowest degree in the underlying undirected graph), "mindegree" (orders the vertices in order of lowest to highest degree in the underlying undirected graph), "random" (orders the vertices randomly), "sort" (orders the vertices by sorting their names).
        Example
            G = graph({1,2,3,4,5},{{1,3},{3,4},{4,5}});
            reindexBy(G,"maxdegree")
            D = digraph({1,2,3,4,5},{{1,2},{2,3},{3,1},{4,5},{5,4}})
            reindexBy(D, "mindegreeout")
    SeeAlso
        reindexBy
///

--spanningForest
doc ///
    Key
        spanningForest
        (spanningForest, Graph)
    Headline
        constructs a spanning forest of a graph
    Usage
        F = spanningForest G
    Inputs
        G:Graph
    Outputs
        F:Graph
            a forest spanning G
    Description
        Text
            A graph is a forest if it is a disjoint collection of trees.  A graph is a tree if any two vertices are connected by a unique path of edges.  A spanning forest is a forest that spans all the vectors of G using edges of G.
        Example
            G = cycleGraph 5;
            spanningForest G
    SeeAlso
        isForest
        isTree
///

--vertexMultiplication
doc ///
    Key
        vertexMultiplication
        (vertexMultiplication, Graph, Thing, Thing)
    Usage
        H = vertexMultiplication (G, v, u)
    Inputs
        G:Graph
        u:Thing
            u is the new vertex to be added
        v:Thing
            v is the vertex whose neighbors become the vertices that u connects to.
    Outputs
        H:Graph
    Description
        Text
            Multiplying the vertex of a graph adds one vertex to the original graph. It also adds several edges, namely, if we are multiplying a vertex v and calling the new vertex u, {u,W} is an edge if and only if {v,w} is an edge.
        Example
            G = completeGraph 5
            H = vertexMultiplication(G, 0, 6)
///


doc /// 
    Key
        topologicalSort
        (topologicalSort, Digraph) 
	(topologicalSort, Digraph, String) 
    Headline
        outputs a list of vertices in a topologically sorted order of a DAG.
    Usage
        topologicalSort(D,S)
	topologicalSort(D)
    Inputs
        D:Digraph
	S:String
    Outputs
         :List 
    Description 
        Text
	    This function outputs a list of vertices in a topologically sorted order of a directed acyclic graph (DAG). 
	    S provides the preference given to the vertices in order to break ties and provide unique topological sorting to the DAG.
	    
	    Permissible values of S are: "random", "max", "min", "degree".
	    
	    S = "random" randomly sort the vertices of graph which have same precedence at any instance of the algorithm to break the ties.
	    
	    S = "min" sort the vertices according to their indices (from low to high) to break the ties.
	    
	    S = "max" sort the vertices according to their indices (from high to low) to break the ties.
	    
	    S = "degree" sort the vertices according to their degree (from low to high) to break the ties.
        Example
	   G = digraph{{5,2},{5,0},{4,0},{4,1},{2,3},{3,1}}
	   topologicalSort G
	   topologicalSort(G,"min")
	   topologicalSort(G,"max")
	   topologicalSort(G,"random")
	   topologicalSort(G,"degree")
	  
    SeeAlso
        topSort
   ///

--------------------------------------------
-- Documentation topSort
--------------------------------------------
doc /// 
    Key
        topSort
        (topSort, Digraph) 
	(topSort, Digraph, String)
    Headline
        outputs a hashtable containing original digraph, new digraph with vertices topologically sorted and a map from vertices of original digraph to new digraph.
    Usage
        topSort(D)
	topSort(D,S)
    Inputs
        D:Digraph
	S: String 
    Outputs
         :HashTable 
    Description 
        Text
	    This method outputs a HashTable with keys digraph, map and newDigraph, where digraph is the original digraph,
	    map is the relation between old ordering and the new ordering of vertices and newDigraph is the Digraph with 
	    topologically sorted vertices. This method needs the input to be directed acyclic graph (DAG).
	    S provides the preference given to the vertices in order to break ties and provide unique topological sorting to the DAG.
	    
	    Permissible values of S are: "random", "max", "min", "degree".
	    
	    S = "random" randomly sort the vertices of graph which have same precedence at any instance of the algorithm to break the ties.
	    
	    S = "min" sort the vertices according to their indices (from low to high) to break the ties.
	    
	    S = "max" sort the vertices according to their indices (from high to low) to break the ties.
	    
	    S = "degree" sort the vertices according to their degree (from low to high) to break the ties.

        Example
	   G = digraph{{5,2},{5,0},{4,0},{4,1},{2,3},{3,1}}
	   H = topSort G
	   H#digraph
	   H#map
	   topSort(G,"min")
	   topSort(G,"max")
	   topSort(G,"random")
	   topSort(G,"degree")
    
    SeeAlso
          topologicalSort
	  SortedDigraph
	  newDigraph
   ///


doc /// 
    Key
        SortedDigraph
    Headline
        hashtable used in topSort
    Description 
        Text
	    This is a type of hashtable.The output of @TO topSort@ has class {\tt SortedDigraph}. In the current version of 
	    Graphs (version 0.3.3) the only use of SortedDigraph is in @TO topSort@.
        Example
	   G = digraph{{5,2},{5,0},{4,0},{4,1},{2,3},{3,1}}
	   H = topSort G
	   class H
    SeeAlso
          topSort
	  newDigraph
	  topologicalSort
   ///


doc /// 
    Key
        newDigraph
    Headline
        key used in the output of topSort
    Description 
        Text
	    This is a key of the hashtable output @TO SortedDigraph@  of @TO topSort@. 
        Example
	   G = digraph{{5,2},{5,0},{4,0},{4,1},{2,3},{3,1}}
	   H = topSort G
	   keys H
    SeeAlso
          topSort
	  SortedDigraph
	  topologicalSort
   ///


doc ///
    Key
        clusteringCoefficient
        (clusteringCoefficient, Graph)  
        (clusteringCoefficient, Graph, Thing)
    Headline
        a method for computing the clustering coefficient of a Graph
    Usage
        c = clusteringCoefficient(G, v)
        g = clusteringCoefficient(G)
    Inputs
        G:Graph
        v:Thing
            v should be a member of the vertex set of G
    Outputs
        c:ZZ
            The local clustering coefficient for G relative to v.
        g:ZZ
            The global clustering coefficient for G.
    Description
        Text
            The clustering coefficient is a measure of the degree to which nodes in a graph tend to cluster together. The global clustering coefficient gives an overall
            indication of the interconnectedness of the graph. The local clustering coefficient gives an indication of how embedded a single vertex is in the graph.
        Example
            clusteringCoefficient cycleGraph 4
            clusteringCoefficient completeGraph 4
///


TEST ///
--test expansion of graphs
G=pathGraph(7);
assert(expansion(G)===1/3);
///

TEST ///
--test connectivity
G=completeGraph(5);
assert(vertexConnectivity(G)===4);
assert(edgeConnectivity(G)===4);
H=graph({{1,2},{1,3},{2,4},{3,4},{4,5},{4,6},{5,7},{6,7}});
assert(vertexConnectivity(H)===1);
assert(edgeConnectivity(H)===2);
///

TEST ///
--test cuts
G=completeGraph(4);
--complete graphs have no vertex cuts
assert(vertexCuts(G)==={});
assert(edgeCuts(G)==={{{0,1},{0,2},{0,3}},{{0,1},{1,2},
{1,3}},{{0,2},{1,2},{2,3}},{{0,3},{1,3},{2,3}}});
H=graph({{1,2},{2,3},{3,4},{4,1}});
assert(vertexCuts(H)==={{1,3},{2,4}});
assert(edgeCuts(H)==={{{1,2},{4,1}},{{1,2},{2,3}},
{{4,1},{2,3}},{{1,2},{4,3}},{{4,1},{4,3}},{{2,3},{4,3}}});
///

TEST ///
--vertices of complete graphs start at zero
assert(vertexSet(completeGraph(4))==={0,1,2,3});
--vertices of path graphs start at zero
assert(vertexSet(pathGraph(4))==={0,1,2,3});
///

TEST ///
--check diameter
assert(diameter(pathGraph(7))===6);
///

TEST ///
--check chromatic number
G=starGraph(4);
H=completeGraph(3);
assert(chromaticNumber(G)===2);
assert(chromaticNumber(H)===3);
assert(chromaticNumber(cartesianProduct(G,H))===max(2,3));
///

TEST ///
--check graphs with vertices from different classes
G=graph({{1,2},{a,b},{3,c}});
assert(numberOfComponents(G)===3);
assert(chromaticNumber(G)===2);
assert(isConnected(G)===false);
assert(neighbors(G,a)===set({b}));
assert(deleteEdges(G,{{a,b}})===graph({1,2,a,b,3,c},{{1,2},{c,3}}));

H=digraph({{1,2},{a,b},{3,c}});
assert(children(H,3)===set({c}));
assert(parents(H,c)===set({3}));
assert(degree(H,c)===1);
///

TEST ///
--check properties of empty graph
G=graph({});
assert(vertexSet(G)==={});
assert(expansion(G)===0);
assert(edgeConnectivity(G)===0);
assert(vertexConnectivity(G)===0);
assert(edgeCuts(G)==={{}});
assert(vertexCuts(G)==={});
assert(connectedComponents(G)==={});
assert(cliqueNumber(G)===0);
assert(chromaticNumber(G)===0);
assert(independenceNumber(G)===0);
assert(numberOfComponents(G)===0);
assert(isConnected(G)===true);
assert(isBipartite(G)===true);
assert(isCyclic(G)===true);
assert(isForest(G)===true);
assert(isChordal(G)===true);
assert(isSimple(G)===true);
///

TEST ///
--check rigidity
assert( isRigid ( graph({{0,1},{0,3},{0,4},{1,3},{2,3}},Singletons => {5}) ) === false )
assert( isRigid ( graph({{0,4},{0,5},{0,6},{1,4},{1,5},{1,6},{2,4},{2,5},{2,6}}) ) === true )
assert( isRigid(graph{{0,1}}) === true )
assert( isRigid(graph{{0,1},{1,2}}) === false )
///

TEST ///

   D = digraph{{2,1},{3,1}}
   assert(topologicalSort D==={2,3,1})
///


TEST ///

   D = digraph{{2,1},{3,1}}
   assert(topSort D ===  new SortedDigraph from {map => new HashTable from {1 => 3, 2 => 1, 3
  => 2}, newDigraph => digraph ({1, 2, 3}, {{1, 3}, {2, 3}}), digraph =>
  digraph ({2, 1, 3}, {{2, 1}, {3, 1}})})
///

TEST ///
assert Equation(degreeSequence pathGraph 5, {2, 2, 2, 1, 1})
///

end;

loadPackage(Graphs, Reload => true)

restart
uninstallPackage "Graphs"
restart
installPackage "Graphs"
viewHelp Graphs

check Graphs

