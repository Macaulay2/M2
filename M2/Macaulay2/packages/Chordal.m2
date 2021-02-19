
newPackage(
    "Chordal",
    Version => "0.1", 
    Date => "2 September 2017",
    Authors => {
      {Name => "Diego Cifuentes",
       Email => "diegcif@mit.edu",
       HomePage => "http://www.mit.edu/~diegcif"},
      {Name => "Pablo A. Parrilo", 
       Email => "parrilo@mit.edu",
       HomePage => "http://www.mit.edu/~parrilo/"}
    },
    Headline => "routines that exploit chordal structure",
    Keywords => {"Commutative Algebra", "Graph Theory"},
    AuxiliaryFiles => true,
    PackageImports => {"PrimaryDecomposition"},
    PackageExports => {"Graphs","TriangularSets"}
)

--  Copyright 2017 Diego Cifuentes, Pablo Parrilo.
--  You may redistribute this file under the terms of the GNU General
--  Public License as published by the Free Software Foundation,
--  either version 2 of the License, or any later version.

export { 
--Types
    "ChordalNetNode",
    "ChordalNetRank",
    "ChordalGraph",
    "ElimTree",
    "ChordalNet",
    "ChordalNetChain",
--Methods/Functions
    "chordalGraph",
    "treewidth",
    "elimTree",
    "constraintGraph",
    "suggestVariableOrder",
    "chordalNet",
    "displayNet",
    "chordalElim",
    "chordalTria",
    "codimCount",
    "rootCount",
    "nextChain",
    "nextOrderedPartition",
    "reduceNet",
    "reduceDimension",
    "adjacentMinorsIdeal",
    "chromaticIdeal",
    "subsetsProductsIdeal",
    "toLex",
    "nodes",
    "setDefaultConfiguration",
    "isTriangular",
    "structure",
    "cliques",
--Method options
    "GetTable"
}

protect NoRoot, protect counter

--##########################################################################--
-- GLOBAL VARIABLES 
--##########################################################################--

BIGPRIME = 10007;


--##########################################################################--
-- DEBUGGING METHODS
--##########################################################################--

debugPrint = s -> if debugLevel>0 then print s

-- print dynamic programming state
printState = (N,States) -> (
    for j in N.elimTree.nodes do(
        print ("Rank: " | toString j);
        print for Nj in nodes(N,j) list States#Nj;
    );
    print ("Rank: null");
    print {States#null};
)

--##########################################################################--
-- SHORTCUTS
--##########################################################################--

uniqueFast = x -> keys set x;

toLex = I -> (
    R := ring I;
    (S,phi) := changeRing(R,,Lex);
    return phi(I);
)

setDefaultConfiguration = method()
setDefaultConfiguration(Package,String,String) := (packge,key,val) -> 
    setDefaultConfiguration(toString packge,key,val)
setDefaultConfiguration(String,String,String) := (packge,key,val) -> (
    initFile := (packge) -> (
        folder := applicationDirectory();
        file := folder | "init-" | packge | ".m2";
        if not fileExists file then
            error "init file not found";
        return file;
    );
    addQuotes := s -> 
        if s#0=="\"" then s else "\""|s|"\"";
    key = addQuotes key;
    val = addQuotes val;
    file := initFile(packge);
    s := replaceKeyValue (key, val, get file);
    if s===null then 
        error "key not found";
    file << s << close;
    print "Configuration changed. Please restart or reload package."
);
replaceKeyValue = (key,val,text) -> (
    r := regex(key,text);
    if r===null then return null;
    i := plus first r;
    r = regex("\"",i,text);
    if r===null then return null;
    j := first first r;
    r = regex("\"",j+1,text);
    if r===null then return null;
    k := first first r;
    s := substring((0,j), text) | val | substring (k+1,text);
    return s;
);

--###################################
-- Type definitions
--###################################

-- type representing chordal graphs
ChordalGraph = new Type of Digraph

-- type representing the elimination tree of a chordal graph
ElimTree = new Type of HashTable

-- type representing chordal networks
ChordalNet = new Type of MutableHashTable

-- type representing a rank of a chordal network
ChordalNetRank = new Type of MutableHashTable

-- TODO: this should be immutable
-- type representing a node of a chordal network
ChordalNetNode = new Type of MutableHashTable

-- type representing a chain of a chordal network
ChordalNetChain = new Type of HashTable

--###################################
-- Chordal graphs
--###################################

-- ChordalGraph constructor
-- finds a chordal completion of a graph
chordalGraph = method()
chordalGraph (Digraph) := ChordalGraph => DG -> (
    (cG,tw) := chordalClosure(DG);
    new ChordalGraph from cG
)
-- completes the graph using a given ordering
chordalGraph (Graph,List) := ChordalGraph => (G,ordering) -> (
    if ordering!={} then G = reorderGraph(G,ordering);
    DG := directEdges(G);
    return chordalGraph(DG);
)
-- completes the graph, searching for a good ordering
chordalGraph (Graph) := ChordalGraph => G -> (
    (o,tw) := minDegreeOrder(G);
    return chordalGraph(G,o);
)
-- chordal graph given by an elimination tree
chordalGraph (ElimTree) := ChordalGraph => tree -> (
    DG:= digraph applyPairs(tree.cliques, (i,C) -> (i,delete(i,C)));
    DG = reorderGraph(DG, tree.nodes);
    return chordalGraph DG;
)

-- completes a digraph using the ordering from G.vertexSet
chordalClosure = DG -> (
    A := mutableMatrix DG.adjacencyMatrix;
    tw := 0;
    for i to numRows(A)-1 do (
        degi := fillMat(A,i,false);
        tw = max(tw,degi);
    );
    Gc := graph(DG.vertexSet, matrix A);
    return (Gc,tw);
)

fillMat = (A,i,sym) -> (
    I := neighborsAdjMat(A,i);
    for e in subsets(I,2) do (
        A_(e_0,e_1) = 1;
        if sym then A_(e_1,e_0) = 1;
    );
    return #I
)
neighborsAdjMat = (A,i) -> (
    Ai := first entries A^{i};
    I := positions (Ai, j-> j!=0);
    return I;
)

-- reorders the vertices of a graph
reorderGraph = (G,ordering) -> graph(ordering, edges G)

-- directs the edges of a graph according to some given order
directEdges = method()
directEdges(Graph,List) := (G,o) -> (
    V := G.vertexSet;
    A := G.adjacencyMatrix;
    I := toList(0..<numRows(A));
    U := matrix table(I,I, (i,j)-> if i<=j then A_(o_i,o_j) else 0);
    return digraph(V_o,U);
)
directEdges(Graph) := G -> (
    return directEdges(G,toList(0..<#G.vertexSet));
)

-- minimum degree ordering
minDegreeOrder = (G) -> (
    A := mutableMatrix G.adjacencyMatrix;
    V := G.vertexSet;
    S := 0..<numRows(A);
    tw := 0;
    ordering := while #S>0 list (
        i := minDegreeVertex(A);
        Vi := V_(S_i);
        degi:= fillMat(A,i,true);
        tw = max(tw,degi);
        J := {0..<i}|{i+1..<#S};
        A = submatrix (A,J,J);
        S = S_J;
        Vi
    );
    return (ordering,tw);
)
minDegreeVertex = (A) -> (
    mdeg := infinity;
    vert := 0;
    for i to numRows(A)-1 do (
        I := neighborsAdjMat(A,i);
        degi := #I;
        simplicial := sum(flatten entries A^I_I)==#I*(#I-1);
        if simplicial then return i;
        if degi < mdeg then (
            mdeg = degi;
            vert = i;
        );
    );
    return vert;
)

--###################################
-- Custom methods for chordal graphs
--###################################

-- TODO: independenceNumber

isChordal(ChordalGraph):= G -> true

isPerfect(ChordalGraph):= G -> true

treewidth = method()
treewidth(ChordalGraph):= G -> 
    max(for v in G.vertexSet list #children(G,v))
treewidth(ElimTree):= tree -> 
    max(for v in tree.nodes list #tree.cliques#v)-1

cliqueNumber(ChordalGraph) := G -> treewidth(G)+1

chromaticNumber(ChordalGraph):= G -> cliqueNumber G

inducedSubgraph(ChordalGraph):= G -> chordalGraph inducedSubgraph G

--###################################
-- Elimination tree
--###################################

-- ElimTree constructor
elimTree = method()
elimTree (List,HashTable,HashTable) := ElimTree => (sortednodes,parnt,clique) -> (
    roots := {};
    child := new MutableHashTable from sortednodes / (i->(i,{})); 
    for i in sortednodes do (
        p := parnt#i;
        if p===null then roots = append(roots,i)
        else child#p = append(child#p,i);
    );
    child#null = roots;
    child = new HashTable from child;
    new ElimTree from {
        symbol nodes => sortednodes,
        symbol parents => parnt, 
        symbol cliques => clique,
        symbol children => child
    }
)
elimTree (ChordalGraph) := ElimTree => G -> (
    X := G.vertexSet;
    I := toList (0..<#X);
    posit := hashTable( I / (i-> (X#i,i)) );
    smallest := L -> (
        idx := min( L / (j->posit#j) );
        if idx != infinity then X#idx );
    parnt := hashTable(
        for i in X list (i,smallest toList children(G,i)) );
    sortverts := V -> for i in X list (
        if member(i,V) then i else continue);
    cliques := hashTable(
        for i in X list (i,prepend(i,sortverts children(G,i))) );
    return elimTree(X,parnt,cliques);
)
elimTree (ChordalNet) := ElimTree => N -> N.elimTree

-- is clique Xj contained in a unique Xc?
cliqueNotContained = (j,tree) -> (
    C := tree.children#j;
    K := tree.cliques;
    if #C!=1 then return true;
    c := first C;
    contained := isSubset(K#j, K#c); 
    return not contained;
)


-- enumerate the pairs (j,p) where p is parent of j
nodePairs = method(Options => {NoRoot=>false, Reverse=>false})
nodePairs (ElimTree) := opts -> tree -> (
    X := tree.nodes;
    if opts.Reverse then X = reverse(X);
    npairs := for j in X list (
        p := tree.parents#j;
        if opts.NoRoot and p===null then continue;
        (j,p) );
    return npairs;
)

leaves(ElimTree) := tree -> select(tree.nodes, i->tree.children#i=={})

--###################################
-- Constraint graph of an ideal
--###################################

-- constructs the constraint graph of a polynomial set
constraintGraph = method()
constraintGraph (Ring,List) := Graph => (R,F) -> (
    E := set();
    for f in F do (
        Vf := support f;
        Ef := set subsets(Vf, 2);
        E = E + Ef;
    );
    return graph(gens R, toList E);
)
constraintGraph (Ideal) := Graph => I -> 
    constraintGraph(ring I, I_*);

-- suggests a good variable ordering
suggestVariableOrder = method()
suggestVariableOrder (Ideal):= I -> (
    g := constraintGraph(I);
    (X,tw) := minDegreeOrder(g);
    return X;
)
changeRing = (R,X,mOrder) -> (
    kk := coefficientRing R;
    if X===null then X = gens R;
    if mOrder===null then mOrder = Lex;
    S := kk(monoid[X,MonomialOrder=>mOrder]);
    phi := map(S,R);
    return (S,phi)
)

-- verifies if the ordering is lexicographic
hasLexOrder = method()
hasLexOrder(Ring) := R -> (
    opts := (options R).MonomialOrder;
    haslex := false;
    badkeys := (Weights,GRevLex,RevLex,GroupLex,GroupRevLex);
    for o in opts do (
        if member(o#0,badkeys) then return false;
        if o#0==Lex then haslex = true;
    );
    return haslex;
)
hasLexOrder(Ideal) := I -> hasLexOrder ring I

--###################################
-- Chordal networks
--###################################

-- ChordalNetRank constructor
chordalNetRank = method()
chordalNetRank (ZZ,Thing,VisibleList) := ChordalNetRank => (k,i,nods) -> (
    new ChordalNetRank from {
        symbol nodes => nods,
        symbol counter => k,
        symbol rank => i}
)
chordalNetRank (Thing,VisibleList) := ChordalNetRank => (i,nods) -> (
    return chordalNetRank(0,i,nods);
)
chordalNetRank (Thing) := ChordalNetRank => i -> (
    return chordalNetRank(i,{});
)



-- ChordalNetNode constructor 
chordalNetNode = method()
chordalNetNode(String,Thing,VisibleList,VisibleList) := ChordalNetNode => (nname,i,F,H) -> (
    --TODO preprocess generators (reduction)
    H = select(H, h -> not isConstant h);
    new ChordalNetNode from {
        symbol label => nname,
        symbol gens => F,
        symbol ineqs => rsort H,
        symbol rank => i,
        symbol parents => {},
        symbol children => {}}
)
chordalNetNode(ChordalNetRank,Thing,VisibleList,VisibleList) :=ChordalNetNode =>  (NRi,i,F,H) -> (
    nname := getNodeLabel(NRi);
    return chordalNetNode(nname,i,F,H);
)

getNodeLabel = NRi -> (
    k := NRi.counter;
    NRi.counter = k + 1;
    label := toString(NRi.rank) | "_" | toString(k);
    return label;
)

-- ChordalNet constructor
chordalNet = method()
chordalNet(Ideal) := ChordalNet => (I) -> (
    g := constraintGraph(I);
    gc := chordalGraph(g,{});
    tree := elimTree(gc);
    if not hasLexOrder I then error("Monomial order must be Lex");
    F := I_*;
    struct:= getStruct(F);
    N := new MutableHashTable from 
        for i in tree.nodes list 
            i => chordalNetRank(i);
    initializeNet(N,tree,F);
    CN := new ChordalNet from {
        symbol elimTree => tree,
        symbol ring => ring I,
        symbol structure => struct,
        symbol isTriangular => false,
        symbol net => N};
    return CN;
)
chordalNet(Ideal,List) := ChordalNet => (I,X) -> (
    R := ring I;
    (S,phi) := changeRing(R,X,Lex);
    return chordalNet(phi I);
)
chordalNet(Ideal,String) := ChordalNet => (I,str) -> (
    if str!="SuggestOrder" then
        error("string not recognized");
    X := suggestVariableOrder(I);
    return chordalNet(I,X);
)

initializeNet = (Nnet,tree,F) -> (
    FF := distributePolys(tree,F);
    for i in tree.nodes do
        Nnet#i.nodes = { chordalNetNode(Nnet#i,i,FF#i,{}) };
    for jp in nodePairs(tree,NoRoot=>true) do (
        (j,p):= jp;
        Nj:= Nnet#j.nodes#0;
        Np:= Nnet#p.nodes#0;
        Nj.parents = {Np};
        addChildren(Np, Nj);
    );
);

-- distribute polynomials on the tree
distributePolys = (tree,F) -> (
    cliques := applyPairs(tree.cliques ,(i,C)->(i,set C));
    placePoly := f -> (
        Xf := support f;
        queue := {mvar f};
        minranks := {};
        while #queue>0 do(
            i := first queue;
            queue = drop(queue,1);
            C := tree.children#i;
            R := for c in C list (
                if isSubset(Xf,cliques#c) then c else continue);
            queue = queue | R;
            if #R==0 then
                minranks = append(minranks,i);
        );
        return minranks;
    );
    FF := new MutableHashTable from 
        for i in tree.nodes list i => {};
    for f in F do(
        ranks := placePoly f;
        for i in ranks do
            FF#i = append(FF#i, f);
    );
    return FF;
)

getStruct = (F) -> (
    nF := max(F / (f-> #terms f));
    if nF==1 then return "Monomial"
    else if nF==2 then return "Binomial"
    else return "None";
)

--  properties of a chordal network
structure = N -> N.structure

isTriangular = method()
isTriangular(ChordalNet) := N -> N.isTriangular

--###################################
-- Access chordal network nodes/arcs
--###################################

--  properties of a node
gens (ChordalNetNode) := Ni -> Ni.gens
ineqs (ChordalNetNode) := Ni -> Ni.ineqs
rank (ChordalNetNode) := Ni -> Ni.rank
label (ChordalNetNode) := Ni -> Ni.label
parents (ChordalNetNode) := Ni -> Ni.parents
children (ChordalNetNode) := Ni -> Ni.children

-- enumerate pairs (i,Ni) where Ni is a rank i node
nodes = method(Options => {Reverse=>false})
nodes (ChordalNet,RingElement) := opts -> (N,i) -> (N.net#i).nodes
nodes (ChordalNet) := opts -> N -> (
    ranks := N.elimTree.nodes;
    if opts.Reverse then ranks = reverse(ranks);
    return flatten for i in ranks list nodes(N,i);
)


-- enumerate arcs of the network
netArcs = N -> (
    tree := N.elimTree;
    E := for ip in nodePairs(tree,NoRoot=>true) list (
        (i,p):= ip;
        for Ni in nodes(N,i) list 
            for Np in Ni.parents list (Ni,Np)
    );
    return flatten flatten E;
)

-- children of a node
nodeChildren = (rk, Ni) -> select( Ni.children, Nc->Nc.rank==rk )

--###################################
-- Representation and Visualization
--###################################

-- printing custom types
net(ChordalGraph) := Net => G -> (
    V := vertexSet G;
    A := adjacencyMatrix G;
    H := hashTable apply(#V, i -> V_i => V_(positions(first entries A^{i}, j -> j != 0)));
    P := for v in V list (v,H#v);
    horizontalJoin flatten (
        net class G,
        "{",
        stack (horizontalJoin \ apply(P, (k,v) -> (net k, " => ", net v))),
        " }"
    )
)
net(ElimTree) := Net => tree -> (
    x := tree.parents;
    horizontalJoin flatten (
        net class tree,
        "{",
        stack (horizontalJoin \ sort apply(pairs x,(k,v) -> (net k, " => ", net v))),
        "}"
    )
)
net(ChordalNetNode) := Net => Ni -> (
    F := Ni.gens;
    netgens := if #F==0 then " "
        else if #F==1 then net first F
        else net F;
    H := Ni.ineqs;
    netineqs := if #H==0 then ""
        else if #H==1 then " / " | net first H
        else " / " | net H;
    return netgens | netineqs;
)
net(ChordalNetRank) := Net => Nrk -> net Nrk.nodes
net(ChordalNet) := Net => N -> (
    P := for i in N.elimTree.nodes list (i,N.net#i);
    horizontalJoin flatten (
        net class N,
        "{ ",
        stack (horizontalJoin \ apply(P,(k,v) -> (net k, " => ", net v))),
        " }"
    )
)
net(TriaSystem) := Net => T -> (
    F := T.gens;
    netgens := if #F==0 then " "
        else if #F==1 then net first F
        else net F;
    H := T.ineqs;
    netineqs := if #H==0 then ""
        else if #H==1 then " / " | net first H
        else " / " | net H;
    return netgens | netineqs;
)

size(ChordalNet) := N -> 
    for i in N.elimTree.nodes list #nodes(N,i)

-- this function should belong to the Graphs package
writeDotFile (String, Digraph) := (filename, G) -> (
    fil := openOut filename;
    fil << "digraph G {" << endl;
    V := vertexSet G;
    scan(#V, i -> fil << "\t" | toString i | " [label=\""|toString V_i|"\"];" << endl);
    A := adjacencyMatrix G;
    E := flatten for i from 0 to #V - 1 list for j from i+1 to #V - 1 list if A_(i,j) == 1 then {i, j} else continue;
    scan(E, e -> fil << "\t" | toString e_0 | " -> " | toString e_1 | ";" << endl);
    fil << "}" << endl << close;
)

-- display an elimination tree
displayGraph (String, String, ElimTree) := (dotfilename, jpgfilename, tree) -> (
    writeDotFile(dotfilename, tree);
    displayDotFile(dotfilename, jpgfilename);
)
displayGraph (ElimTree) := (tree) -> (
    dotfilename:= temporaryFileName()|".dot";
    jpgfilename:= temporaryFileName()|".jpg";
    displayGraph(dotfilename, jpgfilename, tree);
)

writeDotFile (String, ElimTree) := (filename, tree) -> (
    fil := openOut filename;
    fil << "digraph G {" << endl;
    for i in tree.nodes do
        fil << "\t" | toString i | " ;" << endl;
    for ip in nodePairs(tree,NoRoot=>true) do(
        (i,p):= ip;
        Ci := tree.cliques#i;
        Ci = delete(p,delete(i,Ci));
        fil << "\t" | toString i | " -> " | toString p | ";" << endl;
        attr := "[dir=none,style=dotted]";
        fil << "\t" | toString i | " -> " | toString Ci | attr | ";" << endl;
    );
    fil << "}" << endl << close;
)

-- display a chordal network
displayNet = method()
displayNet (String,String,Function,ChordalNet) := (dotfilename, jpgfilename, fun, N) -> (
    writeDotFile(dotfilename, fun, N);
    displayDotFile(dotfilename, jpgfilename);
)
displayNet (Function,ChordalNet) := (fun,N) -> (
    dotfilename:= temporaryFileName()|".dot";
    jpgfilename:= temporaryFileName()|".jpg";
    displayNet(dotfilename, jpgfilename, fun, N);
)
displayNet (ChordalNet) := (N) -> 
    displayNet(net, N);

writeDotFile (String,Function,ChordalNet) := (filename, fun, N) -> (
    fil := openOut filename;
    fil << "digraph G {" << endl;
    for iNR in pairs(N.net) do(
        (i,NRi):= iNR;
        fil << "\tsubgraph cluster_" | toString i | " {" << endl;
        for Ni in NRi.nodes do
            fil << "\t\t" | toString Ni.label | " [label=\""|toString fun(Ni)|"\"];" << endl;
        fil << "\t}" << endl;
    );
    E := netArcs N;
    for e in E do(
        (Ni,Np):= e;
        fil << "\t" | toString Ni.label | " -> " | toString Np.label | ";" << endl;
    );
    fil << "}" << endl << close;
)

displayDotFile = (dotfilename,jpgfilename) -> (
    dotBinary := ((options Graphs).Configuration)#"DotBinary";
    runcmd(dotBinary  | " -Tjpg " | dotfilename | " -o " | jpgfilename);
    show URL("file://" | toAbsolutePath jpgfilename);
)
runcmd = cmd -> (
    stderr << "-- running: " << cmd << endl;
    r := run cmd;
    if r != 0 then error("-- command failed, error return code ", r);
)

--###################################
-- Digraph and Ring maps
--###################################

-- digraph associated to a chordal network
digraph(ChordalNet) := Digraph => opts -> N -> (
    tree := N.elimTree;
    nods := {};
    edges := {};
    alias := Ni -> Ni.label;
    for ip in nodePairs(tree) do(
        (i,p):= ip;
        NRi := nodes(N,i);
        nods = nods | (NRi/alias);
        if p===null then continue;
        Er := for Ni in NRi list(
            {alias(Ni),(Ni.parents)/alias} );
        edges = edges | Er;
    );
    G := digraph (nods,edges);
    return G;
)

-- construct chordal network from a digraph
chordalNet(HashTable,HashTable,ElimTree,Digraph) := ChordalNet => (eqs,ranks,tree,DG) -> (
    V := vertices DG;
    R := ring ranks#(V_0);
    node:= hashTable for v in V list(
        if not instance(v,String) then 
            error "labels must be strings";
        i := ranks#v;
        (F,H) := eqs#v;
        v => chordalNetNode(v,i,F,H)
    );
    N := new MutableHashTable from 
        for i in tree.nodes list( 
            Vi:= select(V, v -> ranks#v==i);
            NRi:= for v in Vi list node#v;
            i => chordalNetRank(i,NRi)
        );
    for v in V do(
        Nv := node#v;
        P := for p in toList children(DG,v) list node#p;
        C := for c in toList parents(DG,v) list node#c;
        addParents(Nv,P);
        addChildren(Nv,C);
    );
    CN := new ChordalNet from {
        symbol elimTree => tree,
        symbol ring => R,
        symbol structure => "None",
        symbol isTriangular => false,
        symbol net => N};
    return CN;
)

RingMap ElimTree := ElimTree => (phi,tree) -> (
    X:= phi \ tree.nodes;
    parnt:= applyPairs( tree.parents, (i,p) ->
        if p=!=null then  (phi i, phi p) else (phi i, null) );
    clique:= applyPairs( tree.cliques, (i,Xi) -> 
        (phi i, phi \ Xi) );
    return elimTree(X,parnt,clique);
)

RingMap ChordalNet := ChordalNet => (phi,N) -> (
    if source phi=!=ring N then
        error("Rings don't match");
    if not hasLexOrder target phi then
        error("Monomial order must be Lex");
    tree := elimTree N;
    nods := nodes(N);
    Phi := hashTable for Ni in nods list
        Ni => chordalNetNode(
            Ni.label, phi Ni.rank, phi\Ni.gens, phi\Ni.ineqs);
    N' := new MutableHashTable from 
        for i in tree.nodes list(
            i' := phi i;
            NRi' := for Ni in nodes(N,i) list Phi#Ni;
            i' => chordalNetRank(i', NRi')
        );
    for Ni in nods do(
        Ni' := Phi#Ni;
        P' := for Np in Ni.parents list Phi#Np;
        C' := for Nc in Ni.children list Phi#Nc;
        addParents(Ni',P');
        addChildren(Ni',C');
    );
    CN := new ChordalNet from {
        symbol elimTree => phi tree,
        symbol ring => target phi,
        symbol structure => N.structure,
        symbol isTriangular => N.isTriangular,
        symbol net => N'};
    return CN;
)

--###################################
-- Modifying chordal networks 
--###################################

-- modify parents/children of a node
addChildren = method()
addChildren(ChordalNetNode,ChordalNetNode) := ( Ni, Nc ) -> (
    Ni.children = uniqueFast append(Ni.children, Nc);
)
addChildren(ChordalNetNode,VisibleList) := ( Ni, childn ) -> (
    Ni.children = uniqueFast join(Ni.children, childn);
)
addParents = method()
addParents(ChordalNetNode,ChordalNetNode) := ( Ni, Np ) -> (
    Ni.parents = uniqueFast append(Ni.parents, Np);
)
addParents(ChordalNetNode,VisibleList) := ( Ni, parnts ) -> (
    Ni.parents = uniqueFast join(Ni.parents, parnts);
)
delChildren = method()
delChildren(ChordalNetNode,ChordalNetNode) := ( Ni, Nc ) -> (
    Ni.children = delete(Nc,Ni.children);
)
delChildren(ChordalNetNode) := Ni -> (
    Ni.children = {};
)
delParents = method()
delParents(ChordalNetNode,ChordalNetNode) := ( Ni, Np ) -> (
    Ni.parents = delete(Np,Ni.parents);
)
delParents(ChordalNetNode) := Ni -> (
    Ni.parents = {};
)

-- delete node
deleteNode = (NRj,Nj) -> (
    disconectNode(Nj);
    NRj.nodes = delete(Nj, NRj.nodes);
);
-- create/remove arcs between nodes
createArc = ( Ni, Np ) -> (
    addParents(Ni, Np);
    addChildren(Np, Ni);
)
removeArc = ( Ni, Np ) -> (
    delChildren(Np, Ni);
    delParents(Ni, Np);
)
disconectNode = ( Ni ) -> (
    for Np in Ni.parents do 
        delChildren(Np, Ni);
    for Nc in Ni.children do 
        delParents(Nc, Ni);
    delParents(Ni);
    delChildren(Ni);
)
copyAllArcs = ( Ni, Ni2 ) -> (
    copyOutArcs(Ni,Ni2);
    copyInArcs(Ni,Ni2,);
)
copyOutArcs = ( Ni, Ni2 ) -> (
    NP:= Ni.parents;
    for Np in NP do 
        addChildren(Np, Ni2);
    addParents(Ni2, copy(NP));
)
copyInArcs = ( Ni, Ni2, filter ) -> (
    NC:= Ni.children;
    if filter=!=null then NC=select(NC,filter);
    for Nc in NC do 
        addParents(Nc, Ni2);
    addChildren(Ni2, copy(NC));
)
-- copy node Ni, ignoring the arcs from rank rk
copyNodeExcept = (NRi,Ni,rk) -> (
    Ni2 := chordalNetNode(NRi,Ni.rank,{},{});
    copyOutArcs(Ni,Ni2);
    copyInArcs(Ni,Ni2, Nc -> Nc.rank=!=rk );
    return Ni2;
)

--###################################
-- Chordal elimination
--###################################

-- Grobner operations
netNodeGb = method();
netNodeGb(ChordalNetNode,OptionTable) := (Ni,opts) -> (
    I := ideal Ni.gens;
    Ni.gens = first entries gens gb(I,opts);
)

-- Elimination operations
-- TODO: merge after elimination can be done more efficiently
-- if two nodes Ni, Ni2 agree on the last elements, then it suffices to eliminate only one of them.
netNodeElim = method();
netNodeElim(Thing,ChordalNetRank,ChordalNetNode) := (p,NRp,Ni) -> (
    i := Ni.rank;
    NP := Ni.parents;
    if #NP==0 then error("node has no parent");
    hasi := f -> (x:= mvar f; x=!=null and x>p);
    F := partition(f -> hasi(f), Ni.gens, {true,false});
    H := partition(f -> hasi(f), Ni.ineqs, {true,false});
    Ni.gens = F#true;
    Ni.ineqs = H#true;
    NP2 := for Np in NP list (
        Np2 := copyNodeExcept(NRp,Np,i);
        Np2.gens = join(Np.gens, F#false);
        Np2.ineqs = join(Np.ineqs, H#false);
        createArc(Ni,Np2);
        removeArc(Ni,Np);
        if not any(Np.children, Nj->Nj.rank==i) then
            disconectNode(Np);
        Np2
    );
    return NP2;
)
netRankElim = method();
netRankElim(Thing,ElimTree,ChordalNetRank,ChordalNetRank) := (p,tree,NRp, NRi) -> (
    elim := Ni -> netNodeElim(p,NRp,Ni);
    NRp.nodes = mergeOut(NRp.rank,tree,NRi.nodes,elim);
    NRi.nodes = mergeIn(NRi.nodes,Ni->{Ni});
)

-- Chordal elimination
chordalElim = method(Options=> {Algorithm=>Inhomogeneous,DegreeLimit=>{}});
chordalElim(ChordalNet) := opts -> N -> (
    if N.isTriangular then error("input is triangularized");
    goodInitial := (i,Ni) -> (
        J := ideal for f in Ni.gens list initial(i,f);
        return J==1;
    );
    tree := N.elimTree;
    guaranteed := true;
    debugPrint("\n%%%%%%%% Chordal Elim %%%%%%%%");
    for ip in nodePairs(tree) do (
        (i,p):= ip;
        debugPrint("\nNODE: " | toString (i,p) );
        if cliqueNotContained(i,tree) then
            for Ni in nodes(N,i) do(
                if guaranteed then
                    guaranteed = goodInitial(i,Ni);
                netNodeGb(Ni,opts);
            );
        debugPrint "\nGroebner Step";
        debugPrint N;
        if p===null then continue;
        debugPrint "Elimation Step";
        debugPrint N;
        netRankElim(p,tree,N.net#p,N.net#i);
    );
    return guaranteed;
)

--###################################
-- Chordal triangularization
--###################################

-- Triangulation operations
netNodeTria = method();
netNodeTria (List,ChordalNetRank,ChordalNetNode) := (TT,NRi,Ni) -> (
    i := Ni.rank;
    TN := for Tj in TT list (
        Fj := Tj.gens;
        Hj := Tj.ineqs;
        Nj := chordalNetNode(NRi,i,Fj,Hj);
        copyAllArcs( Ni, Nj );
        Nj
    );
    disconectNode( Ni );
    return TN;
)
netNodeTria (Ring,String,ChordalNetRank,ChordalNetNode) := (R,alg,NRi,Ni) -> (
    F := Ni.gens;
    H := Ni.ineqs;
    TT := triangularize(R,F,H,TriangularDecompAlgorithm=>alg);
    return netNodeTria(TT,NRi,Ni);
)
netRankTria = method();
netRankTria (Ring,String,ElimTree,ChordalNetRank) := (R,alg,tree,NRi) -> (
    tria := Ni -> netNodeTria(R,alg,NRi,Ni);
    if alg=="Maple" or alg=="Epsilon" then(
        F := hashTable for Ni in NRi.nodes list Ni => Ni.gens;
        H := hashTable for Ni in NRi.nodes list Ni => Ni.ineqs;
        TT := triangularizeBatch(alg,R,F,H);
        tria = Ni -> netNodeTria(TT#Ni,NRi,Ni);
    );
    NRi.nodes = mergeOut(NRi.rank,tree,NRi.nodes,tria);
)

netRankDelIneqs = NRi -> for Ni in NRi.nodes do Ni.ineqs = {};

-- Merge operations
mergeOut = (i,tree,nods,lists) -> (
    Ci := tree.children#i;
    cmpChild := (Ni,Nj) -> (
        badC := 0;
        for c in Ci do 
            if set(nodeChildren(c,Ni))=!=set(nodeChildren(c,Nj)) then(
                badC = badC+1;
                if badC==2 then return false; );
        true );
    cmp := (Ni,Nj) -> if #Ci<=1 then true else cmpChild(Ni,Nj);
    key := Ni -> (Ni.gens,Ni.ineqs,set Ni.parents);
    collide := (Nj,Ncopy) -> (copyInArcs(Ncopy,Nj,); disconectNode(Ncopy););
    return toList uniqueObjects(nods, lists, key, cmp, collide);
)
mergeIn = (nods,lists) -> (
    key := Ni -> (Ni.gens,set Ni.children);
    collide := (Nj,Ncopy) -> (copyOutArcs(Ncopy,Nj); disconectNode(Ncopy););
    return toList uniqueObjects(nods, lists, key, , collide);
)

-- Looks for non equivalent objects in some lists
-- Two objects l,j are equivalent if: key(l)=key(j) and cmp(l,k)
uniqueObjects = (domain, lists, key, cmp, collide) -> (
    searchj := (L,j,seenkj) -> 
        for i in seenkj do 
            if cmp===null or cmp(L#i,j) then return L#i; 
    L := new MutableList;
    seen := new MutableHashTable;
    cnt := 0;
    for i in domain do(
        J := lists(i);
        for j in J do(
            kj := key(j);
            if seen#?kj then (
                l := searchj(L,j,seen#kj);
                if l=!=null then (
                    if collide=!=null then collide(l,j);
                    continue; );
                seen#kj = append(seen#kj,cnt); )
            else 
                seen#kj = {cnt};
            L#cnt = j;
            cnt = cnt+1;
        );
    );
    return L;
)

-- Chordal Triangularization
-- TODO: simplify network by prunning unneeded arcs
-- TODO: allow inputs with size > 1
chordalTria = method(Options=> {TriangularDecompAlgorithm=>null});
chordalTria(ChordalNet) := opts -> N -> (
    if N.isTriangular then return;
    if max size N > 1 then error("network size > 1");
    tree := N.elimTree;
    badC := not testContainedCliques N;
    R := N.ring;
    alg := selectTriaAlg(opts.TriangularDecompAlgorithm, N.structure,R);
    ranks := tree.nodes;
    debugPrint("\n%%%%%%%% Chordal Tria %%%%%%%%");
    for ip in nodePairs(tree) do (
        (i,p):= ip;
        debugPrint("\nNODE: " | toString (i,p) );
        if badC or cliqueNotContained(i,tree) then
            netRankTria(N.ring,alg,tree,N.net#i);
        debugPrint "\nTriangulation Step";
        debugPrint N;
        if p===null then(
            netRankDelIneqs(N.net#i);
            continue;
        );
        netRankElim(p,tree,N.net#p,N.net#i);
        netRankDelIneqs(N.net#i);
        debugPrint "Elimation Step";
        debugPrint N;
    );
    checkConsistency N;
    relabelNet N;
    N.isTriangular = true;
)

testContainedCliques = N -> (
    tree := N.elimTree;
    for i in tree.nodes do
        if not cliqueNotContained(i,tree) then(
            Ni:= first nodes(N,i);
            if #Ni.gens>0 then return false;
        );
    return true;
)

selectTriaAlg = (alg,struct,R) -> (
    if alg=!=null then return alg;
    if struct=!=null and struct=!="None" then return struct;
    if not checkInterface("Maple") then
        error "MapleInterface failed. Maple is needed unless ideal is binomial."; 
    if char R==0 and checkInterface("Epsilon") then return "Epsilon";
    return "Maple";
);

relabelNet = N -> (
    for iNR in pairs(N.net) do(
        (i,NRi):= iNR;
        NRi.counter = 0;
        for Ni in NRi.nodes do
            Ni.label = getNodeLabel(NRi);
    );
)

--###################################
-- Polynomial reductions
--###################################

-- simple polynomial reduction
reduceEqs = F -> (
    (M,C) := coefficients matrix {F};
    C = lift(C, coefficientRing ring C);
    C = transpose rowReduce transpose C;
    return first entries (M*C);
)
rowReduce = A -> (
    (P,L,U) := LUdecomposition A;
    rows:= for Ui in entries U list(
        j := position(Ui, e -> e!=0);
        if j===null then continue;
        p := Ui#j;
        for e in Ui list e//p
    );
    return matrix rows;
)

--###################################
-- Triangular systems
--###################################

triaSystem(ChordalNet,ChordalNetChain) := TriaSystem => (N,C) -> (
    F := flatten for Ni in values C list Ni.gens;
    F = reduceEqs F;
    H := initial F;
    return triaSystem(N.ring,F,H);
)


--###################################
-- ChordalNet Reduction
--###################################

reduceNet = method()
reduceNet(ChordalNet) := N -> (
    reduceDiamonds N;
)

-- remove disconnected nodes
checkConsistency = N -> (
    tree:= N.elimTree;
    for Nj in nodes(N,Reverse=>true) do (
        j:= Nj.rank;
        if tree.parents#j=!=null and 
            #Nj.parents==0 then deleteNode(N.net#j,Nj);
    );
)

-- reduces diamonds of the following form
--   Ni
-- Np  Np'    with Np contained in Np'
--   Nk
reduceDiamonds = N -> (
    keepMaximal := (nods,cmp,del) -> (
        P:= partition(Ni->Ni.gens, nods);
        C0:= if P#?{} then P#{} else {};
        for fC in pairs(P) do(
            (f,C):= fC;
            C1:= if f==={} then C else C|C0;
            if #C1<=1 then continue;
            maxl := maximalObjects(C1,cmp);
            badN:= select(C, Np->not maxl#Np);
            for Np in badN do del(Np);
        );
    );
    tree:= N.elimTree;
    cmp0:= (S,T) -> if S>=T then 1 else if S<=T then -1 else 0;
    for jp in nodePairs(tree,NoRoot=>true) do (
        (j,p):= jp;
        child:= Ni -> select(Ni.children, Nc -> Nc.rank=!=j );
        cmp1:= (Ni,Nk) -> cmp0( set(Ni.parents|child(Ni)), set(Nk.parents|child(Nk)) );
        for Nj in nodes(N,j) do 
            keepMaximal(Nj.parents,cmp1,Np->removeArc(Nj,Np));
    );
    cmp2:= (Ni,Nk) -> cmp0( set(Ni.children), set(Nk.children) );
    for jp in nodePairs(tree,Reverse=>true,NoRoot=>true) do (
        (j,p):= jp;
        --if #tree.children#p!=1 then continue; --seems unnecessary
        for Np in nodes(N,p) do 
            keepMaximal(nodeChildren(j,Np),cmp2,Nj->removeArc(Nj,Np));
    );
)

-- maximal elements of a partial order
-- cmp(a,b) = 1 if a>=b
-- cmp(a,b) = -1 if a<b
-- cmp(a,b) = 0 if incomparable 
maximalObjects = (Os,cmp) -> (
    cmp2 := (a,b) -> cmp(b,a);
    return minimalObjects(Os,cmp2);
)

--###################################
-- Chains of a chordal network
--###################################

chordalNetChain = method()
chordalNetChain(HashTable) := ChordalNetChain => chain -> (
    if mutable chain then 
        chain = new HashTable from chain;
    new ChordalNetChain from chain
)

codim(ChordalNetChain) := optsC -> (
    -- ignore first argument (options)
    chain:= optsC_1;
    cdim:= sum for Nj in values chain list #Nj.gens;
    return cdim;
)
dim(ChordalNetChain) := C -> #(keys C) - codim C

-- iterates over the chains of a chordal network
nextChain = method();
nextChain(ChordalNet) := ChordalNetChain => (N) -> (
    tree := N.elimTree;
    ranks := tree.nodes;
    C := new MutableHashTable from 
        for j in ranks list (j,null);
    rRanks := reverse(ranks);
    success:= fillChain(N,C,rRanks,0,);
    if not success then return null;
    return chordalNetChain(C);
)
nextChain(ChordalNetChain,ChordalNet) := ChordalNetChain => (chain,N) -> (
    tree:= N.elimTree;
    ranks := tree.nodes;
    rRanks := reverse ranks;
    C:= new MutableHashTable from chain;
    succ:= false;
    for i to #ranks-1 when not succ do (
        j := ranks_i;
        ir := #ranks-i-1;
        succ= fillChain(N,C,rRanks,ir,C#j);
    );
    if not succ then return null;
    return chordalNetChain(C); 
)

fillChain = (N,C,ranks,i,Nj0) -> (
    tree := N.elimTree;
    j:= ranks#i;
    succj := nextNj(N,C,tree,j,Nj0,);
    if not succj then return false
    else if i==#ranks-1 then return true;
    succ:= fillChain(N,C,ranks,i+1,);
    return succ;
)
nextNj = (N,C,tree,j,Nj0,filter) -> (
    p:= tree.parents#j;
    NRj:= if p===null then nodes(N,j)
        else nodeChildren(j,C#p);
    if filter=!=null then 
        NRj = select(NRj, Nj -> filter(Nj) );
    k:= if Nj0===null then 0
        else 1+position(NRj, Nj->Nj===C#j);
    if k==#NRj then return false;
    C#j = NRj_k;
    return true;
)

-- iterates over the chains of a given codimension
-- TODO: use codimension counts to make it more efficient
nextChain(ZZ,ChordalNet) := ChordalNetChain => (cdim,N) -> (
    cdimTab := getCdimTable(N);
    if not member(cdim,cdimTab#0#null) then return (null,cdimTab);
    tree := N.elimTree;
    ranks := tree.nodes;
    rRanks := reverse(ranks);
    C := new MutableHashTable from 
        for j in ranks list (j,null);
    Cdim := new MutableHashTable from C;
    roots := tree.children#null;
    succ := fillChainRoots(N,C,rRanks,cdimTab,0,Cdim,roots,cdim,);
    if not succ then return null;
    return (chordalNetChain(C),cdimTab);
)
nextChain(ChordalNetChain,Sequence,ZZ,ChordalNet) := ChordalNetChain => (chain,cdimTab,cdim,N) -> (
    if codim chain =!= cdim then error "bad codimension of initial chain";
    tree:= N.elimTree;
    ranks := tree.nodes;
    C:= new MutableHashTable from chain;
    Cdim:= new MutableHashTable from 
        for j in ranks list (j,null);
    succ:= false;
    ranks = append(ranks,null);
    rRanks:= reverse ranks;
    for i to #ranks-1 when not succ do (
        j := ranks_i;
        child:= tree.children#j;
        P:= for c in child list Cdim#c;
        cdimj:= sum P;
        if j=!=null then Cdim#j = cdimj + #(C#j.gens);
        ir := #ranks-i-1;
        if #child>0 then
            succ= fillChainRoots(N,C,rRanks,cdimTab,ir+1,Cdim,child,cdimj,P);
        if not succ and j=!=null then
            succ= fillChainI(N,C,rRanks,cdimTab,ir,Cdim,C#j);
    );
    if not succ then return null;
    return chordalNetChain(C);
)

getCdimTable = N -> (
    tree := N.elimTree;
    nodeTab := codimCount(N, GetTable=>true);
    nodeTab = applyPairs(nodeTab, (Nj,f)-> (Nj,flatten exponents f));
    rankTab := hashTable for j in tree.nodes list(
        dimsj:= for Nj in nodes(N,j) list nodeTab#Nj;
        (j,unique flatten dimsj) 
    );
    return (nodeTab,rankTab);
)

fillChainRoots = (N,C,ranks,cdimTab,i,Cdim,roots,cdim,P0) -> (
    L:= for c in roots list cdimTab#1#c;
    P:= if P0===null then nextOrderedPartition(cdim,L)
        else nextOrderedPartition(P0,cdim,L);
    while P=!=null do(
        for i to #roots-1 do Cdim#(roots_i) = P_i;
        success:= fillChainI(N,C,ranks,cdimTab,i,Cdim,);
        if success then return true;
        P = nextOrderedPartition(P,cdim,L);
    );
    return false;
)
fillChainI = (N,C,ranks,cdimTab,i,Cdim,Nj0) -> (
    tree := N.elimTree;
    j:= ranks#i;
    filter := Nj -> member(Cdim#j,cdimTab#0#Nj);
    succj := nextNj(N,C,tree,j,Nj0,filter);
    if not succj then return false
    else if i==#ranks-1 then return true;
    child := tree.children#j;
    cdim := Cdim#j - #(C#j.gens);
    succ:= if #child>0 then
            fillChainRoots(N,C,ranks,cdimTab,i+1,Cdim,child,cdim,)
        else fillChainI(N,C,ranks,cdimTab,i+1,Cdim,);
    return succ;
)

-- given lists L1..Lk of distinct nonnegative integers, 
-- iterate over all tuples (l1,..,lk) with sum equal to S
nextOrderedPartition = method()
nextOrderedPartition(ZZ,List) := (S,Lists) -> (
    k:=#Lists;
    if k==1 then return if member(S,Lists_0) then {S} else null;
    P:= new MutableList from (0..<#Lists);
    Lmin := Lists / min;
    success:= fillPartitionI(Lists,Lmin,P,0,S);
    if success then return toList P;
    return null;
)
nextOrderedPartition(List,ZZ,List) := (partn,S,Lists) -> (
    k:=#Lists;
    if k==1 then return null;
    L:= new MutableList from Lists;
    Lmin := Lists / min;
    P:= new MutableList from partn;
    for i in reverse(0..<k) do(
        Li:= select(Lists_i, l -> l<=S-sum(drop(Lmin,i+1)));
        j:= position(Li,l-> l==P#i);
        L#i = drop(Li,j+1);
        Si:= S - sum drop(partn,{i,k-1});
        success:= fillPartitionI(L,Lmin,P,i,Si);
        if success then return toList P;
        L#i = Li;
    );
    return null;
)
fillPartitionI = (L,Lmin,P,i,S) -> (
    if i==#P-1 then(
        if not member(S,L#i) then return false;
        P#i = S;
        return true;
    );
    Li:= select(L#i, l -> l<=S-sum(drop(Lmin,i+1)));
    for l in Li do(
        P#i = l;
        succ:= fillPartitionI(L,Lmin,P,i+1,S-l);
        if succ then return true;
    );
    return false;
)


--###################################
-- Methods for chordal networks
--###################################

ring(ChordalNet) := N -> N.ring

checkTriangular = N -> 
    if not N.isTriangular then error "Network must be triangular";

codim(ChordalNet) := optsN -> (
    -- ignore first argument (options)
    N := optsN_1;
    checkTriangular N;
    initState := Nj -> if Nj=!=null then #Nj.gens else 0;
    initMess := Np -> infinity;
    message := (cdimNj,messg) -> min(cdimNj,messg);
    update := (cdimNp,messg) -> cdimNp+messg;
    Cdims := messagePassing(N,initState,initMess,message,update);
    return Cdims#null;
)

dim(ChordalNet) := N -> numgens(N.ring) - codim N

codimCount = method(Options => {GetTable=>false})
codimCount(ChordalNet) := opts -> N -> (
    checkTriangular N;
    tree := N.elimTree;
    S := ZZ(monoid[getSymbol "t"]);
    t := first gens S;
    initState := Nj -> if Nj=!=null then t^(#Nj.gens) else 1_S;
    initMess := Np -> 0_S;
    message := (cdimNj,messg) -> cdimNj+messg;
    update := (cdimNp,messg) -> cdimNp*messg;
    Cdims := messagePassing(N,initState,initMess,message,update);
    return if opts.GetTable then Cdims else Cdims#null;
)

rootCount = method()
rootCount(ChordalNet) := N -> (
    checkTriangular N;
    initState := Nj -> 
        if Nj===null then 1
        else if #Nj.gens==0 then 0
        else (f:=first Nj.gens; degree(Nj.rank,f));
    initMess := Np -> 0;
    message := (rootsNj,messg) -> rootsNj+messg;
    update := (rootsNp,messg) -> rootsNp*messg;
    Nroots := messagePassing(N,initState,initMess,message,update);
    return Nroots#null;
)

-- message passing algorithm on the nodes of a chordal network
-- top-down pass over the tree
messagePassing = (N,initState,initMess,message,update) -> (
    tree := N.elimTree;
    nods := append(nodes(N), null);
    States := new MutableHashTable from
        for Nj in nods list Nj=>initState(Nj);
    for jp in nodePairs(tree) do (
        (j,p):= jp;
        nodesj := nodes(N,j);
        nodesp := if p===null then {null}
            else nodes(N,p);
        messg := new MutableHashTable from
            for Np in nodesp list Np=>initMess(Np);
        for Np in nodesp do (
            NJ:= if p=!=null then nodeChildren(j,Np) else nodesj;
            for Nj in NJ do
                messg#Np = message(States#Nj,messg#Np);
        );
        for Np in nodesp do
            States#Np = update(States#Np,messg#Np);
    );
    if debugLevel>0 then printState(N,States);
    return new HashTable from States;
)

-- radical membership
-- top-down pass over the tree
pseudoRemainder(RingElement,ChordalNet) := (f,N) -> (
    initializeTop := (nods,R,f) -> (
        x0 := mvar f;
        for Nj in nods list(
            j:= if Nj=!=null then Nj.rank else null; 
            if j===x0 then Nj=>f else Nj=>0_R )
    );
    initializeSplit := (nods,R,f) -> (
        P := partition(m -> mvar m, terms f, (gens R)|{null});
        P = applyPairs ( P , (j,Fj) -> (j, sum Fj) );
        assert( sum values P == f );
        for Nj in nods list(
            j:= if Nj=!=null then Nj.rank else null; 
            Nj => P#j )
    );
    premEval := (Dict, dict, f, Np) -> (
        if Np===null or f==0 then return f;
        x := Np.rank;
        Fp := Np.gens;
        if #Fp>0 then f = pseudoRemainder(x, f, Fp#0);
        f = dict#x(f);
        return f;
    );
    affineCoeffs := (kk,S) -> (
        r := 0_kk;
        hashTable for i to #S-1 list( 
            ri := random kk;
            if i<#S-1 then r = r + ri
            else ri = 1_kk - r;
            S#i=>ri )
    );
    checkTriangular N;
    if ring N =!= ring f then error("Rings don't match");
    R := N.ring;
    if char R == 0 then(
        Q := ZZ/BIGPRIME[gens R, MonomialOrder=>Lex];
        phi := map(Q,R);
        return pseudoRemainder(phi f, phi N);
    );
    kk := coefficientRing R;
    dict := hashTable for x in gens R list 
        (x, map(R,R,{x=>random kk}));
    Dict := product values dict;
    tree := N.elimTree;
    nods := append(nodes N, null);
    F := new MutableHashTable from 
        if hasInitials N then initializeTop(nods,R,f)
        else initializeSplit(nods,R,f);
    for jp in nodePairs(tree) do (
        (j,p):= jp;
        nodesj := nodes(N,j);
        nodesp := if p===null then {null}
            else nodes(N,p);
        for Nj in nodesj do
            F#Nj = premEval(Dict,dict,F#Nj,Nj);
        for Np in nodesp do (
            NJ:= if p=!=null then nodeChildren(j,Np) else nodesj;
            C:= affineCoeffs(kk,NJ);
            for Nj in NJ do
                F#Np = C#Nj * F#Nj + F#Np;
        );
    );
    if debugLevel>0 then printState(N,F);
    return F#null;
)

RingElement % ChordalNet := (f,N) -> pseudoRemainder(f,N)

hasInitials = N -> (
    for Nj in nodes N list (
        Fj:= Nj.gens;
        if #Fj>0 then 
            if #(indices leadTerm Fj#0) > 1 then
                return true;
    );
    return false;
);

-- simple test to check if all chains are prime
isPrimeSimple(ChordalNet) := N -> (
    (tailAny,nonTailAll) := tailNodes N;
    for Nj in tailAny do
        if not isPrime Nj.gens#0 then return false;
    for Nj in nonTailAll do
        if degree(Nj.rank,Nj.gens#0)>1 then return false;
    return true;
)

-- tailAny: nodes that are the tail of some chain
-- tailAll: nodes that are the tail of all chains
-- nonTailAll: complement of tailAll
-- bottom-up pass over the tree
tailNodes = N -> (
    isZero := Nj -> #Nj.gens==0;
    checkTriangular N;
    tree := N.elimTree;
    roots := tree.children#null;
    r := min roots;
    nods := nodes(N);
    nonzero := select(nods, Nj -> not isZero Nj);
    tailAny := nonzero;
    nonTailAll := select(nonzero, Nj -> Nj.rank=!=r);
    if #leaves(tree)>1 then -- TODO: handle more leaves
        return (tailAny,nonTailAll);
    -- any path consisting of zeros
    anyZeroPath := new MutableHashTable from
        for Nj in nods list Nj=>(Nj.rank==r);
    -- all paths consist of zeros
    allZeroPath := copy anyZeroPath;
    anyP := Np -> isZero Np and anyZeroPath#Np;
    allP := Np -> isZero Np and allZeroPath#Np;
    for jp in nodePairs(tree,Reverse=>true,NoRoot=>true) do (
        (j,p):= jp;
        nodesj := nodes(N,j);
        for Nj in nodesj do(
            NP:= Nj.parents;
            anyZeroPath#Nj = any(NP,anyP);
            allZeroPath#Nj = all(NP,allP);
        );
    );
    tailAny = select(nonzero, Nj -> anyZeroPath#Nj);
    nonTailAll = select(nonzero, Nj -> not allZeroPath#Nj);
    return (tailAny,nonTailAll);
)

-- isolates the top dimensional components of a chordal network
-- bottom-up pass over the tree
topComponents(ChordalNet) := N -> (
    tree:= N.elimTree;
    (nodeTab,rankTab) := getCdimTable(N);
    nodeTab = applyPairs(nodeTab, (Nj,cdims)->(Nj,min(cdims)));
    rankTab = applyPairs(rankTab, (j,cdims)->(j,min(cdims)));
    for Nj in nodes(N,Reverse=>true) do (
        j:= Nj.rank;
        cdimNj:= nodeTab#Nj;
        if tree.parents#j=!=null then (
            for Np in Nj.parents do(
                cdimNp:= min(nodeChildren(j,Np) / (Nk->nodeTab#Nk) );
                if cdimNp<cdimNj then removeArc(Nj,Np);
            );
            if #Nj.parents==0 then deleteNode(N.net#j,Nj);
        )else
            if rankTab#j<cdimNj then deleteNode(N.net#j,Nj);
    );
)

-- eliminates arcs/nodes absent in the top k dimensions
-- bottom-up pass over the tree
reduceDimension = (N,k) -> (
    tree:= N.elimTree;
    (nodeTab,rankTab) := getCdimTable(N);
    nodeTab = applyPairs(nodeTab, (Nj,cdims)->(Nj,min(cdims)));
    nods := append(nodes(N), null);
    regret := new MutableHashTable from for Nj in nods list 
            if Nj===null then Nj=>0 else Nj=>infinity;
    for jp in nodePairs(tree,Reverse=>true) do (
        (j,p):= jp;
        nodesj := nodes(N,j);
        nodesp := if p===null then {null}
            else nodes(N,p);
        for Np in nodesp do (
            NJ:= if p=!=null then nodeChildren(j,Np) else nodesj;
            cdim := min(NJ / (Nj->nodeTab#Nj));
            for Nj in NJ do(
                regretjp := regret#Np + (nodeTab#Nj - cdim);
                if regretjp >= k then(
                    if Np=!=null then removeArc(Nj,Np);
                    if #Nj.parents==0 then deleteNode(N.net#j,Nj);
                );
                regret#Nj = min(regretjp,regret#Nj);
            );
        );
    );
    checkConsistency N;
)

-- Components of the network (assuming they are prime)
-- brute force enumeration
components(ChordalNet,ZZ) := (N,topk) -> (
    R := N.ring;
    (Rd,phi) := changeRing(R,,GRevLex);
    psi := map(R,Rd);
    isMinimalPrem := (T,S0) -> (
        return not any(S0, I0 -> isContainedIn(I0,T));
    );
    componentsCdim := (S,cdim) -> (
        L := new MutableList;
        (C,data) := nextChain(cdim,N);
        S0 := flatten values S;
        while C=!=null do(
            T := triaSystem(N,C);
            if isMinimalPrem(T,S0) then
                L#(#L) = psi saturate phi T;
            C = nextChain(C,data,cdim,N);
        );
        S#cdim = unique toList L;
    );
    Cdims := sort flatten exponents codimCount N;
    if topk>0 then Cdims = take(Cdims,{0,topk-1});
    Q := coefficientRing R;
    S := new MutableHashTable;
    for cdim in Cdims do(
        componentsCdim(S,cdim)
    );
    return new HashTable from S;
)
components(ChordalNet) := (N) -> components(N,0)

isContainedIn = method()
-- is I contained in sat(T)?
isContainedIn(Ideal,TriaSystem) := (I,T) -> 
    all(I_*, f -> pseudoRemainder(f,T)==0)
-- is V(I) contained in Z(T)?
-- this implies that sat(T) \subset I
isContainedIn(TriaSystem,Ideal) := (T,I) -> (
    F:= T.gens;
    H:= T.ineqs;
    return all(F, f -> f%I==0) and (I+ideal H)==1;
)


--###################################
-- Examples
--###################################

-- ring with n variables
nVarsRing = (kk,n) -> (
    x := getSymbol "x";
    X := if n<=26 then vars(0..<n)
        else (x_0..x_(n-1));
    return kk(monoid[X,MonomialOrder=>Lex]);
)

-- ideal of adjacent minors of a matrix
adjacentMinorsIdeal = (kk,m,n) -> (
    if m>n then (m,n)=(n,m);
    R := nVarsRing(kk,m*n);
    M := genericMatrix(R,m,n);
    F := for i to n-m list ( 
        L := toList(i..i+m-1);
        determinant(M_L) 
    );
    return ideal F;
)

-- ideal of q-colorings
chromaticIdeal = (kk,G,q) -> (
    V := vertices G;
    n := #vertices G;
    if V!=toList(0..(n-1)) then
        error "vertex set must be 0,1,..,n-1";
    R := nVarsRing(kk,n);
    F := for E in edges G list(
        (i,j):= toSequence elements E;
        sum for d to q-1 list R_i^d*R_j^(q-1-d)
    );
    F = F | for i to n-1 list (R_i)^q-1;
    return ideal F;
)

subsetsProductsIdeal = (kk,n,k) -> (
    R:= nVarsRing(kk,n);
    return ideal (subsets(gens R, k) / (S -> product(S)));
)

--##########################################################################--
-- Documentation and Tests
--##########################################################################--

beginDocumentation()

load "./Chordal/ChordalDoc.m2";

TEST ///
load (Chordal#"source directory" | "./Chordal/test_chordal.m2")
///

