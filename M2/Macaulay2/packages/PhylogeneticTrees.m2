-- -*- utf-8 -*-
newPackage(
     "PhylogeneticTrees",
     Version => "2.0",
     Date => "November 15, 2019",
     Headline => "invariants for group-based phylogenetic models",
     --HomePage => "",
     Authors => {
	  {Name => "Hector Baños", Email => "hbanos@gatech.edu"},
	  {Name => "Nathaniel Bushek", Email => "nbushek@css.edu"},
	  {Name => "Ruth Davidson", Email => "ruth.davidson.math@gmail.com"},
	  {Name => "Elizabeth Gross", Email => "egross@hawaii.edu"},
	  {Name => "Pamela Harris", Email => "peh2@williams.edu"},
	  {Name => "Robert Krone", Email => "rckrone@gmail.com"},
	  {Name => "Colby Long", Email => "clong@wooster.edu"},
	  {Name => "AJ Stewart", Email => "stewaral@seattleu.edu"},
	  {Name => "Robert Walker", Email => "robmarsw@umich.edu"}
	  },
     Keywords => {"Applied Algebraic Geometry"},
     PackageImports => {
	  "FourTiTwo"
	  },
     PackageExports => {
	  "Graphs",
	  "Posets"
	  },
     Certification => {
	  "journal name" => "The Journal of Software for Algebra and Geometry",
	  "journal URI" => "http://j-sag.org/",
	  "article title" => "Phylogenetic trees",
	  "acceptance date" => "8 August 2020",
	  "published article URI" => "https://msp.org/jsag/2021/11-1/p01.xhtml",
          "published article DOI" => "10.2140/jsag.2021.11.1",
	  "published code URI" => "https://msp.org/jsag/2021/11-1/jsag-v11-n1-x01-PhylogeneticTrees.m2",
     	  "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/PhylogeneticTrees.m2",
	  "release at publication" => "abdf0903e7ffc31568c0cc4beb181368d943cb8d",	    -- git commit number in hex
	  "version at publication" => "2.0",
	  "volume number" => "11",
	  "volume URI" => "https://msp.org/jsag/2021/11-1/"
	  }
     )

export {
    "qRing",
    "pRing",
    "secant",
    "joinIdeal",
    "phyloToricFP",
    "phyloToric42",
    "phyloToricLinears",
    "phyloToricQuads",
    "phyloToricRandom",
    "phyloToricAMatrix",
    "toricSecantDim",
    "toricJoinDim",
    "Model",
    "CFNmodel", "JCmodel", "K2Pmodel", "K3Pmodel",
    "leafColorings",
    "model",
    "buckets",
    "group",
    "LeafTree",
    "leafTree",
    "internalEdges",
    "internalVertices",
    "edgeCut",
    "vertexCut",
    "edgeContract",
    "QRing",
    "fourierToProbability",
    "labeledTrees",
    "labeledBinaryTrees",
    "rootedTrees",
    "rootedBinaryTrees",
    "unlabeledTrees",
    "isIsomorphic"
    }
protect \ {Group, Automorphisms, AList, Buckets}
--------------------------------------------------------------------

Model = new Type of HashTable
LeafTree = new Type of List
group = method()
group(Model) := M -> M.Group
buckets = method()
buckets(Model) := M -> M.Buckets
aList = (M,g) -> M.AList#g

model = method()
model(List,List,List) := (G,buckets,auts) -> (
    modelAuts := hashTable for l in auts list (
    	l#0 => (hashTable for i to #G-1 list G#i => G#(l#1#i)));
    AL := hashTable for g in G list (
	g => apply(buckets, b->if member(g,b) then 1 else 0));
    new Model from hashTable {
	Group => G,
	Buckets => buckets,
	Automorphisms => modelAuts,
	AList => AL
	}
    )

--ZZ/2 models
F0 = 0_(ZZ/2)
F1 = 1_(ZZ/2)
ZZ2 = {F0,F1}
--CFN
CFNmodel = model(ZZ2, {{F0}, {F1}}, {})

--ZZ/2 x ZZ/2 models
F00 = {0_(ZZ/2),0_(ZZ/2)}
F01 = {0_(ZZ/2),1_(ZZ/2)}
F10 = {1_(ZZ/2),0_(ZZ/2)}
F11 = {1_(ZZ/2),1_(ZZ/2)}
ZZ2ZZ2 = {F00,F01,F10,F11}

--Jukes-Cantor
JCmodel = model(ZZ2ZZ2, {{F00}, {F01,F10,F11}}, {
	({1,2},{0,2,1,3}),
	({1,3},{0,3,2,1}),
	({2,1},{0,2,1,3}),
	({2,3},{0,1,3,2}),
	({3,1},{0,3,2,1}),
	({3,2},{0,1,3,2})})
	
--Kimura 2-parameter
K2Pmodel = model(ZZ2ZZ2, {{F00}, {F01}, {F10,F11}}, {
	({2,3},{0,1,3,2}),
	({3,2},{0,1,3,2})})
--Kimura 3-parameter
K3Pmodel = model(ZZ2ZZ2, {{F00}, {F01}, {F10}, {F11}}, {})


qRing = method(Options=>{Variable=>null})
qRing(LeafTree,Model) := opts -> (T,M) -> qRing(#(leaves T),M,opts)
qRing(ZZ,Model) := opts -> (n,M) -> (
    qList := leafColorings(n,M);
    q := opts.Variable;
    if q === null then q = getSymbol "q";
    qRingFromList(qList,M,q)
    )
qRingFromList = (qList,M,q) -> (
    G := group M;
    Ghash := hashTable apply(#G,i->(G#i=>i));
    QQ(monoid[apply(qList, qcolors -> (qindex := apply(qcolors, c->Ghash#c); q_qindex))])
    )

pRing = method(Options=>{Variable=>null})
pRing(LeafTree,Model) := opts -> (T,M) -> pRing(#(leaves T),M,opts)
pRing(ZZ,Model) := opts -> (n,M) -> (
    G := group M;
    pList := (n:0)..(n:#G-1);
    p := opts.Variable;
    if p === null then p = getSymbol "p";
    QQ(monoid[apply(pList, pindex->p_pindex)])
    )

fourierToProbability = method()
fourierToProbability(Ring,Ring,ZZ,Model) := (S,R,n,M)  -> (
    if not member(M, set{CFNmodel, JCmodel, K2Pmodel, K3Pmodel}) then
        error "model must be CFNmodel, JCmodel, K2Pmodel, or K3Pmodel";
    K := keys M;
    G := M#(K_1);
    qList := leafColorings(n,M);
    Ghash := hashTable apply(#G,i->(G#i=>i));
    varIndex := apply(qList, qcolors -> apply(qcolors, c->Ghash#c));
    L := (n:0)..(n:#G-1);
    Char := matrix{
	{1,1,1,1},
	{1,-1,1,-1},
	{1,1,-1,-1},
	{1,-1,-1,1}};
    SubVars := for vi in varIndex list (
	 sum for i to #L-1 list (
	   s := product(n, j->(Char_(vi#j, (L#i)#j)));
	   if s>0 then S_i else -S_i
	   )
	);
    map(S,R,matrix{SubVars})
    )

phyloToric42 = method(Options=>{QRing=>null})
phyloToric42(ZZ,List,Model) := opts -> (n,E,M) -> phyloToric42(leafTree(n,E),M,opts)
phyloToric42(Graph,Model) := opts -> (G,M) -> phyloToric42(leafTree G,M,opts)
phyloToric42(LeafTree,Model) := opts -> (T,M) -> (
    A := phyloToricAMatrix(T,M);
    S := if opts.QRing =!= null then opts.QRing else qRing(T,M);
    toricMarkov(A,S)
    )

phyloToricAMatrix = method()
phyloToricAMatrix(ZZ,List,Model) := (n,E,M) -> phyloToricAMatrix(leafTree(n,E),M)
phyloToricAMatrix(Graph,Model) := (G,M) -> phyloToricAMatrix(leafTree G,M)
phyloToricAMatrix(LeafTree,Model) := (T,M) -> (
    ECs := edgeColorings(T,M);
    A := for ec in ECs list flatten for g in ec list aList(M,g);
    transpose matrix A
    )

leafColorings = method()
leafColorings(LeafTree,Model) := (T,M) -> leafColorings(#(leaves T),M)
leafColorings(ZZ,Model) := (n,M) -> (
    G := group M;
    qList := toList(((n-1):0)..((n-1):(#G-1))); -- list of q variable indices
    for qindex in qList list (
	qcolors := apply(qindex,j->G#j);
	append(qcolors,sum toList qcolors)
	)
    )

--List all consistent edge colorings of a tree
edgeColorings = (T,M) -> (
    L := leavesList T;
    Lhash := hashTable apply(#L, i->L#i=>i);
    qList := leafColorings(T,M);
    for qcolors in qList list for e in edges T list (
	sum apply(toList e, l->qcolors#(Lhash#l))
	)
    )


-- Uses Local Structure of Invariants (Theorem 24 in Stumfels and Sullivant) to inductively determine the ideal of 
-- phylogenetic invariants for any k-valent tree 
-- with three types of generators: linear, quadratic and claw tree generators
phyloToricFP = method(Options=>{QRing=>null})
phyloToricFP(ZZ,List,Model) := opts -> (n,E,M) -> phyloToricFP(leafTree(n,E),M,opts)
phyloToricFP(LeafTree,Model) := opts -> (T,M) -> (
    S := if opts.QRing =!= null then opts.QRing else qRing(#(leaves T),M);
    Inv1 := phyloToricLinears(T,M,QRing=>S);
    Inv2 := phyloToricQuads(T,M,QRing=>S);
    Inv3 := phyloToricClaw(T,M,QRing=>S);
    gensList := Inv1|Inv2|Inv3;
    ideal gensList
    )


phyloToricLinears = method(Options=>{QRing=>null,Random=>false})
phyloToricLinears(ZZ,List,Model) := opts -> (n,E,M) -> phyloToricLinears(leafTree(n,E),M,opts)
phyloToricLinears(LeafTree,Model) := opts -> (T,M) -> (
    S := if opts.QRing =!= null then opts.QRing else qRing(T,M);
    ECs := edgeColorings(T,M);
    P := partition(i -> for g in ECs#i list aList(M,g), toList(0..#ECs-1));
    gensList := flatten for p in values P list (
	if #p < 2 then continue;
	for j to #p-2 list sub(S_(p#j)-S_(p#(j+1)),S)
	);
    if not opts.Random then gensList else randomElement gensList
    )


-- Produce the "edge invariants", quadratic invariants for each internal edge of T
phyloToricQuads = method(Options=>{QRing=>null,Random=>false})
phyloToricQuads(ZZ,List,Model) := opts -> (n,E,M) -> phyloToricQuads(leafTree(n,E),M,opts)
phyloToricQuads(LeafTree,Model) := opts -> (T,M) -> (
    S := if opts.QRing =!= null then opts.QRing else qRing(T,M);
    quadTemplates := apply(#(group M), g->({{g,g},{g,g}},{{g,g},{g,g}},{0,3,2,1}));
    if opts.Random then quadTemplates = randomElement quadTemplates;
    newl := symbol newl;
    intEdges := internalEdges T;
    if opts.Random then intEdges = randomElement intEdges;
    gensList := flatten for e in intEdges list (
	P := edgeCut(T,e,newl);
        fillTemplates(T,M,S,P,quadTemplates,newl,opts.Random)
	);
    select(gensList, f -> f != 0_(ring f))
    )


phyloToricClaw = method(Options=>{QRing=>null,Random=>false})
phyloToricClaw(ZZ,List,Model) := opts -> (n,E,M) -> phyloToricClaw(leafTree(n,E),M,opts)
phyloToricClaw(LeafTree,Model) := opts -> (T,M) -> (
    S := if opts.QRing =!= null then opts.QRing else qRing(T,M);
    l := first leaves T;
    newl := symbol newl;
    clawHash := new MutableHashTable; --store claw invariants to avoid recomputing
    intVerts := (internalEdges T)|{set{l}};
    if opts.Random then intVerts = randomElement intVerts;
    gensList := flatten for e in intVerts list (
	P := vertexCut(T,e,l,newl);
        if not clawHash#?(#P,M) then clawHash#(#P,M) = clawInvariants(#P,M);
        fillTemplates(T,M,S,P,clawHash#(#P,M),newl,opts.Random)
	);
    select(gensList, f -> f != 0_(ring f))
    )


phyloToricRandom = method(Options=>{QRing=>null})
phyloToricRandom(ZZ,List,Model) := opts -> (n,E,M) -> phyloToricRandom(leafTree(n,E),M,opts)
phyloToricRandom(LeafTree,Model) := opts -> (T,M) -> (
    S := if opts.QRing =!= null then opts.QRing else qRing(#(leaves T),M);
    n := random 3;
    gensList := if      n == 0 then phyloToricLinears(T,M,QRing=>S,Random=>true)
                else if n == 1 then phyloToricQuads(T,M,QRing=>S,Random=>true)
                else                phyloToricClaw(T,M,QRing=>S,Random=>true);
    if #gensList > 0 then first gensList else phyloToricRandom(T,M,opts)
    )

randomElement = L -> (
    if #L == 0 then return {};
    n := random(#L);
    {L#n}
    )

----------------------
--Auxilary functions for phyloToricFP
----------------------

--A function that takes an invariant on a small tree and extends it in all possible ways to a big tree.
--P is a graph splitting: the list of connected components after deleting the vertex or edge we are focused on.
--Temps is list of "templates" meaning invariants on small trees
fillTemplates = (T,M,S,P,temps,newl,rand) -> (
    G := group M;
    FCs := edgeColorings(T,M); --consistent colorings
    qhash := hashTable apply(#FCs, i->FCs#i => S_i); --maps from consistent edge colorings to variables in the ring
    n := #P;
    cem := compositeEdgeMap(T,P,newl);
    PFCs := apply(P, U->partitionedFCs(U,M,set{newl})); --a List of HashTables of colorings of the graph pieces
    --print PFCs;
    flatten for binom in temps list (
	fbinom0 := flatten binom#0; --flat list of color indices for first monomial of a claw tree invariant
	fbinom1 := flatten binom#1; --same for second monomial
	PFCLists := apply(#fbinom0, j->(PFCs#(j%n))#(G#(fbinom0#j))); --for each entry of fbinom0(itself a list), the list of all coloring extensions
	PFCi := (#(binom#2):0)..(toSequence apply(PFCLists,l->(#l-1))); --sequence of list indices for all combinations of coloring extensions
	if rand then PFCi = toSequence randomElement PFCi;
	newGens := for iList in PFCi list (
	    CList0 := apply(#iList, j->(PFCLists#j)#(iList#j)); --a list of color extensions for first monomial
	    CList1 := apply(binom#2, k->CList0#k); --permutations of the color extensions for second monomial
	    CList1 = apply(#fbinom1, j->(
		    c1 := fbinom1#j;
		    c0 := fbinom0#(binom#2#j);
		    if c1 != c0 then permuteColoring(CList1#j,{c1,c0},M) else CList1#j
		    ));
	    CLists := (CList0,CList1);
	    --print CLists;
	    monom := apply(CLists, CList->product for i from 0 to #(binom#0)-1 list (
		    comcol := compositeColoring(cem, take(CList,{i*n,i*n+n-1}));
		    qhash#comcol
		    ));
	    --print(monom#0,monom#1);
	    monom#0 - monom#1
	    );
	ultimate(flatten,newGens)
	)
    )


--computes the invariants for a claw tree on k leaves.
--converts each binomial to a form used by the fillTemplates function.
--the values for k==3 and the four built-in models are hard-coded
clawInvariants = (k,M) -> (
    if k == 3 and M === CFNmodel then return {};
    if k == 3 and M === JCmodel  then return {
	{{{0,1,1},{1,0,1},{1,1,0}},{{0,0,0},{1,2,3},{1,2,3}},{0,4,8,3,1,2,6,7,5}}};
    if k == 3 and M === K2Pmodel then return {
	{{{1,0,1},{2,1,3},{2,2,0}},{{1,1,0},{2,0,2},{2,3,1}},{0,4,8,3,1,5,6,7,2}},
	{{{0,1,1},{1,2,3},{2,0,2}},{{0,2,2},{1,0,1},{2,1,3}},{0,4,5,3,7,2,6,1,8}},
	{{{0,1,1},{1,2,3},{2,2,0}},{{0,2,2},{1,1,0},{2,3,1}},{0,4,5,3,1,8,6,7,2}},
	{{{0,1,1},{1,0,1},{2,2,0},{2,2,0}},{{0,0,0},{1,1,0},{2,3,1},{2,3,1}},{0,4,8,3,1,11,6,7,2,9,10,5}},
	{{{0,1,1},{2,0,2},{2,2,0}},{{0,0,0},{2,1,3},{2,3,1}},{0,4,8,3,1,5,6,7,2}},
	{{{0,1,1},{1,1,0},{2,0,2},{2,0,2}},{{0,0,0},{1,0,1},{2,1,3},{2,1,3}},{0,7,5,3,10,2,6,1,8,9,4,11}},
	{{{0,2,2},{1,0,1},{2,2,0}},{{0,0,0},{1,2,3},{2,3,1}},{0,4,8,3,1,2,6,7,5}},
	{{{0,2,2},{1,1,0},{2,0,2}},{{0,0,0},{1,2,3},{2,1,3}},{0,7,5,3,1,2,6,4,8}},
	{{{0,2,2},{0,2,2},{1,0,1},{1,1,0}},{{0,0,0},{0,1,1},{1,2,3},{1,2,3}},{0,7,11,3,10,8,6,1,2,9,4,5}}};
    if k == 3 and M === K3Pmodel then return {
	{{{2,2,0},{2,3,1},{3,0,3},{3,1,2}},{{2,0,2},{2,1,3},{3,2,1},{3,3,0}},{0,7,11,3,10,8,6,1,5,9,4,2}},
	{{{1,2,3},{2,3,1},{3,1,2}},{{1,3,2},{2,1,3},{3,2,1}},{0,4,8,3,7,2,6,1,5}},
	{{{1,3,2},{2,2,0},{3,0,3}},{{1,2,3},{2,0,2},{3,3,0}},{0,4,8,3,7,2,6,1,5}},
	{{{1,0,1},{1,3,2},{2,1,3},{2,2,0}},{{1,1,0},{1,2,3},{2,0,2},{2,3,1}},{0,7,11,3,10,8,6,1,5,9,4,2}},
	{{{1,0,1},{2,2,0},{3,1,2}},{{1,1,0},{2,0,2},{3,2,1}},{0,7,5,3,1,8,6,4,2}},
	{{{1,1,0},{2,3,1},{3,0,3}},{{1,0,1},{2,1,3},{3,3,0}},{0,7,5,3,1,8,6,4,2}},
	{{{1,1,0},{1,3,2},{3,0,3},{3,2,1}},{{1,0,1},{1,2,3},{3,1,2},{3,3,0}},{0,7,11,3,10,8,6,1,5,9,4,2}},
	{{{0,2,2},{1,3,2},{2,1,3},{3,0,3}},{{0,3,3},{1,2,3},{2,0,2},{3,1,2}},{0,4,8,3,1,11,6,10,2,9,7,5}},
	{{{0,2,2},{2,3,1},{3,0,3}},{{0,3,3},{2,0,2},{3,2,1}},{0,4,8,3,7,2,6,1,5}},
	{{{0,3,3},{2,2,0},{3,1,2}},{{0,2,2},{2,1,3},{3,3,0}},{0,4,8,3,7,2,6,1,5}},
	{{{0,3,3},{1,3,2},{2,2,0},{3,2,1}},{{0,2,2},{1,2,3},{2,3,1},{3,3,0}},{0,7,5,3,10,2,6,1,11,9,4,8}},
	{{{0,2,2},{1,0,1},{2,3,1},{3,1,2}},{{0,1,1},{1,3,2},{2,0,2},{3,2,1}},{0,10,5,3,7,2,6,4,11,9,1,8}},
	{{{0,1,1},{1,2,3},{2,0,2}},{{0,2,2},{1,0,1},{2,1,3}},{0,4,8,3,7,2,6,1,5}},
	{{{0,1,1},{1,3,2},{2,2,0}},{{0,2,2},{1,1,0},{2,3,1}},{0,7,5,3,1,8,6,4,2}},
	{{{0,1,1},{1,2,3},{2,2,0},{3,1,2}},{{0,2,2},{1,1,0},{2,1,3},{3,2,1}},{0,4,11,3,1,8,6,10,5,9,7,2}},
	{{{0,1,1},{1,3,2},{3,0,3}},{{0,3,3},{1,0,1},{3,1,2}},{0,4,8,3,7,2,6,1,5}},
	{{{0,1,1},{1,2,3},{2,3,1},{3,0,3}},{{0,3,3},{1,0,1},{2,1,3},{3,2,1}},{0,7,5,3,10,2,6,1,11,9,4,8}},
	{{{0,3,3},{1,1,0},{2,3,1},{3,1,2}},{{0,1,1},{1,3,2},{2,1,3},{3,3,0}},{0,4,8,3,1,11,6,10,2,9,7,5}},
	{{{0,3,3},{1,1,0},{3,2,1}},{{0,1,1},{1,2,3},{3,3,0}},{0,4,8,3,7,2,6,1,5}},
	{{{0,0,0},{0,3,3},{3,1,2},{3,2,1}},{{0,1,1},{0,2,2},{3,0,3},{3,3,0}},{0,7,11,3,10,8,6,1,5,9,4,2}},
	{{{0,0,0},{1,1,0},{2,3,1},{3,2,1}},{{0,1,1},{1,0,1},{2,2,0},{3,3,0}},{0,4,8,3,1,11,6,10,2,9,7,5}},
	{{{0,0,0},{2,3,1},{3,1,2}},{{0,1,1},{2,0,2},{3,3,0}},{0,7,5,3,1,8,6,4,2}},
	{{{0,1,1},{2,2,0},{3,0,3}},{{0,0,0},{2,1,3},{3,2,1}},{0,7,5,3,1,8,6,4,2}},
	{{{0,1,1},{1,1,0},{2,0,2},{3,0,3}},{{0,0,0},{1,0,1},{2,1,3},{3,1,2}},{0,7,5,3,10,2,6,1,11,9,4,8}},
	{{{0,1,1},{0,3,3},{2,0,2},{2,2,0}},{{0,0,0},{0,2,2},{2,1,3},{2,3,1}},{0,7,11,3,10,8,6,1,5,9,4,2}},
	{{{0,0,0},{1,3,2},{3,2,1}},{{0,2,2},{1,0,1},{3,3,0}},{0,7,5,3,1,8,6,4,2}},
	{{{0,2,2},{1,0,1},{2,2,0},{3,0,3}},{{0,0,0},{1,2,3},{2,0,2},{3,2,1}},{0,4,8,3,1,11,6,10,2,9,7,5}},
	{{{0,0,0},{1,3,2},{2,2,0},{3,1,2}},{{0,2,2},{1,1,0},{2,0,2},{3,3,0}},{0,7,5,3,10,2,6,1,11,9,4,8}},
	{{{0,2,2},{1,1,0},{3,0,3}},{{0,0,0},{1,2,3},{3,1,2}},{0,7,5,3,1,8,6,4,2}},
	{{{0,0,0},{1,3,2},{2,3,1},{3,0,3}},{{0,3,3},{1,0,1},{2,0,2},{3,3,0}},{0,4,11,3,1,8,6,10,5,9,7,2}},
	{{{0,3,3},{1,0,1},{2,2,0}},{{0,0,0},{1,2,3},{2,3,1}},{0,4,8,3,7,2,6,1,5}},
	{{{0,3,3},{1,1,0},{2,0,2}},{{0,0,0},{1,3,2},{2,1,3}},{0,7,5,3,1,8,6,4,2}},
	{{{0,3,3},{1,1,0},{2,2,0},{3,0,3}},{{0,0,0},{1,2,3},{2,1,3},{3,3,0}},{0,10,5,3,7,2,6,4,11,9,1,8}},
	{{{0,2,2},{0,3,3},{1,0,1},{1,1,0}},{{0,0,0},{0,1,1},{1,2,3},{1,3,2}},{0,7,11,3,10,8,6,1,5,9,4,2}}};

    qList := leafColorings(k,M);
    q := getSymbol "q";
    R := qRingFromList(qList,M,q);
    Igens := flatten entries gens phyloToric42(k,{},M,QRing=>R);
    Igens = select(Igens, f-> 1 < first degree f);
    for f in Igens list binomialTemplate(f,k,M)
    )

--converts a binomial claw tree invariant into "template" form
binomialTemplate = (f,k,M) -> (
    G := group M;
    Ghash := hashTable apply(#G,i->(G#i=>i));
    qList := leafColorings(k,M);
    termsList := for t in exponents f list join toSequence apply(#t, i->toList (t#i:toList apply(qList#i,j->Ghash#j)));
    d := #(termsList#0);
    h := new MutableList;
    for i to d*k-1 do (
	c := position((0..d-1), j->(
		n := j*k + i%k;
		(not h#?n or h#n === null) and
		aList(M,G#((termsList#1)#j#(i%k))) == aList(M,G#((termsList#0)#(i//k)#(i%k)))));
	h#(c*k + i%k) = i;
	);
    h = toList h;
    termsList|{h}
    )

--e is distinguished in T (edges outputs the list of edges)
--eindex just tells you the first time a bool is true
--"or" is necessary because each edge has two ways to be named depending on which side of the partition the edge is located
--partition breaks up list based on value of function and outputs hash table with
--key color values list of edge coloring with that color e
partitionedFCs = (T,M,e) -> (
    eindex := position(edges T, f->f==e or (leaves T)-f==e);
    FCs := edgeColorings(T,M);
    partition(fc->fc#eindex, FCs)
    )

--produces a map from the set of edges of T to the edges of the decomposition at vertex l.
--L is a list of trees in the decomposition
compositeEdgeMap = (T,L,l) -> (
    EL := apply(L, U->apply(edges U, e->orientEdge'(U,e,l)));
    for e in edges T list (
	(i,j) := (0,0);
	while i < #EL do (
	    j = position(EL#i, f -> e==f or (leaves T)-e==f);
	    if j =!= null then break else i = i+1;
	    );
	(i,j)
	)
    )

compositeColoring = (cem,L) -> apply(cem, ij->L#(ij#0)#(ij#1))

permuteColoring = (col,P,M) -> (
    G := group M;
    aut := M.Automorphisms#P;
    apply(col, c->aut#c)
    )

--------------------------
--LeafTree
--------------------------
leafTree = method()
leafTree(List,List) := (L,E) -> (
    E = select(E, e->#e > 1 and #e < #L-1);
    leafEdges := if #L > 2 then apply(L, i->{i}) else if #L == 2 then {{L#0}} else {};
    E = E|leafEdges;
    E = apply(E, e->if class e === Set then e else set e);
    new LeafTree from {L, E}
    )
leafTree(ZZ,List) := (n,E) -> leafTree(toList(0..n-1),E)
leafTree(Graph) := G -> (
    if not isTree G then error "graph must be a tree";
    L := select(vertexSet G, v->isLeaf(G,v));
    E := for e in edges G list (
	G' := deleteEdges(G,{toList e});
	side := first connectedComponents G';
	select(side, v->isLeaf(G,v))
	);
    leafTree(L,E)
    )

edges(LeafTree) := T -> T#1
internalEdges = method()
internalEdges(LeafTree) := T -> select(T#1, e->#e > 1 and #e < #(T#0)-1)
leaves(LeafTree) := T -> set T#0
leavesList = method()
leavesList(LeafTree) := T -> T#0
vertices(LeafTree) := T -> vertices graph T
internalVertices = method()
internalVertices(LeafTree) := T -> (
    select(vertices graph T, v->#v > 1)
    )

graph(LeafTree) := opts -> T -> (
    l := first elements leaves T;
    E := apply(edges T, f->orientEdge(T,f,l));
    E = E|{set{}};
    P := poset(E,isSubset);
    G := graph coveringRelations P;
    newLabels := for v in vertexSet G list (
	children := select(elements neighbors(G,v), w -> #w > #v);
	children = apply(children, w -> (leaves T) - w);
	if #v > 0 then set({v}|children) else set children
	);
    graph(newLabels, adjacencyMatrix G)
    )

digraph(LeafTree,List) := opts -> (T,L) -> digraph(T,set L)
digraph(LeafTree,Set) := opts -> (T,u) -> (
    E := apply(edges T, e->if any(elements u, f->isSubset(e,f)) then (leaves T) - e else e);
    E = E|{set{}};
    P := poset(E,isSubset);
    G := graph coveringRelations P;
    dirE := {};
    newLabels := for v in vertexSet G list (
	children := select(elements neighbors(G,v), w -> #w > #v);
	dirE = dirE|apply(children, w->{v,w});
	children = apply(children, w -> (leaves T) - w);
	if #v > 0 then set({v}|children) else set children
	);
    D := digraph(vertices G, dirE);
    digraph(newLabels, adjacencyMatrix D)
    )


Set == Set := (s,t) -> s === t

LeafTree == LeafTree := (S,T) -> (
    if leaves S != leaves T then return false;
    l := first leavesList S;
    ES := set apply(edges S, e->orientEdge(S,e,l));
    ET := set apply(edges T, e->orientEdge(T,e,l));
    ES == ET
    )

AHU := (G,v) -> (
    chil := children(G,v);
    chilAHU := flatten sort apply(elements chil, w->AHU(G,w));
    {1}|chilAHU|{0}
    )

isIsomorphicRooted = method()
isIsomorphicRooted(LeafTree,List,LeafTree,List) := (T1,v1,T2,v2) -> (
    isIsomorphicRooted(T1,set v1,T2,set v2)
    )
isIsomorphicRooted(LeafTree,Set,LeafTree,Set) := (T1,v1,T2,v2) -> (
    G1 := digraph(T1,v1);
    G2 := digraph(T2,v2);
    AHU(G1,v1) == AHU(G2,v2)
    )

isIsomorphic = method()
isIsomorphic(LeafTree,LeafTree) := (T1,T2) -> (
    if #(leaves T1) != #(leaves T2) or #(edges T1) != #(edges T2) then return false;
    C1 := center graph T1;
    C2 := center graph T2;
    if #C1 != #C2 then return false;
    for v1 in C1 do for v2 in C2 do (
	if isIsomorphicRooted(T1,v1,T2,v2) then return true;
	);
    false
    )

--splits tree T at edge e into a list of two trees
edgeCut = method()
edgeCut(LeafTree,List,Thing) := (T,e,newl) -> edgeCut(T,set e,newl)
edgeCut(LeafTree,Set,Thing) := (T,e,newl) -> (
    Lpart := {e, leaves(T) - e};
    apply(Lpart, P->leafTree((toList P)|{newl}, edgeSelect(T,P)))
    )

edgeSelect = (T,e) -> (
    for f in internalEdges T list (
	if f==e then continue
	else if isSubset(f,e) then f
	else if isSubset((leaves T) - f,e) then (leaves T) - f
	else continue
	)
    )

--gives the side of the partition representing edge e that contains leaf l
orientEdge = (T,e,l) -> if member(l,e) then e else (leaves T) - e
--gives the side of the partition representing edge e that does not contain leaf l
orientEdge' = (T,e,l) -> if member(l,e) then (leaves T) - e else e

--splits tree T at vertex v into a list of trees
--v is specified as the vertex incident to edge e away from leaf l
vertexCut = method()
vertexCut(LeafTree,List,Thing,Thing) := (T,e,l,newl) -> vertexCut(T,set e,l,newl)
vertexCut(LeafTree,Set,Thing,Thing) := (T,e,l,newl) -> (
    e = orientEdge(T,e,l);
    E := apply(edges T, f->orientEdge(T,f,l));
    E = E|{set{}};
    P := poset(E,isSubset);
    G := graph coveringRelations P;
    Lpart := select(elements neighbors(G,e), w -> #w > #e);
    Lpart = apply(Lpart, w -> (leaves T) - w)|{e};
    apply(Lpart, P->leafTree((toList P)|{newl}, edgeSelect(T,P)))
    )

edgeContract = method()
edgeContract(LeafTree,List) := (T,e) -> edgeContract(T,set e)
edgeContract(LeafTree,Set) := (T,e) -> (
    L := leaves T;
    E := select(edges T, f->(f != e and L - f != e));
    if #e == #L-1 then L = e;
    if #e == 1 then L = L - e;
    leafTree(toList L, E)
    )

labeledTrees = method()
labeledTrees(ZZ) := n -> (
    f := L -> (
        P := setPartitions L;
        select(P, p -> #p > 1)
        );
    L := toList (1..n-1);
    apply(buildBranches(L,f), T -> leafTree(n,T))
    )

labeledBinaryTrees = method()
labeledBinaryTrees(ZZ) := n -> (
    f := L -> for s in subsets drop(L,1) list (
        if #s == 0 then continue;
        {s, toList (set L - set s)}
        );
    L := toList (1..n-1);
    apply(buildBranches(L,f), T -> leafTree(n,T))
    )

rootedTrees = method()
rootedTrees(ZZ) := n -> (
    f := L -> for p in partitions(#L) list (
        if #p == 1 then continue;
        k := 0;
        for s in p list (
           k = k+s;
           take(L,{k-s,k-1})
           )
        );
    L := toList (1..n-1);
    apply(buildBranches(L,f), T -> leafTree(n,T))
    )

rootedBinaryTrees = method()
rootedBinaryTrees(ZZ) := n -> (
    f := L -> for i from 1 to (#L)//2 list {take(L,i), take(L,i-#L)};
    L := toList (1..n-1);
    apply(buildBranches(L,f), T -> leafTree(n,T))
    )

unlabeledTrees = method()
unlabeledTrees(ZZ) := n -> (
    rooted := rootedTrees n;
    trees := new MutableList;
    for T in rooted do (
	if not any(trees, S->isIsomorphic(S,T)) then trees#(#trees) = T
	);
    toList trees
    )

--lists all partitions of a set or list of distinct elements
setPartitions = method()
setPartitions(Set) := S -> setPartitions(toList S)
setPartitions(List) := L -> (
    Lhash := new HashTable from apply(#L, i->(L#i => i));
    pList := toList (#L : 0);
    sps := {{L}};
    i := #L-1;
    while i > 0 do (
        if any(i, j -> pList#j >= pList#i) then (
            pList = take(pList, i)|{pList#i + 1}|toList(#L-i-1:0);
            part := values partition(l->pList#(Lhash#l), L);
            sps = append(sps, part);
            i = #L-1;
            )
        else i = i-1;
        );
    sps
    )

--recursive function for building rooted trees.
--takes a leaf set L and function f that lists how leaves can be 
--partitioned at a node, and lists all possible edge sets.
buildBranches = (L,f) -> (
    Trees := for p in f(L) list (
        p = select(p, s -> #s > 1);
        newTrees := {p};
        for E in p do (
            branches := buildBranches(E,f);
            newTrees = flatten apply(newTrees, T->apply(branches, B->T|B));
            );
        newTrees
        );
    flatten Trees
    )

--------------------------
--Secants and Joins
--------------------------
--list the monomials in ring R corresponding to the columns of matrix A
imageMonomials = method()
imageMonomials(Ring,Matrix) := (R,A) -> (
    M := for i from 0 to (numcols A) - 1 list (
	vect := flatten entries A_i;
	product apply(#vect, j->R_j^(vect#j))
	);
    matrix {M}
    )

--computes the nth secant of ideal I using elimination
--if degree d is specified, then the generators up to degree d will be computed (this is much faster)
secant = method(Options=>{DegreeLimit => {}})
secant(Ideal,ZZ) := opts -> (I,n) -> joinIdeal(toList (n:I), opts)

joinIdeal = method(Options=>{DegreeLimit => {}})
joinIdeal(Ideal,Ideal) := opts -> (I,J) -> joinIdeal({I,J},opts)
joinIdeal(List) := opts -> L -> (
    R := ring first L;
    k := numgens R;
    n := #L;
    T := R;
    for i from 0 to n-1 do T = T**R;
    T = newRing(T, MonomialOrder=>Eliminate(n*k));
    Jlinears := apply(k,j->T_(n*k+j) - sum(n,i->T_(i*k+j)));
    Js := apply(n, i->sub(L#i,(vars T)_(toList(i*k..(i+1)*k-1))));
    J := sum(Js) + ideal Jlinears;
    d := opts.DegreeLimit;
    GB := gb(J, DegreeLimit=>join((n+1):d));
    J = selectInSubring(1,gens GB);
    ideal sub(J,matrix{toList (n*k:0_R)}|(vars R))
    )

--Randomized algorithm for affine dimension of kth secant of variety defined by matrix A
toricSecantDim = method()
toricSecantDim(Matrix,ZZ) := (A,k) -> (
    kk := ZZ/32003;
    n := numrows A;
    A = homogenizeMatrix A;
    randPoints := apply(k,i->random(kk^1,kk^(n+1)));
    x := symbol x;
    R := kk[x_0..x_n];
    J := jacobian imageMonomials(R,A);
    tSpace := matrix apply(randPoints, p->{sub(J,p)});
    rank tSpace
    )

toricJoinDim = method()
toricJoinDim(Matrix,Matrix) := (A,B) -> toricJoinDim({A,B})
toricJoinDim(List) := L -> (
    kk := ZZ/32003;
    k := #L;
    n := L/numrows;
    L = L/homogenizeMatrix;
    randPoints := apply(#L, i->random(kk^1,kk^(n#i+1)));
    x := symbol x;
    R := apply(#L, i->kk[x_0..x_(n#i)]);
    J := apply(#L, i->jacobian imageMonomials(R#i,L#i));
    tSpace := matrix apply(#L, i->{sub(J#i,randPoints#i)});
    rank tSpace
    )

homogenizeMatrix = A -> (
    n := numrows A;
    N := numcols A;
    colSums := matrix{toList(n:1)}*A;
    d := max flatten entries colSums;
    A||(matrix{toList(N:d)}-colSums)
    )


------------------------------------------
-- Documentation
------------------------------------------

beginDocumentation()

-------------------------------
-- PhylogeneticTrees
-------------------------------
doc///
    Key
	PhylogeneticTrees
    Headline
        a package to compute phylogenetic invariants associated to group-based models
    Description
        Text
	    {\em PhylogeneticTrees} is a package for phylogenetic algebraic geometry. This 
	    package calculates generating sets for phylogenetic ideals and their joins and 
	    secants.  Additionally, the package computes lower bounds for the dimensions 
	    of secants and joins of phylogenetic ideals. 
            
	    This package handles a class of commonly used tree-based Markov models called 
	    group-based models.  These models are subject to the Fourier-Hadamard coordinate 
	    transformation, which make the parametrizations monomial and the ideals toric. 
	    See the following for more details: [1] and [2].
	    	 
	    For these models, the PhylogeneticTrees package includes two methods for computing 
	    a generating set for ideals of phylogenetic invariants. The first  method calls 
	    @TO FourTiTwo@ to compute the generating set of the toric ideal. The second 
	    implements a theoretical construction for inductively determining the ideal of 
	    phylogenetic invariants for any $k$-valent tree from the $k$-leaf claw tree as 
	    described in Theorem 24 of [3].
            
	    This package also handles the joins and secants of these ideals by implementing 
	    the elimination method described in [4].
	    
	    In cases where computing a generating set for a join or secant ideal is infeasible, 
	    the package provides a probabilistic method, based on Terracini’s Lemma, to compute 
	    a lower bound on the dimension of a join or secant ideal.
	    
	    {\em References:}
	    
	    [1] S.N. Evans and T.P. Speed, it Invariants of some probability models used in phylogenetic inference, {\em Ann. Statist.} 21 (1993), no. 1, 355–377, and
	    
	    [2] L. Székely, P.L. Erdös, M.A. Steel, and D. Penny, A Fourier inversion formula for evolutionary trees, {\em Applied Mathematics Letters} 6 (1993), no. 2, 13–17.
	    
	    [3] Bernd Sturmfels and Seth Sullivant, Toric ideals of phylogenetic invariants, {\em J. Comp. Biol.} 12 (2005), no. 2, 204–228. 
	    
	    [4] Bernd Sturmfels and Seth Sullivant, Combinatorial secant varieties, {\em Quarterly Journal of Pure and Applied Mathematics} 2 (2006), 285–309. 
///
-------------------------------
-- Phylogenetic invariants
-------------------------------
--phyloToricFP
doc///
	Key
		phyloToricFP
		(phyloToricFP,ZZ,List,Model)
		(phyloToricFP,LeafTree,Model)
	Headline
		compute the invariants of a group-based phylogenetic model with toric fiber products
	Usage
		phyloToricFP(T,M)
		phyloToricFP(n,E,M)
	Inputs
	        T:LeafTree
		n:ZZ
			the number of leaves
		E:LeafTree
			the internal edges of the tree, given by one part of the bipartition on leaves
		M:Model
	Outputs
		:Ideal
	Description
	        Text
        		This function computes the invariants of a group-based phylogenetic
	        	tree model based on Theorem 24 of the paper Toric Ideals of
		        Phylogenetic Invariants by Sturmfels and Sullivant.
			
			Invariants are formed in three different ways.  The linear and
			quadratic invariants are computed as in @TO phyloToricLinears@ and 
			@TO phyloToricQuads@ respectively.  Finally higher degree invariants
			are built using a toric fiber product construction from the invariants of
			claw trees.
			
		Example
		        T = leafTree(4, {{0,1}})    
	        	phyloToricFP(T, CFNmodel)

	SeeAlso
	        phyloToric42
///
-------------------------------
--phyloToric42
doc///
	Key
		phyloToric42
		(phyloToric42,ZZ,List,Model)
		(phyloToric42,Graph,Model)
		(phyloToric42,LeafTree,Model)
	Headline
		compute the invariants of a group-based phylogenetic model with 4ti2
	Usage
		phyloToric42(n,E,M)
		phyloToric42(G,M)
		phyloToric42(T,M)
	Inputs
	        T:LeafTree
		n:ZZ
			the number of leaves
		E:LeafTree
			the internal edges of the tree, given by one part of the bipartition on leaves
		G:Graph
			a tree
		M:Model
	Outputs
		:Ideal
	Description
	        Text
		       This function computes the invariants of a group-based phylogenetic
	               tree model by computing the transpose of the matrix
	               that encodes the defining monomial map and then using the function toricMarkov of the
	               @TO FourTiTwo@ package.	
		Example
		       T = leafTree(4, {{0,1}})
	               phyloToric42(T, CFNmodel)
		        
	SeeAlso
	        phyloToricFP
///
-------------------------------
-- phyloToricLinears
doc///
    Key
        phyloToricLinears
	(phyloToricLinears,LeafTree,Model)
	(phyloToricLinears,ZZ,List,Model)
	[phyloToricLinears,Random]
    Headline
        compute the linear invariants of a group-based phylogenetic model
    Usage
	phyloToricLinears(T,M)
	phyloToricLinears(n,E,M)
    Inputs
	T:LeafTree
        n:ZZ
	    the number of leaves
	E:List
	    the internal edges of the tree, given by one part of the bipartition on leaves
	M:Model
    Outputs
	:List
	    a generating set of the linear invariants
    Description
        Text
	    For models such as Jukes-Cantor (@TO "JCmodel"@) and Kimura 2-parameter (@TO "K2Pmodel"@),
	    multiple variables in the Fourier coordinates may map to the same monomial under the
	    monomial map that defines the toric variety of the model.  These equivalencies give rise
	    to linear relations in the space of Fourier coordinates.

	    The number of linear invariants is the codimension of the smallest linear subspace in
	    which the toric variety of the model is contained.

	    The optional argument @TO QRing@ can be passed the ring of Fourier coordinates.  Otherwise
	    the function will create a new ring.
        Example
	    T = leafTree(3,{})
	    S = qRing(T, K2Pmodel)
	    phyloToricLinears(T, K2Pmodel, QRing=>S)
    SeeAlso
        phyloToricFP
	phyloToric42
	phyloToricQuads
///

-------------------------------
-- phyloToricQuads
doc///
    Key
        phyloToricQuads
	(phyloToricQuads,LeafTree,Model)
	(phyloToricQuads,ZZ,List,Model)
	[phyloToricQuads,Random]
    Headline
        compute the quadratic invariants of a group-based phylogenetic model
    Usage
	phyloToricQuads(T,M)
	phyloToricQuads(n,E,M)
    Inputs
	T:LeafTree
        n:ZZ
	    the number of leaves
	E:List
	    the internal edges of the tree, given by one part of the bipartition on leaves
	M:Model
    Outputs
	:List
	    a generating set of the quadratic invariants
    Description
        Text
	    The quadratic invariants are also referred to as the edge invariants of the model.
	    
	    Each Fourier coordinate corresponds to a consistent coloring of the edges of tree {\tt T}.
	    For any given internal edge {\tt e} of {\tt T}, the consistent colorings can be obtained by
	    coloring two smaller graphs and gluing them along {\tt e}.  This corresponds to a fiber
	    product on the corresponding toric varieties. The quadratic invariants naturally
	    arise from this process by gluing a pair of colorings of one small graph to a pair of
	    colorings of the other small graph in two different ways.

	    The optional argument @TO QRing@ can be passed the ring of Fourier coordinates.  Otherwise
	    the function will create a new ring.
        Example
	    T = leafTree(4,{{0,1}})
	    S = qRing(T, CFNmodel)
	    phyloToricQuads(T, CFNmodel, QRing=>S)
    SeeAlso
        phyloToricFP
	phyloToric42
	phyloToricLinears
///

-------------------------------
--phyloToricRandom
doc///
	Key
		phyloToricRandom
		(phyloToricRandom,ZZ,List,Model)
		(phyloToricRandom,LeafTree,Model)
	Headline
		compute a random invariant of a group-based phylogenetic model
	Usage
		phyloToricRandom(T,M)
		phyloToricRandom(n,E,M)
	Inputs
	        T:LeafTree
		n:ZZ
			the number of leaves
		E:LeafTree
			the internal edges of the tree, given by one part of the bipartition on leaves
		M:Model
	Outputs
		:RingElement
		        a randomly selected binomial invariant
	Description
	        Text
        		This function computes a random invariant of a group-based phylogenetic
	        	tree model using the toric fiber product structure.
			
			With equal probability the algorithm decides to return a linear,
			quadratic, or higher degree binomial.  It then selects one of these
			at random (but uniformity is not guaranteed).
			
			This is a much more efficient way to produce a single generator than listing all of them,
			which is useful for Monte Carlo random walk algorithms.
		Example    
	        	phyloToricRandom(4,{{0,1}},CFNmodel)
        Caveat
	        We currently do not guarantee a uniform distribution on the generators, even
		after the choice of linear, quadratic or higher degree.

	SeeAlso
	        phyloToricFP
///

-------------------------------
-- phyloToricAMatrix
doc///
	Key
		phyloToricAMatrix
		(phyloToricAMatrix,LeafTree,Model)
		(phyloToricAMatrix,Graph,Model)
		(phyloToricAMatrix,ZZ,List,Model)
	Headline
		construct the design matrix of a group-based phylogenetic model
	Usage
		phyloToricAMatrix(T,M)
		phyloToricAmatrix(G,M)
		phyloToricAMatrix(n,E,M)
	Inputs
	        T:LeafTree
		G:Graph
		        a tree
		n:ZZ
			the number of leaves
		E:List
			the internal edges of the tree, given by the half the partition on leaves
		M:Model
	Outputs
		:Matrix
		        whose columns parametrize the toric variety
	Description


		Example
		        phyloToricAMatrix(4, {{1, 2}},CFNmodel)
	SeeAlso

///
-------------------------------
-- qRing
doc///
    Key
	qRing
	(qRing,ZZ,Model)
	(qRing,LeafTree,Model)
	[qRing,Variable]
    Headline
	construct the ring of Fourier coordinates
    Usage
	qRing(T,M)
	qRing(n,M)
    Inputs
	T:LeafTree
	n:ZZ
	    the number of leaves
	M:Model
    Outputs
	:Ring
	    of Fourier coordinates
    Description
	Text
	    The Fourier coordinates for a phylogenetic tree model have one coordinate for each consistent coloring
	    of the tree {\tt T}.  A consistent coloring is an assignment of one of the group elements of the model {\tt M} to each of
	    the leaves of {\tt T} such that the sum of all the group elements assigned is $0$.

	    Each variable of the ring is indexed by a sequence representing a consistent coloring with each element of the group
	    represented by an integer between $0$ and $m-1$ where $m$ is the order of the group.
	    
	    A variable name for the ring can be passed using the optional argument {\tt Variable}.
	    Otherwise the symbol {\tt q} is used.
	Example
	    qRing(4,CFNmodel)
	    qRing(3,JCmodel)
    SeeAlso
            pRing
	    leafColorings
///
-------------------------------
-- leafColorings
doc///
    Key
	leafColorings
	(leafColorings,ZZ,Model)
	(leafColorings,LeafTree,Model)
    Headline
	list the consistent colorings of a tree
    Usage
	leafColorings(T,M)
	leafColorings(n,M)
    Inputs
	T:LeafTree
	n:ZZ
	    the number of leaves
	M:Model
    Outputs
	:List
	    the consistent colorings of the tree
    Description
	Text
	    This function outputs a list of all consistent colorings of the leaves of tree {\tt T}.
	    That is all sequences $(g_1,\ldots,g_n)$ such that $g_1+\cdots +g_n = 0$ where each $g_i$ is an
	    element of the group associated to the model {\tt M}, and {\tt n} is the number of leaves of the tree.

	    These correspond the set of subscripts of the variables in the ring output by @TO qRing@,
	    and appear in the same order.
	Example
	    leafColorings(4,CFNmodel)
	    leafColorings(3,JCmodel)
    SeeAlso
            qRing
///
-------------------------------
-- pRing
doc///
    Key
	pRing
	(pRing,ZZ,Model)
	(pRing,LeafTree,Model)
	[pRing,Variable]
    Headline
	construct the ring of probability coordinates
    Usage
	pRing(T,M)
	pRing(n,M)
    Inputs
	T:LeafTree
	n:ZZ
	    the number of leaves
	M:Model
    Outputs
	:Ring
	    of probability coordinates
    Description
	Text
	    The probability coordinates for a phylogenetic tree model have one coordinate for each possible outcome of
	    the model. A possible outcome is any labeling of the leaves of the tree by elements of the group $G$ of the
	    model. Thus the number of coordinates is $|G|^n$ where $n$ is the number of leaves.
	    
	    A variable name for the ring can be passed using the optional argument {\tt Variable}.
	    Otherwise the symbol {\tt p} is used.
	Example
	    pRing(4,CFNmodel)
	    pRing(3,JCmodel)
    SeeAlso
        qRing
///
-------------------------------
-- QRing
doc///
    Key
	QRing
        [phyloToric42,QRing]
	[phyloToricFP,QRing]
	[phyloToricLinears,QRing]
	[phyloToricQuads,QRing]
	[phyloToricRandom,QRing]
    Headline
        optional argument to specify Fourier coordinate ring
    Description
	Text
	    For any of the functions that produce phylogenetic invariants in the ring of Fourier coordinates,
	    the ring can be specified with this optional argument. If {\tt null} is passed then a new ring
	    of Fourier coordinates will be created.
	    
	    The ring passed can be any polynomial ring with sufficiently many variables. The sufficient number
	    is $k = |G|^{n-1}$ where $G$ is the group of labels used by the model, and $n$ is the number of leaves of
	    the phylogenetic tree. The ring may have more than $k$ variables, in which case only the first $k$ will be used.
	Example
	    T = leafTree(4,{{0,1}})
	    phyloToricFP(T,CFNmodel)
	    S = QQ[a..h]
	    phyloToricFP(T,CFNmodel,QRing=>S)
///
-------------------------------
-- fourierToProbability
doc///
    Key
        fourierToProbability
	(fourierToProbability,Ring,Ring,ZZ,Model)
    Headline
        map from Fourier coordinates to probability coordinates
    Usage
        fourierToProbability(S,R,n,M)
    Inputs
        S:Ring
	   of probability coordinates
	R:Ring
	   of Fourier coordinates
	n:ZZ
	   the number of leaves
	M:Model
    Outputs
        :RingMap
	    from Fourier coordinates to probability coordinates
    Description
        Text
	    This function creates a ring map from the ring of Fourier coordinates to the ring of probability coordinates,
	    for the four predefined models, @TO "CFNmodel"@, @TO "JCmodel"@, @TO "K2Pmodel"@ or @TO "K3Pmodel"@.  It will not work with
	    user-defined models.
	    The ring of probability coordinates must have at least $|G|^n$ variables where $G$ is the group
	    associated to the model. The ring of Fourier coordinates must have at least $|G|^{(n-1)}$ variables.
	    
	    
        Example
            M = CFNmodel;
	    S = pRing(3,M)
	    R = qRing(3,M)
            m = fourierToProbability(S,R,3,M)
    SeeAlso
        pRing
	qRing
///
------------------------------
--Models
------------------------------
--CFNmodel
doc///    
    Key
	"CFNmodel"
    Headline
	the model corresponding to the Cavender-Farris-Neyman model or binary Jukes Cantor
    Description
	Text
	    The Cavender-Farris-Neyman (CFN) Model is a Markov model of base substitution. It also known as the binary Jukes-Cantor model.
	    It assumes the root distribution vectors describe all bases occurring uniformly in the ancestral sequence.
	    It also assumes that the rate of all specific base changes is the same.

            The transition matrix has the form
            $$\begin{pmatrix} \alpha&\beta\\
                              \beta&\alpha \end{pmatrix}$$
    SeeAlso
        Model
	"JCmodel"
	"K2Pmodel"
	"K3Pmodel"
///
--------------------------
--JCmodel
doc///    
    Key
	"JCmodel"
    Headline
	the model corresponding to the Jukes Cantor model
    Description
	Text    
	    The Jukes-Cantor (JK) Model is a Markov model of base substitution. 
	    It assumes the root distribution vectors describe all bases occurring uniformly in the ancestral sequence. 
	    It also assumes that the rate of all specific base changes is the same.
	    Thus the rates of bases changes A-G, A-T and A-C are the same.

            The transition matrix has the form
            $$\begin{pmatrix} \alpha&\beta&\beta&\beta\\ 
                              \beta&\alpha&\beta&\beta\\
                              \beta&\beta&\alpha&\beta\\
                              \beta&\beta&\beta&\alpha \end{pmatrix}$$ 
    SeeAlso
        Model
	"CFNmodel"
	"K2Pmodel"
	"K3Pmodel"
///
---------------------------
--K2Pmodel
doc///    
    Key
	"K2Pmodel"
    Headline
	the model corresponding to the Kimura 2-parameter model
    Description
        Text
	    The Kimura 2-parameter (K2P) Model is a Markov model of base substitution. It assumes the root distribution vectors describe
	    all bases occurring uniformly in the ancestral sequence. It allows different probabilities of transitions and transversions.
	    This means that the rate of base changes A-C and A-T are the same (transversions), and the rate of
	    base change A-G can differ from the other two (transitions).

            The transition matrix has the form
            $$\begin{pmatrix} \alpha&\gamma&\beta&\beta\\ 
                              \gamma&\alpha&\beta&\beta\\
                              \beta&\beta&\alpha&\gamma\\
                              \beta&\beta&\gamma&\alpha \end{pmatrix}$$ 
    SeeAlso
        Model
	"CFNmodel"
	"JCmodel"
	"K3Pmodel"
///
-------------------------------
--K3Pmodel
doc///    
    Key
	"K3Pmodel"
    Headline
	the model corresponding to the Kimura 3-parameter model
    Description
        Text
	    The Kimura 3-parameter (K3P) Model is a Markov model of base substitution. 
	    It assumes the root distribution vectors describe all bases occurring uniformly in the ancestral sequence. 
	    It allows different probabilities of the base changes A-G, A-C and A-T.  
	    This is the most general group based model on group $(\mathbb{Z}/2\mathbb{Z})^2$.

            The transition matrix has the form
            $$\begin{pmatrix} \alpha&\gamma&\beta&\delta\\ 
                              \gamma&\alpha&\delta&\beta\\
                              \beta&\delta&\alpha&\gamma\\
                              \delta&\beta&\gamma&\alpha \end{pmatrix}$$ 
    SeeAlso
        Model
	"CFNmodel"
	"JCmodel"
	"K2Pmodel"
///
-------------------------------
-- Secants and Joins
-------------------------------
-- secant
doc///
    Key
	secant
	(secant,Ideal,ZZ)
	[secant,DegreeLimit]
    Headline
	compute the secant of an ideal
    Usage
	secant(I,n)
    Inputs
	I:Ideal
	k:ZZ
	    order of the secant
    Outputs
	:Ideal
	    the {\tt k}th secant of {\tt I}
    Description
	Text
	    This function computes the {\tt k}th secant of {\tt I} by constructing the abstract secant and then projecting with elimination.
	    
	    Here the {\tt k}th secant means the join of {\tt k} copies of {\tt I}.
	    Setting {\tt k} to 1 gives the dimension of the ideal, while 2 is the usual secant, and higher
	    values correspond to higher order secants.

	    Setting the optional argument @TO DegreeLimit@ to {\tt \{d\} } will produce only the generators
	    of the secant ideal up to degree {\tt d}.

	    This method is general and will work for arbitrary polynomial ideals, not just phylogenetic ideals.
	Example
	    R = QQ[a..d]
	    I = ideal {a^2-b,a^3-c,a^4-d}
	    secant(I,2)
    SeeAlso
        joinIdeal
///
-------------------------------
-- joinIdeal
doc///
    Key
	joinIdeal
	(joinIdeal,Ideal,Ideal)
	(joinIdeal,List)
	[joinIdeal,DegreeLimit]
    Headline
	compute the join of several ideals
    Usage
	joinIdeal(I,J)
	joinIdeal L
    Inputs
	I:Ideal
	J:Ideal
	L:List
	    of ideals in the same ring
    Outputs
	:Ideal
	    the join of the input ideals
    Description
	Text
	    This function computes the ideal of the join by constructing the abstract join and then projecting with elimination.

	    Setting the optional argument @TO DegreeLimit@ to {\tt \{d\} } will produce only the generators
	    of the join ideal up to degree {\tt d}.

	    This method is general and will work for arbitrary polynomial ideals, not just phylogenetic ideals.
	Example
	    R = QQ[a,b,c,d]
	    I = ideal {a-d,b^2-c*d}
	    J = ideal {a,b,c}
	    joinIdeal(I,J)
    SeeAlso
        secant
///
-------------------------------
-- toricSecantDim
doc///
    Key
        toricSecantDim
	(toricSecantDim,Matrix,ZZ)
    Headline
        dimension of a secant of a toric variety
    Usage
	toricSecantDim(A,k)
    Inputs
	A:Matrix
	    the A-matrix of a toric variety
	k:ZZ
	    order of the secant
    Outputs
	:ZZ
	    the dimension of the {\tt k}th secant of variety defined by matrix {\tt A}
    Description
        Text
	    A randomized algorithm for computing the affine dimension of a secant of a toric variety
	    using Terracini's Lemma.

	    Here the {\tt k}th secant means the join of {\tt k} copies of {\tt I}.
	    Setting {\tt k} to 1 gives the dimension of the ideal, while 2 is the usual secant, and higher
	    values correspond to higher order secants.

	    The matrix {\tt A} defines a parameterization of the variety.  The algorithm chooses {\tt k}
	    vectors of parameter values at random from a large finite field.  The dimension of the sum of the tangent spaces
	    at those points is computed.

	    This algorithm is much much faster than computing the secant variety.
        Example
	    A = matrix{{4,3,2,1,0},{0,1,2,3,4}}
	    toricSecantDim(A,1)
            toricSecantDim(A,2)
	    toricSecantDim(A,3)
	    toricSecantDim(A,4)
    SeeAlso
	toricJoinDim
	secant
///
-------------------------------
-- toricJoinDim
doc///
    Key
        toricJoinDim
	(toricJoinDim,Matrix,Matrix)
	(toricJoinDim,List)
    Headline
        dimension of a join of toric varieties
    Usage
	toricJoinDim(A,B)
	toricJoinDim L
    Inputs
	A:Matrix
	    the A-matrix of a toric variety
	B:Matrix
	    the A-matrix of a toric variety
	L:List
	    of A-matrices of toric varieties
    Outputs
	:ZZ
	    the dimension of the join of the toric varieties defined by the matrices
    Description
        Text
	    A randomized algorithm for computing the affine dimension of a join of toric varieties
	    using Terracini's Lemma.

	    Each input matrix defines a parameterization of the variety.  For each variety, a vector of parameter values
	    is chosen at random from a large finite field.  The dimension of the sum of the tangent spaces
	    at those points is computed.

	    This algorithm is much much faster than computing the join variety.
        Example
	    A = matrix{{4,3,2,1,0},{0,1,2,3,4}}
	    B = matrix{{1,1,1,1,1}}
	    toricJoinDim(A,B)
	    toricJoinDim(B,B)
    Caveat
	All input matrices must have the same number of columns.
    SeeAlso
	toricSecantDim
	joinIdeal
///
-------------------------------
-- Model functionality
-------------------------------
-- Model
doc///
    Key
        Model
    Headline
	a group-based model
    Description
        Text
	    A phylogenetic tree model on tree $T$ has outcomes that are described by assigning each leaf of the tree
	    any label from a particular set (typically the label set is the set of DNA bases, \{A,T,C,G\}).
	    The probability of a certain assignment of labels depends on transition probabilities between each ordered pair of labels.
	    These transition probabilities are the parameters of the model.

	    In a group based model, the label set is a group $G$ (typically $\mathbb{Z}/2$ or $(\mathbb{Z}/2)^2$), and the transition
	    probability for a pair $(g,h)$ depends only on $h-g$. This reduces the number of parameters from $|G|^2$ to $|G|$.
	    Depending on the model, further identifications of parameters are imposed.

            An object of class @TO Model@ stores the information about a group-based model required to
	    compute phylogenetic invariants.
	    This information includes the elements of the group, how those elements are partitioned, and a set of
	    automorphisms of the group that preserve the partitions.

	    There are four built-in models: Cavender-Farris-Neyman or binary model (@TO "CFNmodel"@); 
	    Jukes-Cantor model (@TO "JCmodel"@); Kimura 2-parameter model (@TO "K2Pmodel"@); 
	    and Kimura 3-parameter model (@TO "K3Pmodel"@).  Other models can be constructed with @TO model@.
        Example
	    M = CFNmodel
	    T = leafTree(3,{})
	    phyloToricAMatrix(T,M)
    SeeAlso
	model
///

-------------------------------
-- model
doc///
    Key
        model
	(model,List,List,List)
    Headline
        construct a Model
    Usage
	model(G,B,aut)
    Inputs
        G:List
	    the group elements
	B:List
	    of lists of which group elements have identified parameters
	aut:List
	    of pairs, assigning pairs of identified group elements to automorphisms of the group that switch the pair
    Outputs
        :Model
    Description
        Text
            The elements of {\tt G} must have an addition operation meaning that if two elements $g, h \in {\tt G$}, then $g+h$ must work.  The usual choices for {\tt G} are the list of elements of
	    $\mathbb{Z}/2$ or $(\mathbb{Z}/2)^2$.
	Example
	    (a,b) = (0_(ZZ/2),1_(ZZ/2))
	    G = {{a,a}, {a,b}, {b,a}, {b,b}}
	Text
	    The elements of {\tt B} are lists of the elements of {\tt G} with the same parameter value.

	    In the following example, the first two elements of {\tt G} receive distinct parameters, while the last two share a parameter.
	    This is precisely the Kimura 2-parameter model.
        Example
            B = {{G#0}, {G#1}, {G#2,G#3}}
	Text
	    Finally, for every ordered pair of group elements sharing a parameter, {\tt aut} must provide an automorphism of the group
	    that switches those two group elements.  In {\tt aut} all of the group elements are identified by their index in $G$,
	    and an automorphism is given by a list of permuted index values.

	    In our example, the pairs requiring an automorphism are {\tt \{2,3\}} and {\tt \{3,2\}}.
	Example
            aut = {({2,3}, {0,1,3,2}),
		   ({3,2}, {0,1,3,2})}
	    model(G,B,aut)
    SeeAlso
	Model
///
-------------------------------
-- group
doc///
    Key
        group
	(group,Model)
    Headline
        the group of a Model
    Usage
	group M
    Inputs
        M:Model
    Outputs
        :List
	    of group elements
    Description
        Text
            Every group-based phyogenetic model has a finite group associated to it.  This function
	    returns the group, represented as a list of elements.
	Example
	    M = K3Pmodel
	    G = group M
    SeeAlso
	Model
///
-------------------------------
-- buckets
doc///
    Key
        buckets
	(buckets,Model)
    Headline
        the equivalence classes of group elements of a Model
    Usage
	buckets M
    Inputs
        M:Model
    Outputs
        :List
	    of lists of group elements
    Description
        Text
            Every group-based phyogenetic model has a finite group {\tt G} associated to it.  Parameters
	    for the model are assigned to equivalence classes of group elements, which are orbits
	    of some subgroup of the automorphism group of {\tt G}.  This function returns the equivalence
	    classes as a list of list of group elements.
	Example
	    M = K2Pmodel
	    B = buckets M
    SeeAlso
	Model
///
-------------------------------
-- LeafTree functionality
-------------------------------
-- leafTree
doc///
    Key
        leafTree
	(leafTree,ZZ,List)
	(leafTree,List,List)
	(leafTree,Graph)
    Headline
        construct a LeafTree
    Usage
        leafTree(n,E)
	leafTree(L,E)
	leafTree(G)
    Inputs
        n:ZZ
	    the number of leaves
	L:List
	    of leaves
	E:List
	    of lists or sets specifying the internal edges
	G:Graph
	    a tree
    Outputs
        :LeafTree
    Description
        Text
            An object of class @TO LeafTree@ is specified by listing its leaves, and for each internal edge, 
	    the partition the edge induces on the set of leaves.
	    {\tt L} is the set of leaves, or if an integer {\tt n} is input then the leaves will be be named $0,\ldots,n-1$.
	    {\tt E} is a list with one entry for each internal edge.
	    Each entry is a partition specified as a list or set of the leaves in one side of the partition.
	    Thus each edge can be specified in two possible ways.

	    An object of class @TO LeafTree@ can also be constructed from a @TO Graph@ provided the graph has no cycles.

	    Here we construct the quartet tree, which is the tree with 4 leaves and one internal edge.
        Example
            T = leafTree({a,b,c,d},{{a,b}})
	    leaves T
	    edges T
	Text
	    Here is a tree with 5 leaves given as a @TO Graph@.
	Example
            G = graph{{a,b},{c,b},{b,d},{d,e},{d,f},{f,g},{f,h}}
     	    T = leafTree G
	    leaves T
	    internalEdges T
///
-------------------------------
-- LeafTree
doc///
    Key
        LeafTree
	(symbol ==,LeafTree,LeafTree)
    Headline
	a tree described in terms of its leaves
    Description
        Text
            A tree can be described in terms of its leaves by specifying a leaf set
	    and specifying the edges as partitions of the leaf set.
	    This leaf centric description is particularly useful for phylogenetic trees.
	    
	    The main constructor method is @TO leafTree@.
        Example
            T = leafTree({a,b,c,d},{{a,b}})
	    leaves T
	    edges T
            G = graph{{a,e},{b,e},{e,f},{c,f},{d,f}}
	    leafTree G
    SeeAlso
	leafTree
///
-------------------------------
-- edges
doc///
    Key
	(edges,LeafTree)
    Headline
        list the edges of a tree
    Usage
        edges T
    Inputs
        T:LeafTree
    Outputs
        :List
	    the edges of {\tt T}
    Description
        Text
	    This function lists all edges of a tree.  Each entry of the list is a @TO Set@ of the leaves on one side of the edge.
        Example
	    T = leafTree(5,{{0,1}});
	    leaves T
	    edges T
    SeeAlso
        internalEdges
///
-------------------------------
-- internalEdges
doc///
    Key
        internalEdges
	(internalEdges,LeafTree)
    Headline
        list the internal edges of a tree
    Usage
        internalEdges T
    Inputs
        T:LeafTree
    Outputs
        :List
	    the internal edges of {\tt T}
    Description
        Text
            An internal edge of a tree is an edge that is not incident to a leaf.
	    This function lists such edges. Each entry of the list is @ofClass Set@ of the leaves on one side of the edge.
        Example
            G = graph {{0,4},{1,4},{4,5},{5,2},{5,3}};
	    T = leafTree G;
	    internalEdges T
    SeeAlso
        (edges,LeafTree)
///
-------------------------------
-- vertices
doc///
    Key
	(vertices,LeafTree)
    Headline
        list the vertices of a tree
    Usage
        vertices T
    Inputs
        T:LeafTree
    Outputs
        :List
	    the vertices of {\tt T}
    Description
        Text
	    This function lists all vertices of a tree.  Each vertex is specified by the partition of the set of leaves
	    formed by removing the vertex.  Each partition is given as a list of sets.
        Example
	    T = leafTree(4,{{0,1}});
	    vertices T
	    #(vertices T)
    Caveat
        The leaves of {\tt T} in the output of {\tt vertices} have a different representation from the one in the output of @TO (leaves,LeafTree)@.
    SeeAlso
        internalVertices
	(leaves,LeafTree)
///
-------------------------------
-- internalVertices
doc///
    Key
        internalVertices
	(internalVertices,LeafTree)
    Headline
        list the internal vertices of a tree
    Usage
        internalVertices T
    Inputs
        T:LeafTree
    Outputs
        :List
	    the internal vertices of {\tt T}
    Description
        Text
            An internal vertex of a tree is a vertex that is not a leaf, meaning it has degree at least 2.
	    This function lists such vertices. Each vertex is specified by the partition of the set of leaves
	    formed by removing the vertex.  Each partition is given as a list of sets.
        Example
	    T = leafTree(4,{{0,1}});
	    internalVertices T
	    #(internalVertices T)
    SeeAlso
        (vertices,LeafTree)
///
-------------------------------
-- leaves
doc///
    Key
	(leaves,LeafTree)
    Headline
        list the leaves of a tree
    Usage
        leaves T
    Inputs
        T:LeafTree
    Outputs
        :Set
	    the leaves of {\tt T}
    Description
        Text
	    This function outputs the leaves of the tree as an object of class @TO Set@.
        Example
	    T = leafTree(4,{{0,1}});
	    vertices T
	    #(vertices T)
    Caveat
        The leaves have a different representation from the one in the output of @TO (vertices,LeafTree)@.
    SeeAlso
        (vertices,LeafTree)
///
-------------------------------
-- isIsomorphic
doc///
    Key
        isIsomorphic
	(isIsomorphic,LeafTree,LeafTree)
    Headline
        check isomorphism of two tree
    Usage
        isIsomorphic(T,U)
    Inputs
        T:LeafTree
	U:LeafTree
    Outputs
        :Boolean
	    if U and T are isomorphic
    Description
        Text
	    This function checks if two objects of class @TO LeafTree@ are isomorphic to each other as unlabeled graphs.
	    This is in contrast to equality of two objects of class @TO LeafTree@, which also checks whether they have the same leaf labeling.
        Example
	    T = leafTree(4,{{0,1}});
	    U = leafTree(4,{{1,2}});
	    isIsomorphic(T,U)
///
-------------------------------
-- edgeCut
doc///
    Key
        edgeCut
	(edgeCut,LeafTree,List,Thing)
	(edgeCut,LeafTree,Set,Thing)
    Headline
        break up a tree at an edge
    Usage
        edgeCut(T,e,newl)
	edgeCut(T,E,newl)
    Inputs
        T:LeafTree
	e:Set
	    an edge specified by the set of leaves on one side of it
	E:List
	    an edge specified by a list of the leaves on one side of it
	newl:Thing
	    the label for a new leaf
    Outputs
        :List
	    of two @TO LeafTree@s that are subtrees of {\tt T}
    Description
        Text
	    This function outputs the two subtrees of {\tt T} obtained by deleting edge {\tt e} from {\tt T} and then re-adding the edge
	    to each of the two resulting subtrees. Both subtrees share a copy of the edge {\tt e}
	    and the newly labeled leaf adjacent to {\tt e}. Other than this overlap, they are disjoint.

	    Each subtree in {\tt P} may have at most one leaf that was not a leaf of {\tt T}, and therefore previously unlabeled.
	    The label for this new leaf is input as {\tt newl}.
        Example
	    T = leafTree(4,{{0,1}})
	    P = edgeCut(T, set {0,1}, 4);
	    P#0
	    P#1
    SeeAlso
        vertexCut
///
-------------------------------
-- vertexCut
doc///
    Key
        vertexCut
	(vertexCut,LeafTree,List,Thing,Thing)
	(vertexCut,LeafTree,Set,Thing,Thing)
    Headline
        break up a tree at a vertex
    Usage
        vertexCut(T,e,l,newl)
	vertexCut(T,E,l,newl)
    Inputs
        T:LeafTree
	e:Set
	    an edge specified by the set of leaves on one side of it
	E:List
	    an edge specified by a list of the leaves on one side of it
	l:Thing
	    a leaf of the tree
	newl:Thing
	    the label for a new leaf
    Outputs
        :List
	    of @TO LeafTree@s that are subtrees of {\tt T}
    Description
        Text
	    Vertices of a tree of class @TO LeafTree@ do not have explicit names.  Therefore a vertex {\tt v} is specified by naming an edge {\tt e}
	    incident to {\tt v}, and leaf {\tt l} on the opposite side of the edge as {\tt v}.

	    The function outputs the subtrees of {\tt T} obtained by deleting the vertex {\tt v} from {\tt T}
	    and then re-adding {\tt v} to each of the resulting subtrees as a new leaf.
	    The new leaf on each subtree is adjacent to the edge previously adjacent 
	    to {\tt v} on {\tt T}. Each subtree has a copy of the vertex labeled {\tt newl}, but their edge sets form a partition
	    of the edge set of {\tt T}.

	    Each subtree in {\tt P} has one leaf that was not a leaf of {\tt T}, and therefore previously unlabeled.
	    The label for this new leaf is input as {\tt newl}.
        Example
	    T = leafTree(4,{{0,1}})
	    P = vertexCut(T, set {0,1}, 0, 4);
	    P#0
	    P#1
	    P#2
    SeeAlso
        edgeCut
///
-------------------------------
-- edgeContract
doc///
    Key
        edgeContract
	(edgeContract,LeafTree,List)
	(edgeContract,LeafTree,Set)
    Headline
        contract an edge of a tree
    Usage
        edgeContract(T,e)
	edgeContract(T,E)
    Inputs
        T:LeafTree
	e:Set
	    an edge specified by the set of leaves on one side of it
	E:List
	    an edge specified by a list of the leaves on one side of it
    Outputs
        :LeafTree
	    obtained from {\tt T} by contracting the specified edge
    Description
        Text
	    This function produces a new object of class @TO LeafTree@ obtained by contracting the edge {\tt e} of tree {\tt T}.
        Example
	    T = leafTree(4,{{0,1}})
	    edgeContract(T, set {0,1})
///
-------------------------------
-- labeledTrees
doc///
    Key
	labeledTrees
	(labeledTrees,ZZ)
    Headline
        enumerate all labeled trees
    Usage
	labeledTrees n
    Inputs
        n:ZZ
	    the number of leaves
    Outputs
        :List
	    of all trees with {\tt n} leaves
    Description
        Text
	    This function enumerates all possible homeomorphically-reduced trees 
	    (no degree-2 vertices) with {\tt n} leaves labeled by $0,\ldots, n-1$, 
	    including all possible labelings.  The trees are represented as objects of class @TO LeafTree@.
        Example
	    L = labeledTrees 4
    SeeAlso
        labeledBinaryTrees
        rootedTrees
        rootedBinaryTrees
	unlabeledTrees
///
-------------------------------
-- labeledBinaryTrees
doc///
    Key
	labeledBinaryTrees
	(labeledBinaryTrees,ZZ)
    Headline
        enumerate all binary labeled trees
    Usage
	labeledTrees n
    Inputs
        n:ZZ
	    the number of leaves
    Outputs
        :List
	    of all binary trees with {\tt n} leaves
    Description
        Text
	    This function enumerates all possible binary trees with {\tt n} leaves 
	    labeled by $0,\ldots, n-1$, including all possible labelings.  
	    The trees are represented as an object of class @TO LeafTree@.
        Example
	    L = labeledBinaryTrees 4
    SeeAlso
        labeledTrees
        rootedTrees
        rootedBinaryTrees
	unlabeledTrees
///
-------------------------------
-- rootedTrees
doc///
    Key
	rootedTrees
	(rootedTrees,ZZ)
    Headline
        enumerate all rooted trees
    Usage
	rootedTrees n
    Inputs
        n:ZZ
	    the number of leaves
    Outputs
        :List
	    of all rooted trees with $n$ leaves
    Description
        Text
	    This function enumerates all possible homeomorphically-reduced trees 
	    (no degree-2 vertices) with a distinguished root and {\tt n-1} unlabeled leaves.  
	    Each tree is an object of class @TO LeafTree@.  For the purposes of representation, 
	    the root is named {\tt 0} and the unlabeled leaves are named $1,\ldots,n-1$.  
	    In other words each class of unlabeled rooted tree is represented once by a particular 
	    labeling of that tree.
        Example
	    L = rootedTrees 4
    SeeAlso
        labeledTrees
        labeledBinaryTrees
        rootedBinaryTrees
	unlabeledTrees
///
-------------------------------
-- rootedBinaryTrees
doc///
    Key
	rootedBinaryTrees
	(rootedBinaryTrees,ZZ)
    Headline
        enumerate all rooted binary trees
    Usage
	rootedBinaryTrees n
    Inputs
        n:ZZ
	    the number of leaves
    Outputs
        :List
	    of all rooted binary @TO LeafTree@s with {\tt n} leaves
    Description
        Text
	    This function enumerates all possible binary trees with a distinguished root 
	    and {\tt n-1} unlabeled leaves. Each tree is an object of class @TO LeafTree@.  
	    For the purposes of representation, the root is named $0$ and the unlabeled leaves 
	    are named $1,\ldots,n-1$.  In other words each class of unlabeled rooted tree is 
	    represented once by a particular labeling of that tree.
        Example
	    L = rootedBinaryTrees 5
    SeeAlso
        labeledTrees
        labeledBinaryTrees
        rootedTrees
	unlabeledTrees
///
-------------------------------
-- unlabeledTrees
doc///
    Key
	unlabeledTrees
	(unlabeledTrees,ZZ)
    Headline
        enumerate all unlabeled trees
    Usage
	unlabeledTrees n
    Inputs
        n:ZZ
	    the number of leaves
    Outputs
        :List
	    of all binary unlabeled trees with {\tt n} leaves
    Description
        Text
	    This function enumerates all possible binary trees with {\tt n} unlabeled leaves.
	    Each tree is an object of class @TO LeafTree@.
	    Each class of unlabeled tree is represented by a particular labeling of that tree.
	    Some duplicates may appear in the list, but each equivalence class is guaranteed to
	    appear at least once.
        Example
	    L = unlabeledTrees 5
    Caveat
        For {\tt n} larger than 5, some equivalence classes of trees may appear more than once.
    SeeAlso
        labeledTrees
        labeledBinaryTrees
        rootedTrees
	rootedBinaryTrees
///
-------------------------------
-- graph
doc///
    Key
	(graph,LeafTree)
    Headline
        convert a LeafTree to Graph
    Usage
        graph T
    Inputs
        T:LeafTree
    Outputs
        :Graph
    Description
        Text
	    This converts a @TO LeafTree@ representation of a tree into a @TO Graph@.

	    The internal vertices of a LeafTree are not named, so each vertex is specified by the partition of the set of leaves
	    formed by removing the vertex.  Each partition is given as a @TO List@ of @TO Set@s.
        Example
	    T = leafTree(4,{{0,1}})
	    G = graph T
	    adjacencyMatrix G
///
-------------------------------
-- digraph
doc///
    Key
	(digraph,LeafTree,List)
	(digraph,LeafTree,Set)
    Headline
        convert a LeafTree to a Digraph
    Usage
        digraph(T,r)
    Inputs
        T:LeafTree
	r:List
	    representing a vertex
    Outputs
        :Digraph
    Description
        Text
	    A rooted tree can be represented by an object of class @TO LeafTree@ and a choice of vertex to be the root.
	    This function converts such a representation of a rooted tree into an object of class @TO Digraph@ with
	    edges oriented away from the root.

	    The internal vertices of an object of class @TO LeafTree@ are not named, so each vertex is specified by the partition of the set of leaves
	    formed by removing the vertex.  Each partition is given as a list of sets.  This is also how the root vertex should
	    be passed to the function.
        Example
	    T = leafTree(4,{{0,1}})
	    r = {set{0,1}, set{2}, set{3}}
	    D = digraph(T,r)
	    adjacencyMatrix D
///

-----------------------------------------------------------
----- TESTS -----
-----------------------------------------------------------

--Here is a test for the leafTree function. The tests depend on the
--internalEdges and leaves methods, to be tested elsewhere.

--We test a six leaf tree by using the internalEdges and leaves methods.
--The internal edges and leaves fully determine a tree.

--First we test the (L,E) input, then we check that
--using the second (ZZ,E) and (Graph) input gives the same result.


TEST ///
S = leafTree(6, {{0,1}, {0,1,2},{0,1, 2, 3}})


D = set internalEdges(S)
L = leaves(S)


d = set {set {0, 1, 2, 3}, set {0, 1, 2}, set {0,1 }}
l = set {0, 1, 2, 3, 4, 5}

assert( D == d)
assert( L == l)

--This test is very simple and potential problems would mostly come from dependencies on graph package
--if this test is not acceptable, go back to leafTree and internalEdges and think of
--how they depend on Graphs package (unrefereed, no low level functionality tests)
///


--The following is a test for leafColorings. We check that leafColorings gives
--the correct SETS. We check that leafColorings(4,CFNmodel) gives the correct
--output set. We also check that leafColorings gives the same output
--for the JCmodel, the K2Pmodel, and the K3Pmodel on the tree with 4 leaves,
--as this method should only depend on the group, and not the actual model.


TEST ///
A =set leafColorings(4, CFNmodel)
B =set leafColorings(4, JCmodel)
C =set leafColorings(4, K2Pmodel)
D =set leafColorings(4, K3Pmodel)
L =set {(0_(ZZ/2), 0_(ZZ/2), 0_(ZZ/2), 0_(ZZ/2)),
    (0_(ZZ/2), 0_(ZZ/2), 1_(ZZ/2), 1_(ZZ/2)),
    (0_(ZZ/2), 1_(ZZ/2), 0_(ZZ/2), 1_(ZZ/2)),
    (0_(ZZ/2), 1_(ZZ/2), 1_(ZZ/2), 0_(ZZ/2)),
    (1_(ZZ/2), 0_(ZZ/2), 0_(ZZ/2), 1_(ZZ/2)),
    (1_(ZZ/2), 0_(ZZ/2), 1_(ZZ/2), 0_(ZZ/2)),
    (1_(ZZ/2), 1_(ZZ/2), 0_(ZZ/2), 0_(ZZ/2)),
    (1_(ZZ/2), 1_(ZZ/2), 1_(ZZ/2), 1_(ZZ/2))}

assert(A == L)
assert(B == C)
assert(C == D)
///

--Here we give a small test that qRing produces a polynomial ring 
--in the correct number of variables and that the elements are as expected

TEST ///
S = leafTree(4, {{0,1}})
R = qRing(S, JCmodel)
assert(dim R == 64)
assert((vars R)_(0,0) == q_(0,0,0,0))
P = qRing(4, JCmodel)
assert(dim P == 64)
assert((vars P)_(0,0) == q_(0,0,0,0))
///



--The following gives tests for phyloToricFP.
--We test the 4-claw, which in
--Sturmfels/Sullivant is the 3-claw. We manually construct the ideal of invariants
--as the kernel of the ring homomorphism determined by the parameterization.
--Similarly, one can check that this is the same as the ideal in Sturmfels/Sullivant
--example 3. To do so, you must include the fourth index on their parameters
--to be the sum of the first three.


TEST ///
T = QQ[q_(0,0,0,0),q_(0,0,1,1), q_(0,1,0,1), q_(0,1,1,0),
    q_(1,0,0,1), q_(1,0,1,0), q_(1,1,0,0), q_(1,1,1,1)]
J = phyloToricFP(4, {}, CFNmodel,QRing=>T)
R = QQ[a_0, a_1, b_0, b_1, c_0, c_1, d_0, d_1]

f = map(R, T, {a_0*b_0*c_0*d_0, a_0*b_0*c_1*d_1, a_0*b_1*c_0*d_1,
	 a_0*b_1*c_1*d_0, a_1*b_0*c_0*d_1, a_1*b_0*c_1*d_0,
	 a_1*b_1*c_0*d_0, a_1*b_1*c_1*d_1})
 I = kernel f
assert(I == J)
///


--The following gives tests for phyloToricLinears and phyloToric42.
--We also include another test for phyloToricFP.
--We construct the toric ideal in the quartet tree with single
--non-trivial split using the Jukes-Cantor model. We construct the ideal as the
--kernel of the homomorphism defined by the standard parameterization. We check that
--this ideal matches the ideal defined by phyloToricFP, I == J. We extract the
--linear generators for the kernel I, and check that these generate the same
--ideal as the generators given as output for phyloToricLinears, M == Q.
--We also check the number of linear generators defined by N and those defined
--by phyloToricLinears, P. Although these sets are not minimal, we check that
--each list is 51. This coincides with the fact that there are 13 distinct
--Fourier coordinates for this tree with the JCmodel, and there are 64 total
--parameters.

TEST ///
T = QQ[q_(0,0,0,0),
    q_(0,0,1,1),q_(0,0,2,2),q_(0,0,3,3),
    q_(0,1,0,1),q_(0,1,1,0),q_(0,1,2,3),
    q_(0,1,3,2),q_(0,2,0,2),q_(0,2,1,3),
    q_(0,2,2,0),q_(0,2,3,1),q_(0,3,0,3),
    q_(0,3,1,2),q_(0,3,2,1),q_(0,3,3,0),
    q_(1,0,0,1),q_(1,0,1,0),q_(1,0,2,3),
    q_(1,0,3,2),q_(1,1,0,0),q_(1,1,1,1),
    q_(1,1,2,2),q_(1,1,3,3),q_(1,2,0,3),
    q_(1,2,1,2),q_(1,2,2,1),q_(1,2,3,0),
    q_(1,3,0,2),q_(1,3,1,3),q_(1,3,2,0),
    q_(1,3,3,1),q_(2,0,0,2),q_(2,0,1,3),
    q_(2,0,2,0),q_(2,0,3,1),q_(2,1,0,3),
    q_(2,1,1,2),q_(2,1,2,1),q_(2,1,3,0),
    q_(2,2,0,0),q_(2,2,1,1),q_(2,2,2,2),
    q_(2,2,3,3),q_(2,3,0,1),q_(2,3,1,0),
    q_(2,3,2,3),q_(2,3,3,2),q_(3,0,0,3),
    q_(3,0,1,2),q_(3,0,2,1),q_(3,0,3,0),
    q_(3,1,0,2),q_(3,1,1,3),q_(3,1,2,0),
    q_(3,1,3,1),q_(3,2,0,1),q_(3,2,1,0),
    q_(3,2,2,3),q_(3,2,3,2),q_(3,3,0,0),
    q_(3,3,1,1),q_(3,3,2,2),q_(3,3,3,3)]

R = QQ[a0, a1, b0, b1, c0, c1, d0, d1, e0, e1]
f = map(R,T, {a0*b0*c0*d0*e0,
          a0*b0*c1*d1*e0, a0*b0*c1*d1*e0, a0*b0*c1*d1*e0,
	  a0*b1*c0*d1*e1, a0*b1*c1*d0*e1, a0*b1*c1*d1*e1,
	  a0*b1*c1*d1*e1, a0*b1*c0*d1*e1, a0*b1*c1*d1*e1,
	  a0*b1*c1*d0*e1, a0*b1*c1*d1*e1, a0*b1*c0*d1*e1,
	  a0*b1*c1*d1*e1, a0*b1*c1*d1*e1, a0*b1*c1*d0*e1,
	  a1*b0*c0*d1*e1, a1*b0*c1*d0*e1, a1*b0*c1*d1*e1,
	  a1*b0*c1*d1*e1, a1*b1*c0*d0*e0, a1*b1*c1*d1*e0,
	  a1*b1*c1*d1*e0, a1*b1*c1*d1*e0, a1*b1*c0*d1*e1,
	  a1*b1*c1*d1*e1, a1*b1*c1*d1*e1, a1*b1*c1*d0*e1,
	  a1*b1*c0*d1*e1, a1*b1*c1*d1*e1, a1*b1*c1*d0*e1,
	  a1*b1*c1*d1*e1, a1*b0*c0*d1*e1, a1*b0*c1*d1*e1,
	  a1*b0*c1*d0*e1, a1*b0*c1*d1*e1, a1*b1*c0*d1*e1,
	  a1*b1*c1*d1*e1, a1*b1*c1*d1*e1, a1*b1*c1*d0*e1,
	  a1*b1*c0*d0*e0, a1*b1*c1*d1*e0, a1*b1*c1*d1*e0,
	  a1*b1*c1*d1*e0, a1*b1*c0*d1*e1, a1*b1*c1*d0*e1,
	  a1*b1*c1*d1*e1, a1*b1*c1*d1*e1, a1*b0*c0*d1*e1,
	  a1*b0*c1*d1*e1, a1*b0*c1*d1*e1, a1*b0*c1*d0*e1,
	  a1*b1*c0*d1*e1, a1*b1*c1*d1*e1, a1*b1*c1*d0*e1,
	  a1*b1*c1*d1*e1, a1*b1*c0*d1*e1, a1*b1*c1*d0*e1,
  	  a1*b1*c1*d1*e1, a1*b1*c1*d1*e1, a1*b1*c0*d0*e0,
  	  a1*b1*c1*d1*e0, a1*b1*c1*d1*e0, a1*b1*c1*d1*e0})

I = ker(f)

--Here's the test for phyloToric42
S = leafTree(4, {{0,1}})
L = phyloToric42(S, JCmodel,  QRing=>T)
assert(I ==L)

--Here's the test for phyloToricFP
J =  phyloToricFP(4, {{0,1}}, JCmodel, QRing=>T)
assert(I == J)

--Here's the test for phyloToricLinears
K={1}
N = for i to 83 when degree(I_i) == K list I_i
M = ideal N
#N
P =  phyloToricLinears(4, {{0,1}}, JCmodel, QRing=>T)
Q = ideal P
#P
assert(#N === 51)
assert(#P === 51)
assert(M == Q)
///

--Here's a test for phyloToricAMatrix. We only test the set of columns, since 
--this only tests the parameterization up to permutation of coordinates. 

TEST ///
A = phyloToricAMatrix(4, {}, CFNmodel)
B = matrix{{1,0,1,0,1,0,1,0},
    {1,0,1,0,0,1,0,1},
    {1,0,0,1,1,0,0,1},
    {1,0,0,1,0,1,1,0},
    {0,1,1,0,1,0,0,1},
    {0,1,1,0,0,1,1,0},
    {0,1,0,1,1,0,1,0},
    {0,1,0,1,0,1,0,1}}
C = transpose B
D = set { submatrix(C, {0}), submatrix(C,{1}), submatrix(C, {2}),
     submatrix(C, {3}), submatrix(C, {4}),  submatrix(C, {5}), 
     submatrix(C, {6}), submatrix(A, {7}) } 
E = set { submatrix(A, {0}), submatrix(A,{1}), submatrix(A, {2}),
     submatrix(A, {3}), submatrix(A, {4}),  submatrix(A, {5}), 
     submatrix(A, {6}), submatrix(A, {7})} 
assert(D == E)
///


--Here is a test for internalVertices. We test a binary tree and also
--a claw tree. We convert all lists to sets to allow for different ordering.

TEST ///
S = leafTree(6, {{0,1}, {0,1,2},{0,1, 2, 3}})
T = leafTree(6, {{}})
internalEdges(S)
internalEdges(T)
IVS= internalVertices(S)
IVSs = set IVS
A = set {set {set {0, 1, 2, 3}, set {4}, set {5}}, set {set {0, 1, 2}, set {3},
	 set {4, 5}}, set {set {0, 1}, set {2}, set {3, 4, 5}}, set {set {0},
      set {1}, set {2, 3, 4, 5}}}
assert( IVSs == A)
IVT = internalVertices(T)
IVTs = set IVT
B = set{set{ set{0}, set {1}, set {2}, set {3}, set {4}, set {5}}}
assert( IVTs == B)
///

--Here is a test for internalEdges. We test a quartet and a 4-claw. 

TEST ///
S = leafTree(4, {{0,1}})
A = set internalEdges(S)
B = set{set{0,1}, set{2,3}}
assert( #(A * B) == 1)
T = leafTree(4, {})
C = set internalEdges(T)
assert( C == set{})
///

--Note for the vertexCut test: This test was written before user declared label
--of new leaf. Now that this label is declared, a much simpler and perhaps more 
--rigourous test can be written. We will leave the old test for now. 

TEST ///
--Here is a test for vertexCut.

S = leafTree(6, {{0,1}, {0,1,2},{0,1, 2, 3}})
VC=vertexCut(S,{0,1,2}, 0, 6)

--First we test that this vertex-cut gives us three connected components, of
--2,3, and 4 leaves each. We check that each tree has the appropriately labeled
--leaves. We have an additional test in which the added vertex is labeled the same on
--each connected component.

M = set{0,1,2, 3, 4, 5}
N= set{0,1,2}
NC = set {3, 4, 5}

L1= for x in VC list leaves(x)

N4 = for x in L1 list(if #x <4 then continue; x)
O4 = for x in N4 list #x
P4 =set flatten for x in N4 list elements x
assert(# set N4 == 1)
assert(O4 == {4})
assert(P4 * NC == set {})

N3 = for x in L1 list(if #x <3 or #x > 3 then continue; x)
P3 =set flatten for x in N3 list elements x
assert(# set N3 == 1)
assert(P3 * N == set{})

N2 = for x in L1 list(if #x <2 or #x > 2 then continue; x)
P2 =set flatten for x in N2 list elements x
assert(# set N2 == 1)
assert(P2 * N == set{})

N1 = for x in L1 list(if #x > 1 then continue; x)
P1 =set flatten for x in N1 list elements x
assert(P1 == set{})

L = (P4 * N ) + (P3 * NC) + (P2 * NC)
assert( L == M)
assert((P4 - N) == (P3 - NC))
assert((P3 - NC) == (P2 - NC))

--Second, we test that the unique 4 leaf component is a quartet tree (and not
--a claw).

A=flatten for x in VC list edges(x)
C =set for x in A list(if #x == 1 then continue; x)
assert(C =!= set{})
///


--Here's a test for edgeCut. 
--First we test that this edge-cut gives us two connected components, of
--4 leaves each. We check that each tree has the appropriately labeled
--leaves.
--Second, we test that the 4 leaf components are the correct quartets.


TEST///
S = leafTree(6, {{0,1}, {0,1,2},{0,1, 2, 3}})
EC = edgeCut(S, {0,1,2}, 6)
N= set{0,1,2,6}
NC = set {3, 4, 5,6}
L1= for x in EC list leaves(x)
T1 = L1#0
T2 = L1#1
assert(  T1 == N or T1 == NC)
assert(  T2 == N or T2 == NC)
A=flatten for x in EC list edges(x)
C =set for x in A list(if #x == 1 then continue; x)
c1 = set{0,1}
d1 = set{2,6}
c2 = set{4,5}
d2 = set{3,6}
assert(C#?c1 or C#?d1)
assert(C#?c2 or C#?d2)
///


--Here is a simple test for edgeContract. 

TEST ///
S = leafTree(4, {{0,1}})
T = edgeContract(S, set{0,1})
A = leaves(T)
B = set internalEdges(T)
assert(A == set {0,1,2,3})
assert(B == set {})
///


TEST ///

-- We test the function joinIdeal by computing the join of the ideal of 
-- the Veronese map with n=1 and d=7 and the ideal of the Segre
-- embedding of P1 x P3. The ideal is computed directly from the 
-- parameterization and using the function joinIdeal. 

R = QQ[x1,x2,x3,x4,x5,x6,x7,x8]
S = QQ[a0,a1,a2,a3,b0,b1,b2,s,t]

f1 = map(S,R,{a0*b0, a1*b0, a2*b0, a3*b0,
	      a0*b1, a1*b1, a2*b1, a3*b1});
f2 = map(S,R,{s^7*t^0, s^6*t^1 ,s^5*t^2, s^4*t^3,s^3*t^4,s^2*t^5,s^1*t^6,s^0*t^7 });

g  = map(S,R,{a0*b0 + s^7*t^0, a1*b0 + s^6*t^1, a2*b0 + s^5*t^2, a3*b0 + s^4*t^3,
	      a0*b1 + s^3*t^4, a1*b1 + s^2*t^5, a2*b1 + s^1*t^6, a3*b1 + s^0*t^7});

I = ker(f1);
J = ker(f2);
assert(joinIdeal(I,J) == ker(g))


///



TEST ///

-- We test the function toricSecantDim by computing the 
-- dimension of a second secant of the CFN model
-- which is known to be non-defective.
-- We also verify that the dimenson of the secant for the
-- CFN model for a 4-leaf tree is no larger than the ambient dimension. 

A = phyloToricAMatrix(6, {{0,1},{2,3},{4,5}},CFNmodel);
assert(toricSecantDim(A,1) == dim(phyloToric42(6, {{0,1},{2,3},{4,5}},CFNmodel)))
assert(toricSecantDim(A,2) == 20) 
assert(toricSecantDim(phyloToricAMatrix(4, {{0,1}},CFNmodel),2) == 8)

///



TEST ///

-- We test the function toricJoinDim using
-- joins of 2 and 3 6-leaf trees.
-- It is known that for the JCmodel, joins  
-- of 2 or 3 arbitrary trees with 6 or more leaves are
-- non-defective.

A = phyloToricAMatrix(6, {{0,1},{2,3},{4,5}},JCmodel);
B = phyloToricAMatrix(6, {{0,1},{0,1,2},{4,5}},JCmodel);
C = phyloToricAMatrix(6, {{1,2},{3,4},{0,5}},JCmodel);

assert(toricSecantDim(A,1) == 10) 
assert(toricSecantDim(B,1) == 10)
assert(toricSecantDim(C,1) == 10)
assert(toricJoinDim(A,B) == 20)
assert(toricJoinDim({A,B,C}) == 30)

///


TEST ///

-- The function phyloToricQuads is tested 
-- by verifying that the ideal generated by 
-- the polynomials returned 
-- modulo the linear invariants
-- is equal to the degree 2 
-- generators of the toric ideal.

Tree = {{0,1},{0,1,2}};
n = 5;
M = JCmodel;
S = qRing(n,M);
L = phyloToricLinears(n,Tree,M,QRing=>S);
T = S/L;
I = ideal phyloToricQuads(n,Tree,M,QRing=>T);
J = phyloToric42(n,Tree,M,QRing=>T);
K = ideal(for i in flatten entries mingens J list (if (degree i)#0 > 2 then continue; i));
assert(I == K)
///


TEST ///

-- The function phyloToricRandom is tested by 
-- verifying that it produces a polynomial in the
-- ideal of invariants for the appropriate model.

Tree = {{0,1}};
n = 4;
M = K2Pmodel;
S = qRing(n,M)
I = phyloToric42(n,Tree, M, QRing=> S);
f = phyloToricRandom(n,Tree,M, QRing=>S);
    
assert(f % I == 0)
///


TEST ///

Tree = {{0,1}};
n = 4;
M = K2Pmodel;
S = qRing(n,M)
I = phyloToric42(n,Tree, M, QRing=> S);
f = phyloToricLinears(n,Tree,M, QRing=>S,Random=>true);
g = phyloToricQuads(n,Tree,M, QRing=>S,Random=>true);
    
assert(f_0 % I == 0)
assert(g_0 % I == 0)
///

TEST ///

--The function fourierToProbability is tested by computing
--the ideal for the same tree in two different ways. The first is
--by computing directly from the parameterization in probability 
--coordinates and the second is by using phyloToric42 to compute
--the ideal in Fourier coordinates and then forming an ideal by 
--converting each of the generators into probability coordinates. 
--We assert the two ideals are equal modulo the certain linear invariants
--that are suppressed when computing in the ring of Fourier coordinates.

S = pRing(4,CFNmodel);
L = ideal apply(8,i->(S_i - S_(15-i)))
R1 = S/L;

R2 = QQ[a0,a1,b0,b1,c0,c1,d0,d1,e0,e1];

f = map(R2,R1,
{a0*b0*c0*d0*e0+a0*b1*c1*d0*e1+a1*b0*c1*d1*e0+a1*b1*c0*d1*e1,
a0*b0*c1*d0*e1+a0*b1*c0*d0*e0+a1*b0*c0*d1*e1+a1*b1*c1*d1*e0,
a0*b0*c0*d0*e1+a0*b1*c1*d0*e0+a1*b0*c1*d1*e1+a1*b1*c0*d1*e0,
a0*b0*c1*d0*e0+a0*b1*c0*d0*e1+a1*b0*c0*d1*e0+a1*b1*c1*d1*e1,
a0*b0*c0*d1*e0+a0*b1*c1*d1*e1+a1*b0*c1*d0*e0+a1*b1*c0*d0*e1,
a0*b0*c1*d1*e1+a0*b1*c0*d1*e0+a1*b0*c0*d0*e1+a1*b1*c1*d0*e0,
a0*b0*c0*d1*e1+a0*b1*c1*d1*e0+a1*b0*c1*d0*e1+a1*b1*c0*d0*e0,
a0*b0*c1*d1*e0+a0*b1*c0*d1*e1+a1*b0*c0*d0*e0+a1*b1*c1*d0*e1,
a0*b0*c1*d1*e0+a0*b1*c0*d1*e1+a1*b0*c0*d0*e0+a1*b1*c1*d0*e1,
a0*b0*c0*d1*e1+a0*b1*c1*d1*e0+a1*b0*c1*d0*e1+a1*b1*c0*d0*e0,
a0*b0*c1*d1*e1+a0*b1*c0*d1*e0+a1*b0*c0*d0*e1+a1*b1*c1*d0*e0,
a0*b0*c0*d1*e0+a0*b1*c1*d1*e1+a1*b0*c1*d0*e0+a1*b1*c0*d0*e1,
a0*b0*c1*d0*e0+a0*b1*c0*d0*e1+a1*b0*c0*d1*e0+a1*b1*c1*d1*e1,
a0*b0*c0*d0*e1+a0*b1*c1*d0*e0+a1*b0*c1*d1*e1+a1*b1*c0*d1*e0,
a0*b0*c1*d0*e1+a0*b1*c0*d0*e0+a1*b0*c0*d1*e1+a1*b1*c1*d1*e0,
a0*b0*c0*d0*e0+a0*b1*c1*d0*e1+a1*b0*c1*d1*e0+a1*b1*c0*d1*e1})

I = ker(f)

T = leafTree(4,{{0,1}});
M = CFNmodel;
J = phyloToric42(T,M);
MJ = mingens J;
FToP = fourierToProbability(R1,ring J,4,M)
J = ideal(FToP MJ_(0,0), FToP MJ_(0,1))
assert(J == I)
///



end
------------------------------------------------------------


restart
installPackage "PhylogeneticTrees"
viewHelp PhylogeneticTrees
