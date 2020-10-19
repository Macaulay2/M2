newPackage(
     "PhylogeneticTrees",
     Version => "1.0",
     Date => "October 12, 2016",
     Headline => "invariants for group-based phylogenetic models",
     --HomePage => "",
     Authors => {
	  {Name => "Hector Banos", Email => "hdbanoscervantes@alaksa.edu"},
	  {Name => "Nathaniel Bushek", Email => "nbushek@alaska.edu"},
	  {Name => "Ruth Davidson", Email => "ruth.davidson.math@gmail.com"},
	  {Name => "Elizabeth Gross", Email => "elizabeth.gross@sjsu.edu"},
	  {Name => "Pamela Harris", Email => "peh2@williams.edu"},
	  {Name => "Robert Krone", Email => "rckrone@gmail.com"},
	  {Name => "Colby Long", Email => "celong2@ncsu.edu"},
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
--    "edges",
--    "vertices",
--    "leaves",
    "LeafTree",
    "leafTree",
    "internalEdges",
    "internalVertices",
    "edgeCut",
    "vertexCut",
    "edgeContract",
    "QRing",
    "fourierToProbability"
    }
protect \ {Group, Automorphisms, AList}
--------------------------------------------------------------------

Model = new Type of HashTable
LeafTree = new Type of List
group = method()
group(Model) := M -> M.Group
aList = (M,g) -> M.AList#g

model = method()
model(List,List,List) := (G,buckets,auts) -> (
    modelAuts := hashTable for l in auts list (
    	l#0 => (hashTable for i to #G-1 list G#i => G#(l#1#i)));
    AL := hashTable for g in G list (
	g => apply(buckets, b->if member(g,b) then 1 else 0));
    new Model from hashTable {
	Group => G,
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


qRing = method()
qRing(LeafTree,Model) := (T,M) -> qRing(#(leaves T),M)
qRing(ZZ,Model) := (n,M) -> (
    qList := leafColorings(n,M);
    qRingFromList(qList,M)
    )
qRingFromList = (qList,M) -> (
    G := group M;
    Ghash := hashTable apply(#G,i->(G#i=>i));
    q := symbol q;
    QQ[apply(qList, qcolors -> (qindex := apply(qcolors, c->Ghash#c); q_qindex))]
    )

pRing = method()
pRing(LeafTree,Model) := (T,M) -> pRing(#(leaves T),M)
pRing(ZZ,Model) := (n,M) -> (
    G := group M;
    pList := (n:0)..(n:#G-1);
    p := symbol p;
    QQ[apply(pList, pindex->p_pindex)]
    )

fourierToProbability = method()
fourierToProbability(Ring,Ring,ZZ,Model) := (S,R,n,M)  -> (
    G := group M;
    qList := leafColorings(n,M);
    Ghash := hashTable apply(#G,i->(G#i=>i));
    varIndex := apply(qList, qcolors -> apply(qcolors, c->Ghash#c));
    L := (n:0)..(n:#G-1);
    SubVars := for vi in varIndex list (
	(1/#G)*sum for i to #L-1 list (
	    s := sum(n, j->vi#j*(L#i)#j);
	    if even s then S_i else -S_i
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

--List all friendly edge colorings of a tree
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

--A function that takes an invariant on a small tree and extends it in all possible ways to a big tree
--P is graph splitting e.g. a list of trees (#P is length of P)
--Temps is list of ``templates" meaning invariants on small trees
--They are called them templates because they are hard to encode in a way that makes sense in MaCaulay2.
--cem is map between pieces of the small tree and the big tree
--binom is a template so list three things (color indices=subscripts like 000, 001)
--binom#2 is a permutation-how to connect the colors in first monomial with second, make stuff match, use extensions both sides to stay in kernel.
--j%n is j mod n: which one I'm dealing with after the flattening
--permuteColoring uses a group automorphism
fillTemplates = (T,M,S,P,temps,newl,rand) -> (
    G := group M;
    FCs := edgeColorings(T,M); --friendly colorings
    qhash := hashTable apply(#FCs, i->FCs#i => S_i); --maps from friendly edge colorings to variables in the ring
    gensList := {};
    n := #P;
    cem := compositeEdgeMap(T,P,newl);
    PFCs := apply(P, U->partitionedFCs(U,M,set{newl})); --a List of HashTables of colorings of the graph pieces
    --print PFCs;
    for binom in temps do (
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
	gensList = gensList|ultimate(flatten,newGens);
	);
    gensList
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
    R := qRingFromList(qList,M);
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
	if #v > 0 then {v}|children else children
	);
    graph(newLabels, adjacencyMatrix G)
    )

Set == Set := (s,t) -> s === t

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
    E := select(edges T, f->(f != e and (leaves T) - f != e));
    leafTree(leavesList T, E)
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
    GB := gb(J, DegreeLimit=>join(d,d));
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
        A package to compute phylogenetic invariants associated to group-based models
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
	    phylogenetic invariants for any k-valent tree from the k-leaf claw tree as 
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
		Compute the invariants of a group-based phylogenetic tree model using the toric fiber product structure
	Usage
		I = phyloToricFP(T,M)
		I = phyloToricFP(n,E,M)
	Inputs
	        T:LeafTree
		        A tree
		n:ZZ
			The number of leaves
		E:LeafTree
			The internal edges of the tree, given by one part of the bipartition on leaves
		M:Model
                        The model (CNFmodel, JCmodel, etc)
	Outputs
		I:Ideal
		        The ideal of the generators computed
	Description
	        Text
        		This function computes the invariants of a group-based phylogenetic
	        	tree model based on theorem 24 of the paper Toric Ideals of
		        Phylogenetic Invariants by Sturmfels and Sullivant.
			
			Invariants are formed in three different ways.  The linear and
			quadratic invariants are computed as in @TO phyloToricLinears@ and 
			@TO phyloToricQuads@ respectively.  Finally higher degree invariants
			are built using a fiber product construction from the invariants of
			claw trees.
			
			In particular, the neighborhood of any internal vertex of a tree is
			a claw tree.  The invariants on this claw tree tree subgraph are extended
			to invariants on the entire graph in various ways.
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
		Compute the invariants of a group-based phylogenetic tree model using  the 4ti2 package.
	Usage
		I = phyloToric42(n,E,M)
		I = phyloToric42(G,M)
		I = phyloToric42(T,M)
	Inputs
	        T:LeafTree
		        A tree
		n:ZZ
			The number of leaves
		E:LeafTree
			The internal edges of the tree, given by one part of the bipartition on leaves
		G:Graph
			A tree
		M:Model
                        The model (CNFmodel, JCmodel, etc)
	Outputs
		I:Ideal
		       The ideal of the generators computed
	Description
	        Text
		       Computes the invariants of a group-based phylogenetic
	               tree model by computing the transpose of the matrix
	               that encondes the monomial map and the using the function toricMarkov of the
	               4ti2 package.	
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
        Compute the linear invariants of a group-based phylogenetic tree model
    Usage
	L = phyloToricLinears(T,M)
	L = phyloToricLinears(n,E,M)
    Inputs
	T:LeafTree
	    A tree
        n:ZZ
	    The number of leaves
	E:LeafTree
	    The internal edges of the tree, given by one part of the bipartition on leaves
	M:Model
	    The model (CNFmodel, JCmodel, etc)
    Outputs
	L:List
	    A generating set of the linear invariants
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
        Compute the quadratic invariants of a group-based phylogenetic tree model
    Usage
	L = phyloToricQuads(T,M)
	L = phyloToricQuads(n,E,M)
    Inputs
	T:LeafTree
	    A tree
        n:ZZ
	    The number of leaves
	E:LeafTree
	    The internal edges of the tree, given by one part of the bipartition on leaves
	M:Model
	    The model (CNFmodel, JCmodel, etc)
    Outputs
	L:List
	    A generating set of the quadratic invariants
    Description
        Text
	    The quadratic invariants are also referred to as the edge invariants of the model.
	    
	    Each Fourier coordinate corresponds to a friendly coloring of the edges of tree $T$.
	    For any given internal edge $e$ of $T$, the friendly colorings can be obtained by
	    coloring two smaller graphs and gluing them along $e$.  This corresponds to a fiber
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
		Compute a random invariant of a group-based phylogenetic tree model using the toric fiber product structure
	Usage
		f = phyloToricRandom(T,M)
		f = phyloToricRandom(n,E,M)
	Inputs
	        T:LeafTree
		        A tree
		n:ZZ
			The number of leaves
		E:LeafTree
			The internal edges of the tree, given by one part of the bipartition on leaves
		M:Model
                        The model (CNFmodel, JCmodel, etc)
	Outputs
		f:RingElement
		        A randomly selected binomial invariant
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
		Constructs the A matrix whose columns parametrize the toric variety of the tree T with n leaves
	Usage
		A = phyloToricAMatrix(T,M)
		A = phyloToricAmatrix(G,M)
		A = phyloToricAMatrix(n,E,M)
	Inputs
	        T:LeafTree
		        A tree
		G:Graph
		        A tree
		n:ZZ
			The number of leaves
		E:List
			The internal edges of the tree, given by the half the partition on leaves
		M:Model
		        The model (CNFmodel, JCmodel, etc)
	Outputs
		A:Matrix
		        The matrix whose columns parametrize the toric variety
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
    Headline
	Constructs the ring of Fourier coordinates
    Usage
	S = qRing(T,M)
	S = qRing(n,M)
    Inputs
	T:LeafTree
	    A tree
	n:ZZ
	    The number of leaves
	M:Model
	    The model (CNFmodel, JCmodel, etc)
    Outputs
	S:Ring
	    The ring of Fourier coordinates
    Description
	Text
	    The Fourier coordinates for a phylogenetic tree model have one coordinate for each "friendly coloring"
	    of the tree $T$.  A friendly coloring is an assignment of one of the group elements of the model $M$ to each of
	    the leaves of $T$ such that the sum of all the group elements assigned is 0.

	    Each variable of the ring is indexed by a sequence representing a friendly coloring, with each element of the group
	    represented by an integer between 0 and n-1 where n is the order of the group.
	    
	    The variables use symbol {\tt q}.
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
	lists the friendly colorings of a tree
    Usage
	L = leafColorings(T,M)
	L = leafColorings(n,M)
    Inputs
	T:LeafTree
	    A tree
	n:ZZ
	    The number of leaves
	M:Model
	    The model (CNFmodel, JCmodel, etc)
    Outputs
	L:List
	    The friendly colorings of the tree
    Description
	Text
	    {\tt leafColorings} outputs a list of all "friendly colorings" of the leaves of tree $T$.
	    That is all sequences $(g_1,\ldots,g_n)$ such that $g_1+\cdots +g_n = 0$ where each $g_i$ is an
	    element of the group associated to the model $M$, and $n$ is the number of leaves of the tree.

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
    Headline
	Constructs the ring of probability coordinates
    Usage
	S = pRing(T,M)
	S = pRing(n,M)
    Inputs
	T:LeafTree
	    A tree
	n:ZZ
	    The number of leaves
	M:Model
	    The model (CNFmodel, JCmodel, etc)
    Outputs
	S:Ring
	    The ring of probability coordinates
    Description
	Text
	    The probability coordinates for a phylogenetic tree model have one coordinate for each possible outcome of
	    the model. A possible outcome is any labeling of the leaves of the tree by elements of the group $G$ of the
	    model. Thus the number of coordinates is $|G|^n$ where $n$ is the number of leaves.
	    
	    The variables use symbol {\tt p}.
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
        Optional argument to specify Fourier coordinate ring
    Description
	Text
	    For any of the functions that produce phylogenetic invariants in the ring of Fourier coordinates,
	    the Ring can be specified with this optional argument. If {\tt null} is passed then a new ring
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
        Map from Fourier coordinates to probablity coordinates
    Usage
        g = fourierToProbability(S,R,n,M)
    Inputs
        S:Ring
	   The ring of probability coordinates
	R:Ring
	   The ring of Fourier coordinates
	n:ZZ
	   The number of leaves
	M:Model
	   The model (CNFmodel, JCmodel, etc)
    Outputs
        N:RingMap
	    The map from Fourier coordinates to probablity coordinates
    Description
        Text
	    Creates a ring map from the ring of Fourier coordinates to the ring of probability coordinates.
	    The ring of probability coordinates must have at least $|G|^n$ variables where $G$ is the group
	    associated to the model. The ring of Fourier coordinates must have at least $|G|^(n-1)$ variables.
	    
	    
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
	The model corresponging to the Cavender-Farris-Neyman model or binary Jukes Cantor
    Description
	Text
	    The Cavender-Farris-Neyman (CFN) Model is a Markov model of base substitution. It also known as the binary Jukes-Cantor model.
	    It assumes the root distribution vectors describe all bases occurring uniformly in the ancestral sequence.
	    It also assumes that the rate of all specific base changes is the same.

            The transistion matrix has the form
            $$\begin{pmatrix} \alpha&\beta\\
                              \beta&\alpha \end{pmatrix}$$
	--Example
            --
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
	The model corresponding to the Jukes Cantor model
    Description
	Text    
	    The Jukes-Cantor (JK) Model is a Markov model of base substitution. 
	    It assumes the root distribution vectors describe all bases occurring uniformly in the ancestral sequence. 
	    It also assumes that the rate of all specific base changes is the same.
	    Thus the rates of bases changes A-G, A-T and A-C are the same.

            The transistion matrix has the form
            $$\begin{pmatrix} \alpha&\beta&\beta&\beta\\ 
                              \beta&\alpha&\beta&\beta\\
                              \beta&\beta&\alpha&\beta\\
                              \beta&\beta&\beta&\alpha \end{pmatrix}$$ 
	--Example
            --

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
	The model corresponging to the Kimura 2-parameter model
    Description
        Text
	    The Kimura 2-parameter (K2P) Model is a Markov model of base substitution. It assumes the root distribution vectors describe
	    all bases occurring uniformly in the ancestral sequence. It allows different probabilities of transitions and transversions.
	    This means that the rate of base changes A-C and A-T are the same (transversions), and the rate of
	    base change A-G can differ (transitions).

            The transistion matrix has the form
            $$\begin{pmatrix} \alpha&\gamma&\beta&\beta\\ 
                              \gamma&\alpha&\beta&\beta\\
                              \beta&\beta&\alpha&\gamma\\
                              \beta&\beta&\gamma&\alpha \end{pmatrix}$$ 
	--Example
            --

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
	The model corresponging to the Kimura 3-parameter model
    Description
        Text
	    The Kimura 3-parameter (K3P) Model is a Markov model of base substitution. 
	    It assumes the root distribution vectors describe all bases occurring uniformly in the ancestral sequence. 
	    It allows different probabilities of the base changes A-G, A-C and A-T.  
	    This is the most general group based model on group $(\mathbb{Z}/2\mathbb{Z})^2$.

            The transistion matrix has the form
            $$\begin{pmatrix} \alpha&\gamma&\beta&\delta\\ 
                              \gamma&\alpha&\delta&\beta\\
                              \beta&\delta&\alpha&\gamma\\
                              \delta&\beta&\gamma&\alpha \end{pmatrix}$$ 
	--Example
            --

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
	Computes the secant of an ideal
    Usage
	J = secant(I,n)
    Inputs
	I:Ideal
	    An ideal of a ring R
	k:ZZ
	    Order of the secant
    Outputs
	J:Ideal
	    The {\tt k}th secant of {\tt I}
    Description
	Text
	    Computes the $k$th secant of $I$ by constructing the abstract secant and then projecting with elimination.

	    Setting $k$ to 1 gives the dimension of the ideal, while 2 is the usual secant, and higher
	    values correspond to higher order secants.

	    Setting the optional argument @TO DegreeLimit@ to $\{d\}$ will produce only the generators
	    of the secant ideal up to degree $d$.

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
	Computes the join of several ideals
    Usage
	K = joinIdeal(I,J)
	K = joinIdeal L
    Inputs
	I:Ideal
	    An ideal of a ring R
	J:Ideal
	    Another ideal of R
	L:List
	    A list of ideals in the same ring
    Outputs
	K:Ideal
	    The join of the input ideals
    Description
	Text
	    Computes the join by constructing the abstract join and then projecting with elimination.

	    Setting the optional argument @TO DegreeLimit@ to $\{d\}$ will produce only the generators
	    of the secant ideal up to degree $d$.

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
	d = toricSecantDim(A,k)
    Inputs
	A:Matrix
	    The A-matrix of a toric variety
	k:ZZ
	    Order of the secant
    Outputs
	d:ZZ
	    The dimension of the {\tt k}th secant of variety defined by matrix {\tt A}
    Description
        Text
	    A randomized algorithm for computing the affine dimension of a secant of a toric variety,
	    using Terracini's Lemma.

	    Setting $k$ to 1 gives the dimension of the toric variety, while 2 is the usual secant, and higher
	    values correspond to higher order secants.

	    The matrix $A$ defines a parameterization of the variety.  $k$ vectors of parameter values
	    are chosen at random from a large finite field.  The dimension of the sum of the tangent spaces
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
        Dimension of a join of toric varieties
    Usage
	d = toricJoinDim(A,B)
	d = toricJoinDim L
    Inputs
	A:Matrix
	    The A-matrix of a toric variety
	B:Matrix
	    The A-matrix of a toric variety
	L:List
	    A list of A-matrices of toric varieties
    Outputs
	d:ZZ
	    The dimension of the join of the toric varieties defined by the matrices
    Description
        Text
	    A randomized algorithm for computing the affine dimension of a join of toric varieties,
	    using Terracini's Lemma.

	    Each input matrix defines a parameterization of the variety.  For each, a vector of parameter values
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
	A group-based model
    Description
        Text
	    A phylogenetic tree model on tree $T$ has outcomes which are described by assigning each leaf of the tree
	    any label from a particular set (typically the label set is the set of DNA bases, \{A,T,C,G\}).
	    The probability of a certain assignment of labels depends on transition probabilities between each ordered pair of labels.
	    These transition probabilities are the parameters of the model.

	    In a group based model, the label set is a group $G$ (typically $mathbb{Z}/2$ or $(\mathbb{Z}/2)^2$), and the transition
	    probability for pair $(g,h)$ depends only on $h-g$. This reduces the number of parameters from $|G|^2$ to $|G|$.
	    Depending on the model, some of the parameters for different group elements are identified to further restrict the model.

            An object of class @TO Model@ stores the necessary information about a group-based model required to
	    compute phylogenetic invariants.
	    This information includes the elements of the group, how those elements are paritioned, and a set of
	    automorphisms of the group that preserve the partitions.

	    There are four built in models which are Cavender-Farris-Neyman or binary model (@TO "CFNmodel"@), 
	    Jukes-Cantor model (@TO "JCmodel"@), Kimura 2-parameter model (@TO "K2Pmodel"@), 
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
        Construct a Model
    Usage
	T = leafTree(G,B,aut)
    Inputs
        G:List
	    The group elements
	B:List
	    A list of lists of which group elements have identified parameters
	aut:List
	    A list of pairs, assigning pairs of indentified group elements to automorphisms of the group that switch the pair
    Description
        Text
            The elements of $G$ must have an addition operation.  The usual choices for $G$ are the list of elements of
	    $\mathbb{Z}/2$ or $(\mathbb{Z}/2)^2$.
	Example
	    (a,b) = (0_(ZZ/2),1_(ZZ/2))
	    G = {{a,a}, {a,b}, {b,a}, {b,b}}
	Text
	    The elements of $B$ are lists of the elements of $G$ with the same parameter value.

	    In the following example, the first two elements of $G$ receive distinct parameters, while the last two share a parameter.
	    This is precisely the Kimura 2-parameter model.
        Example
            B = {{G#0}, {G#1}, {G#2,G#3}}
	Text
	    Finally for every ordered pair of group elements sharing a parameter, {\tt aut} must provide an automorphism of the group
	    that switches those two group elements.  In {\tt aut} all of the group elements are identified by their index in $G$,
	    and an automorphism is given by a list of permuted index values.

	    In our example, the pairs requiring an automorphism are (2,3) and (3,2).
	Example
            aut = {({2,3}, {0,1,3,2}),
		   ({3,2}, {0,1,3,2})}
	    model(G,B,aut)
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
        Construct a LeafTree
    Usage
        T = leafTree(n,E)
	T = leafTree(L,E)
	T = leafTree(G)
    Inputs
        n:ZZ
	    The number of leaves
	L:List
	    A list of leaves
	E:List
	    A list of lists or sets specifying the internal edges
	G:Graph
	    A tree
    Description
        Text
            A @TO LeafTree@ is specified by listing its leaves, and for each internal edge, 
	    the partition the edge induces on the set of leaves.
	    $L$ is the set of leaves, or if an integer $n$ is input then the leaves will be be named 0,...,n-1.
	    $E$ is a list with one entry for each internal edge.
	    Each entry is a partition specified as a List or Set of the leaves in one side of the partition.
	    Thus each edge can be specified in two possible ways.

	    A LeafTree can also be constructed from a @TO Graph@ provided the graph has no cycles.

	    Here we construct the quartet tree which is the tree with 4 leaves and one internal edge.
        Example
            T = leafTree({a,b,c,d},{{a,b}})
	    leaves T
	    edges T
	Text
	    Here is a tree with 5 leaves given as a Graph.
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
    Headline
	A tree described in terms of its leaves
    Description
        Text
            A tree can be described in terms of its leaves by specifying a leaf set,
	    and specifying the edges as partitions of the leaf set.
	    This leaf centric description is particularly useful for phylogenetic trees.
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
        List the edges of a tree
    Usage
        E = edges T
    Inputs
        T:LeafTree
	    A tree
    Outputs
        E:List
	    The edges of {\tt T}
    Description
        Text
	    This function lists all edges of a tree.  Each entry of the list is a Set of the leaves on one side of the edge.
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
        Lists the internal edges of a tree
    Usage
        E = internalEdges T
    Inputs
        T:LeafTree
	    A tree
    Outputs
        E:List
	    The internal edges of {\tt T}
    Description
        Text
            An internal edge of a tree is an edge that is not incident to a leaf.
	    This function lists such edges. Each entry of the list is a Set of the leaves on one side of the edge.
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
        List the vertices of a tree
    Usage
        V = vertices T
    Inputs
        T:LeafTree
	    A tree
    Outputs
        V:List
	    The vertices of {\tt T}
    Description
        Text
	    This function lists all vertices of a tree.  Each vertex is specified by the partition of the set of leaves
	    formed by removing the vertex.  Each partition is given as a List of Sets.
        Example
	    T = leafTree(4,{{0,1}});
	    vertices T
	    #(vertices T)
    Caveat
        The leaves of {\tt T} in the output of {\tt vertices} have a different representation than in the output of @TO (leaves,LeafTree)@.
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
        List the internal vertices of a tree
    Usage
        V = internalVertices T
    Inputs
        T:LeafTree
	    A tree
    Outputs
        V:List
	    The internal vertices of {\tt T}
    Description
        Text
            An internal vertex of a tree is a vertex that is not a leaf, meaning it has degree at least 2.
	    This function lists such vertices. Each vertex is specified by the partition of the set of leaves
	    formed by removing the vertex.  Each partition is given as a List of Sets.
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
        List the leaves of a tree
    Usage
        L = leaves T
    Inputs
        T:LeafTree
	    A tree
    Outputs
        L:Set
	    The leaves of {\tt T}
    Description
        Text
	    Outputs the leaves of the tree as a Set.
        Example
	    T = leafTree(4,{{0,1}});
	    vertices T
	    #(vertices T)
    Caveat
        The leaves have a different representation than in the output of @TO (vertices,LeafTree)@.
    SeeAlso
        (vertices,LeafTree)
///
-------------------------------
-- edgeCut
doc///
    Key
        edgeCut
	(edgeCut,LeafTree,List,Thing)
	(edgeCut,LeafTree,Set,Thing)
    Headline
        Breaks up a tree at an edge
    Usage
        P = edgeCut(T,e,newl)
	P = edgeCut(T,E,newl)
    Inputs
        T:LeafTree
	    A tree
	e:Set
	    An edge specified by the set of leaves on one side of it
	E:List
	    An edge specified by a list of the leaves on one side of it
	newl:Thing
	    The label for a new leaf
    Outputs
        P:List
	    A list of two @TO LeafTree@s that are subtrees of {\tt T}
    Description
        Text
	    The funtion outputs the two subtrees of $T$ obtained by deleting edge $e$ from $T$ and then re-adding the edge
	    to each of the two resulting subtrees. Both subtrees share a copy of the edge $e$
	    and the newly labeled leaf adjacent to $e$. Other than this overlap, they are disjoint.

	    Each subtree in $P$ may have at most one leaf that was not a leaf of $T$, and therefore previously unlabeled.
	    This leaf label is determined by the user and input as newl.
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
        Breaks up a tree at a vertex
    Usage
        P = vertexCut(T,e,l,newl)
	P = vertexCut(T,E,l,newl)
    Inputs
        T:LeafTree
	    A tree
	e:Set
	    An edge specified by the set of leaves on one side of it
	E:List
	    An edge specified by a list of the leaves on one side of it
	l:Thing
	    A leaf of the tree
	newl:Thing
	    The label for a new leaf
    Outputs
        P:List
	    A list of @TO LeafTree@s that are subtrees of {\tt T}
    Description
        Text
	    Vertices of a LeafTree do not have explicit names.  Therefore a vertex $v$ is specified by naming an edge $e$
	    incident to $v$, and leaf $l$ on the opposite side of the edge as $v$.

	    The function outputs the subtrees of $T$ obtained by deleting the vertex $v$ from $T$
	    and then re-adding $v$ to each of the resulting subtrees as a new leaf.
	    The new leaf on each subtree is adjacent to the edge previously adjacent 
	    to $v$ on $T$. Each subtree has a copy of the vertex labeled newl, but their edge sets are disjoint.

	    Each subtree in $P$ may have at most one leaf that was not a leaf of $T$, and therefore previously unlabeled.
	    This leaf label is determined by the user and input as newl.
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
        Contracts an edge of a tree
    Usage
        U = edgeContract(T,e)
	U = edgeContract(T,E)
    Inputs
        T:LeafTree
	    A tree
	e:Set
	    An edge specified by the set of leaves on one side of it
	E:List
	    An edge specified by a list of the leaves on one side of it
    Outputs
        U:LeafTree
	    The tree obtained from {\tt T} by contracting the specified edge
    Description
        Text
	    This function produces a new LeafTree obtained by contracting the edge $e$ of tree $T$.
        Example
	    T = leafTree(4,{{0,1}})
	    edgeContract(T, set {0,1})
///
-------------------------------
-- graph
doc///
    Key
	(graph,LeafTree)
    Headline
        Converts a LeafTree to Graph
    Usage
        G = graph T
    Inputs
        T:LeafTree
    Outputs
        G:Graph
	
    Description
        Text
	    This converts a LeafTree representation of a tree into a @TO Graph@.

	    The internal vertices of a LeafTree are not named, so each vertex is specified by the partition of the set of leaves
	    formed by removing the vertex.  Each partition is given as a List of Sets.
        Example
	    T = leafTree(4,{{0,1}})
	    G = graph T
	    adjacencyMatrix G
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
--as this method should only depend on the group, and not the acutal model.


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
--in the correct number of variables. 

TEST ///
S = leafTree(4, {{0,1}})
R = qRing(S, JCmodel)
P = qRing(4, JCmodel)
assert(dim R == 64)
assert(dim P == 64)
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
--ideal as the generatros given as output for phyloToricLinears, M == Q.
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
IVSs = set for x in IVS list set x
A = set {set {set {0, 1, 2, 3}, set {4}, set {5}}, set {set {0, 1, 2}, set {3},
	 set {4, 5}}, set {set {0, 1}, set {2}, set {3, 4, 5}}, set {set {0},
      set {1}, set {2, 3, 4, 5}}}
assert( IVSs == A)
IVT = internalVertices(T)
IVTs = set for x in IVT list set x
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
--converting each of the generators into probablity coordinates. 
--We assert the two ideals are equal modulo the certain linear invariants
--that are supressed when computing in the ring of Fourier coordinates.

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
