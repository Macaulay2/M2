
-- verifies if for any outgoing arc there is an incoming arc
inconsistentArc = N -> (
    tree := N.elimTree;
    nods := select(tree.nodes, i -> tree.parents#i=!=null);
    for i in nods do (
        p:= tree.parents#i;
        for Ni in nodes(N,i) do (
            for Np in Ni.parents do
                if not member(Ni,Np.children) then return (Ni,Np);
            for Nc in Ni.children do
                if not member(Ni,Nc.parents) then return (Nc,Ni);
            );
    );
    return false;
)

-- monomial ideal in 8 variables
exampleNet8 = (topDim) -> (
    getElimTree:= X -> (
        parnt := hashTable {X_0=>X_5, X_1=>X_2, X_2=>X_5, 
            X_3=>X_4, X_4=>X_5, X_5=>X_6, X_6=>X_7, X_7=>null};
        clique := applyPairs(parnt, (i,p) ->
            if p=!=null then (i,{i,p}) else (i,{i}) );
        return elimTree(X,parnt,clique);
    );
    getDigraph:= (topDim) -> (
        V:={"x_0_0", "x_0_1", "x_1_0", "x_1_1", "x_2_0", "x_2_1", 
            "x_3_0", "x_3_1", "x_4_0", "x_4_1", "x_5_1", "x_5_2", 
            "x_5_4", "x_6_0", "x_6_1", "x_7_0", "x_7_1"};
        E:={{"x_0_0", "x_5_1"}, {"x_0_1", "x_5_2"}, {"x_0_1", "x_5_4"},
            {"x_1_0", "x_2_1"}, {"x_1_1", "x_2_0"}, {"x_2_0", "x_5_1"},
            {"x_2_0", "x_5_2"}, {"x_2_0", "x_5_4"}, {"x_2_1", "x_5_2"},
            {"x_2_1", "x_5_4"}, {"x_3_0", "x_4_1"}, {"x_3_1", "x_4_0"},
            {"x_4_0", "x_5_1"}, {"x_4_0", "x_5_2"}, {"x_4_1", "x_5_4"},
            {"x_5_1", "x_6_0"}, {"x_5_2", "x_6_0"}, {"x_5_2", "x_6_1"},
            {"x_5_4", "x_6_0"}, {"x_5_4", "x_6_1"}, {"x_6_0", "x_7_0"},
            {"x_6_1", "x_7_1"} };
        E':={{"x_1_0","x_2_0"}, {"x_3_0","x_4_0"}};
        if not topDim then E = E | E';
        return digraph(V,E);
    );
    kk := ZZ/10007; 
    R := kk[x_0..x_7, MonomialOrder=>Lex];
    X := gens R;
    tree := getElimTree(X);
    DG := getDigraph(topDim);
    ranks := hashTable for v in vertices DG list
        v => value substring(0,3,v);
    eqs:=hashTable{ "x_0_0" => {X_0}, "x_0_1" => {}, "x_1_0" => {X_1},
        "x_1_1" => {}, "x_2_0" => {X_2}, "x_2_1" => {}, "x_3_0" => {X_3},
        "x_3_1" => {}, "x_4_0" => {X_4}, "x_4_1" => {}, "x_5_0" => {X_5},
        "x_5_1" => {}, "x_5_2" => {X_5}, "x_5_3" => {X_5}, "x_5_4" => {X_5},
        "x_6_0" => {X_6}, "x_6_1" => {}, "x_7_0" => {}, "x_7_1" => {X_7} };
    eqs = applyPairs( eqs, (v,F) -> (v,(F,{})) );
    N := chordalNet(eqs,ranks,tree,DG);
    N.isTriangular = true;
    return N;
)

--###################################
-- Graphs
--###################################

-- Chordal completions

G = wheelGraph(6)
Gc = chordalGraph(G)
assert(#edges Gc == 12)
assert(treewidth Gc == 3)

V = toList(0..9)
E = { {0,{6,7}},{1,{4,9}},{2,{3,5}},{3,{7,8}},
    {4,{5,8}},{5,{8}},{6,{8,9}},{7,{8}},{8,{9}} }
G = graph(V,E)
Gc = chordalGraph(G)
assert(#edges Gc == 21)
assert(treewidth Gc == 3)
Gc' = chordalGraph (G,{0,1,2,3,4,5,6,7,8,9})
assert(adjacencyMatrix Gc == adjacencyMatrix Gc')
Gc' = chordalGraph (G,{0,6,1,4,9,8,7,3,5,2})
assert(#edges Gc' == 21)
assert(treewidth Gc' == 3)

E2 = { {0,{6,7}},{1,{4,9}},{2,{3,5}},{3,{5,7,8}},
    {4,{5,8,9}},{5,{7,8,9}},{6,{7,8,9}},{7,{8,9}},{8,{9}} }
G2 = graph(V, E2)
assert(isChordal G2)
assert(#edges chordalGraph G2 == 21)
G2 = graph(reverse V, E2)
assert(#edges chordalGraph G2 == 21)

-- Elimination tree

tree = elimTree Gc
assert(leaves tree == {0,1,2})
assert(tree.children#5=={3,4})
assert(tree.children#6=={0})
assert(tree.children#null=={9})

-- Constraint graph

R = QQ[x_0..x_3]
I = ideal {x_0^2*x_1*x_2 +2*x_1 +1, x_1^2 +x_2, x_1 +x_2, x_2*x_3}
G = constraintGraph I
assert(#vertices G == 4)
assert(#edges G == 4)

--###################################
-- Chordal networks
--###################################

-- Construction

R = QQ[x_0..x_3, MonomialOrder=>Lex]
F = {x_0^3-x_0, x_0*x_2-x_2, x_1-x_2, x_2^2-x_2, x_2*x_3^2-x_3}
N = chordalNet ideal 0_R
N = chordalNet ideal 1_R
N = chordalNet ideal F
assert(N.structure == "Binomial")

N = chordalNet ideal {x_0^3, x_2*x_3^2}
assert(N.structure == "Monomial")
N = chordalNet ideal {x_0+x_2-x_3^2}
assert(N.structure == "None")

R = ZZ/17[a,b,c, MonomialOrder=>Lex]
N = chordalNet ideal 0_R

R = QQ[x_0..x_3]
I = ideal {x_0^3-x_0, x_0*x_2-x_2, x_1-x_2, x_2^2-x_2, x_2*x_3^2-x_3}
try chordalNet I then error("grevlex unhandled") else 0

-- Suggest order
G = cartesianProduct(cycleGraph 3, pathGraph 5)
I = edgeIdeal G
N = chordalNet(toLex I)
assert(treewidth elimTree N == 6)
N = chordalNet(I,"SuggestOrder")
assert(treewidth elimTree N == 3)

-- Chordal elimination

R = QQ[x_0..x_3, MonomialOrder=>Lex];
I = ideal {x_0^4-1, x_0^2+x_2, x_1^2+x_2, x_2^2+x_3};
N = chordalNet I
getEqs = i -> (first nodes(N,i)).gens
assert(chordalElim N)
assert(getEqs(x_0)=={x_0^2+x_2})
assert(getEqs(x_3)=={x_3+1})

I = ideal {x_0^3-x_0, x_0*x_2-x_2, x_1-x_2, x_2^2-x_2, x_2*x_3^2-x_3}
N = chordalNet I
assert(chordalElim N)
assert(getEqs(x_2)=={x_2*x_3-x_3, x_2^2-x_2})
assert(getEqs(x_3)=={x_3^2-x_3})

R = ZZ/17[x_0..x_2, MonomialOrder=>Lex];
I = ideal {x_0*x_1+1, x_1+x_2, x_1*x_2};
N = chordalNet I
assert(not chordalElim N)
assert(getEqs(x_2)=={x_2^2})

I = chromaticIdeal(QQ, cycleGraph 10, 3);
use ring I
N = chordalNet I
assert(chordalElim N)
assert(getEqs(a)== {a*b-a*j+b^2-j^2, a^2+a*j+j^2})
assert(getEqs(i)== {i^2+i*j+j^2})

--###################################
-- Chains
--###################################

-- nextOrderedPartition
L = {{0,1},{0,1,2},{2,3}}
P = nextOrderedPartition(5,L)
assert(P=={0,2,3})
P = nextOrderedPartition(P,5,L)
assert(P=={1,1,3})
P = nextOrderedPartition(P,5,L)
assert(P=={1,2,2})
assert(nextOrderedPartition(P,5,L)===null)

-- nextChain

numChains = N -> (
    nC := 0;
    C := nextChain N;
    while C=!=null do (
        C=nextChain(C,N); 
        nC=nC+1;
    );
    return nC;
)
numChainsCdim = (N,cdim) -> (
    nC := 0;
    (C,data) := nextChain(cdim,N);
    while C=!=null do (
        if codim C != cdim then error("wrong codimension");
        C=nextChain(C,data,cdim,N); 
        nC=nC+1;
    );
    return nC;
)

-- codimCount

N = exampleNet8(true)
assert(size N == {2,2,2,2,2,3,2,2})
CT = codimCount N
t = (ring CT)_0
assert(CT == 9*t^4)
assert(numChains N == 9 )
assert(numChainsCdim(N,4) == 9 )
assert(numChainsCdim(N,5) == 0 )
assert(numChainsCdim(N,3) == 0 )

N = exampleNet8(false)
CT = codimCount N
t = (ring CT)_0
assert(CT == 3*t^6 + 10*t^5 + 9*t^4)
assert(numChains N == 22 )
assert(numChainsCdim(N,4) == 9 )
assert(numChainsCdim(N,5) == 10 )
assert(numChainsCdim(N,6) == 3 )
assert(numChainsCdim(N,3) == 0 )

-- topComponents

N = exampleNet8(true)
topComponents N
assert(inconsistentArc N === false)
CT = codimCount N
t = (ring CT)_0
assert(CT == 9*t^4)

N = exampleNet8(false)
topComponents N
assert(inconsistentArc N === false)
CT = codimCount N
t = (ring CT)_0
assert(CT == 9*t^4)

-- reduceNet

N = exampleNet8(false)
reduceNet N
assert(inconsistentArc N === false)
CT = codimCount N
t = (ring CT)_0
assert(CT == 9*t^4)

--###################################
-- Chordal triangularization
--###################################

-- Monomial

I = subsetsProductsIdeal(QQ,5,3)
N = chordalNet I
chordalTria N
assert(inconsistentArc N === false)
CT = codimCount N
t = (ring CT)_0
assert(CT == 10*t^3)

G = graph({0,1,2,3,4,5,6,7},   -- 8 vertex tree
    {{0,5},{1,2},{2,5},{3,4},{4,5},{5,6},{6,7}});
I = toLex edgeIdeal G
N = chordalNet I
chordalTria N
CT = codimCount N
t = (ring CT)_0
topCT = min terms CT
assert(topCT == 9*t^4)

-- Binomial

R = QQ[x_0..x_3, MonomialOrder=>Lex];
I = ideal {x_0^3-x_0, x_0*x_2-x_2, x_1-x_2, x_2^2-x_2, x_2*x_3^2-x_3};
N = chordalNet I
chordalTria N
assert(inconsistentArc N === false)
assert(rootCount N == 5)

I = adjacentMinorsIdeal(GF(4),2,10);
N = chordalNet I
chordalTria N
assert(inconsistentArc N === false)
CT = codimCount N
t = (ring CT)_0
topCT = min terms CT
assert(topCT == 55*t^9)
topComponents N
CT = codimCount N
t = (ring CT)_0
assert(CT == 55*t^9)
assert(rootCount N == 0)

--###################################
-- Using chordal networks
--###################################

-- prem 

cyclicMinors = (kk,n) -> (
    I := adjacentMinorsIdeal(kk,2,n);
    X := gens ring I;
    J := I + (X_0 * X_(-1) - X_1*X_(-2));
    f := sum gbList J;
    N := chordalNet J;
    chordalTria N;
    return (f,N);
)

(f,N) = cyclicMinors(ZZ/10007,4);
assert(f % N == 0)
assert any( 0..20, i -> (f+1) % N != 0 )

(f,N) = cyclicMinors(QQ,5);
r = f % N
assert(char ring r > 0)
assert(r == 0)

-- reduceDimension

CT = codimCount N
t = (ring CT)_0
term = terms CT
topCT = sum term_{#term-2,#term-1}
reduceDimension(N,2)
CT2 = codimCount N
phi = map(ring CT, ring CT2)
assert(phi CT2 == topCT)

-- components (monomial)

I = toLex edgeIdeal cycleGraph 8
X = gens ring I
N = chordalNet I;
chordalTria N
C = components(N,1)
assert(#C==1)
assert(#(C#4)==2)
C = components N
assert(#(C#4)==2)
assert(#(C#5)==8)
if C#?6 then assert(#(C#6)==0)
