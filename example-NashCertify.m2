restart
needsPackage "AlphaTest"
load "example-Nash.m2"

G = getNashSystem(3,3)
peek G
R = ring G
L = apply(toList(1..numgens R), i -> random(0.,1.))
SubList := apply(toList(0..numgens R-1), i -> (gens R)#i => L#i)
C = coefficientRing ring G
M = sub(sub(G.PolyMap, SubList), C)
N = numericalIrreducibleDecomposition ideal M
c0 = first (first components N).Points
pre0 = point{apply(SubList, i -> i#1)}

(graph,n') = graphStrategy(transpose G.PolyMap,c0,{pre0},NumberOfEdges => 5,TargetSolutionCount => bkkBound(3,3))   
node0 = graph.Vertices#0
SP = polySystem node0.SpecializedSystem
sols = node0.PartialSols

