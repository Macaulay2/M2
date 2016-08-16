restart
needsPackage "AlphaTest"
load "example-Nash.m2"

G = getNashSystem(3,3)
(c0,pre0) = createSeedPair G

(graph,n') = monodromySolve(transpose G.PolyMap,c0,{pre0},NumberOfEdges => 5,TargetSolutionCount => bkkBound(3,3))   
node0 = graph.Vertices#0
SP = polySystem node0.SpecializedSystem
sols = node0.PartialSols
#sols
peek node0
